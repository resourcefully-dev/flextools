# V2G flexibility management -------------------------------------------------

#' Smart V2G algorithm
#'
#' Prototype vehicle-to-grid simulation built on top of `smart_charging()`
#' helpers. The current implementation mirrors the classic workflow
#' (responsiveness filtering, setpoint definition and scheduling placeholder)
#' but it isolates the new optimisation pieces so the existing API remains
#' untouched.
#'
#' @inheritParams smart_charging
#' @param opt_objective character, optimisation objective being
#'   `"none"`, `"grid"`, `"cost"` or a value between 0 (cost)
#'   and 1 (grid). Only the `"grid"` mode is currently supported
#'   by the V2G prototype.
#'
#' @return list with setpoints, sessions and demand similar to
#'   `smart_charging()`. Scheduling is still charging-only while
#'   the V2G-specific session logic is under development.
#' @export
#'
smart_v2g <- function(sessions, opt_data, opt_objective = "grid",
                      window_days, window_start_hour,
                      responsive = NULL, power_th = 0,
                      charging_power_min = 0, energy_min = 1,
                      include_log = FALSE, show_progress = FALSE,
                      lambda = 0) {
  if (show_progress) cli::cli_h1("Set up (V2G)")

  if (show_progress) cli::cli_progress_step("Checking parameters")

  if (is.null(sessions) || nrow(sessions) == 0) {
    stop("Error: `sessions` parameter is empty.")
  }

  sessions_basic_vars <- c(
    "Session", "Timecycle", "Profile", "ConnectionStartDateTime",
    "ConnectionHours", "Power", "Energy"
  )
  if (!all(sessions_basic_vars %in% colnames(sessions))) {
    stop("Error: `sessions` does not contain all required variables (see Arguments description)")
  }

  if (is.null(opt_data)) {
    stop("Error: `opt_data` parameter is empty.")
  }
  if (!("datetime" %in% colnames(opt_data))) {
    stop("Error: `opt_data` does not contain `datetime` variable")
  }
  if (!any(sessions$ConnectionStartDateTime %in% opt_data$datetime)) {
    stop("Error: `sessions` do not charge during `datetime` period in `opt_data`")
  }

  if (opt_objective == "none") {
    if (!any(c(unique(sessions$Profile), "grid_capacity", "import_capacity", "export_capacity") %in% names(opt_data))) {
      stop('Error: when `opt_objective` = "none" you must set a setpoint in `opt_data` with grid capacity or a user profile name.')
    }
  }

  if (is.null(responsive)) {
    responsive <- purrr::map(
      purrr::set_names(unique(sessions$Timecycle)),
      ~ purrr::map(
        purrr::set_names(unique(sessions$Profile)),
        ~1
      )
    )
  }

  opt_data$flexible <- 0
  opt_data <- check_optimization_data(opt_data, opt_objective)

  if (show_progress) cli::cli_progress_step("Defining optimisation windows")

  dttm_seq <- opt_data$datetime
  time_resolution <- get_time_resolution(dttm_seq, units = "mins")
  sessions <- sessions %>%
    evsim::adapt_charging_features(time_resolution = time_resolution)

  flex_windows_idx <- get_flex_windows(
    dttm_seq, window_days, window_start_hour
  )

  if (show_progress) cli::cli_progress_step("Calculating EV demand")
  profiles_demand <- evsim::get_demand(sessions, dttm_seq)

  if (show_progress) cli::cli_h1("Smart V2G")

  if (show_progress) cli::cli_progress_step("Setting responsiveness")

  windows_data <- purrr::map(
    flex_windows_idx$flex_idx,
    function(flex_idx) {
      list(
        sessions_window = sessions %>%
          dplyr::filter(
            .data$ChargingStartDateTime >= dttm_seq[flex_idx[1]],
            .data$ChargingStartDateTime <= dttm_seq[flex_idx[length(flex_idx)]]
          ) %>%
          set_responsive(dttm_seq[flex_idx], responsive),
        profiles_demand = profiles_demand[flex_idx, ],
        opt_data = opt_data[flex_idx, ]
      )
    }
  )

  if (show_progress) cli::cli_progress_step("Defining V2G setpoints")

  setpoints_lst <- get_setpoints_v2g_parallel(
    windows_data, opt_objective, lambda
  )

  if (show_progress) cli::cli_progress_step("Scheduling V2G sessions")

  scheduling_lst <- smart_v2g_window_parallel(
    windows_data, setpoints_lst, include_log
  )

  if (show_progress) cli::cli_progress_step("Cleaning data set")

  setpoints <- list_rbind(setpoints_lst)

  setpoints_opt <- profiles_demand
  opt_dttm_idx <- setpoints_opt$datetime %in% setpoints$datetime
  setpoints_opt[opt_dttm_idx, names(setpoints)] <- setpoints

  sessions_considered <- purrr::map(
    scheduling_lst, ~ .x$sessions
  ) %>%
    list_rbind()

  if (nrow(sessions_considered) > 0) {
    sessions_not_considered <- sessions[!(sessions$Session %in% sessions_considered$Session), ]
  } else {
    sessions_not_considered <- sessions %>%
      dplyr::mutate(
        Responsive = NA, Flexible = NA, Exploited = NA
      )
  }

  sessions_opt <- dplyr::bind_rows(
    sessions_not_considered,
    sessions_considered
  ) %>%
    dplyr::select(any_of(names(sessions)), dplyr::everything()) %>%
    dplyr::mutate(Session = factor(.data$Session, levels = sessions$Session)) %>%
    dplyr::arrange(.data$Session, .data$ConnectionStartDateTime, .data$ConnectionEndDateTime) %>%
    dplyr::mutate(Session = as.character(.data$Session)) %>%
    dplyr::distinct()

  demand <- purrr::map(
    scheduling_lst, ~ .x$demand
  ) %>%
    list_rbind()

  demand_opt <- profiles_demand
  opt_dttm_idx <- demand_opt$datetime %in% demand$datetime
  demand_opt[opt_dttm_idx, names(demand)] <- demand

  log_lst <- purrr::map(scheduling_lst, ~ .x$log)
  log <- do.call(c, log_lst)

  structure(
    list(
      sessions = sessions_opt,
      setpoints = setpoints_opt,
      demand = demand_opt,
      log = log
    ),
    class = c("SmartV2G", "list")
  )
}


#' Set setpoints for smart V2G
#'
#' @keywords internal
#'
get_setpoints_v2g <- function(sessions_window, opt_data, profiles_demand, opt_objective, lambda) {
  if (nrow(sessions_window) == 0) {
    return(sessions_window)
  }

  dttm_seq <- opt_data$datetime

  if ("static" %in% colnames(opt_data)) {
    L_fixed <- opt_data$static
  } else {
    L_fixed <- rep(0, nrow(opt_data))
  }

  opt_profiles <- get_opt_profiles(sessions_window)
  setpoints <- profiles_demand %>%
    dplyr::select(any_of(c("datetime", opt_profiles)))

  for (profile in opt_profiles) {
    if (profile %in% colnames(opt_data)) {
      setpoints[[profile]] <- opt_data[[profile]]
    } else if (opt_objective != "none") {
      sessions_window_prof_flex <- sessions_window %>%
        dplyr::filter(.data$Profile == profile & .data$Responsive)

      non_responsive_sessions <- sessions_window %>%
        dplyr::filter(.data$Profile == profile & (!.data$Responsive | is.na(.data$Responsive)))

      if (nrow(non_responsive_sessions) > 0) {
        L_fixed_prof <- non_responsive_sessions %>%
          evsim::get_demand(dttm_seq = dttm_seq, by = "Profile") %>%
          dplyr::pull(!!rlang::sym(profile))
      } else {
        L_fixed_prof <- rep(0, length(dttm_seq))
      }

      sessions_window_prof_flex$ConnectionEndDateTime[
        (sessions_window_prof_flex$ConnectionEndDateTime >= dttm_seq[length(dttm_seq)])
      ] <- dttm_seq[length(dttm_seq)]

      window_prof_dttm <- c(
        min(sessions_window_prof_flex$ConnectionStartDateTime),
        min(max(sessions_window_prof_flex$ConnectionEndDateTime), dttm_seq[length(dttm_seq)])
      )
      opt_idxs <- (dttm_seq >= window_prof_dttm[1]) &
        (dttm_seq <= window_prof_dttm[2])

      LF <- setpoints[[profile]] - L_fixed_prof

      L_others <- setpoints %>%
        dplyr::select(-any_of(c(profile, "datetime"))) %>%
        rowSums()
      if (length(L_others) == 0) {
        L_others <- rep(0, length(dttm_seq))
      }
      LS <- L_fixed + L_others + L_fixed_prof

      if (opt_objective == "grid") {
        O <- minimize_net_power_v2g_window(
          G = opt_data$production[opt_idxs],
          LF = LF[opt_idxs],
          LS = LS[opt_idxs],
          direction = "forward",
          time_horizon = NULL,
          LFmax = Inf,
          import_capacity = opt_data$import_capacity[opt_idxs],
          export_capacity = opt_data$export_capacity[opt_idxs],
          lambda = lambda
        )
      } else if (opt_objective == "cost") {
        message("Cost-based optimisation is not yet implemented for V2G; reusing grid objective.")
        O <- minimize_net_power_v2g_window(
          G = opt_data$production[opt_idxs],
          LF = LF[opt_idxs],
          LS = LS[opt_idxs],
          direction = "forward",
          time_horizon = NULL,
          LFmax = Inf,
          import_capacity = opt_data$import_capacity[opt_idxs],
          export_capacity = opt_data$export_capacity[opt_idxs],
          lambda = lambda
        )
      } else if (is.numeric(opt_objective)) {
        message("Combined optimisation is not yet implemented for V2G; reusing grid objective.")
        O <- minimize_net_power_v2g_window(
          G = opt_data$production[opt_idxs],
          LF = LF[opt_idxs],
          LS = LS[opt_idxs],
          direction = "forward",
          time_horizon = NULL,
          LFmax = Inf,
          import_capacity = opt_data$import_capacity[opt_idxs],
          export_capacity = opt_data$export_capacity[opt_idxs],
          lambda = lambda
        )
      }

      setpoints[[profile]][opt_idxs] <- O + L_fixed_prof[opt_idxs]
    } else if ("import_capacity" %in% colnames(opt_data)) {
      L_others <- profiles_demand %>%
        dplyr::select(-any_of(c(profile, "datetime"))) %>%
        rowSums()
      if (length(L_others) == 0) {
        L_others <- rep(0, length(dttm_seq))
      }

      profile_power_limited <- pmin(
        pmax(
          opt_data$import_capacity - (L_fixed + L_others),
          0
        ),
        profiles_demand[[profile]]
      )
      setpoints[[profile]] <- profile_power_limited
    }
  }

  setpoints
}


get_setpoints_v2g_parallel <- function(windows_data, opt_objective, lambda) {
  purrr::map(
    windows_data,
    ~ get_setpoints_v2g(
      sessions_window = .x$sessions_window,
      opt_data = .x$opt_data,
      profiles_demand = .x$profiles_demand,
      opt_objective = opt_objective,
      lambda = lambda
    )
  )
}


smart_v2g_window <- function(sessions_window, profiles_demand, setpoints, include_log = FALSE) {
  if (nrow(sessions_window) == 0) {
    return(
      list(
        sessions = dplyr::tibble(),
        demand = profiles_demand,
        setpoints = setpoints,
        log = list()
      )
    )
  }

  dttm_seq <- setpoints$datetime
  log <- list()
  log_window_name <- as.character(lubridate::date(dttm_seq[1]))
  log[[log_window_name]] <- list()
  sessions_considered <- dplyr::tibble()

  opt_profiles <- get_opt_profiles(sessions_window)

  for (profile in opt_profiles) {
    sessions_window_prof_flex <- sessions_window %>%
      dplyr::filter(.data$Profile == profile & .data$Responsive)

    non_responsive_sessions <- sessions_window %>%
      dplyr::filter(.data$Profile == profile & (!.data$Responsive | is.na(.data$Responsive)))

    if (nrow(non_responsive_sessions) > 0) {
      L_fixed_prof <- non_responsive_sessions %>%
        evsim::get_demand(dttm_seq = dttm_seq, by = "Profile") %>%
        dplyr::pull(!!rlang::sym(profile))
    } else {
      L_fixed_prof <- rep(0, nrow(setpoints))
    }

    setpoint_prof <- dplyr::tibble(
      datetime = dttm_seq,
      setpoint = setpoints[[profile]] - L_fixed_prof
    )

    results <- schedule_sessions_v2g(
      sessions = sessions_window_prof_flex,
      setpoint = setpoint_prof,
      include_log = include_log,
      show_progress = FALSE
    )

    # Final profile sessions
    sessions_window_prof_final <- dplyr::bind_rows(
      results$sessions,
      non_responsive_sessions
    )

    # Update the time-series demand
    if (!("Profile" %in% colnames(sessions_window_prof_final))) {
      sessions_window_prof_final$Profile <- profile
    }

    sessions_window_prof_final_demand <- evsim::get_demand(
      sessions_window_prof_final,
      dttm_seq = dttm_seq,
      by = "Profile"
    )
    profiles_demand[[profile]] <-
      sessions_window_prof_final_demand[[profile]]

    # Join with the rest of data set
    sessions_considered <- dplyr::bind_rows(
      sessions_considered,
      sessions_window_prof_final
    )

    if (include_log) {
      log[[log_window_name]][[profile]] <- results$log
    }
  }

  list(
    sessions = sessions_considered,
    demand = profiles_demand,
    setpoints = setpoints,
    log = log
  )
}


smart_v2g_window_parallel <- function(
  windows_data, setpoints_lst, include_log
) {
  if (!requireNamespace("mirai", quietly = TRUE) ||
    !requireNamespace("carrier", quietly = TRUE)) {
    scheduling_lst <- purrr::map2(
      windows_data, setpoints_lst,
      \(x, y)
      smart_v2g_window(
        sessions_window = x$sessions_window,
        profiles_demand = x$profiles_demand,
        setpoints = y,
        include_log = include_log
      )
    )
  } else {
    scheduling_lst <- purrr::map2(
      windows_data, setpoints_lst,
      in_parallel(
        \(x, y)
        smart_v2g_window(
          sessions_window = x$sessions_window,
          profiles_demand = x$profiles_demand,
          setpoints = y,
          include_log = include_log
        ),
        smart_v2g_window = smart_v2g_window,
        include_log = include_log
      )
    )
  }
  return(scheduling_lst)
}


#' Minimisation of net power with bidirectional flexibility (single window)
#'
#' @keywords internal
#'
minimize_net_power_v2g_window <- function(G, LF, LS, direction, time_horizon, LFmax, import_capacity, export_capacity, lambda = 0) {
  time_slots <- length(LF)
  identityMat <- diag(time_slots)

  P <- 2 * identityMat * (1 + lambda)
  q <- 2 * (LS - G - lambda * LF)

  solve_optimization_window_v2g(
    G, LF, LS, direction, time_horizon, LFmax,
    import_capacity, export_capacity, P, q
  )
}


solve_optimization_window_v2g <- function(G, LF, LS, direction, time_horizon, LFmax, import_capacity, export_capacity, P, q) {
  G <- round(G, 2)
  LF <- round(LF, 2)
  LS <- round(LS, 2)

  time_slots <- length(G)
  if (is.null(time_horizon)) {
    time_horizon <- time_slots
  }

  bounds <- get_bounds_v2g(
    time_slots, G, LF, LS, direction, time_horizon,
    LFmax, import_capacity, export_capacity
  )

  constraint_mats <- list(bounds$Amat_O, bounds$Amat_cumsum, bounds$Amat_energy)
  A <- Matrix::Matrix(do.call(rbind, constraint_mats), sparse = TRUE)
  l_vec <- c(bounds$lb_O, bounds$lb_cumsum, bounds$lb_energy)
  u_vec <- c(bounds$ub_O, bounds$ub_cumsum, bounds$ub_energy)

  solver <- osqp::osqp(
    P = Matrix::forceSymmetric(Matrix::Matrix(P)),
    q = q,
    A = A,
    l = l_vec,
    u = u_vec,
    pars = list(
      verbose = FALSE,
      eps_abs = 1e-6,
      eps_rel = 1e-6,
      polish = TRUE
    )
  )
  O <- solver$Solve()

  if (O$info$status_val %in% c(1, 2)) {
    round(O$x, 2)
  } else {
    message_once(paste0("\u26A0\uFE0F V2G optimisation warning: ", O$info$status, ". No optimisation provided."))
    LF
  }
}


get_bounds_v2g <- function(time_slots, G, LF, LS, direction, time_horizon, LFmax, import_capacity, export_capacity) {
  identityMat <- diag(time_slots)
  cumsumMat <- triangulate_matrix(matrix(1, time_slots, time_slots), "l")

  LFmax_vct <- rep(LFmax, time_slots)
  LFmin_vct <- -LFmax_vct

  lb_O <- round(pmax(G - LS - export_capacity, LFmin_vct), 2)
  ub_O <- round(pmin(G - LS + import_capacity, LFmax_vct), 2)

  Amat_cumsum <- cumsumMat
  if (direction == "forward") {
    if (time_horizon == time_slots) {
      horizonMat_cumsum <- matrix(0, time_slots, time_slots)
    } else {
      horizonMat_cumsum <- triangulate_matrix(matrix(1, time_slots, time_slots), "l", -time_horizon)
    }
    horizonMat_identity <- triangulate_matrix(triangulate_matrix(matrix(1, time_slots, time_slots), "l"), "u", -time_horizon)

    lb_cumsum <- rep(-Inf, time_slots)
    ub_cumsum <- cumsumMat %*% LF

    ub_shift <- horizonMat_identity %*% LF
    ub_O <- pmin(pmax(ub_shift, lb_O), ub_O)
  } else {
    horizonMat_cumsum <- triangulate_matrix(matrix(1, time_slots, time_slots), "l", time_horizon)
    horizonMat_identity <- triangulate_matrix(triangulate_matrix(matrix(1, time_slots, time_slots), "u"), "l", time_horizon)

    lb_cumsum <- cumsumMat %*% LF
    ub_cumsum <- horizonMat_cumsum %*% LF

    ub_shift <- horizonMat_identity %*% LF
    ub_O <- pmin(pmax(ub_shift, lb_O), ub_O)
  }

  Amat_energy <- matrix(1, ncol = time_slots)
  lb_energy <- sum(LF)
  ub_energy <- sum(LF)

  list(
    Amat_O = identityMat,
    lb_O = lb_O,
    ub_O = ub_O,
    Amat_cumsum = Amat_cumsum,
    lb_cumsum = lb_cumsum,
    ub_cumsum = ub_cumsum,
    Amat_energy = Amat_energy,
    lb_energy = lb_energy,
    ub_energy = ub_energy
  )
}


schedule_sessions_v2g <- function(sessions, setpoint, power_th = 0,
                                  include_log = FALSE, show_progress = FALSE) {
  if (show_progress) cli::cli_h1("Scheduling V2G sessions")

  log <- c()

  if (is.null(sessions) || nrow(sessions) == 0) {
    return(list(
      sessions = dplyr::tibble(),
      log = log
    ))
  }
  if (!all(c("datetime", "setpoint") %in% colnames(setpoint))) {
    stop("Error: `setpoint` does not contain all required variables
      (see Arguments description)")
  }

  resolution <- get_time_resolution(setpoint$datetime, units = "mins")
  dttm_tz <- lubridate::tz(setpoint$datetime)

  sessions_sch <- sessions %>%
    dplyr::filter(.data$ConnectionStartDateTime %in% setpoint$datetime) %>%
    dplyr::mutate(
      ChargingHours = .data$Energy / .data$Power
    )

  if (nrow(sessions_sch) == 0) {
    message("Error: no `sessions` for `setpoint$datetime` period")
    return(list(
      sessions = dplyr::tibble(),
      log = log
    ))
  }

  SOCini <- 0.5
  SOCmin <- 0.1
  sessions_expanded <- sessions_sch %>%
    evsim::expand_sessions(resolution = resolution) 
  sessions_expanded <- sessions_expanded %>%
    mutate(
      Power = NA,
      EnergyLeft = .data$EnergyRequired,
      Flexible = NA,
      Exploited = NA,
      BatteryCapacity = .data$EnergyRequired * 2, #  50% initial SOC
      EnergyInBattery = .data$BatteryCapacity * SOCini #  50% initial SOC
    ) %>%
    left_join(
      select(sessions_sch, any_of(c("Session", "ConnectionStartDateTime", "FlexibilityHours"))),
      by = "Session"
    )

  timeslot_dttm <- NULL
  if (show_progress) {
    cli::cli_progress_step(
      "Simulating timeslot: {timeslot_dttm}",
      spinner = TRUE
    )
  }

  for (timeslot in setpoint$datetime) {
    timeslot_dttm <- format(
      lubridate::as_datetime(timeslot, tz = dttm_tz), "%d/%m/%Y %H:%M"
    )

    if (include_log) {
      log <- c(
        log,
        paste(
          "\u2500\u2500", timeslot_dttm,
          # "\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
          # \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
          # \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
          # \u2500\u2500\u2500\u2500\u2500\u2500"
          "\u2500\u2500"
        )
      )
    }

    if (show_progress) {
      cli::cli_progress_update()
    }

    # Filter sessions that are connected during this time slot
    # and calculate:
    #   - `PowerTimeslot`: average charging power in the timeslot.
    #     The `PowerTimeslot` is the `PowerNominal` in all time slots,
    #     except when sessions finish charging in the middle of a time slot.
    #   - `MinEnergyLeft`: minimum energy that must be charged to fulfill
    #       the `energy_min` requirement.
    #   - `PossibleEnergyRest`: energy that can be charged at nominal power
    #       during the rest of connection hours (excluding this time slot).
    idx_timeslot <- sessions_expanded$Timeslot == timeslot
    if (!any(idx_timeslot)) {
      next
    }
    sessions_timeslot <- sessions_expanded[idx_timeslot, ]

    sessions_timeslot <- sessions_timeslot %>%
      mutate(
        PowerTimeslot = pmin(
          .data$EnergyLeft / (resolution / 60), .data$PowerNominal
        ),
        PossibleEnergyRest = .data$PowerNominal *
          pmax(.data$ConnectionHoursLeft - resolution / 60, 0)
      )

    # Flexibility definition for V2G ---------------------------
    # The minimum power that can be charged in this time slot is
    # lower than the nominal power. The minimum power is defined by:
    #  - The minimum energy that must be charged in the time slot, defined by
    #      - The minimum State-of-Charge (`SOCmin`)
    #      - The energy that must be charged to reach 100% SOC at departure
    #      - The energy that can be charged at nominal power the rest of connection hours

    sessions_timeslot <- sessions_timeslot %>%
      mutate(
        MinEnergyTimeslot = pmax( # Can be negative (discharging)
          .data$EnergyLeft - .data$PossibleEnergyRest, # Energy to reach full SOC
          .data$BatteryCapacity * SOCmin - .data$EnergyInBattery #  Energy to reach min SOC
        ),
        MinPowerTimeslot = pmax( # Can be negative (discharging)
          .data$MinEnergyTimeslot / (resolution / 60),
          -.data$PowerNominal # Maximum discharging power
        ),
        Flexible = ifelse( # Added error tolerance
          (.data$EnergyLeft > 0.025) &
            (.data$PowerTimeslot - .data$MinPowerTimeslot > 0.1),
          TRUE,
          FALSE
        )
      )

    # Flexibility exploitation ------------------------------------------------

    # Set `Exploited` to `FALSE` by default
    sessions_timeslot$Exploited <- FALSE

    # Power demand in this time slot
    sessions_timeslot_power <- sum(sessions_timeslot$PowerTimeslot)

    # Setpoint of power for this time slot
    setpoint_power_timeslot <- setpoint$setpoint[
      setpoint$datetime == timeslot
    ]
    if (setpoint_power_timeslot > 0) {
      setpoint_power_timeslot <- setpoint_power_timeslot * (1 + power_th)
    } else {
      setpoint_power_timeslot <- setpoint_power_timeslot * (1 - power_th)
    }

    # Flexibility requirement
    flex_req <- round(
      sessions_timeslot_power - setpoint_power_timeslot, 2
    )

    # If demand should be reduced
    if (flex_req > 0) {
      if (include_log) {
        log_message <- c(
          paste("\u2139 Flexibility requirement of", flex_req, "kW"),
          paste("\u2139", nrow(sessions_timeslot), "potentially flexible sessions")
        )

        # message(log_message)
        log <- c(
          log,
          log_message
        )
      }

      # Power flexibility (V2G) ----------------------------------------------------------------

      # All `Flexible` sessions are `Exploited` with "V2G"
      sessions_timeslot$Exploited[sessions_timeslot$Flexible] <- TRUE

      # Get the BAU power demand from curtailable sessions
      curtailable_sessions_power <- sum(
        sessions_timeslot$PowerTimeslot[sessions_timeslot$Flexible]
      )

      # Power factor that would be required
      power_factor <- (curtailable_sessions_power - flex_req) / curtailable_sessions_power

      # Update the charging power of curtailed sessions
      sessions_timeslot$Power[sessions_timeslot$Exploited] <- pmax(
        sessions_timeslot$PowerTimeslot[sessions_timeslot$Exploited] * power_factor,
        sessions_timeslot$MinPowerTimeslot[sessions_timeslot$Exploited]
      )

      # Update the charging power of sessions that are NOT curtailed
      sessions_timeslot$Power[!sessions_timeslot$Exploited] <-
        sessions_timeslot$PowerTimeslot[!sessions_timeslot$Exploited]

      if (include_log) {
        flex_provided <- round(curtailable_sessions_power -
          sum(sessions_timeslot$Power[sessions_timeslot$Exploited]), 2)
        if (flex_provided < flex_req) {
          log_message <- paste0(
            "\u2716 Not enough flexibility available (", flex_provided, " kW)"
          )
          # message(log_message)
          log <- c(
            log,
            log_message
          )
        }
      }

      # Update flexibility requirement
      flex_req <- round(flex_req - (curtailable_sessions_power - curtailable_sessions_power * power_factor), 2)
    } else {
      # No flexibility required: charge all sessions
      sessions_timeslot$Power <- sessions_timeslot$PowerTimeslot
    }

    # Set `Exploited` to `NA` if sessions are not `Flexible`
    sessions_timeslot$Exploited[!sessions_timeslot$Flexible] <- NA

    # Update data set ----------------------------------------------------------------

    # For every session update in `sessions_expanded` table
    #   1. The charging power during THIS TIME SLOT (session id == `ID`)
    #       If energy left is less than the energy that can be charged in this
    #       time slot, then only charge the energy left, so the AVERAGE charging
    #       power will be calculated with the energy left (lower than nominal power)
    #   2. The energy left and the available flexibility for the FOLLOWING time
    #       slots of THIS SESSION (session id == `Session`)
    for (s in seq_len(nrow(sessions_timeslot))) {
      # Update `Power`
      sessions_expanded$Power[
        sessions_expanded$ID == sessions_timeslot$ID[s]
      ] <- sessions_timeslot$Power[s]
      # Update `Flexible`
      sessions_expanded$Flexible[
        sessions_expanded$ID == sessions_timeslot$ID[s]
      ] <- sessions_timeslot$Flexible[s]
      # Update `Exploited`
      sessions_expanded$Exploited[
        sessions_expanded$ID == sessions_timeslot$ID[s]
      ] <- sessions_timeslot$Exploited[s]


      # If there are more time slots afterwards, update `EnergyLeft`
      session_after_idx <- (sessions_expanded$Session == sessions_timeslot$Session[s]) &
        (sessions_expanded$ID > sessions_timeslot$ID[s])

      if (length(session_after_idx) > 0) {
        session_energy <- sessions_timeslot$Power[s] * resolution / 60

        # Update `EnergyLeft`
        # We assume that the session is charging the whole timeslot, so:
        # `ChargingHours = resolution/60`
        session_energy_left <- sessions_timeslot$EnergyLeft[s] - session_energy
        sessions_expanded$EnergyLeft[session_after_idx] <- session_energy_left

        # Update `EnergyInBattery`
        session_energy_in <- sessions_timeslot$EnergyInBattery[s] + session_energy
        sessions_expanded$EnergyInBattery[session_after_idx] <- session_energy_in
      } else {
        session_energy_left <- 0
        session_energy_in <- 0
      }
    }

    # Log message ----------------------------------------------------------------
    if (include_log) {
      exploited_sessions <- sessions_timeslot %>% filter(.data$Exploited)

      for (s in seq_len(nrow(exploited_sessions))) {
        power_reduction <- round(exploited_sessions$PowerTimeslot[s] - exploited_sessions$Power[s], 2)
        if (power_reduction > 0) {
          pct_power_reduction <- round(power_reduction / exploited_sessions$PowerTimeslot[s] * 100, 1)
          log_message <- paste0(
            "| \u2714 Session ", exploited_sessions$Session[s],
            " provides ", power_reduction, " kW of flexibility (",
            pct_power_reduction, "% power reduction)"
          )
        } else {
          log_message <- NULL
        }

        log <- c(
          log,
          log_message
        )
      }
    }
  }

  if (show_progress) cli::cli_progress_step("Cleaning data set")

  # Update the sessions data set with all variables from the original data set
  sessions_segmented <- sessions_expanded %>%
    select(any_of(c(
      evsim::sessions_feature_names, "Timeslot", "EnergyLeft",
      "ConnectionHoursLeft", "Flexible", "Exploited"
    ))) %>%
    dplyr::mutate(
      ConnectionStartDateTime = .data$Timeslot,
      ConnectionEndDateTime = .data$Timeslot + lubridate::minutes(resolution),
      ChargingStartDateTime = .data$Timeslot,
      ChargingEndDateTime = .data$Timeslot + lubridate::minutes(resolution),
      ConnectionHours = resolution / 60,
      ChargingHours = ifelse(.data$Power != 0, resolution / 60, 0),
      Energy = .data$Power * (.data$ConnectionHours)
    ) %>%
    summarise_by_segment() %>%
    mutate_if(is.numeric, round, 2)

  sessions_sch_flex <- sessions_sch %>%
    select("Session", !any_of(names(sessions_segmented))) %>%
    left_join(sessions_segmented, by = "Session") %>%
    select(any_of(names(sessions_sch)), "Flexible", "Exploited", "EnergyLeft", "ConnectionHoursLeft")


  list(
    sessions = sessions_sch_flex,
    log = log
  )
}
