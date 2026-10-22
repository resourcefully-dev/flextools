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
#' @param energy_min numeric, minimum allowed ratio (between 0 and 1) of required energy.
#' Every session charges at least this share of the energy it requires, net of
#' discharge. The scheduler never discharges or curtails a session below it,
#' and the setpoint optimization may drop energy down to it - and no further -
#' when the grid capacity cannot fit `energy_max`. When even this minimum does
#' not fit, the grid capacity is relaxed only as far as the minimum-energy
#' profile needs.
#' @param energy_max numeric, maximum allowed ratio (between 0 and 1) of required energy.
#' Every session charges at most this share of the energy it requires, net of
#' discharge (energy discharged, grossed up by `discharge_eff`, has to be
#' charged back on top), and the setpoint optimization targets this share for
#' the responsive sessions. Must be higher than 0 and at least `energy_min`.
#' @param discharge_eff numeric, discharge efficiency (0, 1], applied to energy
#'   losses when discharging. For example, 0.9 means that for every 1 kWh
#'   discharged, 1/0.9 kWh must be recharged.
#' @param charging_power_min numeric, minimum power in kW (default 0), for
#'   charging and for discharging alike: a responsive session charges at
#'   `charging_power_min` or more, discharges at `charging_power_min` or more
#'   (e.g. "discharge only from 4 kW upwards"), or does nothing. When the SoC
#'   floor or the energy deadline would only allow a smaller discharge (say
#'   -2 kW against a 4 kW minimum), the session does not discharge in that slot.
#'   The one exception is a session's last charging slot, where it may charge
#'   less than the minimum because it only needs that much to finish (the tail
#'   of its charge). A session whose nominal `Power` is below
#'   `charging_power_min` is not flexible: it is never responsive, charges as
#'   uncontrolled, never discharges, and the setpoint carries its load as
#'   fixed. Up to version 1.12.0 a value below 1 was a share of the nominal
#'   power; it is now always kW.
#' @param soc_min numeric, State-of-Charge floor of every V2G session, as a
#'   fraction of its battery capacity (default 0.2, `0 <= soc_min < 1`). A
#'   session never discharges below it. A session that arrives below it
#'   (`StateOfCharge < soc_min`) charges up to the floor first, as fast as it
#'   needs to (at nominal power when necessary), and can neither be curtailed
#'   nor discharge before it reaches the floor.
#'
#' @details
#' The `sessions` tibble can carry a `StateOfCharge` column, the State-of-Charge
#' of each vehicle at arrival as a fraction (`0 <= StateOfCharge < 1`); when it
#' is absent every session arrives at 0.5. Each session's `Energy` takes its
#' battery from `StateOfCharge` to full, so its battery capacity is
#' `Energy / (1 - StateOfCharge)` (twice the energy at 0.5). With `energy_max`
#' below 1 the session charges only that share of its energy and leaves below
#' full, at `StateOfCharge + energy_max * (1 - StateOfCharge)`.
#'
#' The setpoint plans charge and discharge only while the responsive sessions
#' are connected, within their nominal power in both directions (and
#' `load_capacity`), and keeps the energy owed to every session by its
#' departure: the cumulative flexible load never drops below the `energy_min`
#' share of the energy of the sessions that have left, minus what the connected
#' sessions can still discharge down to `soc_min`.
#'
#' @return list with setpoints, sessions and demand similar to
#'   `smart_charging()`. Scheduling is still charging-only while
#'   the V2G-specific session logic is under development.
#' @export
#'
smart_v2g <- function(
  sessions,
  opt_data,
  opt_objective = "grid",
  window_days,
  window_start_hour,
  responsive = NULL,
  power_th = 0,
  charging_power_min = 0,
  energy_min = 1,
  energy_max = 1,
  discharge_eff = 1,
  include_log = FALSE,
  show_progress = FALSE,
  lambda = 0,
  soc_min = 0.2
) {
  if (show_progress) {
    cli::cli_h1("Set up (V2G)")
  }

  if (show_progress) {
    cli::cli_progress_step("Checking parameters")
  }

  if (is.null(sessions) || nrow(sessions) == 0) {
    stop("Error: `sessions` parameter is empty.")
  }

  sessions_basic_vars <- c(
    "Session",
    "Timecycle",
    "Profile",
    "ConnectionStartDateTime",
    "ConnectionHours",
    "Power",
    "Energy"
  )
  if (!all(sessions_basic_vars %in% colnames(sessions))) {
    stop(
      "Error: `sessions` does not contain all required variables (see Arguments description)"
    )
  }

  check_energy_ratios(energy_min, energy_max)
  check_charging_power_min(charging_power_min)
  check_soc_min(soc_min)
  sessions <- set_state_of_charge(sessions)

  if (is.null(opt_data)) {
    stop("Error: `opt_data` parameter is empty.")
  }
  if (!("datetime" %in% colnames(opt_data))) {
    stop("Error: `opt_data` does not contain `datetime` variable")
  }
  if (!any(sessions$ConnectionStartDateTime %in% opt_data$datetime)) {
    stop(
      "Error: `sessions` do not charge during `datetime` period in `opt_data`"
    )
  }

  if (opt_objective == "none") {
    if (
      !any(
        c(
          unique(sessions$Profile),
          "grid_capacity",
          "import_capacity",
          "export_capacity"
        ) %in%
          names(opt_data)
      )
    ) {
      stop(
        'Error: when `opt_objective` = "none" you must set a setpoint in `opt_data` with grid capacity or a user profile name.'
      )
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

  has_explicit_export_capacity <- any(
    c("grid_capacity", "export_capacity") %in% names(opt_data)
  )

  opt_data$flexible <- 0
  opt_data <- check_optimization_data(opt_data, opt_objective)

  # V2G still uses a signed net-power formulation for the grid objective. In
  # that formulation a negative `import_capacity` means that the site is forced
  # to export power. If the user did not provide an explicit export capacity,
  # the shared optimisation defaults would set `export_capacity = 0`, which
  # makes the V2G bounds contradictory (`lb > ub`) as soon as a negative import
  # limit appears. To preserve the original V2G semantics, missing export
  # capacity remains unconstrained in those signed-capacity windows.
  if (
    !has_explicit_export_capacity &&
      any(opt_data$import_capacity < 0, na.rm = TRUE)
  ) {
    opt_data$export_capacity <- Inf
  }

  if (show_progress) {
    cli::cli_progress_step("Defining optimisation windows")
  }

  dttm_seq <- opt_data$datetime
  time_resolution <- timefully::get_time_resolution(dttm_seq, units = "mins")
  sessions <- sessions %>%
    evsim::adapt_charging_features(time_resolution = time_resolution)

  flex_windows_idx <- get_flex_windows(
    dttm_seq,
    window_days,
    window_start_hour
  )

  if (show_progress) {
    cli::cli_progress_step("Calculating EV demand")
  }
  profiles_demand <- evsim::get_demand(sessions, dttm_seq)

  if (show_progress) {
    cli::cli_h1("Smart V2G")
  }

  if (show_progress) {
    cli::cli_progress_step("Setting responsiveness")
  }

  windows_data <- purrr::map(
    flex_windows_idx$flex_idx,
    function(flex_idx) {
      list(
        sessions_window = sessions %>%
          dplyr::filter(
            .data$ChargingStartDateTime >= dttm_seq[flex_idx[1]],
            .data$ChargingStartDateTime <= dttm_seq[flex_idx[length(flex_idx)]]
          ) %>%
          set_responsive(
            dttm_seq[flex_idx],
            responsive,
            time_resolution,
            charging_power_min
          ),
        profiles_demand = profiles_demand[flex_idx, ],
        opt_data = opt_data[flex_idx, ]
      )
    }
  )

  if (show_progress) {
    cli::cli_progress_step("Defining V2G setpoints")
  }

  setpoints_lst <- get_setpoints_v2g_parallel(
    windows_data,
    opt_objective,
    lambda,
    energy_min,
    energy_max,
    soc_min
  )

  if (show_progress) {
    cli::cli_progress_step("Scheduling V2G sessions")
  }

  scheduling_lst <- smart_v2g_window_parallel(
    windows_data,
    setpoints_lst,
    power_th,
    charging_power_min,
    energy_min,
    energy_max,
    discharge_eff,
    include_log,
    soc_min
  )

  if (show_progress) {
    cli::cli_progress_step("Cleaning data set")
  }

  setpoints <- list_rbind(setpoints_lst)

  setpoints_opt <- profiles_demand
  opt_dttm_idx <- setpoints_opt$datetime %in% setpoints$datetime
  setpoints_opt[opt_dttm_idx, names(setpoints)] <- setpoints

  sessions_considered <- purrr::map(
    scheduling_lst,
    ~ .x$sessions
  ) %>%
    list_rbind()

  if (nrow(sessions_considered) > 0) {
    sessions_not_considered <- sessions[
      !(sessions$Session %in% sessions_considered$Session),
    ]
  } else {
    sessions_not_considered <- sessions %>%
      dplyr::mutate(
        Responsive = NA,
        Flexible = NA,
        Exploited = NA
      )
  }

  sessions_opt <- dplyr::bind_rows(
    sessions_not_considered,
    sessions_considered
  ) %>%
    dplyr::select(any_of(names(sessions)), dplyr::everything()) %>%
    dplyr::mutate(
      Session = factor(.data$Session, levels = sessions$Session)
    ) %>%
    dplyr::arrange(
      .data$Session,
      .data$ConnectionStartDateTime,
      .data$ConnectionEndDateTime
    ) %>%
    dplyr::mutate(Session = as.character(.data$Session)) %>%
    dplyr::distinct()

  demand <- purrr::map(
    scheduling_lst,
    ~ .x$demand
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
#' The setpoint plans charge and discharge within the physical envelope of the
#' responsive sessions (see `get_flex_envelope()`): at most their nominal power
#' while connected, in both directions, and a cumulative load that never drops
#' below the energy owed to the departed sessions minus what the connected ones
#' can still discharge down to `soc_min`.
#'
#' @param energy_min,energy_max numeric, minimum and maximum share (between 0
#'   and 1) of each profile's energy the setpoint must carry, net of discharge.
#'   See [smart_v2g()].
#' @param soc_min numeric, SoC floor of the sessions. See [smart_v2g()].
#'
#' @keywords internal
#'
get_setpoints_v2g <- function(
  sessions_window,
  opt_data,
  profiles_demand,
  opt_objective,
  lambda,
  energy_min = 1,
  energy_max = 1,
  soc_min = 0.2
) {
  # Both ratios are shares of the responsive sessions' own demand `LF`; the
  # non-responsive demand `L_fixed_prof` is added back untouched below.
  energy_ratio <- c(energy_min, energy_max)
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
  setpoints <- profiles_demand

  for (profile in opt_profiles) {
    if (profile %in% colnames(opt_data)) {
      setpoints[[profile]] <- opt_data[[profile]]
    } else {
      sessions_window_prof_flex <- sessions_window %>%
        dplyr::filter(.data$Profile == profile & .data$Responsive)

      non_responsive_sessions <- sessions_window %>%
        dplyr::filter(
          .data$Profile == profile &
            (!.data$Responsive | is.na(.data$Responsive))
        )

      if (nrow(non_responsive_sessions) > 0) {
        L_fixed_prof <- non_responsive_sessions %>%
          evsim::get_demand(dttm_seq = dttm_seq, by = "Profile") %>%
          dplyr::pull(!!rlang::sym(profile))
      } else {
        L_fixed_prof <- rep(0, length(dttm_seq))
      }

      # The optimization flexible load is the load of the responsive sessions
      LF <- setpoints[[profile]] - L_fixed_prof

      # Static load
      # Here we consider `setpoints` instead of `profiles_demand` because we
      # update it in every iteration (optimization)
      L_others <- setpoints %>%
        dplyr::select(-any_of(c(profile, "datetime"))) %>%
        rowSums()
      if (length(L_others) == 0) {
        L_others <- rep(0, length(dttm_seq))
      }
      LS <- L_fixed + L_others + L_fixed_prof

      # sessions_window_prof_flex$ConnectionEndDateTime[
      #   (sessions_window_prof_flex$ConnectionEndDateTime >=
      #     dttm_seq[length(dttm_seq)])
      # ] <- dttm_seq[length(dttm_seq)]

      if (opt_objective != "none") {
        window_prof_dttm <- c(
          min(sessions_window_prof_flex$ConnectionStartDateTime),
          min(
            max(sessions_window_prof_flex$ConnectionEndDateTime),
            dttm_seq[length(dttm_seq)]
          )
        )
        opt_idxs <- (dttm_seq >= window_prof_dttm[1]) &
          (dttm_seq <= window_prof_dttm[2])

        envelope <- get_flex_envelope(
          set_state_of_charge(sessions_window_prof_flex),
          dttm_seq = dttm_seq,
          profile = profile,
          load_capacity = opt_data$load_capacity,
          energy_min = energy_min,
          soc_min = soc_min
        )
        LFmax_prof <- envelope$LFmax[opt_idxs]
        cumsum_min <- (envelope$owed - envelope$reserve)[opt_idxs]
        # The minimum-squared-net-power objectives discharge only to cut
        # import (see v2g_discharge_floor()).
        discharge_floor <- v2g_discharge_floor(
          G = opt_data$production[opt_idxs],
          LS = LS[opt_idxs],
          import_capacity = opt_data$import_capacity[opt_idxs]
        )

        if (opt_objective == "grid") {
          O <- demand_grid_v2g_window(
            G = opt_data$production[opt_idxs],
            LF = LF[opt_idxs],
            LS = LS[opt_idxs],
            direction = "forward",
            time_horizon = NULL,
            LFmax = LFmax_prof,
            import_capacity = opt_data$import_capacity[opt_idxs],
            export_capacity = opt_data$export_capacity[opt_idxs],
            lambda = lambda,
            energy_ratio = energy_ratio,
            cumsum_min = cumsum_min,
            LFmin = discharge_floor
          )
        } else if (opt_objective == "cost") {
          message(
            "Cost-based optimisation is not yet implemented for V2G; reusing grid objective."
          )
          O <- demand_grid_v2g_window(
            G = opt_data$production[opt_idxs],
            LF = LF[opt_idxs],
            LS = LS[opt_idxs],
            direction = "forward",
            time_horizon = NULL,
            LFmax = LFmax_prof,
            import_capacity = opt_data$import_capacity[opt_idxs],
            export_capacity = opt_data$export_capacity[opt_idxs],
            lambda = lambda,
            energy_ratio = energy_ratio,
            cumsum_min = cumsum_min,
            LFmin = discharge_floor
          )
        } else if (opt_objective == "capacity") {
          O <- capacity_v2g_window(
            G = opt_data$production[opt_idxs],
            LF = LF[opt_idxs],
            LS = LS[opt_idxs],
            direction = "forward",
            time_horizon = NULL,
            LFmax = LFmax_prof,
            import_capacity = opt_data$import_capacity[opt_idxs],
            export_capacity = opt_data$export_capacity[opt_idxs],
            lambda = lambda,
            energy_ratio = energy_ratio,
            cumsum_min = cumsum_min
          )
        } else if (is.numeric(opt_objective)) {
          message(
            "Combined optimisation is not yet implemented for V2G; reusing grid objective."
          )
          O <- demand_grid_v2g_window(
            G = opt_data$production[opt_idxs],
            LF = LF[opt_idxs],
            LS = LS[opt_idxs],
            direction = "forward",
            time_horizon = NULL,
            LFmax = LFmax_prof,
            import_capacity = opt_data$import_capacity[opt_idxs],
            export_capacity = opt_data$export_capacity[opt_idxs],
            lambda = lambda,
            energy_ratio = energy_ratio,
            cumsum_min = cumsum_min,
            LFmin = discharge_floor
          )
        } else {
          stop("Error: `opt_objective` not valid")
        }

        setpoints[[profile]][opt_idxs] <- O + L_fixed_prof[opt_idxs]
      } else if ("import_capacity" %in% colnames(opt_data)) {
        # Calculate available capacity for this profile
        capacity_available <- pmax(
          opt_data$import_capacity +
            opt_data$production -
            (opt_data$static + L_others),
          -opt_data$export_capacity # Allow negative power
        )

        setpoints[[profile]] <- capacity_available
      } else {
        stop(paste(
          "Error: `opt_objective` is 'none' but no setpoint or grid capacity
            is configured in `opt_data` for Profile",
          profile
        ))
      }
    }
  }

  return(setpoints)
}


get_setpoints_v2g_parallel <- function(
  windows_data,
  opt_objective,
  lambda,
  energy_min = 1,
  energy_max = 1,
  soc_min = 0.2
) {
  reset_message_once()

  purrr::map(
    windows_data,
    ~ get_setpoints_v2g(
      sessions_window = .x$sessions_window,
      opt_data = .x$opt_data,
      profiles_demand = .x$profiles_demand,
      opt_objective = opt_objective,
      lambda = lambda,
      energy_min = energy_min,
      energy_max = energy_max,
      soc_min = soc_min
    )
  )
}


smart_v2g_window <- function(
  sessions_window,
  profiles_demand,
  setpoints,
  power_th = 0,
  charging_power_min = 0,
  energy_min = 1,
  energy_max = 1,
  discharge_eff = 1,
  include_log = FALSE,
  soc_min = 0.2
) {
  if (nrow(setpoints) == 0) {
    return(list(
      sessions = sessions_window,
      demand = profiles_demand,
      setpoints = profiles_demand,
      log = list()
    ))
  }

  dttm_seq <- setpoints$datetime
  log <- list()
  log_window_name <- as.character(date(dttm_seq[1]))
  log[[log_window_name]] <- character(0) # In the `log` object even though `include_log = FALSE`

  if (nrow(sessions_window) == 0) {
    return(list(
      sessions = sessions_window,
      demand = profiles_demand,
      setpoints = setpoints,
      log = log
    ))
  }

  window_profiles <- unique(sessions_window$Profile)

  sessions_window_flex <- sessions_window %>%
    dplyr::filter(.data$Responsive)

  non_responsive_sessions <- sessions_window %>%
    dplyr::filter(!.data$Responsive | is.na(.data$Responsive))

  if (nrow(non_responsive_sessions) > 0) {
    L_fixed_total <- non_responsive_sessions %>%
      evsim::get_demand(dttm_seq = dttm_seq, by = "Profile") %>%
      dplyr::select(-"datetime") %>%
      rowSums()
  } else {
    L_fixed_total <- rep(0, length(dttm_seq))
  }

  setpoint_total <- setpoints %>%
    dplyr::select(any_of(window_profiles)) %>%
    rowSums()
  if (length(setpoint_total) == 0) {
    setpoint_total <- rep(0, length(dttm_seq))
  }

  setpoint_flex <- dplyr::tibble(
    datetime = dttm_seq,
    setpoint = setpoint_total - L_fixed_total
  )

  results_log <- character(0)
  if (nrow(sessions_window_flex) > 0) {
    results <- schedule_sessions_v2g(
      sessions = sessions_window_flex,
      setpoint = setpoint_flex,
      charging_power_min = charging_power_min,
      energy_min = energy_min,
      energy_max = energy_max,
      discharge_eff = discharge_eff,
      include_log = include_log,
      show_progress = FALSE,
      power_th = power_th,
      soc_min = soc_min
    )
    sessions_window_flex_final <- results$sessions
    results_log <- results$log
  } else {
    sessions_window_flex_final <- dplyr::tibble()
  }

  sessions_window_final <- dplyr::bind_rows(
    sessions_window_flex_final,
    non_responsive_sessions
  )

  if (nrow(sessions_window_final) > 0) {
    sessions_window_final_demand <- evsim::get_demand(
      sessions_window_final,
      dttm_seq = dttm_seq,
      by = "Profile"
    )
    profile_cols <- intersect(window_profiles, names(profiles_demand))
    profiles_demand[profile_cols] <-
      sessions_window_final_demand[profile_cols]
  }

  sessions_considered <- sessions_window_final

  if (include_log) {
    log[[log_window_name]] <- results_log
  }

  list(
    sessions = sessions_considered,
    demand = profiles_demand,
    setpoints = setpoints,
    log = log
  )
}


smart_v2g_window_parallel <- function(
  windows_data,
  setpoints_lst,
  power_th,
  charging_power_min,
  energy_min,
  energy_max,
  discharge_eff,
  include_log,
  soc_min = 0.2
) {
  if (
    !requireNamespace("mirai", quietly = TRUE) ||
      !requireNamespace("carrier", quietly = TRUE)
  ) {
    scheduling_lst <- purrr::map2(
      windows_data,
      setpoints_lst,
      \(x, y) {
        smart_v2g_window(
          sessions_window = x$sessions_window,
          profiles_demand = x$profiles_demand,
          setpoints = y,
          power_th = power_th,
          charging_power_min = charging_power_min,
          energy_min = energy_min,
          energy_max = energy_max,
          discharge_eff = discharge_eff,
          include_log = include_log,
          soc_min = soc_min
        )
      }
    )
  } else {
    scheduling_lst <- purrr::map2(
      windows_data,
      setpoints_lst,
      in_parallel(
        \(x, y) {
          smart_v2g_window(
            sessions_window = x$sessions_window,
            profiles_demand = x$profiles_demand,
            setpoints = y,
            power_th = power_th,
            charging_power_min = charging_power_min,
            energy_min = energy_min,
            energy_max = energy_max,
            discharge_eff = discharge_eff,
            include_log = include_log,
            soc_min = soc_min
          )
        },
        smart_v2g_window = smart_v2g_window,
        power_th = power_th,
        charging_power_min = charging_power_min,
        energy_min = energy_min,
        energy_max = energy_max,
        discharge_eff = discharge_eff,
        include_log = include_log,
        soc_min = soc_min
      )
    )
  }
  return(scheduling_lst)
}


#' Capacity optimisation with bidirectional flexibility (single window)
#'
#' Mirrors `demand_capacity_window()` but falls back to
#' `demand_grid_v2g_window()` so that discharge is considered when
#' resolving capacity violations.
#'
#' The fleet discharges only to cut import (see `v2g_discharge_floor()`),
#' also in the grid LP that places the shifted energy.
#'
#' @param energy_ratio numeric vector `c(min, max)`, share of the flexible
#'   load's energy the optimized load must carry. See
#'   `demand_capacity_window()`.
#'
#' @keywords internal
#'
capacity_v2g_window <- function(
  G,
  LF,
  LS,
  direction,
  time_horizon,
  LFmax,
  import_capacity,
  export_capacity,
  lambda = 0,
  energy_ratio = c(1, 1),
  cumsum_min = NULL
) {
  G <- round(as.numeric(G), 2)
  LF <- round(as.numeric(LF), 2)
  LS <- round(as.numeric(LS), 2)

  time_slots <- length(LF)
  if (is.null(time_horizon)) {
    time_horizon <- time_slots
  }
  LFmax <- as.numeric(rep_len(LFmax, time_slots))
  import_capacity <- as.numeric(rep_len(import_capacity, time_slots))
  export_capacity <- as.numeric(rep_len(export_capacity, time_slots))
  energy_ratio <- demand_check_energy_ratio(
    energy_ratio,
    direction,
    time_horizon,
    time_slots
  )
  tol <- optimization_solution_tolerance()

  discharge_floor <- v2g_discharge_floor(G, LS, import_capacity)

  # As in demand_capacity_window(): an energy target below the requirement is a
  # uniform scaling of the whole profile before the slice is chosen, and the
  # floor stays relative to the ORIGINAL profile.
  LF_target <- if (energy_ratio[2] < 1) round(LF * energy_ratio[2], 2) else LF
  energy_lb <- if (energy_ratio[1] < 1) {
    round(energy_ratio[1] * sum(LF), 2)
  } else {
    NULL
  }

  slice_solution <- select_capacity_slice(
    G = G,
    LF = LF_target,
    LS = LS,
    direction = direction,
    time_horizon = time_horizon,
    LFmax = LFmax,
    import_capacity = import_capacity,
    export_capacity = export_capacity,
    energy_lb = energy_lb,
    cumsum_min = cumsum_min
  )

  if (is.null(slice_solution)) {
    # The capacity slice LP is infeasible under the true caps, even after
    # dropping energy down to the floor. Rather than dropping the grid
    # constraints entirely, relax each cap only as far as a reference profile
    # already needs (import LS + LF_ref - G, export G - LS - LF_ref). Slots
    # within their caps keep the true capacity, so the result can never be
    # worse than that profile. The reference is the original, unshifted
    # profile - or, when the energy range admits less, the original profile
    # scaled down to the minimum energy (see demand_capacity_window()).
    # demand_grid_v2g_window() (via solve_optimization_window_v2g) is feasible
    # by construction with these caps because O = LF_ref is a feasible point.
    LF_ref <- LF
    fallback_ratio <- c(1, 1)
    if (energy_ratio[1] < 1 && sum(LF) > 0) {
      LF_min <- round(LF * energy_ratio[1], 2)
      if (sum(LF_min) > 0) {
        LF_ref <- LF_min
        fallback_ratio <- rep(min(sum(LF_min) / sum(LF), 1), 2)
      }
    }
    import_cap_relaxed <- pmax(import_capacity, LS + LF_ref - G) + tol
    export_cap_relaxed <- pmax(export_capacity, G - LS - LF_ref) + tol
    if (identical(LF_ref, LF)) {
      message_once(
        "\u26a0\ufe0f Optimization warning: optimization not feasible in some windows. Relaxing grid capacity to the original profile in the affected windows."
      )
    } else {
      message_once(
        "\u26a0\ufe0f Optimization warning: even the minimum energy does not fit under the grid capacity in some windows. Relaxing grid capacity to the minimum-energy profile in the affected windows."
      )
    }
    return(
      demand_grid_v2g_window(
        G = G,
        LF = LF,
        LS = LS,
        direction = direction,
        time_horizon = time_horizon,
        LFmax = LFmax,
        import_capacity = import_cap_relaxed,
        export_capacity = export_cap_relaxed,
        lambda = lambda,
        energy_ratio = fallback_ratio,
        cumsum_min = cumsum_min,
        LFmin = discharge_floor
      )
    )
  }

  moved_slice <- slice_solution$slice
  if (all(moved_slice == 0)) {
    return(LF_target)
  }

  # The slice LP decided how much of the removed energy is re-added; the grid
  # LP that places it carries exactly that energy (see
  # demand_capacity_window()).
  moved_energy <- sum(moved_slice)
  added_energy <- sum(slice_solution$added)
  fixed_load <- round(LF_target - moved_slice, 2)
  if (added_energy <= tol) {
    return(fixed_load)
  }
  slice_ratio <- if (added_energy < moved_energy - tol) {
    rep(added_energy / moved_energy, 2)
  } else {
    c(1, 1)
  }

  optimized_slice <- demand_grid_v2g_window(
    G = G,
    LF = moved_slice,
    LS = LS + fixed_load,
    direction = direction,
    time_horizon = time_horizon,
    LFmax = round(pmax(LFmax - fixed_load, 0), 2),
    import_capacity = import_capacity,
    export_capacity = export_capacity,
    lambda = lambda,
    energy_ratio = slice_ratio,
    cumsum_min = if (is.null(cumsum_min)) {
      NULL
    } else {
      rep_len(as.numeric(cumsum_min), time_slots) - cumsum(fixed_load)
    },
    LFmin = discharge_floor - fixed_load
  )

  round(fixed_load + as.numeric(optimized_slice), 2)
}


#' Minimisation of net power with bidirectional flexibility (single window)
#'
#' @param energy_ratio numeric vector `c(min, max)`, share of the flexible
#'   load's energy the optimized load must carry (net of discharge). `c(1, 1)`
#'   (default) preserves the energy exactly; a range lets the optimizer drop
#'   energy - down to `min` - when the grid capacity cannot fit `max`.
#' @param LFmin numeric vector (or `NULL`, default), per-slot floor of the
#'   optimized load on top of `-LFmax`. `get_setpoints_v2g()` and
#'   `capacity_v2g_window()` pass `v2g_discharge_floor()`, so the fleet never
#'   discharges where that only adds export.
#'
#' @keywords internal
#'
demand_grid_v2g_window <- function(
  G,
  LF,
  LS,
  direction,
  time_horizon,
  LFmax,
  import_capacity,
  export_capacity,
  lambda = 0,
  energy_ratio = c(1, 1),
  cumsum_min = NULL,
  LFmin = NULL
) {
  time_slots <- length(LF)
  identityMat <- diag(time_slots)

  P <- 2 * identityMat * (1 + lambda)
  q <- 2 * (LS - G - lambda * LF)

  solve_optimization_window_v2g(
    G,
    LF,
    LS,
    direction,
    time_horizon,
    LFmax,
    import_capacity,
    export_capacity,
    P,
    q,
    energy_ratio,
    cumsum_min,
    LFmin
  )
}


solve_optimization_window_v2g <- function(
  G,
  LF,
  LS,
  direction,
  time_horizon,
  LFmax,
  import_capacity,
  export_capacity,
  P,
  q,
  energy_ratio = c(1, 1),
  cumsum_min = NULL,
  LFmin = NULL
) {
  G <- round(G, 2)
  LF <- round(LF, 2)
  LS <- round(LS, 2)

  time_slots <- length(G)
  if (is.null(time_horizon)) {
    time_horizon <- time_slots
  }

  # Energy the optimized load must carry: by default exactly the profile's own
  # (`energy_ratio = c(1, 1)`), otherwise the range `[min, max] * sum(LF)`.
  energy_ratio <- demand_check_energy_ratio(
    energy_ratio,
    direction,
    time_horizon,
    time_slots
  )
  LF_energy <- sum(LF)
  energy_share <- function(ratio) {
    if (ratio < 1) round(ratio * LF_energy, 2) else LF_energy
  }
  energy_lb <- energy_share(energy_ratio[1])
  energy_ub <- energy_share(energy_ratio[2])
  if (energy_lb < energy_ub) {
    # The objective improves when load is removed, so a range would be read as
    # "deliver the minimum". Reward each unit of optimized load kept by more
    # than the objective can gain from dropping it (see demand_solve_window()):
    # the solver keeps as much energy as the caps admit and shapes it after.
    base_bounds <- get_bounds_v2g(
      time_slots,
      G,
      LF,
      LS,
      direction,
      time_horizon,
      LFmax,
      import_capacity,
      export_capacity,
      cumsum_min,
      LFmin
    )
    reward <- demand_energy_reward(
      P,
      q,
      envelope = max(abs(base_bounds$ub_O[is.finite(base_bounds$ub_O)]), 0) +
        max(abs(LS - G))
    )
    q <- q - reward
  }

  solve_with_caps <- function(
    import_cap,
    export_cap,
    clamp_to_lf = FALSE,
    energy_floor = energy_lb,
    energy_ceiling = energy_ub
  ) {
    bounds <- get_bounds_v2g(
      time_slots,
      G,
      LF,
      LS,
      direction,
      time_horizon,
      LFmax,
      import_cap,
      export_cap,
      cumsum_min,
      LFmin
    )

    # Minimal-relaxation retry: guarantee the original profile O = LF is a
    # feasible point. LFmax can sit below LF (e.g. when the caller pre-shrinks
    # the max power for discharge conversion losses), which would pull ub_O
    # under LF, so widen the optimized-load box to admit it.
    if (clamp_to_lf) {
      bounds$ub_O <- pmax(bounds$ub_O, LF)
      bounds$lb_O <- pmin(bounds$lb_O, LF)
    }

    constraint_mats <- list(
      bounds$Amat_O,
      bounds$Amat_cumsum,
      bounds$Amat_energy
    )
    A <- Matrix::Matrix(do.call(rbind, constraint_mats), sparse = TRUE)
    # The departure deadlines never ask for more than the energy row: both
    # are rounded separately and must not contradict each other.
    l_vec <- c(bounds$lb_O, pmin(bounds$lb_cumsum, energy_floor), energy_floor)
    u_vec <- c(bounds$ub_O, bounds$ub_cumsum, energy_ceiling)

    solve_osqp(
      P = P,
      q = q,
      A = A,
      lb = l_vec,
      ub = u_vec
    )
  }

  # First solve: keep the original grid limits.
  O <- solve_with_caps(import_capacity, export_capacity)
  if (demand_highs_is_optimal(O$result)) {
    return(round(O$x, 2))
  }

  # Fallback solves: the original grid limits make the problem infeasible.
  # Instead of removing the grid constraints entirely, raise the per-slot caps
  # only as far as a reference profile already needs (import LS + LF_ref - G,
  # export G - LS - LF_ref). Slots within their caps keep the true capacity, so
  # the optimiser can never create a violation worse than that profile had.
  # The retry with the original profile is feasible BY CONSTRUCTION: O = LF
  # satisfies energy equality (sum(O) = sum(LF)) and cumsum bounds (derived
  # from LF) trivially, the relaxed caps admit its net flows, and clamp_to_lf
  # lifts ub_O to LF when LFmax would otherwise pull it below. The
  # minimum-energy retry tried first is the same construction on the scaled
  # profile (see demand_solve_window()). A small tolerance absorbs the
  # 2-decimal rounding of the bounds.
  tol <- optimization_solution_tolerance()
  relax_to <- function(LF_ref, energy) {
    solve_with_caps(
      pmax(import_capacity, LS + LF_ref - G) + tol,
      pmax(export_capacity, G - LS - LF_ref) + tol,
      clamp_to_lf = TRUE,
      energy_floor = energy,
      energy_ceiling = energy
    )
  }

  # Reach first: a cap can ask for more than the connected vehicles can give
  # (an import cap of -20 kW with 15 kW of vehicles plugged in, or any cap
  # before the first arrival). Relax each cap only as far as the fleet's
  # physical reach (`LFmax` in both directions), so the setpoint still gets as
  # close to the cap as the vehicles allow instead of falling back to the
  # original profile in the whole window. Not feasible by construction (the
  # energy and departure rows may still conflict), hence the retries below.
  LFmax_vct <- rep_len(as.numeric(LFmax), time_slots)
  if (any(is.finite(LFmax_vct))) {
    import_cap <- rep_len(as.numeric(import_capacity), time_slots)
    export_cap <- rep_len(as.numeric(export_capacity), time_slots)
    import_reach <- pmax(import_cap, LS - G - LFmax_vct)
    export_reach <- pmax(export_cap, G - LS - LFmax_vct)
    unreachable <- (import_reach > import_cap) | (export_reach > export_cap)
    if (any(unreachable, na.rm = TRUE)) {
      O <- solve_with_caps(import_reach + tol, export_reach + tol)
      if (demand_highs_is_optimal(O$result)) {
        message_once(
          "\u26A0\uFE0F V2G optimisation warning: the connected vehicles cannot reach the grid capacity in some slots. Approaching it as far as their power allows."
        )
        return(round(O$x, 2))
      }
    }
  }

  # Energy first: when the range admits less than the full profile, relax the
  # caps only as far as the minimum-energy profile needs and deliver exactly
  # that minimum. A minimum of 0 is skipped (no profile to relax towards).
  LF_min <- if (energy_lb < round(LF_energy, 2) && LF_energy > 0) {
    round(LF * energy_lb / LF_energy, 2)
  } else {
    LF
  }
  if (!identical(LF_min, LF) && sum(LF_min) > 0) {
    message_once(
      "\u26A0\uFE0F V2G optimisation warning: even the minimum energy does not fit under the grid capacity in some windows. Relaxing grid capacity to the minimum-energy profile in the affected windows."
    )
    O <- relax_to(LF_min, sum(LF_min))
    if (demand_highs_is_optimal(O$result)) {
      return(round(O$x, 2))
    }
  }

  # Capacity second: the original, unshifted profile.
  message_once(
    "\u26A0\uFE0F V2G optimisation warning: optimisation not feasible in some windows. Relaxing grid capacity to the original profile in the affected windows."
  )
  O <- relax_to(LF, LF_energy)
  if (demand_highs_is_optimal(O$result)) {
    return(round(O$x, 2))
  }

  # Unreachable for well-formed inputs: the relaxation above is feasible by
  # construction, so reaching here means the solver crashed. Surface it loudly
  # and fall back to the untouched input profile as a last resort.
  message_once(paste0(
    "\u26A0\uFE0F V2G optimisation warning: ",
    O$result$status_message,
    ". No optimisation provided."
  ))
  LF
}


#' Discharge floor of the V2G optimized load
#'
#' Discharge only serves to cut import, or to meet a negative import capacity:
#' in a slot where the site already exports, a discharge only adds export. The
#' floor lets the fleet discharge at most down to a net power of
#' `min(import_capacity, 0)`, so not at all in a slot where the site exports
#' without it (under a non-negative import capacity). Without it the
#' minimum-squared-net-power LP trades a shallow export for a deeper one
#' (discharging into the first to charge more in the second), which raises the
#' export and can push it over `export_capacity`.
#'
#' The floor is always `<= 0 <= LF`, so the original profile stays a feasible
#' point for the fallbacks of `solve_optimization_window_v2g()`.
#'
#' @param G,LS,import_capacity numeric vectors, production, static load and
#'   import capacity of the slots.
#'
#' @return numeric vector, the per-slot floor (`<= 0`) of the optimized load.
#' @keywords internal
#'
v2g_discharge_floor <- function(G, LS, import_capacity) {
  G <- as.numeric(G)
  import_capacity <- rep_len(as.numeric(import_capacity), length(G))
  pmin(G - as.numeric(LS) + pmin(import_capacity, 0), 0)
}


get_bounds_v2g <- function(
  time_slots,
  G,
  LF,
  LS,
  direction,
  time_horizon,
  LFmax,
  import_capacity,
  export_capacity,
  cumsum_min = NULL,
  LFmin = NULL
) {
  identityMat <- diag(time_slots)
  cumsumMat <- triangulate_matrix(matrix(1, time_slots, time_slots), "l")

  # `rep_len`, not `rep`: callers such as capacity_v2g_window pass a per-slot
  # LFmax vector, and `rep(vec, time_slots)` would tile it to length
  # time_slots^2 and silently corrupt every downstream bound.
  LFmax_vct <- rep_len(LFmax, time_slots)
  LFmin_vct <- -LFmax_vct
  # An extra per-slot floor on the optimized load (see capacity_v2g_window()),
  # never above the nominal-power ceiling.
  if (!is.null(LFmin)) {
    LFmin_vct <- pmax(LFmin_vct, pmin(rep_len(LFmin, time_slots), LFmax_vct))
  }

  lb_O <- round(pmax(G - LS - export_capacity, LFmin_vct), 2)
  ub_O <- round(pmin(G - LS + import_capacity, LFmax_vct), 2)

  Amat_cumsum <- cumsumMat
  if (direction == "forward") {
    if (time_horizon == time_slots) {
      horizonMat_cumsum <- matrix(0, time_slots, time_slots)
    } else {
      horizonMat_cumsum <- triangulate_matrix(
        matrix(1, time_slots, time_slots),
        "l",
        -time_horizon
      )
    }
    horizonMat_identity <- triangulate_matrix(
      triangulate_matrix(matrix(1, time_slots, time_slots), "l"),
      "u",
      -time_horizon
    )

    ub_cumsum <- cumsumMat %*% LF
    # Without departure deadlines the load may discharge without limit. With
    # them (`cumsum_min`, see get_flex_envelope()), `cumsum(O)` must stay above
    # the energy owed to the departed sessions minus what the connected ones
    # can still discharge down to their SoC floor. Capped at `cumsum(LF)`, so
    # the original profile stays a feasible point for the fallback.
    lb_cumsum <- if (is.null(cumsum_min)) {
      rep(-Inf, time_slots)
    } else {
      pmin(rep_len(as.numeric(cumsum_min), time_slots), ub_cumsum)
    }

    ub_shift <- horizonMat_identity %*% LF
    ub_O <- pmin(pmax(ub_shift, lb_O), ub_O)
  } else {
    horizonMat_cumsum <- triangulate_matrix(
      matrix(1, time_slots, time_slots),
      "l",
      time_horizon
    )
    horizonMat_identity <- triangulate_matrix(
      triangulate_matrix(matrix(1, time_slots, time_slots), "u"),
      "l",
      time_horizon
    )

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


schedule_sessions_v2g <- function(
  sessions,
  setpoint,
  power_th = 0,
  charging_power_min = 0,
  energy_min = 1,
  energy_max = 1,
  discharge_eff = 1,
  include_log = FALSE,
  show_progress = FALSE,
  soc_min = 0.2
) {
  if (show_progress) {
    cli::cli_h1("Scheduling V2G sessions")
  }

  log <- c()

  if (is.null(sessions) || nrow(sessions) == 0) {
    return(list(
      sessions = dplyr::tibble(),
      log = log
    ))
  }
  if (!all(c("datetime", "setpoint") %in% colnames(setpoint))) {
    stop(
      "Error: `setpoint` does not contain all required variables
      (see Arguments description)"
    )
  }

  resolution <- timefully::get_time_resolution(
    setpoint$datetime,
    units = "mins"
  )
  dttm_tz <- lubridate::tz(setpoint$datetime)

  check_charging_power_min(charging_power_min)
  check_soc_min(soc_min)
  if (
    !is.numeric(discharge_eff) ||
      length(discharge_eff) != 1 ||
      is.na(discharge_eff) ||
      discharge_eff <= 0 ||
      discharge_eff > 1
  ) {
    stop("`discharge_eff` should be a numeric value in (0, 1]")
  }
  check_energy_ratios(energy_min, energy_max)

  # `energy_max` caps every session at a share of the energy it asked for, as
  # in schedule_sessions(). The scaled `Energy` is the net target the session
  # charges towards (`EnergyRequired`, set by `expand_sessions()`), so it stops
  # charging once it has that share net of discharge (`EnergyToCharge` grows
  # by the discharged energy grossed up by `discharge_eff`). `energy_min`
  # keeps meaning "share of the ORIGINAL requirement", hence the floor below is
  # expressed relative to the target.
  energy_min_target <- energy_min / energy_max
  sessions_sch <- sessions %>%
    set_state_of_charge() %>%
    dplyr::filter(.data$ConnectionStartDateTime %in% setpoint$datetime) %>%
    dplyr::mutate(
      Energy = .data$Energy * energy_max,
      ChargingHours = .data$Energy / .data$Power
    )

  if (nrow(sessions_sch) == 0) {
    message("Error: no `sessions` for `setpoint$datetime` period")
    return(list(
      sessions = dplyr::tibble(),
      log = log
    ))
  }

  sessions_expanded <- sessions_sch %>%
    evsim::expand_sessions(resolution = resolution)
  sessions_expanded <- sessions_expanded %>%
    left_join(
      select(
        sessions_sch,
        any_of(c(
          "Session",
          "ConnectionStartDateTime",
          "FlexibilityHours",
          "StateOfCharge"
        ))
      ),
      by = "Session"
    ) %>%
    mutate(
      Power = 0,
      EnergyToCharge = .data$EnergyRequired,
      Flexible = NA,
      Exploited = NA,
      # The ORIGINAL requirement takes the battery from its arrival SoC to
      # full, whatever `energy_max` (which only makes it leave below full).
      BatteryCapacity = v2g_battery_capacity(
        .data$EnergyRequired / energy_max,
        .data$StateOfCharge
      ),
      EnergyInBattery = .data$BatteryCapacity * .data$StateOfCharge
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
      lubridate::as_datetime(timeslot, tz = dttm_tz),
      "%d/%m/%Y %H:%M"
    )

    if (include_log) {
      log <- c(
        log,
        paste(
          "\u2500\u2500",
          timeslot_dttm,
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
    #   - `EnergyCharged`: energy already charged before this time slot.
    #   - `MinEnergyToCharge`: minimum energy that must be charged to fulfill
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
          .data$EnergyToCharge / (resolution / 60),
          .data$PowerNominal
        ),
        EnergyCharged = .data$EnergyRequired - .data$EnergyToCharge,
        MinEnergyToCharge = pmax(
          .data$EnergyRequired * energy_min_target - .data$EnergyCharged,
          0
        ),
        ChargingPowerMin = pmin(.data$PowerNominal, charging_power_min),
        PossibleEnergyRest = .data$PowerNominal *
          pmax(.data$ConnectionHoursLeft - resolution / 60, 0)
      )

    # Flexibility definition for V2G ---------------------------
    # The minimum power that can be charged in this time slot is
    # lower than the nominal power. The minimum power is defined by:
    #  - The minimum energy that must be charged in the time slot, defined by
    #      - The energy that must be charged to reach the `energy_min` target at departure
    #      - The minimum State-of-Charge (`soc_min`) allowed
    #      - The energy that can be charged at nominal power the rest of connection hours

    sessions_timeslot <- sessions_timeslot %>%
      mutate(
        MinEnergyTimeslot = pmax(
          # Can be negative (discharging)
          # Difference between energy to reach the minimum charge and
          # possible energy that can be charged the rest of connection.
          # If it is negative, it means that we have time to charge later
          # so now we can discharge.
          .data$MinEnergyToCharge - .data$PossibleEnergyRest,
          # Difference between energy to reach min SOC and
          # energy in battery
          # If it is negative, it means that we are above min SOC
          # so now we can discharge
          .data$BatteryCapacity * soc_min - .data$EnergyInBattery
        ),
        MinPowerTimeslot = pmax(
          # Can be negative (discharging)
          .data$MinEnergyTimeslot / (resolution / 60),
          -.data$PowerNominal # Maximum discharging power
        ),
        MinPowerTimeslot = ifelse(
          .data$ChargingPowerMin > 0 & .data$MinPowerTimeslot > 0,
          pmax(.data$MinPowerTimeslot, .data$ChargingPowerMin), # Charging
          .data$MinPowerTimeslot # Discharging or no min power defined
        ),
        MaxPowerReduction = .data$PowerTimeslot - .data$MinPowerTimeslot,
        Flexible = ifelse(
          # Added error tolerance
          .data$MaxPowerReduction > 0.1,
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
      sessions_timeslot_power - setpoint_power_timeslot,
      2
    )

    # If demand should be reduced
    if (flex_req > 0) {
      if (include_log) {
        log_message <- c(
          paste("\u2139 Flexibility requirement of", flex_req, "kW"),
          paste("\u2139", nrow(sessions_timeslot), "connected vehicles")
        )

        # message(log_message)
        log <- c(
          log,
          log_message
        )
      }

      # Power flexibility (V2G) ----------------------------------------------------------------

      if (any(sessions_timeslot$Flexible)) {
        # All `Flexible` sessions are `Exploited` with "V2G"
        sessions_timeslot$Exploited[sessions_timeslot$Flexible] <- TRUE

        # Get the maximum power reduction from all `Flexible` sessions
        max_power_reduction <- sum(
          sessions_timeslot$MaxPowerReduction[sessions_timeslot$Flexible]
        )

        # Power reduction factor that would be required
        reduction_factor <- min(flex_req / max_power_reduction, 1)

        # Apply reduction factor to compute target power
        sessions_timeslot$Power[sessions_timeslot$Exploited] <-
          sessions_timeslot$PowerTimeslot[sessions_timeslot$Exploited] -
          sessions_timeslot$MaxPowerReduction[sessions_timeslot$Exploited] *
            reduction_factor

        # Clamp target power to respect `charging_power_min`.
        # Only needed when `MinPowerTimeslot <= 0` (discharging allowed),
        # because the lower-bound constraint does not prevent small-magnitude
        # powers inside (-charging_power_min, +charging_power_min).
        # Allowed zones: [-PowerNominal, -charging_power_min], 0 and
        # [charging_power_min, PowerNominal].
        #  - A charge inside (0, min) goes up to the minimum, or to
        #    `PowerTimeslot` when the session needs less than the minimum to
        #    finish (the tail of its charge, which may be below the minimum).
        #  - A discharge inside (-min, 0) goes down to -min when the SoC floor
        #    and the energy deadline (`MinPowerTimeslot`) allow it, else to 0:
        #    a session never discharges below the minimum.
        if (charging_power_min > 0) {
          idx_exploited <- sessions_timeslot$Exploited

          idx_pos <- idx_exploited &
            sessions_timeslot$MinPowerTimeslot <= 0 &
            sessions_timeslot$Power > 0 &
            sessions_timeslot$Power < sessions_timeslot$ChargingPowerMin
          if (any(idx_pos)) {
            sessions_timeslot$Power[idx_pos] <- pmin(
              sessions_timeslot$ChargingPowerMin[idx_pos],
              sessions_timeslot$PowerTimeslot[idx_pos]
            )
          }

          idx_neg <- idx_exploited &
            sessions_timeslot$MinPowerTimeslot <= 0 &
            sessions_timeslot$Power < 0 &
            sessions_timeslot$Power > -sessions_timeslot$ChargingPowerMin
          if (any(idx_neg)) {
            sessions_timeslot$Power[idx_neg] <- ifelse(
              sessions_timeslot$MinPowerTimeslot[idx_neg] <=
                -sessions_timeslot$ChargingPowerMin[idx_neg],
              -sessions_timeslot$ChargingPowerMin[idx_neg],
              0
            )
          }
        }

        # Update flexibility requirement
        flex_provided <- round(
          sum(
            sessions_timeslot$PowerTimeslot[sessions_timeslot$Exploited] -
              sessions_timeslot$Power[sessions_timeslot$Exploited]
          ),
          2
        )
        flex_req <- round(flex_req - flex_provided, 2)
      } else {
        flex_provided <- 0
      }

      # Update the charging power of sessions that are NOT curtailed
      sessions_timeslot$Power[!sessions_timeslot$Exploited] <-
        sessions_timeslot$PowerTimeslot[!sessions_timeslot$Exploited]

      if (include_log) {
        if (flex_req > 0) {
          log_message <- paste0(
            "\u2716 Not enough flexibility available (",
            flex_provided,
            " kW)"
          )
          log <- c(
            log,
            log_message
          )
        }
      }
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

      # If there are more time slots afterwards, update `EnergyToCharge`
      session_after_idx <- (sessions_expanded$Session ==
        sessions_timeslot$Session[s]) &
        (sessions_expanded$ID > sessions_timeslot$ID[s])

      if (length(session_after_idx) > 0) {
        session_energy <- sessions_timeslot$Power[s] * resolution / 60
        if (session_energy < 0) {
          # Account for discharge losses: more energy must be recharged later.
          session_energy <- session_energy / discharge_eff
        }

        # Update `EnergyToCharge`
        # We assume that the session is charging the whole timeslot, so:
        # `ChargingHours = resolution/60`
        session_energy_to_charge <- sessions_timeslot$EnergyToCharge[s] -
          session_energy
        sessions_expanded$EnergyToCharge[
          session_after_idx
        ] <- session_energy_to_charge

        # Update `EnergyInBattery`
        session_energy_in <- sessions_timeslot$EnergyInBattery[s] +
          session_energy
        sessions_expanded$EnergyInBattery[
          session_after_idx
        ] <- session_energy_in
      } else {
        session_energy_to_charge <- 0
        session_energy_in <- 0
      }
    }

    # Log message ----------------------------------------------------------------
    if (include_log) {
      exploited_sessions <- sessions_timeslot %>% filter(.data$Exploited)

      for (s in seq_len(nrow(exploited_sessions))) {
        power_reduction <- round(
          exploited_sessions$PowerTimeslot[s] - exploited_sessions$Power[s],
          2
        )
        if (power_reduction > 0) {
          pct_power_reduction <- round(
            power_reduction / exploited_sessions$PowerTimeslot[s] * 100,
            1
          )
          log_message <- paste0(
            "| \u2714 Session ",
            exploited_sessions$Session[s],
            " provides ",
            power_reduction,
            " kW of flexibility (",
            pct_power_reduction,
            "% power reduction)"
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

  if (show_progress) {
    cli::cli_progress_step("Cleaning data set")
  }

  # Update the sessions data set with all variables from the original data set
  sessions_segmented <- sessions_expanded %>%
    select(any_of(c(
      evsim::sessions_feature_names,
      "Timeslot",
      "EnergyToCharge",
      "ConnectionHoursLeft",
      "Flexible",
      "Exploited"
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
    select(
      any_of(names(sessions_sch)),
      "Flexible",
      "Exploited",
      "EnergyToCharge",
      "ConnectionHoursLeft"
    )

  list(
    sessions = sessions_sch_flex,
    log = log
  )
}


#' Validate the SoC floor of the V2G sessions
#'
#' @param soc_min numeric, a single value with `0 <= soc_min < 1`.
#'
#' @return `TRUE`, invisibly. Stops with a clear message otherwise.
#' @keywords internal
#'
check_soc_min <- function(soc_min) {
  if (
    !is.numeric(soc_min) ||
      length(soc_min) != 1 ||
      !is.finite(soc_min) ||
      soc_min < 0 ||
      soc_min >= 1
  ) {
    stop("Error: `soc_min` must be a single number with 0 <= soc_min < 1")
  }
  invisible(TRUE)
}


#' Set and validate the arrival State-of-Charge of the V2G sessions
#'
#' Adds `StateOfCharge = 0.5` when the column is absent, and checks that every
#' value is a fraction with `0 <= StateOfCharge < 1` (a full battery has no
#' energy to charge, so it gives no battery size).
#'
#' @param sessions tibble of sessions
#'
#' @return `sessions` with a `StateOfCharge` column
#' @keywords internal
#'
set_state_of_charge <- function(sessions) {
  if (!("StateOfCharge" %in% names(sessions))) {
    sessions$StateOfCharge <- rep(0.5, nrow(sessions))
  }
  soc <- sessions$StateOfCharge
  if (!is.numeric(soc) || any(!is.finite(soc)) || any(soc < 0 | soc >= 1)) {
    stop(
      "Error: `StateOfCharge` in `sessions` must be a fraction with 0 <= StateOfCharge < 1"
    )
  }
  sessions
}


#' Battery capacity of a V2G session
#'
#' The session's energy takes the battery from its arrival State-of-Charge to
#' full: `Energy / (1 - StateOfCharge)` (twice the energy at 50%).
#'
#' @param energy numeric, energy the session requires (kWh)
#' @param state_of_charge numeric, arrival State-of-Charge (fraction, < 1)
#'
#' @return numeric, battery capacity (kWh)
#' @keywords internal
#'
v2g_battery_capacity <- function(energy, state_of_charge) {
  energy / (1 - state_of_charge)
}
