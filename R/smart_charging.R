
# EV flexibility management -----------------------------------------------

#' Smart charging algorithm
#'
#' @param sessions tibble, sessions data set
#' @param fitting_data tibble, optimization fitting data, first column being `datetime`.
#' The other columns could be `solar` (solar generation) and `fixed` (static demand from other sectors like buildings, offices, ...).
#' Only sessions starting within the time sequence of column `datetime` will be shifted.
#' If columns of `fitting_data` are user profiles names, these are used as setpoints and no optimization is performed.
#' @param method character, smart charging method being `none` or `postpone`.
#' If `none`, the scheduling part is skipped and the sessions returned in the results will be identical to the original parameter.
#' @param window_length integer, number of data points of the optimization window (not in hours)
#' @param window_start_hour integer, hour to start the optimization window. If `window_start = 6` the EV sessions are optimized from 6:00 to 6:00.
#' @param opt_weights Named list with the optimization weight `w` of the `minimize_grid_flow` function.
#' The names of the list must exactly match the Time-cycle and User profiles names, for example: list(Monday = list(Worktime = 1, Shortstay = 0.1))
#' @param responsive Named two-layer list with the ratio of sessions responsive to smart charging program.
#' The names of the list must exactly match the Time-cycle and User profiles names, for example: list(Monday = list(Worktime = 1, Shortstay = 0.1))
#' @param only_above_G logical, optimize only the part of flexible load that surpasses Generation.
#' If all demand is lower than Generation the optimization is skipped.
#' @param up_to_G logical, whether to limit the flexible EV demand up to renewable Generation
#' @param power_th numeric, power threshold accepted from setpoint, in percentage
#' @param include_log logical, whether to output the algorithm messages for every user profile and time-slot
#' @param charging_power_min numeric, minimum power to charge vehicles using curtailment method
#' @param charging_minutes_min integer, minimum time (in minutes) that the vehicle must be charging before interruptions
#'
#' @importFrom dplyr tibble %>% filter mutate select everything row_number left_join bind_rows any_of
#' @importFrom lubridate hour minute date
#' @importFrom rlang .data
#' @importFrom stats ecdf quantile
#'
#' @return a list with two elements: optimization setpoints and coordinated sessions schedule
#' @export
#'
smart_charging <- function(sessions, fitting_data, method, window_length, window_start_hour, opt_weights, responsive,
                           only_above_G = FALSE, up_to_G = FALSE, power_th = 0, include_log = FALSE,
                           charging_power_min = 3.7, charging_minutes_min = 30) {
  # Check input sessions
  if (is.null(sessions) | nrow(sessions) == 0) {
    message("sessions object is empty.")
    return(NULL)
  }

  # Datetime optimization parameters according to the window start and length
  window_length <- as.integer(window_length)
  window_start_hour <- as.integer(window_start_hour)
  dttm_seq <- adapt_dttm_seq_to_opt_windows(fitting_data$datetime, window_start_hour, window_length)
  time_interval <- as.integer(as.numeric(dttm_seq[2] - dttm_seq[1], unit = 'hours')*60)
  start <- dttm_seq[1]
  end <- dttm_seq[length(dttm_seq)]

  # Normalize sessions
  sessions_norm <- normalize_sessions(sessions, start, time_interval)
  sessions_norm[['Part']] <- 1
  sessions_norm[['Shifted']] <- 0
  sessions_norm[['Responsive']] <- FALSE

  # Get user profiles demand
  if (include_log) message("Getting EV demand profiles")
  profiles_demand <- get_sessions_demand(sessions_norm, 1:length(dttm_seq), normalized = T) %>%
    rename(timeslot = .data$datetime)

  # Normalize fitting data
  fitting_data_norm <- fitting_data %>%
    filter(.data$datetime %in% dttm_seq) %>%
    mutate(timeslot = row_number()) %>%
    select(.data$timeslot, everything(), -.data$datetime)


  # SMART CHARGING ----------------------------------------------------------
  log <- list()
  # If fitting data contains user profile's name this is considered to be a setpoint (skip optimization)
  if (any(colnames(profiles_demand[-1]) %in% colnames(fitting_data[-1]))) {
    do_opt <- FALSE
    setpoints <- fitting_data_norm
  } else {
    do_opt <- TRUE
    setpoints <- profiles_demand
  }


  # For each optimization window
  if (include_log) message("Smart charging:")
  for (i in seq(1, length(dttm_seq), window_length)) {
    window <- c(i, i+window_length-1)
    log_window_name <- as.character(date(dttm_seq[window[1]]))
    if (include_log) message(paste("--", log_window_name))
    log[[log_window_name]] <- list()

    # Filter only sessions that START CHARGING within the time window
    sessions_window <- sessions_norm %>% filter(.data$chs >= window[1], .data$chs <= window[2])

    # Window's features
    window_timecycle <- names(sort(table(sessions_window$Timecycle), decreasing = TRUE))[1]
    print(paste(log_window_name, "-", window_timecycle))
    window_responsive <- responsive[[window_timecycle]]
    window_opt_weights <- opt_weights[[window_timecycle]]

    # Profiles subjected to optimization:
    #   1. appearing in the sessions set for this optimization window
    #   2. responsive values higher than 0
    #   3. optimization weight higher than 0
    opt_profiles <- names(window_responsive)[
      (names(window_responsive) %in% unique(sessions_window$Profile)) &
        (names(window_responsive) %in% names(window_responsive)[as.numeric(window_responsive) > 0]) &
        (names(window_responsive) %in% names(window_opt_weights)[as.numeric(window_opt_weights) > 0])
    ]

    # For each optimization profile
    for (profile in opt_profiles) {

      if (include_log) message(paste("----", profile))

      # Filter only sessions of this Profile
      sessions_window_prof <- sessions_window %>% filter(.data$Profile == profile)

      # The smart charging algorithm only allows moving demand within the
      # time-window, so:
      #   1. Limit the CONNECTION END TIME of sessions that FINISH CHARGING BEFORE the window end
      sessions_window_prof$coe[(sessions_window_prof$coe > window[2]) & (sessions_window_prof$che < window[2])] <- window[2]
      #   2. Recalculate flexibility with new end connection times
      sessions_window_prof$f <- sessions_window_prof$coe  - sessions_window_prof$che
      #   3. Discard flexibility of sessions that FINISH CHARGING AFTER the window end
      sessions_window_prof$f[sessions_window_prof$che >= window[2]] <- 0

      # Re-define window to profile's connection window
      # # Find the End time for at least 75% of sessions
      # ss_ecdf <- ecdf(sessions_window_prof$coe)
      # ss_coe_75 <- pmin(as.integer(quantile(ss_ecdf)[4]), window[2])
      # # ss_coe_ecdf <- round(ss_ecdf(knots(ss_ecdf)), 1)
      # # ss_coe_90 <- knots(ss_ecdf)[ss_coe_ecdf == 0.9][1] # For the 90%
      # window_prof <- c(min(sessions_window_prof$cos), ss_coe_75)
      window_prof <- c(min(sessions_window_prof$cos), pmin(max(sessions_window_prof$coe), window[2]))
      window_prof_idxs <- (fitting_data_norm$timeslot >= window_prof[1]) & (fitting_data_norm$timeslot <= window_prof[2])
      window_prof_length <- window_prof[2] - window_prof[1] + 1


      # Separate between flexible sessions or not
      set.seed(1234)
      sessions_window_prof[["Responsive"]] <- sample(
        c(T, F),
        nrow(sessions_window_prof),
        replace = T,
        prob = c(window_responsive[[profile]], (1-window_responsive[[profile]]))
      )
      sessions_window_prof_flex <- sessions_window_prof %>% filter(.data$Responsive & .data$f >= 1)
      non_flexible_sessions <- sessions_window_prof %>% filter(!.data$Responsive | .data$f < 1)

      # SETPOINTS ----------------------------------------------------------
      #   Profile sessions that can't provide flexibility are not part of the setpoint
      if (nrow(non_flexible_sessions) > 0) {
        L_fixed_prof <-  get_all_sessions_demand_fast(
          non_flexible_sessions, window_prof[1]:window_prof[2]
        )[["demand"]]
      } else {
        L_fixed_prof <- rep(0, window_prof_length)
      }

      # OPTIMIZATION (if required)
      if (do_opt) {

        if (include_log) message("------ Optimization")

        # The optimization static load consists on:
        #   - Environment fixed load (buildings, lightning, etc)
        if ('fixed' %in% colnames(fitting_data_norm)) {
          L_fixed <- fitting_data_norm$fixed[window_prof_idxs]
        } else {
          L_fixed <- rep(0, window_prof_length)
        }
        #   - Other profiles load
        L_others <- setpoints %>%
          filter(window_prof_idxs) %>%
          select(- any_of(c(profile, 'timeslot'))) %>%
          rowSums()
        if (length(L_others) == 0) {
          L_others <- rep(0, window_prof_length)
        }
        # The optimization flexible load is the load of the responsive sessions
        L_prof <- setpoints[[profile]][window_prof_idxs] - L_fixed_prof

        # Optimize the flexible profile's load
        O <- minimize_grid_flow_window_osqp(
          w = window_opt_weights[[profile]],
          G = fitting_data_norm$solar[window_prof_idxs],
          LF = L_prof,
          LS = L_fixed + L_others + L_fixed_prof,
          direction = 'forward',
          time_horizon = NULL,
          only_above_G = only_above_G,
          up_to_G = up_to_G
        )
        setpoints[[profile]][window_prof_idxs] <- O + L_fixed_prof
      }

      # SCHEDULING ----------------------------------------------------------
      if (method != "none") {
        setpoint_prof <- tibble(
          datetime = dttm_seq[window_prof[1]:window_prof[2]],
          timeslot = window_prof[1]:window_prof[2],
          setpoint = setpoints[[profile]][window_prof[1]:window_prof[2]] - L_fixed_prof
        )

        if (include_log) message("------ Scheduling")

        results <- schedule_sessions(
          sessions_prof = sessions_window_prof_flex, setpoint_prof = setpoint_prof,
          method = method, power_th = power_th, include_log = include_log,
          charging_power_min = charging_power_min, charging_slots_min = round(charging_minutes_min/time_interval)
        )

        sessions_window_prof_flex_opt <- results$sessions
        log[[log_window_name]][[profile]] <- results$log

        # Update original sessions set
        sessions_norm <- sessions_norm[!(sessions_norm$Session %in% sessions_window_prof_flex_opt$Session), ]
        sessions_norm <- bind_rows(sessions_norm, sessions_window_prof_flex_opt)
      }
    }
  }

  denormalized_sessions <- denormalize_sessions(sessions_norm, start, time_interval)
  sessions_opt <- left_join(
    denormalized_sessions,
    select(sessions, 'Session', !any_of(names(denormalized_sessions))),
    by = 'Session'
  )

  return(list(
    setpoints = denormalize_timeseries(setpoints, start, time_interval),
    sessions = sessions_opt,
    log = log
  ))
}


#' Schedule sessions according to optimal setpoint
#'
#' @param sessions_prof tibble, sessions data set normalized from `normalize_sessions` function
#' @param setpoint_prof tibble with columns `datetime`, `timeslot` and rest of columns being the names of user profiles
#' @param method character, being `postpone` or `curtail`
#' @param power_th numeric, power threshold accepted from setpoint, in percentage
#' @param include_log logical, whether to output the algorithm messages for every user profile and time-slot
#' @param charging_power_min numeric, minimum power to charge vehicles using curtailment method
#' @param charging_slots_min integer, minimum time slots that the vehicle must be charging before interruptions
#'
#' @return list of two elements `sessions` and `log`
#' @export
#'
#' @importFrom dplyr tibble %>% filter pull arrange desc bind_rows
#' @importFrom rlang .data
#'
schedule_sessions <- function(sessions_prof, setpoint_prof, method, power_th = 0, include_log = F, charging_power_min = 3.7, charging_slots_min = 2) {
  timeslots_blacklist <- c()
  log <- c()

  # Calculate demand of profiles
  demand_prof <- get_all_sessions_demand_fast(sessions_prof, setpoint_prof$timeslot)

  # Loop
  while (TRUE) {
    # Flexibility requirements
    flex_req <- tibble(
      timeslot = setpoint_prof$timeslot,
      power = round(demand_prof$demand - setpoint_prof$setpoint*(1 + power_th/100), 1)
    ) %>% filter(.data$power > 0, !(.data$timeslot %in% timeslots_blacklist))

    if (nrow(flex_req) == 0) {
      if (include_log) log <- c(log, "No more flexibility requirements or they can not be satisfied.")
      break
    }

    # Select the first time slot with flexibility requirements
    flex_timeslot <- flex_req$timeslot[1]
    flex_timeslot_req <- flex_req %>% filter(.data$timeslot == flex_timeslot) %>% pull(.data$power)

    if (flex_timeslot == setpoint_prof$timeslot[nrow(setpoint_prof)]) {
      if (include_log) log <- c(log, "Can't expand sessions outside the optimization window")
      break
    }

    if (include_log) log <- c(log, paste("---------- Flexibility requirement of", flex_timeslot_req, "kW on",
                                         setpoint_prof$datetime[which(setpoint_prof$timeslot == flex_timeslot)], "----------"))


    if (method %in% c('postpone', 'postpone_interrupt', 'postpone_curtail', 'postpone_interrupt_curtail')) {

      # Postponable sessions:
      #   - at least 1 flexible timeslot (f >= 1)
      #   - flex_timeslot must be the charging start time
      flex_timeslot_sessions <- sessions_prof %>%
        filter(.data$f >= 1, .data$chs == flex_timeslot) %>%
        arrange(.data$chs, desc(.data$f))

      if (nrow(flex_timeslot_sessions) == 0) {
        if (include_log) log <- c(log, "No Postponing flexibility available.")
        timeslots_blacklist <- c(timeslots_blacklist, flex_timeslot)
        next
      }

      reschedule <- postpone_sessions(
        sessions_prof = sessions_prof, flex_timeslot = flex_timeslot, flex_timeslot_sessions = flex_timeslot_sessions,
        flex_timeslot_req = flex_timeslot_req, power_th = 0, demand_prof = demand_prof, log = log, include_log = include_log
      )
      sessions_prof <- reschedule$sessions
      log <- reschedule$log
      demand_prof <- reschedule$demand
    }

    if (method %in% c('postpone_interrupt', 'postpone_interrupt_curtail')) {

      # Interruptable sessions:
      #   - at least 1 flexible timeslot (f >= 1)
      #   - flex_timeslot is at least charging_slots_min later than charging start time and
      #       charging_slots_min earlier than charging end time
      #   - Interrupted less than 3 times
      max_interrupted_sessions <- sessions_prof %>%
        filter(.data$Part >= 5) %>%
        pull(.data$Session) %>%
        unique()
      flex_timeslot_sessions <- sessions_prof %>%
        filter(
          .data$f >= 1,
          flex_timeslot >= (.data$chs + charging_slots_min),
          flex_timeslot <= (.data$che - charging_slots_min),
          !(.data$Session %in% max_interrupted_sessions)
        ) %>%
        arrange(.data$chs, desc(.data$f))

      if (nrow(flex_timeslot_sessions) == 0) {
        if (include_log) log <- c(log, "No Interrupting flexibility available.")
        timeslots_blacklist <- c(timeslots_blacklist, flex_timeslot)
        next
      }

      reschedule <- interrupt_sessions(
        sessions_prof = sessions_prof, flex_timeslot = flex_timeslot, flex_timeslot_sessions = flex_timeslot_sessions,
        flex_timeslot_req = flex_timeslot_req, power_th = 0, demand_prof = demand_prof, log = log, include_log = include_log
      )
      sessions_prof <- reschedule$sessions
      log <- reschedule$log
      demand_prof <- reschedule$demand
    }

    if (method %in% c('curtail', 'postpone_curtail', 'postpone_interrupt_curtail')) {

      # Curtailable sessions:
      #   - at least 1 flexible timeslot (f >= 1)
      #   - flex_timeslot in the middle or beginning of the charging time
      #   - power higher than minimum power
      #   - energy from flex_timeslot can be divided to at least 2 timeslots charging at minimum power
      flex_timeslot_sessions <- sessions_prof %>%
        filter(
          .data$f >= 1, .data$chs <= flex_timeslot,
          .data$che > flex_timeslot,
          .data$p > charging_power_min,
          (.data$che - flex_timeslot)*.data$p >= charging_power_min*2
        ) %>%
        arrange(.data$chs, desc(.data$f))

      if (nrow(flex_timeslot_sessions) == 0) {
        if (include_log) log <- c(log, "No Curtailment flexibility available.")
        timeslots_blacklist <- c(timeslots_blacklist, flex_timeslot)
        next
      }

      reschedule <- curtail_sessions(
        sessions_prof = sessions_prof, flex_timeslot = flex_timeslot, flex_timeslot_sessions = flex_timeslot_sessions,
        flex_timeslot_req = flex_timeslot_req, power_th = 0, charging_power_min = charging_power_min, demand_prof = demand_prof,
        log = log, include_log = include_log
      )

      sessions_prof <- reschedule$sessions
      log <- reschedule$log
      demand_prof <- reschedule$demand
    }
  }

  return(list(sessions = sessions_prof, log = log))
}



postpone_sessions <- function(sessions_prof, flex_timeslot, flex_timeslot_sessions, flex_timeslot_req, power_th, demand_prof, log, include_log = F) {
  for (s in 1:nrow(flex_timeslot_sessions)) {
    session <- flex_timeslot_sessions[s, ]

    # Update session features
    session_idx <- which(sessions_prof$Session == session$Session & sessions_prof$Part == session$Part)
    sessions_prof$chs[session_idx] <- session$chs + 1
    sessions_prof$che[session_idx] <- session$che + 1
    sessions_prof$f[session_idx] <- session$f - 1
    sessions_prof$Shifted[session_idx] <- session$Shifted + 1
    if (include_log) log <- c(log, paste('Postponing session', session$Session, 'part', session$Part))

    # Update flexibility requirement
    flex_timeslot_req <- round(flex_timeslot_req - session$p, 1)

    # Reduce demand in old charging start time slot
    timeslot_idx_was_starting <- which(demand_prof$timeslot == session$chs)
    demand_prof$demand[timeslot_idx_was_starting] <- demand_prof$demand[timeslot_idx_was_starting] - session$p

    # Increase demand in old charging end time slot
    # Sessions ending between time slots suppose an extra-demand at charging end time slot
    extra_demand <- session$p*(session$che %% 1)
    timeslot_idx_was_ending <- which(demand_prof$timeslot == trunc(session$che))
    timeslot_idx_is_ending <- timeslot_idx_was_ending + 1
    demand_prof$demand[timeslot_idx_was_ending] <- demand_prof$demand[timeslot_idx_was_ending] - extra_demand + session$p
    demand_prof$demand[timeslot_idx_is_ending] <- demand_prof$demand[timeslot_idx_is_ending] + extra_demand

    # If the session power is lower than the curtailment power skip this session
    if (flex_timeslot_req <= power_th) {
      if (include_log) log <- c(log, "Setpoint achieved")
      break
    } else {
      if (include_log) log <- c(log, paste(flex_timeslot_req, "kW of flexibility still required"))
    }
  }
  if (flex_timeslot_req > power_th) {
    if (include_log) log <- c(log, "All postponable sessions exploited")
  }
  return(list(sessions = sessions_prof, log = log, demand = demand_prof))
}


curtail_sessions <- function(sessions_prof, flex_timeslot, flex_timeslot_sessions, flex_timeslot_req, power_th, charging_power_min, demand_prof, log, include_log = F) {
  for (s in 1:nrow(flex_timeslot_sessions)) {
    session <- flex_timeslot_sessions[s, ]
    session_idx <- which((sessions_prof$Session == session$Session) & (sessions_prof$Part == session$Part))

    if (length(session_idx) == 0) {
      message(paste("Part", session$Part, "of session", session$Session, "is not in the data set."))
      break
    } else if (length(session_idx) > 1) {
      message("Duplicated session.")
      break
    }

    curtail <- get_curtail_parameters(
      energy = (session$che - flex_timeslot)*session$p,
      available_timeslots = session$coe - flex_timeslot,
      power_minimum = charging_power_min
    )

    # Remove original session demand
    original_session_connection_idx <- (demand_prof$timeslot >= session$cos) & (demand_prof$timeslot < session$coe)
    session_demand_schedule <- get_sessions_schedule(session, session$cos, session$coe-1)
    demand_prof$demand[original_session_connection_idx] <-
      demand_prof$demand[original_session_connection_idx] - session_demand_schedule

    # Multiple curtailment types
    if (flex_timeslot == session$chs) {
      # 1. Session starts when flexibility is required

      session_curtail <- session
      session_curtail$p <- curtail$power
      session_curtail$che <- session_curtail$chs + curtail$timeslots
      session_curtail$f <- 0
      sessions_prof[session_idx, ] <- session_curtail
      if (include_log) log <- c(log, paste('Full curtailment for session', session$Session, 'from', session$p, 'to', curtail$power, 'kW'))

      demand_prof$demand[original_session_connection_idx] <- demand_prof$demand[original_session_connection_idx] +
        get_sessions_schedule(session_curtail, session$cos, session$coe-1)

    } else {
      # 2. Session started before flexibility requirement

      # Original part: change of charging/connection end time
      session_original <- session
      session_original$che <- flex_timeslot
      session_original$coe <- flex_timeslot
      session_original$e <- session_original$p*(session_original$che - session_original$chs)
      session_original$f <- 0
      sessions_prof[session_idx, ] <- session_original

      # Curtailed part: change of power, charging/connection start and session part
      session_curtail <- session
      session_curtail$p <- curtail$power
      session_curtail$cos <- flex_timeslot
      session_curtail$chs <- flex_timeslot
      session_curtail$che <- flex_timeslot + curtail$timeslots
      session_curtail$e <- session_curtail$p*(session_curtail$che - session_curtail$chs)
      session_curtail$f <- 0
      session_curtail$Part <- 2
      sessions_prof <- dplyr::bind_rows(sessions_prof, session_curtail)
      if (include_log) log <- c(log, paste('Partial curtailment for session', session$Session, 'from', session$p, 'to', curtail$power, 'kW'))

      demand_prof$demand[original_session_connection_idx] <- demand_prof$demand[original_session_connection_idx] +
        get_sessions_schedule(dplyr::bind_rows(session_original, session_curtail), session$cos, session$coe-1)
    }

    # Update flexibility requirement
    flex_timeslot_req <- round(flex_timeslot_req - (session$p - curtail$power), 1)

    # If the session power is lower than the curtailment power skip this session
    if (flex_timeslot_req <= power_th) {
      if (include_log) log <- c(log, "Setpoint achieved")
      break
    } else {
      if (include_log) log <- c(log, paste(flex_timeslot_req, "kW of flexibility still required"))
    }
  }
  if (flex_timeslot_req > power_th) {
    if (include_log) log <- c(log, "All curtailable sessions exploited")
  }
  return(list(sessions = sessions_prof, log = log, demand = demand_prof))
}



# Define the time and power of the curtailment to fit the time resolution of the schedule
get_curtail_parameters <- function(energy, available_timeslots, power_minimum) {
  curtail_timeslots <- energy/power_minimum
  # Curtailment length must be integer and higher than available time slots
  curtail_timeslots_int <- pmin(trunc(curtail_timeslots), available_timeslots)
  curtail_power <- energy/curtail_timeslots_int
  return(list(
    power = curtail_power,
    timeslots = curtail_timeslots_int
  ))
}


get_sessions_schedule <- function(sessions, start, end) {
  connection_seq <- start:end
  demand <- rep(0, length(connection_seq))
  for (s in 1:nrow(sessions)) {
    demand[(connection_seq >= sessions$chs[s]) & (connection_seq < sessions$che[s])] <- sessions$p[s]
  }
  return(demand)
}


interrupt_sessions <- function (sessions_prof, flex_timeslot, flex_timeslot_sessions,
                                flex_timeslot_req, power_th, demand_prof, log, include_log = F) {
  for (s in 1:nrow(flex_timeslot_sessions)) {
    session <- flex_timeslot_sessions[s, ]
    session_idx <- which(sessions_prof$Session == session$Session & sessions_prof$Part == session$Part)

    session_first <- session
    session_first$che <- flex_timeslot
    session_first$coe <- flex_timeslot
    session_first$e <- session_first$p * (session_first$che - session_first$chs)
    session_first$f <- 0
    sessions_prof[session_idx, ] <- session_first

    session_second <- session
    session_second$cos <- flex_timeslot + 1
    session_second$chs <- flex_timeslot + 1
    session_second$che <- session$che + 1
    session_second$e <- session_second$p * (session_second$che - session_second$chs)
    session_second$f <- session_second$coe - session_second$che
    session_second$Shifted <- session$Shifted + 1
    session_second$Part <- session$Part + 1
    sessions_prof <- dplyr::bind_rows(sessions_prof, session_second)

    if (include_log)
      log <- c(log, paste("Interrupting session", session$Session, "part", session$Part))

    # Update flexibility requirement
    flex_timeslot_req <- round(flex_timeslot_req - session$p, 1)

    # Reduce demand in the interruption time slot
    timeslot_idx_interruption <- which(demand_prof$timeslot == flex_timeslot)
    demand_prof$demand[timeslot_idx_interruption] <- demand_prof$demand[timeslot_idx_interruption] - session$p

    # Increase demand in old charging end time slot
    # Sessions ending between time slots suppose an extra-demand at charging end time slot
    extra_demand <- session$p*(session$che %% 1)
    timeslot_idx_was_ending <- which(demand_prof$timeslot == trunc(session$che))
    timeslot_idx_is_ending <- timeslot_idx_was_ending + 1
    demand_prof$demand[timeslot_idx_was_ending] <- demand_prof$demand[timeslot_idx_was_ending] - extra_demand + session$p
    demand_prof$demand[timeslot_idx_is_ending] <- demand_prof$demand[timeslot_idx_is_ending] + extra_demand

    if (flex_timeslot_req <= power_th) {
      if (include_log)
        log <- c(log, "Setpoint achieved")
      break
    }
    else {
      if (include_log)
        log <- c(log, paste(flex_timeslot_req, "kW of flexibility still required"))
    }
  }
  if (flex_timeslot_req > power_th) {
    if (include_log)
      log <- c(log, "All interruptable sessions exploited")
  }

  return(list(sessions = sessions_prof, log = log, demand = demand_prof))
}






# # DR potential ---------------------------------------------------------------
#
# get_interval_flexible_power <- function(interval, sessions_flex, interval_mins) {
#   sessions_flex %>%
#     group_by(Profile) %>%
#     filter(
#       StartTime <= interval,
#       (StartTime + convert_time_num_to_period(ChargingTime)) >= (interval + minutes(interval_mins)),
#       Flexibility > interval_mins/60
#     ) %>%
#     summarise(Power = sum(ChargingPower)) %>%
#     mutate(datetime = interval) %>%
#     spread(Profile, Power)
# }
#
# get_flexible_power <- function(sessions_flex, seq_dt, interval_mins) {
#   tibble(datetime = seq_dt) %>%
#     left_join(
#       map_dfr(seq_dt, ~get_interval_flexible_power(.x, sessions_flex, interval_mins)),
#       by = "datetime"
#     ) %>%
#     replace(is.na(.), 0)
# }
