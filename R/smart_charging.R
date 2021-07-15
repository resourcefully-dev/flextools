
# EV flexibility management -----------------------------------------------

#' Smart charging algorithm
#'
#' @param sessions tibble, sessions data set
#' @param fitting_data tibble, optimization fitting data, first column being `datetime`.
#' The other columns could be `solar` (solar generation) and `fixed` (static demand from other sectors like buildings, offices, ...).
#' Only sessions starting within the time sequence of column `datetime` will be shifted.
#' @param method character, smart charging method being `postpone` or `curtail`
#' @param window_length integer, number of data points of the optimization window (not in hours)
#' @param window_start_hour integer, hour to start the optimization window. If `window_start = 6` the EV sessions are optimized from 6:00 to 6:00.
#' @param opt_weights Named list with the optimization weight `w` of the `minimize_grid_flow` function. The names of the list must exactly match the user profiles names.
#' @param responsive Named list with the ratio of sessions responsive to smart charging program for each profile. The names of the list must exactly match the user profiles names.
#' @param up_to_G logical, whether to limit the flexible EV demand up to renewable Generation
#' @param power_th power threshold from to consider flexibility required
#' @param include_log logical, whether to output the algorithm messages for every user profile and time-slot
#' @param power_min numeric, minimum power to charge vehicles using curtailment method
#'
#' @importFrom dplyr %>% filter mutate select everything row_number left_join bind_rows any_of slice_sample
#' @importFrom lubridate hour minute
#' @importFrom rlang .data
#'
#' @return a list with two elements: optimization setpoints and coordinated sessions schedule
#' @export
#'
smart_charging <- function(sessions, fitting_data, method, window_length, window_start_hour, opt_weights, responsive, up_to_G = TRUE, power_th = 0, include_log = FALSE, power_min = 3.7) {
  # Datetime optimization parameters according to the window start and length
  window_length <- as.integer(window_length)
  dttm_seq_original <- fitting_data[['datetime']]
  dttm_seq_window_start <- dttm_seq_original[
    dttm_seq_original >= dttm_seq_original[which((hour(dttm_seq_original) == window_start_hour) & (minute(dttm_seq_original) == 0))[1]]
  ]
  n_windows <- length(dttm_seq_window_start) %/% window_length
  dttm_seq <- dttm_seq_window_start[1:(n_windows*window_length)]
  time_interval <- as.integer(as.numeric(dttm_seq[2] - dttm_seq[1], unit = 'hours')*60)
  start <- dttm_seq[1]
  end <- dttm_seq[length(dttm_seq)]

  # Normalize sessions
  sessions_norm <- normalize_sessions(sessions, start, time_interval)

  # Get user profiles demand
  profiles_demand <- get_sessions_demand(sessions_norm, 1:length(dttm_seq), normalized = T) %>%
    rename(timeslot = .data$datetime)

  # Normalize fitting data
  fitting_data_norm <- fitting_data %>%
    filter(.data$datetime %in% dttm_seq) %>%
    mutate(timeslot = row_number()) %>%
    select(.data$timeslot, everything(), -.data$datetime)

  if ('fixed' %in% colnames(fitting_data_norm)) {
    L_fixed <- fitting_data_norm[['fixed']]
  } else {
    L_fixed <- rep(0, nrow(fitting_data_norm))
  }

  # Profiles subjected to optimization
  #   1. appearing in the sessions set
  #   2. responsive values higher than 1
  opt_profiles <- names(opt_weights)[
    (names(opt_weights) %in% unique(sessions_norm[['Profile']])) &
      (names(opt_weights) %in% names(responsive)[as.numeric(responsive) > 0])
  ]

  # OPTIMIZATION (SETPOINTS)
  setpoints <- profiles_demand
  for (profile in opt_profiles) {
    other_profiles <- unique(sessions_norm[['Profile']])[unique(sessions_norm[['Profile']]) != profile]
    if (length(other_profiles) == 0) {
      other_profiles_load <- rep(0, nrow(setpoints))
    } else {
      other_profiles_load <- rowSums(select(setpoints, any_of(other_profiles)))
    }
    O <- minimize_grid_flow(
      w = 1,
      G = fitting_data_norm[['solar']],
      LF = setpoints[[profile]],
      LS = other_profiles_load + L_fixed,
      direction = 'forward',
      time_horizon = NULL,
      up_to_G = up_to_G,
      window_length = window_length
    )
    setpoints[[profile]] <- O
  }

  # SMART CHARGING
  sessions_norm_opt <- sessions_norm
  log <- list()
  # For each optimization profile
  for (profile in opt_profiles) {

    sessions_prof <- sessions_norm[sessions_norm[['Profile']] == profile, ]
    if (include_log) message(paste("Optimizing", profile, "sessions"))

    # For each optimization window
    for (i in seq(1, nrow(setpoints), window_length)) {
      window <- c(i, i+window_length-1)
      setpoint_prof <- tibble(timeslot = seq(window[1], window[2]), setpoint = setpoints[[profile]][.data$timeslot])

      # Filter only sessions that start and finish charging within the time window
      sessions_prof_window <- filter(sessions_prof, .data$chs >= window[1], .data$che < window[2])

      # Limit sessions End time to the windows's end timeslot
      sessions_prof_window[['coe']][(sessions_prof_window[['coe']] > window[2])] <- window[2]
      sessions_prof_window[['f']] <- (sessions_prof_window[['coe']] - sessions_prof_window[['chs']]) - (sessions_prof_window[['che']] - sessions_prof_window[['chs']])

      # Select responsive sessions
      set.seed(1234)
      sessions_prof_window_responsive <- slice_sample(sessions_prof_window, prop = responsive[[profile]])

      if (nrow(sessions_prof_window_responsive) == 0) next

      # Re-scheduling
      if (method == 'curtail') {
        # Curtail strategy
        sessions_norm_opt[['Part']] <- 1
        sessions_prof[['Part']] <- 1
        sessions_prof_window_responsive[['Part']] <- 1
        results <- schedule_sessions(
          sessions_prof = sessions_prof_window_responsive, setpoint_prof = setpoint_prof,
          method = method, power_th = power_th, include_log = include_log, power_min = power_min
        )
      } else if (method == 'postpone') {
        # Curtail strategy
        sessions_norm_opt[['Shifted']] <- 1
        sessions_prof[['Shifted']] <- 1
        sessions_prof_window_responsive[['Shifted']] <- 1
        results <- schedule_sessions(
          sessions_prof = sessions_prof_window_responsive, setpoint_prof = setpoint_prof,
          method = method, power_th = power_th, include_log = include_log
        )
      }

      sessions_prof_opt <- results$sessions
      log[[profile]][[paste0('t_', window[1])]] <- results$log

      # Update original profile sessions set
      sessions_prof <- sessions_prof %>% filter(!(.data$Session %in% sessions_prof_opt$Session))
      sessions_prof <- bind_rows(sessions_prof, sessions_prof_opt)
    }

    # Update original sessions set
    sessions_norm_opt <- sessions_norm_opt %>% filter(.data$Profile != profile)
    sessions_norm_opt <- bind_rows(sessions_norm_opt, sessions_prof)
  }

  sessions_opt <- left_join(
    sessions['Session'],
    denormalize_sessions(sessions_norm_opt, start, time_interval),
    by = 'Session'
  ) %>% select('Profile', everything())

  return(list(
    setpoints = denormalize_timeseries(setpoints, start, time_interval),
    sessions = sessions_opt,
    log = log
  ))
}


#' Schedule sessions according to optimal setpoint
#'
#' @param sessions_prof tibble, sessions data set normalized from `normalize_sessions` function
#' @param setpoint_prof tibble, first column being `datetime` and rest of columns being the names of user profiles
#' @param method character, being `postpone` or `curtail`
#' @param power_th power threshold from to consider flexibility required
#' @param include_log logical, whether to output the algorithm messages for every user profile and time-slot
#' @param power_min numeric, minimum power to charge vehicles using curtailment method
#'
#' @return list of two elements `sessions` and `log`
#' @export
#'
#' @importFrom dplyr tibble %>% filter pull arrange desc
#' @importFrom rlang .data
#'
schedule_sessions <- function(sessions_prof, setpoint_prof, method, power_th = 1, include_log = F, power_min = 3.7) {
  timeslots_blacklist <- c()
  log <- c()

  # Power threshold must be at least 0 kW
  if (power_th < 0) power_th <- 0

  if (method == 'curtail') {
    # Sessions with low charging power are not flexible
    sessions_prof[['f']][sessions_prof[['p']] <= power_min] <- 0
  }

  # Calculate demand of profiles
  demand_prof <- get_all_sessions_demand_fast(sessions_prof, setpoint_prof$timeslot)

  # Loop
  while (TRUE) {
    # Flexibility requirements
    flex_req <- filter(
      tibble(
        timeslot = setpoint_prof$timeslot,
        power = demand_prof$demand - setpoint_prof$setpoint
      ),
      .data$power > power_th,
      !(.data$timeslot %in% timeslots_blacklist)
    )
    if (nrow(flex_req) == 0) {
      if (include_log) log <- c(log, "No more flexibility required.")
      break
    }

    # Sessions with flexibility
    flex_sessions <- filter(sessions_prof, .data$f > 0) %>% arrange(.data$chs)
    if (nrow(flex_sessions) == 0) {
      if (include_log) log <- c(log, "No more flexibility available.")
      break
    }

    # Filter flexibility requirements that could be satisfied
    flex_req <- filter(flex_req, .data$timeslot >= min(flex_sessions$chs))
    if (nrow(flex_req) == 0) {
      if (include_log) log <- c(log, "No more flexibility available.")
      break
    }

    # Select the first time slot with flexibility requirements
    flex_timeslot <- flex_req$timeslot[1]
    flex_timeslot_req <- filter(flex_req, .data$timeslot == flex_timeslot) %>% pull(.data$power)

    if (method == 'curtail') {
      flex_timeslot_sessions <- filter(flex_sessions, .data$chs <= flex_timeslot, .data$che > flex_timeslot) %>%
        arrange(desc(.data$f))
    } else if (method == 'postpone') {
      flex_timeslot_sessions <- filter(flex_sessions, .data$chs == flex_timeslot) %>%
        arrange(desc(.data$f))
    }

    if (nrow(flex_timeslot_sessions) == 0) {
      if (include_log) log <- c(log, paste("Time slot", flex_timeslot, "has no flexibility available."))
      timeslots_blacklist <- c(timeslots_blacklist, flex_timeslot)
      next
    }

    if (flex_timeslot == setpoint_prof$timeslot[nrow(setpoint_prof)]) {
      if (include_log) log <- c(log, "Can't expand sessions outside the optimization window")
      break
    }

    if (include_log) log <- c(log, paste("-------------- Flexibility requirement of", flex_timeslot_req, "in timeslot", flex_timeslot, "--------------"))

    if (method == 'curtail') {
      reschedule <- curtail_sessions(
        sessions_prof = sessions_prof, flex_timeslot = flex_timeslot, flex_timeslot_sessions = flex_timeslot_sessions,
        flex_timeslot_req = flex_timeslot_req, power_th = power_th, power_min = power_min, demand_prof = demand_prof,
        log = log, include_log = include_log
      )
      sessions_prof <- reschedule$sessions
      log <- reschedule$log
      demand_prof <- reschedule$demand

    } else if (method == 'postpone') {
      reschedule <- postpone_sessions(
        sessions_prof = sessions_prof, flex_timeslot = flex_timeslot, flex_timeslot_sessions = flex_timeslot_sessions,
        flex_timeslot_req = flex_timeslot_req, power_th = power_th, demand_prof = demand_prof, log = log, include_log = include_log
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
    session_idx <- which(sessions_prof$Session == session$Session)
    sessions_prof$chs[session_idx] <- session$chs + 1
    sessions_prof$che[session_idx] <- session$che + 1
    sessions_prof$f[session_idx] <- session$f - 1
    sessions_prof$Shifted[session_idx] <- session$Shifted + 1

    # Update flexibility requirement and demand
    flex_timeslot_req <- flex_timeslot_req - session$p
    timeslot_idx_less <- which(demand_prof$timeslot == session$chs)
    timeslot_idx_more <- which(demand_prof$timeslot == session$che)
    demand_prof$demand[timeslot_idx_less] <- demand_prof$demand[timeslot_idx_less] - session$p
    demand_prof$demand[timeslot_idx_more] <- demand_prof$demand[timeslot_idx_more] + session$p

    # If the session power is lower than the curtailment power skip this session
    if (flex_timeslot_req <= power_th) {
      if (include_log) log <- c(log, "Setpoint achieved")
      break
    } else {
      if (include_log) log <- c(log, paste(round(flex_timeslot_req, 2), "kW of flexibility required"))
    }
  }
  if (flex_timeslot_req > power_th) {
    if (include_log) log <- c(log, "All sessions exploited")
  }
  return(list(sessions = sessions_prof, log = log, demand = demand_prof))
}


curtail_sessions <- function(sessions_prof, flex_timeslot, flex_timeslot_sessions, flex_timeslot_req, power_th, power_min, demand_prof, log, include_log = F) {
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
      power_original = session$p,
      power_minimum = power_min,
      energy = (session$che - flex_timeslot)*session$p,
      length_timeslots = session$coe - flex_timeslot
    )

    # Remove original session demand
    original_session_connection <- dplyr::between(demand_prof$timeslot, session$cos, session$coe)
    demand_prof$demand[original_session_connection] <- demand_prof$demand[original_session_connection] -
      get_sessions_schedule(session, session$cos, session$coe)

    # Multiple curtailment types

    if (flex_timeslot == session$chs) {
      # 1. Session starts when flexibility is required

      session_curtail <- session
      session_curtail$p <- curtail$power
      session_curtail$che <- session_curtail$chs + curtail$timeslots
      session_curtail$f <- 0
      sessions_prof[session_idx, ] <- session_curtail
      if (include_log) log <- c(log, paste('Full curtailment for session', session$Session))

      demand_prof$demand[original_session_connection] <- demand_prof$demand[original_session_connection] +
        get_sessions_schedule(session_curtail, session$cos, session$coe)

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
      if (include_log) log <- c(log, paste('Partial curtailment for session', session$Session))

      demand_prof$demand[original_session_connection] <- demand_prof$demand[original_session_connection] +
        get_sessions_schedule(dplyr::bind_rows(session_original, session_curtail), session$cos, session$coe)
    }

    # Update flexibility requirement
    flex_timeslot_req <- flex_timeslot_req - (session$p - curtail$power)

    # If the session power is lower than the curtailment power skip this session
    if (flex_timeslot_req <= power_th) {
      if (include_log) log <- c(log, paste("For timeslot", flex_timeslot, "setpoint achieved."))
      break
    } else {
      if (include_log) log <- c(log, paste("Timeslot", flex_timeslot, "still requires", round(flex_timeslot_req, 2), "kW of flexibility"))
    }
  }
  if (flex_timeslot_req > power_th) {
    if (include_log) log <- c(log, "All sessions exploited")
  }
  return(list(sessions = sessions_prof, log = log, demand = demand_prof))
}



# Define the time and power of the curtailment to fit the time resolution of the schedule
get_curtail_parameters <- function(energy, available_timeslots, power_minimum) {
  curtail_timeslots <- energy/power_minimum
  # Curtailment length must be integer and higher than available time slots
  curtail_timeslots_int <- pmax(trunc(curtail_timeslots), available_timeslots)
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
