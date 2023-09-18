
# EV flexibility management -----------------------------------------------

#' Smart charging algorithm
#'
#' @param sessions tibble, sessions data set in standard format marked by `{evprof}` package
#' (see [this article](https://mcanigueral.github.io/evprof/articles/sessions-format.html))
#' @param opt_data tibble, optimization contextual data.
#' The first column must be named `datetime` (mandatory) containing the
#' date time sequence where the smart charging algorithm is applied, so
#' only sessions starting within the time sequence of column `datetime`
#' will be optimized.
#' The other columns can be:
#'
#' - `static`: static power demand (in kW) from other sectors like buildings,
#' offices, etc.
#'
#' - `grid_capacity`: maximum imported power from the grid (in kW),
#' for example the contracted power with the energy company.
#'
#' - `solar`: solar power generation (in kW).
#' This is used when `opt_objective = "grid"`.
#'
#'
#' - `price_imported`: price for imported energy (€/kWh).
#' This is used when `opt_objective = "cost"`.
#'
#' - `price_exported`: price for exported energy (€/kWh).
#' This is used when `opt_objective = "cost"`.
#'
#' If columns of `opt_data` are user profiles names, these are used as setpoints
#' and no optimization is performed for the corresponding user profiles.
#'
#' @param opt_objective character, optimization objective being `"none"`, `"grid"` or `"cost"`.
#' See details section for more information about the different objectives.
#' @param method character, scheduling method being `"none"`, `"postpone"`, `"curtail"` or `"interrupt"`.
#' If `none`, the scheduling part is skipped and the sessions returned in the
#' results will be identical to the original parameter.
#' @param window_length integer, number of data points of the optimization window (not in hours)
#' @param window_start_hour integer, hour to start the optimization window.
#' If `window_start = 6` the EV sessions are optimized from 6:00 to 6:00.
#' @param responsive Named two-layer list with the ratio (between 0 and 1)
#'  of sessions responsive to smart charging program.
#' The names of the list must exactly match the Time-cycle and User profiles names.
#' For example: list(Monday = list(Worktime = 1, Shortstay = 0.1))
#' @param power_th numeric, power threshold (between 0 and 1) accepted from setpoint.
#' For example, with `power_th = 0.1` and `setpoint = 100` for a certain time slot,
#' then sessions' demand can reach a value of `110` without needing to schedule sessions.
#' @param charging_power_min numeric, minimum allowed ratio (between 0 and 1) of nominal power.
#' For example, if `charging_power_min = 0.5` and `method = 'curtail'`, sessions' charging power can only
#' be curtailed until the 50% of the nominal charging power (i.e. `Power` variable in `sessions` tibble).
#' @param include_log logical, whether to output the algorithm messages for every user profile and time-slot
#' @param show_progress logical, whether to output the progress bar in the console
#'
#' @importFrom dplyr tibble %>% filter mutate select everything row_number left_join bind_rows any_of pull distinct
#' @importFrom lubridate hour minute date
#' @importFrom rlang .data
#' @importFrom stats ecdf quantile
#'
#' @return a list with three elements: optimal setpoints, sessions schedule and log messages.
#' @export
#'
#' @details
#' An important parameter of this function is `opt_data`, which defines the time
#' sequence of the smart charging algorithm and the optimization variables.
#' The `opt_data` parameter is directly related with the `opt_objective` parameter.
#' There are two different optimization objectives implemented by this function:
#'
#' - Minimize grid interaction (`opt_objective = "grid"`): performs a quadratic
#' optimization to minimize the peak of the flexible load and the amount of
#' imported power from the grid. If `solar` is not found in `opt_data`, only
#' a peak shaving objective will be considered.
#'
#' - Minimize the energy cost (`opt_objective = "cost"`): performs a linear
#' optimization to minimize the energy cost. In this case, the columns
#' `grid_capacity`, `price_imported` and `price_exported` of tibble `opt_data`
#' are important.
#' If these variables are not configured, default values of `grid_capacity = Inf`,
#' `price_imported = 1` and `price_exported = 0` are considered to minimize the
#' imported energy.
#' For this linear optimization the `grid_capacity` should be configured since
#' all possible power will be allocated in the time slots with lower prices.
#'
#' - No optimization (`opt_objective = "none"`): this will skip optimization.
#' If a user profile name appears in `opt_data` columns, then this will be
#' considered as a setpoint for the scheduling algorithm. If not, then this
#' user profile will not be optimized.
#'
#' @examples
#' # Example: we will use the example data set of charging sessions
#' # from the California Technological Institute (Caltech), obtained
#' # through the [ACN-Data website](https://ev.caltech.edu/dataset).
#' # This data set has been clustered into different user profiles
#' # using the R package `{evprof}`
#' # (see [this article](https://mcanigueral.github.io/evprof/articles/california.html)).
#' # The user profiles of this data set are `Visit` and `Worktime`,
#' # identified in two different time cycles `Workday` and `Weekend`.
#'
#' # These two variables in the `sessions` tibble, `Profile` and `Timecycle`,
#' # are required for the `smart_charging` function and give more versatility
#' # to the smart charging context. For example, we may want to only coordinate
#' # `Worktime` sessions instead of all sessions.
#'
#' # For this example we want the following:
#'
#' # - Postpone only `Worktime` sessions, which have a responsiveness rate of
#' # 0.9 (i.e. 90% of Worktime users accept to postpone the session).
#'
#' # - Minimize the power peak of the sessions (peak shaving)
#'
#' # - Time series resolution of 15 minutes
#'
#' # - Optimization window of 24 hours from 6:00AM to 6:00 AM
#'
#' \dontrun{
#' sessions <- evsim::california_ev_sessions_profiles
#' sessions_demand <- get_demand(sessions, resolution = 15)
#'
#' # Don't require any other variable than datetime, since we don't
#' # care about solar generation (just peak shaving objective)
#' opt_data <- tibble(
#'   datetime = sessions_demand$datetime
#' )
#' smart_charging(
#'   sessions, opt_data, opt_objective = "grid", method = "postpone",
#'   window_length = 24*60/15, window_start_hour = 6,
#'   responsive = list(Workday = list(Worktime = 0.9))
#' )
#' }
#'
#'
smart_charging <- function(sessions, opt_data, opt_objective, method,
                           window_length, window_start_hour, responsive,
                           power_th = 0, charging_power_min = 0.5,
                           include_log = FALSE, show_progress = TRUE) {

  # Check input sessions
  if (is.null(sessions) | nrow(sessions) == 0) {
    message("sessions object is empty.")
    return(NULL)
  }

  # Check optimization data available
  if (!("static" %in% names(opt_data))) {
    opt_data$static <- 0
  }
  if (!("grid_capacity" %in% names(opt_data))) {
    opt_data$grid_capacity <- Inf
  }
  if (opt_objective == "grid") {
    if (!("solar" %in% names(opt_data))) {
      message("Warning: `solar` variable not found in `opt_data`.
              No local genaration will be considered.")
      opt_data$solar <- 0
    }
  }
  if (opt_objective == "cost") {
    if (!("price_imported" %in% names(opt_data))) {
      message("Warning: `price_imported` variable not found in `opt_data`.")
      opt_data$price_imported <- 1
    }
    if (!("price_exported" %in% names(opt_data))) {
      message("Warning: `price_exported` variable not found in `opt_data`.")
      opt_data$price_exported <- 0
    }
  }

  # Datetime optimization parameters according to the window start and length
  window_length <- as.integer(window_length)
  window_start_hour <- as.integer(window_start_hour)
  dttm_seq <- adapt_dttm_seq_to_opt_windows(opt_data$datetime, window_start_hour)
  time_resolution <- as.integer(as.numeric(dttm_seq[2] - dttm_seq[1], unit = 'hours')*60)
  start <- dttm_seq[1]
  end <- dttm_seq[length(dttm_seq)]

  # Adapt the data set for the current time resolution
  sessions <- sessions %>%
    adapt_charging_features(time_resolution = time_resolution) %>%
    mutate(Responsive = FALSE)

  # Get user profiles demand
  if (include_log) message("Getting EV demand profiles")
  profiles_demand <- get_demand(
    sessions, dttm_seq, resolution = time_resolution
  )

  # Initialize setpoints tibble with the user profiles demand
  setpoints <- profiles_demand

  # Filter context data within date time sequence
  opt_data <- opt_data %>%
    filter(.data$datetime %in% dttm_seq)




  # SMART CHARGING ----------------------------------------------------------
  if (include_log) message("Smart charging:")

  log <- list()


  # For each optimization window
  sessions_flex_opt <- tibble()

  if (show_progress) {
    pb <- progress::progress_bar$new(
      format = "[:bar] :percent eta: :eta",
      total = trunc(length(dttm_seq)/window_length)+1
    )
  }

  for (i in seq(1, length(dttm_seq), window_length)) {
    if (show_progress) pb$tick()

    window <- c(i, i+window_length-1)
    log_window_name <- as.character(date(dttm_seq[window[1]]))
    if (include_log) {
      message(paste("--", log_window_name))
      log[[log_window_name]] <- list()
    }

    # Filter only sessions that START CHARGING within the time window
    sessions_window <- sessions %>%
      filter(
        .data$ChargingStartDateTime >= dttm_seq[window[1]],
        .data$ChargingStartDateTime < dttm_seq[window[2]]
      )
    if (nrow(sessions_window) == 0) next

    # Window's features
    # Find most common time-cycle in this window
    window_timecycle <- names(sort(table(sessions_window$Timecycle), decreasing = TRUE))[1]
    # Responsiveness of the user profiles in this time-cycle
    window_responsive <- responsive[[window_timecycle]]

    # Profiles subjected to optimization:
    #   1. appearing in the sessions set for this optimization window
    #   2. responsive values higher than 0
    opt_profiles <- names(window_responsive)[
      (names(window_responsive) %in% unique(sessions_window$Profile)) &
        (names(window_responsive) %in% names(window_responsive)[as.numeric(window_responsive) > 0])
    ]

    # For each optimization profile
    for (profile in opt_profiles) {

      if (include_log) message(paste("----", profile))

      # Filter only sessions of this Profile
      sessions_window_prof <- sessions_window %>%
        filter(.data$Profile == profile)

      # The smart charging algorithm only allows moving demand within the
      # time-window, so:
      #   1. Limit the CONNECTION END TIME of sessions that FINISH CHARGING BEFORE the window end
      sessions_window_prof$ConnectionEndDateTime[
        (sessions_window_prof$ConnectionEndDateTime >= dttm_seq[window[2]]) &
          (sessions_window_prof$ChargingEndDateTime <= dttm_seq[window[2]])
      ] <- dttm_seq[window[2]]
      #   2. Recalculate flexibility with new end connection times
      sessions_window_prof$Flexibility <- round(as.numeric(
        sessions_window_prof$ConnectionEndDateTime - sessions_window_prof$ChargingEndDateTime,
        unit = "hours"
      ), 2)
      #   3. Discard flexibility of sessions that FINISH CHARGING AFTER the window end
      sessions_window_prof$Flexibility[
        sessions_window_prof$ChargingEndDateTime >= dttm_seq[window[2]]
      ] <- 0

      # Re-define window to profile's connection window
      # # Find the End time for at least 95% of sessions
      # ss_ecdf <- ecdf(sessions_window_prof$coe)
      # ss_coe_ecdf <- round(ss_ecdf(knots(ss_ecdf)), 1)
      # ss_coe_90 <- knots(ss_ecdf)[ss_coe_ecdf == 0.95][1]
      # window_prof <- c(min(sessions_window_prof$cos), ss_coe_75)
      window_prof_dttm <- c(
        min(sessions_window_prof$ConnectionStartDateTime),
        min(max(sessions_window_prof$ConnectionEndDateTime), dttm_seq[window[2]])
      )
      window_prof_idxs <- (dttm_seq >= window_prof_dttm[1]) &
        (dttm_seq <= window_prof_dttm[2])
      window_prof_length <- sum(window_prof_idxs)

      # Separate between flexible sessions or not
      # Flexible sessions must have more flexibility hours than one time slot
      set.seed(1234)
      sessions_window_prof[["Responsive"]] <- sample(
        c(T, F),
        nrow(sessions_window_prof),
        replace = T,
        prob = c(window_responsive[[profile]], (1-window_responsive[[profile]]))
      )
      sessions_window_prof_flex <- sessions_window_prof %>%
        filter(.data$Responsive & .data$Flexibility >= time_resolution/60)
      non_flexible_sessions <- sessions_window_prof %>%
        filter(!.data$Responsive | .data$Flexibility < time_resolution/60)

      #   Profile sessions that can't provide flexibility are not part of the setpoint
      if (nrow(non_flexible_sessions) > 0) {
        L_fixed_prof <- non_flexible_sessions %>%
          get_demand(dttm_seq = dttm_seq[window_prof_idxs]) %>%
          pull(!!sym(profile))
      } else {
        L_fixed_prof <- rep(0, window_prof_length)
      }


      # OPTIMIZATION  ----------------------------------------------------------

      if (opt_objective != "none") {

        # If `opt_data` contains user profile's name,
        # this is considered to be a setpoint (skip optimization)
        if (!(profile %in% colnames(opt_data[-1]))) {

          if (include_log) message("------ Optimization")

          # The optimization static load consists on:
          #   - Environment fixed load (buildings, lightning, etc)
          if ('static' %in% colnames(opt_data)) {
            L_fixed <- opt_data$static[window_prof_idxs]
          } else {
            L_fixed <- rep(0, window_prof_length)
          }
          #   - Other profiles load
          L_others <- setpoints %>%
            filter(window_prof_idxs) %>%
            select(- any_of(c(profile, 'datetime'))) %>%
            rowSums()
          if (length(L_others) == 0) {
            L_others <- rep(0, window_prof_length)
          }
          # The optimization flexible load is the load of the responsive sessions
          L_prof <- setpoints[[profile]][window_prof_idxs] - L_fixed_prof

          # Optimize the flexible profile's load according to `opt_objective`
          if (opt_objective == "grid") {
            O <- minimize_grid_flow_window(
              G = opt_data$solar[window_prof_idxs],
              LF = L_prof,
              LS = L_fixed + L_others + L_fixed_prof,
              direction = 'forward',
              time_horizon = NULL,
              LFmax = Inf,
              grid_capacity = opt_data$grid_capacity[window_prof_idxs]
            )
          } else if (opt_objective == "cost") {
            O <- minimize_cost_window(
              G = opt_data$solar[window_prof_idxs],
              LF = L_prof,
              LS = L_fixed + L_others + L_fixed_prof,
              PI = opt_data$price_imported[window_prof_idxs],
              PE = opt_data$price_exported[window_prof_idxs],
              direction = 'forward',
              time_horizon = NULL,
              LFmax = Inf,
              grid_capacity = opt_data$grid_capacity[window_prof_idxs]
            )
          }

          setpoints[[profile]][window_prof_idxs] <- O + L_fixed_prof
        } else {
          setpoints[[profile]][window_prof_idxs] <- opt_data[[profile]][window_prof_idxs]
        }
      } else {
        if (profile %in% colnames(opt_data[-1])) {
          setpoints[[profile]][window_prof_idxs] <- opt_data[[profile]][window_prof_idxs]
        }
      }


      # SCHEDULING ----------------------------------------------------------
      if (method != "none") {
        setpoint_prof <- tibble(
          datetime = dttm_seq[window_prof_idxs],
          setpoint = setpoints[[profile]][window_prof_idxs] - L_fixed_prof
        )

        if (include_log) message("------ Scheduling")

        results <- schedule_sessions(
          sessions = sessions_window_prof_flex, setpoint = setpoint_prof,
          method = method, power_th = power_th, charging_power_min = charging_power_min,
          include_log = include_log, show_progress = FALSE
        )

        sessions_flex_opt <- bind_rows(
          sessions_flex_opt,
          results$sessions
        )
        if (include_log) {
          log[[log_window_name]][[profile]] <- results$log
        }
      }
    }
  }

  if (method == "none") {
    sessions_opt <- sessions
  } else {

    # Join the sessions that have been exploited with the non-flexible ones
    sessions_not_exploited <- sessions[!(sessions$Session %in% sessions_flex_opt$Session), ]

    sessions_opt <- bind_rows(
      sessions_not_exploited,
      sessions_flex_opt
    ) %>%
      select(any_of(names(sessions)), everything()) %>% # Set order of variables
      arrange(.data$Session, .data$ConnectionStartDateTime, .data$ConnectionEndDateTime) %>%
      distinct()

  }

  return(list(
    setpoints = setpoints,
    sessions = sessions_opt,
    log = log
  ))
}




#' Schedule sessions according to optimal setpoint
#'
#' @param sessions tibble, sessions data set normalized from `normalize_sessions` function
#' @param setpoint tibble with columns `datetime`, `timeslot` and rest of columns being the names of user profiles
#' @param method character, being `postpone`, `curtail` or `interrupt`.
#' @param power_th numeric, power threshold (between 0 and 1) accepted from setpoint.
#' For example, with `power_th = 0.1` and `setpoint = 100` for a certain time slot,
#' then sessions' demand can reach a value of `110` without needing to schedule sessions.
#' @param charging_power_min numeric, minimum allowed ratio (between 0 and 1) of nominal power.
#' For example, if `charging_power_min = 0.5` and `method = 'curtail'`, sessions' charging power can only
#' be curtailed until the 50% of the nominal charging power (i.e. `Power` variable in `sessions` tibble).
#' @param include_log logical, whether to output the algorithm messages for every user profile and time-slot
#' @param show_progress logical, whether to output the progress bar in the console
#'
#' @return list of two elements `sessions` and `log`
#' @export
#'
#' @importFrom dplyr tibble %>% filter mutate arrange desc
#' @importFrom rlang .data
#'
schedule_sessions <- function(sessions, setpoint, method,
                              power_th = 0, charging_power_min = 0.5,
                              include_log = FALSE, show_progress = TRUE) {

  log <- c()

  resolution <- as.integer(as.numeric(setpoint$datetime[2] - setpoint$datetime[1], unit = 'hours')*60)
  sessions_expanded <- sessions %>%
    expand_sessions(resolution = resolution) %>%
    mutate(
      Power = 0,
      EnergyLeft = .data$RequiredEnergy,
      Exploited = FALSE
    )

  if (show_progress) {
    pb <- progress::progress_bar$new(
      format = "[:bar] :percent eta: :eta",
      total = length(setpoint$datetime)
    )
  }

  for (timeslot in setpoint$datetime) {
    if (show_progress) pb$tick()

    # Filter sessions that are charging (`EnergyLeft > 0`) during this time slot
    # and calculate their average charging power `AveragePower`
    sessions_timeslot <- sessions_expanded %>%
      filter(.data$Timeslot == timeslot, .data$EnergyLeft > 0) %>%
      mutate(
        AveragePower = pmin(
          .data$EnergyLeft/(resolution/60), .data$NominalPower
        )
      )

    if (nrow(sessions_timeslot) == 0) {
      next
    }

    sessions_timeslot_power <- sum(sessions_timeslot$AveragePower)

    setpoint_timeslot_power <- setpoint$setpoint[
      setpoint$datetime == timeslot
    ]

    flex_req <- round(sessions_timeslot_power - setpoint_timeslot_power * (1 + power_th), 2)

    # If demand should be reduced
    if (flex_req > 0) {

      if (include_log) {
        log_message <- paste(
          timeslot,
          "- Flexibility requirement of", flex_req, "kW and",
          nrow(sessions_timeslot), "sessions connected."
        )

        message(log_message)
        log <- c(
          log,
          log_message
        )
      }

      if (method %in% c("postpone", "interrupt")) {

        # Time flexibility (Postpone / Interrupt) ----------------------------------------------------------------

        # Filter shiftable sessions and arrange them by Flexibility potential
        # 1. For postpone, sessions must have to start charging
        #     on this time slot (EnergyLeft == RequiredEnergy).
        #     For interrupt this constraint is not necessary
        # 2. Flexible enough to be shifted one time slot (FlexibilityHours >= resolution/60)
        if (method == "postpone") {
          sessions_timeslot <- sessions_timeslot %>%
            mutate(
              Shiftable = ifelse(
                (.data$EnergyLeft == .data$RequiredEnergy) & (.data$FlexibilityHours > resolution/60), TRUE, FALSE
              )
            )
        } else {
          sessions_timeslot <- sessions_timeslot %>%
            mutate(
              Shiftable = ifelse(
                .data$FlexibilityHours > resolution/60, TRUE, FALSE
              )
            )
        }


        # Get the maximum flexible power of this time slot
        shift_flex_available <- sum(sessions_timeslot$AveragePower[sessions_timeslot$Shiftable])

        # If the available flexibility is higher than the flexibility request,
        # then allow the sessions with less flexibility to charge.
        # Else, shift all sessions.
        if (shift_flex_available >= flex_req) {

          sessions_timeslot_shiftable <- sessions_timeslot %>%
            filter(.data$Shiftable) %>%
            arrange(desc(.data$FlexibilityHours))

          # Get the index of sessions that can charge
          allowed_to_charge_idx <- rep(FALSE, nrow(sessions_timeslot_shiftable))
          for (s in seq_len(nrow(sessions_timeslot_shiftable))) {
            flex_req <- pmax(flex_req - sessions_timeslot_shiftable$AveragePower[s], 0)
            if (flex_req == 0) break
          }
          allowed_to_charge_idx[seq(s+1, length(allowed_to_charge_idx))] <- TRUE

          # Update the `Exploited` variable to `TRUE` for sessions to shift
          sessions_timeslot$Exploited[
            sessions_timeslot$ID %in% sessions_timeslot_shiftable$ID[!allowed_to_charge_idx]
          ] <- TRUE

        } else {

          # All `Shiftable` sessions are `Exploited`
          sessions_timeslot$Exploited <- sessions_timeslot$Shiftable

          # Update flexibility requirement with power from all shiftable sessions
          flex_req <- round(flex_req - shift_flex_available, 2)

          if (include_log) {
            log_message <- paste0(
              " -- Not enough flexibility available (", shift_flex_available, " kW)"
            )
            message(log_message)
            log <- c(
              log,
              log_message
            )
          }

        }

        # Update the charging power of sessions that are NOT shifted
        sessions_timeslot$Power[!sessions_timeslot$Exploited] <-
          sessions_timeslot$AveragePower[!sessions_timeslot$Exploited]


      }

      if ("curtail" %in% method) {

        # Power flexibility (Curtail) ----------------------------------------------------------------

        # Curtailable sessions: flexible enough to be shifted one time slot
        # (FlexibilityHours >= resolution/60)
        sessions_timeslot <- sessions_timeslot %>%
          mutate(
            Curtailable = ifelse(
              .data$FlexibilityHours > resolution/60, TRUE, FALSE
            )
          )

        # Get the power demand from curtailable sessions
        curtailable_sessions_power <- sum(
          sessions_timeslot$AveragePower[sessions_timeslot$Curtailable]
        )

        # Power reduction for all sessions, limited by the minimum percentage
        # allowed of the nominal charging power (`charging_power_min`)
        power_reduction <- (curtailable_sessions_power - flex_req)/curtailable_sessions_power

        if (power_reduction < 0) {

          power_reduction <- charging_power_min

          if (include_log) {
            log_message <- paste0(
              " -- Curtail: not enough flexibility available (",
              curtailable_sessions_power - curtailable_sessions_power*power_reduction,
              " kW). Charging all sessions at a ", charging_power_min*100, "% of nominal power."
            )
            message(log_message)
            log <- c(
              log,
              log_message
            )
          }

        }

        # All `Curtailable` sessions are `Exploited`
        sessions_timeslot$Exploited <- sessions_timeslot$Curtailable

        # Update the charging power of curtailed sessions
        sessions_timeslot$Power[sessions_timeslot$Exploited] <-
          sessions_timeslot$AveragePower[sessions_timeslot$Exploited]*
          power_reduction

        # Update the charging power of sessions that are NOT curtailed
        sessions_timeslot$Power[!sessions_timeslot$Exploited] <-
          sessions_timeslot$AveragePower[!sessions_timeslot$Exploited]

        # Update flexibility requirement
        flex_req <- round(flex_req - (curtailable_sessions_power - curtailable_sessions_power*power_reduction), 2)

      }

    } else {

      if (include_log) {
        log_message <- paste(
          timeslot,
          "- No flexibility required"
        )

        message(log_message)
        log <- c(
          log,
          log_message
        )
      }

      # No flexibility required: charge all sessions
      sessions_timeslot <- sessions_timeslot %>%
        mutate(Power = .data$NominalPower)

    }


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
      session_energy <- round(pmin(sessions_timeslot$Power[s]*resolution/60, sessions_timeslot$EnergyLeft[s]), 2)
      session_power <- round(pmin(session_energy/(resolution/60), sessions_timeslot$NominalPower[s]), 2)
      sessions_expanded$Power[
        sessions_expanded$ID == sessions_timeslot$ID[s]
      ] <- session_power
      sessions_expanded$Exploited[
        sessions_expanded$ID == sessions_timeslot$ID[s]
      ] <- sessions_timeslot$Exploited[s]

      # If there are more time slots following, update `EnergyLeft` and `FlexibilityHours`
      session_after_idx <- (sessions_expanded$Session == sessions_timeslot$Session[s]) &
        sessions_expanded$ID > sessions_timeslot$ID[s]

      if (length(session_after_idx) > 0) {

        # Update `EnergyLeft`
        session_energy_left <- round(sessions_timeslot$EnergyLeft[s] - session_energy, 2)
        sessions_expanded$EnergyLeft[session_after_idx] <- session_energy_left

        # Update `FlexibilityHours`
        session_flexibility_hours <- round(pmax(sessions_timeslot$FlexibilityHours[s] - resolution/60, 0), 2)
        sessions_expanded$FlexibilityHours[session_after_idx] <- session_flexibility_hours

      } else {

        session_energy_left <- 0
        session_flexibility_hours <- 0

      }

    }

    # Log message ----------------------------------------------------------------

    if (include_log) {

      exploited_sessions <- sessions_timeslot %>% filter(.data$Exploited)

      for (s in seq_len(nrow(exploited_sessions))) {
        power_reduction <- round(exploited_sessions$AveragePower[s] - exploited_sessions$Power[s], 2)
        pct_power_reduction <- round(power_reduction/exploited_sessions$AveragePower[s]*100)
        session_flexibility_hours <- round(pmax(exploited_sessions$FlexibilityHours[s] - resolution/60, 0), 2)
        original_flexibility <- sessions$FlexibilityHours[sessions$Session == exploited_sessions$Session[s]]
        pct_flexibility_available <- round(session_flexibility_hours/original_flexibility*100)

        log_message <- paste0(
          "---- Session ", exploited_sessions$Session[s],
          " provides ", power_reduction, " kW of flexibility (power reduction of ",
          pct_power_reduction, "%).",
          " Flexibility still available: ", pct_flexibility_available, "% of original."
        )

        message(log_message)
        log <- c(
          log,
          log_message
        )
      }

    }

  }

  # Update the sessions data set with all variables from the original data set
  sessions_expanded2 <- left_join(
    sessions_expanded %>%
      select('Session', 'Timeslot', 'Power'),
    sessions %>%
      select('Session', !any_of(c(
        'ConnectionStartDateTime', 'ConnectionEndDateTime', 'ConnectionHours',
        'ChargingStartDateTime', 'ChargingEndDateTime', 'ChargingHours',
        'Power', 'Energy', 'FlexibilityHours'
      ))),
    by = 'Session'
  ) %>%
    mutate(
      ConnectionStartDateTime = .data$Timeslot,
      ConnectionEndDateTime = .data$Timeslot + minutes(resolution),
      ChargingStartDateTime = .data$Timeslot,
      ChargingEndDateTime = .data$Timeslot + minutes(resolution),
      ConnectionHours = resolution/60,
      ChargingHours = resolution/60,
      Energy = .data$Power*.data$ChargingHours,
      FlexibilityHours = 0,
    ) %>%
    select(any_of(names(sessions)), -"Timeslot")

  return(
    list(
      sessions = sessions_expanded2,
      log = log
    )
  )
}



#' #' Simulate charging power limitation based on grid capacity signals
#' #'
#' #' @param sessions tibble, charging sessions data set.
#' #' If sessions have been expanded using `expand_session` function, set parameter `expand = FALSE`.
#' #' @param grid_capacity tibble, grid capacity time-series with columns `datetime` and `max_amps` per phase
#' #' @param amps_car_min integer, minimum current to charge the vehicles
#' #' @param expand logical, whether to expand session using `expand_session` function
#' #' @param include_log logical, whether to print and return log messages
#' #'
#' #' @return tibble
#' #' @export
#' #'
#' #' @importFrom dplyr %>% tibble mutate select all_of row_number filter group_by summarise
#' #' @importFrom lubridate as_datetime tz
#' #' @importFrom rlang .data
#' #'
#' simulate_flexpower <- function(sessions, grid_capacity, amps_car_min = 8, expand = TRUE, include_log = FALSE) {
#'   log_messages <- c()
#'
#'   if (expand) {
#'     resolution <- as.integer(as.numeric(grid_capacity$datetime[2] - grid_capacity$datetime[1], unit = 'hours')*60)
#'     sessions_expanded <- sessions %>%
#'       split(1:nrow(sessions)) %>%
#'       map_dfr(
#'         ~ expand_session(.x, resolution = resolution)
#'       ) %>%
#'       mutate(ID = row_number())
#'   } else {
#'     sessions_expanded <- sessions
#'   }
#'
#'
#'   for (timeslot in grid_capacity$datetime) {
#'
#'     # Sessions that are charging (EnergyLeft > 0) during this time slot
#'     sessions_timeslot <- sessions_expanded %>%
#'       filter(.data$Timeslot == timeslot, .data$EnergyLeft > 0) %>%
#'       mutate(ChargingPointLimit = 16)
#'
#'     if (nrow(sessions_timeslot) == 0) {
#'       next
#'     }
#'
#'     # Find the maximum current allowed by the MSR per charging socket
#'     phases_used_by_chpoint <- sessions_timeslot %>%
#'       group_by(.data$ChargingPoint) %>%
#'       summarise(n_phases = sum(.data$Phases)) %>%
#'       mutate(n_sessions = ifelse(.data$n_phases <= 3, 1, 2))
#'     n_times_phases_used <- sum(phases_used_by_chpoint$n_sessions)
#'
#'     amps_msr_max <- grid_capacity$max_amps[
#'       grid_capacity$datetime == timeslot
#'     ]/n_times_phases_used
#'
#'     # Find the maximum current allowed by every charging point per charging socket
#'     for (chpoint in unique(sessions_timeslot$ChargingPoint)) {
#'       sessions_chpoint <- sessions_timeslot %>%
#'         filter(.data$ChargingPoint == chpoint)
#'
#'       if (nrow(sessions_chpoint) > 2) {
#'         message(paste(
#'           "Warning: more than 2 sessions in charging point",
#'           chpoint, "at", sessions_timeslot$Timeslot[1]
#'         ))
#'         print(sessions_chpoint)
#'         sessions_chpoint <- sessions_chpoint[c(1, 2), ]
#'       }
#'
#'       amps_charging_point_max <- ifelse(
#'         sum(sessions_chpoint$Phases) <= 3, 16, 12.5
#'       )
#'
#'       sessions_timeslot$ChargingPointLimit[
#'         sessions_timeslot$ChargingPoint == chpoint
#'       ] <- amps_charging_point_max
#'     }
#'
#'     # Charging current of session, comparing:
#'     #   1. Charging point limit
#'     #   2. MSR limit
#'     #   4. Minimum charging current per vehicle (typically 8A). The equivalence
#'     #      of the rotation system is to divide minimum current by the number
#'     #      of charging cars during that time slot.
#'     sessions_timeslot <- sessions_timeslot %>%
#'       mutate(
#'         Amps = max(
#'           min(.data$ChargingPointLimit, amps_msr_max),
#'           amps_car_min/nrow(sessions_timeslot)
#'         )
#'       )
#'
#'     # For every session update in `sessions` table
#'     #   1. The charging power during THIS time slot (session id == `ID`)
#'     #   2. The energy left for the ALL time slots of the session (session id == `Session`)
#'     for (s in 1:nrow(sessions_timeslot)) {
#'       session_power <- sessions_timeslot$Amps[s]*230*sessions_timeslot$Phases[s]/1000
#'       session_energy <- session_power*resolution/60
#'
#'       # If energy left is less than the energy that can be charged in this
#'       # time slot, then only charge the energy left.
#'       if (sessions_timeslot$EnergyLeft[s] < session_energy) {
#'         session_power <- sessions_timeslot$EnergyLeft[s]/(resolution/60)
#'         session_energy <- sessions_timeslot$EnergyLeft[s]
#'       }
#'
#'       if (include_log) {
#'         log_message <- paste(
#'           as_datetime(timeslot, tz = tz(grid_capacity$datetime)),
#'           "- Session", sessions_timeslot$Session[s],
#'           "at", session_power, "kW. Charged",
#'           session_energy, "kWh of",
#'           sessions_timeslot$EnergyLeft[s], "kWh required"
#'         )
#'
#'         message(log_message)
#'         log_messages <- c(
#'           log_messages,
#'           log_message
#'         )
#'       }
#'
#'       # Update power
#'       sessions_expanded$Power[
#'         sessions_expanded$ID == sessions_timeslot$ID[s]
#'       ] <- session_power
#'
#'       # Update energy
#'       session_energy_idx <- sessions_expanded$Session == sessions_timeslot$Session[s]
#'       sessions_expanded$EnergyLeft[session_energy_idx] <-
#'         round(sessions_timeslot$EnergyLeft[s] - session_energy, 2)
#'     }
#'   }
#'
#'   return(
#'     list(
#'       sessions = sessions_expanded,
#'       log = log_messages
#'     )
#'   )
#' }
