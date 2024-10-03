
# EV flexibility management -----------------------------------------------

#' Smart charging algorithm
#'
#' This function provides a framework to simulate different smart charging methods
#' (i.e. postpone, interrupt and curtail) to reach multiple goals (e.g. grid congestion,
#' net power minimization, cost minimization). See implementation examples and
#' the formulation of the optimization problems in the
#' [documentation website](https://mcanigueral.github.io/flextools/).
#'
#' @param sessions tibble, sessions data set containig the following variables:
#' `"Session"`, `"Timecycle"`, `"Profile"`, `"ConnectionStartDateTime"`, `"ConnectionHours"`, `"Power"` and `"Energy"`.
#'
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
#' - `production`: local power generation (in kW).
#' This is used when `opt_objective = "grid"`.
#'
#' - `price_imported`: price for imported energy (€/kWh).
#' This is used when `opt_objective = "cost"`.
#'
#' - `price_exported`: price for exported energy (€/kWh).
#' This is used when `opt_objective = "cost"`.
#'
#' - `price_turn_down`: price for turn-down energy use (€/kWh).
#' This is used when `opt_objective = "cost"`.
#'
#' - `price_turn_up`: price for turn-up energy use (€/kWh).
#' This is used when `opt_objective = "cost"`.
#'
#' If columns of `opt_data` are user profiles names, these are used as setpoints
#' and no optimization is performed for the corresponding user profiles.
#'
#' @param opt_objective character, optimization objective being `"none"`, `"grid"`,
#'  `"cost"` or a value between 0 (cost) and 1 (grid).
#' See details section for more information about the different objectives.
#' @param method character, scheduling method being `"none"`, `"postpone"`, `"curtail"` or `"interrupt"`.
#' If `none`, the scheduling part is skipped and the sessions returned in the
#' results will be identical to the original parameter.
#' @param window_days integer, number of days to consider as optimization window.
#' @param window_start_hour integer, starting hour of the optimization window.
#' @param responsive Named two-level list with the ratio (between 0 and 1)
#'  of sessions responsive to smart charging program.
#' The names of the list must exactly match the Time-cycle and User profiles names.
#' For example: `list(Monday = list(Worktime = 1, Shortstay = 0.1))`
#' @param power_th numeric, power threshold (between 0 and 1) accepted from setpoint.
#' For example, with `power_th = 0.1` and `setpoint = 100` for a certain time slot,
#' then sessions' demand can reach a value of `110` without needing to schedule sessions.
#' @param charging_power_min numeric, minimum allowed ratio (between 0 and 1) of nominal power.
#' For example, if `charging_power_min = 0.5` and `method = 'curtail'`, sessions' charging power can only
#' be curtailed until the 50% of the nominal charging power (i.e. `Power` variable in `sessions` tibble).
#' @param energy_min numeric, minimum allowed ratio (between 0 and 1) of required energy.
#' @param include_log logical, whether to output the algorithm messages for every user profile and time-slot
#' @param show_progress logical, whether to output the progress bar in the console
#' @param lambda numeric, penalty on change for the flexible load.
#' @param mc.cores integer, number of cores to use.
#' Must be at least one, and parallelization requires at least two cores.
#'
#' @importFrom dplyr tibble %>% filter mutate select everything row_number left_join bind_rows any_of pull distinct between sym all_of
#' @importFrom lubridate hour minute date
#' @importFrom rlang .data
#' @importFrom stats sd
#' @importFrom purrr set_names
#' @importFrom evsim get_demand adapt_charging_features
#'
#' @return a list with three elements:
#' optimal setpoints (tibble), sessions schedule (tibble) and log messages
#' (list with character strings). The date-time values in the log list are in
#' the time zone of the `opt_data`.
#' @export
#'
#' @details
#' An important parameter of this function is `opt_data`, which defines the time
#' sequence of the smart charging algorithm and the optimization variables.
#' The `opt_data` parameter is directly related with the `opt_objective` parameter.
#' There are three different optimization objectives implemented by this function:
#'
#' - Minimize grid interaction (`opt_objective = "grid"`): minimizes the peak of
#' the flexible load and the amount of imported power from the grid.
#' If `production` is not found in `opt_data`, only a peak shaving objective
#' will be considered.
#'
#' - Minimize the energy cost (`opt_objective = "cost"`): minimizes the energy cost.
#' In this case, the columns
#' `grid_capacity`, `price_imported`, `price_exported`,
#' `price_turn_up` and `price_turn_down` of tibble `opt_data` are important.
#' If these variables are not configured, default values of `grid_capacity = Inf`,
#' `price_imported = 1`, `price_exported = 0`, `price_turn_up = 0` and
#' `price_turn_down = 0` are considered to just minimize the imported energy.
#'
#' - Combined optimization (`opt_objective` between `0` and `1`): minimizes both
#' the net power peaks and energy cost.
#'
#' - No optimization (`opt_objective = "none"`): this will skip optimization and
#' at least one user profile name must be in an `opt_data` column to be
#' considered as a setpoint for the scheduling algorithm. The user profiles that
#' don't appear in `opt_data` will not be optimized.
#'
#' @examples
#' # Example: we will use the example data set of charging sessions
#' # from the `evsim` package.
#'
#' # The user profiles of this data set are `Visit` and `Worktime`,
#' # identified in two different time cycles `Workday` and `Weekend`.
#' # These two variables in the `sessions` tibble, `Profile` and `Timecycle`,
#' # are required for the `smart_charging` function and give more versatility
#' # to the smart charging context. For example, we may want to only coordinate
#' # `Worktime` sessions instead of all sessions.
#'
#' # For this example we want the following:
#' # - Curtail only `Worktime` sessions, which have a responsiveness rate of
#' # 0.9 (i.e. 90% of Worktime users accept to postpone the session).
#' # - Minimize the power peak of the sessions (peak shaving)
#' # - Time series resolution of 15 minutes
#' # - Optimization window of 24 hours from 6:00AM to 6:00 AM
#' # - The energy charged can be reduced up to 50% of the original requirement
#'
#' library(dplyr)
#' sessions <- evsim::california_ev_sessions_profiles %>%
#'   slice_head(n = 50) %>%
#'   evsim::adapt_charging_features(time_resolution = 15)
#' sessions_demand <- evsim::get_demand(sessions, resolution = 15)
#'
#' # Don't require any other variable than datetime, since we don't
#' # care about local generation (just peak shaving objective)
#' opt_data <- tibble(
#'   datetime = sessions_demand$datetime,
#'   production = 0
#' )
#' sc_results <- smart_charging(
#'   sessions, opt_data, opt_objective = "grid", method = "curtail",
#'   window_days = 1, window_start_hour = 6,
#'   responsive = list(Workday = list(Worktime = 0.9)),
#'   energy_min = 0.5
#' )
#'
#'
smart_charging <- function(sessions, opt_data, opt_objective, method,
                           window_days, window_start_hour,
                           responsive = NULL, power_th = 0,
                           charging_power_min = 0, energy_min = 1,
                           include_log = FALSE, show_progress = FALSE,
                           lambda = 0, mc.cores = 1) {

  # Parameters check
  if (is.null(sessions) | nrow(sessions) == 0) {
    message("Error: `sessions` parameter is empty.")
    return( NULL )
  }
  sessions_basic_vars <- c(
    "Session", "Timecycle", "Profile", "ConnectionStartDateTime",
    "ConnectionHours", "Power", "Energy"
  )
  if (!all(sessions_basic_vars %in% colnames(sessions))) {
    message("Error: `sessions` does not contain all required variables (see Arguments description)")
    return( NULL )
  }
  if (is.null(opt_data)) {
    return( NULL )
  }
  if (!("datetime" %in% colnames(opt_data))) {
    message("Error: `opt_data` does not contain `datetime` variable")
    return( NULL )
  }
  if (!any(sessions$ConnectionStartDateTime %in% opt_data$datetime)) {
    message("Error: `sessions` do not charge during `datetime` period in `opt_data`")
    return( NULL )
  }
  if (opt_objective != "none") {
    opt_data$flexible <- 0
    opt_data <- check_optimization_data(opt_data, opt_objective)
  } else {
    if (!any(unique(sessions$Profile) %in% names(opt_data))) {
      message("Error: when `opt_objective` = 'none', at least one EV user profile from `sessions` must appear as a column in `opt_data` to be considered as a setpoint")
      return( NULL )
    }
  }
  if (is.null(responsive)) {
    responsive <- map(
      set_names(unique(sessions$Timecycle)),
      ~ map(
        set_names(unique(sessions$Profile)),
        ~ 1
      )
    )
  } else {
    responsive_time_cycles <- names(responsive)
    responsive_user_profiles <- unlist(map(responsive, names))
    sessions_time_cycles <- unique(sessions$Timecycle)
    sessions_user_profiles <- unique(sessions$Profile)

    # Check that all content in `responsive` match the content in `sessions`
    if (!all(responsive_time_cycles %in% sessions_time_cycles)) {
      message("Error: time cycle name in `responsive` not found in `sessions`")
      return( NULL )
    }
    if (!all(responsive_user_profiles %in% sessions_user_profiles)) {
      message("Error: user profile name in `responsive` not found in `sessions`")
      return( NULL )
    }
  }


  # Adapt the data set for the current time resolution
  dttm_seq <- opt_data$datetime
  time_resolution <- get_time_resolution(dttm_seq, units = "mins")
  sessions <- sessions %>%
    adapt_charging_features(time_resolution = time_resolution) %>%
    mutate(
      Responsive = FALSE
    )


  # Optimization windows according to `window_days` and `window_start_hour`
  flex_windows_idx <- get_flex_windows(
    dttm_seq, window_days, window_start_hour
  )



  # Get user profiles demand
  if (include_log) message("Getting EV demand profiles")
  profiles_demand <- get_demand(
    sessions, dttm_seq, mc.cores = mc.cores
  )

  # Initialize setpoints tibble with the user profiles demand
  setpoints <- profiles_demand


  # SMART CHARGING ----------------------------------------------------------
  if (include_log) message("Smart charging:")
  log <- list()
  sessions_flex_opt <- tibble()

  if (show_progress) {
    pb <- progress::progress_bar$new(
      format = "[:bar] :percent eta: :eta",
      total = nrow(flex_windows_idx)
    )
  }

  # For each optimization window
  for (i in seq_len(nrow(flex_windows_idx))) {
    if (show_progress) pb$tick()

    window <- c(flex_windows_idx$start[i], flex_windows_idx$end[i])
    log_window_name <- as.character(date(dttm_seq[window[1]]))
    if (include_log) {
      message(paste("--", log_window_name))
      log[[log_window_name]] <- list()
    }

    # Filter only sessions that START CHARGING within the time window
    sessions_window <- sessions %>%
      filter(
        .data$ChargingStartDateTime >= dttm_seq[window[1]],
        .data$ChargingStartDateTime < dttm_seq[window[2]+1]
      )
    if (nrow(sessions_window) == 0) next

    # Window's features
    # Find most common time-cycle in this window
    window_timecycle <- names(sort(table(sessions_window$Timecycle), decreasing = TRUE))[1]
    # Responsiveness of the user profiles in this time-cycle
    # If the time cycle is not configured in `responsive` then skip smart charging
    if (!(window_timecycle %in% names(responsive))) next
    window_responsive <- responsive[[window_timecycle]]

    if (length(window_responsive) == 0) next

    # Profiles subjected to optimization:
    #   1. appearing in the sessions set for this optimization window
    #   2. responsive values higher than 0
    opt_profiles <- names(window_responsive)[
      (names(window_responsive) %in% unique(sessions_window$Profile)) &
        (names(window_responsive) %in% names(window_responsive)[as.numeric(window_responsive) > 0])
    ]

    if (length(opt_profiles) == 0) next

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
        (sessions_window_prof$ConnectionEndDateTime >= dttm_seq[window[2]+1]) &
          (sessions_window_prof$ChargingEndDateTime <= dttm_seq[window[2]+1])
      ] <- dttm_seq[window[2]+1]
      #   2. Recalculate flexibility with new connection duration
      sessions_window_prof$ConnectionHours <- round(as.numeric(
        sessions_window_prof$ConnectionEndDateTime - sessions_window_prof$ConnectionStartDateTime,
        unit = "hours"
      ), 2)
      sessions_window_prof$FlexibilityHours <- round(
        sessions_window_prof$ConnectionHours - sessions_window_prof$ChargingHours, 2
      )
      #   3. Discard flexibility of sessions that FINISH CHARGING AFTER the window end
      sessions_window_prof$FlexibilityHours[
        sessions_window_prof$ChargingEndDateTime >= dttm_seq[window[2]+1]
      ] <- 0

      # Re-define window to profile's connection window
      # Using the rule mean+-2*sd (95.45%) for the start and end times
      start_time_mean <- mean(as.numeric(sessions_window_prof$ConnectionStartDateTime))
      start_time_sd <- sd(as.numeric(sessions_window_prof$ConnectionStartDateTime))
      end_time_mean <- mean(as.numeric(sessions_window_prof$ConnectionEndDateTime))
      end_time_sd <- sd(as.numeric(sessions_window_prof$ConnectionEndDateTime))
      sessions_window_prof$FlexibilityHours[
        !(
          between(
            as.numeric(sessions_window_prof$ConnectionStartDateTime),
            round_to_interval(start_time_mean - 2*start_time_sd, time_resolution*60),
            round_to_interval(start_time_mean + 2*start_time_sd, time_resolution*60)
          ) &
            between(
              as.numeric(sessions_window_prof$ConnectionEndDateTime),
              round_to_interval(end_time_mean - 2*end_time_sd, time_resolution*60),
              round_to_interval(end_time_mean + 2*end_time_sd, time_resolution*60)
            )
        )
      ] <- 0

      if (nrow(sessions_window_prof) == 0) next

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
        replace = TRUE,
        prob = c(window_responsive[[profile]], (1-window_responsive[[profile]]))
      )
      sessions_window_prof_flex <- sessions_window_prof %>%
        filter(.data$Responsive & .data$FlexibilityHours >= time_resolution/60)

      if (nrow(sessions_window_prof_flex) == 0) next

      non_flexible_sessions <- sessions_window_prof %>%
        filter(!.data$Responsive | .data$FlexibilityHours < time_resolution/60)

      #   Profile sessions that can't provide flexibility are not part of the setpoint
      if (nrow(non_flexible_sessions) > 0) {
        L_fixed_prof <- non_flexible_sessions %>%
          get_demand(dttm_seq = dttm_seq[window_prof_idxs], mc.cores = 1) %>%
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
            O <- minimize_net_power_window(
              G = opt_data$production[window_prof_idxs],
              LF = L_prof,
              LS = L_fixed + L_others + L_fixed_prof,
              direction = 'forward',
              time_horizon = NULL,
              LFmax = Inf,
              grid_capacity = opt_data$grid_capacity[window_prof_idxs],
              lambda = lambda
            )
          } else if (opt_objective == "cost") {
            O <- minimize_cost_window(
              G = opt_data$production[window_prof_idxs],
              LF = L_prof,
              LS = L_fixed + L_others + L_fixed_prof,
              PI = opt_data$price_imported[window_prof_idxs],
              PE = opt_data$price_exported[window_prof_idxs],
              PTU = opt_data$price_turn_up[window_prof_idxs],
              PTD = opt_data$price_turn_down[window_prof_idxs],
              direction = 'forward',
              time_horizon = NULL,
              LFmax = Inf,
              grid_capacity = opt_data$grid_capacity[window_prof_idxs],
              lambda = lambda
            )
          } else if (is.numeric(opt_objective)) {
            O <- optimize_demand_window(
              G = opt_data$production[window_prof_idxs],
              LF = L_prof,
              LS = L_fixed + L_others + L_fixed_prof,
              PI = opt_data$price_imported[window_prof_idxs],
              PE = opt_data$price_exported[window_prof_idxs],
              PTU = opt_data$price_turn_up[window_prof_idxs],
              PTD = opt_data$price_turn_down[window_prof_idxs],
              direction = 'forward',
              time_horizon = NULL,
              LFmax = Inf,
              grid_capacity = opt_data$grid_capacity[window_prof_idxs],
              w = opt_objective,
              lambda = lambda
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
      if ((method != "none")) {
        setpoint_prof <- tibble(
          datetime = dttm_seq[window_prof_idxs],
          setpoint = setpoints[[profile]][window_prof_idxs] - L_fixed_prof
        )

        if (include_log) message("------ Scheduling")

        results <- schedule_sessions(
          sessions = sessions_window_prof_flex, setpoint = setpoint_prof,
          method = method, power_th = power_th,
          charging_power_min = charging_power_min, energy_min = energy_min,
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
      mutate(Session = factor(.data$Session, levels = sessions$Session)) %>%
      arrange(.data$Session, .data$ConnectionStartDateTime, .data$ConnectionEndDateTime) %>%
      distinct()

  }

  results <- list(
    setpoints = setpoints,
    sessions = sessions_opt,
    log = log
  )

  class(results) <- "SmartCharging"

  return(results)
}










#' Schedule sessions according to optimal setpoint
#'
#' @param sessions tibble, sessions data set in standard format marked by `{evprof}` package
#' (see [this article](https://mcanigueral.github.io/evprof/articles/sessions-format.html))
#' @param setpoint tibble with columns `datetime` and rest of columns being the names of user profiles
#' @param method character, being `"postpone"`, `"curtail"` or `"interrupt"`.
#' @param power_th numeric, power threshold (between 0 and 1) accepted from setpoint.
#' For example, with `power_th = 0.1` and `setpoint = 100` for a certain time slot,
#' then sessions' demand can reach a value of `110` without needing to schedule sessions.
#' @param charging_power_min numeric, minimum allowed ratio (between 0 and 1) of nominal power.
#' For example, if `charging_power_min = 0.5` and `method = 'curtail'`, sessions' charging power can only
#' be curtailed until the 50% of the nominal charging power (i.e. `Power` variable in `sessions` tibble).
#' @param energy_min numeric, minimum allowed ratio (between 0 and 1) of required energy.
#' @param include_log logical, whether to output the algorithm messages for every user profile and time-slot
#' @param show_progress logical, whether to output the progress bar in the console
#'
#' @return list of two elements `sessions` and `log`
#' @export
#'
#' @importFrom dplyr tibble %>% filter mutate arrange desc left_join select
#' @importFrom rlang .data
#' @importFrom lubridate as_datetime tz
#' @importFrom evsim expand_sessions
#'
schedule_sessions <- function(sessions, setpoint, method, power_th = 0,
                              charging_power_min = 0.5, energy_min = 1,
                              include_log = FALSE, show_progress = TRUE) {
    log <- c()


  resolution <- get_time_resolution(setpoint$datetime, units = "mins")
  sessions_expanded <- sessions %>%
    expand_sessions(resolution = resolution)
  sessions_expanded <- sessions_expanded %>%
    mutate(
      Power = 0,
      EnergyLeft = sessions_expanded$EnergyRequired,
      Flexible = FALSE,
      Exploited = FALSE
    ) %>%
    left_join(
      select(sessions, all_of(c("Session", "ConnectionStartDateTime", "FlexibilityHours"))),
      by = "Session"
    )

  if (show_progress) {
    pb <- progress::progress_bar$new(
      format = "[:bar] :percent eta: :eta",
      total = length(setpoint$datetime)
    )
  }

  dttm_tz <- tz(setpoint$datetime)

  for (timeslot in setpoint$datetime) {

    if (show_progress) pb$tick()

    # Filter sessions that are charging (`EnergyLeft > 0`) during this time slot
    # and calculate their average charging power `PowerTimeslot` in the time slot
    # The `PowerTimeslot` is the `PowerNominal` in all time slots, except when
    # the session finishes charging in the middle of a time slot.
    sessions_timeslot <- sessions_expanded %>%
      filter(.data$Timeslot == timeslot, .data$EnergyLeft > 0)

    if (nrow(sessions_timeslot) == 0) {
      next
    }

    sessions_timeslot <- sessions_timeslot %>%
      mutate(
        PowerTimeslot = pmin(
          .data$EnergyLeft/(resolution/60), .data$PowerNominal
        ),
        EnergyCharged = .data$EnergyRequired - .data$EnergyLeft,
        MinEnergyLeft = pmax(.data$EnergyRequired*energy_min - .data$EnergyCharged, 0),
        PossibleEnergyRest = .data$PowerNominal*pmax(.data$ConnectionHoursLeft - resolution/60, 0),
      )

    sessions_timeslot_power <- sum(sessions_timeslot$PowerTimeslot)

    setpoint_timeslot_power <- setpoint$setpoint[
      setpoint$datetime == timeslot
    ]

    flex_req <- round(sessions_timeslot_power - setpoint_timeslot_power * (1 + power_th), 2)

    # If demand should be reduced
    if (flex_req > 0) {

      if (include_log) {
        log_message <- paste(
          as_datetime(timeslot, tz=dttm_tz),
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
        # * Postpone: the EV has not started charging yet, and the energy required
        #    can be charged during the rest of the connection time at the nominal charging power.
        # * Interrupt: the charge is not completed yet, and the energy required
        #    can be charged during the rest of the connection time at the nominal charging power.
        if (method == "postpone") {
          sessions_timeslot <- sessions_timeslot %>%
            mutate(
              Flexible = ifelse(
                (.data$EnergyCharged == 0) & (.data$MinEnergyLeft <= .data$PossibleEnergyRest),
                TRUE, FALSE
              )
            )
        } else {
          sessions_timeslot <- sessions_timeslot %>%
            mutate(
              Flexible = ifelse(
                (.data$EnergyLeft > 0) & (.data$MinEnergyLeft <= .data$PossibleEnergyRest),
                TRUE, FALSE
              )
            )
        }


        # Get the maximum flexible power of this time slot
        shift_flex_available <- sum(sessions_timeslot$PowerTimeslot[sessions_timeslot$Flexible])

        # If the available flexibility is higher than the flexibility request,
        # then allow the sessions with less flexibility to charge.
        # Else, shift all sessions.
        if (shift_flex_available >= flex_req) {

          sessions_timeslot_shiftable <- sessions_timeslot %>%
            filter(.data$Flexible)

          # Arrange charging priority according to the method:
          #  - Postpone: start and end times define priority
          #  - Interrupt: start and end times, and energy charged define priority
          if (method == "postpone") {
            sessions_timeslot_shiftable <- sessions_timeslot_shiftable %>%
              arrange(desc(.data$ConnectionStartDateTime), desc(.data$ConnectionHoursLeft))
          } else {
            sessions_timeslot_shiftable <- sessions_timeslot_shiftable %>%
              arrange(desc(.data$EnergyCharged), desc(.data$ConnectionStartDateTime), desc(.data$ConnectionHoursLeft))
          }

          # Get the index of sessions that can charge
          allowed_to_charge_idx <- rep(FALSE, nrow(sessions_timeslot_shiftable))
          for (s in seq_len(nrow(sessions_timeslot_shiftable))) {
            flex_req <- max(flex_req - sessions_timeslot_shiftable$PowerTimeslot[s], 0)
            if (flex_req == 0) break
          }
          if ((s+1) <= length(allowed_to_charge_idx)) {
            allowed_to_charge_idx[seq(s+1, length(allowed_to_charge_idx))] <- TRUE
          }

          # Update the `Exploited` variable to `TRUE` for sessions to shift
          sessions_timeslot$Exploited[
            sessions_timeslot$ID %in% sessions_timeslot_shiftable$ID[!allowed_to_charge_idx]
          ] <- TRUE

        } else {

          # All `Flexible` sessions are `Exploited`
          sessions_timeslot$Exploited <- sessions_timeslot$Flexible

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
          sessions_timeslot$PowerTimeslot[!sessions_timeslot$Exploited]


      }

      if ("curtail" %in% method) {

        # Power flexibility (Curtail) ----------------------------------------------------------------

        # Flexible sessions:
        # If the minimum power that can be charged in this time slot is lower
        # than the nominal power. The minimum power is defined by:
        #  - The `charging_power_min` parameter
        #  - The minimum energy that must be charged in the time slot, defined by
        #      - The minimum energy that must be charged in total (considering `energy_min`)
        #      - The energy that can be charged at nominal power the rest of connection hours
        sessions_timeslot <- sessions_timeslot %>%
          mutate(
            MinEnergyTimeslot = pmax(.data$MinEnergyLeft - .data$PossibleEnergyRest, 0),
            MinPowerTimeslot = pmax(
              .data$MinEnergyTimeslot/(resolution/60),
              .data$PowerNominal*charging_power_min
            ),
            Flexible = ifelse(
              (.data$EnergyLeft > 0) &
                (.data$MinPowerTimeslot < .data$PowerTimeslot),
              TRUE,
              FALSE
            )
          )

        # Get the power demand from curtailable sessions
        curtailable_sessions_power <- sum(
          sessions_timeslot$PowerTimeslot[sessions_timeslot$Flexible]
        )

        # Power factor that would be required
        power_factor <- (curtailable_sessions_power - flex_req)/curtailable_sessions_power

        # All `Flexible` sessions are `Exploited`
        sessions_timeslot$Exploited <- sessions_timeslot$Flexible

        # Update the charging power of curtailed sessions
        sessions_timeslot$Power[sessions_timeslot$Exploited] <- pmax(
          sessions_timeslot$PowerTimeslot[sessions_timeslot$Exploited]*power_factor,
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
              " -- Not enough flexibility available (", flex_provided, " kW)"
            )
            message(log_message)
            log <- c(
              log,
              log_message
            )
          }
        }

        # Update flexibility requirement
        flex_req <- round(flex_req - (curtailable_sessions_power - curtailable_sessions_power*power_factor), 2)

      }

    } else {

      if (include_log) {
        log_message <- paste(
          as_datetime(timeslot, tz=dttm_tz),
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
        mutate(Power = .data$PowerTimeslot)

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


      # If there are more time slots afterwards, update `EnergyLeft` and `FlexibilityHours`
      session_after_idx <- (sessions_expanded$Session == sessions_timeslot$Session[s]) &
        (sessions_expanded$ID > sessions_timeslot$ID[s])

      if (length(session_after_idx) > 0) {

        # Update `EnergyLeft`
        session_energy <- round(sessions_timeslot$Power[s]*resolution/60, 2)
        session_energy_left <- round(sessions_timeslot$EnergyLeft[s] - session_energy, 2)
        sessions_expanded$EnergyLeft[session_after_idx] <- session_energy_left

        # Update `FlexibilityHours`
        session_flexibility_hours <- round(max(sessions_timeslot$FlexibilityHours[s] - resolution/60, 0), 2)
        sessions_timeslot$FlexibilityHours[s] <- session_flexibility_hours
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

        if (method %in% c("postpone", "interrupt")) {
          session_flexibility_hours <- exploited_sessions$FlexibilityHours[s]
          original_flexibility <- sessions$FlexibilityHours[sessions$Session == exploited_sessions$Session[s]]
          pct_flexibility_available <- round(session_flexibility_hours/original_flexibility*100)
          log_message <- paste0(
            "---- Session ", exploited_sessions$Session[s], " shifted (",
            exploited_sessions$PowerTimeslot[s], " kW, ",
            pct_flexibility_available, "% of flexible time still available)"
          )
        } else {
          power_reduction <- round(exploited_sessions$PowerTimeslot[s] - exploited_sessions$Power[s], 2)
          pct_power_reduction <- round(power_reduction/exploited_sessions$PowerTimeslot[s]*100)
          log_message <- paste0(
            "---- Session ", exploited_sessions$Session[s],
            " provides ", power_reduction, " kW of flexibility (",
            pct_power_reduction, "% power reduction)"
          )
        }

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
      select('Session', 'Timeslot', 'Power', 'FlexibilityHours', 'EnergyLeft', 'ConnectionHoursLeft', 'Flexible', 'Exploited'),
    sessions %>%
      select('Session', !any_of(evsim::sessions_feature_names)),
    by = 'Session'
  ) %>%
    mutate(
      ConnectionStartDateTime = .data$Timeslot,
      ConnectionEndDateTime = .data$Timeslot + minutes(resolution),
      ChargingStartDateTime = .data$Timeslot,
      ChargingEndDateTime = .data$Timeslot + minutes(resolution),
      ConnectionHours = resolution/60,
      ChargingHours = resolution/60,
      Energy = round(.data$Power*.data$ChargingHours, 2)
    ) %>%
    select(any_of(names(sessions)), 'EnergyLeft', 'ConnectionHoursLeft', 'Flexible', 'Exploited')

  return(
    list(
      sessions = sessions_expanded2,
      log = log
    )
  )
}



#' `print` method for `SmartCharging` object class
#'
#' @param x  `SmartCharging` object returned by `smart_charging`  function
#' @param ... further arguments passed to or from other methods.
#'
#' @returns nothing but prints information about the `SmartCharging` object
#' @export
#' @keywords internal
#'
#' @importFrom dplyr select all_of group_by summarise_all summarise mutate_if n
#' @importFrom tidyr drop_na pivot_longer
#'
#'
print.SmartCharging <- function(x, ...) {
  nS <- length(unique(x$sessions$Session))
  scS <- x$sessions %>%
    select(all_of(c("Session", "Responsive", "Flexible", "Exploited"))) %>%
    drop_na()
  nscS <- length(unique(scS$Session))
  scS2 <- scS %>%
    group_by(.data$Session) %>%
    summarise_all(sum)
  summaryS <- scS2 %>%
    replace(is.na(scS2), 0) %>%
    mutate_if(is.numeric, ~ ifelse(.x > 0, TRUE, FALSE)) %>%
    pivot_longer(- "Session") %>% group_by(.data$name) %>%
    summarise(pct = round(sum(.data$value, na.rm = TRUE)/n()*100))
  cat('Smart charging results as a list of 3 objects: charging sessions, user profiles setpoints and log messages.\n')
  cat('Simulation from', as.character(min(date(x$setpoints$datetime))),
      'to', as.character(max(date(x$setpoints$datetime))), 'with a time resolution of',
      get_time_resolution(x$setpoints$datetime, units = "mins"), 'minutes.\n')
  cat('For this time period,', nscS, 'charging sessions could participate (',
      round(nscS/nS*100), '% of total data set), from which:\n')
  cat('  - Responsive sessions:', summaryS$pct[summaryS$name == "Responsive"], '%\n')
  cat('  - Flexible sessions:', summaryS$pct[summaryS$name == "Flexible"], '%\n')
  cat('  - Exploited sessions:', summaryS$pct[summaryS$name == "Exploited"], '%\n')
  if (length(x$log) == 0) {
    cat('No log messages in this simulation.\n')
  } else {
    cat('For more information see the log messages.\n')
  }
}


#' #' Simulate charging power limitation based on grid capacity signals
#' #'
#' #' @param sessions tibble, charging sessions data set.
#' #' If sessions have been expanded using `evsim::expand_session` function, set parameter `expand = FALSE`.
#' #' @param grid_capacity tibble, grid capacity time-series with columns `datetime` and `max_amps` per phase
#' #' @param amps_car_min integer, minimum current to charge the vehicles
#' #' @param expand logical, whether to expand session using `evsim::expand_session` function
#' #' @param include_log logical, whether to print and return log messages
#' #'
#' #' @return tibble
#' #' @export
#' #'
#' #' @importFrom dplyr %>% tibble mutate select all_of row_number filter group_by summarise
#' #' @importFrom lubridate as_datetime tz
#' #' @importFrom rlang .data
#' #' @importFrom evsim get_demand expand_sessions
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
