
# EV flexibility management -----------------------------------------------

#' Smart charging algorithm
#'
#' This function provides a framework to simulate different smart charging methods
#' (i.e. postpone, interrupt and curtail) to reach multiple goals (e.g. grid congestion,
#' net power minimization, cost minimization). See implementation examples and
#' the formulation of the optimization problems in the
#' [documentation website](https://resourcefully-dev.github.io/flextools/).
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
#' - `import_capacity`: maximum imported power from the grid (in kW),
#' for example the contracted power with the energy company.
#'
#' - `export_capacity`: maximum exported power from the grid (in kW),
#' for example the contracted power with the energy company.
#'
#' - `production`: local power generation (in kW).
#' This is used when `opt_objective = "grid"`.
#'
#' - `price_imported`: price for imported energy (Euro/kWh).
#' This is used when `opt_objective = "cost"`.
#'
#' - `price_exported`: price for exported energy (Euro/kWh).
#' This is used when `opt_objective = "cost"`.
#'
#' - `price_turn_down`: price for turn-down energy use (Euro/kWh).
#' This is used when `opt_objective = "cost"`.
#'
#' - `price_turn_up`: price for turn-up energy use (Euro/kWh).
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
#' @param charging_power_min numeric. It can be configured in two ways:
#' (1) minimum allowed ratio (between 0 and 1) of nominal power (i.e. `Power` column in `sessions`), or
#' (2) specific value of minimum power (in kW) higher than 1 kW.
#'
#' For example, if `charging_power_min = 0.5` and `method = 'curtail'`, sessions' charging power can only
#' be curtailed until the 50% of the nominal charging power.
#' And if `charging_power_min = 2`, sessions' charging power can be curtailed until 2 kW.
#'
#' @param energy_min numeric, minimum allowed ratio (between 0 and 1) of required energy.
#' @param include_log logical, whether to output the algorithm messages for every user profile and time-slot
#' @param show_progress logical, whether to output the progress bar in the console
#' @param lambda numeric, penalty on change for the flexible load.
#'
#' @importFrom dplyr tibble %>% filter mutate select everything row_number left_join bind_rows any_of pull distinct between sym all_of
#' @importFrom lubridate hour minute date
#' @importFrom rlang .data
#' @importFrom stats sd
#' @importFrom purrr set_names map map2 in_parallel
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
#'
#' # Use first 50 sessions
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
                           lambda = 0) {

  if (show_progress) cli::cli_h1("Set up")

  if (show_progress) cli::cli_progress_step("Checking parameters")

  # Parameters check
  if (is.null(sessions) | nrow(sessions) == 0) {
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
      message("Warning: time cycle name in `responsive` not found in `sessions`")
      # return( NULL )
    }
    if (!all(responsive_user_profiles %in% sessions_user_profiles)) {
      message("Warning: user profile name in `responsive` not found in `sessions`")
      # return( NULL )
    }
  }

  opt_data$flexible <- 0
  opt_data <- check_optimization_data(opt_data, opt_objective)

  if (show_progress) cli::cli_progress_step("Defining optimization windows")

  # Adapt the data set for the current time resolution
  dttm_seq <- opt_data$datetime
  time_resolution <- get_time_resolution(dttm_seq, units = "mins")
  sessions <- sessions %>%
    adapt_charging_features(time_resolution = time_resolution)

  # Optimization windows according to `window_days` and `window_start_hour`
  flex_windows_idx <- get_flex_windows(
    dttm_seq, window_days, window_start_hour
  )

  # Get user profiles demand
  if (show_progress) cli::cli_progress_step("Calculating EV demand")
  profiles_demand <- get_demand(sessions, dttm_seq)


  # SMART CHARGING ----------------------------------------------------------
  if (show_progress) cli::cli_h1("Smart charging")

  # Set responsive sessions -------------------------------------------------
  if (show_progress) cli::cli_progress_step("Setting responsiveness")

  windows_data <- map(
    flex_windows_idx$flex_idx,
    function(flex_idx) {
      list(
        sessions_window = sessions %>%
          filter(
            .data$ChargingStartDateTime >= dttm_seq[flex_idx[1]],
            .data$ChargingStartDateTime <= dttm_seq[flex_idx[length(flex_idx)]]
          ) %>%
          set_responsive(dttm_seq[flex_idx], responsive),
        profiles_demand = profiles_demand[flex_idx, ],
        opt_data = opt_data[flex_idx, ]
      )
    }
  )

  # Get setpoints -----------------------------------------------------------
  if (show_progress) cli::cli_progress_step("Defining setpoints")

  setpoints_lst <- get_setpoints_parallel(
    windows_data, opt_objective, lambda
  )

  # setpoints_lst <- map(
  #   windows_data,
  #   in_parallel(
  #     \(x)
  #     get_setpoints(
  #       sessions_window = x$sessions_window,
  #       profiles_demand = x$profiles_demand,
  #       opt_data = x$opt_data,
  #       opt_objective = opt_objective,
  #       lambda = lambda
  #     ),
  #     get_setpoints = get_setpoints,
  #     opt_objective = opt_objective,
  #     lambda = lambda
  #   )
  # )

  # Scheduling --------------------------------------------------------------

  if (method != "none") {
    if (show_progress) cli::cli_progress_step("Scheduling EV sessions")

    scheduling_lst <- smart_charging_window_parallel(
      windows_data, setpoints_lst, method, power_th,
      charging_power_min, energy_min, include_log
    )

    # scheduling_lst <- map2(
    #   windows_data, setpoints_lst,
    #   in_parallel(
    #     \(x, y)
    #     smart_charging_window(
    #       sessions_window = x$sessions_window,
    #       profiles_demand = x$profiles_demand,
    #       setpoints = y,
    #       method = method,
    #       power_th = power_th,
    #       charging_power_min = charging_power_min,
    #       energy_min = energy_min,
    #       include_log = include_log
    #     ),
    #     smart_charging_window = smart_charging_window,
    #     method = method,
    #     power_th = power_th,
    #     charging_power_min = charging_power_min,
    #     energy_min = energy_min,
    #     include_log = include_log
    #   )
    # )
  }

  if (show_progress) cli::cli_progress_step("Cleaning data set")

  setpoints <- list_rbind(setpoints_lst)

  # Join with the original datetime sequence and demand
  setpoints_opt <- profiles_demand
  opt_dttm_idx <- setpoints_opt$datetime %in% setpoints$datetime
  setpoints_opt[opt_dttm_idx, names(setpoints)] <- setpoints

  if (method == "none") {

    sessions_opt <- sessions
    demand_opt <- setpoints_opt
    log <- list()

  } else {

    # Join the sessions that have been exploited with the non-flexible ones
    sessions_considered <- map(
      scheduling_lst, ~ .x$sessions
    ) %>%
      list_rbind()

    if (nrow(sessions_considered) > 0) {
      sessions_not_considered <- sessions[!(sessions$Session %in% sessions_considered$Session), ]
    } else {
      sessions_not_considered <- sessions %>%
        mutate(
          Responsive = NA, Flexible = NA, Exploited = NA
        )
    }

    sessions_opt <- bind_rows(
      sessions_not_considered,
      sessions_considered
    ) %>%
      select(any_of(names(sessions)), everything()) %>% # Set order of columns
      mutate(Session = factor(.data$Session, levels = sessions$Session)) %>% # Convert `Session` to factor to be sorted
      arrange(.data$Session, .data$ConnectionStartDateTime, .data$ConnectionEndDateTime) %>%
      mutate(Session = as.character(.data$Session)) %>% # Convert `Session` back to character
      distinct()

    demand <- map(
      scheduling_lst, ~ .x$demand
    ) %>%
      list_rbind()

    # Join with the original datetime sequence and demand
    demand_opt <- profiles_demand
    opt_dttm_idx <- demand_opt$datetime %in% demand$datetime
    demand_opt[opt_dttm_idx, names(demand)] <- demand
    # demand_opt[is.na(demand_opt)] <- 0 # ReplaceNA

    log_lst <- map(
      scheduling_lst, ~ .x$log
    )
    log <- do.call(c, log_lst)
  }

  results <- list(
    sessions = sessions_opt,
    setpoints = setpoints_opt,
    demand = demand_opt,
    log = log
  )

  class(results) <- "SmartCharging"

  return(results)
}

#' Set `Responsive` column in `sessions`
#'
#' @param sessions_window tibble, sessions corresponding to a single windows
#' @param dttm_seq datetime vector
#' @param responsive named list with responsive ratios
#'
#' @importFrom dplyr tibble %>% filter mutate select everything row_number left_join bind_rows any_of pull distinct between sym all_of
#' @importFrom lubridate hour minute date
#' @importFrom rlang .data
#' @importFrom stats sd
#' @importFrom purrr set_names
#' @importFrom evsim get_demand adapt_charging_features
#'
#' @keywords internal
#'
set_responsive <- function(sessions_window, dttm_seq, responsive) {

  if (nrow(sessions_window) == 0) {
    return( tibble() )
  }

  # Window's features
  # Find most common time-cycle in this window
  window_timecycle <- names(sort(table(sessions_window$Timecycle), decreasing = TRUE))[1]
  # Responsiveness of the user profiles in this time-cycle
  # If the time cycle is not configured in `responsive` then skip smart charging
  if (!(window_timecycle %in% names(responsive))) {
    return( tibble() )
  }
  window_responsive <- responsive[[window_timecycle]]

  if (length(window_responsive) == 0) {
    return( tibble() )
  }

  # Profiles subjected to optimization:
  #   1. appearing in the sessions set for this optimization window
  #   2. responsive values higher than 0
  opt_profiles <- names(window_responsive)[
    (names(window_responsive) %in% unique(sessions_window$Profile)) &
      (names(window_responsive) %in% names(window_responsive)[as.numeric(window_responsive) > 0])
  ]

  if (length(opt_profiles) == 0) {
    return( tibble() )
  }

  sessions_considered <- tibble()

  # For each optimization profile
  for (profile in opt_profiles) {

    # Filter only sessions of this Profile
    sessions_window_prof <- sessions_window %>%
      filter(.data$Profile == profile)

    if (nrow(sessions_window_prof) == 0) {
      return( tibble() )
    }

    # RESPONSIVENESS
    sessions_window_prof$Responsive <- NA

    # Potentially responsive sessions are defined according to the following conditions:
    #   1. Charging end time inside the optimization window
    end_charge_window <- sessions_window_prof$ChargingEndDateTime <= dttm_seq[length(dttm_seq)]

    # #   2. Connection times inside the 95% percentile using the rule mean+-2*sd (95.45%)
    # #       This is done only if optimization is used to find a setpoint
    # if (opt_objective != "none") {
    #   not_outliers <- get_window_not_outliers(sessions_window_prof, pct = 95, time_resolution)
    # } else {
    #   not_outliers <- TRUE
    # }

    # Sessions that are "potentially responsive":
    # potentially_responsive_idx <- which(end_charge_window & not_outliers)
    potentially_responsive_idx <- which(end_charge_window)

    # From the potentially responsive sessions select randomly the configured
    # number of `responsive` sessions:
    n_responsive <- round(length(potentially_responsive_idx)*window_responsive[[profile]])
    set.seed(1234)
    responsive_idx <- sample(potentially_responsive_idx, n_responsive)
    non_responsive_idx <- potentially_responsive_idx[!(potentially_responsive_idx %in% responsive_idx)]
    sessions_window_prof$Responsive[responsive_idx] <- TRUE
    sessions_window_prof$Responsive[non_responsive_idx] <- FALSE

    sessions_considered <- bind_rows(
      sessions_considered, sessions_window_prof
    )
  }

  return( sessions_considered )
}


get_opt_profiles <- function (sessions_window) {
  sessions_window %>%
    dplyr::filter(.data$Responsive) %>%
    arrange_by_flex_potential(descendent = TRUE) %>%
    dplyr::pull("Profile") %>%
    unique()
}


arrange_by_flex_potential <- function(sessions, descendent = TRUE) {
  profiles_flexpotential <- sessions %>%
    dplyr::mutate(
      FlexibilityHours = .data$ConnectionHours - .data$ChargingHours
    ) %>%
    dplyr::group_by(.data$Profile) %>%
    dplyr::summarise(FlexibilityHours = mean(.data$FlexibilityHours))

  if (descendent) {
    profiles_flexpotential%>%
      dplyr::arrange(desc(.data$FlexibilityHours)) %>%
      dplyr::select(-"FlexibilityHours")
  } else {
    profiles_flexpotential%>%
      dplyr::arrange(.data$FlexibilityHours) %>%
      dplyr::select(-"FlexibilityHours")
  }
}


#' Set setpoints for smart charging
#'
#' @param sessions_window tibble, sessions corresponding to a single windows
#' @param opt_data tibble, optimization data
#' @param profiles_demand tibble, user profiles power demand
#' @param opt_objective character, optimization objective
#' @param lambda numeric, penalty on change for the flexible load.
#'
#' @importFrom dplyr tibble %>% filter mutate select everything row_number left_join bind_rows any_of pull distinct between sym all_of
#' @importFrom lubridate hour minute date
#' @importFrom rlang .data
#' @importFrom stats sd
#' @importFrom purrr set_names
#' @importFrom evsim get_demand adapt_charging_features
#'
#' @keywords internal
#'
get_setpoints <- function(sessions_window, opt_data, profiles_demand, opt_objective, lambda) {

  if (nrow(sessions_window) == 0) {
    return( sessions_window )
  }

  dttm_seq <- opt_data$datetime
  time_resolution <- get_time_resolution(dttm_seq, units = "mins")

  if ('static' %in% colnames(opt_data)) {
    L_fixed <- opt_data$static
  } else {
    L_fixed <- rep(0, nrow(opt_data))
  }

  opt_profiles <- get_opt_profiles(sessions_window)
  setpoints <- profiles_demand %>%
    select(any_of(c("datetime", opt_profiles)))

  for (profile in opt_profiles) {

    # If `opt_data` contains user profile's name,
    # this is considered to be a setpoint (skip optimization)
    if (profile %in% colnames(opt_data)) {

      setpoints[[profile]] <- opt_data[[profile]]

    } else if (opt_objective != "none") {


      # Separate responsive and non-responsive sessions -----------------------------------------------------
      sessions_window_prof_flex <- sessions_window %>%
        filter(.data$Profile == profile & .data$Responsive)

      non_responsive_sessions <- sessions_window %>%
        filter(.data$Profile == profile & (!.data$Responsive | is.na(.data$Responsive)))

      if (nrow(non_responsive_sessions) > 0) {
        L_fixed_prof <- non_responsive_sessions %>%
          get_demand(dttm_seq = dttm_seq, by = "Profile") %>%
          pull(!!sym(profile))
      } else {
        L_fixed_prof <- rep(0, length(dttm_seq))
      }


      # Optimization ------------------------------------------------------------

      # Limit `ConnectionEndDateTime` to window's end
      sessions_window_prof_flex$ConnectionEndDateTime[
        (sessions_window_prof_flex$ConnectionEndDateTime >= dttm_seq[length(dttm_seq)]+minutes(time_resolution))
      ] <- dttm_seq[length(dttm_seq)]+minutes(time_resolution)

      # Setpoint datetime sequence
      window_prof_dttm <- c(
        min(sessions_window_prof_flex$ConnectionStartDateTime),
        min(max(sessions_window_prof_flex$ConnectionEndDateTime), dttm_seq[length(dttm_seq)])
      )
      opt_idxs <- (dttm_seq >= window_prof_dttm[1]) &
        (dttm_seq <= window_prof_dttm[2])
      # window_prof_length <- sum(window_prof_idxs)

      # The optimization flexible load is the load of the responsive sessions
      LF <- setpoints[[profile]] - L_fixed_prof

      # Static load
      # Here we consider `setpoints` instead of `profiles_demand` because we
      # update it in every iteration (optimization)
      L_others <- setpoints %>%
        select(- any_of(c(profile, 'datetime'))) %>%
        rowSums()
      if (length(L_others) == 0) {
        L_others <- rep(0, length(dttm_seq))
      }
      LS <- L_fixed + L_others + L_fixed_prof

      # Optimize the flexible profile's load according to `opt_objective`
      if (opt_objective == "grid") {
        O <- minimize_net_power_window(
          G = opt_data$production[opt_idxs],
          LF = LF[opt_idxs],
          LS = LS[opt_idxs],
          direction = 'forward',
          time_horizon = NULL,
          LFmax = Inf,
          import_capacity = opt_data$import_capacity[opt_idxs],
          export_capacity = opt_data$export_capacity[opt_idxs],
          lambda = lambda
        )
      } else if (opt_objective == "cost") {
        O <- minimize_cost_window(
          G = opt_data$production[opt_idxs],
          LF = LF[opt_idxs],
          LS = LS[opt_idxs],
          PI = opt_data$price_imported[opt_idxs],
          PE = opt_data$price_exported[opt_idxs],
          PTU = opt_data$price_turn_up[opt_idxs],
          PTD = opt_data$price_turn_down[opt_idxs],
          direction = 'forward',
          time_horizon = NULL,
          LFmax = Inf,
          import_capacity = opt_data$import_capacity[opt_idxs],
          export_capacity = opt_data$export_capacity[opt_idxs],
          lambda = lambda
        )
      } else if (is.numeric(opt_objective)) {
        O <- optimize_demand_window(
          G = opt_data$production[opt_idxs],
          LF = LF[opt_idxs],
          LS = LS[opt_idxs],
          PI = opt_data$price_imported[opt_idxs],
          PE = opt_data$price_exported[opt_idxs],
          PTU = opt_data$price_turn_up[opt_idxs],
          PTD = opt_data$price_turn_down[opt_idxs],
          direction = 'forward',
          time_horizon = NULL,
          LFmax = Inf,
          import_capacity = opt_data$import_capacity[opt_idxs],
          export_capacity = opt_data$export_capacity[opt_idxs],
          w = opt_objective,
          lambda = lambda
        )
      }

      setpoints[[profile]][opt_idxs] <- O + L_fixed_prof[opt_idxs]

    } else if ("import_capacity" %in% colnames(opt_data)) {

      # Other profiles load
      # Here we consider `profiles_demand` instead of `setpoint` because we
      # update it in the scheduling (no optimization, just grid capacity)
      L_others <- profiles_demand %>%
        select(- any_of(c(profile, 'datetime'))) %>%
        rowSums()
      if (length(L_others) == 0) {
        L_others <- rep(0, length(dttm_seq))
      }

      # Limit power profile up to total grid capacity
      profile_power_limited <- pmin(
        pmax(
          opt_data$import_capacity - (L_fixed + L_others),
          0
        ),
        profiles_demand[[profile]]
      )
      setpoints[[profile]] <- profile_power_limited
    } else {
      stop(paste(
        "Error: `opt_objective` is 'none' but no setpoint configured in `opt_data` for Profile", profile
      ))
    }
  }

  return(setpoints)
}


get_setpoints_parallel <- function(windows_data, opt_objective, lambda) {

  if (!requireNamespace("mirai", quietly = TRUE) ||
      !requireNamespace("carrier", quietly = TRUE)) {

    setpoints_lst <- purrr::map(
      windows_data,
      \(x)
      get_setpoints(
        sessions_window = x$sessions_window,
        profiles_demand = x$profiles_demand,
        opt_data = x$opt_data,
        opt_objective = opt_objective,
        lambda = lambda
      )
    )

  } else {

    setpoints_lst <- purrr::map(
      windows_data,
      purrr::in_parallel(
        \(x)
        get_setpoints(
          sessions_window = x$sessions_window,
          profiles_demand = x$profiles_demand,
          opt_data = x$opt_data,
          opt_objective = opt_objective,
          lambda = lambda
        ),
        get_setpoints = get_setpoints,
        opt_objective = opt_objective,
        lambda = lambda
      )
    )
  }
  return( setpoints_lst )
}



#' Set setpoints for smart charging
#'
#' @param sessions_window tibble, sessions corresponding to a single windows
#' @param profiles_demand tibble, user profiles power demand
#' @param setpoints tibble, user profiles power setpoints
#' @param method character, scheduling method being `"none"`, `"postpone"`, `"curtail"` or `"interrupt"`.
#' If `none`, the scheduling part is skipped and the sessions returned in the
#' results will be identical to the original parameter.
#' @param power_th numeric, power threshold (between 0 and 1) accepted from setpoint.
#' For example, with `power_th = 0.1` and `setpoint = 100` for a certain time slot,
#' then sessions' demand can reach a value of `110` without needing to schedule sessions.
#' @param charging_power_min numeric. It can be configured in two ways:
#' (1) minimum allowed ratio (between 0 and 1) of nominal power (i.e. `Power` column in `sessions`), or
#' (2) specific value of minimum power (in kW) higher than 1 kW.
#'
#' For example, if `charging_power_min = 0.5` and `method = 'curtail'`, sessions' charging power can only
#' be curtailed until the 50% of the nominal charging power.
#' And if `charging_power_min = 2`, sessions' charging power can be curtailed until 2 kW.
#'
#' @param energy_min numeric, minimum allowed ratio (between 0 and 1) of required energy.
#' @param include_log logical, whether to output the algorithm messages for every user profile and time-slot
#'
#' @importFrom dplyr tibble %>% filter mutate select everything row_number left_join bind_rows any_of pull distinct between sym all_of
#' @importFrom lubridate hour minute date
#' @importFrom rlang .data
#' @importFrom stats sd
#' @importFrom purrr set_names
#' @importFrom evsim get_demand adapt_charging_features
#'
#' @keywords internal
#'
smart_charging_window <- function(sessions_window, profiles_demand, setpoints, method, power_th = 0,
                                  charging_power_min = 0, energy_min = 1,
                                  include_log = FALSE) {

  if (nrow(sessions_window) == 0) {
    return( sessions_window )
  }

  dttm_seq <- setpoints$datetime
  time_resolution <- get_time_resolution(dttm_seq, units = "mins")

  sessions_considered <- tibble()
  log <- list()
  log_window_name <- as.character(date(dttm_seq[1]))
  log[[log_window_name]] <- list() # In the `log` object even though `include_log = FALSE`

  opt_profiles <- get_opt_profiles(sessions_window)

  for (profile in opt_profiles) {

    # Select only RESPONSIVE sessions
    sessions_window_prof_flex <- sessions_window %>%
      filter(.data$Profile == profile & .data$Responsive)

    # Profile sessions that can't provide flexibility are not part of the setpoint
    # Select now NON-RESPONSIVE sessions
    non_responsive_sessions <- sessions_window %>%
      filter(.data$Profile == profile & (!.data$Responsive | is.na(.data$Responsive)))

    if (nrow(non_responsive_sessions) > 0) {
      L_fixed_prof <- non_responsive_sessions %>%
        get_demand(dttm_seq = dttm_seq, by = "Profile") %>%
        pull(!!sym(profile))
    } else {
      L_fixed_prof <- rep(0, nrow(setpoints))
    }

    setpoint_prof <- tibble(
      datetime = dttm_seq,
      setpoint = setpoints[[profile]] - L_fixed_prof
    )
    results <- schedule_sessions(
      sessions = sessions_window_prof_flex, setpoint = setpoint_prof,
      method = method, power_th = power_th,
      charging_power_min = charging_power_min, energy_min = energy_min,
      include_log = include_log, show_progress = FALSE
    )

    # Final profile sessions
    sessions_window_prof_final <- bind_rows(
      results$sessions,
      non_responsive_sessions
    )

    # Update the time-series demand
    sessions_window_prof_final_demand <- get_demand(
      sessions_window_prof_final, dttm_seq = dttm_seq
    )
    profiles_demand[[profile]] <-
      sessions_window_prof_final_demand[[profile]]

    # Join with the rest of data set
    sessions_considered <- bind_rows(
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


smart_charging_window_parallel <- function(
    windows_data, setpoints_lst, method, power_th, charging_power_min,
    energy_min, include_log
  ) {

  if (!requireNamespace("mirai", quietly = TRUE) ||
      !requireNamespace("carrier", quietly = TRUE)) {

    scheduling_lst <- purrr::map2(
      windows_data, setpoints_lst,
      \(x, y)
      smart_charging_window(
        sessions_window = x$sessions_window,
        profiles_demand = x$profiles_demand,
        setpoints = y,
        method = method,
        power_th = power_th,
        charging_power_min = charging_power_min,
        energy_min = energy_min,
        include_log = include_log
      )
    )

  } else {

    scheduling_lst <- purrr::map2(
      windows_data, setpoints_lst,
      in_parallel(
        \(x, y)
        smart_charging_window(
          sessions_window = x$sessions_window,
          profiles_demand = x$profiles_demand,
          setpoints = y,
          method = method,
          power_th = power_th,
          charging_power_min = charging_power_min,
          energy_min = energy_min,
          include_log = include_log
        ),
        smart_charging_window = smart_charging_window,
        method = method,
        power_th = power_th,
        charging_power_min = charging_power_min,
        energy_min = energy_min,
        include_log = include_log
      )
    )

  }
  return( scheduling_lst )
}




#' Schedule sessions according to optimal setpoint
#'
#' @param sessions tibble, sessions data set containing the following variables:
#' `"Session"`, `"ConnectionStartDateTime"`, `"ConnectionHours"`, `"Power"` and `"Energy"`.
#'
#' IMPORTANT: Make sure that the `sessions` `ConnectionStartDateTime` and
#' `ChargingStartDateTime` are in the same time resolution than `setpoint$datetime`.
#' @param setpoint tibble with columns `datetime` and `setpoint`.
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
#' @importFrom dplyr tibble %>% filter mutate arrange desc left_join select mutate_if
#' @importFrom rlang .data
#' @importFrom lubridate as_datetime tz
#' @importFrom evsim expand_sessions
#'
schedule_sessions <- function(sessions, setpoint, method, power_th = 0,
                              charging_power_min = 0.5, energy_min = 1,
                              include_log = FALSE, show_progress = TRUE) {

  if (show_progress) cli::cli_h1("Scheduling charging sessions")

  log <- c()

  # Parameters check
  if (show_progress) cli::cli_progress_step("Checking parameters")

  if (is.null(sessions) | nrow(sessions) == 0) {
    stop("Error: `sessions` parameter is empty.")
  }
  sessions_basic_vars <- c(
    "Session", "ConnectionStartDateTime", "ConnectionHours", "Power", "Energy"
  )
  if (!all(sessions_basic_vars %in% colnames(sessions))) {
    stop("Error: `sessions` does not contain all required variables (see Arguments description)")
  }
  if (!all(c("datetime", "setpoint") %in% colnames(setpoint))) {
    stop("Error: `setpoint` does not contain all required variables (see Arguments description)")
  }
  if(!(method %in% c("postpone", "interrupt", "curtail"))) {
    stop("Error: `method` not valid (see Arguments description)")
  }

  resolution <- get_time_resolution(setpoint$datetime, units = "mins")
  dttm_tz <- tz(setpoint$datetime)

  if (show_progress) cli::cli_progress_step("Preparing sessions data set")

  sessions_sch <- sessions %>%
    filter(.data$ConnectionStartDateTime %in% setpoint$datetime) %>%
    mutate(
      ChargingHours = .data$Energy/.data$Power
    )

  if (method %in% c("postpone", "interrupt")) {
    sessions_sch <- sessions_sch %>%
      mutate(
        ConnectionHours = round(as.numeric(
          .data$ConnectionEndDateTime - .data$ConnectionStartDateTime, unit = "hours"
        ), 2),
        FlexibilityHours = .data$ConnectionHours - .data$ChargingHours
      )
  }

  if (nrow(sessions_sch) == 0) {
    message("Error: no `sessions` for `setpoint$datetime` period")
    return( NULL )
  }

  sessions_expanded <- sessions_sch %>%
    expand_sessions(resolution = resolution)
  sessions_expanded <- sessions_expanded %>%
    mutate(
      Power = NA,
      EnergyLeft = .data$EnergyRequired,
      Flexible = NA,
      Exploited = NA
    ) %>%
    left_join(
      select(sessions_sch, any_of(c("Session", "ConnectionStartDateTime", "FlexibilityHours"))),
      by = "Session"
    )


  timeslot_dttm <- NULL
  if (show_progress)
    cli::cli_progress_step("Simulating timeslot: {timeslot_dttm}", spinner = TRUE)

  for (timeslot in setpoint$datetime) {
    timeslot_dttm <- format(as_datetime(timeslot, tz=dttm_tz), "%d/%m/%Y %H:%M")

    if (include_log) {
      log <- c(
        log,
        paste("\u2500\u2500", timeslot_dttm, "\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500")
      )
    }

    if (show_progress)
      cli::cli_progress_update()

    # Filter sessions that are connected during this time slot
    # and calculate their average charging power `PowerTimeslot`
    # The `PowerTimeslot` is the `PowerNominal` in all time slots, except when
    # the session finishes charging in the middle of a time slot.
    sessions_timeslot <- sessions_expanded %>%
      filter(.data$Timeslot == timeslot)

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


    # Flexibility definition --------------------------------------------------
    if (method == "postpone") {
      # * Postpone: the EV has not started charging yet, and the energy required
      #    can be charged during the rest of the connection time at the nominal charging power.
      sessions_timeslot <- sessions_timeslot %>%
        mutate(
          Flexible = ifelse(
            (.data$EnergyCharged == 0) & (.data$MinEnergyLeft <= .data$PossibleEnergyRest),
            TRUE, FALSE
          )
        )
    } else if (method == "interrupt") {
      # * Interrupt: the charge is not completed yet, and the energy required
      #    can be charged during the rest of the connection time at the nominal charging power.
      sessions_timeslot <- sessions_timeslot %>%
        mutate(
          Flexible = ifelse( # Added error tolerance
            (.data$EnergyLeft > 0.025) & (.data$MinEnergyLeft <= .data$PossibleEnergyRest),
            TRUE, FALSE
          )
        )
    } else if (method == "curtail") {
      # If the minimum power that can be charged in this time slot is lower
      # than the nominal power. The minimum power is defined by:
      #  - The `charging_power_min` parameter
      #  - The minimum energy that must be charged in the time slot, defined by
      #      - The minimum energy that must be charged in total (considering `energy_min`)
      #      - The energy that can be charged at nominal power the rest of connection hours

      if (!is.numeric(charging_power_min)) {
        stop("`charging_power_min` should be numeric")
      }

      if (charging_power_min <= 1) {
        charging_power_min_ratio <- charging_power_min
        charging_power_min_kW <- Inf
      } else {
        charging_power_min_ratio <- 1
        charging_power_min_kW <- charging_power_min
      }

      sessions_timeslot <- sessions_timeslot %>%
        mutate(
          MinEnergyTimeslot = pmax(.data$MinEnergyLeft - .data$PossibleEnergyRest, 0),
          MinPowerTimeslot = pmax(
            .data$MinEnergyTimeslot/(resolution/60),
            min(.data$PowerNominal*charging_power_min_ratio, charging_power_min_kW)
          ),
          Flexible = ifelse( # Added error tolerance
            (.data$EnergyLeft > 0.025) &
              (.data$PowerTimeslot - .data$MinPowerTimeslot > 0.1),
            TRUE,
            FALSE
          )
        )
    }


    # Flexibility exploitation ------------------------------------------------

    # Set `Exploited` to `FALSE` by default
    sessions_timeslot$Exploited <- FALSE

    # Power demand in this time slot
    sessions_timeslot_power <- sum(sessions_timeslot$PowerTimeslot)

    # Setpoint of power for this time slot
    setpoint_power_timeslot <- setpoint$setpoint[
      setpoint$datetime == timeslot
    ]

    # Flexibility requirement
    flex_req <- round(sessions_timeslot_power - setpoint_power_timeslot * (1 + power_th), 2)

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

      if (method %in% c("postpone", "interrupt")) {

        # Time flexibility (Postpone / Interrupt) ----------------------------------------------------------------

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

          # All `Flexible` sessions are `Exploited` when the flexibility required
          # is higher than the flexibility available
          sessions_timeslot$Exploited[sessions_timeslot$Flexible] <- TRUE

          # Update flexibility requirement with power from all shiftable sessions
          flex_req <- round(flex_req - shift_flex_available, 2)

          if (include_log) {
            log_message <- paste0(
              "\u2716 Not enough flexibility available (", shift_flex_available, " kW)"
            )
            # message(log_message)
            log <- c(
              log,
              log_message
            )
          }

        }

        # Update the charging power of sessions that are shifted
        sessions_timeslot$Power[sessions_timeslot$Exploited] <- 0

        # Update the charging power of sessions that are NOT shifted
        sessions_timeslot$Power[!sessions_timeslot$Exploited] <-
          sessions_timeslot$PowerTimeslot[!sessions_timeslot$Exploited]
      }

      if ("curtail" %in% method) {

        # Power flexibility (Curtail) ----------------------------------------------------------------

        # All `Flexible` sessions are `Exploited` with `curtail`
        sessions_timeslot$Exploited[sessions_timeslot$Flexible] <- TRUE

        # Get the power demand from curtailable sessions
        curtailable_sessions_power <- sum(
          sessions_timeslot$PowerTimeslot[sessions_timeslot$Flexible]
        )

        # Power factor that would be required
        power_factor <- (curtailable_sessions_power - flex_req)/curtailable_sessions_power

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
        flex_req <- round(flex_req - (curtailable_sessions_power - curtailable_sessions_power*power_factor), 2)

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


      # If there are more time slots afterwards, update `EnergyLeft` and `FlexibilityHours`
      session_after_idx <- (sessions_expanded$Session == sessions_timeslot$Session[s]) &
        (sessions_expanded$ID > sessions_timeslot$ID[s])

      if (length(session_after_idx) > 0) {

        # Update `EnergyLeft`
        # We assume that the session is charging the whole timeslot, so:
        # `ChargingHours = resolution/60`
        session_energy <- sessions_timeslot$Power[s]*resolution/60
        session_energy_left <- sessions_timeslot$EnergyLeft[s] - session_energy
        sessions_expanded$EnergyLeft[session_after_idx] <- session_energy_left

        # Update `FlexibilityHours`
        if (method %in% c("postpone", "interrupt")) {
          session_flexibility_hours <- max(sessions_timeslot$FlexibilityHours[s] - resolution/60, 0)
          sessions_timeslot$FlexibilityHours[s] <- session_flexibility_hours
          sessions_expanded$FlexibilityHours[session_after_idx] <- session_flexibility_hours
        }

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
          original_flexibility <- sessions_sch$FlexibilityHours[sessions$Session == exploited_sessions$Session[s]]
          pct_flexibility_available <- round(session_flexibility_hours/original_flexibility*100, 1)
          log_message <- paste0(
            "| \u2714 Session ", exploited_sessions$Session[s], " shifted (",
            exploited_sessions$PowerTimeslot[s], " kW, ",
            pct_flexibility_available, "% of flexible time still available)"
          )
        } else {
          power_reduction <- round(exploited_sessions$PowerTimeslot[s] - exploited_sessions$Power[s], 2)
          if (power_reduction > 0) {
            pct_power_reduction <- round(power_reduction/exploited_sessions$PowerTimeslot[s]*100, 1)
            log_message <- paste0(
              "| \u2714 Session ", exploited_sessions$Session[s],
              " provides ", power_reduction, " kW of flexibility (",
              pct_power_reduction, "% power reduction)"
            )
          } else {
            log_message <- NULL
          }
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
      evsim::sessions_feature_names, 'Timeslot', 'EnergyLeft',
      'ConnectionHoursLeft', 'Flexible', 'Exploited'
    ))) %>%
    mutate(
      ConnectionStartDateTime = .data$Timeslot,
      ConnectionEndDateTime = .data$Timeslot + minutes(resolution),
      ChargingStartDateTime = .data$Timeslot,
      ChargingEndDateTime = .data$Timeslot + minutes(resolution),
      ConnectionHours = resolution/60,
      ChargingHours = ifelse(.data$Power > 0, resolution/60, 0),
      Energy = .data$Power*.data$ChargingHours
    ) %>%
    summarise_by_segment() %>%
    mutate_if(is.numeric, round, 2)

  sessions_sch_flex <- sessions_sch %>%
    select('Session', !any_of(names(sessions_segmented))) %>%
    left_join(sessions_segmented, by = 'Session') %>%
    select(any_of(names(sessions_sch)), 'Flexible', 'Exploited', 'EnergyLeft', 'ConnectionHoursLeft')

  return(
    list(
      sessions = sessions_sch_flex,
      log = log
    )
  )
}


# Window outliers ---------------------------------------------------------

get_sd_factor <- function(pct = 95) {
  stats::qnorm(1 - (1 - pct / 100) / 2)
}

get_window_not_outliers <- function(sessions, pct, time_resolution) {
  sd_factor <- get_sd_factor(pct)
  start_time_mean <- mean(as.numeric(sessions$ConnectionStartDateTime))
  start_time_sd <- sd(as.numeric(sessions$ConnectionStartDateTime))
  end_time_mean <- mean(as.numeric(sessions$ConnectionEndDateTime))
  end_time_sd <- sd(as.numeric(sessions$ConnectionEndDateTime))
  not_outliers <- dplyr::between(
    as.numeric(sessions$ConnectionStartDateTime),
    round_to_interval(start_time_mean - sd_factor*start_time_sd, time_resolution*60),
    round_to_interval(start_time_mean + sd_factor*start_time_sd, time_resolution*60)
  ) &
    dplyr::between(
      as.numeric(sessions$ConnectionEndDateTime),
      round_to_interval(end_time_mean - sd_factor*end_time_sd, time_resolution*60),
      round_to_interval(end_time_mean + sd_factor*end_time_sd, time_resolution*60)
    )
  not_outliers
}



# Summarise by segment ----------------------------------------------------

get_segment_number <- function(power_vct) {
  rle_segment <- rle(power_vct)
  rep(seq_along(rle_segment$lengths), rle_segment$lengths)
}


#' Summarise sessions by segment
#'
#' Simplify the extended sessions schedule by joining consecutive rows with
#' same charging power (power segments).
#'
#' @param ss sessions expanded schedule from `schedule_sessions` function
#'
#' @importFrom dplyr  %>% group_by mutate ungroup summarise select left_join summarise_all first
#' @keywords internal
#'
summarise_by_segment <- function(ss) {
  ss_segmented <- ss %>%
    group_by(.data$Session) %>%
    mutate(Segment = get_segment_number(.data$Power))

  ss_basic_vars <- ss_segmented %>%
    group_by(.data$Session, .data$Segment) %>%
    summarise(
      ConnectionStartDateTime = min(.data$ConnectionStartDateTime),
      ConnectionEndDateTime = max(.data$ConnectionEndDateTime),
      ChargingStartDateTime = min(.data$ConnectionStartDateTime),
      ChargingEndDateTime = max(.data$ChargingEndDateTime),
      Power = first(.data$Power),
      Energy = sum(.data$Energy),
      ConnectionHours = sum(.data$ConnectionHours),
      ChargingHours = sum(.data$ChargingHours),
      .groups = "drop"
    )

  ss_other_vars <- ss_segmented %>%
    select(!any_of(names(ss_basic_vars)), any_of(c("Session", "Segment"))) %>%
    group_by(.data$Session, .data$Segment) %>%
    summarise_all(first) %>%
    ungroup()

  left_join(
    ss_basic_vars,
    ss_other_vars,
    by = c("Session", "Segment")
  ) %>%
    select(-"Segment")
}




# Print smart charging results --------------------------------------------


#' `print` method for `SmartCharging` object class
#'
#' @param x  `SmartCharging` object returned by `smart_charging`  function
#' @param ... further arguments passed to or from other methods.
#'
#' @returns nothing but prints information about the `SmartCharging` object
#' @export
#' @keywords internal
#'
print.SmartCharging <- function(x, ...) {
  n_windows <- length(x$log)
  summaryS <- summarise_profile_smart_charging_sessions(x$sessions)
  n_sessions <- sum(summaryS$n_sessions[summaryS$group == "Total"])
  n_considered <- sum(summaryS$n_sessions[summaryS$subgroup == "Considered"])
  n_responsive <- sum(summaryS$n_sessions[summaryS$subgroup == "Responsive"])
  n_flexible <- sum(summaryS$n_sessions[summaryS$subgroup == "Flexible"])
  n_exploited <- sum(summaryS$n_sessions[summaryS$subgroup == "Exploited"])

  cat('Smart charging results as a list of 3 objects: charging sessions, user profiles setpoints and log messages.\n')
  cat('Simulation from', as.character(min(date(x$setpoints$datetime))),
      'to', as.character(max(date(x$setpoints$datetime))), 'with a time resolution of',
      get_time_resolution(x$setpoints$datetime, units = "mins"), 'minutes.\n')
  cat('For this time period, there were', n_windows, 'smart charging windows, where:\n')
  cat('  -', n_considered, 'sessions were considered (',
      round(n_considered/n_sessions*100), '% of total data set).\n')
  cat('  -', n_responsive, 'sessions were Responsive (',
      round(n_responsive/n_considered*100), '% of considered sessions).\n')
  cat('  -', n_flexible, 'sessions were Flexible (',
      round(n_flexible/n_responsive*100), '% of responsive sessions).\n')
  cat('  -', n_exploited, 'sessions were Exploited (',
      round(n_exploited/n_flexible*100), '% of flexible sessions).\n')
  if (length(x$log[[1]]) > 0) {
    cat('For more information see the log messages.\n')
  }
}



# Plot smart charging -----------------------------------------------------


#' Plot smart charging results
#'
#' HTML interactive plot showing the comparison between the smart charging setpoint
#' and the actual EV demand after the smart charging program. Also, it is possible
#' to plot the original EV demand.
#'
#' @param smart_charging SmartCharging object, returned by function `smart_charging()`
#' @param sessions tibble, sessions data set containig the following variables:
#' `"Session"`, `"Timecycle"`, `"Profile"`, `"ConnectionStartDateTime"`, `"ConnectionHours"`, `"Power"` and `"Energy"`
#' @param show_setpoint logical, whether to show the setpoint line or not
#' @param by character, name of a character column in `smart_charging$sessions` (e.g. `"Profile"`) or
#' `"FlexType"` (i.e. "Exploited", "Not exploited", "Not flexible", "Not responsive" and "Not considered")
#' @param ... extra arguments of function `evsim::plot_ts()` or other arguments
#' to pass to `dygraphs::dyOptions()`.
#'
#' @return dygraphs plot
#' @export
#'
#' @importFrom dplyr %>% mutate group_by summarise left_join select
#' @importFrom evsim get_demand plot_ts
#' @importFrom dygraphs dyStackedRibbonGroup
#' @importFrom rlang .data
#'
#' @examples
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
#'
#' sc_results <- smart_charging(
#'   sessions, opt_data,
#'   opt_objective = "grid",
#'   method = "curtail",
#'   window_days = 1, window_start_hour = 6
#' )
#'
#' # Plot of setpoint and final EV demand
#' plot_smart_charging(sc_results, legend_show = "onmouseover")
#'
#' # Native `plot` function also works
#' plot(sc_results, legend_show = "onmouseover")
#'
#' # Plot with original demand line
#' plot_smart_charging(sc_results, sessions = sessions, legend_show = "onmouseover")
#'
#' # Plot by "FlexType"
#' plot_smart_charging(sc_results, sessions = sessions, by = "FlexType", legend_show = "onmouseover")
#'
#' # Plot by user "Profile"
#' plot_smart_charging(sc_results, sessions = sessions, by = "Profile", legend_show = "onmouseover")
#'
plot_smart_charging <- function(smart_charging, sessions = NULL, show_setpoint = TRUE, by = NULL, ...) {

  opt_sessions <- smart_charging$sessions

  # Create setpoint time-series profile
  plot_df <- smart_charging$setpoints['datetime']

  if (show_setpoint) {
    plot_df <- plot_df %>%
      mutate(
        Setpoint = rowSums(smart_charging$setpoints[-1])
      )
  }


  # Create flexible demand time-series profile
  if (is.null(by)) {
    ev_demand_flex <- opt_sessions %>%
      mutate(Profile = "Flexible EVs") %>%
      get_demand(dttm_seq = plot_df$datetime, by = "Profile")
  } else {
    if (by == "FlexType") {
      opt_sessions <- opt_sessions %>%
        mutate(
          FlexType = ifelse(
            is.na(.data$Responsive), "Not considered",
            ifelse(
              is.na(.data$Flexible), "Not responsive",
              ifelse(
                is.na(.data$Exploited), "Not flexible",
                ifelse(
                  !.data$Exploited, "Not exploited",
                  "Exploited"
                )
              )
            )
          )
        )

      ribbon_names_all <- c("Not considered", "Not responsive", "Not flexible", "Not exploited", "Exploited")
      ribbon_colors_all <- c("#660066", "#003366", "#003300", "#663300", "#ff9900")
    }

    if (by %in% colnames(select_if(opt_sessions, is.character))) {
      ev_demand_flex <- opt_sessions %>%
        get_demand(dttm_seq = plot_df$datetime, by = by)
      if (by == "FlexType") {
        flextypes_in_data <- which(ribbon_names_all %in% colnames(ev_demand_flex))
        ribbon_names <- ribbon_names_all[flextypes_in_data]
        ribbon_colors <- ribbon_colors_all[flextypes_in_data]
      } else {
        ribbon_names <- unique(opt_sessions[[by]])
        ribbon_colors <- NULL
      }
    } else {
      stop("Error: invalid `by` value")
    }
  }

  plot_df <- plot_df %>%
    left_join(
      ev_demand_flex, by = "datetime"
    )

  if (!is.null(sessions)) {
    ev_demand_static <- sessions %>%
      mutate(Profile = "Original EVs") %>%
      get_demand(dttm_seq = plot_df$datetime, by = "Profile")
    plot_df <- plot_df %>%
      left_join(
        ev_demand_static, by = "datetime"
      )
  }

  # Make plot
  plot_dy <- plot_df %>%
    plot_ts(ylab = "Power (kW)", strokeWidth = 2, ...)

  if (show_setpoint) {
    plot_dy <- plot_dy %>%
      dySeries("Setpoint", strokePattern = "dashed", color = "red", strokeWidth = 2)
  }

  if (is.null(by)) {
    plot_dy <- plot_dy %>%
      dySeries("Flexible EVs", color = "navy")
  } else {
    plot_dy <- plot_dy %>%
      dyStackedRibbonGroup(name = ribbon_names, color = ribbon_colors)
  }

  if (!is.null(sessions)) {
    plot_dy <- plot_dy %>%
      dySeries("Original EVs", strokePattern = "dashed", color = "gray", strokeWidth = 2)
  }

  plot_dy
}


#' `plot` method for `SmartCharging` object class
#'
#' @param x  `SmartCharging` object returned by `smart_charging`  function
#' @param ... further arguments passed to or from other methods.
#'
#' @returns HTML interctive plot
#' @export
#' @keywords internal
#'
#'
plot.SmartCharging <- function(x, ...) {
  plot_smart_charging(x, ...)
}



# Log viewer --------------------------------------------------------------

#' Interactive Log Viewer
#'
#' Launches an interactive Shiny app to explore smart charging logs by window and profile.
#' This function requires the `shiny` package to be installed.
#'
#' @param smart_charging `SmartCharging` object returned by `smart_charging`  function
#'
#' @return Opens Viewer with the log viewer mini app
#' @export
#' @examples
#' \dontrun{
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
#'
#' sc_results <- smart_charging(
#'   sessions, opt_data,
#'   opt_objective = "grid",
#'   method = "curtail",
#'   window_days = 1, window_start_hour = 6,
#'   include_log = TRUE
#' )
#' view_logs(sc_results$log)
#' }
view_smart_charging_logs <- function(smart_charging) {

  log <- smart_charging$log

  if (length(log) == 0) {
    stop("Error: no log messages to visualise.")
  }

  # Check for required packages
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("The 'shiny' package is required to use this function. Please install it with:\ninstall.packages('shiny')", call. = FALSE)
  }

  ui <- shiny::fluidPage(
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::selectInput("selected_window", "Select Window", choices = names(log)),
        shiny::uiOutput("profile_selector")  # Dynamically generated profile list
      ),
      shiny::mainPanel(
        shiny::verbatimTextOutput("log_output")
      )
    )
  )

  server <- function(input, output, session) {

    # Update profile list based on selected window
    output$profile_selector <- shiny::renderUI({
      shiny::req(input$selected_window)
      profiles <- names(log[[input$selected_window]])
      if (is.null(profiles)) return("No logs for this window.")
      shiny::selectInput("selected_profile", "Select Profile", choices = profiles)
    })

    # Show logs based on both selections
    output$log_output <- shiny::renderText({
      shiny::req(input$selected_window, input$selected_profile)
      msgs <- log[[input$selected_window]][[input$selected_profile]]
      if (is.null(msgs)) return("No logs for this profile.")
      paste(msgs, collapse = "\n")
    })

    shiny::observeEvent(input$done, shiny::stopApp())
  }

  shiny::runGadget(ui, server, viewer = shiny::paneViewer())
}




# Sessions' flex type -----------------------------------------------------


#' Get a summary of the new schedule of charging sessions
#'
#' A table is provided containing the number of `Considered`, `Responsive`,
#' `Flexbile` and `Exploited` sessions, by user profile.
#'
#' @param smart_charging SmartCharging object, returned by function `smart_charging()`
#'
#' @importFrom dplyr  %>% group_by group_split group_keys pull
#' @importFrom purrr map list_rbind set_names
#'
#' @export
#'
#' @return tibble with columns:
#' `timecycle` (time-cycle name),
#' `profile` (user profile name),
#' `group` (name of sessions' group),
#' `subgroup` (nome of sessions' subgroup),
#' `n_sessions` (number of sessions) and
#' `pct` (percentage of subgroup sessions from the group)
#'
#'
#' @examples
#' library(dplyr)
#'
#' # Use first 50 sessions
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
#' summarise_smart_charging_sessions(sc_results)
#'
summarise_smart_charging_sessions <- function(smart_charging) {
  grouped_sessions <- smart_charging$sessions %>%
    group_by(.data$Timecycle)

  grouped_sessions %>%
    group_split(.keep = FALSE) %>%
    set_names(group_keys(grouped_sessions)$Timecycle) %>%
    map(
      ~ .x %>%
        group_by(.data$Profile) %>%
        group_split(.keep = FALSE) %>%
        set_names(
          .x %>%
            group_by(.data$Profile) %>%
            group_keys() %>%
            pull(.data$Profile)
        ) %>%
        map(summarise_profile_smart_charging_sessions) %>%
        list_rbind(names_to = "profile")
    ) %>%
    list_rbind(names_to = "timecycle")
}


summarise_timecycle_smart_charging_sessions <- function(time_cycle_sessions) {
  grouped_sessions <- time_cycle_sessions %>%
    group_by(.data$Profile)

  grouped_sessions %>%
    group_split() %>%
    set_names(group_keys(grouped_sessions)$Profile) %>%
    map(summarise_profile_smart_charging_sessions) %>%
    list_rbind(names_to = "profile")
}


#' Get a summary of the new schedule of charging sessions
#'
#' A table is provided containing the number of `Considered`, `Responsive`,
#' `Flexible` and `Exploited` sessions.
#'
#' @param profile_sessions tibble, charging `sessions` object from `smart_charging()`
#'
#' @importFrom dplyr  %>% select group_by all_of summarise mutate_if mutate count filter as_tibble ungroup
#' @importFrom tidyr pivot_longer
#' @importFrom purrr map list_rbind
#'
#' @keywords internal
#'
#' @return tibble with columns
#' `group` (name of sessions' group),
#' `subgroup` (nome of sessions' subgroup),
#' `n_sessions` (number of sessions) and
#' `pct` (percentage of subgroup sessions from the group)
#'
summarise_profile_smart_charging_sessions <- function(profile_sessions) {
  summaryS <- profile_sessions %>%
    select(all_of(c("Session", "Responsive", "Flexible", "Exploited"))) %>%
    group_by(.data$Session) %>%
    summarise(
      Responsive = sum(.data$Responsive, na.rm = FALSE),
      Flexible = sum(.data$Flexible, na.rm = TRUE),
      Exploited = sum(.data$Exploited, na.rm = TRUE)
    ) %>%
    mutate_if(is.numeric, ~ ifelse(.x > 0, TRUE, FALSE)) %>%
    mutate(
      Flexible = ifelse(.data$Responsive, .data$Flexible, NA),
      Exploited = ifelse(.data$Flexible, .data$Exploited, NA)
    ) %>%
    pivot_longer(- "Session") %>%
    group_by(.data$name, .data$value) %>%
    count() %>%
    filter(!(.data$name %in% c("Exploited", "Flexible") & is.na(.data$value)))
  n_sessions <- sum(summaryS$n[summaryS$name == "Responsive"])
  n_considered <- sum(summaryS$n[
    summaryS$name == "Responsive" & !is.na(summaryS$value)
  ], na.rm = T)
  n_responsive <- sum(summaryS$n[
    summaryS$name == "Responsive" & summaryS$value == TRUE
  ], na.rm = T)
  n_flexible <- sum(summaryS$n[
    summaryS$name == "Flexible" & summaryS$value == TRUE
  ], na.rm = T)
  n_exploited <- sum(summaryS$n[
    summaryS$name == "Exploited" & summaryS$value == TRUE
  ], na.rm = T)

  summary_list <- list(
    "Total" = list(
      "Considered" = n_considered,
      "Not considered" = n_sessions - n_considered
    ),
    "Considered" = list(
      "Responsive" = n_responsive,
      "Non responsive" = n_considered - n_responsive
    ),
    "Responsive" = list(
      "Flexible" = n_flexible,
      "Non-flexible" = n_responsive - n_flexible
    ),
    "Flexible" = list(
      "Exploited" = n_exploited,
      "Not exploited" = n_flexible - n_exploited
    )
  )

  summary_list %>%
    map(
      ~ .x %>%
        as_tibble() %>%
        pivot_longer(everything(), names_to = "subgroup", values_to = "n_sessions")
    ) %>%
    list_rbind(names_to = "group") %>%
    group_by(.data$group) %>%
    mutate(pct = round(.data$n_sessions/sum(.data$n_sessions)*100)) %>%
    ungroup() %>%
    filter(n_sessions > 0)
}
















#'
#' #' Control sessions according to optimal setpoint
#' #'
#' #' @param sessions tibble, sessions data set containing the following variables:
#' #' `"Session"`, `"ConnectionStartDateTime"`, `"ConnectionHours"`, `"Power"` and `"Energy"`.
#' #'
#' #' IMPORTANT: Make sure that the `sessions` `ConnectionStartDateTime` and
#' #' `ChargingStartDateTime` are in the same time resolution than `setpoint$datetime`.
#' #' @param setpoint tibble with columns `datetime` and `setpoint`.
#' #' @param method character, being `"postpone"`, `"curtail"` or `"interrupt"`.
#' #' @param power_th numeric, power threshold (between 0 and 1) accepted from setpoint.
#' #' For example, with `power_th = 0.1` and `setpoint = 100` for a certain time slot,
#' #' then sessions' demand can reach a value of `110` without needing to schedule sessions.
#' #' @param user_end_time logical, whether the end time of the session is known or not.
#' #' @param include_log logical, whether to output the algorithm messages for every user profile and time-slot
#' #' @param show_progress logical, whether to output the progress bar in the console
#' #'
#' #' @return list of two elements `sessions` and `log`
#' #' @export
#' #'
#' #' @importFrom dplyr tibble %>% filter mutate arrange desc left_join select
#' #' @importFrom rlang .data
#' #' @importFrom lubridate as_datetime tz
#' #' @importFrom evsim expand_sessions
#' #'
#' control_sessions <- function(sessions, setpoint, method, power_th = 0,
#'                               user_end_time = FALSE,
#'                               include_log = FALSE, show_progress = TRUE) {
#'   log <- c()
#'
#'   # Parameters check
#'   if (is.null(sessions) | nrow(sessions) == 0) {
#'     message("Error: `sessions` parameter is empty.")
#'     return( NULL )
#'   }
#'   sessions_basic_vars <- c(
#'     "Session", "ConnectionStartDateTime", "ConnectionHours", "Power", "Energy"
#'   )
#'   if (!all(sessions_basic_vars %in% colnames(sessions))) {
#'     message("Error: `sessions` does not contain all required variables (see Arguments description)")
#'     return( NULL )
#'   }
#'   if (!all(c("datetime", "setpoint") %in% colnames(setpoint))) {
#'     message("Error: `setpoint` does not contain all required variables (see Arguments description)")
#'     return( NULL )
#'   }
#'
#'   sessions_sch <- sessions %>%
#'     filter(.data$ConnectionStartDateTime %in% setpoint$datetime) %>%
#'     mutate(
#'       ChargingHours = round(.data$Energy/.data$Power, 2),
#'       FlexibilityHours = round(.data$ConnectionHours - .data$ChargingHours, 2)
#'     )
#'   if (nrow(sessions_sch) == 0) {
#'     message("Error: no `sessions` for `setpoint$datetime` period")
#'     return( NULL )
#'   }
#'
#'   resolution <- get_time_resolution(setpoint$datetime, units = "mins")
#'   sessions_expanded <- sessions_sch %>%
#'     expand_sessions(resolution = resolution)
#'   sessions_expanded <- sessions_expanded %>%
#'     mutate(
#'       Power = 0,
#'       EnergyLeft = sessions_expanded$EnergyRequired,
#'       Flexible = FALSE,
#'       Exploited = FALSE
#'     ) %>%
#'     left_join(
#'       select(sessions_sch, all_of(c("Session", "ConnectionStartDateTime", "FlexibilityHours"))),
#'       by = "Session"
#'     )
#'
#'   if (show_progress) {
#'     pb <- progress::progress_bar$new(
#'       format = "[:bar] :percent eta: :eta",
#'       total = length(setpoint$datetime)
#'     )
#'   }
#'
#'   dttm_tz <- tz(setpoint$datetime)
#'
#'   for (timeslot in setpoint$datetime) {
#'
#'     if (show_progress) pb$tick()
#'
#'     # Filter sessions that are charging (`EnergyLeft > 0`) during this time slot
#'     # and calculate their average charging power `PowerTimeslot` in the time slot
#'     # The `PowerTimeslot` is the `PowerNominal` in all time slots, except when
#'     # the session finishes charging in the middle of a time slot.
#'     sessions_timeslot <- sessions_expanded %>%
#'       filter(.data$Timeslot == timeslot, .data$EnergyLeft > 0)
#'
#'     if (nrow(sessions_timeslot) == 0) {
#'       next
#'     }
#'
#'     sessions_timeslot <- sessions_timeslot %>%
#'       mutate(
#'         PowerTimeslot = pmin(
#'           .data$EnergyLeft/(resolution/60), .data$PowerNominal
#'         ),
#'         EnergyCharged = .data$EnergyRequired - .data$EnergyLeft
#'       )
#'
#'     sessions_timeslot_power <- sum(sessions_timeslot$PowerTimeslot)
#'
#'     setpoint_timeslot_power <- setpoint$setpoint[
#'       setpoint$datetime == timeslot
#'     ]
#'
#'     flex_req <- round(sessions_timeslot_power - setpoint_timeslot_power * (1 + power_th), 2)
#'
#'     # If demand should be reduced
#'     if (flex_req > 0) {
#'
#'       if (include_log) {
#'         log_message <- paste(
#'           as_datetime(timeslot, tz=dttm_tz),
#'           "- Flexibility requirement of", flex_req, "kW and",
#'           nrow(sessions_timeslot), "sessions connected."
#'         )
#'
#'         message(log_message)
#'         log <- c(
#'           log,
#'           log_message
#'         )
#'       }
#'
#'       if (method %in% c("postpone", "interrupt")) {
#'
#'         # Time flexibility (Postpone / Interrupt) ----------------------------------------------------------------
#'
#'         # Filter shiftable sessions and arrange them by Flexibility potential
#'         # * Postpone: the EV has not started charging yet, and the energy required
#'         #    can be charged during the rest of the connection time at the nominal charging power.
#'         # * Interrupt: the charge is not completed yet, and the energy required
#'         #    can be charged during the rest of the connection time at the nominal charging power.
#'         if (method == "postpone") {
#'           sessions_timeslot <- sessions_timeslot %>%
#'             mutate(
#'               Flexible = ifelse(
#'                 .data$EnergyCharged == 0,
#'                 TRUE, FALSE
#'               )
#'             )
#'         } else {
#'           sessions_timeslot <- sessions_timeslot %>%
#'             mutate(
#'               Flexible = ifelse(
#'                 .data$EnergyLeft > 0,
#'                 TRUE, FALSE
#'               )
#'             )
#'         }
#'
#'         # Get the maximum flexible power of this time slot
#'         shift_flex_available <- sum(sessions_timeslot$PowerTimeslot[sessions_timeslot$Flexible])
#'
#'         # If the available flexibility is higher than the flexibility request,
#'         # then allow the sessions with less flexibility to charge.
#'         # Else, shift all sessions.
#'         if (shift_flex_available >= flex_req) {
#'
#'           sessions_timeslot_shiftable <- sessions_timeslot %>%
#'             filter(.data$Flexible)
#'
#'           # Arrange charging priority according to the method:
#'           #  - Postpone: start and end times define priority
#'           #  - Interrupt: start and end times, and energy charged define priority
#'           if (method == "postpone") {
#'             if (user_end_time) {
#'               sessions_timeslot_shiftable <- sessions_timeslot_shiftable %>%
#'                 arrange(desc(.data$ConnectionStartDateTime), desc(.data$ConnectionHoursLeft))
#'             } else {
#'               sessions_timeslot_shiftable <- sessions_timeslot_shiftable %>%
#'                 arrange(desc(.data$ConnectionStartDateTime))
#'             }
#'           } else {
#'             if (user_end_time) {
#'               sessions_timeslot_shiftable <- sessions_timeslot_shiftable %>%
#'                 arrange(desc(.data$EnergyCharged), desc(.data$ConnectionStartDateTime), desc(.data$ConnectionHoursLeft))
#'             } else {
#'               sessions_timeslot_shiftable <- sessions_timeslot_shiftable %>%
#'                 arrange(desc(.data$EnergyCharged), desc(.data$ConnectionStartDateTime))
#'             }
#'           }
#'
#'           # Get the index of sessions that can charge
#'           allowed_to_charge_idx <- rep(FALSE, nrow(sessions_timeslot_shiftable))
#'           for (s in seq_len(nrow(sessions_timeslot_shiftable))) {
#'             flex_req <- max(flex_req - sessions_timeslot_shiftable$PowerTimeslot[s], 0)
#'             if (flex_req == 0) break
#'           }
#'           if ((s+1) <= length(allowed_to_charge_idx)) {
#'             allowed_to_charge_idx[seq(s+1, length(allowed_to_charge_idx))] <- TRUE
#'           }
#'
#'           # Update the `Exploited` variable to `TRUE` for sessions to shift
#'           sessions_timeslot$Exploited[
#'             sessions_timeslot$ID %in% sessions_timeslot_shiftable$ID[!allowed_to_charge_idx]
#'           ] <- TRUE
#'
#'         } else {
#'
#'           # All `Flexible` sessions are `Exploited`
#'           sessions_timeslot$Exploited <- sessions_timeslot$Flexible
#'
#'           # Update flexibility requirement with power from all shiftable sessions
#'           flex_req <- round(flex_req - shift_flex_available, 2)
#'
#'           if (include_log) {
#'             log_message <- paste0(
#'               " -- Not enough flexibility available (", shift_flex_available, " kW)"
#'             )
#'             message(log_message)
#'             log <- c(
#'               log,
#'               log_message
#'             )
#'           }
#'
#'         }
#'
#'         # Update the charging power of sessions that are NOT shifted
#'         sessions_timeslot$Power[!sessions_timeslot$Exploited] <-
#'           sessions_timeslot$PowerTimeslot[!sessions_timeslot$Exploited]
#'       }
#'
#'       if ("curtail" %in% method) {
#'
#'         # Power flexibility (Curtail) ----------------------------------------------------------------
#'
#'         # Flexible sessions:
#'         sessions_timeslot$Flexible[
#'           sessions_timeslot$EnergyLeft > 0
#'         ] <- TRUE
#'
#'         # Get the power demand from curtailable sessions
#'         curtailable_sessions_power <- sum(
#'           sessions_timeslot$PowerTimeslot[sessions_timeslot$Flexible]
#'         )
#'
#'         # Power factor that would be required
#'         power_factor <- (curtailable_sessions_power - flex_req)/curtailable_sessions_power
#'
#'         # All `Flexible` sessions are `Exploited`
#'         sessions_timeslot$Exploited <- sessions_timeslot$Flexible
#'
#'         # Update the charging power of curtailed sessions
#'         sessions_timeslot$Power[sessions_timeslot$Exploited] <-
#'           sessions_timeslot$PowerTimeslot[sessions_timeslot$Exploited]*power_factor
#'
#'         # Update the charging power of sessions that are NOT curtailed
#'         sessions_timeslot$Power[!sessions_timeslot$Exploited] <-
#'           sessions_timeslot$PowerTimeslot[!sessions_timeslot$Exploited]
#'
#'         if (include_log) {
#'           flex_provided <- round(curtailable_sessions_power -
#'                                    sum(sessions_timeslot$Power[sessions_timeslot$Exploited]), 2)
#'           if (flex_provided < flex_req) {
#'             log_message <- paste0(
#'               " -- Not enough flexibility available (", flex_provided, " kW)"
#'             )
#'             message(log_message)
#'             log <- c(
#'               log,
#'               log_message
#'             )
#'           }
#'         }
#'
#'         # Update flexibility requirement
#'         flex_req <- round(flex_req - (curtailable_sessions_power - curtailable_sessions_power*power_factor), 2)
#'
#'       }
#'
#'     } else {
#'
#'       if (include_log) {
#'         log_message <- paste(
#'           as_datetime(timeslot, tz=dttm_tz),
#'           "- No flexibility required"
#'         )
#'
#'         message(log_message)
#'         log <- c(
#'           log,
#'           log_message
#'         )
#'       }
#'
#'       # No flexibility required: charge all sessions
#'       sessions_timeslot <- sessions_timeslot %>%
#'         mutate(Power = .data$PowerTimeslot)
#'
#'     }
#'
#'
#'     # Update data set ----------------------------------------------------------------
#'
#'     # For every session update in `sessions_expanded` table
#'     #   1. The charging power during THIS TIME SLOT (session id == `ID`)
#'     #       If energy left is less than the energy that can be charged in this
#'     #       time slot, then only charge the energy left, so the AVERAGE charging
#'     #       power will be calculated with the energy left (lower than nominal power)
#'     #   2. The energy left and the available flexibility for the FOLLOWING time
#'     #       slots of THIS SESSION (session id == `Session`)
#'     for (s in seq_len(nrow(sessions_timeslot))) {
#'
#'       # Update `Power`
#'       sessions_expanded$Power[
#'         sessions_expanded$ID == sessions_timeslot$ID[s]
#'       ] <- sessions_timeslot$Power[s]
#'       # Update `Flexible`
#'       sessions_expanded$Flexible[
#'         sessions_expanded$ID == sessions_timeslot$ID[s]
#'       ] <- sessions_timeslot$Flexible[s]
#'       # Update `Exploited`
#'       sessions_expanded$Exploited[
#'         sessions_expanded$ID == sessions_timeslot$ID[s]
#'       ] <- sessions_timeslot$Exploited[s]
#'
#'
#'       # If there are more time slots afterwards, update `EnergyLeft` and `FlexibilityHours`
#'       session_after_idx <- (sessions_expanded$Session == sessions_timeslot$Session[s]) &
#'         (sessions_expanded$ID > sessions_timeslot$ID[s])
#'
#'       if (length(session_after_idx) > 0) {
#'
#'         # Update `EnergyLeft`
#'         session_energy <- round(sessions_timeslot$Power[s]*resolution/60, 2)
#'         session_energy_left <- round(sessions_timeslot$EnergyLeft[s] - session_energy, 2)
#'         sessions_expanded$EnergyLeft[session_after_idx] <- session_energy_left
#'
#'         # Update `FlexibilityHours`
#'         session_flexibility_hours <- round(max(sessions_timeslot$FlexibilityHours[s] - resolution/60, 0), 2)
#'         sessions_timeslot$FlexibilityHours[s] <- session_flexibility_hours
#'         sessions_expanded$FlexibilityHours[session_after_idx] <- session_flexibility_hours
#'
#'       } else {
#'         session_energy_left <- 0
#'         session_flexibility_hours <- 0
#'       }
#'
#'     }
#'
#'     # Log message ----------------------------------------------------------------
#'
#'     if (include_log) {
#'
#'       exploited_sessions <- sessions_timeslot %>% filter(.data$Exploited)
#'
#'       for (s in seq_len(nrow(exploited_sessions))) {
#'
#'         if (method %in% c("postpone", "interrupt")) {
#'           if (user_end_time) {
#'             session_flexibility_hours <- exploited_sessions$FlexibilityHours[s]
#'             original_flexibility <- sessions_sch$FlexibilityHours[sessions$Session == exploited_sessions$Session[s]]
#'             pct_flexibility_available <- round(session_flexibility_hours/original_flexibility*100)
#'             log_message <- paste0(
#'               "---- Session ", exploited_sessions$Session[s], " shifted (",
#'               exploited_sessions$PowerTimeslot[s], " kW, ",
#'               pct_flexibility_available, "% of flexible time still available)"
#'             )
#'           } else {
#'             log_message <- paste0(
#'               "---- Session ", exploited_sessions$Session[s], " shifted (",
#'               exploited_sessions$PowerTimeslot[s], " kW)"
#'             )
#'           }
#'
#'         } else {
#'           power_reduction <- round(exploited_sessions$PowerTimeslot[s] - exploited_sessions$Power[s], 2)
#'           pct_power_reduction <- round(power_reduction/exploited_sessions$PowerTimeslot[s]*100)
#'           log_message <- paste0(
#'             "---- Session ", exploited_sessions$Session[s],
#'             " provides ", power_reduction, " kW of flexibility (",
#'             pct_power_reduction, "% power reduction)"
#'           )
#'         }
#'
#'         message(log_message)
#'         log <- c(
#'           log,
#'           log_message
#'         )
#'       }
#'
#'     }
#'
#'   }
#'
#'   # Update the sessions data set with all variables from the original data set
#'   sessions_expanded2 <- left_join(
#'     sessions_expanded %>%
#'       select(any_of(c(
#'         'Session', 'Timeslot', 'Power', 'FlexibilityHours', 'EnergyLeft',
#'         'ConnectionHoursLeft', 'Flexible', 'Exploited'
#'       ))),
#'     sessions_sch %>%
#'       select('Session', !any_of(evsim::sessions_feature_names)),
#'     by = 'Session'
#'   ) %>%
#'     mutate(
#'       ConnectionStartDateTime = .data$Timeslot,
#'       ConnectionEndDateTime = .data$Timeslot + minutes(resolution),
#'       ChargingStartDateTime = .data$Timeslot,
#'       ChargingEndDateTime = .data$Timeslot + minutes(resolution),
#'       ConnectionHours = resolution/60,
#'       ChargingHours = resolution/60,
#'       Energy = round(.data$Power*.data$ChargingHours, 2)
#'     ) %>%
#'     select(any_of(names(sessions)), 'EnergyLeft', 'ConnectionHoursLeft', 'Flexible', 'Exploited')
#'
#'   return(
#'     list(
#'       sessions = sessions_expanded2,
#'       log = log
#'     )
#'   )
#' }
#'



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
