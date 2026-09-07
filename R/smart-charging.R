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
#' - `load_capacity`: maximum charging power (in kW) allowed for the flexible
#' EV demand at every time slot. By default the algorithm already limits the
#' optimized setpoint to the nominal power of the connected EVs (the physical
#' charging envelope). This variable can be used to tighten that limit further
#' (e.g. a charging point or transformer power limit).
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
#' @param opt_objective character, optimization objective being `"none"`,
#'  `"grid"`, `"cost"`, `"capacity"` or a value between 0 (cost) and 1 (grid).
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
#' (1) minimum allowed ratio (between 0 and 0.999) of nominal power (i.e. `Power` column in `sessions`), or
#' (2) specific value of minimum power (in kW) from 1 kW or higher.
#'
#' For example, if `charging_power_min = 0.5` and `method = 'curtail'`, sessions' charging power can only
#' be curtailed until the 50% of the nominal charging power.
#' And if `charging_power_min = 2`, sessions' charging power can be curtailed until 2 kW.
#'
#' @param energy_min numeric, minimum allowed ratio (between 0 and 1) of required energy.
#' Every session charges at least this share of the energy it requires. The
#' scheduler never curtails, postpones or interrupts a session below it, and the
#' setpoint optimization may drop energy down to it - and no further - when the
#' grid capacity cannot fit `energy_max`. When even this minimum does not fit,
#' the grid capacity is relaxed only as far as the minimum-energy profile needs,
#' so the minimum is delivered at the expense of the capacity.
#' @param energy_max numeric, maximum allowed ratio (between 0 and 1) of required energy.
#' Every session charges at most this share of the energy it requires, and the
#' setpoint optimization targets exactly this share for the responsive sessions.
#' Lower it to save energy or cost. Must be higher than 0 and at least `energy_min`.
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
#' @importFrom timefully get_time_resolution
#'
#' @return a list with three elements:
#' optimal setpoints (tibble), sessions schedule (tibble) and log messages
#' (list of character vectors, one per window). The date-time values in the log
#' list are in the time zone of the `opt_data`.
#'
#' A session whose connection crosses an optimization window boundary is split
#' into one part per window before scheduling (energy shared by connection
#' time), so the sessions schedule carries a `Part` column and such a session
#' returns several rows per part; its delivered energy is the sum of its rows'
#' `Energy` grouped by `Session`.
#' @export
#'
#' @details
#' An important parameter of this function is `opt_data`, which defines the time
#' sequence of the smart charging algorithm and the optimization variables.
#' The `opt_data` parameter is directly related with the `opt_objective` parameter.
#' There are four different optimization objectives implemented by this function:
#'
#' - Minimize grid interaction (`opt_objective = "grid"`): minimizes the peak of
#' the flexible load and the amount of imported power from the grid.
#' If `production` is not found in `opt_data`, only a peak shaving objective
#' will be considered.
#'
#' - Minimize capacity violations (`opt_objective = "capacity"`): minimizes only
#' the load slices that exceed the grid capacity limits (`import_capacity` or
#' `export_capacity` in `opt_data`), leaving the rest of the load profile
#' unchanged. Falls back to grid objective when the capacity slice is infeasible.
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
#' considered as a setpoint for the scheduling algorithm, or a grid capacity variable
#' such as `grid_capacity`, `import_capacity`or `export_capacity`.
#' The user profiles that don't appear in `opt_data` will not be optimized.
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
#'   sessions, opt_data,
#'   opt_objective = "grid", method = "curtail",
#'   window_days = 1, window_start_hour = 6,
#'   responsive = list(Workday = list(Worktime = 0.9)),
#'   energy_min = 0.5
#' )
#'
smart_charging <- function(
  sessions,
  opt_data,
  opt_objective,
  method,
  window_days,
  window_start_hour,
  responsive = NULL,
  power_th = 0,
  charging_power_min = 0,
  energy_min = 1,
  energy_max = 1,
  include_log = FALSE,
  show_progress = FALSE,
  lambda = 0
) {
  if (show_progress) {
    cli::cli_h1("Set up")
  }

  if (show_progress) {
    cli::cli_progress_step("Checking parameters")
  }

  # Parameters check
  if (is.null(sessions) | nrow(sessions) == 0) {
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

  check_energy_ratios(energy_min, energy_max)

  if (is.null(responsive)) {
    responsive <- map(
      set_names(unique(sessions$Timecycle)),
      ~ map(
        set_names(unique(sessions$Profile)),
        ~1
      )
    )
  } else {
    responsive_time_cycles <- names(responsive)
    responsive_user_profiles <- unlist(map(responsive, names))
    sessions_time_cycles <- unique(sessions$Timecycle)
    sessions_user_profiles <- unique(sessions$Profile)

    # Check that all content in `responsive` match the content in `sessions`
    if (!all(responsive_time_cycles %in% sessions_time_cycles)) {
      message(
        "Warning: time cycle name in `responsive` not found in `sessions`"
      )
      # return( NULL )
    }
    if (!all(responsive_user_profiles %in% sessions_user_profiles)) {
      message(
        "Warning: user profile name in `responsive` not found in `sessions`"
      )
      # return( NULL )
    }
  }

  opt_data$flexible <- 0
  opt_data <- check_optimization_data(opt_data, opt_objective)

  if (show_progress) {
    cli::cli_progress_step("Defining optimization windows")
  }

  # Adapt the data set for the current time resolution
  dttm_seq <- opt_data$datetime
  time_resolution <- get_time_resolution(dttm_seq, units = "mins")
  sessions <- sessions %>%
    adapt_charging_features(time_resolution = time_resolution)

  # Optimization windows according to `window_days` and `window_start_hour`
  flex_windows_idx <- get_flex_windows(
    dttm_seq,
    window_days,
    window_start_hour
  )

  # A session whose connection crosses a window boundary is split into one
  # part per window (see `split_sessions_at_windows()`), so every part is
  # scheduled in the window it is connected in. Before this, such a session
  # belonged to the window it started in, was never marked responsive there
  # (its charging ended past the window) and charged unmanaged at full power.
  window_boundaries <- c(
    dttm_seq[flex_windows_idx$start],
    dttm_seq[max(flex_windows_idx$end)] + lubridate::minutes(time_resolution)
  )
  sessions <- split_sessions_at_windows(
    sessions,
    window_boundaries,
    time_resolution
  )

  # Get user profiles demand
  if (show_progress) {
    cli::cli_progress_step("Calculating EV demand")
  }
  profiles_demand <- get_demand(sessions, dttm_seq)

  # SMART CHARGING ----------------------------------------------------------
  if (show_progress) {
    cli::cli_h1("Smart charging")
  }

  # Set responsive sessions -------------------------------------------------
  if (show_progress) {
    cli::cli_progress_step("Setting responsiveness")
  }

  windows_data <- map(
    flex_windows_idx$flex_idx,
    function(flex_idx) {
      sessions_window <- sessions %>%
        filter(
          .data$ChargingStartDateTime >= dttm_seq[flex_idx[1]],
          .data$ChargingStartDateTime <= dttm_seq[flex_idx[length(flex_idx)]]
        ) %>%
        set_responsive(
          dttm_seq[flex_idx],
          responsive,
          time_resolution
        )
      list(
        sessions_window = sessions_window,
        profiles_demand = profiles_demand[flex_idx, ],
        opt_data = opt_data[flex_idx, ]
      )
    }
  )

  # Get setpoints -----------------------------------------------------------
  if (show_progress) {
    cli::cli_progress_step("Defining setpoints")
  }

  setpoints_lst <- get_setpoints_parallel(
    windows_data,
    opt_objective,
    lambda,
    energy_min,
    energy_max
  )

  # Scheduling --------------------------------------------------------------

  if (method != "none") {
    if (show_progress) {
      cli::cli_progress_step("Scheduling EV sessions")
    }

    scheduling_lst <- smart_charging_window_parallel(
      windows_data,
      setpoints_lst,
      method,
      power_th,
      charging_power_min,
      energy_min,
      energy_max,
      include_log
    )
  }

  if (show_progress) {
    cli::cli_progress_step("Cleaning data set")
  }

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
      scheduling_lst,
      ~ .x$sessions
    ) %>%
      list_rbind()

    if (nrow(sessions_considered) > 0) {
      # Key on (Session, Part): a split session's parts live in different
      # windows, and one part being scheduled says nothing about the others.
      sessions_not_considered <- sessions[
        !(session_part_key(sessions) %in% session_part_key(sessions_considered)),
      ]
    } else {
      sessions_not_considered <- sessions %>%
        mutate(
          Responsive = NA,
          Flexible = NA,
          Exploited = NA
        )
    }

    sessions_opt <- bind_rows(
      sessions_not_considered,
      sessions_considered
    ) %>%
      select(any_of(names(sessions)), everything()) %>% # Set order of columns
      mutate(Session = factor(.data$Session, levels = unique(sessions$Session))) %>% # Convert `Session` to factor to be sorted
      arrange(
        .data$Session,
        .data$ConnectionStartDateTime,
        .data$ConnectionEndDateTime
      ) %>%
      mutate(Session = as.character(.data$Session)) %>% # Convert `Session` back to character
      distinct()

    demand <- map(
      scheduling_lst,
      ~ .x$demand
    ) %>%
      list_rbind()

    # Join with the original datetime sequence and demand
    demand_opt <- profiles_demand
    opt_dttm_idx <- demand_opt$datetime %in% demand$datetime
    demand_opt[opt_dttm_idx, names(demand)] <- demand
    # demand_opt[is.na(demand_opt)] <- 0 # ReplaceNA
    # Round once, here, and to the same four decimals as the session rows. Not
    # to two: the demand is one column per profile and callers sum the
    # columns, so three profiles each rounded up by half a cent read as a cap
    # met at 3.00 kW being exceeded at 3.01.
    demand_cols <- names(demand_opt) != "datetime"
    demand_opt[demand_cols] <- lapply(
      demand_opt[demand_cols],
      round,
      SCHEDULE_OUTPUT_DIGITS
    )

    log_lst <- map(
      scheduling_lst,
      ~ .x$log
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


#' Split sessions at the optimization window boundaries
#'
#' A session whose connection crosses a window boundary is cut into one part
#' per window: each part keeps the session's `Session` id, `Power`, `Profile`
#' and `Timecycle`, gets the real connection interval of its window, a `Part`
#' number, and a share of the energy proportional to its connection time.
#' Sessions that cross no boundary are returned unchanged with `Part = 1`.
#'
#' The share is by connection time, not by what nominal charging would deliver
#' first: a car that arrives 45 minutes before a boundary and stays nine hours
#' would otherwise carry most of its energy into those 45 minutes, where no
#' capacity could hold it and the `energy_min` floor would force it through.
#' Proportional puts the energy where the flexibility is. The parts sum to the
#' session's energy up to the 2-decimal rounding of each part.
#'
#' A part that could not fill one time slot at nominal power (energy below
#' `Power * time_resolution / 60`) is folded into its longest neighbouring
#' part instead of being emitted: [evsim::get_demand()] renders sub-slot
#' charging as a whole slot at nominal power, so such a part would enter the
#' optimizer as several times its real energy. Folding keeps the energy and
#' drops the sliver of connection time, which is what the scheduler could do
#' with it anyway. A session that arrives shortly before a boundary and stays
#' long therefore moves whole into the next window. The charging features of
#' every part are recomputed with [evsim::adapt_charging_features()].
#'
#' @param sessions tibble, sessions as returned by
#'   [evsim::adapt_charging_features()]
#' @param boundaries POSIXct vector, window start instants (and the end of the
#'   last window); only boundaries strictly inside a connection split it
#' @param time_resolution numeric, minutes
#'
#' @return the sessions tibble with a `Part` column, possibly more rows
#' @keywords internal
#'
split_sessions_at_windows <- function(sessions, boundaries, time_resolution) {
  if (!("Part" %in% names(sessions))) {
    sessions$Part <- 1L
  }
  if (nrow(sessions) == 0 || length(boundaries) == 0) {
    return(sessions)
  }
  boundaries <- sort(unique(boundaries))

  inner_boundaries <- lapply(seq_len(nrow(sessions)), function(i) {
    boundaries[
      boundaries > sessions$ConnectionStartDateTime[i] &
        boundaries < sessions$ConnectionEndDateTime[i]
    ]
  })
  crosses <- lengths(inner_boundaries) > 0
  if (!any(crosses)) {
    return(sessions)
  }

  one_slot_hours <- time_resolution / 60

  split_one <- function(session, cuts) {
    cuts <- c(session$ConnectionStartDateTime, cuts, session$ConnectionEndDateTime)
    starts <- cuts[-length(cuts)]
    ends <- cuts[-1]
    hours <- as.numeric(ends - starts, units = "hours")
    energy <- session$Energy * hours / sum(hours)

    # Fold every part that cannot fill one slot at nominal power into its
    # longest neighbour: the neighbour keeps its own interval and gains the
    # energy, the sliver's connection time is dropped.
    one_slot_energy <- session$Power * one_slot_hours
    repeat {
      tiny <- which(energy < one_slot_energy)
      if (length(tiny) == 0 || length(hours) == 1) {
        break
      }
      i <- tiny[1]
      neighbours <- c(i - 1, i + 1)
      neighbours <- neighbours[neighbours >= 1 & neighbours <= length(hours)]
      into <- neighbours[which.max(hours[neighbours])]
      energy[into] <- energy[into] + energy[i]
      starts <- starts[-i]
      ends <- ends[-i]
      hours <- hours[-i]
      energy <- energy[-i]
    }

    parts <- session[rep(1, length(hours)), ]
    parts$ConnectionStartDateTime <- starts
    parts$ConnectionEndDateTime <- ends
    parts$ConnectionHours <- round(hours, 2)
    parts$Energy <- round(energy, 2)
    parts$Part <- seq_along(hours)
    parts[parts$Energy > 0, ]
  }

  split_parts <- purrr::map2(
    split(sessions[crosses, ], seq_len(sum(crosses))),
    inner_boundaries[crosses],
    split_one
  ) %>%
    list_rbind() %>%
    adapt_charging_features(time_resolution = time_resolution)

  bind_rows(sessions[!crosses, ], split_parts) %>%
    arrange(.data$ConnectionStartDateTime, .data$Session, .data$Part)
}


session_part_key <- function(sessions) {
  paste(sessions$Session, sessions$Part, sep = "")
}


#' Validate the pair of energy ratios
#'
#' Shared by [smart_charging()] and [schedule_sessions()]. Both ratios are
#' shares of each session's energy requirement: `energy_min` is what must be
#' delivered even at the expense of the grid capacity, `energy_max` is what may
#' be delivered at most.
#'
#' @param energy_min numeric, between 0 and 1.
#' @param energy_max numeric, higher than 0 and at most 1; at least `energy_min`.
#'
#' @return `TRUE`, invisibly. Stops with a clear message otherwise.
#' @keywords internal
#'
check_energy_ratios <- function(energy_min, energy_max) {
  is_ratio <- function(x) is.numeric(x) && length(x) == 1 && is.finite(x)
  if (!is_ratio(energy_min) || energy_min < 0 || energy_min > 1) {
    stop("Error: `energy_min` must be a single number between 0 and 1")
  }
  if (!is_ratio(energy_max) || energy_max <= 0 || energy_max > 1) {
    stop("Error: `energy_max` must be a single number higher than 0 and at most 1")
  }
  if (energy_min > energy_max) {
    stop("Error: `energy_min` cannot be higher than `energy_max`")
  }
  invisible(TRUE)
}

#' Set `Responsive` column in `sessions`
#'
#' @param sessions_window tibble, sessions corresponding to a single windows
#' @param dttm_seq datetime vector
#' @param responsive named list with responsive ratios
#' @param time_resolution numeric, time resolution in minutes
#'
#' @importFrom dplyr tibble %>% filter mutate select everything row_number left_join bind_rows any_of pull distinct between sym all_of
#' @importFrom lubridate hour minute date minutes
#' @importFrom rlang .data
#' @importFrom purrr set_names
#' @importFrom evsim get_demand adapt_charging_features
#'
#' @keywords internal
#'
set_responsive <- function(
  sessions_window,
  dttm_seq,
  responsive,
  time_resolution
) {
  if (nrow(sessions_window) == 0) {
    return(tibble())
  }

  # The window ends one resolution after its last slot.
  end_dttm <- dttm_seq[length(dttm_seq)]
  max_end_connection_dttm <- end_dttm + minutes(time_resolution)

  # Responsiveness is looked up per (time cycle, profile) of each session, not
  # by the window's dominant time cycle: a Friday session whose connection
  # carries over into Saturday's window keeps Friday's responsiveness. A pair
  # that `responsive` does not configure, or configures at 0, is not
  # considered (it stays out of the returned set, as before).
  groups <- sessions_window %>%
    distinct(.data$Timecycle, .data$Profile)

  sessions_considered <- tibble()

  for (g in seq_len(nrow(groups))) {
    time_cycle <- groups$Timecycle[g]
    profile <- groups$Profile[g]
    ratio <- responsive[[time_cycle]][[profile]]
    if (is.null(ratio) || !is.numeric(ratio) || length(ratio) != 1 || ratio <= 0) {
      next
    }

    sessions_group <- sessions_window %>%
      filter(.data$Timecycle == time_cycle, .data$Profile == profile)

    # RESPONSIVENESS
    sessions_group$Responsive <- NA

    # Potentially responsive: the charging ends inside the window. A session
    # whose charging ends exactly on the next boundary is inside. Since
    # `split_sessions_at_windows()` no connection crosses a boundary, so this
    # only excludes sessions that outlast the whole sequence.
    #
    # That is the only condition. Up to 1.7.x a second one excluded sessions
    # whose connection times fell outside the profile's 95% band in the window
    # (mean +- 2 sd), to stop one late arrival from stretching the setpoint
    # span and smearing the profile's energy into slots where only that car was
    # plugged in. The setpoint LP now bounds every slot by the nominal power of
    # the sessions actually connected (`LFmax`), which removes that failure
    # mode at the source; what the band still did was leave the excluded
    # sessions charging unmanaged at full power, and understate the responsive
    # share the caller asked for.
    potentially_responsive_idx <- which(
      sessions_group$ChargingEndDateTime <= max_end_connection_dttm
    )

    # From the potentially responsive sessions, randomly select the configured
    # share. `sample.int` on the count, not `sample()` on the indices: with a
    # single eligible row `sample(idx, 1)` draws from `1:idx` instead.
    n_responsive <- round(length(potentially_responsive_idx) * ratio)
    set.seed(1234)
    responsive_idx <- potentially_responsive_idx[
      sample.int(length(potentially_responsive_idx), n_responsive)
    ]
    non_responsive_idx <- setdiff(potentially_responsive_idx, responsive_idx)
    sessions_group$Responsive[responsive_idx] <- TRUE
    sessions_group$Responsive[non_responsive_idx] <- FALSE

    # For the `Responsive` sessions, limit the `ConnectionEndDateTime` to the
    # window's end
    sessions_group$ConnectionEndDateTime[
      sessions_group$Responsive %in% TRUE &
        (sessions_group$ConnectionEndDateTime > max_end_connection_dttm)
    ] <- max_end_connection_dttm
    sessions_group$ConnectionHours <- round(
      as.numeric(
        sessions_group$ConnectionEndDateTime -
          sessions_group$ConnectionStartDateTime,
        unit = "hours"
      ),
      2
    )

    sessions_considered <- bind_rows(sessions_considered, sessions_group)
  }

  return(sessions_considered)
}


get_opt_profiles <- function(sessions_window) {
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
    profiles_flexpotential %>%
      dplyr::arrange(desc(.data$FlexibilityHours)) %>%
      dplyr::select(-"FlexibilityHours")
  } else {
    profiles_flexpotential %>%
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
#' @param energy_min,energy_max numeric, minimum and maximum share (between 0
#'   and 1) of each profile's energy the setpoint must carry. See
#'   [smart_charging()].
#'
#' @importFrom dplyr tibble %>% filter mutate select everything row_number left_join bind_rows any_of pull distinct between sym all_of
#' @importFrom lubridate hour minute date
#' @importFrom rlang .data
#' @importFrom stats sd
#' @importFrom purrr set_names
#' @importFrom evsim get_demand adapt_charging_features
#' @importFrom timefully get_time_resolution
#'
#' @keywords internal
#'
get_setpoints <- function(
  sessions_window,
  opt_data,
  profiles_demand,
  opt_objective,
  lambda,
  energy_min = 1,
  energy_max = 1
) {
  # Both ratios are shares of the responsive sessions' own demand `LF`; the
  # non-responsive demand `L_fixed_prof` is added back untouched below.
  energy_ratio <- c(energy_min, energy_max)
  if (nrow(sessions_window) == 0) {
    return(profiles_demand)
  }

  dttm_seq <- opt_data$datetime
  time_resolution <- get_time_resolution(dttm_seq, units = "mins")

  if ("static" %in% colnames(opt_data)) {
    L_fixed <- opt_data$static
  } else {
    L_fixed <- rep(0, nrow(opt_data))
  }

  opt_profiles <- get_opt_profiles(sessions_window)
  setpoints <- profiles_demand

  for (profile in opt_profiles) {
    # If `opt_data` contains user profile's name,
    # this is considered to be a setpoint (skip optimization)
    if (profile %in% colnames(opt_data)) {
      setpoints[[profile]] <- opt_data[[profile]]
    } else {
      # Separate responsive and non-responsive sessions -----------------------------------------------------
      sessions_window_prof_flex <- sessions_window %>%
        filter(.data$Profile == profile & .data$Responsive)

      non_responsive_sessions <- sessions_window %>%
        filter(
          .data$Profile == profile &
            (!.data$Responsive | is.na(.data$Responsive))
        )

      if (nrow(non_responsive_sessions) > 0) {
        L_fixed_prof <- non_responsive_sessions %>%
          get_demand(dttm_seq = dttm_seq, by = "Profile") %>%
          pull(!!sym(profile))
      } else {
        L_fixed_prof <- rep(0, length(dttm_seq))
      }

      # The optimization flexible load is the load of the responsive sessions
      LF <- setpoints[[profile]] - L_fixed_prof

      # Static load
      # Here we consider `setpoints` instead of `profiles_demand` because we
      # update it in every iteration (optimization)
      L_others <- setpoints %>%
        select(-any_of(c(profile, "datetime"))) %>%
        rowSums()
      if (length(L_others) == 0) {
        L_others <- rep(0, length(dttm_seq))
      }
      LS <- L_fixed + L_others + L_fixed_prof

      # Optimization ------------------------------------------------------------

      # Setpoint datetime sequence
      window_prof_dttm <- c(
        min(sessions_window_prof_flex$ConnectionStartDateTime),
        min(
          max(sessions_window_prof_flex$ConnectionEndDateTime),
          dttm_seq[length(dttm_seq)]
        )
      )
      opt_idxs <- (dttm_seq >= window_prof_dttm[1]) &
        (dttm_seq <= window_prof_dttm[2])

      # Maximum charging power of the flexible load at each time slot.
      # The optimized setpoint can be reshaped in time but must never exceed
      # the power that the connected EVs can physically draw, i.e. their
      # nominal power while connected. This envelope is the sum of the nominal
      # `Power` of the responsive sessions over their connection window,
      # computed as the demand they would produce if charging at nominal power
      # during the whole connection time. A user-supplied `load_capacity`
      # column in `opt_data` can tighten this limit further.
      if (nrow(sessions_window_prof_flex) > 0) {
        LFmax_prof <- sessions_window_prof_flex %>%
          mutate(
            ChargingStartDateTime = .data$ConnectionStartDateTime,
            ChargingEndDateTime = .data$ConnectionEndDateTime,
            ChargingHours = .data$ConnectionHours,
            Energy = .data$Power * .data$ConnectionHours
          ) %>%
          get_demand(dttm_seq = dttm_seq, by = "Profile") %>%
          pull(!!sym(profile))
      } else {
        LFmax_prof <- rep(Inf, length(dttm_seq))
      }
      LFmax_prof <- pmin(LFmax_prof, opt_data$load_capacity)

      if (opt_objective != "none") {
        # Optimize the flexible profile's load according to `opt_objective`
        if (opt_objective == "grid") {
          O <- demand_grid_window(
            G = opt_data$production[opt_idxs],
            LF = LF[opt_idxs],
            LS = LS[opt_idxs],
            direction = "forward",
            time_horizon = NULL,
            LFmax = LFmax_prof[opt_idxs],
            import_capacity = opt_data$import_capacity[opt_idxs],
            export_capacity = opt_data$export_capacity[opt_idxs],
            lambda = lambda,
            energy_ratio = energy_ratio
          )
        } else if (opt_objective == "cost") {
          O <- demand_cost_window(
            G = opt_data$production[opt_idxs],
            LF = LF[opt_idxs],
            LS = LS[opt_idxs],
            PI = opt_data$price_imported[opt_idxs],
            PE = opt_data$price_exported[opt_idxs],
            PTU = opt_data$price_turn_up[opt_idxs],
            PTD = opt_data$price_turn_down[opt_idxs],
            direction = "forward",
            time_horizon = NULL,
            LFmax = LFmax_prof[opt_idxs],
            import_capacity = opt_data$import_capacity[opt_idxs],
            export_capacity = opt_data$export_capacity[opt_idxs],
            lambda = lambda,
            energy_ratio = energy_ratio
          )
        } else if (opt_objective == "capacity") {
          O <- demand_capacity_window(
            G = opt_data$production[opt_idxs],
            LF = LF[opt_idxs],
            LS = LS[opt_idxs],
            direction = "forward",
            time_horizon = NULL,
            LFmax = LFmax_prof[opt_idxs],
            import_capacity = opt_data$import_capacity[opt_idxs],
            export_capacity = opt_data$export_capacity[opt_idxs],
            lambda = lambda,
            energy_ratio = energy_ratio
          )
        } else if (is.numeric(opt_objective)) {
          O <- demand_combined_window(
            G = opt_data$production[opt_idxs],
            LF = LF[opt_idxs],
            LS = LS[opt_idxs],
            PI = opt_data$price_imported[opt_idxs],
            PE = opt_data$price_exported[opt_idxs],
            PTU = opt_data$price_turn_up[opt_idxs],
            PTD = opt_data$price_turn_down[opt_idxs],
            direction = "forward",
            time_horizon = NULL,
            LFmax = LFmax_prof[opt_idxs],
            import_capacity = opt_data$import_capacity[opt_idxs],
            export_capacity = opt_data$export_capacity[opt_idxs],
            w = opt_objective,
            lambda = lambda,
            energy_ratio = energy_ratio
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
          0 # Not negative power
        )

        # Capacity available should allow the energy that MUST be charged (the
        # `energy_min` share of the profile's demand) to avoid pushing the
        # demand to the end of the window. In case of capacity limitation, we
        # increase the capacity available by a factor - only as far as that
        # minimum requires. The scheduler stops every session at its
        # `energy_max` target, so this setpoint is a ceiling, not a target.
        inc_capacity_factor <- max(
          energy_min *
            sum(profiles_demand[[profile]]) /
            sum(capacity_available),
          1
        )
        setpoints[[profile]] <- capacity_available *
          inc_capacity_factor
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


get_setpoints_parallel <- function(
  windows_data,
  opt_objective,
  lambda,
  energy_min = 1,
  energy_max = 1
) {
  reset_message_once()

  if (
    !requireNamespace("mirai", quietly = TRUE) ||
      !requireNamespace("carrier", quietly = TRUE)
  ) {
    setpoints_lst <- purrr::map(
      windows_data,
      \(x) {
        get_setpoints(
          sessions_window = x$sessions_window,
          profiles_demand = x$profiles_demand,
          opt_data = x$opt_data,
          opt_objective = opt_objective,
          lambda = lambda,
          energy_min = energy_min,
          energy_max = energy_max
        )
      }
    )
  } else {
    setpoints_lst <- purrr::map(
      windows_data,
      purrr::in_parallel(
        \(x) {
          get_setpoints(
            sessions_window = x$sessions_window,
            profiles_demand = x$profiles_demand,
            opt_data = x$opt_data,
            opt_objective = opt_objective,
            lambda = lambda,
            energy_min = energy_min,
            energy_max = energy_max
          )
        },
        get_setpoints = get_setpoints,
        opt_objective = opt_objective,
        lambda = lambda,
        energy_min = energy_min,
        energy_max = energy_max
      )
    )
  }
  return(setpoints_lst)
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
#' @param energy_max numeric, maximum allowed ratio (between 0 and 1) of required energy.
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
smart_charging_window <- function(
  sessions_window,
  profiles_demand,
  setpoints,
  method,
  power_th = 0,
  charging_power_min = 0,
  energy_min = 1,
  energy_max = 1,
  include_log = FALSE
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
    filter(.data$Responsive)

  non_responsive_sessions <- sessions_window %>%
    filter(!.data$Responsive | is.na(.data$Responsive))

  if (nrow(non_responsive_sessions) > 0) {
    L_fixed_total <- non_responsive_sessions %>%
      get_demand(dttm_seq = dttm_seq, by = "Profile") %>%
      select(-"datetime") %>%
      rowSums()
  } else {
    L_fixed_total <- rep(0, length(dttm_seq))
  }

  setpoint_total <- setpoints %>%
    select(any_of(window_profiles)) %>%
    rowSums()
  if (length(setpoint_total) == 0) {
    setpoint_total <- rep(0, length(dttm_seq))
  }

  setpoint_flex <- tibble(
    datetime = dttm_seq,
    setpoint = setpoint_total - L_fixed_total
  )

  results_log <- character(0)
  if (nrow(sessions_window_flex) > 0) {
    results <- schedule_sessions(
      sessions = sessions_window_flex,
      setpoint = setpoint_flex,
      method = method,
      power_th = power_th,
      charging_power_min = charging_power_min,
      energy_min = energy_min,
      energy_max = energy_max,
      include_log = include_log,
      show_progress = FALSE
    )
    sessions_window_flex_final <- results$sessions
    scheduled_demand <- results$demand
    results_log <- results$log
  } else {
    sessions_window_flex_final <- tibble()
    scheduled_demand <- tibble(datetime = dttm_seq)
  }

  sessions_window_final <- bind_rows(
    sessions_window_flex_final,
    non_responsive_sessions
  )

  if (nrow(sessions_window_final) > 0) {
    # The window's demand for the profiles it schedules: the scheduler's exact
    # per-slot powers for the responsive sessions, plus the unmanaged demand
    # of the non-responsive ones. Rebuilding the responsive part from the
    # session rows would round each session's slot power to 2 decimals first,
    # which is how a setpoint met exactly used to come back a cent above.
    profile_cols <- intersect(window_profiles, names(profiles_demand))
    scheduled_demand <- tibble(datetime = dttm_seq) %>%
      left_join(scheduled_demand, by = "datetime")
    unmanaged_demand <- if (nrow(non_responsive_sessions) > 0) {
      get_demand(non_responsive_sessions, dttm_seq = dttm_seq, by = "Profile")
    } else {
      tibble(datetime = dttm_seq)
    }
    for (profile in profile_cols) {
      scheduled <- scheduled_demand[[profile]]
      unmanaged <- unmanaged_demand[[profile]]
      if (is.null(scheduled)) scheduled <- 0
      if (is.null(unmanaged)) unmanaged <- 0
      scheduled[is.na(scheduled)] <- 0
      unmanaged[is.na(unmanaged)] <- 0
      profiles_demand[[profile]] <- scheduled + unmanaged
    }
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


smart_charging_window_parallel <- function(
  windows_data,
  setpoints_lst,
  method,
  power_th,
  charging_power_min,
  energy_min,
  energy_max,
  include_log
) {
  if (
    !requireNamespace("mirai", quietly = TRUE) ||
      !requireNamespace("carrier", quietly = TRUE)
  ) {
    scheduling_lst <- purrr::map2(
      windows_data,
      setpoints_lst,
      \(x, y) {
        smart_charging_window(
          sessions_window = x$sessions_window,
          profiles_demand = x$profiles_demand,
          setpoints = y,
          method = method,
          power_th = power_th,
          charging_power_min = charging_power_min,
          energy_min = energy_min,
          energy_max = energy_max,
          include_log = include_log
        )
      }
    )
  } else {
    scheduling_lst <- purrr::map2(
      windows_data,
      setpoints_lst,
      in_parallel(
        \(x, y) {
          smart_charging_window(
            sessions_window = x$sessions_window,
            profiles_demand = x$profiles_demand,
            setpoints = y,
            method = method,
            power_th = power_th,
            charging_power_min = charging_power_min,
            energy_min = energy_min,
            energy_max = energy_max,
            include_log = include_log
          )
        },
        smart_charging_window = smart_charging_window,
        method = method,
        power_th = power_th,
        charging_power_min = charging_power_min,
        energy_min = energy_min,
        energy_max = energy_max,
        include_log = include_log
      )
    )
  }
  return(scheduling_lst)
}


# Scheduler tolerances. Decisions are taken on exact arithmetic against these
# named thresholds; the returned rows are rounded once, at the end. Up to 1.7.x
# the requirement was rounded to 2 decimals before the `> 0` test and every
# session's power was rounded to 2 decimals on the way out, so a cap met
# exactly came back as 3.01 kW whenever the per-session roundings added up.
#
# A flexibility requirement below one watt is floating-point dust, not a
# request to curtail: acting on it would flip `Flexible`/`Exploited` in every
# slot and cut spurious segments.
SCHEDULE_FLEX_TOL_KW <- 0.001
# A session can only be curtailed if it can give up at least this much power,
# and counts as charged once this little energy is left. Both unchanged.
SCHEDULE_MIN_CURTAIL_KW <- 0.1
SCHEDULE_MIN_ENERGY_KWH <- 0.025
# Precision of the returned session rows (kW, kWh, hours). Four decimals keep
# the sum of curtailed powers on the setpoint instead of a cent above it.
SCHEDULE_OUTPUT_DIGITS <- 4L

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
#' A session is never curtailed, postponed or interrupted below this share of
#' the energy it requires, whatever the setpoint says.
#' @param energy_max numeric, maximum allowed ratio (between 0 and 1) of required energy.
#' Every session stops charging once it has received this share of the energy
#' it requires. Must be higher than 0 and at least `energy_min`.
#' @param include_log logical, whether to output the algorithm messages for every user profile and time-slot
#' @param show_progress logical, whether to output the progress bar in the console
#'
#' @return list of three elements: `sessions` (the schedule, one row per
#'   constant-power segment), `demand` (the scheduled power per time slot,
#'   one column per `Profile` when the sessions carry one, else `Demand`,
#'   summed from the exact per-slot powers) and `log`
#' @export
#'
#' @importFrom dplyr tibble %>% filter mutate arrange desc left_join select mutate_if
#' @importFrom rlang .data
#' @importFrom lubridate as_datetime tz
#' @importFrom evsim expand_sessions
#' @importFrom timefully get_time_resolution
#'
schedule_sessions <- function(
  sessions,
  setpoint,
  method,
  power_th = 0,
  charging_power_min = 0.5,
  energy_min = 1,
  energy_max = 1,
  include_log = FALSE,
  show_progress = TRUE
) {
  if (show_progress) {
    cli::cli_h1("Scheduling charging sessions")
  }

  log <- c()

  # Parameters check
  if (show_progress) {
    cli::cli_progress_step("Checking parameters")
  }

  if (is.null(sessions) | nrow(sessions) == 0) {
    stop("Error: `sessions` parameter is empty.")
  }
  sessions_basic_vars <- c(
    "Session",
    "ConnectionStartDateTime",
    "ConnectionHours",
    "Power",
    "Energy"
  )
  if (!all(sessions_basic_vars %in% colnames(sessions))) {
    stop(
      "Error: `sessions` does not contain all required variables
      (see Arguments description)"
    )
  }
  if (!all(c("datetime", "setpoint") %in% colnames(setpoint))) {
    stop(
      "Error: `setpoint` does not contain all required variables
      (see Arguments description)"
    )
  }
  if (!(method %in% c("postpone", "interrupt", "curtail"))) {
    stop("Error: `method` not valid (see Arguments description)")
  }
  check_energy_ratios(energy_min, energy_max)

  resolution <- get_time_resolution(setpoint$datetime, units = "mins")
  dttm_tz <- tz(setpoint$datetime)

  if (show_progress) {
    cli::cli_progress_step("Preparing sessions data set")
  }

  # `energy_max` caps every session at a share of the energy it asked for. The
  # scaled `Energy` is the target the scheduler charges towards, so the charging
  # hours, the flexibility hours and `EnergyRequired` (set by
  # `expand_sessions()` from `Energy`) all follow from this one place.
  # `energy_min` keeps meaning "share of the ORIGINAL requirement", hence the
  # floor below is expressed relative to the target.
  energy_min_target <- energy_min / energy_max
  sessions_sch <- sessions %>%
    filter(.data$ConnectionStartDateTime %in% setpoint$datetime) %>%
    mutate(
      Energy = .data$Energy * energy_max,
      ChargingHours = .data$Energy / .data$Power
    )

  if (method %in% c("postpone", "interrupt")) {
    sessions_sch <- sessions_sch %>%
      mutate(
        ConnectionHours = round(
          as.numeric(
            .data$ConnectionEndDateTime - .data$ConnectionStartDateTime,
            unit = "hours"
          ),
          2
        ),
        FlexibilityHours = .data$ConnectionHours - .data$ChargingHours
      )
  }

  if (nrow(sessions_sch) == 0) {
    message("Error: no `sessions` for `setpoint$datetime` period")
    return(NULL)
  }

  sessions_expanded <- sessions_sch %>%
    expand_sessions(resolution = resolution)
  sessions_expanded <- sessions_expanded %>%
    mutate(
      Power = 0,
      EnergyToCharge = .data$EnergyRequired,
      Flexible = NA,
      Exploited = NA
    ) %>%
    left_join(
      select(
        sessions_sch,
        any_of(c("Session", "ConnectionStartDateTime", "FlexibilityHours"))
      ),
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
      as_datetime(timeslot, tz = dttm_tz),
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
    #   - `EnergyCharged`: energy that has been already charged before this time slot.
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
        PossibleEnergyRest = .data$PowerNominal *
          pmax(.data$ConnectionHoursLeft - resolution / 60, 0)
      )

    # Flexibility definition --------------------------------------------------
    if (method == "postpone") {
      # * Postpone: the EV has not started charging yet, and the energy required
      #    can be charged during the rest of the connection time at the nominal charging power.
      sessions_timeslot <- sessions_timeslot %>%
        mutate(
          Flexible = ifelse(
            (.data$EnergyCharged == 0) &
              (.data$MinEnergyToCharge <= .data$PossibleEnergyRest),
            TRUE,
            FALSE
          )
        )
    } else if (method == "interrupt") {
      # * Interrupt: the charge is not completed yet, and the energy required
      #    can be charged during the rest of the connection time at the nominal charging power.
      sessions_timeslot <- sessions_timeslot %>%
        mutate(
          Flexible = ifelse(
            (.data$EnergyToCharge > SCHEDULE_MIN_ENERGY_KWH) &
              (.data$MinEnergyToCharge <= .data$PossibleEnergyRest),
            TRUE,
            FALSE
          )
        )
    } else if (method == "curtail") {
      # * Curtail: the minimum power that can be charged in this time slot is
      # lower than the nominal power. The minimum power is defined by:
      #  - The `charging_power_min` parameter
      #  - The minimum energy that must be charged in the time slot, defined by
      #      - The minimum energy that must be charged in total (considering `energy_min`)
      #      - The energy that can be charged at nominal power the rest of connection hours

      if (!is.numeric(charging_power_min)) {
        stop("`charging_power_min` should be numeric")
      }

      if (charging_power_min < 1) {
        charging_power_min_ratio <- charging_power_min
        charging_power_min_kW <- Inf
      } else {
        charging_power_min_ratio <- 1
        charging_power_min_kW <- charging_power_min
      }

      sessions_timeslot <- sessions_timeslot %>%
        mutate(
          MinEnergyTimeslot = pmax(
            .data$MinEnergyToCharge - .data$PossibleEnergyRest,
            0
          ),
          MinPowerTimeslot = pmax(
            .data$MinEnergyTimeslot / (resolution / 60),
            pmin(
              .data$PowerNominal * charging_power_min_ratio,
              charging_power_min_kW
            )
          ),
          Flexible = ifelse(
            (.data$EnergyToCharge > SCHEDULE_MIN_ENERGY_KWH) &
              (.data$PowerTimeslot - .data$MinPowerTimeslot >
                SCHEDULE_MIN_CURTAIL_KW),
            TRUE,
            FALSE
          )
        )
    }

    # Flexibility exploitation ------------------------------------------------

    # Set `Exploited` to `FALSE` by default
    sessions_timeslot$Exploited <- FALSE

    # Power demand in this time slot
    sessions_timeslot_power <- sum(
      sessions_timeslot$PowerTimeslot,
      na.rm = TRUE
    )

    # Setpoint of power for this time slot
    setpoint_power_timeslot <- setpoint$setpoint[
      setpoint$datetime == timeslot
    ]
    if (length(setpoint_power_timeslot) == 0) {
      setpoint_power_timeslot <- NA_real_
    }
    if (is.na(setpoint_power_timeslot)) {
      if (include_log) {
        log <- c(
          log,
          paste0(
            "! Missing setpoint at ",
            timeslot_dttm,
            "; 
            skipping this timeslot."
          )
        )
      }
      setpoint_power_timeslot <- sessions_timeslot_power
    }

    # Flexibility requirement, on exact arithmetic (see SCHEDULE_FLEX_TOL_KW)
    flex_req <- sessions_timeslot_power -
      setpoint_power_timeslot * (1 + power_th)

    # If demand should be reduced
    if (flex_req > SCHEDULE_FLEX_TOL_KW) {
      if (include_log) {
        log_message <- c(
          paste("\u2139 Flexibility requirement of", round(flex_req, 2), "kW"),
          paste(
            "\u2139",
            nrow(sessions_timeslot),
            "potentially flexible sessions"
          )
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
        shift_flex_available <- sum(sessions_timeslot$PowerTimeslot[
          sessions_timeslot$Flexible
        ])

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
              arrange(
                desc(.data$ConnectionStartDateTime),
                desc(.data$ConnectionHoursLeft)
              )
          } else {
            sessions_timeslot_shiftable <- sessions_timeslot_shiftable %>%
              arrange(
                desc(.data$EnergyCharged),
                desc(.data$ConnectionStartDateTime),
                desc(.data$ConnectionHoursLeft)
              )
          }

          # Get the index of sessions that can charge
          allowed_to_charge_idx <- rep(FALSE, nrow(sessions_timeslot_shiftable))
          for (s in seq_len(nrow(sessions_timeslot_shiftable))) {
            flex_req <- max(
              flex_req - sessions_timeslot_shiftable$PowerTimeslot[s],
              0
            )
            if (flex_req == 0) break
          }
          if ((s + 1) <= length(allowed_to_charge_idx)) {
            allowed_to_charge_idx[seq(
              s + 1,
              length(allowed_to_charge_idx)
            )] <- TRUE
          }

          # Update the `Exploited` variable to `TRUE` for sessions to shift
          sessions_timeslot$Exploited[
            sessions_timeslot$ID %in%
              sessions_timeslot_shiftable$ID[!allowed_to_charge_idx]
          ] <- TRUE
        } else {
          # All `Flexible` sessions are `Exploited` when the flexibility required
          # is higher than the flexibility available
          sessions_timeslot$Exploited[sessions_timeslot$Flexible] <- TRUE

          # Update flexibility requirement with power from all shiftable sessions
          flex_req <- flex_req - shift_flex_available

          if (include_log) {
            log_message <- paste0(
              "\u2716 Not enough flexibility available (",
              shift_flex_available,
              " kW)"
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

        sessions_timeslot <- sessions_timeslot %>%
          mutate(
            # Before we set that `Flexible` if `MaxPowerReduction` > 0.1,
            MaxPowerReduction = .data$PowerTimeslot - .data$MinPowerTimeslot
          )

        if (any(sessions_timeslot$Flexible)) {
          # All `Flexible` sessions are `Exploited` with `curtail`
          sessions_timeslot$Exploited[sessions_timeslot$Flexible] <- TRUE

          # Get the maximum power reduction from all `Flexible` sessions
          max_power_reduction <- sum(
            sessions_timeslot$MaxPowerReduction[sessions_timeslot$Flexible]
          )

          # Power reduction factor that would be required
          reduction_factor <- min(flex_req / max_power_reduction, 1)

          # Update the charging power of curtailed sessions
          sessions_timeslot$Power[sessions_timeslot$Exploited] <-
            sessions_timeslot$PowerTimeslot[sessions_timeslot$Exploited] -
            sessions_timeslot$MaxPowerReduction[sessions_timeslot$Exploited] *
              reduction_factor

          # Update flexibility requirement
          flex_provided <- max_power_reduction * reduction_factor
          flex_req <- flex_req - flex_provided
        } else {
          flex_provided <- 0
        }

        # Update the charging power of sessions that are NOT curtailed
        sessions_timeslot$Power[!sessions_timeslot$Exploited] <-
          sessions_timeslot$PowerTimeslot[!sessions_timeslot$Exploited]

        if (include_log) {
          if (flex_req > SCHEDULE_FLEX_TOL_KW) {
            log_message <- paste0(
              "\u2716 Not enough flexibility available (",
              round(flex_provided, 2),
              " kW)"
            )
            # message(log_message)
            log <- c(
              log,
              log_message
            )
          }
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

      # If there are more time slots afterwards, update `EnergyToCharge` and `FlexibilityHours`
      session_after_idx <- (sessions_expanded$Session ==
        sessions_timeslot$Session[s]) &
        (sessions_expanded$ID > sessions_timeslot$ID[s])

      if (length(session_after_idx) > 0) {
        # Update `EnergyToCharge`
        # We assume that the session is charging the whole timeslot, so:
        # `ChargingHours = resolution/60`
        session_energy <- sessions_timeslot$Power[s] * resolution / 60
        session_energy_to_charge <- sessions_timeslot$EnergyToCharge[s] -
          session_energy
        sessions_expanded$EnergyToCharge[
          session_after_idx
        ] <- session_energy_to_charge

        # Update `FlexibilityHours`
        if (method %in% c("postpone", "interrupt")) {
          session_flexibility_hours <- max(
            sessions_timeslot$FlexibilityHours[s] - resolution / 60,
            0
          )
          sessions_timeslot$FlexibilityHours[s] <- session_flexibility_hours
          sessions_expanded$FlexibilityHours[
            session_after_idx
          ] <- session_flexibility_hours
        }
      } else {
        session_energy_to_charge <- 0
        session_flexibility_hours <- 0
      }
    }

    # Log message ----------------------------------------------------------------

    if (include_log) {
      exploited_sessions <- sessions_timeslot %>% filter(.data$Exploited)

      for (s in seq_len(nrow(exploited_sessions))) {
        if (method %in% c("postpone", "interrupt")) {
          session_flexibility_hours <- exploited_sessions$FlexibilityHours[s]
          original_flexibility <- sessions_sch$FlexibilityHours[
            sessions$Session == exploited_sessions$Session[s]
          ]
          pct_flexibility_available <- round(
            session_flexibility_hours / original_flexibility * 100,
            1
          )
          log_message <- paste0(
            "| \u2714 Session ",
            exploited_sessions$Session[s],
            " shifted (",
            exploited_sessions$PowerTimeslot[s],
            " kW, ",
            pct_flexibility_available,
            "% of flexible time still available)"
          )
        } else {
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
    mutate(
      ConnectionStartDateTime = .data$Timeslot,
      ConnectionEndDateTime = .data$Timeslot + minutes(resolution),
      ChargingStartDateTime = .data$Timeslot,
      ChargingEndDateTime = .data$Timeslot + minutes(resolution),
      ConnectionHours = resolution / 60,
      ChargingHours = ifelse(.data$Power > 0, resolution / 60, 0),
      Energy = .data$Power * .data$ChargingHours
    ) %>%
    summarise_by_segment() %>%
    mutate_if(is.numeric, round, SCHEDULE_OUTPUT_DIGITS)

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

  # The scheduled demand per time slot (and per `Profile` when the sessions
  # carry one), summed from the exact per-slot powers the scheduler decided.
  # Rebuilding it from the session rows through `evsim::get_demand()` rounds
  # every session's slot power to 2 decimals first, and those roundings add up
  # to a cent above a setpoint that was met exactly.
  demand_scheduled <- sessions_expanded %>%
    left_join(
      sessions_sch %>% select(any_of(c("Session", "Profile"))) %>% distinct(),
      by = "Session"
    ) %>%
    mutate(datetime = .data$Timeslot)
  if (!("Profile" %in% names(demand_scheduled))) {
    demand_scheduled$Profile <- "Demand"
  }
  demand_scheduled <- demand_scheduled %>%
    group_by(.data$datetime, .data$Profile) %>%
    summarise(Power = sum(.data$Power), .groups = "drop") %>%
    tidyr::pivot_wider(
      names_from = "Profile",
      values_from = "Power",
      values_fill = 0
    ) %>%
    arrange(.data$datetime)

  return(
    list(
      sessions = sessions_sch_flex,
      demand = demand_scheduled,
      log = log
    )
  )
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

  cat(
    "Smart charging results as a list of 3 objects: charging sessions, user profiles setpoints and log messages.\n"
  )
  cat(
    "Simulation from",
    as.character(min(date(x$setpoints$datetime))),
    "to",
    as.character(max(date(x$setpoints$datetime))),
    "with a time resolution of",
    get_time_resolution(x$setpoints$datetime, units = "mins"),
    "minutes.\n"
  )
  cat(
    "For this time period, there were",
    n_windows,
    "smart charging windows, where:\n"
  )
  cat(
    "  -",
    n_considered,
    "sessions were considered (",
    round(n_considered / n_sessions * 100),
    "% of total data set).\n"
  )
  cat(
    "  -",
    n_responsive,
    "sessions were Responsive (",
    round(n_responsive / n_considered * 100),
    "% of considered sessions).\n"
  )
  cat(
    "  -",
    n_flexible,
    "sessions were Flexible (",
    round(n_flexible / n_responsive * 100),
    "% of responsive sessions).\n"
  )
  cat(
    "  -",
    n_exploited,
    "sessions were Exploited (",
    round(n_exploited / n_flexible * 100),
    "% of flexible sessions).\n"
  )
  if (length(x$log[[1]]) > 0) {
    cat("For more information see the log messages.\n")
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
#' @param ... extra arguments of function `timefully::plot_ts()` or other arguments
#' to pass to `dygraphs::dyOptions()`.
#'
#' @return dygraphs plot
#' @export
#'
#' @importFrom dplyr %>% mutate group_by summarise left_join select select_if
#' @importFrom evsim get_demand
#' @importFrom timefully plot_ts
#' @importFrom dygraphs dyStackedRibbonGroup dySeries
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
plot_smart_charging <- function(
  smart_charging,
  sessions = NULL,
  show_setpoint = TRUE,
  by = NULL,
  ...
) {
  opt_sessions <- smart_charging$sessions

  # Create setpoint time-series profile
  plot_df <- smart_charging$setpoints["datetime"]

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
            is.na(.data$Responsive),
            "Not considered",
            ifelse(
              is.na(.data$Flexible),
              "Not responsive",
              ifelse(
                is.na(.data$Exploited),
                "Not flexible",
                ifelse(
                  !.data$Exploited,
                  "Not exploited",
                  "Exploited"
                )
              )
            )
          )
        )

      ribbon_names_all <- c(
        "Not considered",
        "Not responsive",
        "Not flexible",
        "Not exploited",
        "Exploited"
      )
      ribbon_colors_all <- c(
        "#660066",
        "#003366",
        "#003300",
        "#663300",
        "#ff9900"
      )
    }

    if (by %in% colnames(select_if(opt_sessions, is.character))) {
      ev_demand_flex <- opt_sessions %>%
        get_demand(dttm_seq = plot_df$datetime, by = by)
      if (by == "FlexType") {
        flextypes_in_data <- which(
          ribbon_names_all %in% colnames(ev_demand_flex)
        )
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
      ev_demand_flex,
      by = "datetime"
    )

  if (!is.null(sessions)) {
    ev_demand_static <- sessions %>%
      mutate(Profile = "Original EVs") %>%
      get_demand(dttm_seq = plot_df$datetime, by = "Profile")
    plot_df <- plot_df %>%
      left_join(
        ev_demand_static,
        by = "datetime"
      )
  }

  # Make plot
  plot_dy <- plot_df %>%
    plot_ts(ylab = "Power (kW)", strokeWidth = 2, ...)

  if (show_setpoint) {
    plot_dy <- plot_dy %>%
      dySeries(
        "Setpoint",
        strokePattern = "dashed",
        color = "red",
        strokeWidth = 2
      )
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
      dySeries(
        "Original EVs",
        strokePattern = "dashed",
        color = "gray",
        strokeWidth = 2
      )
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
#' Launches an interactive Shiny app to explore smart charging logs by window.
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
#' view_smart_charging_logs(sc_results)
#' }
view_smart_charging_logs <- function(smart_charging) {
  log <- smart_charging$log

  if (length(log) == 0) {
    stop("Error: no log messages to visualise.")
  }

  # The viewer launches a Shiny gadget, so it must not run during
  # non-interactive workflows such as package checks or coverage jobs.
  if (!interactive()) {
    stop(
      "Error: `view_smart_charging_logs()` is only available in interactive sessions."
    )
  }

  # Check for required packages
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop(
      "The 'shiny' package is required to use this function. Please install it with:\ninstall.packages('shiny')",
      call. = FALSE
    )
  }

  ui <- shiny::fluidPage(
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::selectInput(
          "selected_window",
          "Select Window",
          choices = names(log)
        )
      ),
      shiny::mainPanel(
        shiny::verbatimTextOutput("log_output")
      )
    )
  )

  server <- function(input, output, session) {
    # Show logs based on window selection
    output$log_output <- shiny::renderText({
      shiny::req(input$selected_window)
      msgs <- log[[input$selected_window]]
      if (is.null(msgs) || length(msgs) == 0) {
        return("No logs for this window.")
      }
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
#'   sessions, opt_data,
#'   opt_objective = "grid", method = "curtail",
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
    pivot_longer(-"Session") %>%
    group_by(.data$name, .data$value) %>%
    count() %>%
    filter(!(.data$name %in% c("Exploited", "Flexible") & is.na(.data$value)))
  n_sessions <- sum(summaryS$n[summaryS$name == "Responsive"])
  n_considered <- sum(
    summaryS$n[
      summaryS$name == "Responsive" & !is.na(summaryS$value)
    ],
    na.rm = T
  )
  n_responsive <- sum(
    summaryS$n[
      summaryS$name == "Responsive" & summaryS$value == TRUE
    ],
    na.rm = T
  )
  n_flexible <- sum(
    summaryS$n[
      summaryS$name == "Flexible" & summaryS$value == TRUE
    ],
    na.rm = T
  )
  n_exploited <- sum(
    summaryS$n[
      summaryS$name == "Exploited" & summaryS$value == TRUE
    ],
    na.rm = T
  )

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
        pivot_longer(
          everything(),
          names_to = "subgroup",
          values_to = "n_sessions"
        )
    ) %>%
    list_rbind(names_to = "group") %>%
    group_by(.data$group) %>%
    mutate(pct = round(.data$n_sessions / sum(.data$n_sessions) * 100)) %>%
    ungroup() %>%
    filter(n_sessions > 0)
}
