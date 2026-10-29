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
#' @param discharge_eff numeric, efficiency (0, 1] of a discharge, counted as
#'   the round trip grid -> battery -> grid. For every kWh a session discharges
#'   to the grid, its battery loses `1 / discharge_eff` kWh, which it charges
#'   back from the grid before it leaves (0.9: 1.11 kWh). The sessions'
#'   `Energy` and `Power` are grid-side, as metered at the charge point, so
#'   they already include the charging losses of a normal charge, and there is
#'   no `charge_eff`. The losses of charging back the discharged energy are not
#'   separate either: pass the round-trip efficiency (about 0.9 for 95% each
#'   way) to count them, which also makes the SoC floor slightly conservative,
#'   because the battery bookkeeping counts grid-side kWh. The scheduler sizes
#'   each discharge so the session still meets its energy deadline and its SoC
#'   floor after the losses, and the `"cost"` and combined setpoints plan a
#'   discharge only when the price spread covers them.
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
#' @param soc_min numeric, State-of-Charge floor of every V2G session, in % of
#'   its battery capacity (default 20, `0 <= soc_min < 100`). A session never
#'   discharges below it. A session that arrives below it (`StateOfCharge <
#'   soc_min`) charges up to the floor first, as fast as it needs to (at
#'   nominal power when necessary), and can neither be curtailed nor discharge
#'   before it reaches the floor. Up to version 1.16.0 it was a fraction
#'   (default 0.2).
#' @param soc_max numeric, State-of-Charge ceiling of every V2G session, in % of
#'   its battery capacity (default 100, `soc_min < soc_max <= 100`), and the
#'   level every session aims for at departure. With a `BatteryCapacity`
#'   column (and no `StateOfCharge` column) a session's `Energy` is what it
#'   charges to reach `soc_max`: it arrives at `soc_max - 100 * Energy /
#'   BatteryCapacity`, and a session whose energy does not fit below `soc_max`
#'   (arriving below 0 %) is an error. A session never holds more than it
#'   leaves with, so it never charges above `soc_max`. A session whose
#'   departure level would be above `soc_max` (given its `StateOfCharge`)
#'   charges only up to it (`Energy` capped at `(soc_max - StateOfCharge) /
#'   100 * BatteryCapacity`, with a message), and a session arriving at or
#'   above `soc_max` is an error.
#' @param soc_final_min numeric, minimum State-of-Charge every V2G session
#'   leaves with, in % of its battery capacity (default `soc_max`, `soc_min <=
#'   soc_final_min <= soc_max`). Every session aims for `soc_max` (all its
#'   `Energy`), and may leave lower, down to `soc_final_min`, only where the
#'   grid capacities cannot fit its full energy (see Details). A session
#'   arriving above it may leave at its arrival level. With the default no
#'   session leaves below `soc_max`. Up to version 1.17.1 `smart_v2g()` also
#'   took `energy_min` and `energy_max`, the share of the energy every session
#'   charges at least and at most: `soc_final_min` and `soc_max` replace them.
#' @param cycle_cost numeric, degradation cost of one full cycle of a V2G
#'   battery, in Euro per `BatteryCapacity` kWh discharged (NOT Euro/kWh), as
#'   in `add_battery_optimization()`. Default 0. Divided by the battery
#'   capacity to obtain the per-kWh penalty, so a battery costing `p` Euro/kWh
#'   and rated for `n` cycles has `cycle_cost = p * BatteryCapacity / n`. The
#'   setpoint plans each profile's fleet as a whole, so it divides by the mean
#'   battery capacity of the profile's responsive sessions (exact for a fleet
#'   of one battery size). The `"cost"` objective adds the per-kWh penalty to
#'   every kWh discharged to the grid, so the fleet discharges only when the
#'   price spread, net of the `discharge_eff` losses, pays more than it. A
#'   combined objective `w` counts `(1 - w)` of it, the cost side's share. The
#'   `"grid"` and `"capacity"` objectives, which carry no prices and discharge
#'   only to cut import, do not count it.
#'
#' @details
#' The `sessions` tibble can carry a `StateOfCharge` column, the State-of-Charge
#' of each vehicle at arrival in % (`0 <= StateOfCharge < 100`); when it is
#' absent every session arrives at 50. Up to version 1.16.0 it was a fraction.
#' The returned `sessions` carry it in % too.
#'
#' It can also carry a `BatteryCapacity` column, the battery size of each
#' vehicle in kWh (a positive finite number). Without a `StateOfCharge`
#' column, each session's `Energy` then takes it to `soc_max`: it arrives at
#' `soc_max - 100 * Energy / BatteryCapacity` (21 kWh on a 70 kWh battery
#' arrives at 70 % with `soc_max = 100`), and can discharge `(70 - soc_min) /
#' 100 * 70` kWh while it still has the time to charge it back. With a
#' `StateOfCharge` column too, the session arrives at it and leaves at most at
#' `StateOfCharge + 100 * Energy / BatteryCapacity`; it must fit in its
#' battery, `Energy <= BatteryCapacity * (100 - StateOfCharge) / 100`, or
#' `smart_v2g()` stops and names it. When `BatteryCapacity` is absent, each
#' session's `Energy` takes its battery from `StateOfCharge` to full, so its
#' battery capacity is `Energy / (1 - StateOfCharge / 100)` (twice the energy
#' at 50 %), and `soc_final_min` applies to that derived battery: a session
#' arriving at 50 % with `soc_final_min = 80` must charge at least 60 % of its
#' energy.
#'
#' Like `SOCmin` and `SOCmax` of `add_battery_optimization()`, `soc_min` and
#' `soc_max` are the floor and the ceiling of the batteries (in %), and
#' `soc_final_min` is the least each one leaves with, within the two.
#'
#' Each session charges at most its `Energy` and at least what takes it to
#' `soc_final_min`, `(soc_final_min - StateOfCharge) / 100 * BatteryCapacity`
#' (nothing when it arrives above it), net of discharge. The setpoint aims for the maximum of
#' every session and drops energy towards its minimum only where the grid
#' capacities cannot fit the maximum, under every objective. The scheduler
#' charges every session towards its maximum: it postpones charge and
#' discharges what the session can charge back first, and curtails or
#' discharges it below its maximum, never below its minimum, only as far as
#' the import capacity needs. So nothing is ever given back: a session leaves
#' at `StateOfCharge + 100 * Energy / BatteryCapacity` (`soc_max` without a
#' `StateOfCharge` column, full without a `BatteryCapacity` column), or lower
#' only when the grid capacities need it.
#'
#' The setpoint plans charge and discharge only while the responsive sessions
#' are connected, within their nominal power in both directions (and
#' `load_capacity`), and keeps the energy owed to every session by its
#' departure: the cumulative flexible load never drops below the minimum share
#' of the energy of the sessions that have left, minus what the connected
#' sessions can still discharge down to `soc_min`.
#'
#' The `opt_objective` shapes that plan as in `smart_charging()`:
#' * `"grid"` minimises the squared net power of the site. The fleet
#'   discharges only to cut import (or to meet a negative `import_capacity`),
#'   never into a slot where the site already exports.
#' * `"cost"` minimises the energy cost of the site, `sum(import *
#'   price_imported - export * price_exported)` (plus the turn-up and
#'   turn-down prices, as in `smart_charging()`). The fleet charges in the
#'   cheap slots and discharges in the expensive ones, also into export when
#'   the export price pays for the energy charged back later (an evening export
#'   price above the night's import price); it never discharges into export
#'   that earns nothing. The import and export capacities still bind.
#' * A number `w` between 0 and 1 blends the two: `w` weighs the squared net
#'   power and `1 - w` the cost, as in `smart_charging()`. `w = 1` is `"grid"`
#'   and `w = 0` is `"cost"`; in between the fleet discharges only to cut
#'   import, as under `"grid"`.
#' * `"capacity"` moves only the slice of the plan that a breach of the grid
#'   capacities needs (see `smart_charging()`), and leaves windows within them
#'   unchanged. The slice may discharge, only to cut import: to cut an import
#'   breach, or to make room, while the site imports, for the energy it then
#'   charges back in an export breach. The rest of the fleet is not
#'   discharged to flatten slots within the caps.
#'
#' Under `"cost"` and combined objectives the setpoint counts the discharge
#' losses (`discharge_eff`): every discharged kWh is charged back as
#' `1 / discharge_eff` kWh, so a discharge is planned only when the price
#' spread covers the losses. The `"grid"` and `"capacity"` setpoints, which
#' discharge only to cut import, do not count them; the scheduler charges the
#' losses back on top under every objective.
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
  discharge_eff = 1,
  include_log = FALSE,
  show_progress = FALSE,
  lambda = 0,
  soc_min = 20,
  soc_max = 100,
  soc_final_min = soc_max,
  cycle_cost = 0
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

  check_charging_power_min(charging_power_min)
  check_soc_min(soc_min, full = 100)
  check_soc_max(soc_max, soc_min, full = 100)
  check_soc_final_min(soc_final_min, soc_min, soc_max)
  check_cycle_cost(cycle_cost)
  sessions <- set_v2g_arrival(sessions, soc_max)
  sessions <- set_state_of_charge(sessions, full = 100)
  # The arguments and the `StateOfCharge` column are in %, as in
  # add_battery_optimization(); the V2G internals work with fractions. A
  # minimum final SoC at `soc_max` lowers no session (`NULL` internally, see
  # v2g_energy_min_ratio()).
  sessions$StateOfCharge <- sessions$StateOfCharge / 100
  soc_min <- soc_min / 100
  soc_final_min <- if (soc_final_min < soc_max) soc_final_min / 100
  soc_max <- soc_max / 100
  check_v2g_battery_fit(sessions)
  sessions <- cap_v2g_departure(sessions, soc_max)

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
    soc_min,
    discharge_eff,
    soc_final_min,
    cycle_cost
  )

  if (show_progress) {
    cli::cli_progress_step("Scheduling V2G sessions")
  }

  scheduling_lst <- smart_v2g_window_parallel(
    windows_data,
    setpoints_lst,
    power_th,
    charging_power_min,
    discharge_eff,
    include_log,
    soc_min,
    soc_final_min
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

  # Back to %, as given
  sessions_opt$StateOfCharge <- round(sessions_opt$StateOfCharge * 100, 8)

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
#' @param soc_min numeric, SoC floor of the sessions, as a fraction (the
#'   `soc_min` of [smart_v2g()] divided by 100).
#' @param discharge_eff numeric, discharge efficiency of the vehicles, counted
#'   by the cost and combined objectives. See [smart_v2g()].
#' @param soc_final_min numeric or `NULL`, minimum SoC the sessions leave
#'   with, as a fraction (`NULL`: all their energy). See [smart_v2g()]
#'   and `v2g_energy_min_ratio()`.
#' @param cycle_cost numeric, cost of one full battery cycle, counted by the
#'   cost and combined objectives per kWh discharged as `cycle_cost /
#'   BatteryCapacity`. See [smart_v2g()].
#'
#' @keywords internal
#'
get_setpoints_v2g <- function(
  sessions_window,
  opt_data,
  profiles_demand,
  opt_objective,
  lambda,
  soc_min = 0.2,
  discharge_eff = 1,
  soc_final_min = NULL,
  cycle_cost = 0
) {
  if (nrow(sessions_window) == 0) {
    return(sessions_window)
  }

  # As in optimize_demand(): the end weights of a combined objective are the
  # pure objectives (`w = 1` minimises the net power, `w = 0` the cost).
  if (is.numeric(opt_objective)) {
    if (opt_objective <= 0) {
      opt_objective <- "cost"
    } else if (opt_objective >= 1) {
      opt_objective <- "grid"
    }
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

        # Each session owes at least its own minimum share at departure (what
        # takes it to `soc_final_min`); the setpoint targets all the energy
        # and drops towards the fleet's minimum only where the grid
        # capacities cannot fit it. Both ratios are shares of the responsive
        # sessions' own demand `LF`; the non-responsive demand `L_fixed_prof`
        # is added back untouched below.
        sessions_soc <- set_state_of_charge(sessions_window_prof_flex)
        energy_min_session <- v2g_energy_min_ratio(sessions_soc, soc_final_min)
        energy_ratio <- c(
          min(
            sum(energy_min_session * sessions_soc$Energy) /
              max(sum(sessions_soc$Energy), 1e-9),
            1
          ),
          1
        )
        envelope <- get_flex_envelope(
          sessions_soc,
          dttm_seq = dttm_seq,
          profile = profile,
          load_capacity = opt_data$load_capacity,
          energy_min = energy_min_session,
          soc_min = soc_min
        )
        LFmax_prof <- envelope$LFmax[opt_idxs]
        cumsum_min <- (envelope$owed - envelope$reserve)[opt_idxs]
        # Wear per kWh discharged: the cost of a full cycle over the mean
        # battery of the profile's responsive sessions.
        discharge_cost <- 0
        if (cycle_cost > 0) {
          discharge_cost <- cycle_cost /
            mean(v2g_battery_capacity(
              sessions_soc$Energy,
              sessions_soc$StateOfCharge,
              sessions_soc[["BatteryCapacity"]]
            ))
        }
        # The objectives with a squared-net-power term (grid, combined and
        # capacity) discharge only to cut import (see v2g_discharge_floor()).
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
          # No discharge floor: the cost LP discharges into export only when
          # the export price pays for the energy charged back later (see
          # demand_cost_v2g_window()).
          O <- demand_cost_v2g_window(
            G = opt_data$production[opt_idxs],
            LF = LF[opt_idxs],
            LS = LS[opt_idxs],
            PI = opt_data$price_imported[opt_idxs],
            PE = opt_data$price_exported[opt_idxs],
            PTD = opt_data$price_turn_down[opt_idxs],
            PTU = opt_data$price_turn_up[opt_idxs],
            direction = "forward",
            time_horizon = NULL,
            LFmax = LFmax_prof,
            import_capacity = opt_data$import_capacity[opt_idxs],
            export_capacity = opt_data$export_capacity[opt_idxs],
            lambda = lambda,
            energy_ratio = energy_ratio,
            cumsum_min = cumsum_min,
            discharge_eff = discharge_eff,
            discharge_cost = discharge_cost
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
          O <- demand_combined_v2g_window(
            G = opt_data$production[opt_idxs],
            LF = LF[opt_idxs],
            LS = LS[opt_idxs],
            PI = opt_data$price_imported[opt_idxs],
            PE = opt_data$price_exported[opt_idxs],
            PTD = opt_data$price_turn_down[opt_idxs],
            PTU = opt_data$price_turn_up[opt_idxs],
            direction = "forward",
            time_horizon = NULL,
            LFmax = LFmax_prof,
            import_capacity = opt_data$import_capacity[opt_idxs],
            export_capacity = opt_data$export_capacity[opt_idxs],
            w = opt_objective,
            lambda = lambda,
            energy_ratio = energy_ratio,
            cumsum_min = cumsum_min,
            LFmin = discharge_floor,
            discharge_eff = discharge_eff,
            discharge_cost = discharge_cost
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
  soc_min = 0.2,
  discharge_eff = 1,
  soc_final_min = NULL,
  cycle_cost = 0
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
      soc_min = soc_min,
      discharge_eff = discharge_eff,
      soc_final_min = soc_final_min,
      cycle_cost = cycle_cost
    )
  )
}


smart_v2g_window <- function(
  sessions_window,
  profiles_demand,
  setpoints,
  opt_data = NULL,
  power_th = 0,
  charging_power_min = 0,
  discharge_eff = 1,
  include_log = FALSE,
  soc_min = 0.2,
  soc_final_min = NULL
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
  # The sessions give up energy (below their full target) only as far as the
  # import capacity needs it: `power_cap` is the most the flexible load may
  # draw with the rest of the site as the setpoint plans it. A setpoint below
  # what they can postpone or discharge, within that cap, comes from planning
  # the fleet as a whole, and there they keep their energy.
  if (!is.null(opt_data) && "import_capacity" %in% names(opt_data)) {
    other_profiles <- setpoints %>%
      dplyr::select(-"datetime", -any_of(window_profiles)) %>%
      rowSums()
    if (length(other_profiles) == 0) {
      other_profiles <- 0
    }
    setpoint_flex$power_cap <- opt_data$import_capacity - opt_data$static +
      opt_data$production - L_fixed_total - other_profiles
  }

  results_log <- character(0)
  if (nrow(sessions_window_flex) > 0) {
    results <- schedule_sessions_v2g(
      sessions = sessions_window_flex,
      setpoint = setpoint_flex,
      charging_power_min = charging_power_min,
      discharge_eff = discharge_eff,
      include_log = include_log,
      show_progress = FALSE,
      power_th = power_th,
      soc_min = soc_min,
      soc_final_min = soc_final_min
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
  discharge_eff,
  include_log,
  soc_min = 0.2,
  soc_final_min = NULL
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
          opt_data = x$opt_data,
          power_th = power_th,
          charging_power_min = charging_power_min,
          discharge_eff = discharge_eff,
          include_log = include_log,
          soc_min = soc_min,
          soc_final_min = soc_final_min
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
            opt_data = x$opt_data,
            power_th = power_th,
            charging_power_min = charging_power_min,
            discharge_eff = discharge_eff,
            include_log = include_log,
            soc_min = soc_min,
            soc_final_min = soc_final_min
          )
        },
        smart_v2g_window = smart_v2g_window,
        power_th = power_th,
        charging_power_min = charging_power_min,
        discharge_eff = discharge_eff,
        include_log = include_log,
        soc_min = soc_min,
        soc_final_min = soc_final_min
      )
    )
  }
  return(scheduling_lst)
}


#' Capacity optimisation with bidirectional flexibility (single window)
#'
#' The V2G counterpart of `demand_capacity_window()`. The slice that has to
#' move may include discharge: the fleet may discharge (within the connected
#' power `LFmax` and only to cut import, see `v2g_discharge_floor()`) to make
#' room for energy it charges back in an export breach, within the departure
#' deadlines and SoC floor of `cumsum_min`. The slice is then placed with
#' `demand_grid_v2g_window()` charge-only (`LFmin = 0`): the placement only
#' puts back the energy the slice took, and never discharges the rest of the
#' fleet to flatten slots within the caps.
#'
#' @param lambda numeric, penalty on change of the placed slice.
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
  time_slots <- length(LF)
  LFmax <- as.numeric(rep_len(LFmax, time_slots))
  discharge_floor <- v2g_discharge_floor(
    round(as.numeric(G), 2),
    round(as.numeric(LS), 2),
    import_capacity
  )

  capacity_window(
    G = G,
    LF = LF,
    LS = LS,
    direction = direction,
    time_horizon = time_horizon,
    LFmax = LFmax,
    import_capacity = import_capacity,
    export_capacity = export_capacity,
    place_slice = function(LF, time_horizon, LS, LFmax, import_capacity,
                           export_capacity, energy_ratio, cumsum_min) {
      demand_grid_v2g_window(
        G = G,
        LF = LF,
        LS = LS,
        direction = direction,
        time_horizon = time_horizon,
        LFmax = LFmax,
        import_capacity = import_capacity,
        export_capacity = export_capacity,
        lambda = lambda,
        energy_ratio = energy_ratio,
        cumsum_min = cumsum_min,
        # Charge-only: the placement puts back the slice's energy and never
        # discharges the rest of the fleet (which would reshape slots within
        # the caps). Any discharge the breach needs is already in the slice.
        LFmin = 0
      )
    },
    energy_ratio = energy_ratio,
    cumsum_min = cumsum_min,
    LFmin = pmin(pmax(-LFmax, discharge_floor), 0)
  )
}


#' Minimisation of net power with bidirectional flexibility (single window)
#'
#' @param energy_ratio numeric vector `c(min, max)`, share of the flexible
#'   load's energy the optimized load must carry (net of discharge). `c(1, 1)`
#'   (default) preserves the energy exactly; a range lets the optimizer drop
#'   energy - down to `min` - when the grid capacity cannot fit `max`.
#' @param LFmin numeric vector (or `NULL`, default), per-slot floor of the
#'   optimized load on top of `-LFmax`. `get_setpoints_v2g()` (for the grid
#'   and combined objectives) and `capacity_v2g_window()` pass
#'   `v2g_discharge_floor()`, so the fleet never discharges where that only
#'   adds export.
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


#' Minimisation of the energy cost with bidirectional flexibility (single
#' window)
#'
#' The V2G counterpart of `demand_cost_window()`: the optimized load `O` may be
#' negative (discharge), within the same bounds as `demand_grid_v2g_window()`
#' (`get_bounds_v2g()`), and the site's import `I` and export `E` are priced
#' at `PI` and `PE`. A binary grid mode keeps the site from importing and
#' exporting in the same slot (when `lambda = 0`; otherwise the problem is a
#' QP with the export price clipped to the import price, as in
#' `demand_cost_window()`).
#'
#' No discharge floor (`v2g_discharge_floor()`) applies by default: the fleet
#' discharges into export when the export price pays for the energy charged
#' back later, which is the economic case of V2G. A fourth block of variables,
#' the fleet's net discharge `D >= max(-O, 0)`, carries a tiny cost so that a
#' discharge that earns nothing (a tie, e.g. discharging into a flat export
#' price to charge in another exporting slot) is never chosen. It also carries
#' the `discharge_cost` of the batteries, so the fleet discharges only when
#' the price spread pays for it.
#'
#' @inheritParams demand_cost_window
#' @param lambda numeric, penalty on the change of the optimized load between
#'   slots, `lambda * sum((O_t - O_{t-1})^2)`, as in `demand_cost_window()`.
#' @param LFmin numeric vector or `NULL` (default), per-slot floor of the
#'   optimized load. See `demand_grid_v2g_window()`.
#' @param discharge_eff numeric, discharge efficiency (0, 1] of the vehicles.
#'   Below 1, every kWh discharged must be charged back as `1 / discharge_eff`
#'   kWh, as the scheduler counts it, so a discharge pays only when the price
#'   spread covers the losses (see `solve_optimization_window_v2g()`).
#' @param discharge_cost numeric, cost per kWh discharged to the grid
#'   (default 0), on top of the tie-break price.
#'
#' @return numeric vector, the optimized load
#' @keywords internal
#'
demand_cost_v2g_window <- function(
  G,
  LF,
  LS,
  PI,
  PE,
  PTD,
  PTU,
  direction,
  time_horizon,
  LFmax,
  import_capacity,
  export_capacity,
  lambda = 0,
  energy_ratio = c(1, 1),
  cumsum_min = NULL,
  LFmin = NULL,
  discharge_eff = 1,
  discharge_cost = 0
) {
  time_slots <- length(LF)
  PI <- rep_len(as.numeric(PI), time_slots)
  PE <- rep_len(as.numeric(PE), time_slots)
  PTD <- rep_len(as.numeric(PTD), time_slots)
  PTU <- rep_len(as.numeric(PTU), time_slots)

  # Unknown variable: X = [O, I, E, D]
  # As in demand_cost_window(), the quadratic term penalizes ramping, with a
  # tiny diagonal term that makes it positive definite for OSQP.
  P <- matrix(0, 4 * time_slots, 4 * time_slots)
  P[seq_len(time_slots), seq_len(time_slots)] <- 2 *
    lambda *
    (get_lambda_matrix(time_slots) + 1e-6 * diag(time_slots))
  # The tie-break price goes on the discharge and on the export: a discharge
  # that earns nothing is not made, and one that earns as much by cutting
  # import as by exporting cuts import.
  tiebreak <- v2g_discharge_tiebreak(c(PI, PE, PTD, PTU))
  q <- c(
    PTD - PTU,
    PI,
    -PE + tiebreak,
    rep(tiebreak + discharge_cost, time_slots)
  )

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
    LFmin,
    discharge_eff = discharge_eff
  )
}


#' Combined optimisation with bidirectional flexibility (single window)
#'
#' Blends `demand_grid_v2g_window()` and `demand_cost_v2g_window()` as
#' `demand_combined_window()` blends the one-way objectives: `w` weighs the
#' squared net power (normalised by the squared mean import price) and `1 - w`
#' the energy cost. `w = 1` minimises the net power and `w = 0` the cost;
#' `get_setpoints_v2g()` routes those end weights to the pure objectives. The
#' problem is a QP (export price clipped to the import price), and
#' `get_setpoints_v2g()` passes the discharge floor of the grid objective,
#' because the squared net-power term trades a shallow export for a deeper one
#' (see `v2g_discharge_floor()`).
#'
#' With a `discharge_cost` above 0, a fourth block of variables, the net
#' discharge `D` (as in `demand_cost_v2g_window()`), carries `(1 - w) *
#' discharge_cost`, the cost side's share.
#'
#' @inheritParams demand_cost_v2g_window
#' @param w numeric, optimization objective weight (`w = 1` minimizes net
#'   power while `w = 0` minimizes cost).
#'
#' @return numeric vector, the optimized load
#' @keywords internal
#'
demand_combined_v2g_window <- function(
  G,
  LF,
  LS,
  PI,
  PE,
  PTD,
  PTU,
  direction,
  time_horizon,
  LFmax,
  import_capacity,
  export_capacity,
  w,
  lambda = 0,
  energy_ratio = c(1, 1),
  cumsum_min = NULL,
  LFmin = NULL,
  discharge_eff = 1,
  discharge_cost = 0
) {
  time_slots <- length(LF)
  PI <- rep_len(as.numeric(PI), time_slots)
  PE <- rep_len(as.numeric(PE), time_slots)
  PTD <- rep_len(as.numeric(PTD), time_slots)
  PTU <- rep_len(as.numeric(PTU), time_slots)
  scale <- mean(PI)^2

  # Unknown variable: X = [O, I, E]
  # Grid term normalized by the squared mean import price, ramping penalty as
  # in demand_combined_window().
  P <- matrix(0, 3 * time_slots, 3 * time_slots)
  P[seq_len(time_slots), seq_len(time_slots)] <-
    2 * w * scale * diag(time_slots) +
    2 * lambda * get_lambda_matrix(time_slots)
  q <- c(
    (1 - w) * (PTD - PTU) - 2 * w * scale * (round(G, 2) - round(LS, 2)),
    (1 - w) * PI,
    -(1 - w) * PE
  )
  if (discharge_cost > 0) {
    # X = [O, I, E, D], with the net discharge `D` priced at the cost side's
    # share of the degradation.
    P <- rbind(
      cbind(P, matrix(0, 3 * time_slots, time_slots)),
      matrix(0, time_slots, 4 * time_slots)
    )
    q <- c(q, rep((1 - w) * discharge_cost, time_slots))
  }

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
    LFmin,
    discharge_eff = discharge_eff
  )
}


#' Cost of a unit of V2G discharge that breaks ties
#'
#' A linear cost objective is indifferent between many profiles: with a flat
#' export price, discharging in one exporting slot to charge in another earns
#' exactly nothing. `demand_cost_v2g_window()` charges the fleet's net
#' discharge this tiny price, 1e-4 of the largest absolute price (1e-4 when
#' every price is 0), so such round trips are never chosen, while a discharge
#' that earns more than that still is.
#'
#' @param prices numeric vector, the prices of the window.
#'
#' @return numeric scalar
#' @keywords internal
#'
v2g_discharge_tiebreak <- function(prices) {
  prices <- abs(prices[is.finite(prices)])
  largest <- max(c(prices, 0))
  1e-4 * if (largest > 0) largest else 1
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
  LFmin = NULL,
  discharge_eff = 1,
  discharge_slots = NULL
) {
  G <- round(G, 2)
  LF <- round(LF, 2)
  LS <- round(LS, 2)

  time_slots <- length(G)
  if (is.null(time_horizon)) {
    time_horizon <- time_slots
  }

  # `q` longer than the window: the cost and combined objectives, whose
  # unknown is X = [O, I, E] (and the net discharge `D`, see
  # demand_cost_v2g_window()) instead of the optimized load alone.
  has_grid_flows <- length(q) > time_slots

  # Discharge losses (cost and combined objectives): a kWh discharged to the
  # grid drains `1 / discharge_eff` kWh from the battery, which the vehicle
  # must charge back, as the scheduler counts it (schedule_sessions_v2g()).
  # Counting them with a discharge variable `D >= -O` would let the solver
  # waste energy (claim losses it does not have) whenever charging more pays,
  # e.g. to absorb a PV export under the combined objective. Two passes keep
  # the model convex and exact instead: the first plans without losses and
  # fixes the slots that discharge; the second weighs the optimized load of
  # those slots by `1 / discharge_eff` in the energy and departure rows, with
  # discharge only (and charge only elsewhere), so it may trim a discharge the
  # losses make not worth it. When the second pass is infeasible the first
  # plan stands.
  if (has_grid_flows && discharge_eff < 1 && is.null(discharge_slots)) {
    solve_pass <- function(discharge_eff, discharge_slots) {
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
        LFmin,
        discharge_eff = discharge_eff,
        discharge_slots = discharge_slots
      )
    }
    O_lossless <- solve_pass(1, NULL)
    discharging <- O_lossless < 0
    if (!any(discharging)) {
      return(O_lossless)
    }
    O_losses <- solve_pass(discharge_eff, discharging)
    if (is.null(O_losses)) {
      message_once(
        "\u26A0\uFE0F V2G optimisation warning: the discharge losses do not fit in some windows. Planning them without the losses; the scheduler still charges them back."
      )
      return(O_lossless)
    }
    return(O_losses)
  }

  if (has_grid_flows) {
    P <- optimization_normalize_quadratic(
      P,
      problem_name = "V2G optimisation"
    )
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
    # Rewarded is the energy delivered, as the energy row counts it: in the
    # second pass of the discharge losses a discharge drains `1 /
    # discharge_eff`, so a discharge and its recharge earn nothing.
    if (!is.null(discharge_slots)) {
      reward <- reward * ifelse(discharge_slots, 1 / discharge_eff, 1)
    }
    q[seq_len(time_slots)] <- q[seq_len(time_slots)] - reward
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

    if (!is.null(discharge_slots)) {
      # Second pass of the discharge losses (see above): delivered energy is
      # `O / discharge_eff` in the discharging slots and `O` elsewhere.
      weight <- ifelse(discharge_slots, 1 / discharge_eff, 1)
      bounds$Amat_cumsum <- sweep(bounds$Amat_cumsum, 2, weight, `*`)
      bounds$Amat_energy <- bounds$Amat_energy * matrix(weight, nrow = 1)
      bounds$ub_O[discharge_slots] <- pmin(bounds$ub_O[discharge_slots], 0)
      bounds$lb_O[!discharge_slots] <- pmax(bounds$lb_O[!discharge_slots], 0)
    }

    if (has_grid_flows) {
      return(solve_grid_flows_v2g(
        bounds,
        G,
        LS,
        P,
        q,
        energy_floor,
        energy_ceiling
      ))
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
    return(round(O$x[seq_len(time_slots)], 2))
  }
  if (!is.null(discharge_slots)) {
    # Second pass of the discharge losses: no fallbacks, the first pass stands.
    return(NULL)
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
        return(round(O$x[seq_len(time_slots)], 2))
      }
    }
  }

  # Energy first: when the range admits less than the full profile, relax the
  # caps only as far as the minimum-energy profile needs and deliver that
  # minimum. Then add as much of the range on top as fits with no more import
  # than that solution (the energy reward above): only an import breach needs
  # less energy, while an export breach needs more, so the minimum alone would
  # make it worse. A minimum of 0 is skipped (no profile to relax towards).
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
      O_min <- O$x[seq_len(time_slots)]
      O <- solve_with_caps(
        pmax(import_capacity, LS + O_min - G) + tol,
        pmax(export_capacity, G - LS - LF_min) + tol,
        energy_floor = min(energy_lb, sum(O_min)),
        energy_ceiling = energy_ub
      )
      if (demand_highs_is_optimal(O$result)) {
        O_min <- O$x[seq_len(time_slots)]
      }
      return(round(O_min, 2))
    }
  }

  # Capacity second: the original, unshifted profile.
  message_once(
    "\u26A0\uFE0F V2G optimisation warning: optimisation not feasible in some windows. Relaxing grid capacity to the original profile in the affected windows."
  )
  O <- relax_to(LF, LF_energy)
  if (demand_highs_is_optimal(O$result)) {
    return(round(O$x[seq_len(time_slots)], 2))
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


#' Solve a V2G window priced on its grid flows
#'
#' Builds the problem of `demand_cost_v2g_window()` and
#' `demand_combined_v2g_window()` on the bounds of `get_bounds_v2g()`. The
#' unknown is `X = [O, I, E]`, or `[O, I, E, D]` with the net discharge `D`:
#' the optimized load keeps the box, departure and energy rows of the grid
#' objective, and the site balance `O - I + E = G - LS` ties it to the import
#' and the export. Their upper bounds follow from the box of `O` (which already
#' carries the grid capacities, also negative ones), so the original-profile
#' fallbacks of `solve_optimization_window_v2g()` stay feasible by
#' construction: `I` and `E` are the profile's own net flows and
#' `D = max(-O, 0)`. Solved as in `demand_solve_window()`: a MILP with a binary
#' grid mode (no import and export in the same slot) when the objective is
#' linear, otherwise a QP with the export price clipped to the import price.
#'
#' @param bounds list, output of `get_bounds_v2g()`.
#' @param G,LS numeric vectors, production and static load.
#' @param P numeric matrix or `NULL`, normalised quadratic term.
#' @param q numeric vector, linear term.
#' @param energy_floor,energy_ceiling numeric, energy range of the window.
#'
#' @return list with `result` (its `status_message`) and `x`
#' @keywords internal
#'
solve_grid_flows_v2g <- function(
  bounds,
  G,
  LS,
  P,
  q,
  energy_floor,
  energy_ceiling
) {
  time_slots <- length(G)
  n_blocks <- length(q) %/% time_slots
  has_discharge <- n_blocks == 4
  identityMat <- diag(time_slots)
  zeroMat <- identityMat * 0
  row_block <- function(O, I = O * 0, E = O * 0, D = O * 0) {
    if (has_discharge) cbind(O, I, E, D) else cbind(O, I, E)
  }

  # The tightest grid flows the optimized-load box admits (its bounds already
  # carry the grid capacities).
  import_mode_ub <- round(pmax(bounds$ub_O + LS - G, 0), 2)
  export_mode_ub <- round(pmax(G - LS - bounds$lb_O, 0), 2)

  A <- rbind(
    row_block(identityMat),
    row_block(zeroMat, I = identityMat),
    row_block(zeroMat, E = identityMat),
    # Site balance: O - I + E = G - LS
    row_block(identityMat, I = -identityMat, E = identityMat),
    row_block(bounds$Amat_cumsum),
    row_block(bounds$Amat_energy)
  )
  lb <- c(
    bounds$lb_O,
    rep(0, time_slots),
    rep(0, time_slots),
    G - LS,
    # The departure deadlines never ask for more than the energy row.
    pmin(bounds$lb_cumsum, energy_floor),
    energy_floor
  )
  ub <- c(
    bounds$ub_O,
    import_mode_ub,
    export_mode_ub,
    G - LS,
    bounds$ub_cumsum,
    energy_ceiling
  )
  if (has_discharge) {
    # Net discharge: 0 <= D <= max(-lb_O, 0) and D + O >= 0.
    A <- rbind(
      A,
      row_block(zeroMat, D = identityMat),
      row_block(identityMat, D = identityMat)
    )
    lb <- c(lb, rep(0, time_slots), rep(0, time_slots))
    ub <- c(ub, round(pmax(-bounds$lb_O, 0), 2), rep(Inf, time_slots))
  }

  lb <- round(as.numeric(lb), 2)
  ub <- round(as.numeric(ub), 2)
  if (any(lb > ub)) {
    # An empty box (e.g. a negative import capacity the connected vehicles
    # cannot reach): infeasible, as OSQP reports for the grid objective, so
    # the caller moves on to its fallbacks without a HiGHS warning.
    return(list(result = list(status_message = "Infeasible"), x = NULL))
  }

  solver_data <- list(
    time_slots = time_slots,
    n_variables = n_blocks * time_slots,
    optimized_idx = seq_len(time_slots),
    import_idx = time_slots + seq_len(time_slots),
    export_idx = 2 * time_slots + seq_len(time_slots),
    grid_mode_idx = n_blocks * time_slots + seq_len(time_slots),
    A = A,
    P = P,
    q = q,
    has_grid_flows = TRUE
  )
  solve_window_problem <- demand_select_window_solver(solver_data)
  solve_window_problem(
    solver_data,
    list(
      lb = lb,
      ub = ub,
      import_mode_ub = import_mode_ub,
      export_mode_ub = export_mode_ub
    )
  )
}


#' Discharge floor of the V2G optimized load
#'
#' Under the objectives with a squared net-power term (`"grid"`, combined and
#' `"capacity"`), discharge only serves to cut import, or to meet a negative
#' import capacity: in a slot where the site already exports, a discharge only
#' adds export. The
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
#' The `"cost"` objective has no floor: there, discharging into export earns
#' the export price, and the fleet does it when that pays for the energy
#' charged back later (see `demand_cost_v2g_window()`).
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
  discharge_eff = 1,
  include_log = FALSE,
  show_progress = FALSE,
  soc_min = 0.2,
  soc_final_min = NULL
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
  if (!is.null(soc_final_min)) {
    check_soc_final_min(soc_final_min, soc_min, soc_max = 1)
  }
  if (
    !is.numeric(discharge_eff) ||
      length(discharge_eff) != 1 ||
      is.na(discharge_eff) ||
      discharge_eff <= 0 ||
      discharge_eff > 1
  ) {
    stop("`discharge_eff` should be a numeric value in (0, 1]")
  }

  # `Energy` is the net target the session charges towards (`EnergyRequired`,
  # set by `expand_sessions()`), so it stops charging once it has it net of
  # discharge (`EnergyToCharge` grows by the discharged energy grossed up by
  # `discharge_eff`). `EnergyMinTarget` is the share of it the session must
  # charge at least, to leave at `soc_final_min` (see v2g_energy_min_ratio()).
  sessions_sch <- sessions %>%
    set_state_of_charge() %>%
    dplyr::filter(.data$ConnectionStartDateTime %in% setpoint$datetime)
  sessions_sch$EnergyMinTarget <- v2g_energy_min_ratio(
    sessions_sch,
    soc_final_min
  )
  sessions_sch <- sessions_sch %>%
    dplyr::mutate(ChargingHours = .data$Energy / .data$Power)

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
          "StateOfCharge",
          "BatteryCapacity",
          "EnergyMinTarget"
        ))
      ),
      by = "Session"
    )
  # Without a `BatteryCapacity` column, the requirement takes the battery from
  # its arrival SoC to full.
  sessions_expanded$BatteryCapacity <- v2g_battery_capacity(
    sessions_expanded$EnergyRequired,
    sessions_expanded$StateOfCharge,
    sessions_expanded[["BatteryCapacity"]]
  )
  sessions_expanded <- sessions_expanded %>%
    mutate(
      Power = 0,
      EnergyToCharge = .data$EnergyRequired,
      Flexible = NA,
      Exploited = NA,
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
    #       the minimum energy (`EnergyMinTarget`).
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
          .data$EnergyRequired * .data$EnergyMinTarget - .data$EnergyCharged,
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
    #      - The energy that must be charged to reach the minimum energy at departure
    #      - The minimum State-of-Charge (`soc_min`) allowed
    #      - The energy that can be charged at nominal power the rest of connection hours

    # `MinPowerTimeslot` keeps the minimum energy (`MinEnergyToCharge`), and
    # `FullPowerTimeslot` the full one (`EnergyToCharge`): down to it the
    # session only postpones its charge or discharges what it can still charge
    # back, so it still leaves at its full target.
    min_power_timeslot <- function(energy_to_charge) {
      min_energy <- pmax(
        # Can be negative (discharging)
        # Difference between energy to reach the target and possible
        # energy that can be charged the rest of connection.
        # If it is negative, it means that we have time to charge later
        # so now we can discharge.
        energy_to_charge - sessions_timeslot$PossibleEnergyRest,
        # Difference between energy to reach min SOC and
        # energy in battery
        # If it is negative, it means that we are above min SOC
        # so now we can discharge
        sessions_timeslot$BatteryCapacity * soc_min -
          sessions_timeslot$EnergyInBattery
      )
      # Both slacks are battery-side: a discharge of `d` kWh to the grid
      # drains `d / discharge_eff` from the battery and adds as much to the
      # energy to charge, so the session can discharge only
      # `slack * discharge_eff` to the grid without missing its deadline or
      # going below its SoC floor.
      min_energy <- ifelse(min_energy < 0, min_energy * discharge_eff, min_energy)
      min_power <- pmax(
        # Can be negative (discharging)
        min_energy / (resolution / 60),
        -sessions_timeslot$PowerNominal # Maximum discharging power
      )
      ifelse(
        sessions_timeslot$ChargingPowerMin > 0 & min_power > 0,
        pmax(min_power, sessions_timeslot$ChargingPowerMin), # Charging
        min_power # Discharging or no min power defined
      )
    }
    sessions_timeslot$MinPowerTimeslot <- min_power_timeslot(
      sessions_timeslot$MinEnergyToCharge
    )
    sessions_timeslot$FullPowerTimeslot <- pmin(
      pmax(
        min_power_timeslot(sessions_timeslot$EnergyToCharge),
        sessions_timeslot$MinPowerTimeslot
      ),
      sessions_timeslot$PowerTimeslot
    )

    sessions_timeslot <- sessions_timeslot %>%
      mutate(
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

        # The setpoint is met in stages, each in proportion to what the
        # `Flexible` sessions can give in it, and the next one only for what
        # is left:
        #   1. postpone charge (down to 0, or to what the session must charge
        #      now to still reach its full target),
        #   2. curtail below the full target, down to the minimum energy,
        #   3. discharge what the session can still charge back to its full
        #      target,
        #   4. discharge below it, down to the minimum energy.
        # Stages 2 and 4 give up energy (`soc_max` towards `soc_final_min`),
        # so they go only as far as the import capacity needs (`power_cap`,
        # see smart_v2g_window()). A session never discharges into another
        # one that is still charging when postponing or curtailing suffices.
        flex_idx <- sessions_timeslot$Flexible
        full_floor <- pmin(
          pmax(sessions_timeslot$FullPowerTimeslot, 0),
          sessions_timeslot$PowerTimeslot
        )
        min_floor <- pmin(pmax(sessions_timeslot$MinPowerTimeslot, 0), full_floor)
        levels <- list(
          sessions_timeslot$PowerTimeslot,
          full_floor,
          min_floor,
          pmin(sessions_timeslot$FullPowerTimeslot, full_floor),
          sessions_timeslot$MinPowerTimeslot
        )
        levels[[5]] <- pmin(levels[[5]], levels[[3]], levels[[4]])
        # Without a `power_cap` (a setpoint given directly), as far as the
        # setpoint asks
        power_cap <- if ("power_cap" %in% names(setpoint)) {
          setpoint$power_cap[setpoint$datetime == timeslot]
        } else {
          -Inf
        }
        power <- levels[[1]]
        req <- flex_req
        for (k in seq_along(levels)[-1]) {
          stage_req <- req
          if (k %in% c(3, 5)) {
            # Below the full target only down to the import capacity
            stage_req <- min(req, round(sum(power) - power_cap, 2))
          }
          # The reduction from the current power to this stage's level
          reduction <- pmax(power - levels[[k]], 0)[flex_idx]
          if (stage_req <= 0 || sum(reduction) <= 0) {
            next
          }
          factor <- min(stage_req / sum(reduction), 1)
          # A stage given in full lands exactly on its level (no rounding
          # residue, which would split the session into more segments)
          if (factor > 1 - 1e-9) {
            factor <- 1
          }
          power[flex_idx] <- if (factor == 1) {
            pmin(power, levels[[k]])[flex_idx]
          } else {
            power[flex_idx] - reduction * factor
          }
          req <- req - factor * sum(reduction)
        }
        # Rounded so that no floating-point residue (which differs between
        # platforms) splits a session into more segments
        sessions_timeslot$Power[flex_idx] <- round(power[flex_idx], 8)

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
      any_of(setdiff(names(sessions_sch), "EnergyMinTarget")),
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
#' @param soc_min numeric, a single value with `0 <= soc_min < full`.
#' @param full numeric, a full battery: 1 for a fraction (the internal
#'   functions), 100 for % ([smart_v2g()]).
#'
#' @return `TRUE`, invisibly. Stops with a clear message otherwise.
#' @keywords internal
#'
check_soc_min <- function(soc_min, full = 1) {
  if (
    !is.numeric(soc_min) ||
      length(soc_min) != 1 ||
      !is.finite(soc_min) ||
      soc_min < 0 ||
      soc_min >= full
  ) {
    stop(
      "Error: `soc_min` must be a single number with 0 <= soc_min < ", full
    )
  }
  invisible(TRUE)
}


#' Validate the SoC ceiling of the V2G sessions
#'
#' @param soc_max numeric, a single value with `soc_min < soc_max <= full`.
#' @param soc_min numeric, the SoC floor.
#' @inheritParams check_soc_min
#'
#' @return `TRUE`, invisibly. Stops with a clear message otherwise.
#' @keywords internal
#'
check_soc_max <- function(soc_max, soc_min, full = 1) {
  if (
    !is.numeric(soc_max) ||
      length(soc_max) != 1 ||
      !is.finite(soc_max) ||
      soc_max <= soc_min ||
      soc_max > full
  ) {
    stop(
      "Error: `soc_max` must be a single number with soc_min < soc_max <= ",
      full
    )
  }
  invisible(TRUE)
}


#' Validate the cycle cost of the V2G batteries
#'
#' @param cycle_cost numeric, a single value `>= 0`.
#'
#' @return `TRUE`, invisibly. Stops with a clear message otherwise.
#' @keywords internal
#'
check_cycle_cost <- function(cycle_cost) {
  if (
    !is.numeric(cycle_cost) ||
      length(cycle_cost) != 1 ||
      !is.finite(cycle_cost) ||
      cycle_cost < 0
  ) {
    stop("Error: `cycle_cost` must be a single number >= 0")
  }
  invisible(TRUE)
}


#' Set and validate the arrival State-of-Charge of the V2G sessions
#'
#' Adds `StateOfCharge` at half a battery when the column is absent, and
#' checks that every value is within `0 <= StateOfCharge < full` (a full
#' battery has no energy to charge, so it gives no battery size). When
#' `sessions` has a `BatteryCapacity` column, it also checks that every value
#' is a positive finite number (kWh).
#'
#' @param sessions tibble of sessions
#' @inheritParams check_soc_min
#'
#' @return `sessions` with a `StateOfCharge` column
#' @keywords internal
#'
set_state_of_charge <- function(sessions, full = 1) {
  if (!("StateOfCharge" %in% names(sessions))) {
    sessions$StateOfCharge <- rep(0.5 * full, nrow(sessions))
  }
  soc <- sessions$StateOfCharge
  if (!is.numeric(soc) || any(!is.finite(soc)) || any(soc < 0 | soc >= full)) {
    stop(
      "Error: `StateOfCharge` in `sessions` must be within 0 <= StateOfCharge < ",
      full,
      if (full == 100) " (%)" else " (fraction)"
    )
  }
  if ("BatteryCapacity" %in% names(sessions)) {
    check_battery_capacity(sessions)
  }
  sessions
}


#' Validate the `BatteryCapacity` column of the V2G sessions
#'
#' @param sessions tibble of sessions with a `BatteryCapacity` column
#'
#' @return `TRUE`, invisibly. Stops naming the sessions whose capacity is not
#'   a positive finite number.
#' @keywords internal
#'
check_battery_capacity <- function(sessions) {
  capacity <- sessions$BatteryCapacity
  if (!is.numeric(capacity)) {
    stop("Error: `BatteryCapacity` in `sessions` must be numeric (kWh)")
  }
  bad <- !is.finite(capacity) | capacity <= 0
  if (any(bad)) {
    stop(
      "Error: `BatteryCapacity` in `sessions` must be a positive finite number (kWh). Sessions: ",
      paste(unique(sessions$Session[bad]), collapse = ", ")
    )
  }
  invisible(TRUE)
}


#' Validate the minimum final SoC of the V2G sessions
#'
#' @param soc_final_min numeric, a single value with `soc_min <= soc_final_min
#'   <= soc_max`.
#' @param soc_min,soc_max numeric, the SoC floor and ceiling.
#'
#' @return `TRUE`, invisibly. Stops with a clear message otherwise.
#' @keywords internal
#'
check_soc_final_min <- function(soc_final_min, soc_min, soc_max) {
  if (
    !is.numeric(soc_final_min) ||
      length(soc_final_min) != 1 ||
      !is.finite(soc_final_min) ||
      soc_final_min < soc_min ||
      soc_final_min > soc_max
  ) {
    stop(
      "Error: `soc_final_min` must be a single number with soc_min (",
      soc_min, ") <= soc_final_min <= soc_max (", soc_max, ")"
    )
  }
  invisible(TRUE)
}


#' Set the arrival State-of-Charge of the V2G sessions from `soc_max`
#'
#' A session's `Energy` is what it charges to reach `soc_max`: with a
#' `BatteryCapacity` column and no `StateOfCharge` column, each session arrives
#' at `soc_max - 100 * Energy / BatteryCapacity`, all in %. A session whose
#' energy does not fit below `soc_max` (it would arrive below 0) is an error.
#'
#' @param sessions tibble of sessions
#' @param soc_max numeric, the SoC ceiling in %
#'
#' @return `sessions`, with a `StateOfCharge` column when it has a
#'   `BatteryCapacity` column and no `StateOfCharge` column
#' @keywords internal
#'
set_v2g_arrival <- function(sessions, soc_max) {
  if (
    !("BatteryCapacity" %in% names(sessions)) ||
      "StateOfCharge" %in% names(sessions)
  ) {
    return(sessions)
  }
  check_battery_capacity(sessions)
  soc <- soc_max - 100 * sessions$Energy / sessions$BatteryCapacity
  too_big <- soc < -1e-9
  if (any(too_big)) {
    stop(
      "Error: the `Energy` of some sessions does not fit below `soc_max` ",
      "(`Energy > soc_max / 100 * BatteryCapacity`, arriving below 0 %). ",
      "Sessions: ",
      paste(unique(sessions$Session[too_big]), collapse = ", ")
    )
  }
  sessions$StateOfCharge <- pmax(soc, 0)
  sessions
}


#' Minimum share of the energy of each V2G session
#'
#' A session must leave at least at `soc_final_min`: it must charge at least
#' `(soc_final_min - StateOfCharge) * BatteryCapacity` (nothing when it
#' arrives above it), out of its `Energy`, which takes it to `soc_max`.
#' Without a `BatteryCapacity` column the battery is derived from the energy
#' (see `v2g_battery_capacity()`).
#'
#' @param sessions tibble of sessions, with `StateOfCharge` (fraction)
#' @param soc_final_min numeric or `NULL`, the minimum final SoC (fraction).
#'   `NULL`, as [smart_v2g()] passes it when `soc_final_min` is at `soc_max`,
#'   gives every session all its energy (no rounding of the adapted energy can
#'   then lower it).
#'
#' @return numeric vector, one share (between 0 and 1) per session
#' @keywords internal
#'
v2g_energy_min_ratio <- function(sessions, soc_final_min = NULL) {
  if (is.null(soc_final_min)) {
    return(rep(1, nrow(sessions)))
  }
  capacity <- v2g_battery_capacity(
    sessions$Energy,
    sessions$StateOfCharge,
    sessions[["BatteryCapacity"]]
  )
  energy_floor <- pmax(soc_final_min - sessions$StateOfCharge, 0) * capacity
  ratio <- ifelse(
    sessions$Energy > 0,
    energy_floor / pmax(sessions$Energy, 1e-9),
    1
  )
  pmin(ratio, 1)
}


#' Check that every V2G session fits in its battery
#'
#' With a `BatteryCapacity` column, a session can charge at most what its
#' battery holds above its arrival SoC: `Energy <= BatteryCapacity * (1 -
#' StateOfCharge)`. Run on the sessions as given to `smart_v2g()`, before
#' `evsim::adapt_charging_features()` rounds their energy.
#'
#' @param sessions tibble of sessions, with `StateOfCharge` (see
#'   `set_state_of_charge()`)
#' @param tol numeric, tolerance in kWh
#'
#' @return `TRUE`, invisibly. Stops naming the sessions that do not fit.
#' @keywords internal
#'
check_v2g_battery_fit <- function(sessions, tol = 1e-9) {
  if (!("BatteryCapacity" %in% names(sessions))) {
    return(invisible(TRUE))
  }
  room <- sessions$BatteryCapacity * (1 - sessions$StateOfCharge)
  too_big <- sessions$Energy > room + tol
  if (any(too_big)) {
    stop(
      "Error: the `Energy` of some sessions does not fit in their battery ",
      "(`Energy > BatteryCapacity * (100 - StateOfCharge) / 100`). Sessions: ",
      paste(unique(sessions$Session[too_big]), collapse = ", ")
    )
  }
  invisible(TRUE)
}


#' Cap the departure level of the V2G sessions at `soc_max`
#'
#' `soc_min` and `soc_max` are always the limits of the battery: a session
#' whose departure level, `StateOfCharge + Energy / BatteryCapacity`, would be
#' above `soc_max` charges only up to it, `(soc_max - StateOfCharge) *
#' BatteryCapacity`. A session without `BatteryCapacity` leaves full, so with
#' `soc_max < 1` it is capped too; its battery is then kept as the derived
#' one, `Energy / (1 - StateOfCharge)`, in the `BatteryCapacity` column. A
#' session arriving at or above `soc_max` has nothing to charge: an error.
#'
#' @param sessions tibble of sessions, with `StateOfCharge` (see
#'   `set_state_of_charge()`)
#' @param soc_max numeric, the SoC ceiling
#'
#' @return `sessions`, with `Energy` capped
#' @keywords internal
#'
cap_v2g_departure <- function(sessions, soc_max) {
  above <- sessions$StateOfCharge >= soc_max
  if (any(above)) {
    stop(
      "Error: some sessions arrive at or above `soc_max`, with nothing to charge. Sessions: ",
      paste(unique(sessions$Session[above]), collapse = ", ")
    )
  }
  capacity <- v2g_battery_capacity(
    sessions$Energy,
    sessions$StateOfCharge,
    sessions[["BatteryCapacity"]]
  )
  room <- (soc_max - sessions$StateOfCharge) * capacity
  capped <- sessions$Energy > room + 1e-9
  if (!any(capped)) {
    return(sessions)
  }
  message(
    "\u2139 ", sum(capped), " V2G session(s) would leave above `soc_max`: ",
    "they charge only up to it. Sessions: ",
    paste(unique(sessions$Session[capped]), collapse = ", ")
  )
  sessions$BatteryCapacity <- capacity
  sessions$Energy[capped] <- room[capped]
  sessions
}


#' Battery capacity of a V2G session
#'
#' The session's own `BatteryCapacity` when it has one. Otherwise its energy
#' takes the battery from its arrival State-of-Charge to full: `Energy / (1 -
#' StateOfCharge)` (twice the energy at 50%).
#'
#' @param energy numeric, energy the session requires (kWh)
#' @param state_of_charge numeric, arrival State-of-Charge (fraction, < 1)
#' @param battery_capacity numeric or `NULL`, the sessions' `BatteryCapacity`
#'   column (kWh), `NULL` when the sessions have none
#'
#' @return numeric, battery capacity (kWh)
#' @keywords internal
#'
v2g_battery_capacity <- function(
  energy,
  state_of_charge,
  battery_capacity = NULL
) {
  if (!is.null(battery_capacity)) {
    return(battery_capacity)
  }
  energy / (1 - state_of_charge)
}
