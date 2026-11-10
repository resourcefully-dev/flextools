# Fixtures follow the "flextools spec" of the FLEX+ thermal model plan
# (Phase 2). No PV unless a test says otherwise, so the COP-0 rule and the
# fallback are tested without surplus to absorb.

tank_data <- function(
  days = 1,
  resolution = 60,
  heat_demand = 1,
  cop = 3,
  flexible = 0.33,
  static = 0,
  production = 0,
  cheap_hours = 0:5
) {
  n <- days * 24 * 60 / resolution
  datetime <- as.POSIXct("2024-01-01", tz = "UTC") +
    resolution * 60 * (seq_len(n) - 1)
  tibble(
    datetime = datetime,
    production = rep_len(production, n),
    static = rep_len(static, n),
    heat_demand = rep_len(heat_demand, n),
    cop = rep_len(cop, n),
    flexible = rep_len(flexible, n),
    price_imported = ifelse(lubridate::hour(datetime) %in% cheap_hours, 0.1, 0.3),
    price_exported = 0
  )
}


# Evening peak: 6 kW of heat for 3 hours against a 1 kW compressor at COP 3
# (3 kW of heat) and a 3 kWh tank. The peak lacks 3 * 3 - 3 = 6 kWh of heat.
peak_data <- function(...) {
  heat_demand <- rep(1, 24)
  heat_demand[18:20] <- 6
  tank_data(heat_demand = heat_demand, ...)
}


# A FLEX+ space-heating tank on a sunny day without heat demand: 50 L between
# 25 and 35 degrees C (0.58 kWh), a 4 kW compressor, 15-minute slots. PV peaks
# at 5.5 kW at noon over a 14-hour day under a 0.5 kW household load, and the
# COP swings between 3 (at 22:00) and 6 (at 10:00). `heat_demand` is the
# day's heat demand in kWh, spread evenly.
small_tank_day <- function(heat_demand = 0) {
  datetime <- seq(
    as.POSIXct("2025-05-28 00:00", tz = "Europe/Amsterdam"),
    by = "15 min",
    length.out = 96
  )
  h <- (seq_len(96) - 1) / 4
  tibble(
    datetime = datetime,
    production = 5.5 * pmax(0, sin(pi * (h - 5) / 14)),
    static = 0.5,
    heat_demand = heat_demand / 24,
    cop = 4.5 - 1.5 * cos(2 * pi * (h - 22) / 24),
    flexible = 0,
    import_capacity = 17,
    export_capacity = 17,
    price_imported = 0.25,
    price_exported = 0.05
  )
}

small_tank_args <- list(
  Pc = 4,
  SOCmin = 0,
  SOCmax = 100,
  SOCini = 100,
  loss = 0.5,
  on_failure = "error"
)

# Runs whose windows OSQP solves, with their results pinned in
# `golden-tank-schedules.rds` from flextools 1.18.1, before the level was
# normalised: the small-tank day with a larger tank or with demand, and the
# OSQP fixtures below (grid, combined and ramping objectives, hourly and
# 15-minute, with and without the soft floor and the backup heater).
tank_golden_cases <- function() {
  evening <- tank_data(static = c(rep(0.5, 17), rep(3, 4), rep(0.5, 3)))
  quarter <- tank_data(resolution = 15)
  cases <- list(
    small_day_3kwh = list(small_tank_day(), c(list("grid", Tcap = 3), small_tank_args)),
    small_day_demand = list(
      small_tank_day(heat_demand = 3),
      c(list("grid", Tcap = 0.58), small_tank_args)
    ),
    evening_grid_3 = list(evening, list("grid", Tcap = 3, Pc = 3, SOCini = 100)),
    evening_w05_3 = list(evening, list(0.5, Tcap = 3, Pc = 3, SOCini = 100)),
    evening_grid_30 = list(evening, list("grid", Tcap = 30, Pc = 1, SOCini = 0)),
    evening_w05_30 = list(evening, list(0.5, Tcap = 30, Pc = 1, SOCini = 0)),
    evening_ramp = list(evening, list("grid", Tcap = 3, Pc = 3, lambda = 0.5)),
    quarter_grid_loss = list(quarter, list("grid", Tcap = 3, Pc = 3, loss = 1)),
    quarter_w05_loss = list(quarter, list(0.5, Tcap = 3, Pc = 3, loss = 1)),
    peak_backup_grid = list(peak_data(), list("grid", Tcap = 3, Pc = 1, Pb = 3)),
    peak_backup_w05 = list(peak_data(), list(0.5, Tcap = 3, Pc = 1, Pb = 3)),
    peak_soft_grid = list(peak_data(), list("grid", Tcap = 3, Pc = 1)),
    peak_soft_w05 = list(peak_data(), list(0.5, Tcap = 3, Pc = 1)),
    peak_soft_backup_grid = list(peak_data(), list("grid", Tcap = 3, Pc = 1, Pb = 1))
  )
  cases
}

run_tank_case <- function(case) {
  suppressMessages(do.call(add_tank_optimization, c(list(case[[1]]), case[[2]])))
}


# Two winter days of a heat pump the size of a FLEX+ hospital's, 15-minute
# slots from 06:00: 1550 kW of heat at a COP of 2.2, then from 06:00 on the
# second day 1900 kW until 14:00 and 1000 kW at a COP of 2.8 after, next to
# a static load of 165 to 205 kW. `flexible` runs the compressor at its cap
# from the start of each hour until it has made the hour's heat.
hospital_tank_days <- function() {
  h <- rep(0:47, each = 4)
  heat_demand <- ifelse(h < 24, 1550, ifelse(h < 32, 1900, 1000))
  cop <- ifelse(h < 32, 2.2, 2.8)
  slot <- rep(0:3, 48)
  tibble(
    datetime = as.POSIXct("2025-01-04 06:00", tz = "Europe/Amsterdam") +
      900 * (seq_along(h) - 1),
    production = 0,
    static = 185 + 20 * sin(2 * pi * h / 24),
    heat_demand = heat_demand,
    cop = cop,
    flexible = round(pmin(pmax(4 * heat_demand / cop - 1000 * slot, 0), 1000), 2),
    import_capacity = 1553,
    export_capacity = 1553,
    price_imported = 0.25,
    price_exported = 0.05
  )
}

hospital_tank_args <- list(
  "grid",
  Tcap = 1312.1947222222223,
  Pc = 1000,
  Pb = 500,
  SOCmin = 0,
  SOCmax = 100,
  window_start_hour = 6,
  loss = 0.5,
  on_failure = "error"
)
