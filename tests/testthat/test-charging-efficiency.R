library(dplyr)

# The per-session charging efficiency (`Efficiency` column): `Energy` is what
# reaches the battery, `Power` is drawn from the grid, and at `Power` the
# battery gains `Power * Efficiency` (V2G: a discharge of `Power` to the grid
# takes `Power / Efficiency` from the battery).

eff_dttm <- seq(
  as.POSIXct("2025-06-02 00:00", tz = "UTC"),
  by = "15 min",
  length.out = 96
)

# 18 kWh to the battery at 10 kW, plugged in from 08:00 to 18:00
eff_car <- tibble(
  Session = "S1",
  Timecycle = "Workday",
  Profile = "Fleet",
  ConnectionStartDateTime = eff_dttm[1] + 8 * 3600,
  ConnectionHours = 10,
  Power = 10,
  Energy = 18
)

# An import capacity of 5 kW: the car is curtailed to half its power
eff_capped <- tibble(
  datetime = eff_dttm,
  production = 0,
  static = 0,
  import_capacity = 5
)

run_eff <- function(sessions, method, opt_data = eff_capped,
                    opt_objective = "none", ...) {
  suppressMessages(smart_charging(
    sessions,
    opt_data,
    opt_objective = opt_objective,
    method = method,
    window_days = 1,
    window_start_hour = 0,
    ...
  ))
}

# kWh drawn from the grid by a profile of the demand
grid_kwh <- function(results, profile = "Fleet") {
  sum(results$demand[[profile]]) * 15 / 60
}

loss_cols <- c("Efficiency", "ChargingLosses", "DischargeLosses")

test_that("a curtailed V1G session at 0.9 draws Energy / 0.9 and delivers Energy", {
  results <- run_eff(mutate(eff_car, Efficiency = 0.9), "curtail")
  sessions <- results$sessions

  expect_equal(grid_kwh(results), 18 / 0.9)
  expect_equal(sum(sessions$Energy), 18)
  expect_equal(sum(sessions$ChargingLosses), 2)
  expect_equal(sum(sessions$DischargeLosses), 0)
  # Curtailed to the 5 kW capacity, for 20 / 5 hours instead of 18 / 5
  expect_equal(max(sessions$Power), 5)
  expect_equal(sum(sessions$ChargingHours), 4)
  # A row's grid-side energy is `Energy + ChargingLosses`
  expect_equal(
    sessions$Power * sessions$ChargingHours,
    sessions$Energy + sessions$ChargingLosses
  )
  expect_true(all(sessions$Efficiency == 0.9))
})

test_that("a postponed V1G session at 0.9 gives the same with a longer charge", {
  # 22.5 kWh: 2.25 h lossless and 2.5 h at 0.9, both whole slots
  car <- mutate(eff_car, Energy = 22.5)
  lossless <- run_eff(car, "postpone")
  results <- run_eff(mutate(car, Efficiency = 0.9), "postpone")
  sessions <- results$sessions

  expect_equal(grid_kwh(results), 22.5 / 0.9)
  expect_equal(sum(sessions$Energy), 22.5)
  expect_equal(sum(sessions$ChargingLosses), 2.5)
  # `Energy / (Power * Efficiency)` hours at full power: 2.5 h, not 2.25 h
  expect_equal(max(sessions$Power), 10)
  expect_equal(sum(sessions$ChargingHours), 2.5)
  expect_equal(sum(lossless$sessions$ChargingHours), 2.25)
  # Postponed to the end of the connection, starting earlier than lossless
  charging <- sessions[sessions$Power > 0, ]
  expect_equal(
    max(charging$ChargingStartDateTime + charging$ChargingHours * 3600),
    eff_car$ConnectionStartDateTime + 10 * 3600
  )
})

test_that("an uncontrolled V1G session at 0.9 charges Energy / (Power * 0.9) hours", {
  results <- run_eff(
    mutate(eff_car, Efficiency = 0.9),
    "none",
    opt_data = tibble(datetime = eff_dttm, production = 0),
    opt_objective = "grid"
  )
  expect_equal(results$sessions$ChargingHours, 2)
  expect_equal(results$sessions$Energy, 18)
  expect_equal(results$sessions$ChargingLosses, 2)
  expect_equal(grid_kwh(results), 20)
})

test_that("energy_min applies to the grid-side energy", {
  # No room at all: the session charges only its minimum share
  results <- run_eff(
    mutate(eff_car, Efficiency = 0.9),
    "curtail",
    opt_data = mutate(eff_capped, import_capacity = 0),
    energy_min = 0.5
  )
  expect_equal(grid_kwh(results), 0.5 * 18 / 0.9)
  expect_equal(sum(results$sessions$Energy), 0.5 * 18)
})

test_that("a fleet with mixed efficiencies draws each session's Energy / Efficiency", {
  fleet <- bind_rows(
    mutate(eff_car, Session = "A", Efficiency = 0.9),
    mutate(eff_car, Session = "B", Efficiency = 0.8, Energy = 12),
    mutate(eff_car, Session = "C", Efficiency = 1, Energy = 6)
  )
  for (method in c("curtail", "postpone", "interrupt")) {
    results <- run_eff(
      fleet,
      method,
      opt_data = mutate(eff_capped, import_capacity = 12)
    )
    per_session <- results$sessions %>%
      group_by(.data$Session) %>%
      summarise(
        Energy = sum(.data$Energy),
        Grid = sum(.data$Power * .data$ChargingHours),
        ChargingLosses = sum(.data$ChargingLosses)
      )
    expect_equal(per_session$Energy, c(18, 12, 6), tolerance = 1e-3)
    expect_equal(per_session$Grid, c(20, 15, 6), tolerance = 1e-3)
    expect_equal(per_session$ChargingLosses, c(2, 3, 0), tolerance = 1e-3)
    expect_equal(grid_kwh(results), 41, tolerance = 1e-3)
    expect_lte(max(results$demand$Fleet), 12 + 1e-6)
  }
})

test_that("schedule_sessions() takes the Efficiency column too", {
  sessions <- mutate(eff_car, Efficiency = 0.9) %>%
    evsim::adapt_charging_features(time_resolution = 15)
  setpoint <- tibble(datetime = eff_dttm, setpoint = 5)
  results <- schedule_sessions(
    sessions,
    setpoint,
    method = "curtail",
    show_progress = FALSE
  )
  expect_equal(sum(results$sessions$Energy), 18)
  expect_equal(sum(results$sessions$ChargingLosses), 2)
  expect_equal(sum(results$demand$Fleet) * 15 / 60, 20)
})

test_that("Efficiency = 1 gives the same results as no Efficiency column", {
  fleet <- bind_rows(
    mutate(eff_car, Session = "A"),
    mutate(eff_car, Session = "B", Energy = 12, Power = 7.4)
  )
  for (method in c("none", "curtail", "postpone", "interrupt")) {
    lossless <- run_eff(fleet, method, opt_objective = "grid")
    unit <- run_eff(mutate(fleet, Efficiency = 1), method, opt_objective = "grid")
    expect_equal(select(unit$sessions, -any_of(loss_cols)), lossless$sessions)
    expect_equal(unit$demand, lossless$demand)
    expect_equal(unit$setpoints, lossless$setpoints)
    expect_true(all(unit$sessions$ChargingLosses == 0))
    expect_false(any(loss_cols %in% names(lossless$sessions)))
  }
})

test_that("an Efficiency of 0, above 1 or NA is an error naming the sessions", {
  fleet <- bind_rows(
    mutate(eff_car, Session = "ok", Efficiency = 0.9),
    mutate(eff_car, Session = "bad", Efficiency = 0.9)
  )
  for (value in list(0, 1.1, NA_real_, -0.5)) {
    sessions <- fleet
    sessions$Efficiency[2] <- value
    expect_error(
      run_eff(sessions, "curtail"),
      "`Efficiency` in `sessions` must be a fraction in \\(0, 1\\].*Sessions: bad$"
    )
    expect_error(
      suppressMessages(smart_v2g(
        mutate(sessions, BatteryCapacity = 60),
        eff_capped,
        opt_objective = "grid",
        window_days = 1,
        window_start_hour = 0
      )),
      "`Efficiency` in `sessions` must be a fraction in \\(0, 1\\].*Sessions: bad$"
    )
  }
  expect_error(
    run_eff(mutate(fleet, Efficiency = "0.9"), "curtail"),
    "`Efficiency` in `sessions` must be numeric"
  )
})


# V2G -------------------------------------------------------------------------

# 20 kWh battery at 50 %, 2 kWh to charge at 4 kW, plugged in 08:00 - 12:00
v2g_car <- tibble(
  Session = "S1",
  Timecycle = "Workday",
  Profile = "Fleet",
  ConnectionStartDateTime = eff_dttm[1] + 8 * 3600,
  ConnectionHours = 4,
  Power = 4,
  Energy = 2,
  StateOfCharge = 50,
  BatteryCapacity = 20
)

# The fleet's setpoint given directly: -4 kW from 08:00 to 08:15 (1 kWh to the
# grid), then up to 4 kW
v2g_setpoint <- tibble(
  datetime = eff_dttm,
  production = 0,
  Fleet = ifelse(eff_dttm == eff_dttm[1] + 8 * 3600, -4, 4)
)

run_v2g_eff <- function(sessions, opt_data = v2g_setpoint,
                        opt_objective = "none", ...) {
  suppressMessages(smart_v2g(
    sessions,
    opt_data,
    opt_objective = opt_objective,
    window_days = 1,
    window_start_hour = 0,
    soc_min = 20,
    ...
  ))
}

# Battery level (kWh) at the end of each row, from the arrival level and the
# battery-side `Energy` of the rows in time order
battery_path <- function(sessions, arrival) {
  sessions <- sessions[order(sessions$ChargingStartDateTime), ]
  arrival + cumsum(sessions$Energy)
}

test_that("a V2G session at 0.9 discharging 1 kWh loses 1/0.9 and charges back at 0.9", {
  results <- run_v2g_eff(mutate(v2g_car, Efficiency = 0.9))
  sessions <- results$sessions
  discharge <- sessions[sessions$Power < 0, ]
  charge <- sessions[sessions$Power > 0, ]

  # 1 kWh to the grid, 1.11 kWh from the battery
  expect_equal(sum(discharge$Power * discharge$ChargingHours), -1)
  expect_equal(sum(discharge$Energy), -1.11)
  expect_equal(sum(discharge$DischargeLosses), 0.11)
  expect_equal(sum(discharge$ChargingLosses), 0)
  # Charged back at 0.9: (2 + 1 / 0.9) / 0.9 kWh from the grid
  grid_charged <- sum(charge$Power * charge$ChargingHours)
  expect_equal(grid_charged, (2 + 1 / 0.9) / 0.9, tolerance = 0.01)
  expect_equal(sum(charge$Energy), grid_charged * 0.9, tolerance = 0.01)
  expect_equal(sum(charge$ChargingLosses), grid_charged * 0.1, tolerance = 0.01)
  # Leaves with its 2 kWh: from 10 kWh to 12 kWh
  expect_equal(sum(sessions$Energy), 2, tolerance = 0.02)
  # A row's grid-side energy is `Energy + ChargingLosses + DischargeLosses`
  expect_equal(
    sessions$Power * sessions$ChargingHours,
    sessions$Energy + sessions$ChargingLosses + sessions$DischargeLosses,
    tolerance = 0.011
  )
  expect_equal(unique(sessions$BatteryCapacity), 20)
  expect_equal(unique(sessions$StateOfCharge), 50)
})

test_that("a V2G session at 0.9 never goes below soc_min, losses included", {
  # Asked to discharge all morning, then to charge back from 10:00
  setpoint <- tibble(
    datetime = eff_dttm,
    production = 0,
    Fleet = ifelse(eff_dttm < eff_dttm[1] + 10 * 3600, -4, 4)
  )
  results <- run_v2g_eff(
    mutate(v2g_car, Efficiency = 0.9, ConnectionHours = 6),
    opt_data = setpoint
  )
  path <- battery_path(results$sessions, 10)
  # The floor is 20 % of 20 kWh: 6 kWh of battery to give, 5.4 kWh to the grid
  expect_gte(min(path), 4 - 0.02)
  expect_lt(min(path), 4 + 0.05)
  discharge <- results$sessions[results$sessions$Power < 0, ]
  expect_equal(
    -sum(discharge$Power * discharge$ChargingHours),
    6 * 0.9,
    tolerance = 0.02
  )
  expect_equal(path[length(path)], 12, tolerance = 0.02)
})

test_that("V2G cost setpoints and departures count the per-session efficiency", {
  hour <- as.numeric(format(eff_dttm, "%H"))
  evening <- tibble(
    datetime = eff_dttm,
    production = 0,
    static = 5,
    price_imported = ifelse(hour %in% 17:19, 0.6, 0.1),
    price_exported = ifelse(hour %in% 17:19, 0.5, 0)
  )
  fleet <- tibble(
    Session = c("A", "B"),
    Timecycle = "Workday",
    Profile = "Fleet",
    ConnectionStartDateTime = eff_dttm[1] + 8 * 3600,
    ConnectionHours = 13,
    Power = 11,
    Energy = 21,
    BatteryCapacity = 70,
    Efficiency = c(0.95, 0.85)
  )
  results <- run_v2g_eff(
    fleet,
    opt_data = evening,
    opt_objective = "cost",
    soc_final_min = 80
  )
  for (s in fleet$Session) {
    sessions <- results$sessions[results$sessions$Session == s, ]
    eff <- fleet$Efficiency[fleet$Session == s]
    # It discharges into the evening price, and leaves at 80 %
    expect_true(any(sessions$Power < 0))
    path <- battery_path(sessions, 35)
    expect_equal(path[length(path)], 56, tolerance = 0.1)
    expect_gte(min(path), 14 - 0.05)
    expect_lte(max(path), 70 + 0.05)
    # Its losses are its own
    discharge <- sessions[sessions$Power < 0, ]
    charge <- sessions[sessions$Power > 0, ]
    expect_equal(
      sum(discharge$DischargeLosses),
      -sum(discharge$Power * discharge$ChargingHours) * (1 / eff - 1),
      tolerance = 0.05
    )
    expect_equal(
      sum(charge$ChargingLosses),
      sum(charge$Power * charge$ChargingHours) * (1 - eff),
      tolerance = 0.05
    )
  }
})

test_that("the V2G fleet setpoint weighs the round trip of the connected sessions by Energy", {
  sessions <- tibble(
    ConnectionStartDateTime = eff_dttm[c(1, 3)],
    ConnectionEndDateTime = eff_dttm[c(5, 4)],
    Energy = c(3, 1),
    Efficiency = c(0.9, 0.8)
  )
  expect_equal(
    v2g_fleet_discharge_eff(sessions, eff_dttm[1:6]),
    c(0.81, 0.81, (3 * 0.81 + 0.64) / 4, 0.81, 1, 1)
  )
  expect_equal(v2g_fleet_discharge_eff(select(sessions, -"Efficiency"), eff_dttm, 0.9), 0.9)
})

test_that("discharge_eff and the Efficiency column together are an error", {
  expect_error(
    run_v2g_eff(mutate(v2g_car, Efficiency = 0.9), discharge_eff = 0.9),
    "`discharge_eff` is superseded by the `Efficiency` column"
  )
  # discharge_eff = 1 (the default) with the column is fine
  expect_no_error(run_v2g_eff(mutate(v2g_car, Efficiency = 0.9), discharge_eff = 1))
  # discharge_eff without the column still works
  legacy <- run_v2g_eff(v2g_car, discharge_eff = 0.81)
  expect_false(any(loss_cols %in% names(legacy$sessions)))
})

test_that("V2G at Efficiency = 1 gives the same results as no Efficiency column", {
  hour <- as.numeric(format(eff_dttm, "%H"))
  evening <- tibble(
    datetime = eff_dttm,
    production = 0,
    static = 5,
    price_imported = ifelse(hour %in% 17:19, 0.6, 0.1),
    price_exported = ifelse(hour %in% 17:19, 0.5, 0)
  )
  car <- mutate(v2g_car, ConnectionHours = 13, Power = 11, Energy = 21,
                StateOfCharge = NULL, BatteryCapacity = 70)
  for (objective in list("none", "grid", "cost", 0.5, "capacity")) {
    opt_data <- if (identical(objective, "none")) v2g_setpoint else evening
    lossless <- run_v2g_eff(car, opt_data = opt_data,
                            opt_objective = objective, soc_final_min = 80)
    unit <- run_v2g_eff(mutate(car, Efficiency = 1), opt_data = opt_data,
                        opt_objective = objective, soc_final_min = 80)
    expect_equal(select(unit$sessions, -any_of(loss_cols)), lossless$sessions)
    expect_equal(unit$demand, lossless$demand)
    expect_equal(unit$setpoints, lossless$setpoints)
  }
})
