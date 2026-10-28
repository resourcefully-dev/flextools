library(dplyr)

# Charging ahead (`soc_max`) and the battery degradation cost of V2G. One
# vehicle with a 70 kWh battery, plugged in from 08:00 to 21:00, arriving at
# 66% and leaving at 80% (9.8 kWh). The site exports PV above its export
# capacity from 10:00 to 15:00 and imports above its import capacity from 17:00
# to 21:00, right up to the departure.

ahead_dttm <- seq(
  as.POSIXct("2025-06-02 00:00", tz = "UTC"),
  by = "15 min",
  length.out = 96
)
ahead_hour <- as.numeric(format(ahead_dttm, "%H"))
ahead_pv <- ahead_hour >= 10 & ahead_hour < 15
ahead_peak <- ahead_hour >= 17 & ahead_hour < 21

ahead_opt_data <- tibble(
  datetime = ahead_dttm,
  production = ifelse(ahead_pv, 30, 0),
  static = ifelse(ahead_peak, 30, 5),
  import_capacity = 20,
  export_capacity = 10,
  price_imported = ifelse(ahead_peak, 0.4, 0.25),
  price_exported = ifelse(ahead_pv, 0.05, 0.1)
)

ahead_session <- tibble(
  Session = "S1",
  Timecycle = "Workday",
  Profile = "Fleet",
  ConnectionStartDateTime = ahead_dttm[1] + 8 * 3600,
  ConnectionHours = 13,
  Power = 11,
  Energy = 0.14 * 70,
  StateOfCharge = 0.66,
  BatteryCapacity = 70
)

run_ahead <- function(objective, ...) {
  suppressMessages(smart_v2g(
    ahead_session,
    ahead_opt_data,
    opt_objective = objective,
    window_days = 1,
    window_start_hour = 0,
    soc_min = 0.2,
    ...
  ))
}

# Energy in the battery at the end of each slot (kWh)
ahead_battery <- function(results) {
  0.66 * 70 + cumsum(results$demand$Fleet) * 15 / 60
}

test_that("with soc_max the v2g fleet charges ahead in the export and discharges in the peak", {
  for (objective in c("capacity", "cost")) {
    results <- run_ahead(objective, soc_max = 1)
    battery <- ahead_battery(results)
    fleet <- results$demand$Fleet

    # Above its 80% departure level during the export, up to 100%
    expect_gt(max(battery[ahead_pv]), 0.8 * 70 + 1, label = objective)
    expect_true(all(battery <= 70 + 0.02), info = objective)
    # Discharges in the import peak
    expect_lt(sum(fleet[ahead_peak]) * 15 / 60, -1, label = objective)
    # Never below the floor, and leaves at 80%: net energy = Energy
    expect_true(all(battery >= 0.2 * 70 - 0.02), info = objective)
    expect_equal(sum(fleet) * 15 / 60, 9.8, tolerance = 0.02, info = objective)
    expect_equal(battery[length(battery)], 0.8 * 70, tolerance = 0.02)
  }

  # With soc_max at its departure level it never holds more than it leaves
  # with, so it cannot discharge in a peak that lasts until it leaves. (The
  # departure level of the adapted session: evsim rounds 9.8 kWh at 11 kW to
  # 0.89 hours, 9.79 kWh.)
  departure_soc <- 0.66 + 9.79 / 70
  for (objective in c("capacity", "cost", "grid")) {
    results <- run_ahead(objective, soc_max = departure_soc)
    battery <- ahead_battery(results)
    expect_true(all(battery <= 0.8 * 70 + 0.02), info = objective)
    expect_gte(sum(results$demand$Fleet[ahead_peak]), -0.01)
  }
})

test_that("soc_max caps the departure level of the v2g sessions", {
  # Arriving at 66% with 9.8 kWh to charge it would leave at 80%: with
  # soc_max = 0.75 it charges only (0.75 - 0.66) * 70 = 6.3 kWh.
  expect_message(
    results <- smart_v2g(
      ahead_session,
      ahead_opt_data,
      opt_objective = "grid",
      window_days = 1,
      window_start_hour = 0,
      soc_min = 0.2,
      soc_max = 0.75
    ),
    "leave above `soc_max`.*Sessions: S1"
  )
  battery <- ahead_battery(results)
  expect_equal(sum(results$demand$Fleet) * 15 / 60, 6.3, tolerance = 0.02)
  expect_true(all(battery <= 0.75 * 70 + 0.02))

  # A session without BatteryCapacity leaves full: capped as well, on its
  # derived battery, 9.8 / (1 - 0.66) = 28.8 kWh
  results <- suppressMessages(smart_v2g(
    select(ahead_session, -BatteryCapacity),
    ahead_opt_data,
    opt_objective = "grid",
    window_days = 1,
    window_start_hour = 0,
    soc_min = 0.2,
    soc_max = 0.9
  ))
  expect_equal(
    sum(results$demand$Fleet) * 15 / 60,
    (0.9 - 0.66) * 9.8 / (1 - 0.66),
    tolerance = 0.02
  )

  # Arriving at or above soc_max leaves nothing to charge
  expect_error(run_ahead("grid", soc_max = 0.66), "at or above `soc_max`.*S1")
})

test_that("a session without BatteryCapacity has no headroom", {
  # Its battery is derived from its energy, so it leaves full
  envelope <- get_flex_envelope(
    evsim::adapt_charging_features(
      select(ahead_session, -BatteryCapacity),
      time_resolution = 15
    ),
    dttm_seq = ahead_dttm,
    profile = "Fleet",
    soc_min = 0.2,
    soc_max = 1
  )
  expect_true(all(envelope$headroom == 0))

  # With it: (1 - 0.66) * 70 - 9.8 = 14 kWh, 56 kW over a 15-minute slot,
  # from its arrival up to the slot before it leaves
  envelope <- get_flex_envelope(
    evsim::adapt_charging_features(ahead_session, time_resolution = 15),
    dttm_seq = ahead_dttm,
    profile = "Fleet",
    soc_min = 0.2,
    soc_max = 1
  )
  connected <- which(envelope$headroom > 0)
  expect_equal(unique(envelope$headroom[connected]), 56, tolerance = 0.1)
  expect_equal(ahead_dttm[min(connected)], ahead_session$ConnectionStartDateTime)
})

test_that("the v2g scheduler gives back the charge ahead before departure", {
  # A setpoint that always asks for more: the session charges up to soc_max
  # and still leaves at its departure level.
  session <- ahead_session %>%
    evsim::adapt_charging_features(time_resolution = 15) %>%
    mutate(Responsive = TRUE)
  setpoint <- tibble(datetime = ahead_dttm, setpoint = 100)
  for (soc_max in c(0.9, 1)) {
    results <- schedule_sessions_v2g(
      session,
      setpoint,
      soc_min = 0.2,
      soc_max = soc_max
    )
    energy <- results$sessions$Energy
    battery <- 0.66 * 70 + cumsum(energy)
    expect_equal(max(battery), soc_max * 70, tolerance = 0.02)
    expect_equal(battery[length(battery)], 0.8 * 70, tolerance = 0.02)
  }
})

# Plugged in all day at 50% of 70 kWh with 10 kWh to charge. Import costs
# 0.30 in the morning and evening and 0.10 midday; a discharge in the
# expensive slots saves 0.30 and its energy is charged back midday at
# 0.10 / 0.9: a spread of about 0.19 per kWh discharged.
degradation_discharged <- function(objective, degradation_cost) {
  dttm <- ahead_dttm
  hour <- as.numeric(format(dttm, "%H"))
  opt_data <- tibble(
    datetime = dttm,
    production = 0,
    static = 10,
    price_imported = ifelse(hour >= 11 & hour < 15, 0.1, 0.3),
    price_exported = 0
  )
  session <- tibble(
    Session = "S1",
    Timecycle = "Workday",
    Profile = "Fleet",
    ConnectionStartDateTime = dttm[1] + 7 * 3600,
    ConnectionHours = 13,
    Power = 11,
    Energy = 10,
    StateOfCharge = 0.5,
    BatteryCapacity = 70
  )
  results <- suppressMessages(smart_v2g(
    session,
    opt_data,
    opt_objective = objective,
    window_days = 1,
    window_start_hour = 0,
    soc_min = 0.2,
    discharge_eff = 0.9,
    degradation_cost = degradation_cost
  ))
  -sum(pmin(results$demand$Fleet, 0)) * 15 / 60
}

test_that("degradation_cost stops the v2g arbitrage the price spread does not pay", {
  # Below the spread the fleet discharges as much as without it, above it not
  # at all.
  expect_gt(degradation_discharged("cost", 0), 1)
  expect_equal(
    degradation_discharged("cost", 0.1),
    degradation_discharged("cost", 0),
    tolerance = 0.02
  )
  expect_equal(degradation_discharged("cost", 0.25), 0)
})

test_that("degradation_cost weighs on the cost side of a combined v2g objective", {
  # w = 0.1: the cost side (0.9 of the objective) makes the fleet discharge.
  # The degradation is counted on the same side, (1 - w) * degradation_cost,
  # so it trims the discharge where the squared net power gains little (at
  # 0.1, below the spread) and stops it above the spread (0.25), as under
  # "cost".
  no_degradation <- degradation_discharged(0.1, 0)
  below_spread <- degradation_discharged(0.1, 0.1)
  expect_gt(no_degradation, 1)
  expect_gt(below_spread, 0)
  expect_lt(below_spread, no_degradation - 1)
  expect_equal(degradation_discharged(0.1, 0.25), 0)
})

test_that("smart_v2g validates soc_max and degradation_cost", {
  for (soc_max in list(NULL, 0.2, 0.1, 1.1, NA_real_, c(0.8, 0.9), "1")) {
    expect_error(run_ahead("grid", soc_max = soc_max), "`soc_max`")
  }
  for (degradation_cost in list(-0.01, NA_real_, Inf, c(0, 1), "0.1")) {
    expect_error(
      run_ahead("cost", degradation_cost = degradation_cost),
      "`degradation_cost`"
    )
  }
})

test_that("soc_final sets the arrival State-of-Charge of the v2g sessions", {
  session <- select(ahead_session, -StateOfCharge)
  run_final <- function(soc_final, soc_max = 1, sessions = session) {
    smart_v2g(
      sessions,
      ahead_opt_data,
      opt_objective = "grid",
      window_days = 1,
      window_start_hour = 0,
      soc_min = 0.2,
      soc_max = soc_max,
      soc_final = soc_final
    )
  }

  # Leaving at 80% with 9.8 kWh: arrives at 0.8 - 9.8 / 70 = 66%
  results <- suppressMessages(run_final(0.8))
  expect_equal(unique(results$sessions$StateOfCharge), 0.66)
  battery <- 0.66 * 70 + cumsum(results$demand$Fleet) * 15 / 60
  expect_equal(battery[length(battery)], 0.8 * 70, tolerance = 0.02)

  # Above soc_max it is lowered to soc_max: arrives at 0.75 - 0.14 = 61%,
  # still with its 9.8 kWh
  expect_message(
    results <- run_final(0.8, soc_max = 0.75),
    "`soc_final` \\(0.8\\) is above `soc_max`"
  )
  expect_equal(unique(results$sessions$StateOfCharge), 0.61)
  expect_equal(sum(results$demand$Fleet) * 15 / 60, 9.8, tolerance = 0.02)

  # Below soc_min, invalid, or not fitting below it: errors
  expect_error(run_final(0.1), "`soc_final` cannot be below `soc_min`")
  for (soc_final in list(1.1, NA_real_, c(0.8, 0.9), "0.8")) {
    expect_error(run_final(soc_final), "`soc_final` must be")
  }
  expect_error(
    run_final(0.5, sessions = bind_rows(
      mutate(session, Session = "fits"),
      mutate(session, Session = "too_big", Energy = 40)
    )),
    "does not fit below `soc_final`.*Sessions: too_big$"
  )
  # It needs BatteryCapacity, and replaces the StateOfCharge column
  expect_error(
    run_final(0.8, sessions = select(session, -BatteryCapacity)),
    "needs a `BatteryCapacity` column"
  )
  expect_error(run_final(0.8, sessions = ahead_session), "not both")
})
