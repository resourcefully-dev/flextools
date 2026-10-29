library(dplyr)

# The SoC ceiling (`soc_max`) and the battery cycle cost of V2G, in % as
# smart_v2g() takes them. One vehicle with a 70 kWh battery, plugged in from
# 08:00 to 21:00, arriving at 66% and leaving at 80% (9.8 kWh). The site
# exports PV above its export capacity from 10:00 to 15:00 and imports above
# its import capacity from 17:00 to 21:00, right up to the departure.

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
  StateOfCharge = 66,
  BatteryCapacity = 70
)

run_ahead <- function(objective, ...) {
  suppressMessages(smart_v2g(
    ahead_session,
    ahead_opt_data,
    opt_objective = objective,
    window_days = 1,
    window_start_hour = 0,
    soc_min = 20,
    ...
  ))
}

# Energy in the battery at the end of each slot (kWh)
ahead_battery <- function(results) {
  0.66 * 70 + cumsum(results$demand$Fleet) * 15 / 60
}

test_that("a v2g session never holds more than it leaves with", {
  # Nothing is charged ahead and given back: the battery never goes above its
  # departure level (80%), so the session cannot discharge in a peak that
  # lasts until it leaves, under any objective and whatever `soc_max`.
  for (objective in c("capacity", "cost", "grid")) {
    for (soc_max in c(85, 100)) {
      results <- run_ahead(objective, soc_max = soc_max)
      battery <- ahead_battery(results)
      expect_true(all(battery <= 0.8 * 70 + 0.02), info = objective)
      expect_true(all(battery >= 0.2 * 70 - 0.02), info = objective)
      expect_gte(sum(results$demand$Fleet[ahead_peak]), -0.01)
      expect_equal(sum(results$demand$Fleet) * 15 / 60, 9.8, tolerance = 0.02)
    }
  }
})

test_that("soc_max caps the departure level of the v2g sessions", {
  # Arriving at 66% with 9.8 kWh to charge it would leave at 80%: with
  # soc_max = 75 it charges only (75 - 66) % of 70 kWh = 6.3 kWh.
  expect_message(
    results <- smart_v2g(
      ahead_session,
      ahead_opt_data,
      opt_objective = "grid",
      window_days = 1,
      window_start_hour = 0,
      soc_min = 20,
      soc_max = 75
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
    soc_min = 20,
    soc_max = 90
  ))
  expect_equal(
    sum(results$demand$Fleet) * 15 / 60,
    (0.9 - 0.66) * 9.8 / (1 - 0.66),
    tolerance = 0.02
  )

  # Arriving at or above soc_max leaves nothing to charge
  expect_error(run_ahead("grid", soc_max = 66), "at or above `soc_max`.*S1")
})

test_that("the v2g scheduler never charges above the departure level", {
  # A setpoint that always asks for more: the session charges its energy and
  # stops, whatever `soc_max` (internal: SoC as a fraction).
  session <- ahead_session %>%
    mutate(StateOfCharge = 0.66) %>%
    evsim::adapt_charging_features(time_resolution = 15) %>%
    mutate(Responsive = TRUE)
  setpoint <- tibble(datetime = ahead_dttm, setpoint = 100)
  results <- schedule_sessions_v2g(session, setpoint, soc_min = 0.2)
  battery <- 0.66 * 70 + cumsum(results$sessions$Energy)
  expect_equal(max(battery), 0.8 * 70, tolerance = 0.02)
  expect_equal(battery[length(battery)], 0.8 * 70, tolerance = 0.02)
})

# Plugged in all day at 50% of 70 kWh with 10 kWh to charge. Import costs
# 0.30 in the morning and evening and 0.10 midday; a discharge in the
# expensive slots saves 0.30 and its energy is charged back midday at
# 0.10 / 0.9: a spread of about 0.19 per kWh discharged, 13.2 per full
# cycle of the 70 kWh battery (`cycle_cost` is per cycle, as in
# add_battery_optimization()).
cycle_discharged <- function(objective, cycle_cost) {
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
    StateOfCharge = 50,
    BatteryCapacity = 70
  )
  results <- suppressMessages(smart_v2g(
    session,
    opt_data,
    opt_objective = objective,
    window_days = 1,
    window_start_hour = 0,
    soc_min = 20,
    discharge_eff = 0.9,
    cycle_cost = cycle_cost
  ))
  -sum(pmin(results$demand$Fleet, 0)) * 15 / 60
}

test_that("cycle_cost stops the v2g arbitrage the price spread does not pay", {
  # Below the spread (7 per cycle, 0.1 per kWh) the fleet discharges as much
  # as without it, above it (17.5 per cycle, 0.25 per kWh) not at all.
  expect_gt(cycle_discharged("cost", 0), 1)
  expect_equal(
    cycle_discharged("cost", 7),
    cycle_discharged("cost", 0),
    tolerance = 0.02
  )
  expect_equal(cycle_discharged("cost", 17.5), 0)
})

test_that("cycle_cost weighs on the cost side of a combined v2g objective", {
  # w = 0.1: the cost side (0.9 of the objective) makes the fleet discharge.
  # The cycle cost is counted on the same side, (1 - w) of it, so it trims the
  # discharge where the squared net power gains little (7, below the spread)
  # and stops it above the spread (17.5), as under "cost".
  no_cycle_cost <- cycle_discharged(0.1, 0)
  below_spread <- cycle_discharged(0.1, 7)
  expect_gt(no_cycle_cost, 1)
  expect_gt(below_spread, 0)
  expect_lt(below_spread, no_cycle_cost - 1)
  expect_equal(cycle_discharged(0.1, 17.5), 0)
})

test_that("smart_v2g validates soc_min, soc_max, soc_final_min and cycle_cost in %", {
  expect_error(
    smart_v2g(
      ahead_session,
      ahead_opt_data,
      opt_objective = "grid",
      window_days = 1,
      window_start_hour = 0,
      soc_min = 100
    ),
    "0 <= soc_min < 100"
  )
  for (soc_max in list(NULL, 20, 10, 101, NA_real_, c(80, 90), "100")) {
    expect_error(run_ahead("grid", soc_max = soc_max), "`soc_max`")
  }
  expect_error(run_ahead("grid", soc_max = 101), "soc_max <= 100")
  expect_no_error(run_ahead("grid", soc_max = 90))
  # soc_min <= soc_final_min <= soc_max, both ends included
  for (soc_final_min in list(10, 95, NA_real_, c(80, 90), "80", NULL)) {
    expect_error(
      run_ahead("grid", soc_max = 90, soc_final_min = soc_final_min),
      "`soc_final_min` must be a single number with soc_min \\(20\\) <= soc_final_min <= soc_max \\(90\\)"
    )
  }
  expect_no_error(run_ahead("grid", soc_max = 90, soc_final_min = 20))
  expect_no_error(run_ahead("grid", soc_max = 90, soc_final_min = 90))
  for (cycle_cost in list(-0.01, NA_real_, Inf, c(0, 1), "0.1")) {
    expect_error(
      run_ahead("cost", cycle_cost = cycle_cost),
      "`cycle_cost`"
    )
  }
  # The StateOfCharge column is in % too
  expect_error(
    smart_v2g(
      mutate(ahead_session, StateOfCharge = 100),
      ahead_opt_data,
      opt_objective = "grid",
      window_days = 1,
      window_start_hour = 0
    ),
    "0 <= StateOfCharge < 100 \\(%\\)"
  )
})

test_that("v2g capacity solves a degenerate window quickly", {
  # One window of a month-long V2G fleet (3 sessions) with an export breach
  # the fleet cannot remove. The squared-excess QP of 1.17.0 cycled on it in
  # HiGHS without end (then with the sessions charging ahead, which they no
  # longer do).
  w <- readRDS(test_path("v2g-capacity-degenerate-window.rds"))
  elapsed <- system.time(
    O <- suppressMessages(capacity_v2g_window(
      G = w$G,
      LF = w$LF,
      LS = w$LS,
      direction = "forward",
      time_horizon = NULL,
      LFmax = w$LFmax,
      import_capacity = w$import_capacity,
      export_capacity = w$export_capacity,
      cumsum_min = w$cumsum_min
    ))
  )[["elapsed"]]
  expect_lt(elapsed, 5)

  net <- w$LS + O - w$G
  net_uncontrolled <- w$LS + w$LF - w$G
  export_excess <- pmax(-net - w$export_capacity, 0)
  export_excess_uncontrolled <- pmax(-net_uncontrolled - w$export_capacity, 0)
  # No discharge while exporting, and never more export above the cap than
  # uncontrolled
  expect_false(any(O < -0.01 & net < -0.01))
  expect_true(all(export_excess <= export_excess_uncontrolled + 0.01))
  # Within the caps on import, the same energy, never ahead of the
  # uncontrolled profile and within the departure deadlines
  expect_true(all(net <= w$import_capacity + 0.01))
  expect_equal(sum(O), sum(w$LF), tolerance = 0.01)
  expect_true(all(cumsum(O) <= cumsum(w$LF) + 0.02))
  expect_true(all(cumsum(O) >= pmin(w$cumsum_min, cumsum(w$LF)) - 0.02))
})
