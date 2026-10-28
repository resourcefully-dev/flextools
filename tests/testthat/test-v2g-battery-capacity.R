library(dplyr)

# V2G sessions with their own `BatteryCapacity` (kWh): a fleet of 70 kWh
# batteries leaving at 80%, so the arrival SoC is 0.8 - Energy / 70. Synthetic
# sessions at 15 minutes, one profile (`Fleet`).

capacity_dttm <- seq(
  as.POSIXct("2025-03-03 00:00", tz = "UTC"),
  by = "15 min",
  length.out = 32
)

capacity_session <- function(energy, soc, capacity = 70, hours = 8, id = "S1") {
  tibble(
    Session = id,
    Timecycle = "Workday",
    Profile = "Fleet",
    ConnectionStartDateTime = capacity_dttm[1],
    ConnectionHours = hours,
    Power = 11,
    Energy = energy,
    StateOfCharge = soc,
    BatteryCapacity = capacity
  )
}

test_that("the v2g reserve is (StateOfCharge - soc_min) * BatteryCapacity", {
  reserve_kwh <- function(sessions) {
    envelope <- get_flex_envelope(
      evsim::adapt_charging_features(sessions, time_resolution = 15),
      dttm_seq = capacity_dttm,
      profile = "Fleet",
      soc_min = 0.2
    )
    # kW over a 15-minute slot, back to kWh
    envelope$reserve[1] * 15 / 60
  }

  # A 10 kWh session arriving at 66%: (0.66 - 0.2) * 70 = 32.2 kWh
  expect_equal(reserve_kwh(capacity_session(10, 0.66)), 32.2)
  # A 40 kWh session arriving at 23%: (0.23 - 0.2) * 70 = 2.1 kWh
  expect_equal(reserve_kwh(capacity_session(40, 0.23)), 2.1)
  # Both connected at once add up
  expect_equal(
    reserve_kwh(bind_rows(
      capacity_session(10, 0.66, id = "S1"),
      capacity_session(40, 0.23, id = "S2")
    )),
    34.3
  )

  # Without `BatteryCapacity` the small session is a small battery leaving
  # full: 10.01 kWh / (1 - 0.66) = 29.4 kWh, so a reserve of 13.5 kWh.
  expect_equal(
    reserve_kwh(select(capacity_session(10, 0.66), -BatteryCapacity)),
    (0.66 - 0.2) * 10.01 / (1 - 0.66)
  )
})

test_that("a session with a BatteryCapacity leaves at StateOfCharge + Energy / BatteryCapacity", {
  session <- capacity_session(10, 0.66) %>%
    evsim::adapt_charging_features(time_resolution = 15) %>%
    mutate(Responsive = TRUE)
  # Discharge for the first 2 hours, then charge
  setpoint <- tibble(
    datetime = capacity_dttm,
    setpoint = c(rep(-11, 8), rep(11, 24))
  )
  results <- schedule_sessions_v2g(session, setpoint, soc_min = 0.2)
  energy <- results$sessions$Energy

  # It discharges well beyond what a 29 kWh battery leaving full would allow
  expect_lt(sum(pmin(energy, 0)), -13.5)
  # It never goes below its floor of the 70 kWh battery
  battery <- 0.66 * 70 + cumsum(energy)
  expect_true(all(battery >= 0.2 * 70 - 0.02))
  # And it leaves at 0.66 + 10.01 / 70, about 0.8, not full
  expect_equal(sum(energy), 10.01, tolerance = 0.02)
  expect_equal(battery[length(battery)] / 70, 0.66 + 10.01 / 70, tolerance = 1e-3)
})

test_that("a session with a BatteryCapacity below soc_min charges up to the floor first", {
  # Arrives at 10% of 70 kWh (7 kWh) with a floor at 20% (14 kWh): it has to
  # charge 7 kWh before it can discharge, whatever the setpoint asks. Without
  # `BatteryCapacity` it would be a 22.2 kWh battery needing only 2.2 kWh.
  session <- capacity_session(20, 0.1) %>%
    evsim::adapt_charging_features(time_resolution = 15) %>%
    mutate(Responsive = TRUE)
  setpoint <- tibble(
    datetime = capacity_dttm,
    setpoint = c(rep(-11, 8), rep(11, 24))
  )
  results <- schedule_sessions_v2g(session, setpoint, soc_min = 0.2)
  # One row per block of constant power, in time order
  sch <- results$sessions
  energy <- sch$Energy

  battery <- 0.1 * 70 + cumsum(energy)
  floor_reached <- which(battery >= 0.2 * 70 - 0.02)[1]
  # It charges, at nominal power first, until it reaches the floor
  expect_false(is.na(floor_reached))
  expect_true(all(energy[seq_len(floor_reached)] > 0))
  expect_equal(sch$Power[1], 11)
  expect_equal(battery[floor_reached], 0.2 * 70, tolerance = 1e-3)
  # During the discharge request it stops at the floor: it has nothing above
  # it to discharge, so it waits and charges the rest when the setpoint allows
  expect_true(all(energy >= 0))
  later <- energy > 0 & seq_along(energy) > floor_reached
  expect_true(all(sch$ChargingStartDateTime[later] >= capacity_dttm[9]))
  expect_equal(sum(energy), 20.02, tolerance = 0.02)
})

test_that("a BatteryCapacity matching the derived one keeps the v2g results", {
  # 22 kWh at 11 kW is exactly 2 hours, so the adapted energy is unchanged and
  # the derived battery is 22 / (1 - 0.5) = 44 kWh.
  sessions <- bind_rows(
    capacity_session(22, 0.5, capacity = 44, id = "S1"),
    capacity_session(22, 0.5, capacity = 44, hours = 6, id = "S2")
  )
  opt_data <- tibble(
    datetime = capacity_dttm,
    production = 0,
    static = c(rep(5, 8), rep(20, 8), rep(5, 16))
  )
  run <- function(sessions) {
    suppressMessages(smart_v2g(
      sessions,
      opt_data,
      opt_objective = "grid",
      window_days = 1,
      window_start_hour = 0,
      soc_min = 0.2
    ))
  }
  with_capacity <- run(sessions)
  without_capacity <- run(select(sessions, -BatteryCapacity))

  expect_equal(with_capacity$setpoints, without_capacity$setpoints)
  expect_equal(with_capacity$demand, without_capacity$demand)
  expect_equal(
    select(with_capacity$sessions, -BatteryCapacity),
    without_capacity$sessions
  )
})

test_that("smart_v2g stops on a BatteryCapacity that is invalid or too small", {
  opt_data <- tibble(datetime = capacity_dttm, production = 0, static = 5)
  run <- function(sessions) {
    suppressMessages(smart_v2g(
      sessions,
      opt_data,
      opt_objective = "grid",
      window_days = 1,
      window_start_hour = 0
    ))
  }

  # 40 kWh does not fit in 70 kWh above 50% (35 kWh)
  expect_error(
    run(bind_rows(
      capacity_session(10, 0.5, id = "fits"),
      capacity_session(40, 0.5, id = "too_big")
    )),
    "does not fit in their battery.*Sessions: too_big$"
  )
  # Exactly full fits
  expect_no_error(run(capacity_session(35, 0.5)))

  for (capacity in list(0, -70, NA_real_, Inf)) {
    expect_error(
      run(bind_rows(
        capacity_session(10, 0.5, id = "ok"),
        capacity_session(10, 0.5, capacity = capacity, id = "bad")
      )),
      "positive finite number.*Sessions: bad$",
      info = capacity
    )
  }
  expect_error(
    run(capacity_session(10, 0.5, capacity = "70")),
    "must be numeric"
  )
})
