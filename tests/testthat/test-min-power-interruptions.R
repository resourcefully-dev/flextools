library(dplyr)

# Stopping sessions under a minimum charging power: `allow_interruptions` for
# `method = "curtail"`, and the fleet-level stop rule of V2G.

mp_start <- as.POSIXct("2026-01-05 08:00", tz = "UTC")
mp_dttm <- seq(mp_start, by = "15 min", length.out = 32)

mp_session <- function(
  id,
  start = mp_start,
  hours = 8,
  power = 11,
  energy = 11
) {
  tibble(
    Session = id,
    Timecycle = "Monday",
    Profile = "Worktime",
    ConnectionStartDateTime = start,
    ConnectionEndDateTime = start + hours * 3600,
    ConnectionHours = hours,
    ChargingStartDateTime = start,
    ChargingEndDateTime = start + energy / power * 3600,
    ChargingHours = energy / power,
    Power = power,
    Energy = energy
  )
}

mp_setpoint <- function(low, low_slots = 16) {
  tibble(
    datetime = mp_dttm,
    setpoint = c(rep(low, low_slots), rep(50, length(mp_dttm) - low_slots))
  )
}

# Power of every session in every slot (kW), one column per session
mp_slot_powers <- function(sessions) {
  sessions %>%
    evsim::get_demand(dttm_seq = mp_dttm, by = "Session") %>%
    select(-"datetime")
}


# Curtail ------------------------------------------------------------------

test_that("curtail with interruptions stops a session the setpoint has no room for", {
  on <- schedule_sessions(
    mp_session("S1"),
    mp_setpoint(0),
    method = "curtail",
    charging_power_min = 4,
    allow_interruptions = TRUE,
    show_progress = FALSE
  )
  expect_equal(on$demand$Worktime[1:20], c(rep(0, 16), rep(11, 4)))
  expect_equal(sum(on$sessions$Energy), 11)

  # Without the switch the session keeps charging at the minimum, as before
  off <- schedule_sessions(
    mp_session("S1"),
    mp_setpoint(0),
    method = "curtail",
    charging_power_min = 4,
    show_progress = FALSE
  )
  expect_equal(off$demand$Worktime[1:12], c(rep(4, 11), 0))
})

test_that("curtail stops one session and keeps the other at or above the minimum", {
  # Both curtailed to 4 kW give 8 kW against 6 kW: 2 kW over, less than one
  # minimum. One session stops and the other takes the slot's 6 kW.
  two <- bind_rows(
    mp_session("A"),
    mp_session("B", start = mp_start + 15 * 60, hours = 7.75)
  )
  res <- schedule_sessions(
    two,
    mp_setpoint(6),
    method = "curtail",
    charging_power_min = 4,
    allow_interruptions = TRUE,
    show_progress = FALSE
  )
  powers <- mp_slot_powers(res$sessions)

  # From the slot both are connected until they are charged
  both <- 2:13
  expect_true(all(rowSums(powers[both, ]) <= 6 + 1e-6))
  expect_true(all(rowSums(powers[both, ] == 0) == 1))
  expect_true(all(apply(powers[both, ], 1, max) >= 4))
  # No slot power between 0 and the minimum, but the tail of a charge
  tails <- c(
    max(which(powers$A > 0)),
    max(which(powers$B > 0))
  )
  in_gap <- function(p) p > 1e-6 & p < 4 - 1e-6
  expect_false(any(in_gap(powers$A[-tails[1]])))
  expect_false(any(in_gap(powers$B[-tails[2]])))
  expect_equal(sum(res$sessions$Energy), 22)
})

test_that("curtail never stops a session that could not reach its minimum energy later", {
  # T is plugged in for exactly the hour its 11 kWh take at 11 kW: no slot of
  # it can be given up. F has hours left and stops instead.
  sessions <- bind_rows(
    mp_session("T", hours = 1),
    mp_session("F")
  )
  res <- schedule_sessions(
    sessions,
    mp_setpoint(0, low_slots = 4),
    method = "curtail",
    charging_power_min = 4,
    allow_interruptions = TRUE,
    show_progress = FALSE
  )
  powers <- mp_slot_powers(res$sessions)
  expect_equal(powers$T[1:4], rep(11, 4))
  expect_equal(powers$F[1:4], rep(0, 4))
  expect_equal(
    res$sessions %>% filter(Session == "T") %>% pull(Energy) %>% sum(),
    11
  )
})

test_that("with no minimum charging power the switch changes nothing", {
  sessions <- bind_rows(
    mp_session("A"),
    mp_session("B", start = mp_start + 15 * 60, hours = 2, energy = 15)
  )
  for (sp in c(0, 6)) {
    off <- schedule_sessions(
      sessions,
      mp_setpoint(sp),
      method = "curtail",
      charging_power_min = 0,
      include_log = TRUE,
      show_progress = FALSE
    )
    on <- schedule_sessions(
      sessions,
      mp_setpoint(sp),
      method = "curtail",
      charging_power_min = 0,
      allow_interruptions = TRUE,
      include_log = TRUE,
      show_progress = FALSE
    )
    expect_identical(on, off)
  }
})

test_that("the switch is ignored by the other methods", {
  for (method in c("postpone", "interrupt")) {
    off <- schedule_sessions(
      mp_session("S1"),
      mp_setpoint(2),
      method = method,
      charging_power_min = 4,
      show_progress = FALSE
    )
    on <- schedule_sessions(
      mp_session("S1"),
      mp_setpoint(2),
      method = method,
      charging_power_min = 4,
      allow_interruptions = TRUE,
      show_progress = FALSE
    )
    expect_identical(on, off)
  }
})

test_that("smart_charging passes the switch to the scheduler", {
  # The stop goes down to the import capacity: 0 kW for four hours
  opt_data <- tibble(
    datetime = mp_dttm,
    production = 0,
    import_capacity = c(rep(0, 16), rep(50, 16)),
    Worktime = c(rep(0, 16), rep(50, 16))
  )
  sc <- function(allow_interruptions) {
    smart_charging(
      mp_session("S1"),
      opt_data,
      opt_objective = "none",
      method = "curtail",
      window_days = 1,
      window_start_hour = 8,
      charging_power_min = 4,
      allow_interruptions = allow_interruptions
    )
  }
  expect_equal(sc(TRUE)$demand$Worktime[1:20], c(rep(0, 16), rep(11, 4)))
  expect_equal(sc(FALSE)$demand$Worktime[1:12], c(rep(4, 11), 0))
})

test_that("curtail never stops a session whose power is below the minimum", {
  # A 3.7 kW session against a 4 kW minimum is not flexible: it charges as
  # uncontrolled however far the setpoint is below. The 11 kW one stops.
  sessions <- bind_rows(
    mp_session("Low", power = 3.7, energy = 7.4),
    mp_session("F")
  )
  res <- schedule_sessions(
    sessions,
    mp_setpoint(0),
    method = "curtail",
    charging_power_min = 4,
    allow_interruptions = TRUE,
    show_progress = FALSE
  )
  powers <- mp_slot_powers(res$sessions)
  expect_equal(powers$Low[1:9], c(rep(3.7, 8), 0))
  expect_equal(powers$F[1:16], rep(0, 16))
})

test_that("sessions below the minimum charge as uncontrolled across a window boundary", {
  # Two 3.7 kW vans plugged in 17:00 to 07:00 against a 4 kW minimum and a
  # 4 kW import capacity, with the window starting at 06:00. Not flexible, they
  # charge from arrival; the boundary must not carry energy of the 22 kWh van
  # to 06:00.
  start <- as.POSIXct("2025-03-12 17:00", tz = "UTC")
  dttm <- seq(as.POSIXct("2025-03-12 06:00", tz = "UTC"), by = "15 min", length.out = 96 * 2)
  sessions <- bind_rows(
    mp_session("V1", start = start, hours = 14, power = 3.7, energy = 22),
    mp_session("V2", start = start, hours = 14, power = 3.7, energy = 11)
  ) %>%
    mutate(Profile = "Van")
  opt_data <- tibble(datetime = dttm, production = 0, import_capacity = 4)
  res <- smart_charging(
    sessions,
    opt_data,
    opt_objective = "grid",
    method = "curtail",
    window_days = 1,
    window_start_hour = 6,
    charging_power_min = 4,
    allow_interruptions = TRUE
  )
  uncontrolled <- sessions %>%
    evsim::adapt_charging_features(time_resolution = 15) %>%
    evsim::get_demand(dttm_seq = dttm)
  expect_equal(res$demand$Van, uncontrolled$Van)
})

test_that("allow_interruptions must be a single TRUE or FALSE", {
  for (bad in list(NA, "yes", 1, c(TRUE, FALSE), logical(0), NULL)) {
    expect_error(
      schedule_sessions(
        mp_session("S1"),
        mp_setpoint(0),
        method = "curtail",
        charging_power_min = 4,
        allow_interruptions = bad,
        show_progress = FALSE
      ),
      "allow_interruptions"
    )
  }
  expect_error(
    smart_charging(
      mp_session("S1"),
      tibble(datetime = mp_dttm, production = 0, Worktime = 0),
      opt_objective = "none",
      method = "curtail",
      window_days = 1,
      window_start_hour = 8,
      allow_interruptions = NA
    ),
    "allow_interruptions"
  )
})


# V2G ----------------------------------------------------------------------

mp_v2g_session <- function(id, ...) {
  mp_session(id, ...) %>%
    mutate(BatteryCapacity = 50, StateOfCharge = 0.5)
}

test_that("V2G stops a session whose charge would be below the minimum", {
  for (sp in c(1, 2)) {
    res <- schedule_sessions_v2g(
      mp_v2g_session("S1"),
      mp_setpoint(sp),
      charging_power_min = 4,
      soc_min = 0.2
    )
    powers <- mp_slot_powers(res$sessions)
    expect_equal(powers$S1[1:20], c(rep(0, 16), rep(11, 4)))
  }
})

test_that("a V2G slot with several sessions below the minimum stays at or below the setpoint", {
  # Postponing in proportion leaves the three 2 kW each, in (0, 4): raised
  # to 4 kW they would draw 12 kW. Two stop and one charges 4 kW.
  sessions <- bind_rows(
    mp_v2g_session("A"),
    mp_v2g_session("B", start = mp_start + 15 * 60, hours = 7.75),
    mp_v2g_session("C", start = mp_start + 30 * 60, hours = 7.5)
  )
  res <- schedule_sessions_v2g(
    sessions,
    mp_setpoint(6),
    charging_power_min = 4,
    soc_min = 0.2
  )
  powers <- mp_slot_powers(res$sessions)
  low <- 1:16
  expect_true(all(rowSums(powers[low, ]) <= 6 + 0.01))
  # No slot power between 0 and the minimum, but the tail of a charge
  for (p in powers) {
    p <- p[-max(which(p != 0))]
    expect_false(any(abs(p) > 1e-6 & abs(p) < 4 - 1e-6))
  }
  expect_equal(
    res$sessions %>% group_by(Session) %>% summarise(E = sum(Energy)) %>% pull(E),
    rep(11, 3),
    tolerance = 0.01
  )
})

test_that("V2G stops sessions only down to the import capacity", {
  # Two vans overnight under a flat 10 kW load and a 14 kW import capacity:
  # 4 kW of room. The "grid" setpoint spreads their 33 kWh at about 2.4 kW,
  # below one van's 4 kW minimum in every slot. Stopping down to the setpoint
  # stopped both everywhere (10 kWh in 1.19.0); down to the capacity one van
  # charges at 4 kW at a time and both get their energy.
  tz <- "Europe/Amsterdam"
  dttm <- seq(
    as.POSIXct("2025-01-13 00:00", tz = tz),
    by = "15 min",
    length.out = 2 * 96
  )
  vans <- tibble(
    Session = c("S1", "S2"),
    Timecycle = "Workday",
    Profile = "Vans",
    ConnectionStartDateTime = as.POSIXct(
      c("2025-01-13 17:00", "2025-01-13 18:00"),
      tz = tz
    ),
    ConnectionHours = c(14, 12),
    Power = 11,
    Energy = c(22, 11),
    BatteryCapacity = 70
  )
  opt_data <- tibble(
    datetime = dttm,
    production = 0,
    static = 10,
    import_capacity = 14
  )
  res <- suppressWarnings(suppressMessages(smart_v2g(
    vans,
    opt_data,
    opt_objective = "grid",
    window_days = 1,
    window_start_hour = 12,
    charging_power_min = 4,
    discharge_eff = 0.9,
    soc_min = 20,
    soc_max = 100,
    soc_final_min = 80,
    cycle_cost = 0
  )))

  delivered <- res$sessions %>%
    group_by(Session) %>%
    summarise(Energy = sum(Energy))
  expect_equal(delivered$Energy, c(22, 11), tolerance = 0.01)
  expect_true(all(res$demand$Vans + opt_data$static <= 14 + 1e-6))
})

test_that("curtail stops sessions only down to the import capacity", {
  # The V2G fixture above through `method = "curtail"`. The "grid" setpoint
  # spreads the 33 kWh at about 2.4 kW, below one van's minimum. Stopping
  # down to it stopped both vans until they had to charge at full power at
  # the end (a 22 kW peak at 05:00 in 1.19.0); down to the 4 kW of room one
  # van charges at a time at 4 kW.
  tz <- "Europe/Amsterdam"
  dttm <- seq(
    as.POSIXct("2025-01-13 00:00", tz = tz),
    by = "15 min",
    length.out = 2 * 96
  )
  vans <- tibble(
    Session = c("S1", "S2"),
    Timecycle = "Workday",
    Profile = "Vans",
    ConnectionStartDateTime = as.POSIXct(
      c("2025-01-13 17:00", "2025-01-13 18:00"),
      tz = tz
    ),
    ConnectionHours = c(14, 12),
    Power = 11,
    Energy = c(22, 11)
  )
  opt_data <- tibble(
    datetime = dttm,
    production = 0,
    static = 10,
    import_capacity = 14
  )
  res <- suppressWarnings(suppressMessages(smart_charging(
    vans,
    opt_data,
    opt_objective = "grid",
    method = "curtail",
    window_days = 1,
    window_start_hour = 12,
    charging_power_min = 4,
    allow_interruptions = TRUE
  )))

  delivered <- res$sessions %>%
    group_by(Session) %>%
    summarise(Energy = sum(Energy))
  expect_equal(delivered$Energy, c(22, 11), tolerance = 0.01)
  expect_true(all(res$demand$Vans + opt_data$static <= 14 + 1e-6))
  # No slot power between 0 and the minimum, but the tail of a charge
  powers <- res$sessions %>%
    evsim::get_demand(dttm_seq = dttm, by = "Session") %>%
    select(-"datetime")
  for (p in powers) {
    p <- p[-max(which(p != 0))]
    expect_false(any(p > 1e-6 & p < 4 - 1e-6))
  }
})

test_that("V2G discharges at the minimum only where the plan or the capacity asks for it", {
  # One car, 21 kWh on a 70 kWh battery (arrives at 70 %), 08:00 to 18:00,
  # next to 11 kW of fixed load until 16:00 under a 13 kW import capacity:
  # 2 kW of room until 16:00, below the 4 kW minimum, and 13 kW after. The
  # "grid" setpoint discharges 0.1 kW until 16:00 to flatten the site. Raised
  # to the 4 kW minimum because the soc_final_min deadline allowed it, that
  # was 23 kWh of discharge the 13 kW cap had no room to charge back: in
  # 1.19.2 the car charged 11 kW from 15:00 on top of the fixed load (22 kW)
  # to leave at 80 %. It now waits until 16:00 and leaves with all 21 kWh.
  tz <- "Europe/Amsterdam"
  dttm <- seq(
    as.POSIXct("2025-03-12 00:00", tz = tz),
    by = "15 min",
    length.out = 2 * 96
  )
  hour <- as.numeric(format(dttm, "%H")) + as.numeric(format(dttm, "%M")) / 60
  static <- ifelse(as.Date(dttm, tz = tz) == as.Date("2025-03-12") &
    hour >= 8 & hour < 16, 11, 0)
  opt_data <- tibble(
    datetime = dttm,
    production = 0,
    static = static,
    import_capacity = 13,
    export_capacity = 13
  )
  car <- tibble(
    Session = "S1",
    Timecycle = "Workday",
    Profile = "Car",
    ConnectionStartDateTime = as.POSIXct("2025-03-12 08:00", tz = tz),
    ConnectionHours = 10,
    Power = 11,
    Energy = 21,
    BatteryCapacity = 70
  )
  res <- suppressWarnings(suppressMessages(smart_v2g(
    car,
    opt_data,
    opt_objective = "grid",
    window_days = 1,
    window_start_hour = 7,
    charging_power_min = 4,
    discharge_eff = 0.9,
    soc_min = 20,
    soc_max = 100,
    soc_final_min = 80
  )))

  expect_true(any(res$setpoints$Car < -0.05))
  expect_true(all(res$demand$Car >= 0))
  expect_true(all(res$demand$Car + static <= 13 + 1e-6))
  expect_equal(sum(res$sessions$Energy), 21, tolerance = 0.01)
})

test_that("V2G discharges a small setpoint at the minimum for the capacity", {
  # The same discharge below the minimum, now needed: a -2 kW setpoint (given
  # directly, so it is the cap) still takes the session to -4 kW, and with a
  # `power_cap` the slot needs, the session discharges at the minimum too.
  setpoint <- mp_setpoint(-2)
  res <- schedule_sessions_v2g(
    mp_v2g_session("S1"),
    setpoint,
    charging_power_min = 4,
    soc_min = 0.2
  )
  expect_equal(mp_slot_powers(res$sessions)$S1[1], -4)

  res_cap <- schedule_sessions_v2g(
    mp_v2g_session("S1"),
    mutate(setpoint, power_cap = setpoint),
    charging_power_min = 4,
    soc_min = 0.2
  )
  expect_equal(mp_slot_powers(res_cap$sessions)$S1[1], -4)

  # With room under the cap, a setpoint below one minimum of discharge is
  # not rounded up to it.
  res_room <- schedule_sessions_v2g(
    mp_v2g_session("S1"),
    mutate(setpoint, power_cap = 5),
    charging_power_min = 4,
    soc_min = 0.2
  )
  expect_equal(mp_slot_powers(res_room$sessions)$S1[1], 0)
})
