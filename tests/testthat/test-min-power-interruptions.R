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
  opt_data <- tibble(
    datetime = mp_dttm,
    production = 0,
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
