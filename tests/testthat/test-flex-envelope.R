library(dplyr)

# Departure deadlines, the V2G connection envelope and State-of-Charge, and
# the minimum charging power in kW. Synthetic sessions at 15 minutes, one
# profile (`Fleet`), so every quantity can be checked by hand. The sessions are
# adapted to the resolution up front, as the functions do: that rounds the
# charging hours to 2 decimals and recomputes the energy (20 kWh at 11 kW
# becomes 20.02 kWh), so the expectations use the adapted `Energy`.

envelope_sessions <- function(df) {
  df %>%
    mutate(
      Timecycle = "Day",
      Profile = "Fleet",
      ConnectionEndDateTime = ConnectionStartDateTime +
        lubridate::minutes(round(ConnectionHours * 60)),
      ChargingHours = Energy / Power,
      ChargingStartDateTime = ConnectionStartDateTime,
      ChargingEndDateTime = ChargingStartDateTime +
        lubridate::minutes(round(ChargingHours * 60)),
      FlexibilityHours = ConnectionHours - ChargingHours
    ) %>%
    evsim::adapt_charging_features(time_resolution = 15)
}

envelope_dttm <- function(from, hours = 24) {
  start <- lubridate::ymd_hms(from, tz = "UTC")
  seq(start, start + lubridate::hours(hours) - lubridate::minutes(15), by = "15 min")
}

# Power of every session in every slot (one column per session)
slot_power <- function(sessions, dttm_seq) {
  sessions %>%
    filter(.data$ChargingHours > 0) %>%
    evsim::get_demand(dttm_seq = dttm_seq, by = "Session")
}

# Slots where a session charges or discharges inside (-min, min) but not 0,
# other than the slot that completes its charge (the tail, whose power is
# what it still needs, below the minimum).
below_min_slots <- function(sessions, dttm_seq, power_min, target) {
  powers <- slot_power(sessions, dttm_seq)
  found <- list()
  for (s in setdiff(names(powers), "datetime")) {
    p <- powers[[s]]
    energy <- cumsum(p) * 15 / 60
    small <- which(p != 0 & abs(p) < power_min - 1e-6)
    tail <- p[small] > 0 & energy[small] >= target[[s]] - 0.01
    if (any(!tail)) {
      found[[s]] <- tibble(Session = s, datetime = dttm_seq[small[!tail]], Power = p[small[!tail]])
    }
  }
  bind_rows(found)
}


# Departure deadlines ------------------------------------------------------

# Two sessions with equal energy plugged in at 18:00, one leaving at 04:00 and
# one at 17:00. The site is busy (40 kW) until 08:00, so the grid objective
# would push all the charging to the morning: past the first departure.
deadline_dttm <- envelope_dttm("2024-01-10 18:00:00")
deadline_sessions <- envelope_sessions(tibble(
  Session = c("early", "late"),
  ConnectionStartDateTime = lubridate::ymd_hms("2024-01-10 18:00:00", tz = "UTC"),
  ConnectionHours = c(10, 23),
  Power = 11,
  Energy = 20
))
deadline_opt_data <- tibble(
  datetime = deadline_dttm,
  production = 0,
  static = ifelse(lubridate::hour(deadline_dttm) %in% c(18:23, 0:7), 40, 0)
)
# The last slot the early session can charge in starts at 03:45
early_due <- which(deadline_dttm == lubridate::ymd_hms("2024-01-11 03:45:00", tz = "UTC"))

test_that("smart_charging delivers an early departure before it leaves", {
  sc <- smart_charging(
    deadline_sessions, deadline_opt_data,
    opt_objective = "grid", method = "curtail",
    window_days = 1, window_start_hour = 18
  )

  setpoint_energy <- cumsum(sc$setpoints$Fleet) * 15 / 60
  expect_true(all(setpoint_energy[early_due:length(deadline_dttm)] >= 20 - 0.01))

  early <- filter(sc$sessions, Session == "early")
  expect_equal(sum(early$Energy), 20.02, tolerance = 1e-3)
  expect_true(all(early$ChargingEndDateTime[early$Power > 0] <=
    lubridate::ymd_hms("2024-01-11 04:00:00", tz = "UTC")))
  # The rest still goes to the morning
  expect_equal(sum(sc$sessions$Energy), 40.04, tolerance = 1e-3)
})

test_that("smart_v2g delivers an early departure before it leaves", {
  # The late session arrives at its SoC floor, so it has nothing to discharge
  # and the early session's energy has to come from the grid.
  sessions_v2g <- mutate(deadline_sessions, StateOfCharge = c(50, 20))
  v2g <- smart_v2g(
    sessions_v2g, deadline_opt_data,
    opt_objective = "grid",
    window_days = 1, window_start_hour = 18
  )

  setpoint_energy <- cumsum(v2g$setpoints$Fleet) * 15 / 60
  expect_true(all(setpoint_energy[early_due:length(deadline_dttm)] >= 20 - 0.01))

  early_power <- slot_power(filter(v2g$sessions, Session == "early"), deadline_dttm)$early
  expect_equal(sum(early_power[seq_len(early_due)]) * 15 / 60, 20.02, tolerance = 1e-3)
  expect_true(all(early_power[-seq_len(early_due)] == 0))
})

test_that("get_flex_envelope owes each session's energy at its departure slot", {
  envelope <- get_flex_envelope(
    deadline_sessions,
    dttm_seq = deadline_dttm,
    profile = "Fleet",
    energy_min = 0.5
  )
  late_due <- which(deadline_dttm == lubridate::ymd_hms("2024-01-11 16:45:00", tz = "UTC"))
  # 50% of 20.02 kWh is 40.04 kW over a 15-minute slot
  expect_equal(envelope$owed[early_due - 1], 0)
  expect_equal(envelope$owed[early_due], 40.04)
  expect_equal(envelope$owed[late_due], 80.08)
  # Both connected at nominal power until 04:00, then only the late one
  expect_equal(envelope$LFmax[early_due], 22)
  expect_equal(envelope$LFmax[early_due + 1], 11)
  expect_true(all(envelope$reserve == 0))

  # SoC 0.5, floor 0.2: (0.5 - 0.2) * 40.04 kWh = 12.012 kWh, 48.048 kW over
  # a slot, until the slot before the one each session is due in
  envelope <- get_flex_envelope(
    mutate(deadline_sessions, StateOfCharge = 0.5),
    dttm_seq = deadline_dttm,
    profile = "Fleet",
    soc_min = 0.2
  )
  expect_equal(envelope$reserve[1], 96.096)
  expect_equal(envelope$reserve[early_due - 1], 96.096)
  expect_equal(envelope$reserve[early_due], 48.048)
  expect_equal(envelope$reserve[late_due], 0)
})


# V2G connection envelope ---------------------------------------------------

# Three vehicles plugged in from 08:00 to 17:00. The site is busy at night and
# has a peak at noon: the V2G setpoint used to discharge at night with no
# vehicle connected.
connected_dttm <- envelope_dttm("2024-01-10 00:00:00")
connected_sessions <- envelope_sessions(tibble(
  Session = paste0("S", 1:3),
  ConnectionStartDateTime = lubridate::ymd_hms("2024-01-10 08:00:00", tz = "UTC"),
  ConnectionHours = 9,
  Power = 11,
  Energy = 15
))
connected_opt_data <- tibble(
  datetime = connected_dttm,
  production = 0,
  static = case_when(
    lubridate::hour(connected_dttm) < 8 ~ 40,
    lubridate::hour(connected_dttm) >= 17 ~ 40,
    lubridate::hour(connected_dttm) == 12 ~ 60,
    TRUE ~ 0
  )
)

test_that("smart_v2g never charges or discharges with no vehicle connected", {
  v2g <- smart_v2g(
    connected_sessions, connected_opt_data,
    opt_objective = "grid",
    window_days = 1, window_start_hour = 0
  )
  connected <- connected_dttm >= lubridate::ymd_hms("2024-01-10 08:00:00", tz = "UTC") &
    connected_dttm < lubridate::ymd_hms("2024-01-10 17:00:00", tz = "UTC")

  expect_true(all(abs(v2g$setpoints$Fleet[!connected]) < 1e-6))
  expect_true(all(abs(v2g$demand$Fleet[!connected]) < 1e-6))
  # It still discharges at the noon peak, while connected
  expect_true(any(v2g$demand$Fleet[connected] < 0))
  # And never more than the connected vehicles' power
  expect_true(all(v2g$setpoints$Fleet >= -33 - 1e-6))

  # A gap between two sessions, with the site's peak in it: nothing to
  # discharge from there.
  gap_sessions <- envelope_sessions(tibble(
    Session = c("morning", "afternoon"),
    ConnectionStartDateTime = lubridate::ymd_hms(
      c("2024-01-10 07:00:00", "2024-01-10 14:00:00"),
      tz = "UTC"
    ),
    ConnectionHours = 4,
    Power = 11,
    Energy = 10
  ))
  gap <- connected_dttm >= lubridate::ymd_hms("2024-01-10 11:00:00", tz = "UTC") &
    connected_dttm < lubridate::ymd_hms("2024-01-10 14:00:00", tz = "UTC")
  v2g_gap <- smart_v2g(
    gap_sessions,
    mutate(connected_opt_data, static = ifelse(gap, 60, 0)),
    opt_objective = "grid",
    window_days = 1, window_start_hour = 0
  )
  expect_true(all(abs(v2g_gap$setpoints$Fleet[gap]) < 1e-6))
  expect_true(all(abs(v2g_gap$demand$Fleet[gap]) < 1e-6))
})


# State-of-Charge and SoC floor ---------------------------------------------

soc_levels <- function(v2g, sessions, dttm_seq) {
  powers <- slot_power(v2g$sessions, dttm_seq)
  lapply(set_names(sessions$Session), function(s) {
    session <- sessions[sessions$Session == s, ]
    # `StateOfCharge` of smart_v2g() is in %
    soc <- session$StateOfCharge / 100
    capacity <- session$Energy / (1 - soc)
    level <- soc * capacity + cumsum(powers[[s]]) * 15 / 60
    list(
      capacity = capacity,
      start = soc * capacity,
      level = level,
      power = powers[[s]]
    )
  })
}

test_that("smart_v2g keeps every session above its SoC floor and fills it by departure", {
  sessions_soc <- connected_sessions %>%
    mutate(StateOfCharge = c(30, 60, 10))
  soc_min <- 30

  v2g <- smart_v2g(
    sessions_soc, connected_opt_data,
    opt_objective = "grid",
    window_days = 1, window_start_hour = 0,
    soc_min = soc_min
  )
  levels <- soc_levels(v2g, sessions_soc, connected_dttm)

  # Discharge happens, so the floor is actually tested
  expect_true(any(unlist(lapply(levels, `[[`, "power")) < 0))

  # The scheduler returns powers rounded to 2 decimals: 0.02 kWh of slack
  for (s in c("S1", "S2")) {
    floor <- soc_min / 100 * levels[[s]]$capacity
    expect_true(all(levels[[s]]$level >= floor - 0.02), label = s)
  }
  # S3 arrives below the floor: it never discharges before reaching it, and
  # never goes back below it afterwards
  s3 <- levels$S3
  floor <- soc_min / 100 * s3$capacity
  reached <- which(s3$level >= floor - 0.02)[1]
  expect_false(is.na(reached))
  expect_true(all(s3$power[seq_len(reached)] >= 0))
  expect_true(all(s3$level[reached:length(s3$level)] >= floor - 0.02))

  # Each session leaves full: its energy takes it from its arrival SoC to 100%
  for (s in names(levels)) {
    expect_equal(
      levels[[s]]$level[length(connected_dttm)],
      levels[[s]]$capacity,
      tolerance = 1e-3,
      label = s
    )
  }
})

test_that("with soc_max below 100 a V2G session leaves below full", {
  # Arriving at 40 %, soc_max = 88 caps it at 80 % of its energy
  sessions_soc <- connected_sessions %>% mutate(StateOfCharge = 40)
  v2g <- suppressMessages(smart_v2g(
    sessions_soc, connected_opt_data,
    opt_objective = "grid",
    window_days = 1, window_start_hour = 0,
    soc_max = 88
  ))
  levels <- soc_levels(v2g, sessions_soc, connected_dttm)
  for (s in names(levels)) {
    final_soc <- levels[[s]]$level[length(connected_dttm)] / levels[[s]]$capacity
    expect_equal(final_soc, 0.4 + 0.8 * (1 - 0.4), tolerance = 1e-3, label = s)
  }
})

test_that("StateOfCharge defaults to 50 % and both V2G arguments are validated", {
  run_v2g <- function(sessions, ...) {
    smart_v2g(
      sessions, connected_opt_data,
      opt_objective = "grid",
      window_days = 1, window_start_hour = 0, ...
    )
  }
  expect_equal(
    run_v2g(connected_sessions)$demand,
    run_v2g(mutate(connected_sessions, StateOfCharge = 50))$demand
  )
  expect_error(run_v2g(connected_sessions, soc_min = 100), "soc_min")
  expect_error(run_v2g(connected_sessions, soc_min = -0.1), "soc_min")
  expect_error(run_v2g(connected_sessions, soc_min = c(0.1, 0.2)), "soc_min")
  expect_error(run_v2g(mutate(connected_sessions, StateOfCharge = 100)), "StateOfCharge")
  expect_error(run_v2g(mutate(connected_sessions, StateOfCharge = NA_real_)), "StateOfCharge")
  expect_error(run_v2g(connected_sessions, charging_power_min = -1), "charging_power_min")
})


# Minimum power in kW -------------------------------------------------------

# Four 11 kW sessions overnight: the grid objective flattens their load to
# about 7 kW in total, so each one would be curtailed to under 2 kW.
min_power_dttm <- envelope_dttm("2024-01-10 18:00:00")
min_power_sessions <- envelope_sessions(tibble(
  Session = paste0("S", 1:4),
  ConnectionStartDateTime = lubridate::ymd_hms("2024-01-10 18:00:00", tz = "UTC") +
    lubridate::minutes(15 * (0:3)),
  ConnectionHours = 12,
  Power = 11,
  Energy = 20
))
min_power_opt_data <- tibble(
  datetime = min_power_dttm,
  production = 0,
  static = ifelse(lubridate::hour(min_power_dttm) %in% c(18:21), 30, 0)
)
min_power_target <- set_names(min_power_sessions$Energy, min_power_sessions$Session)

test_that("smart_charging never curtails a session below charging_power_min (kW)", {
  sc <- smart_charging(
    min_power_sessions, min_power_opt_data,
    opt_objective = "grid", method = "curtail",
    window_days = 1, window_start_hour = 18,
    charging_power_min = 4
  )
  powers <- slot_power(sc$sessions, min_power_dttm)
  # Curtailment happens, so the minimum is actually tested
  expect_true(any(as.matrix(powers[-1]) > 0 & as.matrix(powers[-1]) < 11 - 1e-6))
  expect_equal(nrow(below_min_slots(sc$sessions, min_power_dttm, 4, min_power_target)), 0)
  expect_equal(sum(sc$sessions$Energy), sum(min_power_sessions$Energy), tolerance = 1e-3)
})

test_that("smart_v2g never charges or discharges below charging_power_min (kW)", {
  opt_data_peak <- min_power_opt_data %>%
    mutate(static = ifelse(lubridate::hour(datetime) %in% c(18:21), 30, 5))
  v2g <- smart_v2g(
    min_power_sessions, opt_data_peak,
    opt_objective = "grid",
    window_days = 1, window_start_hour = 18,
    charging_power_min = 4
  )
  powers <- as.matrix(slot_power(v2g$sessions, min_power_dttm)[-1])
  expect_true(any(powers < 0))
  expect_equal(nrow(below_min_slots(v2g$sessions, min_power_dttm, 4, min_power_target)), 0)
})

# One 11 kW session asked to discharge as much as it can in its first slot.
single_session <- function(state_of_charge = 0.5, energy = 10) {
  envelope_sessions(tibble(
    Session = "S1",
    ConnectionStartDateTime = lubridate::ymd_hms("2024-01-10 08:00:00", tz = "UTC"),
    ConnectionHours = 4,
    Power = 11,
    Energy = energy,
    StateOfCharge = state_of_charge
  )) %>%
    mutate(Responsive = TRUE)
}
single_setpoint <- function(first_slot) {
  tibble(
    datetime = envelope_dttm("2024-01-10 08:00:00", hours = 4),
    setpoint = c(first_slot, rep(11, 15))
  )
}
first_slot_power <- function(results) {
  slot_power(results$sessions, envelope_dttm("2024-01-10 08:00:00", hours = 4))$S1[1]
}

test_that("a V2G discharge the SoC floor limits below the minimum does not happen", {
  # Battery of 20 kWh at 50%, floor at 47.5%: 0.5 kWh to give, 2 kW in a slot
  run <- function(charging_power_min) {
    schedule_sessions_v2g(
      single_session(), single_setpoint(-11),
      charging_power_min = charging_power_min,
      soc_min = 0.475
    )
  }
  expect_equal(first_slot_power(run(0)), -2, tolerance = 1e-6)
  expect_equal(first_slot_power(run(4)), 0)
  expect_equal(first_slot_power(run(1)), -2, tolerance = 1e-6)
})

test_that("charging_power_min below or at 1 is kW, not a share of the nominal power", {
  # Curtailing to a 0 kW setpoint stops at the minimum: 0.5 kW, not 5.5 kW
  for (power_min in c(0.5, 1)) {
    sc <- schedule_sessions(
      single_session(energy = 5), single_setpoint(0),
      method = "curtail",
      charging_power_min = power_min,
      show_progress = FALSE
    )
    expect_equal(first_slot_power(sc), power_min, tolerance = 1e-6)

    # A small V2G discharge goes up to the minimum: -0.5 kW, not -5.5 kW
    v2g <- schedule_sessions_v2g(
      single_session(energy = 5), single_setpoint(-0.3),
      charging_power_min = power_min,
      soc_min = 0
    )
    expect_equal(first_slot_power(v2g), -power_min, tolerance = 1e-6)
  }
  expect_error(
    schedule_sessions(
      single_session(), single_setpoint(0),
      method = "curtail", charging_power_min = "1", show_progress = FALSE
    ),
    "charging_power_min"
  )
})


# Sessions below the minimum power -------------------------------------------

# A 3.7 kW session next to two 11 kW sessions, with a 4 kW minimum.
slow_sessions <- envelope_sessions(tibble(
  Session = c("S1", "S2", "slow"),
  ConnectionStartDateTime = lubridate::ymd_hms("2024-01-10 18:00:00", tz = "UTC"),
  ConnectionHours = 11,
  Power = c(11, 11, 3.7),
  Energy = c(20, 20, 10)
))
slow_demand <- slow_sessions %>%
  filter(Session == "slow") %>%
  evsim::get_demand(dttm_seq = min_power_dttm) %>%
  pull(Fleet)

expect_slow_session_uncontrolled <- function(results) {
  slow <- filter(results$sessions, Session == "slow")
  expect_false(any(slow$Responsive %in% TRUE))
  slow_power <- slot_power(slow, min_power_dttm)$slow
  expect_equal(slow_power, slow_demand)
  expect_true(all(slow_power >= 0))
}

test_that("smart_charging leaves a session below the minimum power uncontrolled", {
  run <- function(sessions, opt_data) {
    smart_charging(
      sessions, opt_data,
      opt_objective = "grid", method = "curtail",
      window_days = 1, window_start_hour = 18,
      charging_power_min = 4
    )
  }
  sc <- run(slow_sessions, min_power_opt_data)
  expect_slow_session_uncontrolled(sc)

  # The setpoint carries it as fixed load: the same as optimizing without it
  # and its demand added to the static load
  sc_without <- run(
    filter(slow_sessions, Session != "slow"),
    mutate(min_power_opt_data, static = static + slow_demand)
  )
  expect_equal(sc$setpoints$Fleet, sc_without$setpoints$Fleet + slow_demand)
})

test_that("smart_v2g leaves a session below the minimum power uncontrolled", {
  run <- function(sessions, opt_data) {
    smart_v2g(
      sessions, opt_data,
      opt_objective = "grid",
      window_days = 1, window_start_hour = 18,
      charging_power_min = 4
    )
  }
  v2g <- run(slow_sessions, min_power_opt_data)
  expect_slow_session_uncontrolled(v2g)
  # The 11 kW sessions do discharge
  expect_true(any(filter(v2g$sessions, Session != "slow")$Power < 0))

  v2g_without <- run(
    filter(slow_sessions, Session != "slow"),
    mutate(min_power_opt_data, static = static + slow_demand)
  )
  expect_equal(v2g$setpoints$Fleet, v2g_without$setpoints$Fleet + slow_demand)
})
