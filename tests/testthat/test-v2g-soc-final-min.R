library(dplyr)

# The minimum final SoC of V2G (`soc_final_min`): a session's `Energy` takes
# it to `soc_max`, every objective aims for that, and it leaves lower, down to
# `soc_final_min`, only where the grid capacities cannot fit its energy.

final_dttm <- seq(
  as.POSIXct("2025-06-02 00:00", tz = "UTC"),
  by = "15 min",
  length.out = 96
)
final_hour <- as.numeric(format(final_dttm, "%H"))

# 70 kWh, plugged in at 08:00, 21 kWh to reach 100%: arrives at 70%
final_car <- tibble(
  Session = "S1",
  Timecycle = "Workday",
  Profile = "Fleet",
  ConnectionStartDateTime = final_dttm[1] + 8 * 3600,
  ConnectionHours = 10,
  Power = 11,
  Energy = 21,
  BatteryCapacity = 70
)

final_opt_data <- tibble(
  datetime = final_dttm,
  production = 0,
  static = 5,
  price_imported = ifelse(final_hour >= 11 & final_hour < 15, 0.1, 0.3),
  price_exported = 0
)

run_final <- function(objective, sessions = final_car,
                      opt_data = final_opt_data, ...) {
  suppressMessages(smart_v2g(
    sessions,
    opt_data,
    opt_objective = objective,
    window_days = 1,
    window_start_hour = 0,
    soc_min = 20,
    soc_max = 100,
    soc_final_min = 80,
    ...
  ))
}

# Departure SoC (%) of the single car, battery-side: a discharge drains
# `1 / discharge_eff` of what reaches the grid
final_departure <- function(results, arrival = 70, discharge_eff = 1) {
  fleet <- results$demand$Fleet * 15 / 60
  battery <- sum(ifelse(fleet < 0, fleet / discharge_eff, fleet))
  arrival + 100 * battery / 70
}

test_that("a v2g session arrives at soc_max - 100 * Energy / BatteryCapacity", {
  results <- run_final("grid")
  expect_equal(unique(results$sessions$StateOfCharge), 70)

  # Not fitting below soc_max (arriving below 0 %): an error naming it
  expect_error(
    run_final("grid", sessions = bind_rows(
      mutate(final_car, Session = "fits"),
      mutate(final_car, Session = "too_big", Energy = 75)
    )),
    "does not fit below `soc_max`.*arriving below 0 %.*Sessions: too_big$"
  )
  # A StateOfCharge column keeps its arrival
  results <- run_final("grid", sessions = mutate(final_car, StateOfCharge = 50))
  expect_equal(unique(results$sessions$StateOfCharge), 50)
})

test_that("without a grid constraint the v2g session leaves at soc_max", {
  for (objective in list("grid", "capacity", "cost", 0.5)) {
    results <- run_final(objective)
    expect_equal(
      final_departure(results),
      100,
      tolerance = 0.05,
      label = format(objective)
    )
  }
})

test_that("an import cap lowers the v2g departure only as far as it needs", {
  # 4 hours connected and 4 kW left under the import cap: 16 of the 21 kWh,
  # so it leaves at 70 + 100 * 16 / 70 = 92.9 %, between 80 and 100 %
  car <- mutate(final_car, ConnectionHours = 4)
  opt_data <- mutate(final_opt_data, import_capacity = 9)
  for (objective in list("grid", "capacity", "cost", 0.5)) {
    results <- run_final(objective, sessions = car, opt_data = opt_data)
    net <- opt_data$static + results$demand$Fleet - opt_data$production
    expect_true(all(net <= 9 + 0.01), info = format(objective))
    expect_equal(
      final_departure(results),
      70 + 100 * 16 / 70,
      tolerance = 0.05,
      label = format(objective)
    )
  }

  # A cap that leaves less than the minimum (2 kW, 8 kWh to reach 80 % needs
  # 14 kWh): the minimum is still charged, the cap relaxed only for it
  opt_data <- mutate(final_opt_data, import_capacity = 7)
  results <- run_final("grid", sessions = car, opt_data = opt_data)
  expect_gte(final_departure(results), 80 - 0.05)
})

test_that("soc_final_min applies to the battery derived from the energy", {
  # Without BatteryCapacity, 20 kWh from 50 % is a 40 kWh battery: leaving at
  # least at 80 % is at least 12 kWh, 60 % of its energy
  car <- final_car %>%
    select(-BatteryCapacity) %>%
    mutate(Energy = 20, StateOfCharge = 50, ConnectionHours = 4)
  sessions <- set_state_of_charge(mutate(car, StateOfCharge = 0.5))
  expect_equal(v2g_energy_min_ratio(sessions, 0.8), 0.6)
  expect_equal(v2g_energy_min_ratio(sessions, 0.4), 0)
  expect_equal(v2g_energy_min_ratio(sessions, NULL), 1)

  # 1 kW under the import cap for 4 hours (4 kWh): it charges its 12 kWh
  # minimum, with the cap relaxed only for that minimum
  opt_data <- mutate(final_opt_data, import_capacity = 6)
  results <- run_final("grid", sessions = car, opt_data = opt_data)
  net <- opt_data$static + results$demand$Fleet - opt_data$production
  expect_gte(sum(results$demand$Fleet) * 15 / 60, 12 - 0.05)
  expect_lte(sum(pmax(net - 6, 0)) * 15 / 60, 12 - 4 + 0.05)
})


# A V06-like month: 3-31 March at 15 minutes, 20 sessions per weekday and 3
# per weekend day (70 kWh, 11 kW, arriving 06:00-22:00 for 3-16 hours, 5-45
# kWh to reach 100 %), an office with PV exporting above its 50 kW export cap
# around noon, and a 120 kW evening load on Mondays, Wednesdays and Fridays
# that takes some evenings above the 200 kW import cap. Up to 1.17.1 the
# fleet charged ahead above an exact final SoC and had to give it back before
# leaving, so it discharged while the site exported and shaved evening peaks
# on days without an import breach.
v06_scenario <- function(last_day = "2025-03-31") {
  set.seed(1)
  dttm <- seq(
    as.POSIXct("2025-03-03 00:00", tz = "UTC"),
    as.POSIXct(paste(last_day, "23:45"), tz = "UTC"),
    by = "15 min"
  )
  hour <- as.numeric(format(dttm, "%H")) + as.numeric(format(dttm, "%M")) / 60
  wday <- as.POSIXlt(dttm)$wday
  weekend <- wday %in% c(0, 6)
  opt_data <- tibble(
    datetime = dttm,
    production = round(ifelse(
      hour >= 8 & hour <= 18,
      160 * sin(pi * (hour - 8) / 10),
      0
    ), 2),
    static = ifelse(!weekend & hour >= 7 & hour < 19, 40, 10) +
      ifelse(wday %in% c(1, 3, 5) & hour >= 17 & hour < 21, 120, 0),
    import_capacity = 200,
    export_capacity = 50,
    price_imported = ifelse(
      hour >= 17 & hour < 21,
      0.35,
      ifelse(hour >= 10 & hour < 16, 0.12, 0.25)
    ),
    price_exported = 0.05
  )
  days <- unique(as.Date(dttm))
  sessions <- purrr::map_dfr(seq_along(days), function(i) {
    n <- if (as.POSIXlt(days[i])$wday %in% c(0, 6)) 3 else 20
    tibble(
      Session = paste0("D", i, "S", seq_len(n)),
      Timecycle = "Workday",
      Profile = "Fleet",
      ConnectionStartDateTime = as.POSIXct(days[i], tz = "UTC") +
        round(runif(n, 6, 22) * 4) * 900,
      ConnectionHours = round(runif(n, 3, 16) * 4) / 4,
      Power = 11,
      Energy = round(runif(n, 5, 45), 1),
      BatteryCapacity = 70
    )
  }) %>%
    filter(.data$ConnectionStartDateTime + .data$ConnectionHours * 3600 <= max(dttm))
  list(opt_data = opt_data, sessions = sessions)
}

v06_run <- function(scenario, objective, discharge_eff = 1) {
  suppressMessages(smart_v2g(
    scenario$sessions,
    scenario$opt_data,
    opt_objective = objective,
    window_days = 1,
    window_start_hour = 6,
    soc_min = 20,
    soc_max = 100,
    soc_final_min = 80,
    discharge_eff = discharge_eff
  ))
}

# Departure SoC (%) of every session, battery-side
v06_departures <- function(scenario, results, discharge_eff = 1) {
  results$sessions %>%
    group_by(.data$Session) %>%
    summarise(
      battery = sum(ifelse(
        .data$Energy < 0,
        .data$Energy / discharge_eff,
        .data$Energy
      ))
    ) %>%
    left_join(
      select(scenario$sessions, "Session", "Energy", "BatteryCapacity"),
      by = "Session"
    ) %>%
    mutate(
      departure = 100 - 100 * (.data$Energy - .data$battery) /
        .data$BatteryCapacity
    ) %>%
    pull("departure")
}

test_that("a V06-like v2g fleet discharges neither into export nor to shave a peak", {
  scenario <- v06_scenario()
  opt_data <- scenario$opt_data
  results <- v06_run(scenario, "capacity")
  fleet <- results$demand$Fleet
  uncontrolled <- evsim::get_demand(
    evsim::adapt_charging_features(scenario$sessions, time_resolution = 15),
    dttm_seq = opt_data$datetime
  )$Fleet
  net <- opt_data$static + fleet - opt_data$production
  net_uncontrolled <- opt_data$static + uncontrolled - opt_data$production

  # Never discharges in a slot where the site exports
  expect_false(any(fleet < -0.1 & net < -0.1))

  # The import breach comes down, and no energy is lost doing it
  expect_gt(sum(pmax(net_uncontrolled - 200, 0)), 100)
  expect_lt(sum(pmax(net - 200, 0)) / 4, 0.1)
  expect_equal(sum(fleet), sum(uncontrolled), tolerance = 1e-3)

  # On the days (06:00 windows) without an import breach, no peak is shaved:
  # the fleet only discharges before that day's export breach, to make room
  # for charging in it (as much as it discharged, or more)
  day <- as.Date(opt_data$datetime - 6 * 3600)
  days <- tibble(day, fleet, uncontrolled, net_uncontrolled, i = seq_along(day)) %>%
    group_by(day) %>%
    summarise(
      breach = any(net_uncontrolled > 200 + 0.01),
      last_export_breach = max(c(i[-net_uncontrolled > 50 + 0.01], 0)),
      late_discharge = sum(fleet < -0.1 & i > last_export_breach),
      discharged = -sum(pmin(fleet, 0)),
      charged_in_breach = sum(pmax(fleet - uncontrolled, 0)[
        -net_uncontrolled > 50 + 0.01
      ])
    )
  expect_true(any(days$breach))
  expect_true(any(!days$breach & days$discharged > 1))
  expect_equal(sum(days$late_discharge[!days$breach]), 0)
  with(
    filter(days, !breach),
    expect_true(all(discharged <= charged_in_breach + 0.1))
  )

  # Every session leaves at least at 80 %
  departures <- v06_departures(scenario, results)
  expect_true(all(departures >= 80 - 0.05))
  expect_gt(mean(departures), 99)

  # Nor under the grid objective (which may shave peaks: it flattens the net
  # power), with the same energy
  results <- v06_run(scenario, "grid")
  net <- opt_data$static + results$demand$Fleet - opt_data$production
  expect_false(any(results$demand$Fleet < -0.1 & net < -0.1))
  expect_lt(sum(pmax(net - 200, 0)) / 4, 0.1)
  expect_true(all(v06_departures(scenario, results) >= 80 - 0.05))
})

test_that("v2g departures stay above soc_final_min after the discharge losses", {
  # One week of the V06-like fleet under the cost objective, with a 0.9
  # discharge efficiency: the fleet discharges in the expensive evening and
  # charges back what the losses drain
  scenario <- v06_scenario(last_day = "2025-03-09")
  results <- v06_run(scenario, "cost", discharge_eff = 0.9)
  fleet <- results$demand$Fleet
  hour <- as.numeric(format(scenario$opt_data$datetime, "%H"))
  expect_gt(-sum(pmin(fleet[hour >= 17 & hour < 21], 0)) / 4, 50)
  departures <- v06_departures(scenario, results, discharge_eff = 0.9)
  expect_true(all(departures >= 80 - 0.05))
  expect_gt(mean(departures), 99)
})

test_that("the v2g energy reward does not pay for the discharge losses", {
  # Two cars arriving at 70 % at 08:00 for 14 hours. Discharging in the
  # morning (0.25) to charge back midday (0.10 / 0.9) earns 0.14 per kWh,
  # but only on the 5 kW the site imports; the rest goes to an export worth
  # nothing. With an energy range the second pass of the discharge losses
  # once rewarded the grid-side energy of a discharge and its recharge, so
  # the fleet discharged at full power, 44 kWh per car.
  cars <- bind_rows(
    mutate(final_car, Session = "S1", ConnectionHours = 14),
    mutate(final_car, Session = "S2", ConnectionHours = 14)
  )
  opt_data <- mutate(
    final_opt_data,
    import_capacity = ifelse(final_hour >= 21, 15, Inf),
    price_imported = case_when(
      final_hour >= 10 & final_hour < 14 ~ 0.1,
      final_hour >= 17 & final_hour < 21 ~ 0.4,
      TRUE ~ 0.25
    ),
    static = ifelse(final_hour >= 17 & final_hour < 21, 30, 5)
  )
  discharged <- function(soc_final_min) {
    results <- suppressMessages(smart_v2g(
      cars,
      opt_data,
      opt_objective = "cost",
      window_days = 1,
      window_start_hour = 0,
      soc_min = 20,
      soc_final_min = soc_final_min,
      discharge_eff = 0.9
    ))
    -sum(pmin(results$demand$Fleet, 0)) / 4
  }
  expect_equal(discharged(80), discharged(100), tolerance = 0.05)
})

test_that("a v2g session is not cut for a cap another one's discharge makes room for", {
  # The rebound case of the simulator: S1 (30 kWh, 17:00 to 07:00) and S2
  # (20 kWh, 17:00 to 21:00), 11 kW each, under a 20 kW import cap with a
  # 12 kW load until 21:00. S2 alone has 8 kW of room for 4 hours, so its
  # 20 kWh fit. The grid setpoint discharges into the evening; S1's discharge
  # makes room under the cap, so S2 must not be curtailed below its energy
  # (1.18.0 gave it 14 kWh, leaving at 91.4 %).
  dttm <- seq(as.POSIXct("2025-01-02 00:00", tz = "UTC"), by = "15 min", length.out = 96 * 3)
  hour <- as.numeric(format(dttm, "%H"))
  opt_data <- tibble(
    datetime = dttm,
    production = 0,
    static = ifelse(hour %in% 17:20 & dttm < dttm[1] + 86400, 12, 2),
    import_capacity = 20,
    export_capacity = 20
  )
  start <- dttm[1] + 17 * 3600
  sessions <- tibble(
    Session = c("S1", "S2"),
    Timecycle = "Workday",
    Profile = "Fleet",
    ConnectionStartDateTime = start,
    ConnectionHours = c(14, 4),
    Power = 11,
    Energy = c(30, 20),
    BatteryCapacity = 70
  )
  results <- suppressMessages(smart_v2g(
    sessions,
    opt_data,
    opt_objective = "grid",
    window_days = 1,
    window_start_hour = 12,
    soc_min = 20,
    soc_max = 100,
    soc_final_min = 80
  ))
  energy <- tapply(results$sessions$Energy, results$sessions$Session, sum)
  expect_equal(as.numeric(energy[c("S1", "S2")]), c(30, 20), tolerance = 0.05)
  site <- opt_data$static[match(results$demand$datetime, dttm)] +
    results$demand$Fleet
  expect_true(all(site <= 20 + 0.05))
  expect_true(all(abs(results$sessions$Power) <= 11 + 1e-6))
})
