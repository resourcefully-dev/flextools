library(dplyr)

# The departure level of V2G (`soc_final_min`) and the headroom above it: a
# session's `Energy` takes it to `soc_final_min`, every objective plans its
# departure there, it leaves lower only where the grid capacities cannot fit
# its energy, and it may hold up to `soc_max` where its objective gains,
# without owing it back.

final_dttm <- seq(
  as.POSIXct("2025-06-02 00:00", tz = "UTC"),
  by = "15 min",
  length.out = 96
)
final_hour <- as.numeric(format(final_dttm, "%H"))

# 70 kWh, plugged in from 08:00 to 21:00, 21 kWh to reach 80 %: arrives at 50 %
final_car <- tibble(
  Session = "S1",
  Timecycle = "Workday",
  Profile = "Fleet",
  ConnectionStartDateTime = final_dttm[1] + 8 * 3600,
  ConnectionHours = 13,
  Power = 11,
  Energy = 21,
  BatteryCapacity = 70
)

# A flat site and flat prices: no objective gains from the headroom
final_flat <- tibble(
  datetime = final_dttm,
  production = 0,
  static = 5,
  price_imported = 0.25,
  price_exported = 0.05
)

run_final <- function(objective, sessions = final_car,
                      opt_data = final_flat, soc_max = 100, ...) {
  suppressMessages(smart_v2g(
    sessions,
    opt_data,
    opt_objective = objective,
    window_days = 1,
    window_start_hour = 0,
    soc_min = 20,
    soc_max = soc_max,
    soc_final_min = 80,
    ...
  ))
}

# State of charge (%) of the single car at the end of each slot,
# battery-side: a discharge drains `1 / discharge_eff` of what reaches the
# grid
final_battery <- function(results, discharge_eff = 1) {
  energy <- results$demand$Fleet * 15 / 60
  50 + 100 * cumsum(ifelse(energy < 0, energy / discharge_eff, energy)) / 70
}
final_departure <- function(results, discharge_eff = 1) {
  battery <- final_battery(results, discharge_eff)
  battery[length(battery)]
}

objectives <- list("grid", "capacity", "cost", 0.5)

test_that("a v2g session arrives at soc_final_min - 100 * Energy / BatteryCapacity", {
  results <- run_final("grid")
  expect_equal(unique(results$sessions$StateOfCharge), 50)

  # Not fitting below soc_final_min (arriving below 0 %): an error naming it,
  # also when it fits below soc_max
  expect_error(
    run_final("grid", sessions = bind_rows(
      mutate(final_car, Session = "fits"),
      mutate(final_car, Session = "too_big", Energy = 60)
    )),
    "does not fit below `soc_final_min`.*arriving below 0 %.*Sessions: too_big$"
  )
  # A StateOfCharge column keeps its arrival
  results <- run_final("grid", sessions = mutate(final_car, StateOfCharge = 40))
  expect_equal(unique(results$sessions$StateOfCharge), 40)
})

test_that("with nothing binding a v2g session leaves at soc_final_min", {
  for (objective in objectives) {
    results <- run_final(objective)
    expect_equal(
      final_departure(results),
      80,
      tolerance = 0.05,
      label = format(objective)
    )
  }
})

test_that("no v2g session charges into its headroom where nothing gains", {
  # Flat load and flat prices: never above 80 %, but for the setpoint's
  # rounding to 0.01 kW per slot (0.06 kWh here), and no discharge either
  # (under "cost" a discharge and its recharge at a flat price earn nothing;
  # HiGHS's default MIP gap let 37.5 kWh of it through in 1.21.0)
  for (objective in objectives) {
    results <- run_final(objective)
    expect_lte(max(final_battery(results)), 80 + 0.1)
    expect_gte(min(results$demand$Fleet), -0.01)
  }
  # A setpoint given directly is a limit, not a plan: no headroom
  opt_data <- mutate(final_flat, import_capacity = 100)
  results <- run_final("none", opt_data = opt_data)
  expect_lte(max(final_battery(results)), 80 + 0.05)
})

# PV above the 10 kW export capacity from 10:00 to 15:00, and an evening load
# above the 20 kW import capacity from 17:00 to 21:00, the car's departure.
# The site breaks both caps without the car.
breach_opt_data <- mutate(
  final_flat,
  production = ifelse(final_hour >= 10 & final_hour < 15, 30, 0),
  static = ifelse(final_hour >= 17 & final_hour < 21, 30, 5),
  import_capacity = 20,
  export_capacity = 10
)
breach_excess <- function(results) {
  net <- breach_opt_data$static + results$demand$Fleet -
    breach_opt_data$production
  c(
    export = sum(pmax(-net - 10, 0)) * 15 / 60,
    import = sum(pmax(net - 20, 0)) * 15 / 60
  )
}

test_that("v2g charges into the headroom for an export breach and gives it back in a peak", {
  for (objective in list("capacity", "grid")) {
    results <- run_final(objective, opt_data = breach_opt_data)
    no_headroom <- run_final(objective, opt_data = breach_opt_data, soc_max = 80)
    battery <- final_battery(results)
    info <- format(objective)

    # Up to 100 % in the export breach, never above
    expect_gt(max(battery), 99, label = info)
    expect_lte(max(battery), 100 + 0.05, label = info)
    expect_lte(max(final_battery(no_headroom)), 80 + 0.05, label = info)
    # Both breaches come down further than without headroom
    excess <- breach_excess(results)
    excess_no_headroom <- breach_excess(no_headroom)
    expect_lt(excess[["export"]], excess_no_headroom[["export"]] - 10)
    expect_lt(excess[["import"]], excess_no_headroom[["import"]] - 10)
    # It leaves at 80 % or above
    expect_gte(final_departure(results), 80 - 0.05, label = info)
    expect_gte(final_departure(no_headroom), 80 - 0.05, label = info)
    # And never discharges while the site exports
    net <- breach_opt_data$static + results$demand$Fleet -
      breach_opt_data$production
    expect_false(any(results$demand$Fleet < -0.01 & net < -0.01), info = info)
  }
})

# Import costs 0.10 from 11:00 to 15:00 and 0.40 from 17:00 to 21:00, 0.25
# otherwise, and the site always imports. A car from 07:00 to 21:00 can only
# discharge in the expensive evening what it charged ahead midday.
spread_car <- mutate(
  final_car,
  ConnectionStartDateTime = final_dttm[1] + 7 * 3600,
  ConnectionHours = 14
)
spread_opt_data <- mutate(
  final_flat,
  static = 10,
  price_imported = case_when(
    final_hour >= 11 & final_hour < 15 ~ 0.1,
    final_hour >= 17 & final_hour < 21 ~ 0.4,
    TRUE ~ 0.25
  ),
  price_exported = 0
)
evening <- final_hour >= 17 & final_hour < 21

test_that("cost v2g charges into the headroom for a price spread", {
  # 0.40 against 0.10 / 0.9: 0.29 per kWh discharged in the evening
  results <- run_final(
    "cost",
    sessions = spread_car,
    opt_data = spread_opt_data,
    discharge_eff = 0.9
  )
  expect_gt(max(final_battery(results, 0.9)), 99)
  expect_gt(-sum(pmin(results$demand$Fleet[evening], 0)) * 15 / 60, 10)
  expect_gte(final_departure(results, 0.9), 80 - 0.05)

  # Without headroom there is nothing to discharge in the evening
  results <- run_final(
    "cost",
    sessions = spread_car,
    opt_data = spread_opt_data,
    discharge_eff = 0.9,
    soc_max = 80
  )
  expect_lte(max(final_battery(results, 0.9)), 80 + 0.05)
  expect_gte(min(results$demand$Fleet[evening]), -0.01)

  # Nor where the cycle cost (0.30 per kWh of a 70 kWh battery) is above the
  # spread
  results <- run_final(
    "cost",
    sessions = spread_car,
    opt_data = spread_opt_data,
    discharge_eff = 0.9,
    cycle_cost = 21
  )
  expect_gte(min(results$demand$Fleet), -0.01)
  expect_lte(max(final_battery(results, 0.9)), 80 + 0.05)
})

test_that("an import cap lowers the v2g departure only as far as it needs", {
  # 4 hours connected and 4 kW left under the import cap: 16 of the 21 kWh,
  # so it leaves at 50 + 100 * 16 / 70 = 72.9 %, above its arrival
  car <- mutate(final_car, ConnectionHours = 4)
  opt_data <- mutate(final_flat, import_capacity = 9)
  for (objective in objectives) {
    results <- run_final(objective, sessions = car, opt_data = opt_data)
    net <- opt_data$static + results$demand$Fleet - opt_data$production
    expect_true(all(net <= 9 + 0.01), info = format(objective))
    expect_equal(
      final_departure(results),
      50 + 100 * 16 / 70,
      tolerance = 0.05,
      label = format(objective)
    )
  }

  # With 6 kW of room (24 kWh) the energy fits: it leaves at 80 %
  opt_data <- mutate(final_flat, import_capacity = 11)
  for (objective in objectives) {
    results <- run_final(objective, sessions = car, opt_data = opt_data)
    expect_equal(
      final_departure(results),
      80,
      tolerance = 0.05,
      label = format(objective)
    )
  }

  # A cap the site breaks on its own does not drain the car: it keeps its
  # energy and adds nothing to the breach
  opt_data <- mutate(final_flat, static = 12, import_capacity = 9)
  for (objective in objectives) {
    results <- run_final(objective, sessions = car, opt_data = opt_data)
    expect_gte(final_departure(results), 50 - 0.05)
  }
})

test_that("the v2g headroom is soc_max - soc_final_min above the departure, never above soc_max", {
  sessions <- tibble(
    Session = c("derived", "below", "above", "no_battery"),
    Energy = c(21, 14, 28, 20),
    StateOfCharge = c(0.5, 0.5, 0.5, 0.5),
    BatteryCapacity = c(70, 70, 70, NA)
  )
  headroom <- v2g_headroom(sessions[1:3, ], soc_max = 1, soc_final_min = 0.8)
  # 14 kWh above 80 %; 14 kWh above 70 %; 7 kWh above 90 %, up to 100 %
  expect_equal(headroom, c(14, 14, 7))
  # Without BatteryCapacity the energy fills the battery: no headroom
  expect_equal(
    v2g_headroom(
      select(sessions[4, ], -BatteryCapacity),
      soc_max = 1,
      soc_final_min = 0.8
    ),
    0
  )
  # soc_final_min at soc_max: none
  expect_equal(
    v2g_headroom(sessions[1:3, ], soc_max = 1, soc_final_min = 1),
    c(0, 0, 0)
  )
})


# A V06-like month: 3-31 March at 15 minutes, 20 sessions per weekday and 3
# per weekend day (70 kWh, 11 kW, arriving 06:00-22:00 for 3-16 hours, 5-45
# kWh to reach 80 %), an office with PV exporting above its 50 kW export cap
# around noon, and a 120 kW evening load on Mondays, Wednesdays and Fridays
# that takes some evenings above the 200 kW import cap. Up to 1.17.1 the
# fleet charged ahead above an exact final SoC and had to give it back before
# leaving, so it discharged while the site exported.
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

# Departure SoC (%) of every session, battery-side, against what it can
# charge in its connection at nominal power (some draws ask for more), with
# whether it left on a day (06:00 to 06:00) whose evening breaks the import
# capacity (Mondays, Wednesdays and Fridays)
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
    left_join(scenario$sessions, by = "Session") %>%
    mutate(
      reachable = pmin(.data$Energy, .data$Power * .data$ConnectionHours),
      departure = 80 - 100 * (.data$reachable - .data$battery) /
        .data$BatteryCapacity,
      breach_day = as.POSIXlt(
        as.Date(.data$ConnectionStartDateTime - 6 * 3600)
      )$wday %in% c(1, 3, 5)
    )
}

# Every session leaves at 80 % or above, but for the rounding of its
# segments to 0.01 kWh, and never above 100 %. On the days whose evening
# breaks the import capacity a session may leave a little lower, where the
# capacity forced it.
expect_v06_departures <- function(departures, info) {
  with(departures, {
    expect_true(all(departure[!breach_day] >= 80 - 0.15), info = info)
    expect_true(all(departure >= 79), info = info)
    expect_true(all(departure <= 100 + 0.05), info = info)
    expect_gt(mean(departure), 82)
  })
}

test_that("a V06-like v2g fleet never discharges into export and leaves at 80 % or above", {
  scenario <- v06_scenario()
  opt_data <- scenario$opt_data
  uncontrolled <- evsim::get_demand(
    evsim::adapt_charging_features(scenario$sessions, time_resolution = 15),
    dttm_seq = opt_data$datetime
  )$Fleet
  net_uncontrolled <- opt_data$static + uncontrolled - opt_data$production

  for (objective in c("capacity", "grid")) {
    results <- v06_run(scenario, objective)
    fleet <- results$demand$Fleet
    net <- opt_data$static + fleet - opt_data$production

    # Never discharges in a slot where the site exports
    expect_false(any(fleet < -0.1 & net < -0.1), info = objective)
    # The import breach comes down
    expect_gt(sum(pmax(net_uncontrolled - 200, 0)), 100)
    expect_lt(sum(pmax(net - 200, 0)) / 4, 0.1)
    # The headroom takes more of the export breach than the uncontrolled
    # fleet, and the fleet keeps at least its energy
    expect_lt(
      sum(pmax(-net - 50, 0)),
      sum(pmax(-net_uncontrolled - 50, 0)) - 100
    )
    expect_gte(sum(fleet), sum(uncontrolled) - 0.5)
    expect_v06_departures(v06_departures(scenario, results), objective)
  }
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
  expect_v06_departures(
    v06_departures(scenario, results, discharge_eff = 0.9),
    "cost"
  )
})

test_that("the v2g energy reward does not pay for the discharge losses", {
  # One window of two cars (21 kWh each, 08:00 to 22:00) next to a 5 kW load.
  # Discharging in the morning (0.25) to charge back midday (0.10 / 0.9)
  # earns 0.14 per kWh, but only on the 5 kW the site imports; the rest goes
  # to an export worth nothing. The energy range of the relaxed plan once
  # rewarded the grid-side energy of a discharge and its recharge in the
  # second pass of the discharge losses, so the fleet discharged at full
  # power. With a range or without, it discharges the same.
  slots <- final_hour >= 8 & final_hour < 22
  LF <- ifelse(final_hour >= 8 & final_hour < 10, 21, 0)
  PI <- case_when(
    final_hour >= 10 & final_hour < 14 ~ 0.1,
    final_hour >= 17 & final_hour < 21 ~ 0.4,
    TRUE ~ 0.25
  )
  discharged <- function(energy_ratio) {
    O <- suppressMessages(demand_cost_v2g_window(
      G = rep(0, 96), LF = LF, LS = rep(5, 96),
      PI = PI, PE = 0, PTD = 0, PTU = 0,
      direction = "forward", time_horizon = NULL,
      LFmax = ifelse(slots, 22, 0),
      import_capacity = Inf, export_capacity = Inf,
      energy_ratio = energy_ratio,
      cumsum_min = -cumsum(ifelse(slots, 0, 0)) - 4 * 28,
      discharge_eff = 0.9
    ))
    -sum(pmin(O, 0)) / 4
  }
  expect_gt(discharged(c(1, 1)), 1)
  expect_equal(discharged(c(0, 1)), discharged(c(1, 1)), tolerance = 0.05)
})

test_that("a v2g session is not cut for a cap another one's discharge makes room for", {
  # The rebound case of the simulator: S1 (30 kWh, 17:00 to 07:00) and S2
  # (20 kWh, 17:00 to 21:00), 11 kW each, under a 20 kW import cap with a
  # 12 kW load until 21:00. S2 alone has 8 kW of room for 4 hours, so its
  # 20 kWh fit. The grid setpoint discharges into the evening; S1's discharge
  # makes room under the cap, so S2 must not be curtailed below its energy
  # (1.18.0 gave it 14 kWh).
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
  expect_gte(energy[["S1"]], 30 - 0.05)
  expect_equal(energy[["S2"]], 20, tolerance = 0.05)
  site <- opt_data$static[match(results$demand$datetime, dttm)] +
    results$demand$Fleet
  expect_true(all(site <= 20 + 0.05))
  expect_true(all(abs(results$sessions$Power) <= 11 + 1e-6))
})
