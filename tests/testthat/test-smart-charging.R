library(dplyr)

# Use first 50 sessions
sessions <- evsim::california_ev_sessions_profiles %>%
  slice_head(n = 100) %>%
  evsim::adapt_charging_features(time_resolution = 15)
sessions_demand <- evsim::get_demand(sessions, resolution = 15)

# Don't require any other variable than datetime, since we don't
# care about local generation (just peak shaving objective)
opt_data <- tibble(
  datetime = sessions_demand$datetime,
  production = 0,
  price_imported = 0.1,
  price_exporte = 0
)

# # To test log viewer
# sc_results <- smart_charging(
#   sessions, opt_data, opt_objective = "grid", method = "curtail",
#   window_days = 1, window_start_hour = 6, energy_min = 0,
#   include_log = TRUE, show_progress = TRUE
# )
# view_smart_charging_logs(sc_results)

test_that("Get error when missing `sessions`", {
  expect_error(
    smart_charging(
      sessions = NULL,
      opt_data,
      opt_objective = "grid",
      method = "curtail",
      window_days = 1,
      window_start_hour = 6
    )
  )
})

test_that("Get error when missing `opt_data`", {
  expect_error(
    smart_charging(
      sessions = sessions,
      opt_data = NULL,
      opt_objective = "grid",
      method = "curtail",
      window_days = 1,
      window_start_hour = 6
    )
  )
})
test_that("Get error when `opt_data` has no `datetime`", {
  expect_error(
    smart_charging(
      sessions = sessions,
      opt_data = opt_data[2],
      opt_objective = "grid",
      method = "curtail",
      window_days = 1,
      window_start_hour = 6
    )
  )
})

test_that("Get error when `opt_objective` is mispelled", {
  expect_error(
    smart_charging(
      sessions,
      opt_data,
      opt_objective = "gridx",
      method = "curtail",
      window_days = 1,
      window_start_hour = 6
    )
  )
})

test_that("Get error when `method` is mispelled", {
  expect_error(
    smart_charging(
      sessions,
      opt_data,
      opt_objective = "grid",
      method = "curtailx",
      window_days = 1,
      window_start_hour = 6
    )
  )
})


test_that("Get error when no user profiles in `opt_data` and not optimization", {
  expect_error(smart_charging(
    sessions,
    opt_data,
    opt_objective = "none",
    method = "curtail",
    window_days = 1,
    window_start_hour = 6,
    responsive = list(Workday = list(Worktime = 0.9)),
    charging_power_min = 2
  ))
})


test_that("smart charging works with grid objective and 'none' method", {
  sc_results <- smart_charging(
    sessions,
    opt_data,
    opt_objective = "grid",
    method = "none",
    window_days = 1,
    window_start_hour = 5
  )

  # plot_smart_charging(sc_results, sessions, legend_width = 150)
  expect_equal(sc_results$demand, sc_results$setpoints)
})

test_that("smart charging works with grid objective and curtail method", {
  sc_results <- smart_charging(
    sessions,
    opt_data,
    opt_objective = "grid",
    method = "curtail",
    window_days = 1,
    window_start_hour = 5
  )
  # plot_smart_charging(sc_results, sessions, legend_width = 150)
  expect_type(sc_results, "list")
  print(sc_results) # Check print as well
  # Expect same amount of sessions "smart"
  expect_equal(
    length(unique(sessions$Session)),
    length(unique(sc_results$sessions$Session))
  )
  # Expect all sessions charge 100% of their energy
  expect_equal(
    trunc(sum(sessions$Energy) - sum(sc_results$sessions$Energy)),
    0
  )
  # Same demand in setpoints
  expect_equal(
    trunc(sum(sessions_demand$Worktime) - sum(sc_results$setpoints$Worktime)),
    0
  )
  # Same demand in optimal demand
  expect_equal(
    trunc(sum(sessions_demand$Worktime) - sum(sc_results$demand$Worktime)),
    0
  )
})


test_that("smart charging works with cost objective, interrupt method, responsiveness, and min energy of 0.5", {
  sc_results <- smart_charging(
    sessions,
    opt_data,
    opt_objective = "cost",
    method = "interrupt",
    window_days = 1,
    window_start_hour = 6,
    responsive = list(Workday = list(Worktime = 0.9)),
    energy_min = 0.5
  )
  expect_type(sc_results, "list")
})

test_that("smart charging works with combined objective, curtail method and min charging power ratio of 0.5", {
  opt_data <- opt_data %>%
    mutate(Workime = 0.5 * max(sessions_demand$Worktime))
  sc_results <- smart_charging(
    sessions,
    opt_data,
    opt_objective = 0.5,
    method = "curtail",
    window_days = 1,
    window_start_hour = 6,
    responsive = list(Workday = list(Worktime = 0.9)),
    charging_power_min = 0.5
  )
  expect_type(sc_results, "list")
})

test_that("smart charging works without optimization, curtail method and min charging power of 2kW, including logs and progress", {
  opt_data$Worktime <- 10
  sc_results <- smart_charging(
    sessions,
    opt_data,
    opt_objective = "none",
    method = "curtail",
    window_days = 1,
    window_start_hour = 6,
    responsive = list(Workday = list(Worktime = 0.9)),
    charging_power_min = 2,
    include_log = TRUE,
    show_progress = TRUE
  )
  expect_true(
    length(sc_results$log[[1]]) > 0
  )
  expect_type(sc_results, "list")
})

test_that("smart charging works with capacity objective and curtail method", {
  opt_data_cap <- opt_data %>%
    mutate(import_capacity = 50)
  sc_results <- smart_charging(
    sessions,
    opt_data_cap,
    opt_objective = "capacity",
    method = "curtail",
    window_days = 1,
    window_start_hour = 0,
    responsive = list(Workday = list(Worktime = 1))
  )
  expect_type(sc_results, "list")
  expect_equal(
    trunc(sum(sessions$Energy) - sum(sc_results$sessions$Energy)),
    0
  )
})

test_that("smart charging works without optimization but grid capacity limit and curtail method", {
  opt_data$grid_capacity <- 50
  sc_results <- smart_charging(
    sessions,
    opt_data,
    opt_objective = "none",
    method = "curtail",
    window_days = 1,
    window_start_hour = 0,
    responsive = list(Workday = list(Worktime = 1))
  )
  flex_demand <- round(rowSums(sc_results$demand[-1]))
  expect_true(all(flex_demand <= opt_data$grid_capacity))
})

test_that("using responsiveness for specific user profiles", {
  sc_results <- smart_charging(
    sessions,
    opt_data,
    opt_objective = "grid",
    method = "curtail",
    window_days = 1,
    window_start_hour = 6,
    responsive = list(Workday = list(Worktime = 0.5)),
    include_log = FALSE
  )
  summaryS <- summarise_smart_charging_sessions(sc_results)
  pct_responsive <- round(
    summaryS$pct[summaryS$subgroup == "Responsive"] / 100,
    1
  )
  expect_equal(pct_responsive, 0.5)
  expect_type(sc_results, "list")
})

test_that("invalid responsive names emit warnings but smart charging still runs", {
  expect_message(
    sc_results <- smart_charging(
      sessions,
      opt_data,
      opt_objective = "grid",
      method = "curtail",
      window_days = 1,
      window_start_hour = 6,
      responsive = list(Weekday = list(UnknownProfile = 1))
    ),
    "not found in `sessions`"
  )

  expect_type(sc_results, "list")
})

test_that("smart charging uses profile setpoints directly when opt_objective is none", {
  opt_data_profile <- tibble(
    datetime = sessions_demand$datetime,
    production = 0,
    Worktime = round(sessions_demand$Worktime * 0.5, 2),
    Visit = round(sessions_demand$Visit * 0.5, 2)
  )

  sc_results <- smart_charging(
    sessions,
    opt_data_profile,
    opt_objective = "none",
    method = "none",
    window_days = 1,
    window_start_hour = 6
  )

  expect_true(all(
    c("datetime", "Worktime", "Visit") %in% names(sc_results$setpoints)
  ))
  expect_equal(
    length(unique(sc_results$sessions$Session)),
    length(unique(sessions$Session))
  )
  expect_equal(
    sum(sc_results$sessions$Energy),
    sum(sessions$Energy),
    tolerance = 0.1
  )
  expect_equal(length(sc_results$log), 0)
})

test_that("using energy_min=NULL all sessions charge 100% for curtail", {
  sc_results <- smart_charging(
    sessions,
    opt_data,
    opt_objective = "grid",
    method = "curtail",
    window_days = 1,
    window_start_hour = 6
  )
  energy_summary <- summarise_energy_charged(sc_results, sessions) %>%
    filter(PctEnergyCharged < 99) # Has 1% tolerance
  expect_equal(nrow(energy_summary), 0)
})

test_that("using energy_min=NULL all sessions charge 100% for postpone", {
  sc_results <- smart_charging(
    sessions,
    opt_data,
    opt_objective = "grid",
    method = "postpone",
    window_days = 1,
    window_start_hour = 6
  )
  energy_summary <- summarise_energy_charged(sc_results, sessions) %>%
    filter(PctEnergyCharged < 100)
  expect_equal(nrow(energy_summary), 0)
})

test_that("using energy_min=NULL all sessions charge 100% for interrupt", {
  sc_results <- smart_charging(
    sessions,
    opt_data,
    opt_objective = "grid",
    method = "interrupt",
    window_days = 1,
    window_start_hour = 6
  )
  energy_summary <- summarise_energy_charged(sc_results, sessions) %>%
    filter(PctEnergyCharged < 100)
  expect_equal(nrow(energy_summary), 0)
})

test_that("using energy_min=0 setpoint can be achieved with curtail", {
  sc_results <- smart_charging(
    sessions,
    opt_data,
    opt_objective = "grid",
    method = "curtail",
    window_days = 1,
    window_start_hour = 5,
    energy_min = 0,
    include_log = TRUE
  )
  setpoint_df <- timefully::aggregate_timeseries(
    sc_results$setpoints,
    "setpoint"
  )
  demand_gt_setpiont <- timefully::aggregate_timeseries(
    get_demand(sc_results$sessions, setpoint_df$datetime),
    "demand"
  ) %>%
    mutate(setpoint_df['setpoint']) %>%
    filter(round(demand) > round(setpoint))
  expect_equal(nrow(demand_gt_setpiont), 0)
})

# Sessions flex type -----------------------------------------------------

sc_results <- smart_charging(
  sessions,
  opt_data,
  opt_objective = "grid",
  method = "curtail",
  window_days = 1,
  window_start_hour = 5,
  responsive = list(Workday = list(Worktime = 0.9)),
  energy_min = 0.5
)

test_that("smart charging sessions are summarised", {
  sc_results <- smart_charging(
    sessions,
    opt_data,
    opt_objective = "grid",
    method = "curtail",
    window_days = 1,
    window_start_hour = 6,
    energy_min = 0
  )
  ss_summary <- summarise_smart_charging_sessions(sc_results)
  expect_true(nrow(ss_summary) > 0)
})

test_that("smart charging sessions can be summarised by timecycle", {
  timecycle_summary <- summarise_timecycle_smart_charging_sessions(
    sc_results$sessions
  )
  expect_true(nrow(timecycle_summary) > 0)
  expect_true(all(
    c("profile", "group", "subgroup", "n_sessions", "pct") %in%
      names(timecycle_summary)
  ))
})


# Plots -------------------------------------------------------------------

test_that("smart charging results are plotted", {
  plot <- plot_smart_charging(sc_results, sessions = sessions)
  expect_equal(class(plot), c("dygraphs", "htmlwidget"))
})

test_that("smart charging results are plotted with native `plot` function, without setpoint", {
  plot <- plot(sc_results, sessions = sessions, show_setpoint = FALSE)
  expect_equal(class(plot), c("dygraphs", "htmlwidget"))
})

test_that("smart charging results are plotted by `FlexType`", {
  plot <- plot_smart_charging(sc_results, sessions = sessions, by = "FlexType")
  expect_equal(class(plot), c("dygraphs", "htmlwidget"))
})

test_that("view_smart_charging_logs errors when there are no log messages", {
  expect_error(
    view_smart_charging_logs(list(log = list())),
    "no log messages"
  )
})


# Energy ratios -----------------------------------------------------------

test_that("results at the default energy ratios are unchanged by the energy range", {
  # Baseline captured with exactly these calls on 1.6.0 (commit 77312479),
  # before `energy_min` entered the setpoint LP and `energy_max` existed
  # (1.7.0 left every one of them byte-identical), and re-captured on 1.8.0:
  # two sessions of this fixture cross the 06:00 boundary and are now split,
  # and the 95% arrival-time band no longer leaves sessions unscheduled (34
  # instead of 38 sessions outside every window's schedule for the grid
  # scenarios, 20 instead of 27 for the capacity ones). Setpoints moved by up
  # to 10 kW in a slot with the energy conserved; only `none_curtail_cap`,
  # which never applied the band, is unchanged since 1.6.0. Every scenario
  # runs at the default ratios (or, for `grid_curtail_min0`, with a range that
  # a strictly convex objective without a binding capacity must ignore), so
  # the range machinery must leave the setpoints byte-identical.
  #
  # The scheduler is compared with a tolerance: it rounds to 2 decimals at
  # several points and a rounding tie flips on floating-point dust, so the same
  # setpoints can schedule 0.01-0.02 kW differently between environments (seen
  # under covr instrumentation, with identical setpoints). That noise predates
  # this change and is not what this test guards.
  golden <- readRDS(test_path("golden-energy-range-defaults.rds"))
  scheduler_tol <- 0.05

  expect_same_run <- function(actual, expected, key) {
    expect_equal(actual$setpoints, expected$setpoints, label = paste(key, "setpoints"))

    demand_diff <- max(abs(
      as.matrix(actual$demand[-1]) - as.matrix(expected$demand[-1])
    ), na.rm = TRUE)
    expect_lte(demand_diff, scheduler_tol, label = paste(key, "demand"))

    energy_per_session <- function(sessions) {
      sessions %>%
        group_by(Session) %>%
        summarise(Energy = sum(Energy), .groups = "drop") %>%
        arrange(Session)
    }
    actual_energy <- energy_per_session(actual$sessions)
    expected_energy <- energy_per_session(expected$sessions)
    expect_equal(actual_energy$Session, expected_energy$Session, label = paste(key, "sessions"))
    expect_lte(
      max(abs(actual_energy$Energy - expected_energy$Energy)),
      scheduler_tol,
      label = paste(key, "energy per session")
    )
  }

  golden_opt_data <- tibble(
    datetime = sessions_demand$datetime,
    production = 0,
    price_imported = 0.1,
    price_exported = 0
  )
  run <- function(opt_data_run, ...) {
    r <- suppressMessages(smart_charging(sessions, opt_data_run, ...))
    list(setpoints = r$setpoints, demand = r$demand, sessions = r$sessions)
  }

  cases <- list(
    grid_curtail = list(
      golden_opt_data,
      opt_objective = "grid", method = "curtail",
      window_days = 1, window_start_hour = 5
    ),
    grid_none = list(
      golden_opt_data,
      opt_objective = "grid", method = "none",
      window_days = 1, window_start_hour = 5
    ),
    grid_postpone = list(
      golden_opt_data,
      opt_objective = "grid", method = "postpone",
      window_days = 1, window_start_hour = 6
    ),
    combined_curtail = list(
      golden_opt_data,
      opt_objective = 0.5, method = "curtail",
      window_days = 1, window_start_hour = 6,
      responsive = list(Workday = list(Worktime = 0.9)),
      charging_power_min = 0.5
    ),
    grid_curtail_min0 = list(
      golden_opt_data,
      opt_objective = "grid", method = "curtail",
      window_days = 1, window_start_hour = 5, energy_min = 0
    ),
    capacity_curtail = list(
      mutate(golden_opt_data, import_capacity = 50),
      opt_objective = "capacity", method = "curtail",
      window_days = 1, window_start_hour = 0,
      responsive = list(Workday = list(Worktime = 1))
    ),
    capacity_curtail_tight = list(
      mutate(golden_opt_data, import_capacity = 15),
      opt_objective = "capacity", method = "curtail",
      window_days = 1, window_start_hour = 0,
      responsive = list(Workday = list(Worktime = 1))
    ),
    grid_curtail_tight = list(
      mutate(golden_opt_data, import_capacity = 15),
      opt_objective = "grid", method = "curtail",
      window_days = 1, window_start_hour = 0,
      responsive = list(Workday = list(Worktime = 1))
    ),
    none_curtail_cap = list(
      mutate(golden_opt_data, grid_capacity = 50),
      opt_objective = "none", method = "curtail",
      window_days = 1, window_start_hour = 0,
      responsive = list(Workday = list(Worktime = 1))
    )
  )

  for (key in names(cases)) {
    expect_same_run(do.call(run, cases[[key]]), golden[[key]], key)
  }
})

# A synthetic fleet whose sessions all sit inside one optimization window
# (18:00 to 06:00, windows starting at 06:00), so every session is responsive
# and the energy arithmetic is exact: 20 kWh at 11 kW over a 12 h connection.
# `sessions_per_day` controls whether the fleet fits under a 3 kW capacity:
# 2 sessions need 40 kWh against 36 kWh of room (3 kW x 12 h), 3 sessions need
# 60 kWh — unreachable, whatever the schedule.
synthetic_fleet <- function(sessions_per_day, days = 0:4) {
  do.call(rbind, lapply(days, function(day) {
    tibble(
      Session = paste0("S", day, "_", seq_len(sessions_per_day)),
      Timecycle = "Weekday",
      Profile = "Home",
      ConnectionStartDateTime = lubridate::ymd_hms(
        "2024-01-10 18:00:00", tz = "UTC"
      ) +
        lubridate::days(day) +
        lubridate::minutes(15 * (seq_len(sessions_per_day) - 1)),
      ConnectionHours = 12,
      Power = 11,
      Energy = 20
    )
  })) %>%
    mutate(
      ChargingHours = Energy / Power,
      ChargingStartDateTime = ConnectionStartDateTime,
      ChargingEndDateTime = ChargingStartDateTime +
        lubridate::minutes(round(ChargingHours * 60)),
      ConnectionEndDateTime = ConnectionStartDateTime +
        lubridate::hours(ConnectionHours),
      FlexibilityHours = ConnectionHours - ChargingHours
    )
}

fleet_dttm_seq <- seq(
  lubridate::ymd_hms("2024-01-10 00:00:00", tz = "UTC"),
  lubridate::ymd_hms("2024-01-16 05:45:00", tz = "UTC"),
  by = "15 min"
)

fleet_opt_data <- function(capacity_kw) {
  tibble(
    datetime = fleet_dttm_seq,
    production = 0,
    static = 0,
    import_capacity = capacity_kw,
    export_capacity = capacity_kw
  )
}

fleet_smart_charging <- function(fleet, opt_data, ...) {
  smart_charging(
    fleet,
    opt_data,
    window_days = 1,
    window_start_hour = 6,
    responsive = list(Weekday = list(Home = 1)),
    power_th = 0,
    charging_power_min = 0,
    ...
  )
}

fleet_energy_kwh <- function(profiles) {
  sum(rowSums(profiles[-1])) * 15 / 60
}

test_that("energy_max caps every session at its share of the requirement (curtail)", {
  fleet <- synthetic_fleet(2)
  # `energy_min` defaults to 1, so a ceiling below it must come with a floor.
  sc <- suppressMessages(fleet_smart_charging(
    fleet, fleet_opt_data(50),
    opt_objective = "capacity", method = "curtail",
    energy_min = 0.8, energy_max = 0.8
  ))

  charged <- summarise_energy_charged(sc, fleet)
  expect_true(all(charged$PctEnergyCharged >= 79 & charged$PctEnergyCharged <= 81))
  expect_equal(sum(sc$sessions$Energy), 0.8 * sum(fleet$Energy), tolerance = 0.01)

  # The setpoint carries the same energy the scheduler delivers.
  expect_equal(
    fleet_energy_kwh(sc$setpoints),
    fleet_energy_kwh(sc$demand),
    tolerance = 0.01
  )
})

test_that("energy_max also caps postpone and interrupt", {
  fleet <- synthetic_fleet(2)
  for (method in c("postpone", "interrupt")) {
    sc <- suppressMessages(fleet_smart_charging(
      fleet, fleet_opt_data(50),
      opt_objective = "grid", method = method,
      energy_min = 0.8, energy_max = 0.8
    ))
    charged <- summarise_energy_charged(sc, fleet)
    expect_true(
      all(charged$PctEnergyCharged >= 79 & charged$PctEnergyCharged <= 81),
      label = method
    )
  }
})

test_that("with method 'none' the setpoint itself carries the energy_max target", {
  fleet <- synthetic_fleet(2)
  static_kwh <- fleet_energy_kwh(get_demand(
    evsim::adapt_charging_features(fleet, time_resolution = 15),
    fleet_dttm_seq
  ))

  sc <- suppressMessages(fleet_smart_charging(
    fleet, fleet_opt_data(50),
    opt_objective = "grid", method = "none",
    energy_min = 0.8, energy_max = 0.8
  ))

  expect_equal(sc$demand, sc$setpoints)
  expect_equal(fleet_energy_kwh(sc$demand), 0.8 * static_kwh, tolerance = 0.01)
})

test_that("energy_min = 0 holds an unreachable capacity instead of relaxing it", {
  fleet <- synthetic_fleet(3)
  capacity_kw <- 3

  expect_no_message(
    sc <- fleet_smart_charging(
      fleet, fleet_opt_data(capacity_kw),
      opt_objective = "capacity", method = "curtail", energy_min = 0
    ),
    message = "Relaxing grid capacity"
  )

  expect_lte(max(rowSums(sc$setpoints[-1])), capacity_kw + 0.01)
  expect_lte(max(rowSums(sc$demand[-1])), capacity_kw + 0.01)

  # 3 kW over the 12 h the fleet is connected is 36 of the 60 kWh a day.
  pct <- sum(sc$sessions$Energy) / sum(fleet$Energy) * 100
  expect_gte(pct, 50)
  expect_lte(pct, 62)
})

test_that("energy_min below what fits keeps the capacity and delivers at least the minimum", {
  fleet <- synthetic_fleet(3)
  capacity_kw <- 3

  expect_no_message(
    sc <- fleet_smart_charging(
      fleet, fleet_opt_data(capacity_kw),
      opt_objective = "capacity", method = "curtail", energy_min = 0.5
    ),
    message = "Relaxing grid capacity"
  )

  expect_lte(max(rowSums(sc$setpoints[-1])), capacity_kw + 0.01)
  expect_gte(sum(sc$sessions$Energy) / sum(fleet$Energy), 0.5)
})

test_that("energy_min above what fits relaxes the capacity to the minimum-energy profile only", {
  fleet <- synthetic_fleet(3)
  capacity_kw <- 3

  # 80% of 60 kWh is 48 kWh a day against 36 kWh of room: the minimum wins.
  expect_message(
    sc_min <- fleet_smart_charging(
      fleet, fleet_opt_data(capacity_kw),
      opt_objective = "capacity", method = "curtail", energy_min = 0.8
    ),
    "minimum energy does not fit"
  )
  sc_full <- suppressMessages(fleet_smart_charging(
    fleet, fleet_opt_data(capacity_kw),
    opt_objective = "capacity", method = "curtail", energy_min = 1
  ))

  pct_min <- sum(sc_min$sessions$Energy) / sum(fleet$Energy)
  expect_gte(pct_min, 0.79)
  # The capacity is exceeded, but by less than at 100%: less energy is forced
  # through it.
  expect_lt(fleet_energy_kwh(sc_min$setpoints), fleet_energy_kwh(sc_full$setpoints))
  expect_lte(
    max(rowSums(sc_min$setpoints[-1])),
    max(rowSums(sc_full$setpoints[-1])) + 0.01
  )
})

test_that("without optimization the capacity is only inflated as far as energy_min requires", {
  # 4 sessions a day need 80 kWh; a 3 kW capacity over the 24 h window offers
  # 72, so the "none" objective has to inflate the capacity to fit them all.
  fleet <- synthetic_fleet(4)
  opt_data <- fleet_opt_data(3) %>% select(-import_capacity, -export_capacity)
  opt_data$grid_capacity <- 3

  sc_hard <- suppressMessages(fleet_smart_charging(
    fleet, opt_data,
    opt_objective = "none", method = "curtail", energy_min = 0
  ))
  sc_full <- suppressMessages(fleet_smart_charging(
    fleet, opt_data,
    opt_objective = "none", method = "curtail"
  ))

  expect_lte(max(rowSums(sc_hard$setpoints[-1])), 3 + 0.01)
  expect_gt(max(rowSums(sc_full$setpoints[-1])), 3 + 0.01)
})

# Window boundaries -------------------------------------------------------

# A session that straddles the 06:00 window boundary the way the one that
# motivated the split did: arrives 05:15, stays until 14:34, needs 16.28 kWh
# at 11 kW (so its unmanaged charge would end 06:43, past the window it starts
# in). One per day; `days` selects which days, so a test can avoid the first
# day, whose 05:15 part falls before the first window of the sequence.
straddling_fleet <- function(days = 1:4, profile = "Worktime") {
  do.call(rbind, lapply(days, function(day) {
    tibble(
      Session = paste0("X", day),
      Timecycle = "Weekday",
      Profile = profile,
      ConnectionStartDateTime = lubridate::ymd_hms(
        "2024-01-10 05:15:00", tz = "UTC"
      ) + lubridate::days(day),
      ConnectionHours = 9.32,
      Power = 11,
      Energy = 16.28
    )
  }))
}

first_window_start <- lubridate::ymd_hms("2024-01-10 06:00:00", tz = "UTC")

test_that("a session straddling the window boundary is split and scheduled in both windows", {
  fleet <- straddling_fleet()
  capacity_kw <- 3

  sc <- suppressMessages(smart_charging(
    fleet, fleet_opt_data(capacity_kw),
    opt_objective = "capacity", method = "curtail",
    window_days = 1, window_start_hour = 6,
    responsive = list(Weekday = list(Worktime = 1)),
    power_th = 0, charging_power_min = 0, energy_min = 0
  ))

  # The 45 minutes before the boundary would carry 1.3 kWh — less than one
  # slot at 11 kW — so that sliver is folded into the part after the boundary:
  # the session moves whole into the window where its flexibility is, and is
  # scheduled there. No session is left with the NA responsiveness that used to
  # mean "charged unmanaged at 11 kW".
  starts <- sc$sessions %>%
    group_by(Session) %>%
    summarise(start = min(ConnectionStartDateTime), parts = n_distinct(Part), .groups = "drop")
  expect_true(all(starts$parts == 1))
  expect_true(all(format(starts$start, "%H:%M") == "06:00"))
  expect_false(any(is.na(sc$sessions$Responsive)))
  expect_true(all(sc$sessions$Responsive))

  # The cap holds through the boundary, 05:15-06:43 included, from the first
  # window on (the day-1 part before the first window is outside every window).
  in_windows <- sc$demand$datetime >= first_window_start
  expect_lte(max(rowSums(sc$demand[in_windows, -1])), capacity_kw + 0.01)
  expect_lte(max(rowSums(sc$setpoints[in_windows, -1])), capacity_kw + 0.01)

  # And the energy is delivered: 15 kWh in 8.5 hours under a 3 kW cap fits,
  # so the split turns an unmanaged 11 kW spike into a fully served session.
  charged <- summarise_energy_charged(sc, fleet)
  expect_true(all(charged$PctEnergyCharged >= 95))
})

test_that("the split conserves the energy at the default ratios", {
  fleet <- straddling_fleet()
  sc <- suppressMessages(smart_charging(
    fleet, fleet_opt_data(50),
    opt_objective = "grid", method = "curtail",
    window_days = 1, window_start_hour = 6,
    responsive = list(Weekday = list(Worktime = 1)),
    power_th = 0, charging_power_min = 0
  ))
  delivered <- sc$sessions %>%
    group_by(Session) %>%
    summarise(Energy = sum(Energy), .groups = "drop")
  expect_equal(
    delivered$Energy,
    fleet$Energy[match(delivered$Session, fleet$Session)],
    tolerance = 0.01
  )
  # The sliver before the boundary was folded forward, so the whole session
  # sits in the window it is connected in.
  x2 <- sc$sessions %>% filter(Session == "X2")
  expect_equal(format(min(x2$ConnectionStartDateTime), "%H:%M"), "06:00")
  expect_equal(sum(x2$Energy), 16.28, tolerance = 0.01)
})

test_that("parts share the energy by connection time when both can fill a slot", {
  # 22:00 -> 10:00 (12 h) across the 06:00 boundary, 11 kW, 33 kWh: 8 h and
  # 4 h of connection, so 22 and 11 kWh — both well above one slot (2.75 kWh).
  fleet <- tibble(
    Session = "HALF",
    Timecycle = "Weekday",
    Profile = "Home",
    ConnectionStartDateTime = lubridate::ymd_hms("2024-01-11 22:00:00", tz = "UTC"),
    ConnectionHours = 12,
    Power = 11,
    Energy = 33
  )
  sc <- suppressMessages(smart_charging(
    fleet, fleet_opt_data(50),
    opt_objective = "grid", method = "curtail",
    window_days = 1, window_start_hour = 6,
    responsive = list(Weekday = list(Home = 1)),
    power_th = 0, charging_power_min = 0
  ))
  by_part <- sc$sessions %>%
    group_by(Part) %>%
    summarise(
      start = min(ConnectionStartDateTime),
      energy = sum(Energy),
      .groups = "drop"
    )
  expect_equal(by_part$Part, 1:2)
  expect_equal(format(by_part$start, "%H:%M"), c("22:00", "06:00"))
  expect_equal(by_part$energy, c(22, 11), tolerance = 0.01)
})

test_that("a connection spanning three windows becomes three parts", {
  fleet <- tibble(
    Session = "LONG",
    Timecycle = "Weekday",
    Profile = "Home",
    ConnectionStartDateTime = lubridate::ymd_hms("2024-01-11 20:00:00", tz = "UTC"),
    ConnectionHours = 40, # 20:00 -> 12:00 two days later: crosses 06:00 twice
    Power = 11,
    Energy = 40
  )
  sc <- suppressMessages(smart_charging(
    fleet, fleet_opt_data(50),
    opt_objective = "grid", method = "curtail",
    window_days = 1, window_start_hour = 6,
    responsive = list(Weekday = list(Home = 1)),
    power_th = 0, charging_power_min = 0
  ))
  expect_equal(sort(unique(sc$sessions$Part)), 1:3)
  expect_equal(sum(sc$sessions$Energy), 40, tolerance = 0.02)
})

test_that("a session whose charging ends exactly on the boundary is responsive", {
  # 22:00 -> 06:00, 87 kWh at 11 kW: charging ends 05:54, inside the window
  # but after its last slot (05:45), which used to exclude it.
  fleet <- tibble(
    Session = "EDGE",
    Timecycle = "Weekday",
    Profile = "Home",
    ConnectionStartDateTime = lubridate::ymd_hms("2024-01-11 22:00:00", tz = "UTC"),
    ConnectionHours = 8,
    Power = 11,
    Energy = 87
  )
  sc <- suppressMessages(smart_charging(
    fleet, fleet_opt_data(50),
    opt_objective = "grid", method = "curtail",
    window_days = 1, window_start_hour = 6,
    responsive = list(Weekday = list(Home = 1)),
    power_th = 0, charging_power_min = 0
  ))
  expect_equal(unique(sc$sessions$Part), 1L)
  expect_true(all(sc$sessions$Responsive))
})

test_that("a straddler's tail is not lost when its profile has other sessions in the next window", {
  # Before the split the window a straddler spilled into overwrote the
  # profile's demand with its own sessions' demand, dropping the tail.
  fleet <- bind_rows(
    straddling_fleet(days = 1:4, profile = "Home"),
    synthetic_fleet(1) # Home sessions 18:00 -> 06:00 in every window
  )
  sc <- suppressMessages(smart_charging(
    fleet, fleet_opt_data(50),
    opt_objective = "grid", method = "curtail",
    window_days = 1, window_start_hour = 6,
    responsive = list(Weekday = list(Home = 1)),
    power_th = 0, charging_power_min = 0
  ))
  expect_equal(fleet_energy_kwh(sc$demand), sum(fleet$Energy), tolerance = 0.005)
})

test_that("an unusual arrival time no longer excludes a session from scheduling", {
  # Eight tightly clustered evening arrivals used to make both a 22:00 arrival
  # and a 06:00 carried-over part outliers of the profile's 95% band, left to
  # charge unmanaged. The band is gone: both parts are scheduled.
  evening <- do.call(rbind, lapply(0:4, function(day) {
    tibble(
      Session = paste0("E", day, "_", 1:8),
      Timecycle = "Weekday",
      Profile = "Home",
      ConnectionStartDateTime = lubridate::ymd_hms(
        "2024-01-10 18:00:00", tz = "UTC"
      ) +
        lubridate::days(day) +
        lubridate::minutes(15 * (0:7)),
      ConnectionHours = 4,
      Power = 11,
      Energy = 8
    )
  }))
  # 22:00 -> 10:00, 33 kWh: the 06:00 -> 10:00 part carries 11 kWh.
  straddler <- tibble(
    Session = "LATE",
    Timecycle = "Weekday",
    Profile = "Home",
    ConnectionStartDateTime = lubridate::ymd_hms("2024-01-11 22:00:00", tz = "UTC"),
    ConnectionHours = 12,
    Power = 11,
    Energy = 33
  )
  fleet <- bind_rows(evening, straddler)
  sc <- suppressMessages(smart_charging(
    fleet, fleet_opt_data(50),
    opt_objective = "grid", method = "curtail",
    window_days = 1, window_start_hour = 6,
    responsive = list(Weekday = list(Home = 1)),
    power_th = 0, charging_power_min = 0
  ))
  late <- sc$sessions %>% filter(Session == "LATE")
  expect_equal(sort(unique(late$Part)), 1:2)
  expect_true(all(late$Responsive))
})

test_that("a carried-over part keeps its own time cycle's responsiveness", {
  # A Friday Commuters session whose connection reaches into Saturday's
  # window, where Commuters is not a configured profile. Looked up under the
  # window's cycle it would find nothing and stay unscheduled; looked up under
  # its own cycle it is responsive.
  friday <- tibble(
    Session = "FRI",
    Timecycle = "Friday",
    Profile = "Commuters",
    ConnectionStartDateTime = lubridate::ymd_hms("2024-01-12 22:00:00", tz = "UTC"),
    ConnectionHours = 12,
    Power = 11,
    Energy = 33
  )
  saturday <- tibble(
    Session = paste0("SAT", 1:3),
    Timecycle = "Saturday",
    Profile = "Home",
    ConnectionStartDateTime = lubridate::ymd_hms("2024-01-13 10:00:00", tz = "UTC") +
      lubridate::hours(0:2),
    ConnectionHours = 6,
    Power = 11,
    Energy = 20
  )
  fleet <- bind_rows(friday, saturday)
  sc <- suppressMessages(smart_charging(
    fleet, fleet_opt_data(50),
    opt_objective = "grid", method = "curtail",
    window_days = 1, window_start_hour = 6,
    responsive = list(
      Friday = list(Commuters = 1),
      Saturday = list(Home = 1)
    ),
    power_th = 0, charging_power_min = 0
  ))
  fri <- sc$sessions %>% filter(Session == "FRI")
  expect_equal(sort(unique(fri$Part)), 1:2)
  expect_true(all(fri$Responsive))
  expect_true(all(sc$sessions$Responsive[sc$sessions$Session != "FRI"]))
})

test_that("the energy ratios are validated", {
  fleet <- synthetic_fleet(2)
  expect_error(
    fleet_smart_charging(
      fleet, fleet_opt_data(50),
      opt_objective = "grid", method = "curtail",
      energy_min = 0.9, energy_max = 0.5
    ),
    "cannot be higher"
  )
  expect_error(
    fleet_smart_charging(
      fleet, fleet_opt_data(50),
      opt_objective = "grid", method = "curtail", energy_max = 0
    ),
    "energy_max"
  )
  expect_error(
    fleet_smart_charging(
      fleet, fleet_opt_data(50),
      opt_objective = "grid", method = "curtail", energy_min = 1.5
    ),
    "energy_min"
  )
  expect_error(
    schedule_sessions(
      fleet,
      tibble(datetime = fleet_dttm_seq, setpoint = 50),
      method = "curtail",
      energy_min = 0.5, energy_max = 0.2,
      show_progress = FALSE
    ),
    "cannot be higher"
  )
})
