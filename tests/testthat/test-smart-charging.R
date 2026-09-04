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
  # Baseline captured on 1.6.0 (commit 77312479) with exactly these calls,
  # before `energy_min` entered the setpoint LP and `energy_max` existed. Every
  # scenario runs at the default ratios (or, for `grid_curtail_min0`, with a
  # range that a strictly convex objective without a binding capacity must
  # ignore), so the range machinery must leave the setpoints byte-identical.
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
