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
    include_log = T,
    show_progress = T
  )
  expect_true(
    length(sc_results$log[[1]]) > 0
  )
  expect_type(sc_results, "list")
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
