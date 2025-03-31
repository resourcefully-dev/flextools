library(dplyr)

# Use first 50 sessions
sessions <- evsim::california_ev_sessions_profiles %>%
  slice_head(n = 50) %>%
  evsim::adapt_charging_features(time_resolution = 15)
sessions_demand <- evsim::get_demand(sessions, resolution = 15)

# Don't require any other variable than datetime, since we don't
# care about local generation (just peak shaving objective)
opt_data <- tibble(
  datetime = sessions_demand$datetime,
  production = 0
)

test_that("Get error when missing `sessions`", {
  expect_error(
    smart_charging(
      sessions = NULL, opt_data, opt_objective = "grid", method = "curtail",
      window_days = 1, window_start_hour = 6
    )
  )
})

test_that("Get error when missing `opt_data`", {
  expect_error(
    smart_charging(
      sessions = sessions, opt_data = NULL, opt_objective = "grid", method = "curtail",
      window_days = 1, window_start_hour = 6
    )
  )
})
test_that("Get error when `opt_data` has no `datetime`", {
  expect_error(
    smart_charging(
      sessions = sessions, opt_data = opt_data[2], opt_objective = "grid", method = "curtail",
      window_days = 1, window_start_hour = 6
    )
  )
})

test_that("Get error when `opt_objective` is mispelled", {
  expect_error(
    smart_charging(
      sessions, opt_data, opt_objective = "gridx", method = "curtail",
      window_days = 1, window_start_hour = 6
    )
  )
})

test_that("Get error when `method` is mispelled", {
  expect_error(
    smart_charging(
      sessions, opt_data, opt_objective = "grid", method = "curtailx",
      window_days = 1, window_start_hour = 6
    )
  )
})

test_that("smart charging works with grid objective, postpone method with power_th and multi-core", {
  sc_results <- smart_charging(
    sessions, opt_data, opt_objective = "grid", method = "postpone",
    window_days = 1, window_start_hour = 6,
    responsive = list(Workday = list(Worktime = 0.9)),
    power_th = 0.2, mc.cores = 2, include_log = TRUE
  )
  print(sc_results) # Check print as well
  expect_type(sc_results, "list")
})

test_that("smart charging works with cost objective, interrupt method and min energy of 0.5", {
  sc_results <- smart_charging(
    sessions, opt_data, opt_objective = "cost", method = "interrupt",
    window_days = 1, window_start_hour = 6,
    responsive = list(Workday = list(Worktime = 0.9)),
    energy_min = 0.5
  )
  expect_type(sc_results, "list")
})

test_that("smart charging works with combined objective, curtail method and min charging power ratio of 0.5 and no optimization", {
  opt_data <- opt_data %>%
    mutate(Workime = 0.5*max(sessions_demand$Worktime))
  sc_results <- smart_charging(
    sessions, opt_data, opt_objective = 0.5, method = "curtail",
    window_days = 1, window_start_hour = 6,
    responsive = list(Workday = list(Worktime = 0.9)),
    charging_power_min = 0.5, show_progress = TRUE
  )
  expect_type(sc_results, "list")
})

test_that("smart charging works without optimization,  curtail method and min charging power of 2kW, including logs and progress", {
  opt_data$Worktime <- 10
  sc_results <- smart_charging(
    sessions, opt_data, opt_objective = "none", method = "curtail",
    window_days = 1, window_start_hour = 6,
    responsive = list(Workday = list(Worktime = 0.9)),
    charging_power_min = 2, include_log = TRUE, show_progress = TRUE
  )
  expect_type(sc_results, "list")
})

test_that("Error when no user profiles in `opt_data` and not optimization", {
  expect_error(smart_charging(
    sessions, opt_data, opt_objective = "none", method = "curtail",
    window_days = 1, window_start_hour = 6,
    responsive = list(Workday = list(Worktime = 0.9)),
    charging_power_min = 2, include_log = TRUE, show_progress = TRUE
  ))
})
