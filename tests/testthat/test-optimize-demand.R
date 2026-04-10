library(dplyr)

opt_data <- flextools::energy_profiles %>%
  filter(lubridate::isoweek(datetime) == 18) %>%
  rename(
    production = "solar"
  )

test_that("Get error when missing `opt_data`", {
  expect_error(
    optimize_demand(
      opt_data = NULL,
      opt_objective = "grid",
      direction = "forward",
      flex_window_hours = 6,
      time_horizon = 12
    )
  )
})

test_that("Get error when missing `flexible` column in `opt_data`", {
  expect_error(
    optimize_demand(
      opt_data = opt_data,
      opt_objective = "grid",
      direction = "forward",
      flex_window_hours = 6,
      time_horizon = 12
    )
  )
})

test_that("Get message when missing `production` column in `opt_data`", {
  expect_warning(
    opt_data %>%
      mutate(
        flexible = building
      ) %>%
      select(-production) %>%
      optimize_demand(
        opt_objective = "grid",
        direction = "forward",
        flex_window_hours = 6,
        time_horizon = 12
      )
  )
})

test_that("Get message when missing `flex_window_hours` is too high", {
  expect_message(
    opt_data %>%
      mutate(
        flexible = building
      ) %>%
      optimize_demand(
        opt_objective = "grid",
        direction = "forward",
        window_days = 1,
        flex_window_hours = 48
      )
  )
})

test_that("Get error when `direction` is mispelled", {
  expect_error(
    opt_data %>%
      mutate(
        flexible = building
      ) %>%
      optimize_demand(
        opt_objective = "grid",
        direction = "forwards",
        flex_window_hours = 6,
        time_horizon = 12
      )
  )
})

test_that("optimization of demand works for cost objective and forward direction", {
  opt_building <- opt_data %>%
    mutate(
      flexible = building
    ) %>%
    optimize_demand(
      opt_objective = "cost",
      direction = "forward",
      flex_window_hours = 6,
      time_horizon = 12
    )

  expect_type(opt_building, "double")
})

test_that("optimization of demand works for combined objective and forward direction", {
  opt_building <- opt_data %>%
    mutate(
      flexible = building
    ) %>%
    optimize_demand(
      opt_objective = 0.5,
      direction = "forward",
      flex_window_hours = 6,
      time_horizon = 12
    )

  expect_type(opt_building, "double")
})

test_that("optimization of demand works for grid objective and backward direction and a window of 2 days", {
  opt_building <- opt_data %>%
    mutate(
      flexible = building
    ) %>%
    optimize_demand(
      opt_objective = "grid",
      direction = "backward",
      window_days = 2
    )

  expect_type(opt_building, "double")
})

test_that("error when `opt_objective` is wrong in optimization demand", {
  expect_error(
    opt_data %>%
      mutate(
        flexible = building
      ) %>%
      optimize_demand(
        opt_objective = "grids",
        direction = "backward",
        window_days = 2
      )
  )
})
