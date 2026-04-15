library(dplyr)
devtools::load_all()

opt_data <- flextools::energy_profiles |>
  filter(lubridate::isoweek(datetime) == 18) |>
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
    opt_data |>
      mutate(
        flexible = building
      ) |>
      select(-production) |>
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
    opt_data |>
      mutate(
        flexible = building
      ) |>
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
    opt_data |>
      mutate(
        flexible = building
      ) |>
      optimize_demand(
        opt_objective = "grid",
        direction = "forwards",
        flex_window_hours = 6,
        time_horizon = 12
      )
  )
})

test_that("optimization of demand works for cost objective and forward direction", {
  opt_building <- expect_no_message(
    opt_data |>
      mutate(
        flexible = building
      ) |>
      optimize_demand(
        opt_objective = "cost",
        direction = "forward",
        flex_window_hours = 6,
        time_horizon = 12
      )
  )

  expect_type(opt_building, "double")
})

test_that("optimization of demand works for combined objective and forward direction", {
  opt_building <- expect_no_message(
    opt_data |>
      mutate(
        flexible = building
      ) |>
      optimize_demand(
        opt_objective = 0.5,
        direction = "forward",
        flex_window_hours = 6,
        time_horizon = 12
      )
  )

  expect_type(opt_building, "double")
})

test_that("numeric demand objective endpoints reuse pure objective formulations", {
  opt_grid_chr <- opt_data |>
    mutate(
      flexible = building
    ) |>
    optimize_demand(
      opt_objective = "grid",
      direction = "forward",
      flex_window_hours = 6,
      time_horizon = 12
    )

  opt_grid_num <- expect_no_message(
    opt_data |>
      mutate(
        flexible = building
      ) |>
      optimize_demand(
        opt_objective = 1,
        direction = "forward",
        flex_window_hours = 6,
        time_horizon = 12
      )
  )

  opt_cost_chr <- opt_data |>
    mutate(
      flexible = building
    ) |>
    optimize_demand(
      opt_objective = "cost",
      direction = "forward",
      flex_window_hours = 6,
      time_horizon = 12
    )

  opt_cost_num <- expect_no_message(
    opt_data |>
      mutate(
        flexible = building
      ) |>
      optimize_demand(
        opt_objective = 0,
        direction = "forward",
        flex_window_hours = 6,
        time_horizon = 12
      )
  )

  expect_equal(
    as.numeric(opt_grid_num),
    as.numeric(opt_grid_chr),
    tolerance = 1e-6
  )
  expect_equal(
    as.numeric(opt_cost_num),
    as.numeric(opt_cost_chr),
    tolerance = 1e-6
  )
})

test_that("optimization of demand works for grid objective and backward direction and a window of 2 days", {
  opt_building <- opt_data |>
    mutate(
      flexible = building
    ) |>
    optimize_demand(
      opt_objective = "grid",
      direction = "backward",
      window_days = 2
    )

  expect_type(opt_building, "double")
})

test_that("error when `opt_objective` is wrong in optimization demand", {
  expect_error(
    opt_data |>
      mutate(
        flexible = building
      ) |>
      optimize_demand(
        opt_objective = "grids",
        direction = "backward",
        window_days = 2
      )
  )
})


expect_no_simultaneous_demand_grid_flows <- function(
  profile,
  tolerance = 1e-5
) {
  imported <- attr(profile, "import")
  exported <- attr(profile, "export")

  expect_false(any(imported > tolerance & exported > tolerance))
}


test_that("demand cost and combined windows do not import and export simultaneously", {
  week_data <- flextools::energy_profiles |>
    filter(lubridate::isoweek(datetime) == 18)
  start_idx <- which(
    format(week_data$datetime, "%Y-%m-%d %H:%M:%S") == "2023-05-03 05:00:00"
  )[1]
  window_data <- week_data |>
    slice(start_idx:(start_idx + 96 - 1))

  import_capacity <- rep(
    max(window_data$building, na.rm = TRUE),
    nrow(window_data)
  )
  export_capacity <- rep(
    max(window_data$solar, na.rm = TRUE),
    nrow(window_data)
  )

  cost_profile <- minimize_cost_window(
    G = window_data$solar,
    LF = window_data$building,
    LS = rep(0, nrow(window_data)),
    PI = window_data$price_imported,
    PE = window_data$price_exported,
    PTD = rep(0, nrow(window_data)),
    PTU = rep(0, nrow(window_data)),
    direction = "forward",
    time_horizon = 12,
    LFmax = rep(Inf, nrow(window_data)),
    import_capacity = import_capacity,
    export_capacity = export_capacity,
    lambda = 0
  )

  combined_profile <- optimize_demand_window(
    G = window_data$solar,
    LF = window_data$building,
    LS = rep(0, nrow(window_data)),
    PI = window_data$price_imported,
    PE = window_data$price_exported,
    PTD = rep(0, nrow(window_data)),
    PTU = rep(0, nrow(window_data)),
    direction = "forward",
    time_horizon = 12,
    LFmax = rep(Inf, nrow(window_data)),
    import_capacity = import_capacity,
    export_capacity = export_capacity,
    w = 0.5,
    lambda = 0
  )

  expect_no_simultaneous_demand_grid_flows(cost_profile)
  expect_no_simultaneous_demand_grid_flows(combined_profile)
})
