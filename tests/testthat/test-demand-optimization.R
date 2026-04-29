library(dplyr)
devtools::load_all()

# Time benchmark for a whole year:
# - grid (1): 2.43s
# - cost (0): 5.78s
# - combined (0.1): 7.45s
# - combined (0.5): 4.43s
# compare_demand_year()

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


# Time benchmarking for demand optimization ----------------------
test_demand_year <- function(opt_objective) {
  message(sprintf(
    "Testing demand optimization for objective: %s",
    opt_objective
  ))

  timefully::tic()
  O <- flextools::energy_profiles |>
    rename(
      production = "solar",
      flexible = building
    ) |>
    mutate(
      load_capacity = max(flextools::energy_profiles$building)
    ) |>
    optimize_demand(
      opt_objective = opt_objective,
      direction = "forward"
    )
  time <- timefully::toc()

  cost <- evaluate_cost(
    flextools::energy_profiles |>
      rename(
        production = "solar",
        static = building
      ),
    O
  )

  list(
    profile = O,
    time = time,
    cost = cost
  )
}
compare_demand_year <- function() {
  res_grid <- test_demand_year("grid")
  res_cost <- test_demand_year("cost")
  res_combined_0.1 <- test_demand_year(0.1)
  res_combined_0.5 <- test_demand_year(0.5)

  kpis <- purrr::map(
    purrr::set_names(c(
      "grid",
      "cost",
      "combined_0.1",
      "combined_0.5"
    )),
    ~ tibble(
      time = get(paste0("res_", .x))$time,
      cost = get(paste0("res_", .x))$cost
    )
  ) |>
    purrr::list_rbind(names_to = "objective")
  print(kpis)

  flextools::energy_profiles |>
    select(
      -any_of(c("price_turn_up", "price_turn_down"))
    ) |>
    mutate(
      demand_grid = res_grid$profile,
      demand_cost = res_cost$profile,
      demand_combined_0.1 = res_combined_0.1$profile,
      demand_combined_0.5 = res_combined_0.5$profile
    ) |>
    timefully::plot_ts(
      title = sprintf(
        "Benchmarking: Grid: %0.1fs, Cost: %0.1fs, 
        Combined (0.1): %0.1fs, Combined (0.5): %0.1fs",
        res_grid$time,
        res_cost$time,
        res_combined_0.1$time,
        res_combined_0.5$time
      ),
      legend_width = 200
    )
}
