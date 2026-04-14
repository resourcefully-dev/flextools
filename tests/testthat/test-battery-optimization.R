library(dplyr)

opt_data <- flextools::energy_profiles |>
  filter(lubridate::isoweek(datetime) == 18) |>
  select(
    datetime,
    production = solar,
    static = building,
    price_exported,
    price_imported
  )


test_that("battery optimization works for grid objective", {
  opt_battery <- opt_data |>
    add_battery_optimization_qp(
      opt_objective = "grid",
      Bcap = 50,
      Bc = 4,
      Bd = 4,
      window_start_hour = 5
    )

  expect_type(opt_battery, "double")
})

test_that("battery optimization qp helper returns a feasible battery profile", {
  opt_battery_qp <- opt_data |>
    add_battery_optimization_qp(
      Bcap = 50,
      Bc = 4,
      Bd = 4,
      window_start_hour = 5
    )

  expect_type(opt_battery_qp, "double")
  expect_equal(length(opt_battery_qp), nrow(opt_data))

  storage <- get_storage_level(
    opt_battery_qp,
    time_resolution = 15,
    init = 0
  )

  expect_lte(max(opt_battery_qp), 4 + 1e-6)
  expect_gte(min(opt_battery_qp), -4 - 1e-6)
  expect_lte(max(storage), 50 + 1e-6)
  expect_gte(min(storage), -1e-6)
  expect_equal(tail(storage, 1), 0, tolerance = 1e-6)
})

test_that("battery optimization qp returns zero profile when bounds are infeasible", {
  opt_data_infeasible <- opt_data |>
    mutate(
      production = 0,
      static = 10,
      import_capacity = 0,
      export_capacity = 0
    )

  opt_battery_qp <- opt_data_infeasible |>
    add_battery_optimization_qp(
      Bcap = 50,
      Bc = 1,
      Bd = 1,
      window_start_hour = 5
    )

  expect_equal(
    as.numeric(opt_battery_qp),
    rep(0, nrow(opt_data_infeasible))
  )
})


test_that("error when `opt_objective` is wrong in battery optimization", {
  expect_error(
    opt_data |>
      add_battery_optimization_qp(
        opt_objective = "grids",
        Bcap = 50,
        Bc = 4,
        Bd = 4,
        window_start_hour = 5
      )
  )
})

test_that("battery optimization works with constrained import capacity", {
  opt_data_batt <- opt_data |>
    mutate(
      production = .data$production * 0,
      static = .data$static * 100,
      import_capacity = rep(
        c(rep(500, 9 * 4), rep(150, 12 * 4), rep(500, 3 * 4)),
        7
      )
    )

  # opt_data_batt |>
  #   plot_ts()

  opt_battery_vct <- opt_data_batt |>
    add_battery_optimization_qp(
      opt_objective = "grid",
      Bcap = 5000,
      Bc = 5000,
      Bd = 5000,
      window_start_hour = 0
    )

  opt_battery <- opt_data_batt |>
    mutate(
      battery = opt_battery_vct,
      consumption = static + battery
    ) |>
    get_energy_balance()

  # opt_battery |>
  #   plot_ts()

  expect_false(
    any(round(opt_battery$import_capacity - opt_battery$imported) < 0) # There's still some error
  )
})

test_that("battery optimization works with constrained import capacity and 'capacity' objective", {
  opt_data_batt <- opt_data |>
    mutate(
      production = .data$production * 0,
      static = .data$static * 100,
      import_capacity = 350
    )

  opt_battery_vct <- opt_data_batt |>
    add_battery_optimization_qp(
      opt_objective = "capacity",
      Bcap = 5000,
      Bc = 500,
      Bd = 500,
      window_start_hour = 0
    )

  opt_battery <- opt_data_batt |>
    mutate(
      battery = opt_battery_vct,
      consumption = static + battery
    ) |>
    get_energy_balance()

  # opt_battery |>
  #   plot_ts() |>
  #   dygraphs::dyLegend(show = "onmouseover")

  expect_false(
    any(round(opt_battery$import_capacity - opt_battery$imported) < 0) # There's still some error
  )
})
