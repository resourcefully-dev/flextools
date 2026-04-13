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
    add_battery_optimization(
      opt_objective = "grid",
      Bcap = 50,
      Bc = 4,
      Bd = 4,
      window_start_hour = 5,
      charge_eff = 0.9,
      discharge_eff = 0.9
    )

  expect_type(opt_battery, "double")
})

test_that("battery optimization works for grid objective and a whole year", {
  timefully::tic()
  opt_battery <- flextools::energy_profiles |>
    select(
      datetime,
      production = solar,
      static = building,
      price_exported,
      price_imported
    ) |>
    add_battery_optimization(
      opt_objective = 1,
      Bcap = 50,
      Bc = 4,
      Bd = 4,
      window_start_hour = 5,
      charge_eff = 0.9,
      discharge_eff = 0.9
    )
  timefully::toc()

  expect_type(opt_battery, "double")
})

test_that("battery optimization works for cost objective", {
  opt_battery <- opt_data |>
    add_battery_optimization(
      opt_objective = "cost",
      Bcap = 50,
      Bc = 4,
      Bd = 4,
      window_start_hour = 5,
      charge_eff = 0.9,
      discharge_eff = 0.9,
      lambda = 0
    )

  expect_type(opt_battery, "double")
})


test_that("battery optimization works for combined objective", {
  opt_battery <- expect_no_message(
    opt_data |>
      add_battery_optimization(
        opt_objective = 0.5,
        Bcap = 50,
        Bc = 4,
        Bd = 4,
        window_start_hour = 5,
        charge_eff = 0.9,
        discharge_eff = 0.9
      )
  )

  expect_type(opt_battery, "double")
})

test_that("numeric battery objective endpoints reuse pure objective formulations", {
  opt_grid_chr <- opt_data |>
    add_battery_optimization(
      opt_objective = "grid",
      Bcap = 50,
      Bc = 4,
      Bd = 4,
      window_start_hour = 5,
      charge_eff = 0.9,
      discharge_eff = 0.9
    )

  opt_grid_num <- expect_no_message(
    opt_data |>
      add_battery_optimization(
        opt_objective = 1,
        Bcap = 50,
        Bc = 4,
        Bd = 4,
        window_start_hour = 5,
        charge_eff = 0.9,
        discharge_eff = 0.9
      )
  )

  opt_cost_chr <- opt_data |>
    add_battery_optimization(
      opt_objective = "cost",
      Bcap = 50,
      Bc = 4,
      Bd = 4,
      window_start_hour = 5,
      charge_eff = 0.9,
      discharge_eff = 0.9,
      lambda = 0
    )

  opt_cost_num <- expect_no_message(
    opt_data |>
      add_battery_optimization(
        opt_objective = 0,
        Bcap = 50,
        Bc = 4,
        Bd = 4,
        window_start_hour = 5,
        charge_eff = 0.9,
        discharge_eff = 0.9,
        lambda = 0
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

test_that("error when `opt_objective` is wrong in battery optimization", {
  expect_error(
    opt_data |>
      add_battery_optimization(
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

  opt_battery_vct <- opt_data_batt |>
    add_battery_optimization(
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
    add_battery_optimization(
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


test_that("battery optimization works with grid optimization and efficiencies", {
  opt_data_batt <- opt_data |>
    mutate(
      production = .data$production * 100,
      static = .data$static * 100
    ) |>
    timefully::change_timeseries_resolution(60, "average")

  Bcap <- 500
  opt_battery_vct <- opt_data_batt |>
    add_battery_optimization(
      opt_objective = "grid",
      Bcap = Bcap,
      Bc = 500,
      Bd = 500,
      window_start_hour = 0,
      SOCini = 50,
      charge_eff = 0.9,
      discharge_eff = 0.9
    )

  opt_battery <- opt_data_batt |>
    mutate(
      battery = opt_battery_vct,
      storage = get_storage_level(
        battery,
        charge_eff = 0.9,
        discharge_eff = 0.9,
        time_resolution = 60,
        init = 50 / 100 * Bcap
      ),
      soc = round(storage / Bcap * 100, 2)
    )

  # opt_battery |>
  #   plot_ts()

  expect_lte(max(opt_battery$storage), Bcap)
  expect_gte(min(opt_battery$storage), 0)
})


expect_no_simultaneous_battery_flows <- function(profile, tolerance = 1e-5) {
  charge <- attr(profile, "charge")
  discharge <- attr(profile, "discharge")

  expect_false(any(charge > tolerance & discharge > tolerance))
  expect_equal(
    as.numeric(profile),
    charge - discharge,
    tolerance = tolerance
  )
}


test_that("battery optimization does not use simultaneous cycling as an energy sink", {
  opt_data_batt <- opt_data |>
    mutate(
      export_capacity = 0,
      import_capacity = 1000
    )

  objectives <- list(
    grid = "grid",
    capacity = "capacity",
    cost = "cost",
    combined = 0.5
  )

  results <- lapply(objectives, function(objective) {
    add_battery_optimization(
      opt_data = opt_data_batt,
      opt_objective = objective,
      Bcap = 25,
      Bc = 6,
      Bd = 6,
      window_days = 1,
      window_start_hour = 0,
      flex_window_hours = 4,
      charge_eff = 0.95,
      discharge_eff = 0.95
    )
  })

  lapply(results, expect_no_simultaneous_battery_flows)

  lapply(results, function(profile) {
    charge <- attr(profile, "charge")
    discharge <- attr(profile, "discharge")
    storage <- get_storage_level(
      profile,
      time_resolution = 15,
      charge_eff = 0.95,
      discharge_eff = 0.95
    )

    expect_equal(as.numeric(profile), rep(0, length(profile)), tolerance = 1e-5)
    expect_equal(storage, rep(0, length(storage)), tolerance = 1e-5)
  })
})
