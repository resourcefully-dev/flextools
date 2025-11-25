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
  expect_message(
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

test_that("battery optimization works for grid objective", {
  opt_battery <- opt_data %>%
    select(any_of(c(
      "datetime", "production", "building", "price_imported", "price_exported"
    ))) %>%
    mutate(
      static = .data$building
    ) %>%
    add_battery_optimization(
      opt_objective = "grid",
      Bcap = 50, Bc = 4, Bd = 4,
      window_start_hour = 5,
      charge_eff = 0.9, discharge_eff = 0.9
    )

  expect_type(opt_battery, "double")
})

test_that("battery optimization works for cost objective", {
  opt_battery <- opt_data %>%
    select(any_of(c(
      "datetime", "production", "building", "price_imported", "price_exported"
    ))) %>%
    mutate(
      static = .data$building
    ) %>%
    add_battery_optimization(
      opt_objective = "cost",
      Bcap = 50, Bc = 4, Bd = 4,
      window_start_hour = 5,
      charge_eff = 0.9, discharge_eff = 0.9
    )

  expect_type(opt_battery, "double")
})


test_that("battery optimization works for combined objective", {
  opt_battery <- opt_data %>%
    select(any_of(c(
      "datetime", "production", "building", "price_imported", "price_exported"
    ))) %>%
    mutate(
      static = .data$building
    ) %>%
    add_battery_optimization(
      opt_objective = 0.5,
      Bcap = 50, Bc = 4, Bd = 4,
      window_start_hour = 5,
      charge_eff = 0.9, discharge_eff = 0.9
    )

  expect_type(opt_battery, "double")
})

test_that("error when `opt_objective` is wrong in battery optimization", {
  expect_error(
    opt_data %>%
      select(any_of(c(
        "datetime", "production", "building", "price_imported", "price_exported"
      ))) %>%
      mutate(
        static = .data$building
      ) %>%
      add_battery_optimization(
        opt_objective = "grids",
        Bcap = 50, Bc = 4, Bd = 4,
        window_start_hour = 5
      )
  )
})


test_that("battery optimization works with constrained import capacity", {
  opt_data_batt <- opt_data %>%
    select(datetime, production, static = building) %>%
    mutate(
      production = .data$production*0,
      static = .data$static*100,
      # import_capacity = 500
      import_capacity = rep(
        c(rep(500, 9*4), rep(150, 12*4), rep(500, 3*4)), 7
      )
    )

  opt_battery_vct <- opt_data_batt %>%
    add_battery_optimization(
      opt_objective = "grid",
      Bcap = 5000, Bc = 5000, Bd = 5000,
      window_start_hour = 0
    )

  opt_battery <- opt_data_batt %>%
    mutate(
      battery = opt_battery_vct,
      consumption = static + battery
    ) %>%
    get_energy_balance()

  opt_battery %>%
    plot_ts()

  expect_false(
    any(round(opt_battery$import_capacity - opt_battery$imported) < 0) # There's still some error
  )
})


test_that("battery optimization works with constrained import capacity and 'curtail' objective", {
  opt_data_batt <- opt_data %>%
    select(datetime, production, static = building) %>%
    mutate(
      production = .data$production*0,
      static = .data$static*100,
      import_capacity = 350
      # import_capacity = rep(
      #   c(rep(500, 9*4), rep(150, 12*4), rep(500, 3*4)), 7
      # )
    )

  opt_battery_vct <- opt_data_batt %>%
    add_battery_optimization(
      opt_objective = "curtail",
      Bcap = 5000, Bc = 500, Bd = 500,
      window_start_hour = 0
    )

  opt_battery <- opt_data_batt %>%
    mutate(
      battery = opt_battery_vct,
      consumption = static + battery
    ) %>%
    get_energy_balance()

  opt_battery %>%
    plot_ts() %>%
    dygraphs::dyLegend(show="onmouseover")

  expect_false(
    any(round(opt_battery$import_capacity - opt_battery$imported) < 0) # There's still some error
  )
})


test_that("battery optimization works with grid optimization and efficiencies", {
  opt_data_batt <- opt_data %>%
    select(datetime, production, static = building) %>%
    mutate(
      production = .data$production*100,
      static = .data$static*100
    ) |>
    decrease_timeseries_resolution(60, "average")

  Bcap <- 500
  opt_battery_vct <- opt_data_batt %>%
    add_battery_optimization(
      opt_objective = "grid",
      Bcap = Bcap, Bc = 500, Bd = 500,
      window_start_hour = 0, SOCini = 50,
      charge_eff = 0.9, discharge_eff = 0.9
    )

  opt_battery <- opt_data_batt %>%
    mutate(
      battery = opt_battery_vct,
      storage = get_storage_level(
        battery, 
        charge_eff = 0.9, discharge_eff = 0.9,
        time_resolution = 60, init = 50/100*Bcap
      ),
      soc = round(storage/Bcap*100, 2)
    )

  # opt_battery %>%
  #   plot_ts()

  expect_lte(max(opt_battery$storage), Bcap)
  expect_gte(min(opt_battery$storage), 0)
})
