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


make_qp_opt_data <- function(
  flexible,
  production = 0,
  static = 0,
  import_capacity = Inf,
  export_capacity = Inf,
  load_capacity = Inf
) {
  slots <- length(flexible)

  tibble(
    datetime = seq.POSIXt(
      as.POSIXct("2023-01-01 00:00:00", tz = "UTC"),
      by = "15 min",
      length.out = slots
    ),
    flexible = flexible,
    production = rep_len(production, slots),
    static = rep_len(static, slots),
    import_capacity = rep_len(import_capacity, slots),
    export_capacity = rep_len(export_capacity, slots),
    load_capacity = rep_len(load_capacity, slots)
  )
}


expect_demand_profile_within_capacity <- function(
  profile,
  opt_data,
  tolerance = 1e-5
) {
  balance <- opt_data |>
    mutate(consumption = static + profile) |>
    get_energy_balance()

  expect_true(all(balance$imported <= balance$import_capacity + tolerance))
  expect_true(all(balance$exported <= balance$export_capacity + tolerance))
}


demand_moved_energy <- function(profile, original_profile) {
  sum(abs(as.numeric(profile) - as.numeric(original_profile))) / 2
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

  cost_profile <- demand_cost_window(
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

  combined_profile <- demand_combined_window(
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


test_that("demand qp capacity objective is accepted and no-ops inside limits", {
  opt_data_qp <- make_qp_opt_data(
    flexible = c(1, 1, 1, 1),
    import_capacity = 1,
    export_capacity = 0
  )

  opt_profile <- opt_data_qp |>
    optimize_demand_qp(
      opt_objective = "capacity",
      direction = "forward",
      time_horizon = 4
    )

  expect_type(opt_profile, "double")
  expect_equal(length(opt_profile), nrow(opt_data_qp))
  expect_equal(as.numeric(opt_profile), opt_data_qp$flexible, tolerance = 1e-6)
})


test_that("demand qp capacity objective removes import overflow", {
  opt_data_qp <- make_qp_opt_data(
    flexible = c(2, 2, 0, 0),
    import_capacity = 1,
    export_capacity = Inf
  )

  opt_profile <- opt_data_qp |>
    optimize_demand_qp(
      opt_objective = "capacity",
      direction = "forward",
      time_horizon = 4
    )

  expect_demand_profile_within_capacity(opt_profile, opt_data_qp)
})


test_that("demand qp capacity objective can pull load into export-overflow periods", {
  opt_data_qp <- make_qp_opt_data(
    flexible = c(0, 0, 2, 2),
    production = c(2, 2, 0, 0),
    import_capacity = Inf,
    export_capacity = 0
  )

  opt_profile <- opt_data_qp |>
    optimize_demand_qp(
      opt_objective = "capacity",
      direction = "backward",
      time_horizon = 4
    )

  expect_demand_profile_within_capacity(opt_profile, opt_data_qp)
  expect_gt(sum(opt_profile[1:2]), 0)
})


test_that("demand qp capacity objective moves less energy than grid objective when feasible", {
  opt_data_qp <- make_qp_opt_data(
    flexible = c(2, 2, 0, 0, 0, 0),
    import_capacity = 1,
    export_capacity = Inf
  )

  capacity_profile <- opt_data_qp |>
    optimize_demand_qp(
      opt_objective = "capacity",
      direction = "forward",
      time_horizon = 6
    )

  grid_profile <- opt_data_qp |>
    optimize_demand_qp(
      opt_objective = "grid",
      direction = "forward",
      time_horizon = 6
    )

  expect_demand_profile_within_capacity(capacity_profile, opt_data_qp)
  expect_lte(
    demand_moved_energy(capacity_profile, opt_data_qp$flexible),
    demand_moved_energy(grid_profile, opt_data_qp$flexible) + 1e-6
  )
})


test_that("demand qp capacity objective falls back to unconstrained grid optimization when infeasible", {
  opt_data_qp <- make_qp_opt_data(
    flexible = c(3, 0, 0, 0),
    import_capacity = 0,
    export_capacity = 0
  )

  capacity_profile <- NULL
  expect_message(
    capacity_profile <- opt_data_qp |>
      optimize_demand_qp(
        opt_objective = "capacity",
        direction = "forward",
        time_horizon = 4
      ),
    "Removing grid constraints"
  )

  grid_profile_inf <- minimize_net_power_window_qp(
    G = opt_data_qp$production,
    LF = opt_data_qp$flexible,
    LS = opt_data_qp$static,
    direction = "forward",
    time_horizon = 4,
    LFmax = opt_data_qp$load_capacity,
    import_capacity = rep(Inf, nrow(opt_data_qp)),
    export_capacity = rep(Inf, nrow(opt_data_qp)),
    lambda = 0
  )

  expect_equal(capacity_profile, grid_profile_inf, tolerance = 1e-6)
})
