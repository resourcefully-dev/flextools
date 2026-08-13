library(dplyr)
# devtools::load_all()

# Time benchmark for a whole year:
# - grid (1): 1.7s
# - cost (0): 17.33s
# - cost with eff (0): 17.33s
# - combined (0.1): 4.77s
# - combined (0.5): 4.77s
# compare_battery_year()

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
      window_start_hour = 5
    )

  expect_type(opt_battery, "double")
})

test_that("battery optimization returns a feasible battery profile", {
  opt_battery <- opt_data |>
    add_battery_optimization(
      Bcap = 50,
      Bc = 4,
      Bd = 4,
      window_start_hour = 5
    )

  expect_type(opt_battery, "double")
  expect_equal(length(opt_battery), nrow(opt_data))

  storage <- get_storage_level(
    opt_battery,
    time_resolution = 15,
    init = 0
  )

  expect_lte(max(opt_battery), 4 + 1e-6)
  expect_gte(min(opt_battery), -4 - 1e-6)
  expect_lte(max(storage), 50 + 1e-6)
  expect_gte(min(storage), -1e-6)
  expect_equal(tail(storage, 1), 0, tolerance = 1e-6)
})

test_that("battery optimization returns zero profile when bounds are infeasible", {
  opt_data_infeasible <- opt_data |>
    mutate(
      production = 0,
      static = 10,
      import_capacity = 0,
      export_capacity = 0
    )

  opt_battery <- opt_data_infeasible |>
    add_battery_optimization(
      Bcap = 50,
      Bc = 1,
      Bd = 1,
      window_start_hour = 5
    )

  expect_equal(
    as.numeric(opt_battery),
    rep(0, nrow(opt_data_infeasible))
  )
})


test_that("battery relaxes grid caps minimally to the pre-battery profile", {
  # A single 10 kW peak sits above the 3 kW import cap, so the window is
  # infeasible under the true caps and the grid caps must be relaxed. The
  # relaxation must be minimal: only the peak slot may exceed the cap (up to its
  # pre-battery net flow), while every other slot stays hard-capped at 3 kW.
  n <- 6
  G <- rep(0, n)
  L <- c(1, 1, 10, 1, 1, 1)
  import_capacity <- rep(3, n)
  export_capacity <- rep(3, n)

  B <- suppressMessages(flextools:::battery_grid_window(
    G = G, L = L, Bcap = 20, Bc = 5, Bd = 5,
    SOCmin = 0, SOCmax = 100, SOCini = 50,
    import_capacity = import_capacity, export_capacity = export_capacity
  ))

  relax_tol <- 0.02
  net_import <- pmax(L + B - G, 0)
  net_export <- pmax(G - L - B, 0)
  orig_import <- pmax(L - G, 0)
  orig_export <- pmax(G - L, 0)

  # Never worse than the pre-battery profile.
  expect_true(all(net_import <= pmax(import_capacity, orig_import) + relax_tol))
  expect_true(all(net_export <= pmax(export_capacity, orig_export) + relax_tol))
  # Slots within their cap stay hard-capped at the true capacity.
  within <- orig_import <= import_capacity
  expect_true(all(net_import[within] <= import_capacity[within] + relax_tol))
  # The battery is energy-neutral and actually acts (not the do-nothing guard).
  expect_equal(sum(B), 0, tolerance = 1e-6)
  expect_true(any(abs(B) > 1e-6))
})


# Negative capacities: forced flow -----------------------------------------
# A negative `import_capacity` is an obligation to EXPORT at least that much.
# When it cannot be met the battery must still spend everything it physically
# has approaching it, rather than abandoning the constraint and falling back to
# plain net-power flattening.

test_that("unreachable forced export still discharges to the physical limit", {
  # 60 kW of flat load and a -100 kW import capacity in the second half: the
  # site would need ~160 kW of discharge, far beyond the battery. What limits
  # the answer here is the charging headroom (100 - 60 = 40 kW per slot) plus
  # energy neutrality, so the battery charges 40 and gives 40 back.
  #
  # Note the quadratic objective PREFERS doing nothing (sum(net^2) is 28 800 at
  # a flat 60 kW against 41 600 for the answer below), so this test is also the
  # check that the slack penalty dominates the objective.
  n <- 8
  G <- rep(0, n)
  L <- rep(60, n)
  import_capacity <- c(rep(100, 4), rep(-100, 4))
  export_capacity <- rep(200, n)

  B <- suppressMessages(flextools:::battery_grid_window(
    G = G, L = L, Bcap = 400, Bc = 95, Bd = 105,
    SOCmin = 0, SOCmax = 100, SOCini = 50,
    import_capacity = import_capacity, export_capacity = export_capacity
  ))
  net <- L - G + B

  expect_equal(B, c(40, 40, 40, 40, -40, -40, -40, -40), tolerance = 1e-3)
  expect_equal(net, c(100, 100, 100, 100, 20, 20, 20, 20), tolerance = 1e-3)
  # The point of the test: it exports harder than the do-nothing profile.
  expect_true(all(net[5:8] < 60))
  expect_equal(sum(B), 0, tolerance = 1e-6)
  # Slots that can meet their capacity are still hard-capped.
  expect_true(all(net[1:4] <= 100 + 1e-3))
})


test_that("reachable forced export meets the capacity exactly", {
  n <- 8
  G <- rep(0, n)
  L <- rep(60, n)

  B <- suppressMessages(flextools:::battery_grid_window(
    G = G, L = L, Bcap = 800, Bc = 95, Bd = 105,
    SOCmin = 0, SOCmax = 100, SOCini = 50,
    import_capacity = c(rep(200, 4), rep(-20, 4)),
    export_capacity = rep(200, n)
  ))
  net <- L - G + B

  expect_true(all(net[5:8] <= -20 + 1e-3))
  expect_equal(net[5:8], rep(-20, 4), tolerance = 1e-3)
})


test_that("cost and combined objectives accept a negative import capacity", {
  # Regression: a negative capacity used to be pushed onto the one-sided import
  # variable, producing the box [0, -100]. HiGHS reported "Col N has
  # inconsistent bounds", the solve failed, and the battery was disabled for the
  # whole window.
  n <- 96
  dttm <- seq(as.POSIXct("2025-01-01 00:00", tz = "UTC"), by = 900, length.out = n)
  hour <- as.integer(format(dttm, "%H"))
  opt_data <- dplyr::tibble(
    datetime = dttm,
    production = 0,
    static = 60,
    price_imported = 0.20,
    price_exported = 0.10,
    import_capacity = ifelse(hour >= 16 & hour < 20, -100, 100),
    export_capacity = 200
  )

  for (objective in list("grid", "cost", 0.5)) {
    B <- expect_no_warning(suppressMessages(add_battery_optimization(
      opt_data, opt_objective = objective,
      Bcap = 500, Bc = 95, Bd = 105,
      SOCmin = 0, SOCmax = 100, SOCini = 50, window_start_hour = 0
    )))
    net <- opt_data$static - opt_data$production + B
    window <- hour >= 16 & hour < 20

    expect_false(all(abs(B) < 1e-9), label = paste("battery acts for", objective))
    # Exports during the obligation instead of sitting at the +60 kW baseline.
    expect_lt(median(net[window]), 0)
  }
})


test_that("unconstrained results are unchanged by the soft-capacity machinery", {
  # With no capacity limits every slack ceiling is zero, so the solver must take
  # the fast path and reproduce the pre-soft-capacity results exactly. Baseline
  # captured on the commit before soft capacities were introduced.
  golden <- readRDS(test_path("golden-battery-unconstrained.rds"))

  opt_data <- flextools::energy_profiles |>
    filter(lubridate::isoweek(datetime) == 18) |>
    select(
      datetime,
      production = solar,
      static = building,
      price_exported,
      price_imported
    )

  for (objective in list("grid", "capacity", "cost", 0.5)) {
    key <- if (is.numeric(objective)) paste0("w", objective) else objective
    B <- suppressMessages(add_battery_optimization(
      opt_data, opt_objective = objective,
      Bcap = 50, Bc = 4, Bd = 4, window_start_hour = 5
    ))
    expect_equal(B, golden[[key]], label = paste("objective", key))
  }

  # A finite capacity that never binds must take the same path.
  B_loose <- suppressMessages(
    opt_data |>
      mutate(import_capacity = 1e4, export_capacity = 1e4) |>
      add_battery_optimization(
        opt_objective = "grid",
        Bcap = 50, Bc = 4, Bd = 4, window_start_hour = 5
      )
  )
  expect_equal(B_loose, golden[["grid_loose_cap"]])
})


test_that("slack ceiling is zero exactly where the capacity can be met", {
  expect_equal(
    flextools:::optimization_slack_ceiling(c(10, 10, 10), c(4, 10, 25)),
    c(0, 0, 15)
  )
  # Infinite capacity can never be missed, and must not produce NaN.
  expect_equal(
    flextools:::optimization_slack_ceiling(c(Inf, Inf), c(5, Inf)),
    c(0, 0)
  )
})


test_that("battery optimization falls back to a heuristic profile on solver failure", {
  testthat::local_mocked_bindings(
    battery_solve_osqp = function(P, q, A, lower, upper) {
      list(
        result = list(
          info = list(
            status_val = 7L,
            status = "mock solver failure"
          )
        ),
        profile = NULL
      )
    },
    .package = "flextools"
  )

  profile <- flextools:::battery_solve_grid_window(
    G = c(8, 8, 0, 0),
    L = c(0, 0, 8, 8),
    Bcap = 8,
    Bc = 4,
    Bd = 4,
    SOCmin = 0,
    SOCmax = 100,
    SOCini = 0,
    import_capacity = Inf,
    export_capacity = Inf,
    P = 2 * diag(4),
    q = 2 * c(-8, -8, 8, 8)
  )

  storage <- cumsum(profile)

  expect_equal(profile, c(4, 4, -4, -4))
  expect_true(any(abs(profile) > 1e-9))
  expect_gte(min(storage), -1e-8)
  expect_lte(max(storage), 8 + 1e-8)
  expect_equal(sum(profile), 0, tolerance = 1e-8)
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
  #   timefully::plot_ts()

  expect_false(
    any(round(opt_battery$import_capacity - opt_battery$imported) < 0)
  )
})

test_that("battery optimization works with constrained import capacity and 'capacity' objective", {
  opt_data_batt <- opt_data |>
    mutate(
      production = .data$production * 0,
      static = .data$static * 100,
      import_capacity = 350
    ) |>
    filter(
      lubridate::day(datetime) %in% c(2, 3, 4, 5)
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
  #   timefully::plot_ts(legend_width = 150)

  expect_false(
    any(round(opt_battery$import_capacity - opt_battery$imported) < 0)
  )
})

test_that("battery optimization works for cost objective", {
  opt_battery <- opt_data |>
    add_battery_optimization(
      opt_objective = "cost",
      Bcap = 50,
      Bc = 4,
      Bd = 4,
      window_start_hour = 5
    )

  expect_type(opt_battery, "double")
  expect_equal(length(opt_battery), nrow(opt_data))
})

test_that("battery optimization works for combined objective", {
  opt_battery <- opt_data |>
    add_battery_optimization(
      opt_objective = 0.5,
      Bcap = 50,
      Bc = 4,
      Bd = 4,
      window_start_hour = 5
    )

  expect_type(opt_battery, "double")
  expect_equal(length(opt_battery), nrow(opt_data))
})

test_that("battery optimization works for grid objective with lambda > 0", {
  opt_battery <- opt_data |>
    add_battery_optimization(
      opt_objective = "grid",
      Bcap = 50,
      Bc = 4,
      Bd = 4,
      window_start_hour = 5,
      lambda = 0.1
    )

  expect_type(opt_battery, "double")
  expect_equal(length(opt_battery), nrow(opt_data))
})


# Time benchmarking for battery optimization ----------------------
test_battery_year <- function(opt_objective, eff = FALSE) {
  message(sprintf(
    "Testing battery optimization for objective: %s",
    opt_objective
  ))

  efficiency <- 0.95

  timefully::tic()
  B <- flextools::energy_profiles |>
    rename(
      production = "solar",
      static = building
    ) |>
    add_battery_optimization(
      opt_objective = opt_objective,
      Bcap = 50,
      Bc = 4,
      Bd = 4,
      charge_eff = ifelse(eff, efficiency, 1),
      discharge_eff = ifelse(eff, efficiency, 1),
      cycle_cost = 0.02
    )
  time <- timefully::toc()

  # losses <- get_conversion_losses(
  #   B,
  #   charge_eff = efficiency,
  #   discharge_eff = efficiency
  # )
  # B <- B + losses

  cost <- evaluate_cost(
    flextools::energy_profiles |>
      rename(
        production = "solar",
        static = building
      ),
    B
  )

  n_cycles <- -sum(pmin(B, 0)) / 50

  list(
    profile = B,
    time = time,
    cost = cost,
    n_cycles = n_cycles
  )
}
compare_battery_year <- function() {
  res_grid <- test_battery_year("grid")
  res_cost <- test_battery_year("cost")
  res_cost_eff <- test_battery_year("cost", eff = TRUE)
  res_combined_0.1 <- test_battery_year(0.1)
  res_combined_0.5 <- test_battery_year(0.5)

  kpis <- purrr::map(
    purrr::set_names(c(
      "grid",
      "cost",
      "cost_eff",
      "combined_0.1",
      "combined_0.5"
    )),
    ~ tibble(
      time = get(paste0("res_", .x))$time,
      cost = get(paste0("res_", .x))$cost,
      n_cycles = get(paste0("res_", .x))$n_cycles
    )
  ) |>
    purrr::list_rbind(names_to = "objective")
  print(kpis)

  flextools::energy_profiles |>
    rename(
      production = "solar",
      static = building
    ) |>
    select(
      -any_of(c("price_turn_up", "price_turn_down"))
    ) |>
    mutate(
      battery_grid = res_grid$profile,
      battery_cost = res_cost$profile,
      battery_cost_eff = res_cost_eff$profile,
      battery_combined_0.1 = res_combined_0.1$profile,
      battery_combined_0.5 = res_combined_0.5$profile
    ) |>
    timefully::plot_ts(
      title = sprintf(
        "Benchmarking: Grid: %0.1fs, Cost: %0.1fs, Cost w/ eff: %0.1fs, Combined (0.1): %0.1fs, Combined (0.5): %0.1fs",
        res_grid$time,
        res_cost$time,
        res_cost_eff$time,
        res_combined_0.1$time,
        res_combined_0.5$time
      ),
      legend_width = 200
    )
}
