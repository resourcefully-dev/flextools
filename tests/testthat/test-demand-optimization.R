library(dplyr)
# devtools::load_all()

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


# Minimal-relaxation fallback invariant --------------------------
# When a window is infeasible under its grid caps, the optimizer must relax the
# per-slot caps only as far as the ORIGINAL (unshifted) profile needs. The
# resulting invariant: per-slot net flow never exceeds
# max(capacity, original net flow), so the result is never worse than the input
# profile had, and slots that were within their caps stay hard-capped at the
# true capacity. `tol` absorbs the 2-decimal rounding of the bounds.
relax_tol <- 0.02

test_that("capacity objective relaxes minimally when a window is infeasible", {
  n <- 6
  G <- rep(0, n)
  LS <- rep(0, n)
  LF <- c(10, 0, 0, 10, 0, 0)
  import_capacity <- rep(2, n)
  export_capacity <- rep(0, n)

  # time_horizon = 1 leaves almost no room to shift the two 10 kW spikes below
  # the 2 kW import cap, so the capacity slice LP is infeasible.
  O <- suppressMessages(demand_capacity_window(
    G = G, LF = LF, LS = LS, direction = "forward",
    time_horizon = 1L, LFmax = rep(10, n),
    import_capacity = import_capacity, export_capacity = export_capacity
  ))

  net_import <- pmax(O + LS - G, 0)
  orig_import <- pmax(LF + LS - G, 0)

  # Never worse than the input profile had.
  expect_true(all(net_import <= pmax(import_capacity, orig_import) + relax_tol))
  # Slots that were within their cap stay hard-capped at the true capacity.
  within <- orig_import <= import_capacity
  expect_true(all(net_import[within] <= import_capacity[within] + relax_tol))
  # Flexible energy is preserved.
  expect_equal(sum(O), sum(LF), tolerance = 1e-6)
  # The relaxation actually optimizes; it does not hit the crash guard (LF).
  expect_false(isTRUE(all.equal(as.numeric(O), LF)))
})

test_that("grid objective relaxes minimally and clamps ub_O when LFmax < LF", {
  n <- 6
  G <- rep(0, n)
  LS <- rep(0, n)
  LF <- c(10, 0, 0, 10, 0, 0)
  import_capacity <- rep(2, n)
  export_capacity <- rep(0, n)

  # LFmax below the LF peak forces the ub_O clamp in the relaxation retry.
  O <- suppressMessages(demand_grid_window(
    G = G, LF = LF, LS = LS, direction = "forward",
    time_horizon = 1L, LFmax = rep(5, n),
    import_capacity = import_capacity, export_capacity = export_capacity
  ))

  net_import <- pmax(O + LS - G, 0)
  orig_import <- pmax(LF + LS - G, 0)

  expect_true(all(net_import <= pmax(import_capacity, orig_import) + relax_tol))
  within <- orig_import <= import_capacity
  expect_true(all(net_import[within] <= import_capacity[within] + relax_tol))
  expect_equal(sum(O), sum(LF), tolerance = 1e-6)
  expect_false(isTRUE(all.equal(as.numeric(O), LF)))
})


# Energy range ------------------------------------------------------------

test_that("optimize_demand results are unchanged at the default energy ratio", {
  # Baseline captured on 1.6.0 (commit 77312479) with exactly these calls.
  golden <- readRDS(test_path("golden-energy-range-defaults.rds"))

  O_grid <- suppressMessages(suppressWarnings(optimize_demand(
    opt_data |> mutate(flexible = building),
    opt_objective = "grid", direction = "forward",
    flex_window_hours = 6, time_horizon = 12
  )))
  expect_equal(O_grid, golden[["optimize_demand_grid"]])

  O_cost <- suppressMessages(suppressWarnings(optimize_demand(
    opt_data |> mutate(flexible = building),
    opt_objective = "cost", direction = "forward",
    flex_window_hours = 6, time_horizon = 12
  )))
  expect_equal(O_cost, golden[["optimize_demand_cost"]])

  O_capacity <- suppressMessages(suppressWarnings(optimize_demand(
    opt_data |> mutate(flexible = building, import_capacity = 1.5),
    opt_objective = "capacity", direction = "forward", window_days = 1
  )))
  expect_equal(O_capacity, golden[["optimize_demand_capacity"]])
})

# Two 10 kW slots followed by two empty ones, against a 4 kW import capacity:
# 20 units of energy where only 16 fit. Energy conservation makes this window
# infeasible; an energy range lets the optimizer keep the 16 that fit.
range_case <- list(
  G = rep(0, 4),
  LF = c(10, 10, 0, 0),
  LS = rep(0, 4),
  LFmax = rep(10, 4),
  import_capacity = rep(4, 4),
  export_capacity = rep(0, 4)
)

test_that("grid window drops only the energy the capacity leaves no room for", {
  expect_no_message(
    O <- demand_grid_window(
      G = range_case$G, LF = range_case$LF, LS = range_case$LS,
      direction = "forward", time_horizon = NULL, LFmax = range_case$LFmax,
      import_capacity = range_case$import_capacity,
      export_capacity = range_case$export_capacity,
      energy_ratio = c(0.5, 1)
    )
  )
  expect_equal(sum(O), 16, tolerance = 1e-6)
  expect_true(all(O <= 4 + relax_tol))

  # A ceiling below what fits is met exactly.
  O_low <- suppressMessages(demand_grid_window(
    G = range_case$G, LF = range_case$LF, LS = range_case$LS,
    direction = "forward", time_horizon = NULL, LFmax = range_case$LFmax,
    import_capacity = range_case$import_capacity,
    export_capacity = range_case$export_capacity,
    energy_ratio = c(0, 0.6)
  ))
  expect_equal(sum(O_low), 12, tolerance = 1e-6)
  expect_true(all(O_low <= 4 + relax_tol))
})

test_that("cost window keeps as much energy as the capacity admits", {
  O <- suppressMessages(demand_cost_window(
    G = range_case$G, LF = range_case$LF, LS = range_case$LS,
    PI = rep(0.2, 4), PE = rep(0, 4), PTD = rep(0, 4), PTU = rep(0, 4),
    direction = "forward", time_horizon = NULL, LFmax = range_case$LFmax,
    import_capacity = range_case$import_capacity,
    export_capacity = range_case$export_capacity,
    energy_ratio = c(0, 1)
  ))
  # Dropping energy is cheaper, but the reward for keeping it dominates.
  expect_equal(sum(O), 16, tolerance = 1e-6)
  expect_true(all(O <= 4 + relax_tol))
})

test_that("capacity window shifts before it drops, and scales to the ceiling", {
  solve <- function(energy_ratio) {
    demand_capacity_window(
      G = range_case$G, LF = range_case$LF, LS = range_case$LS,
      direction = "forward", time_horizon = NULL, LFmax = range_case$LFmax,
      import_capacity = range_case$import_capacity,
      export_capacity = range_case$export_capacity,
      energy_ratio = energy_ratio
    )
  }

  expect_no_message(O_half <- solve(c(0.5, 1)))
  expect_equal(sum(O_half), 16, tolerance = 1e-6)
  expect_true(all(O_half <= 4 + relax_tol))

  # 80% of 20 is exactly what fits: nothing has to be dropped.
  expect_no_message(O_80 <- solve(c(0, 0.8)))
  expect_equal(sum(O_80), 16, tolerance = 1e-6)
  expect_true(all(O_80 <= 4 + relax_tol))

  # A ceiling below what fits is met exactly, with the same shape scaled down
  # wherever the capacity holds.
  expect_no_message(O_50 <- solve(c(0, 0.5)))
  expect_equal(sum(O_50), 10, tolerance = 1e-6)
  expect_true(all(O_50 <= 4 + relax_tol))
})

test_that("a minimum above what fits relaxes the capacity to the minimum-energy profile only", {
  expect_message(
    O <- demand_grid_window(
      G = range_case$G, LF = range_case$LF, LS = range_case$LS,
      direction = "forward", time_horizon = NULL, LFmax = range_case$LFmax,
      import_capacity = range_case$import_capacity,
      export_capacity = range_case$export_capacity,
      energy_ratio = c(0.9, 1)
    ),
    "minimum energy does not fit"
  )
  # Exactly the minimum is delivered: the capacity is exceeded by what the
  # minimum needs and no more.
  expect_equal(sum(O), 18, tolerance = 1e-6)
  # The relaxed cap is what the 90% profile (9, 9, 0, 0) draws; slots that
  # were within their cap stay hard-capped.
  expect_true(all(O <= pmax(range_case$import_capacity, 0.9 * range_case$LF) + relax_tol))

  # Same order of relaxation in the capacity objective. The warning is emitted
  # once per smart_charging() call, so clear the cache the previous call filled.
  flextools:::reset_message_once()
  expect_message(
    O_cap <- demand_capacity_window(
      G = range_case$G, LF = range_case$LF, LS = range_case$LS,
      direction = "forward", time_horizon = NULL, LFmax = range_case$LFmax,
      import_capacity = range_case$import_capacity,
      export_capacity = range_case$export_capacity,
      energy_ratio = c(0.9, 1)
    ),
    "minimum energy does not fit"
  )
  expect_equal(sum(O_cap), 18, tolerance = 1e-6)
  expect_true(all(O_cap <= pmax(range_case$import_capacity, 0.9 * range_case$LF) + relax_tol))
})

test_that("a minimum of 0 never hands a zero ratio to a follow-up LP", {
  # Static load already fills the 4 kW import capacity in every slot, so the
  # capacity slice LP removes all the EV energy and can re-add none of it.
  # 1.7.0 turned the empty re-add into a c(0, 0) ratio for the follow-up grid
  # LP, which rejected it ("the maximum energy ratio must be higher than 0").
  O <- suppressMessages(demand_capacity_window(
    G = range_case$G, LF = range_case$LF, LS = rep(20, 4),
    direction = "forward", time_horizon = NULL, LFmax = range_case$LFmax,
    import_capacity = range_case$import_capacity,
    export_capacity = range_case$export_capacity,
    energy_ratio = c(0, 1)
  ))
  expect_equal(as.numeric(O), rep(0, 4))

  # Infeasible for a reason energy cannot fix: production above the export
  # capacity forces load the fleet cannot physically draw. The minimum-energy
  # profile is empty at 0%, so the capacity objective falls back to the
  # original profile as 1.6.0 did — with the original-profile warning, and no
  # error.
  export_case <- list(
    G = c(30, 30, 0, 0), LF = range_case$LF, LS = range_case$LS,
    LFmax = range_case$LFmax, import_capacity = range_case$import_capacity,
    export_capacity = rep(5, 4)
  )
  flextools:::reset_message_once()
  expect_message(
    O_cap <- demand_capacity_window(
      G = export_case$G, LF = export_case$LF, LS = export_case$LS,
      direction = "forward", time_horizon = NULL, LFmax = export_case$LFmax,
      import_capacity = export_case$import_capacity,
      export_capacity = export_case$export_capacity,
      energy_ratio = c(0, 1)
    ),
    "original profile"
  )
  expect_equal(sum(O_cap), sum(export_case$LF), tolerance = 1e-6)

  # The grid objective clips the export-side floor at the load capacity, so the
  # same window is feasible for it: the reward keeps the full profile, no
  # relaxation, no error.
  O_grid <- suppressMessages(demand_grid_window(
    G = export_case$G, LF = export_case$LF, LS = export_case$LS,
    direction = "forward", time_horizon = NULL, LFmax = export_case$LFmax,
    import_capacity = export_case$import_capacity,
    export_capacity = export_case$export_capacity,
    energy_ratio = c(0, 1)
  ))
  expect_equal(sum(O_grid), sum(export_case$LF), tolerance = 1e-6)
})

test_that("an energy range is rejected where energy cannot be dropped", {
  expect_error(
    demand_grid_window(
      G = range_case$G, LF = range_case$LF, LS = range_case$LS,
      direction = "backward", time_horizon = NULL, LFmax = range_case$LFmax,
      import_capacity = range_case$import_capacity,
      export_capacity = range_case$export_capacity,
      energy_ratio = c(0.5, 1)
    ),
    "forward"
  )
  expect_error(
    demand_grid_window(
      G = range_case$G, LF = range_case$LF, LS = range_case$LS,
      direction = "forward", time_horizon = 1L, LFmax = range_case$LFmax,
      import_capacity = range_case$import_capacity,
      export_capacity = range_case$export_capacity,
      energy_ratio = c(0.5, 1)
    ),
    "whole window"
  )
  expect_error(
    flextools:::demand_check_energy_ratio(c(0.8, 0.5), "forward", NULL, 4),
    "min <= max"
  )
  expect_error(
    flextools:::demand_check_energy_ratio(c(0, 0), "forward", NULL, 4),
    "higher than 0"
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
