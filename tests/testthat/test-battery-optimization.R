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


# Concentrating an unavoidable miss ----------------------------------------
# The slack penalty is linear in the volume missed, so the solver is
# indifferent to HOW a limited amount of energy is spread over the slots that
# miss their capacity, and the quadratic term flattens it - leaving every slot
# marginally over. `congestion_time` counts slots, not kWh, so that is the one
# distribution which never improves it.

congestion_scenario <- function(
  static = 50,
  import_capacity = -100,
  export_capacity = 200,
  n = 96
) {
  dttm <- seq(as.POSIXct("2025-01-01 00:00", tz = "UTC"), by = 900, length.out = n)
  hour <- as.integer(format(dttm, "%H"))
  window <- hour >= 17 & hour < 21
  list(
    window = window,
    opt_data = dplyr::tibble(
      datetime = dttm,
      production = 0,
      static = static,
      price_imported = 0.20,
      price_exported = 0.05,
      import_capacity = ifelse(window, import_capacity, 100),
      export_capacity = export_capacity
    )
  )
}


# A flat baseline well inside the capacity with one short peak that overshoots
# it. `congestion_scenario()` cannot tell the two objectives apart: its load is
# flat, so the profile that respects the capacity is also the flattest one and
# `grid` and `capacity` return the same answer. Here they disagree - `grid`
# keeps pulling the peak down past the capacity, `capacity` stops on it.
peak_scenario <- function(
  base = 2000,
  peak = 4000,
  import_capacity = 3400,
  n = 96
) {
  dttm <- seq(as.POSIXct("2025-11-01 00:00", tz = "UTC"), by = 900, length.out = n)
  hour <- as.integer(format(dttm, "%H"))
  window <- hour >= 7 & hour < 9
  list(
    window = window,
    capacity = import_capacity,
    # 600 kW over two hours = 1200 kWh to move.
    overshoot_kwh = (peak - import_capacity) * sum(window) / 4,
    opt_data = dplyr::tibble(
      datetime = dttm,
      production = 0,
      static = ifelse(window, peak, base),
      price_imported = 0.20,
      price_exported = 0.05,
      import_capacity = import_capacity,
      export_capacity = 3000
    )
  )
}


test_that("an energy-limited forced export is met in as many slots as it covers", {
  scenario <- congestion_scenario()
  window <- scenario$window

  B <- suppressMessages(add_battery_optimization(
    scenario$opt_data, opt_objective = "grid",
    Bcap = 600, Bc = 200, Bd = 200,
    SOCmin = 0, SOCmax = 100, SOCini = 50, window_start_hour = 0
  ))
  net <- scenario$opt_data$static - scenario$opt_data$production + B
  capacity <- scenario$opt_data$import_capacity
  over <- net[window] > capacity[window] + 1e-6

  # The battery cannot cover the whole 4-hour obligation, so some slots must
  # miss it - but not all of them, which is what flattening produced.
  expect_gt(sum(over), 0)
  expect_lt(sum(over), sum(window))
  # The slots it does cover sit exactly on the capacity, not near it.
  expect_equal(net[window][!over], rep(-100, sum(!over)), tolerance = 1e-3)
  # Concentrating moves energy inside the window; it does not invent any. The
  # tolerance is OSQP's own: its solution for this window closes the neutrality
  # row to ~1e-5, and the redistribution is exact on top of whatever it returns.
  expect_equal(sum(B), 0, tolerance = 1e-4)
})


test_that("congestion_time decreases with battery size instead of stepping", {
  # The regression this guards: with the miss spread thinly, every one of these
  # batteries left 100% of the window in violation, so the sizing sweep read as
  # "no battery helps" until one covered the window outright.
  scenario <- congestion_scenario()
  window <- scenario$window

  violated <- vapply(
    c(200, 400, 600, 800),
    function(Bcap) {
      B <- suppressMessages(add_battery_optimization(
        scenario$opt_data, opt_objective = "grid",
        Bcap = Bcap, Bc = 200, Bd = 200,
        SOCmin = 0, SOCmax = 100, SOCini = 50, window_start_hour = 0
      ))
      net <- scenario$opt_data$static + B
      sum(net[window] > scenario$opt_data$import_capacity[window] + 1e-6)
    },
    numeric(1)
  )

  expect_true(all(diff(violated) < 0))
})


test_that("a forced import is concentrated the same way", {
  # Mirror image: a negative `export_capacity` obliges the site to IMPORT, so
  # the battery charges, and the slots it covers sit on that capacity exactly.
  scenario <- congestion_scenario(
    static = 0,
    import_capacity = 200,
    export_capacity = -100
  )
  window <- scenario$window
  opt_data <- scenario$opt_data
  opt_data$production <- 50
  opt_data$export_capacity <- ifelse(window, -100, 200)

  B <- suppressMessages(add_battery_optimization(
    opt_data, opt_objective = "grid",
    Bcap = 300, Bc = 200, Bd = 200,
    SOCmin = 0, SOCmax = 100, SOCini = 50, window_start_hour = 0
  ))
  net <- opt_data$static - opt_data$production + B
  under <- -net[window] > opt_data$export_capacity[window] + 1e-6

  expect_gt(sum(under), 0)
  expect_lt(sum(under), sum(window))
  expect_equal(net[window][!under], rep(100, sum(!under)), tolerance = 1e-3)
  expect_equal(sum(B), 0, tolerance = 1e-4)
})


test_that("concentration keeps the energy and the storage envelope", {
  B <- c(20, 20, 20, -30, -30, -30)
  net0 <- rep(10, 6)
  out <- flextools:::optimization_concentrate_slack(
    B = B,
    net0 = net0,
    import_capacity = c(rep(100, 3), rep(-50, 3)),
    export_capacity = rep(200, 6),
    lb_B = rep(-100, 6),
    ub_B = rep(100, 6),
    lb_cumsum = rep(-200, 6),
    ub_cumsum = rep(200, 6)
  )

  expect_equal(sum(out), sum(B))
  # -60 meets the -50 capacity exactly; the 90 kWh available covers 1.5 slots.
  expect_equal(out, c(20, 20, 20, -60, -30, 0))
  expect_equal(min(cumsum(out)), min(cumsum(B)))
})


test_that("concentration is skipped when it cannot buy a slot back", {
  # Power-limited: 160 kW of discharge is needed and only 105 kW exists, so no
  # arrangement of the energy meets the capacity anywhere. Rearranging would
  # only spike the profile, so the flat answer stands.
  B <- rep(-40, 4)
  out <- flextools:::optimization_concentrate_slack(
    B = B,
    net0 = rep(60, 4),
    import_capacity = rep(-100, 4),
    export_capacity = rep(200, 4),
    lb_B = rep(-105, 4),
    ub_B = rep(0, 4),
    lb_cumsum = rep(-400, 4),
    ub_cumsum = rep(400, 4)
  )

  expect_equal(out, B)
})


test_that("concentration leaves a profile within its capacities untouched", {
  B <- c(10, -10, 10, -10)
  expect_equal(
    flextools:::optimization_concentrate_slack(
      B = B,
      net0 = rep(20, 4),
      import_capacity = rep(100, 4),
      export_capacity = rep(100, 4),
      lb_B = rep(-50, 4),
      ub_B = rep(50, 4),
      lb_cumsum = rep(-100, 4),
      ub_cumsum = rep(100, 4)
    ),
    B
  )
})


# The `capacity` objective ---------------------------------------------------
# It minimizes battery usage subject to the capacity, rather than minimizing
# the net grid flow the way `grid` does. So it discharges onto the capacity
# line and no further, and the nameplate does not decide how hard it works.

test_that("the capacity objective meets a reachable capacity at any SOCini", {
  scenario <- congestion_scenario()
  window <- scenario$window

  for (soc_ini in c(20, 50, 80)) {
    B <- suppressMessages(add_battery_optimization(
      scenario$opt_data, opt_objective = "capacity",
      Bcap = 4000, Bc = 200, Bd = 200,
      SOCmin = 0, SOCmax = 100, SOCini = soc_ini, window_start_hour = 0
    ))
    net <- scenario$opt_data$static + B
    expect_true(
      all(net[window] <= -100 + 1e-3),
      label = paste("capacity met at SOCini", soc_ini)
    )
  }
})


test_that("the capacity objective does not throw away extra battery capacity", {
  # Regression: the reserve was the overshoot volume itself, so the usable
  # energy around SOCini was a fraction of it and the capacity went unmet no
  # matter how much battery the caller had - adding capacity changed nothing.
  scenario <- congestion_scenario()
  window <- scenario$window

  for (Bcap in c(1000, 2000)) {
    B <- suppressMessages(add_battery_optimization(
      scenario$opt_data, opt_objective = "capacity",
      Bcap = Bcap, Bc = 200, Bd = 200,
      SOCmin = 0, SOCmax = 100, SOCini = 50, window_start_hour = 0
    ))
    net <- scenario$opt_data$static + B
    expect_true(
      all(net[window] <= -100 + 1e-3),
      label = paste("capacity met with Bcap", Bcap)
    )
  }
})


test_that("the capacity objective cycles only the overshoot volume", {
  # A 20 kW overshoot over four hours against a 2 MWh battery: the overshoot,
  # not the nameplate, must bound the energy cycled.
  scenario <- congestion_scenario(import_capacity = 30)
  window <- scenario$window

  B <- suppressMessages(add_battery_optimization(
    scenario$opt_data, opt_objective = "capacity",
    Bcap = 2000, Bc = 500, Bd = 500,
    SOCmin = 0, SOCmax = 100, SOCini = 50, window_start_hour = 0
  ))
  net <- scenario$opt_data$static + B

  expect_true(all(net[window] <= 30 + 1e-3))
  # 50 kW against a 30 kW capacity for 16 slots is 20 kW * 4 h = 80 kWh, and
  # every kWh discharged has to be recharged inside the same window.
  expect_equal(-sum(B[B < 0]) / 4, 80, tolerance = 1e-3)
  expect_equal(sum(pmax(B, 0)) / 4, 80, tolerance = 1e-3)
})


test_that("the capacity objective discharges onto the capacity, not below it", {
  # Regression (#71): the objective was `sum(net^2)` on a battery whose
  # nameplate had been shrunk to the overshoot, so it minimized the peak
  # instead of the battery and pulled the profile well under the capacity -
  # visibly so once the reserve was sized against the SOC band and doubled. On
  # this scenario it reached 2788 kW against a 3400 kW capacity and cycled
  # twice the overshoot volume to do it.
  scenario <- peak_scenario()
  window <- scenario$window

  B <- suppressMessages(add_battery_optimization(
    scenario$opt_data, opt_objective = "capacity",
    Bcap = 4000, Bc = 2000, Bd = 2000,
    SOCmin = 0, SOCmax = 100, SOCini = 50, window_start_hour = 0
  ))
  net <- scenario$opt_data$static + B

  # The peak sits exactly on the capacity, not under it: going under spends
  # battery for nothing.
  expect_equal(net[window], rep(scenario$capacity, sum(window)), tolerance = 1e-3)
  expect_equal(max(net), scenario$capacity, tolerance = 1e-3)
  # Exactly the overshoot volume is discharged, and recharged flat afterwards.
  expect_equal(-sum(B[B < 0]) / 4, scenario$overshoot_kwh, tolerance = 1e-3)
  # 1200 kWh spread over the 22 remaining hours is ~55 kW, far below the
  # 2000 kW the battery could have used.
  expect_lt(max(B), 100)
})


test_that("the capacity objective ignores battery it does not need", {
  # The counterpart of "does not throw away extra capacity": once the battery
  # is big enough to clear the window, a bigger one must change nothing. This
  # is what makes the objective usable for sizing - it answers "how little
  # battery does this limit need" rather than "how flat can this battery get".
  scenario <- congestion_scenario(import_capacity = 30)

  solutions <- lapply(c(500, 1000, 2000), function(Bcap) {
    suppressMessages(add_battery_optimization(
      scenario$opt_data, opt_objective = "capacity",
      Bcap = Bcap, Bc = 500, Bd = 500,
      SOCmin = 0, SOCmax = 100, SOCini = 50, window_start_hour = 0
    ))
  })

  expect_equal(solutions[[2]], solutions[[1]], tolerance = 1e-3)
  expect_equal(solutions[[3]], solutions[[1]], tolerance = 1e-3)
})


test_that("the grid objective still minimizes the net flow, not the battery", {
  # The two objectives must stay distinguishable: `grid` is expected to spend
  # the whole battery flattening the profile, which is why it is the wrong tool
  # for a capacity limit and the right one for a peak.
  scenario <- peak_scenario()

  args <- list(
    scenario$opt_data, Bcap = 4000, Bc = 2000, Bd = 2000,
    SOCmin = 0, SOCmax = 100, SOCini = 50, window_start_hour = 0
  )
  B_capacity <- suppressMessages(
    do.call(add_battery_optimization, c(args, opt_objective = "capacity"))
  )
  B_grid <- suppressMessages(
    do.call(add_battery_optimization, c(args, opt_objective = "grid"))
  )

  # Both respect the capacity; only `grid` goes on to flatten below it, and it
  # spends materially more battery doing so.
  expect_lte(max(scenario$opt_data$static + B_capacity), scenario$capacity + 1e-3)
  expect_lt(max(scenario$opt_data$static + B_grid), scenario$capacity - 100)
  expect_gt(sum(abs(B_grid)), 1.5 * sum(abs(B_capacity)))
})


test_that("a loose finite capacity does not disable the battery", {
  # Regression: the slack penalty was derived from the capacities rather than
  # from the flows a solution can reach, so a capacity standing in for
  # "unlimited" as a large finite number (rather than `Inf`, which is filtered
  # out) produced a penalty of ~2e7. OSQP returned no solution at that
  # conditioning and the window fell through to a disabled battery.
  scenario <- congestion_scenario(import_capacity = 30, export_capacity = 1e6)
  window <- scenario$window

  B <- suppressMessages(add_battery_optimization(
    scenario$opt_data, opt_objective = "capacity",
    Bcap = 2000, Bc = 500, Bd = 500,
    SOCmin = 0, SOCmax = 100, SOCini = 50, window_start_hour = 0
  ))
  net <- scenario$opt_data$static + B

  expect_false(all(abs(B) < 1e-9))
  expect_equal(net[window], rep(30, sum(window)), tolerance = 1e-3)
})


test_that("a battery pinned at one end of its SOC band still optimizes", {
  # SOCini == SOCmin gives a one-sided storage band: the battery can only
  # charge first and discharge afterwards. The congestion window is late enough
  # in the window for that to be possible, so the capacity is still met.
  scenario <- congestion_scenario()
  window <- scenario$window

  B <- suppressMessages(add_battery_optimization(
    scenario$opt_data, opt_objective = "capacity",
    Bcap = 1000, Bc = 200, Bd = 200,
    SOCmin = 0, SOCmax = 100, SOCini = 0, window_start_hour = 0
  ))
  net <- scenario$opt_data$static + B

  expect_false(all(abs(B) < 1e-9))
  expect_true(all(net[window] <= -100 + 1e-3))
})


test_that("the capacity objective does nothing when there is no overshoot", {
  scenario <- congestion_scenario(import_capacity = 100)
  B <- suppressMessages(add_battery_optimization(
    scenario$opt_data, opt_objective = "capacity",
    Bcap = 1000, Bc = 200, Bd = 200,
    SOCmin = 0, SOCmax = 100, SOCini = 50, window_start_hour = 0
  ))
  expect_equal(B, rep(0, nrow(scenario$opt_data)))
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
