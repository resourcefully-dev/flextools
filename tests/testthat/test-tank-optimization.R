library(dplyr)

# Fixtures follow the "flextools spec" of the FLEX+ thermal model plan
# (Phase 2). No PV unless a test says otherwise, so the COP-0 rule and the
# fallback are tested without surplus to absorb.

tank_data <- function(
  days = 1,
  resolution = 60,
  heat_demand = 1,
  cop = 3,
  flexible = 0.33,
  static = 0,
  production = 0,
  cheap_hours = 0:5
) {
  n <- days * 24 * 60 / resolution
  datetime <- as.POSIXct("2024-01-01", tz = "UTC") +
    resolution * 60 * (seq_len(n) - 1)
  tibble(
    datetime = datetime,
    production = rep_len(production, n),
    static = rep_len(static, n),
    heat_demand = rep_len(heat_demand, n),
    cop = rep_len(cop, n),
    flexible = rep_len(flexible, n),
    price_imported = ifelse(lubridate::hour(datetime) %in% cheap_hours, 0.1, 0.3),
    price_exported = 0
  )
}

# Unrounded level path, start of each slot plus the end of the last one
tank_path <- function(P, d, init, loss = 0, resolution = 60, clamp = 0) {
  dt <- resolution / 60
  tank_level_path(
    P,
    d$heat_demand,
    d$cop,
    init,
    (1 - loss / 100)^dt,
    dt,
    clamp
  )
}

# delivered - (demand + losses + change in level + clamp)
tank_balance_gap <- function(P, d, init, loss = 0, resolution = 60, clamp = 0) {
  dt <- resolution / 60
  k <- (1 - loss / 100)^dt
  path <- tank_path(P, d, init, loss, resolution, clamp)
  n <- length(P)
  delivered <- sum(d$cop * P * dt)
  demand <- sum(d$heat_demand * dt)
  losses <- sum((1 - k) * path[seq_len(n)])
  delivered - (demand + losses + (path[n + 1] - init) + sum(clamp))
}

net_cost <- function(P, d) {
  net <- d$static + P - d$production
  sum(d$price_imported * pmax(net, 0) - d$price_exported * pmax(-net, 0))
}

# Rounding the power to 2 decimals moves the level by at most half a
# rounding step of heat per slot: 0.005 * cop * dt.
rounding_tol <- 0.005 * 3 + 1e-9


test_that("one tank: heat and electricity add up and the level stays in the band", {
  d <- tank_data()
  P <- add_tank_optimization(d, "cost", Tcap = 3, Pc = 3, SOCini = 100)

  expect_type(P, "double")
  expect_length(P, nrow(d))
  expect_equal(sum(d$cop * P), 24, tolerance = 0.02)
  expect_equal(sum(P), 8, tolerance = 0.01)

  level <- tank_path(P, d, init = 3)
  expect_gte(min(level), 0 - rounding_tol)
  expect_lte(max(level), 3 + rounding_tol)

  # The tank is full when the cheap hours end: the electricity sits in the
  # cheap hours as far as the capacity allows.
  expect_equal(level[7], 3, tolerance = rounding_tol)
  expect_equal(sum(P[7:24]), 18 / 3, tolerance = 0.01)

  expect_length(attr(P, "infeasible_windows"), 0)
  expect_equal(attr(P, "tank_clamp_kwh"), rep(0, nrow(d)))
})

test_that("the compressor cap binds", {
  d <- tank_data()
  P1 <- add_tank_optimization(d, "cost", Tcap = 30, Pc = 1, SOCini = 0)
  P3 <- add_tank_optimization(d, "cost", Tcap = 30, Pc = 3, SOCini = 0)

  expect_lte(max(P1), 1)
  expect_gt(max(P3), 1)
  # With the larger cap the whole day's heat fits in the cheap hours
  expect_equal(sum(P3[1:6]), 8, tolerance = 0.01)
})

test_that("an infeasible window falls back to `flexible`, and the next is clamped", {
  heat_demand <- rep(1, 72)
  heat_demand[24 + 9:11] <- 5
  d <- tank_data(days = 3, heat_demand = heat_demand, flexible = 0.33)
  expect_message(
    P <- add_tank_optimization(
      d,
      "cost",
      Tcap = 3,
      Pc = 1,
      SOCmin = 20,
      SOCini = 100
    ),
    "cannot hold its band"
  )

  expect_equal(as.numeric(P[25:48]), d$flexible[25:48])
  expect_equal(attr(P, "infeasible_windows"), d$datetime[25])
  expect_false(isTRUE(all.equal(as.numeric(P[1:24]), d$flexible[1:24])))
  expect_false(isTRUE(all.equal(as.numeric(P[49:72]), d$flexible[49:72])))

  clamp <- attr(P, "tank_clamp_kwh")
  path <- tank_path(P, d, init = 3)
  expect_lt(clamp[49], 0)
  expect_equal(clamp[49], path[49] - 3 * 20 / 100)
  expect_equal(clamp[-49], rep(0, 71))

  # With the third day removed the fallback window is the last: nothing is
  # clamped.
  P2 <- suppressMessages(add_tank_optimization(
    d[1:48, ],
    "cost",
    Tcap = 3,
    Pc = 1,
    SOCmin = 20,
    SOCini = 100
  ))
  expect_equal(attr(P2, "infeasible_windows"), d$datetime[25])
  expect_equal(attr(P2, "tank_clamp_kwh"), rep(0, 48))
})

test_that("every window ends at least as full as it started", {
  # Prices that reward ending each window empty: cheap first, dear last
  d <- tank_data(days = 2, cheap_hours = 0:11)
  d$price_imported[lubridate::hour(d$datetime) >= 18] <- 1
  P <- add_tank_optimization(d, "cost", Tcap = 6, Pc = 3, SOCini = 50)

  path <- tank_path(P, d, init = 3)
  expect_gte(path[25], path[1] - rounding_tol)
  expect_gte(path[49], path[25] - rounding_tol)

  # The solver's own (unrounded) solution meets it within its tolerance
  P_window <- tank_solve_window(
    G = d$production[1:24],
    L = d$static[1:24],
    Q = d$heat_demand[1:24],
    cop = d$cop[1:24],
    PI = d$price_imported[1:24],
    PE = d$price_exported[1:24],
    import_capacity = Inf,
    export_capacity = Inf,
    S0 = 3,
    Smin = 0,
    Smax = 6,
    Pc = 3,
    k = 1,
    dt = 1,
    objective = "cost"
  )
  window_path <- tank_level_path(P_window, d$heat_demand[1:24], d$cop[1:24], 3, 1, 1)
  expect_gte(window_path[25], 3 - optimization_solution_tolerance())
})

test_that("`SOCini = NULL` is `SOCmax`, and inputs out of range stop", {
  d <- tank_data()
  expect_equal(
    add_tank_optimization(d, "cost", Tcap = 3, Pc = 3),
    add_tank_optimization(d, "cost", Tcap = 3, Pc = 3, SOCini = 100)
  )
  expect_error(
    add_tank_optimization(d, Tcap = 3, Pc = 3, SOCmin = 50, SOCini = 20),
    "SOCini"
  )
  expect_error(add_tank_optimization(d, Tcap = 0, Pc = 3), "Tcap")
  expect_error(add_tank_optimization(d, Tcap = 3, Pc = -1), "Pc")
  expect_error(add_tank_optimization(d, Tcap = 3, Pc = 3, SOCmax = 120), "SOCmax")
  expect_error(
    add_tank_optimization(d, Tcap = 3, Pc = 3, SOCmin = 60, SOCmax = 60),
    "SOCmin"
  )
  expect_error(add_tank_optimization(d, Tcap = 3, Pc = 3, loss = -1), "loss")
  expect_error(add_tank_optimization(d, "capacity", Tcap = 3, Pc = 3), "capacity")
  expect_error(add_tank_optimization(d, 1.5, Tcap = 3, Pc = 3), "opt_objective")
  expect_error(
    add_tank_optimization(select(d, -cop), Tcap = 3, Pc = 3),
    "cop"
  )
  expect_error(
    add_tank_optimization(mutate(d, heat_demand = -1), Tcap = 3, Pc = 3),
    "heat_demand"
  )
  expect_error(
    add_tank_optimization(mutate(d, flexible = NA_real_), Tcap = 3, Pc = 3),
    "flexible"
  )
})

test_that("15 minutes and hourly give the same daily totals", {
  hourly <- tank_data()
  quarter <- tank_data(resolution = 15)

  for (objective in list("grid", "cost", 0.5)) {
    for (loss in c(0, 1)) {
      P_h <- add_tank_optimization(hourly, objective, Tcap = 3, Pc = 3, loss = loss)
      P_q <- add_tank_optimization(quarter, objective, Tcap = 3, Pc = 3, loss = loss)

      # The loss is charged on the level at the start of each slot, so where
      # the level moves within an hour four quarters lose a little less than
      # one hour does: up to 0.03 kWh a day on this fixture with a loss, when
      # the tank drains through whole hours.
      tol <- if (loss > 0) 0.03 else 0.01
      expect_equal(sum(P_q * 0.25), sum(P_h), tolerance = tol / 3)
      expect_equal(
        sum(quarter$cop * P_q * 0.25),
        sum(hourly$cop * P_h),
        tolerance = tol
      )
    }
  }
})

test_that("`grid` and `cost` each win on their own objective", {
  static <- rep(0.5, 24)
  static[18:21] <- 3
  for (args in list(
    list(Tcap = 3, Pc = 3, SOCini = 100),
    list(Tcap = 30, Pc = 1, SOCini = 0)
  )) {
    d <- tank_data(static = static)
    P_grid <- do.call(add_tank_optimization, c(list(d, "grid"), args))
    P_cost <- do.call(add_tank_optimization, c(list(d, "cost"), args))
    P_mix <- do.call(add_tank_optimization, c(list(d, 0.5), args))

    expect_lte(sum((d$static + P_grid)^2), sum((d$static + P_cost)^2) + 0.05)
    expect_lte(net_cost(P_cost, d), net_cost(P_grid, d) + 0.005)
    expect_length(P_mix, 24)
    expect_gte(min(P_mix), 0)
  }
})

test_that("`get_tank_level()` closes the heat balance", {
  d <- tank_data()
  P <- add_tank_optimization(d, "cost", Tcap = 3, Pc = 3, loss = 1)
  expect_equal(tank_balance_gap(P, d, init = 3, loss = 1), 0, tolerance = 1e-9)

  level <- get_tank_level(P, d$heat_demand, d$cop, init = 3, loss = 1)
  expect_equal(level, round(tank_path(P, d, 3, loss = 1)[1:24], 2))
  expect_gte(min(level), 0 - 0.01 - rounding_tol)
  expect_lte(max(level), 3 + 0.01 + rounding_tol)

  # The solver's level: the rounded power stays within rounding of it
  P_window <- tank_solve_window(
    G = d$production,
    L = d$static,
    Q = d$heat_demand,
    cop = d$cop,
    PI = d$price_imported,
    PE = d$price_exported,
    import_capacity = Inf,
    export_capacity = Inf,
    S0 = 3,
    Smin = 0,
    Smax = 3,
    Pc = 3,
    k = 0.99,
    dt = 1,
    objective = "cost"
  )
  solver_level <- tank_path(P_window, d, 3, loss = 1)
  expect_lte(max(abs(tank_path(P, d, 3, loss = 1) - solver_level)), rounding_tol)

  # Fixture 3: the clamp closes the balance across the fallback window
  heat_demand <- rep(1, 72)
  heat_demand[24 + 9:11] <- 5
  d3 <- tank_data(days = 3, heat_demand = heat_demand)
  P3 <- suppressMessages(
    add_tank_optimization(d3, "cost", Tcap = 3, Pc = 1, SOCmin = 20, loss = 1)
  )
  clamp <- attr(P3, "tank_clamp_kwh")
  expect_equal(
    tank_balance_gap(P3, d3, init = 3, loss = 1, clamp = clamp),
    0,
    tolerance = 1e-9
  )
  level3 <- get_tank_level(
    P3,
    d3$heat_demand,
    d3$cop,
    init = 3,
    loss = 1,
    clamp = clamp
  )
  expect_equal(level3[49], 3 * 20 / 100)
  expect_lt(min(level3[25:48]), 3 * 20 / 100)
})

test_that("a clamp between windows tops the tank up at the next window start", {
  # Two windows 06:00-18:00; the slots from 18:00 to 06:00 run `flexible`,
  # which draws nothing, against a demand that empties the tank.
  d <- tank_data(days = 3, flexible = 0)[1:54, ]
  P <- add_tank_optimization(
    d,
    "grid",
    Tcap = 10,
    Pc = 3,
    SOCmin = 20,
    window_start_hour = 6,
    flex_window_hours = 12
  )

  expect_length(attr(P, "infeasible_windows"), 0)
  expect_equal(as.numeric(P[c(1:6, 19:30, 43:54)]), rep(0, 30))

  clamp <- attr(P, "tank_clamp_kwh")
  expect_lt(clamp[31], 0)
  expect_equal(clamp[-31], rep(0, 53))

  level <- get_tank_level(P, d$heat_demand, d$cop, init = 10, clamp = clamp)
  expect_equal(level[31], 10 * 20 / 100)
  expect_equal(
    tank_balance_gap(P, d, init = 10, clamp = clamp),
    0,
    tolerance = 1e-9
  )
})

test_that("the compressor does not run where the COP is 0, even with PV surplus", {
  cop <- rep(3, 24)
  cop[11:15] <- 0
  production <- rep(0, 24)
  production[10:16] <- 5
  d <- tank_data(cop = cop, production = production)
  P <- add_tank_optimization(d, "grid", Tcap = 6, Pc = 3)

  expect_equal(as.numeric(P[11:15]), rep(0, 5))
  expect_gt(max(P[c(10, 16)]), 0.34)
})

test_that("`get_tank_level()` follows the balance and validates its inputs", {
  expect_equal(
    get_tank_level(c(1, 0, 0), heat_demand = 1, cop = 3, init = 2),
    c(2, 4, 3)
  )
  # 2 %/hour keeps 0.98^0.25 of the level per 15-minute slot
  expect_equal(
    get_tank_level(rep(0, 5), 0, 3, init = 10, loss = 2, time_resolution = 15)[5],
    round(10 * 0.98, 2)
  )
  expect_equal(
    get_tank_level(c(0, 0, 0), 0, 3, init = 5, clamp = c(0, 2, 0)),
    c(5, 3, 3)
  )
  expect_error(get_tank_level(1, 1, 3, init = NA), "init")
  expect_error(get_tank_level(1, 1, 3, init = 0, loss = -1), "loss")
  expect_error(get_tank_level(c(1, 1), c(1, 1, 1), 3, init = 0), "heat_demand")
})
