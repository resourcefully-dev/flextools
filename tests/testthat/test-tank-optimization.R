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

# Unrounded level path, start of each slot plus the end of the last one. It
# stops at the inlet temperature (level 0).
tank_path <- function(P, d, init, loss = 0, resolution = 60, backup = 0) {
  dt <- resolution / 60
  tank_level_path(
    P,
    d$heat_demand,
    d$cop,
    init,
    (1 - loss / 100)^dt,
    dt,
    backup
  )
}

# delivered - (demand served + losses + change in level), the demand served
# being what the tank could give above the inlet temperature
tank_balance_gap <- function(P, d, init, loss = 0, resolution = 60, backup = 0) {
  dt <- resolution / 60
  k <- (1 - loss / 100)^dt
  path <- tank_path(P, d, init, loss, resolution, backup)
  n <- length(P)
  backup <- rep_len(backup, n)
  heat_in <- (d$cop * P + backup - d$heat_demand) * dt
  # Below 0 by more than rounding: demand the tank did not serve
  s_raw <- k * path[seq_len(n)] + heat_in
  tol <- 0.005 * max(d$cop, 1) * dt + 1e-9
  not_served <- sum(ifelse(s_raw < -tol, -s_raw, 0))
  delivered <- sum((d$cop * P + backup) * dt)
  served <- sum(d$heat_demand * dt) - not_served
  losses <- sum((1 - k) * path[seq_len(n)])
  delivered - (served + losses + (path[n + 1] - init))
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

test_that("below its comfort floor the tank cools, and that heat is unmet", {
  # Day 2: 5 kW of heat for 3 hours against 3 kW from the compressor, 6 kWh
  # short. The tank holds 3 kWh above the inlet temperature (level 0), 2.4 of
  # them above the comfort floor.
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
    "below its comfort floor"
  )

  expect_equal(attr(P, "backup_kw"), rep(0, 72))
  unmet <- attr(P, "unmet_heat_kw")
  expect_equal(unmet[-(25:48)], rep(0, 48))
  # Everything beyond the 2.4 kWh above the floor is unmet: the 0.6 kWh the
  # tank gives below it, and the 3 kWh it cannot give at all
  expect_equal(sum(unmet), 6 - 2.4, tolerance = 0.02)

  # The level goes below the floor, down to the inlet temperature and not
  # below, and the heat balance closes
  path <- tank_path(P, d, init = 3)
  expect_equal(min(path[25:49]), 0, tolerance = rounding_tol)
  expect_gte(min(path), 0)
  expect_equal(tank_balance_gap(P, d, init = 3), 0, tolerance = 1e-9)
  # Heat is unmet only in slots that end below the floor
  expect_true(all(path[which(unmet > 0) + 1] < 3 * 20 / 100))

  # A shorter peak (2 kWh short against 0.6 kWh above an 80 % floor) cools
  # the tank below its floor without emptying it: 1.4 kWh unmet
  d2 <- d
  d2$heat_demand[24 + 10:11] <- 1
  P2 <- suppressMessages(add_tank_optimization(
    d2, "cost", Tcap = 3, Pc = 1, SOCmin = 80, SOCini = 100
  ))
  expect_equal(sum(attr(P2, "unmet_heat_kw")), 1.4, tolerance = 0.02)
  path2 <- tank_path(P2, d2, init = 3)
  expect_lt(min(path2), 3 * 80 / 100 - 1)
  expect_gt(min(path2), 0)
})

test_that("a grid obligation the heat pump cannot meet is approached, not abandoned", {
  # An export capacity of -5 kW obliges the site to import 5 kW, and so the
  # heat pump to draw them, but the compressor draws at most 3 and the
  # backup heater 1, and the tank must hold the heat.
  d <- tank_data(days = 2, flexible = 5)
  d$export_capacity <- 1000
  d$export_capacity[25:48] <- -5
  expect_message(
    P <- add_tank_optimization(d, "cost", Tcap = 1000, Pc = 3, SOCini = 0, Pb = 1),
    "grid capacity not reachable"
  )
  expect_null(attr(P, "infeasible_windows"))
  # Compressor and backup heater run flat out, missing the obligation by 1 kW
  expect_equal(as.numeric(P[25:48]), rep(3, 24))
  expect_equal(attr(P, "backup_kw")[25:48], rep(1, 24))
  expect_equal(attr(P, "unmet_heat_kw"), rep(0, 48))

  # With a small tank the heat has nowhere to go: the heat pump draws what
  # the tank can still hold, and the level stays in the band
  P_small <- suppressMessages(
    add_tank_optimization(d, "cost", Tcap = 30, Pc = 3, SOCini = 0, Pb = 1)
  )
  B <- attr(P_small, "backup_kw")
  path <- tank_path(P_small, d, init = 0, backup = B)
  expect_lte(max(path), 30 + rounding_tol)
  expect_equal(tank_balance_gap(P_small, d, init = 0, backup = B), 0, tolerance = 1e-9)
})

test_that("an import capacity is missed before heat is left unmet", {
  # A 1 kW import capacity on a 0.5 kW static load leaves the heat pump 0.5 kW,
  # 1.5 kW of heat against a 3 kW demand. The static schedule draws 1 kW, so
  # the capacity may be missed by 0.5 kW, which covers the demand exactly.
  d <- tank_data(heat_demand = 3, static = 0.5, flexible = 1)
  d$import_capacity <- 1
  P <- suppressMessages(
    add_tank_optimization(d, "cost", Tcap = 3, Pc = 3, SOCini = 0)
  )
  expect_lte(max(d$static + P), 1.5 + 0.01)
  expect_equal(attr(P, "unmet_heat_kw"), rep(0, 24))

  # With the static schedule within the capacity, there is nothing to miss
  d$flexible <- 0.5
  P2 <- suppressMessages(
    add_tank_optimization(d, "cost", Tcap = 3, Pc = 3, SOCini = 0)
  )
  expect_lte(max(d$static + P2), 1 + 0.01)
  expect_equal(sum(attr(P2, "unmet_heat_kw")), 24 * 1.5, tolerance = 0.05)
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
  window_path <- tank_level_path(
    P_window$power,
    d$heat_demand[1:24],
    d$cop[1:24],
    3,
    1,
    1
  )
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
  solver_level <- tank_path(P_window$power, d, 3, loss = 1)
  expect_lte(max(abs(tank_path(P, d, 3, loss = 1) - solver_level)), rounding_tol)

  # Fixture 3: the unmet heat closes the balance where the band cannot hold
  heat_demand <- rep(1, 72)
  heat_demand[24 + 9:11] <- 5
  d3 <- tank_data(days = 3, heat_demand = heat_demand)
  P3 <- suppressMessages(
    add_tank_optimization(d3, "cost", Tcap = 3, Pc = 1, SOCmin = 20, loss = 1)
  )
  unmet <- attr(P3, "unmet_heat_kw")
  expect_equal(
    tank_balance_gap(P3, d3, init = 3, loss = 1),
    0,
    tolerance = 1e-9
  )
  level3 <- get_tank_level(
    P3,
    d3$heat_demand,
    d3$cop,
    init = 3,
    loss = 1
  )
  expect_gte(min(level3), 0 - 0.01 - rounding_tol)
})

test_that("slots that run `flexible` leave heat unmet at the inlet temperature", {
  # Two windows 06:00-18:00; the slots from 18:00 to 06:00 run `flexible`,
  # which draws nothing, against a demand that empties the tank.
  d <- tank_data(days = 3, flexible = 0)[1:54, ]
  P <- suppressMessages(add_tank_optimization(
    d,
    "grid",
    Tcap = 10,
    Pc = 3,
    SOCmin = 20,
    window_start_hour = 6,
    flex_window_hours = 12
  ))

  expect_equal(as.numeric(P[43:54]), rep(0, 12))

  # The tank empties overnight, and once it reaches the inlet temperature the
  # demand the `flexible` schedule leaves uncovered is unmet
  U <- attr(P, "unmet_heat_kw")
  expect_gt(sum(U[19:30]), 0)
  expect_equal(U[31:42], rep(0, 12))

  level <- get_tank_level(P, d$heat_demand, d$cop, init = 10)
  expect_lt(min(level), 10 * 20 / 100)
  expect_gte(min(level), 0 - rounding_tol)
  expect_equal(
    tank_balance_gap(P, d, init = 10),
    0,
    tolerance = 1e-9
  )
})

test_that("slots that run `flexible` stop the compressor at the top of the band", {
  # The `flexible` schedule before 06:00 and overnight heats 9 kW into a 1 kW
  # demand
  d <- tank_data(days = 3, flexible = 3)[1:54, ]
  P <- add_tank_optimization(
    d,
    "grid",
    Tcap = 10,
    Pc = 3,
    window_start_hour = 6,
    flex_window_hours = 12
  )

  # The tank starts full: the thermostat holds it there with just the demand
  # (1/3 kW, in steps of 0.01 kW)
  expect_true(all(P[1:6] %in% c(0.33, 0.34)))
  expect_lt(max(P[19:30]), 3)
  expect_null(attr(P, "tank_clamp_kwh"))
  expect_equal(attr(P, "unmet_heat_kw"), rep(0, 54))

  path <- tank_path(P, d, init = 10)
  expect_lte(max(path), 10 + rounding_tol)
  expect_equal(tank_balance_gap(P, d, init = 10), 0, tolerance = 1e-9)
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
  expect_error(get_tank_level(1, 1, 3, init = NA), "init")
  expect_error(get_tank_level(1, 1, 3, init = 0, loss = -1), "loss")
  expect_error(get_tank_level(c(1, 1), c(1, 1, 1), 3, init = 0), "heat_demand")
  # The backup heater adds heat one to one, and the level stops at the inlet
  # temperature
  expect_equal(
    get_tank_level(c(0, 0, 0), 1, 3, init = 1, backup = c(1, 0, 0)),
    c(1, 1, 0)
  )
  expect_error(get_tank_level(1, 1, 3, init = 0, backup = NA_real_), "backup")
})


# Backup heater ----------------------------------------------------------------

# Evening peak: 6 kW of heat for 3 hours against a 1 kW compressor at COP 3
# (3 kW of heat) and a 3 kWh tank. The peak lacks 3 * 3 - 3 = 6 kWh of heat.
peak_data <- function(...) {
  heat_demand <- rep(1, 24)
  heat_demand[18:20] <- 6
  tank_data(heat_demand = heat_demand, ...)
}

test_that("the backup heater stays off while the compressor suffices", {
  production <- rep(0, 24)
  production[10:16] <- 5 # PV surplus the backup heater could soak up
  d <- tank_data(production = production)
  for (objective in list("grid", "cost", 0.5)) {
    P0 <- add_tank_optimization(d, objective, Tcap = 3, Pc = 3)
    P <- add_tank_optimization(d, objective, Tcap = 3, Pc = 3, Pb = 2)
    expect_equal(attr(P, "backup_kw"), rep(0, 24))
    expect_equal(attr(P, "unmet_heat_kw"), rep(0, 24))
    expect_equal(sum(P), sum(P0), tolerance = 0.02)
  }
  # No backup heater: `Pb = 0` and `Pb = NULL` are the function without one
  expect_identical(
    add_tank_optimization(d, "cost", Tcap = 3, Pc = 3, Pb = 0),
    add_tank_optimization(d, "cost", Tcap = 3, Pc = 3)
  )
  expect_error(add_tank_optimization(d, Tcap = 3, Pc = 3, Pb = -1), "Pb")
})

test_that("the backup heater covers what the compressor and the tank cannot", {
  d <- peak_data()
  for (objective in list("grid", "cost", 0.5)) {
    P <- add_tank_optimization(d, objective, Tcap = 3, Pc = 1, Pb = 3)
    B <- attr(P, "backup_kw")
    U <- attr(P, "unmet_heat_kw")

    expect_equal(U, rep(0, 24))
    expect_lte(max(B), 3)
    # The compressor runs flat out through the peak, and the backup heater
    # delivers only the heat it lacks, during the peak
    expect_equal(as.numeric(P[18:20]), rep(1, 3))
    expect_equal(sum(B), 6, tolerance = 0.02)
    expect_equal(B[-(18:20)], rep(0, 21))

    path <- tank_path(P, d, init = 3, backup = B)
    expect_gte(min(path), 0 - rounding_tol)
    expect_lte(max(path), 3 + rounding_tol)
    expect_equal(tank_balance_gap(P, d, init = 3, backup = B), 0, tolerance = 1e-9)
  }
})

test_that("heat is left unmet only beyond the compressor, the backup and the tank", {
  d <- peak_data()
  for (objective in list("grid", "cost", 0.5)) {
    for (Pb in list(NULL, 1)) {
      P <- suppressMessages(
        add_tank_optimization(d, objective, Tcap = 3, Pc = 1, Pb = Pb)
      )
      B <- attr(P, "backup_kw")
      U <- attr(P, "unmet_heat_kw")
      backup_heat <- if (is.null(Pb)) 0 else 3 * Pb

      # Everything the heat pump has runs through the peak (the compressor
      # makes up the rounding of the unmet heat, by up to a rounding step)
      expect_gte(min(P[18:20]), 0.99)
      expect_equal(sum(B[18:20]), backup_heat, tolerance = 0.02)
      expect_equal(sum(U), 6 - backup_heat, tolerance = 0.02)
      expect_equal(U[-(18:20)], rep(0, 21))

      path <- tank_path(P, d, init = 3, backup = B)
      expect_gte(min(path), 0 - rounding_tol)
      # Heat is unmet only where the tank ends the slot empty (floor 0)
      expect_equal(path[which(U > 0) + 1], rep(0, sum(U > 0)), tolerance = 0.02)
      expect_equal(
        tank_balance_gap(P, d, init = 3, backup = B),
        0,
        tolerance = 1e-9
      )
    }
  }
})

test_that("the backup heater runs where the COP is 0, the compressor does not", {
  cop <- rep(3, 24)
  cop[11:15] <- 0
  heat_demand <- rep(1, 24)
  heat_demand[11:15] <- 3 # 15 kWh while the compressor is off, 3 kWh tank
  d <- tank_data(cop = cop, heat_demand = heat_demand)
  for (objective in list("grid", "cost")) {
    P <- add_tank_optimization(d, objective, Tcap = 3, Pc = 3, Pb = 3)
    B <- attr(P, "backup_kw")
    expect_equal(as.numeric(P[11:15]), rep(0, 5))
    expect_equal(sum(B[11:15]), 12, tolerance = 0.02)
    expect_equal(attr(P, "unmet_heat_kw"), rep(0, 24))
    expect_equal(tank_balance_gap(P, d, init = 3, backup = B), 0, tolerance = 1e-9)
  }
})


# Partial windows --------------------------------------------------------------

test_that("the slots before the first window start form a leading window", {
  # A run from 00:00 with windows from 06:00: 00:00-05:45 is a leading window,
  # 06:00-05:45 the next day a full one, and 06:00-23:45 of the second day a
  # trailing one. The `flexible` schedule (5 kW) is outside the compressor's
  # range, so no optimised slot can equal it.
  d <- tank_data(days = 2, resolution = 15, flexible = 5)
  d$heat_demand <- ifelse(lubridate::hour(d$datetime) %in% 2:4, 6, 1)
  P <- suppressMessages(add_tank_optimization(
    d,
    "cost",
    Tcap = 6,
    Pc = 1,
    SOCmin = 20,
    SOCini = 50,
    window_start_hour = 6,
    flex_window_hours = 24,
    Pb = 2
  ))

  expect_false(any(P == d$flexible))

  B <- attr(P, "backup_kw")
  U <- attr(P, "unmet_heat_kw")
  path <- tank_path(P, d, init = 3, resolution = 15, backup = B)
  tol <- 0.005 * max(d$cop, 1) * 0.25 * 4 + 1e-9
  expect_gte(min(path), 6 * 20 / 100 - tol)
  expect_lte(max(path), 6 + tol)
  # The leading window ends at least as full as the run started
  expect_gte(path[25], 3 - tol)
  # Its peak (18 kWh in 3 hours against 3 kW of compressor heat and a tank
  # that must end where it started) needs the backup heater before 06:00
  expect_gt(sum(B[1:24]), 0)
  expect_equal(
    tank_balance_gap(P, d, init = 3, resolution = 15, backup = B),
    0,
    tolerance = 1e-9
  )
})


# Unmet heat of any schedule ---------------------------------------------------

test_that("`get_tank_unmet_heat()` is the attribute of an optimised run", {
  heat_demand <- rep(1, 72)
  heat_demand[24 + 9:11] <- 5
  d <- tank_data(days = 3, heat_demand = heat_demand)
  for (Pb in list(NULL, 0.5)) {
    for (loss in c(0, 1)) {
      P <- suppressMessages(add_tank_optimization(
        d, "cost", Tcap = 3, Pc = 1, SOCmin = 20, loss = loss, Pb = Pb
      ))
      unmet <- get_tank_unmet_heat(
        as.numeric(P),
        d$heat_demand,
        d$cop,
        init = 3,
        Tcap = 3,
        SOCmin = 20,
        loss = loss,
        backup = attr(P, "backup_kw")
      )
      expect_gt(sum(unmet), 0)
      expect_identical(unmet, attr(P, "unmet_heat_kw"))
    }
  }
})

test_that("`get_tank_unmet_heat()` follows its definition and validates its inputs", {
  # A 3 kWh tank with its floor at 1 kWh, emptied by 2 kW for 2 hours: the
  # first hour ends at the floor, the second needs 2 kWh more to stay there
  expect_equal(
    get_tank_unmet_heat(
      c(0, 0, 0), c(2, 2, 0), 3, init = 3, Tcap = 3, SOCmin = 100 / 3
    ),
    c(0, 2, 0)
  )
  # Below the floor, at 15 minutes: 1 kW out of an empty tank is 1 kW unmet,
  # and the backup heater covers it
  expect_equal(
    get_tank_unmet_heat(
      c(0, 0), 1, 3, init = 0, Tcap = 3, SOCmin = 50, time_resolution = 15
    ),
    c(1, 1)
  )
  expect_equal(
    get_tank_unmet_heat(
      c(0, 0), 1, 3, init = 0, Tcap = 3, SOCmin = 50, time_resolution = 15,
      backup = 1
    ),
    c(0, 0)
  )
  expect_error(get_tank_unmet_heat(0, 1, 3, init = 0, Tcap = 0, SOCmin = 0), "Tcap")
  expect_error(get_tank_unmet_heat(0, 1, 3, init = 0, Tcap = 3, SOCmin = 120), "SOCmin")
  expect_error(
    get_tank_unmet_heat(c(0, 0), 1, 3, init = 0, Tcap = 3, SOCmin = 0, backup = c(1, 1, 1)),
    "backup"
  )
})


# Solver failures --------------------------------------------------------------

test_that("`on_failure` turns a solver failure into an error", {
  d <- tank_data(days = 2)
  local_mocked_bindings(
    tank_solve_window = function(...) tank_failure("OSQP: mocked failure")
  )

  expect_error(
    add_tank_optimization(d, "cost", Tcap = 3, Pc = 3, on_failure = "error"),
    "2024-01-01 00:00:00 UTC \\(OSQP: mocked failure\\)"
  )

  # The default runs `flexible` there, with a message
  expect_message(
    P <- add_tank_optimization(d, "cost", Tcap = 3, Pc = 3),
    "solver failed"
  )
  expect_equal(as.numeric(P), d$flexible)

  expect_error(
    add_tank_optimization(d, "cost", Tcap = 3, Pc = 3, on_failure = "stop"),
    "on_failure"
  )
})
