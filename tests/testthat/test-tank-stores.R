library(dplyr)

# Stores behind one compressor: `add_tank_optimization(stores = )`. The
# fixtures follow Part 1 of the FLEX+ plan "Heat pump as one compressor"
# (`docs/plans/heatpump-one-compressor-flextools.md`).

# The tank columns of `d`, suffixed for each store; `heat_demand`, `cop` and
# `flexible` named in `...` replace a store's column, as `store = value`
store_data <- function(d, stores, heat_demand = list(), cop = list(), flexible = list()) {
  for (s in stores) {
    d[[paste0("heat_demand_", s)]] <- heat_demand[[s]] %||% d$heat_demand
    d[[paste0("cop_", s)]] <- cop[[s]] %||% d$cop
    d[[paste0("flexible_", s)]] <- flexible[[s]] %||% d$flexible
  }
  select(d, -heat_demand, -cop, -flexible)
}

`%||%` <- function(x, y) if (is.null(x)) y else x

# A store's heat balance from its own attributes: the level `get_tank_level()`
# gives
store_level <- function(P, d, s, init, loss = 0, resolution = 60) {
  demand <- attr(P, "house_heat_kw")[[s]] %||% d[[paste0("heat_demand_", s)]]
  backup <- attr(P, "backup_kw")[[s]] + (attr(P, "gas_backup_kw")[[s]] %||% 0)
  get_tank_level(
    attr(P, "power_kw")[[s]], demand, d[[paste0("cop_", s)]],
    init = init, loss = loss, time_resolution = resolution, backup = backup
  )
}

# Rounding the power to 2 decimals moves the level by at most half a
# rounding step of heat per slot: 0.005 * cop * dt.
rounding_tol <- 0.005 * 3 + 1e-9


test_that("the stores' summed compressor power never passes `Pc`", {
  # Two 6 kWh stores of 1 kW of heat each at a COP of 3 behind a 2 kW
  # compressor. Alone, each would draw the whole 2 kW in the cheap hours.
  d <- tank_data()
  d2 <- store_data(d, c("dhw", "space_heating"))
  for (objective in list("cost", "grid", 0.5)) {
    P <- add_tank_optimization(
      d2, objective, Tcap = 6, Pc = 2, stores = c("dhw", "space_heating")
    )
    power <- attr(P, "power_kw")
    expect_named(power, c("dhw", "space_heating"))
    expect_equal(as.numeric(P), power$dhw + power$space_heating, tolerance = 1e-12)
    expect_lte(max(P), 2 + 1e-9)
    expect_lte(max(power$dhw + power$space_heating), 2 + 1e-9)
    for (s in names(power)) {
      expect_equal(attr(P, "unmet_heat_kw")[[s]], rep(0, 24))
      expect_equal(attr(P, "backup_kw")[[s]], rep(0, 24))
      level <- store_level(P, d2, s, init = 6)
      expect_equal(attr(P, "level_kwh")[[s]], level)
      expect_gte(min(level), -0.01 - rounding_tol)
      expect_lte(max(level), 6 + rounding_tol)
    }
  }
  # Under "cost" the compressor reaches its nameplate in the cheap hours,
  # where two separate heat pumps would draw twice it
  P <- add_tank_optimization(d2, "cost", Tcap = 6, Pc = 2, stores = c("dhw", "space_heating"))
  expect_equal(max(P), 2)
  P_alone <- add_tank_optimization(d, "cost", Tcap = 6, Pc = 2)
  expect_gt(max(2 * P_alone), 2)
})

test_that("one store reproduces the form without `stores`", {
  cases <- list(
    one_tank = list(tank_data(), list("cost", Tcap = 3, Pc = 3, SOCini = 100)),
    backup_grid = list(peak_data(), list("grid", Tcap = 3, Pc = 1, Pb = 3)),
    backup_cost = list(peak_data(), list("cost", Tcap = 3, Pc = 1, Pb = 3)),
    backup_w05 = list(peak_data(), list(0.5, Tcap = 3, Pc = 1, Pb = 3)),
    gas = list(peak_data(), list("cost", Tcap = 3, Pc = 1, Pg = 3)),
    house_grid = list(peak_data(), list("grid", Tcap = 3, Pc = 1, Hcap = 9, Hloss = 0)),
    house_cost = list(peak_data(), list("cost", Tcap = 3, Pc = 1, Hcap = 9, Hloss = 0))
  )
  for (key in names(cases)) {
    d <- cases[[key]][[1]]
    args <- cases[[key]][[2]]
    P0 <- suppressMessages(do.call(add_tank_optimization, c(list(d), args)))
    P1 <- suppressMessages(do.call(
      add_tank_optimization,
      c(list(store_data(d, "x")), args, list(stores = "x"))
    ))
    expect_lte(max(abs(as.numeric(P1) - as.numeric(P0))), 0.01, label = key)
    expect_equal(attr(P1, "power_kw")$x, as.numeric(P1), label = key)
    for (part in setdiff(names(attributes(P0)), "names")) {
      expect_lte(
        max(abs(attr(P1, part)$x - attr(P0, part))), 0.01,
        label = paste(key, part)
      )
    }
    expect_setequal(
      names(attributes(P1)),
      c(names(attributes(P0)), "power_kw", "level_kwh")
    )
    expect_equal(
      attr(P1, "level_kwh")$x,
      store_level(P1, store_data(d, "x"), "x", init = 3),
      label = key
    )
  }
})

test_that("a store that holds nothing pre-heats the house behind it", {
  # No buffer: the compressor heats the house directly, and does so in the
  # last cheap hour, as far as the house holds
  d <- tank_data(cheap_hours = 0:5)
  d1 <- store_data(d, "space_heating")
  for (stores in list(NULL, "space_heating")) {
    P <- if (is.null(stores)) {
      add_tank_optimization(d, "cost", Tcap = 0, Pc = 2, Hcap = 5, Hloss = 2)
    } else {
      add_tank_optimization(
        d1, "cost", Tcap = 0, Pc = 2, Hcap = 5, Hloss = 2, stores = stores
      )
    }
    one <- function(x) if (is.list(x)) x$space_heating else x
    power <- as.numeric(P)
    house_heat <- one(attr(P, "house_heat_kw"))
    house_level <- one(attr(P, "house_level_kwh"))
    expect_equal(one(attr(P, "unmet_heat_kw")), rep(0, 24))
    expect_equal(max(house_level), 5)
    expect_gt(house_level[7], 0)
    expect_equal(power[6], 2)
    # The compressor's heat is the heat into the house, slot by slot up to
    # half a rounding step of the power, and over the day up to one
    expect_lte(max(abs(d$cop * power - house_heat)), 0.03 + 1e-9)
    expect_lte(abs(sum(d$cop * power - house_heat)), 0.03 + 1e-9)
    # It holds nothing: its level is 0 up to the rounding of the power
    level <- get_tank_level(power, house_heat, d$cop, init = 0)
    expect_lte(max(abs(level)), 0.02)
    if (!is.null(stores)) {
      expect_equal(attr(P, "level_kwh")$space_heating, level)
    }
  }
})

test_that("a store's house holds heat only for its own demand ahead", {
  # Summer: hot water all day, no space heating, a negative price before
  # 06:00 on each of three days. The hot-water store has no house; the
  # space-heating store's house takes no heat, and from the second day on
  # the compressor makes the hot water only.
  d <- tank_data(days = 3, heat_demand = 0.5, flexible = 0, cheap_hours = integer(0))
  d$price_imported[lubridate::hour(d$datetime) %in% 0:5] <- -0.05
  d2 <- store_data(
    d, c("dhw", "space_heating"),
    heat_demand = list(space_heating = rep(0, 72)),
    cop = list(dhw = rep(2.5, 72))
  )
  P <- suppressMessages(add_tank_optimization(
    d2, "cost", Tcap = c(3, 2), Pc = 2, Hcap = c(NA, 30), Hloss = c(NA, 3),
    stores = c("dhw", "space_heating")
  ))
  expect_named(attr(P, "house_level_kwh"), "space_heating")
  expect_equal(attr(P, "house_heat_kw")$space_heating, rep(0, 72))
  expect_equal(attr(P, "house_level_kwh")$space_heating, rep(0, 72))
  expect_equal(sum(P[25:72]), 2 * 24 * 0.5 / 2.5, tolerance = 0.02)
})

test_that("a store that holds nothing follows the demand, up to its share of `Pc`", {
  heat_demand <- rep(1, 24)
  heat_demand[18] <- 9
  d <- tank_data(heat_demand = heat_demand)
  for (objective in list("cost", "grid")) {
    P <- suppressMessages(add_tank_optimization(d, objective, Tcap = 0, Pc = 2))
    unmet <- attr(P, "unmet_heat_kw")
    # The compressor makes the demand, 2 kW short of 3 in the peak
    expect_equal(as.numeric(P[18]), 2)
    expect_equal(as.numeric(P[-18]), heat_demand[-18] / 3, tolerance = 0.03)
    expect_equal(sum(d$cop * P) + sum(unmet), sum(heat_demand), tolerance = 0.01)
    expect_equal(unmet[18], 3, tolerance = 0.01)
    expect_equal(unmet[-18], rep(0, 23))

    # A backup heater makes the peak's 3 kW
    P <- add_tank_optimization(d, objective, Tcap = 0, Pc = 2, Pb = 4)
    expect_equal(attr(P, "backup_kw")[18], 3, tolerance = 0.01)
    expect_equal(attr(P, "backup_kw")[-18], rep(0, 23))
    expect_equal(attr(P, "unmet_heat_kw"), rep(0, 24))
  }
})

test_that("two stores that hold nothing share the compressor", {
  # 4.5 kW of heat on each store at 09:00: 1.5 kW of compressor each, 3 kW
  # together, against a 2 kW compressor
  d <- tank_data()
  demand <- rep(1, 24)
  demand[10] <- 4.5
  d2 <- store_data(
    d, c("dhw", "space_heating"),
    heat_demand = list(dhw = demand, space_heating = demand)
  )
  for (objective in list("cost", "grid")) {
    P <- suppressMessages(add_tank_optimization(
      d2, objective, Tcap = 0, Pc = 2, stores = c("dhw", "space_heating")
    ))
    expect_equal(as.numeric(P[10]), 2)
    expect_lte(max(P), 2 + 1e-9)
    unmet <- attr(P, "unmet_heat_kw")
    # 9 kW of heat against 6 from the compressor, split between the stores
    expect_equal(unmet$dhw[10] + unmet$space_heating[10], 3, tolerance = 0.01)
    expect_equal(sum(unmet$dhw[-10] + unmet$space_heating[-10]), 0)
    for (s in c("dhw", "space_heating")) {
      expect_lte(max(abs(attr(P, "level_kwh")[[s]])), 0.02)
    }
  }
})

test_that("a store's COP of 0 stops its compressor, not the other store's", {
  cop_dhw <- rep(3, 24)
  cop_dhw[11:15] <- 0
  production <- rep(0, 24)
  production[10:16] <- 5
  d <- tank_data(production = production)
  d2 <- store_data(d, c("dhw", "space_heating"), cop = list(dhw = cop_dhw))
  P <- add_tank_optimization(
    d2, "grid", Tcap = 6, Pc = 3, stores = c("dhw", "space_heating")
  )
  power <- attr(P, "power_kw")
  expect_equal(power$dhw[11:15], rep(0, 5))
  expect_gt(max(power$space_heating[11:15]), 0.34)
  expect_lte(max(P), 3 + 1e-9)
  expect_equal(attr(P, "unmet_heat_kw")$dhw, rep(0, 24))
})

test_that("15 minutes and hourly give the same daily totals per store", {
  hourly <- store_data(tank_data(), c("dhw", "space_heating"))
  quarter <- store_data(tank_data(resolution = 15), c("dhw", "space_heating"))
  for (objective in list("cost", "grid")) {
    P_h <- add_tank_optimization(
      hourly, objective, Tcap = 6, Pc = 2, loss = 1, stores = c("dhw", "space_heating")
    )
    P_q <- add_tank_optimization(
      quarter, objective, Tcap = 6, Pc = 2, loss = 1, stores = c("dhw", "space_heating")
    )
    # As for one tank: the loss is charged on the level at the start of each
    # slot, so four quarters lose a little less than one hour where the level
    # moves within it, and each run rounds its own slots. Up to 0.015 kWh of
    # electricity a day (0.045 kWh of heat) on this fixture.
    for (s in c("dhw", "space_heating")) {
      p_h <- attr(P_h, "power_kw")[[s]]
      p_q <- attr(P_q, "power_kw")[[s]]
      expect_lte(abs(sum(p_q * 0.25) - sum(p_h)), 0.02, label = s)
      expect_lte(abs(sum(3 * p_q * 0.25) - sum(3 * p_h)), 0.06, label = s)
      expect_equal(attr(P_h, "unmet_heat_kw")[[s]], rep(0, 24), label = s)
      expect_equal(attr(P_q, "unmet_heat_kw")[[s]], rep(0, 96), label = s)
    }
  }
})

test_that("the stores form checks its inputs", {
  d2 <- store_data(tank_data(), c("dhw", "space_heating"))
  run <- function(...) {
    add_tank_optimization(d2, "cost", Tcap = 6, Pc = 2, ..., stores = c("dhw", "space_heating"))
  }
  expect_error(
    add_tank_optimization(d2, "cost", Tcap = c(6, 6, 6), Pc = 2, stores = c("dhw", "space_heating")),
    "`Tcap`.*`stores`"
  )
  expect_error(run(SOCmin = c(10, 20, 30)), "`SOCmin`.*`stores`")
  expect_error(run(Pb = c(1, 2, 3)), "`Pb`.*`stores`")
  expect_error(
    add_tank_optimization(
      select(d2, -heat_demand_dhw), Tcap = 6, Pc = 2, stores = c("dhw", "space_heating")
    ),
    "heat_demand_dhw"
  )
  expect_error(
    add_tank_optimization(
      mutate(d2, heat_demand = 1), Tcap = 6, Pc = 2, stores = c("dhw", "space_heating")
    ),
    "`heat_demand`"
  )
  expect_error(
    add_tank_optimization(d2, Tcap = 6, Pc = 2, stores = c("dhw", "dhw")),
    "`stores`.*`dhw`"
  )
  expect_error(
    add_tank_optimization(d2, Tcap = 6, Pc = 2, stores = c("dhw", "Space heating")),
    "`stores`"
  )
  expect_error(add_tank_optimization(d2, Tcap = 6, Pc = 2, stores = character(0)), "`stores`")
  expect_error(run(SOCmin = c(NA, 10)), "`SOCmin`.*`dhw`")
  expect_error(run(Pb = c(1, 0), Pg = c(1, 0)), "`Pb`.*`Pg`.*`dhw`")
  expect_error(run(Hcap = c(NA, 5), Hloss = c(NA, NA)), "`Hcap`.*`Hloss`.*`space_heating`")
  expect_error(run(Pb = c(-1, 0)), "`Pb`.*`dhw`")
  expect_error(
    add_tank_optimization(
      mutate(d2, flexible_dhw = 1.5, flexible_space_heating = 1),
      Tcap = 6, Pc = 2, stores = c("dhw", "space_heating")
    ),
    "flexible_<store>.*2024-01-01 00:00:00"
  )
  # `NA` is no backup or no house on a store, and a band a store that holds
  # nothing does not read
  P <- suppressMessages(add_tank_optimization(
    d2, "cost", Tcap = c(0, 6), Pc = 2,
    SOCmin = c(NA, 10), SOCmax = c(NA, 100), SOCini = c(NA, 100), loss = c(NA, 1),
    Pb = c(2, NA), Hcap = c(NA, 5), Hloss = c(NA, 2),
    stores = c("dhw", "space_heating")
  ))
  expect_named(attr(P, "backup_kw"), c("dhw", "space_heating"))
  expect_named(attr(P, "house_heat_kw"), "space_heating")
  expect_named(attr(P, "house_level_kwh"), "space_heating")
  expect_null(attr(P, "gas_backup_kw"))
  expect_equal(attr(P, "level_kwh")$dhw, store_level(P, d2, "dhw", init = 0))
  expect_lte(max(P), 2 + 1e-9)
})

test_that("`get_tank_level()` and `get_tank_unmet_heat()` take a store that holds nothing", {
  power <- c(1, 0.5, 0, 1)
  heat_demand <- c(3, 3, 1, 2)
  expect_equal(get_tank_level(power, heat_demand, 3, init = 0), rep(0, 4))
  expect_equal(
    get_tank_unmet_heat(power, heat_demand, 3, init = 0, Tcap = 0, SOCmin = 0),
    c(0, 1.5, 1, 0)
  )
  expect_equal(
    get_tank_unmet_heat(
      power, heat_demand, 3, init = 0, Tcap = 0, SOCmin = 0, backup = c(0, 1, 0, 0)
    ),
    c(0, 0.5, 1, 0)
  )
})
