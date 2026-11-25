# Tank optimization (OSQP / HiGHS) ---------------------------------------------
#
# A heat pump charging one thermal tank. The compressor's electric power `P`
# is the decision, the heat demand `Q` discharges the tank, and the COP `cop`
# converts one into the other. A resistive backup heater `B` (COP 1) can add
# heat next to the compressor, or else a gas boiler `H`, whose heat is not
# electric load. Per slot of `dt` hours, with the level `S` in kWh thermal
# above the inlet temperature (level 0):
#
#   S[t] = k * S[t-1] + (cop[t] * P[t] + B[t] + H[t] - Q[t]) * dt
#   Smin <= S[t] <= Smax                              k = (1 - loss/100)^dt
#   0 <= P[t] <= Pc,  P[t] = 0 where cop[t] = 0
#   0 <= B[t] <= Pb   (also where cop[t] = 0)
#   0 <= H[t] <= Pg   (also where cop[t] = 0)
#   S[last slot of a window] >= S[slot before its first]
#
# A space-heating tank may carry the house it heats behind it, a second,
# lossy store: the tank passes `F` to the house, whose level `R` (kWh above
# its setpoint) serves the demand. The house only keeps to its comfort band:
# it has no end rule, and its heat does not count towards the tank's.
#
#   S[t] = k * S[t-1] + (cop[t] * P[t] + B[t] + H[t] - F[t]) * dt
#   R[t] = kh * R[t-1] + (F[t] - Q[t]) * dt            kh = (1 - Hloss/100)^dt
#   0 <= R[t] <= Hcap,  0 <= F[t]
#   S[last slot of a window] >= S[slot before its first]
#
# A window with no schedule that holds this is solved again with a soft
# comfort floor (the level may go down to 0, with the depth below `Smin`
# penalised, plus `U` for the demand the tank cannot serve at 0) and soft grid
# capacities. `B` is a variable only with a backup heater (`Pb > 0`) and `H`
# only with a gas backup (`Pg > 0`), and `F` and `R` only with a house
# (`Hcap > 0`), so without them, and where the band holds, the problem is the
# one without them.
#
# The level carries from one window to the next, so windows are solved in
# order, and every slot no window optimises runs the static `flexible`
# schedule.
#
# One compressor may serve several stores (a hot-water cylinder and a heating
# buffer behind a diverter valve), each with its own balance, band, backup
# and house as above, sharing the compressor's cap within every slot:
#
#   sum_s P_s[t] <= Pc
#
# A store of capacity 0 holds nothing: its heat in equals its heat out.


#' Tank level path (unrounded)
#'
#' The level at the start of each slot, and after the last one. The level
#' never goes below 0, the inlet temperature (a tank there gives no more
#' heat), except by less than half a rounding step of the power.
#'
#' @param power numeric vector, compressor electric power (kW)
#' @param heat_demand numeric vector, heat demand (kW thermal)
#' @param cop numeric vector, coefficient of performance
#' @param init numeric, level before the first slot (kWh thermal)
#' @param k numeric, per-slot retention `(1 - loss/100)^dt`
#' @param dt numeric, slot length in hours
#' @param backup numeric vector, backup heater power (kW, electric = thermal)
#'
#' @return numeric vector of length `length(power) + 1`
#' @keywords internal
#'
tank_level_path <- function(
  power,
  heat_demand,
  cop,
  init,
  k,
  dt,
  backup = 0
) {
  n <- length(power)
  heat_in <- (cop * power + backup - heat_demand) * dt
  # A level below 0 by less than half a rounding step of the power is the
  # power's rounding, which the next slot makes up
  tol <- 0.005 * max(cop, 1) * dt + 1e-9
  level <- numeric(n + 1)
  s <- init
  for (t in seq_len(n)) {
    level[t] <- s
    s <- k * s + heat_in[t]
    if (s < -tol) {
      s <- 0
    }
  }
  level[n + 1] <- s
  level
}


#' Round the compressor power to 2 decimals while tracking the level
#'
#' Rounding each slot on its own drifts the level: a third of a kW rounds to
#' 0.33 in every slot and the tank loses 0.01 kWh of heat per slot and kW of
#' COP. Instead each slot takes the 2-decimal power that brings the level
#' closest to the level the unrounded solution reaches at the end of that slot,
#' so the rounded level never strays further from it than half a rounding step
#' of heat, and never goes above the top of the band. The backup heater is
#' rounded on its own and the compressor makes up its rounding; where the COP
#' is 0, or the compressor is at its cap or off and cannot make it up, the
#' backup heater tracks the level in the compressor's place.
#'
#' @inheritParams tank_level_path
#' @param target numeric vector, level at the end of each slot under the
#'   unrounded power (kWh)
#' @param Pc numeric, compressor cap (kW)
#' @param Pb numeric, backup heater cap (kW), 0 without one
#' @param power numeric vector, the unrounded power (kW). Where the tank ends a
#'   slot empty, the demand it leaves unserved is not for the rounding to make
#'   up: the power there stays within a rounding step of it.
#' @param Smax numeric, top of the band (kWh): where the nearest step would
#'   take the level above it, the power rounds down instead
#'
#' @return list of the rounded `power` and `backup` (kW)
#' @keywords internal
#'
tank_round_power <- function(
  target,
  heat_demand,
  cop,
  init,
  k,
  dt,
  Pc,
  backup = 0,
  Pb = 0,
  power = NULL,
  Smax = Inf
) {
  n <- length(target)
  b_raw <- rep_len(backup, n)
  p_max <- floor(Pc * 100 + 1e-9) / 100
  b_max <- floor(Pb * 100 + 1e-9) / 100
  tol <- 0.005 * max(cop, 1) * dt + 1e-9
  backup <- pmin(pmax(round(rep_len(backup, n), 2), 0), b_max)
  out <- numeric(n)
  s <- init
  for (t in seq_len(n)) {
    step <- tank_round_step(
      s, target[t], heat_demand[t], cop[t], backup[t], b_raw[t],
      if (is.null(power)) NULL else power[t],
      k, dt, p_max, b_max, Smax, tol
    )
    out[t] <- step$power
    backup[t] <- step$backup
    s <- step$level
  }
  list(power = out, backup = backup)
}


#' One slot of [tank_round_power()]
#'
#' @param s numeric, level at the start of the slot (kWh)
#' @param target,heat_demand,cop,backup,b_raw,power numbers, the slot's
#'   values of [tank_round_power()]'s arguments, `backup` rounded and
#'   `b_raw` not; `power` is `NULL` where [tank_round_power()]'s is
#' @param k,dt,Smax as in [tank_round_power()]
#' @param p_max,b_max numeric, the 2-decimal caps of the compressor and the
#'   backup (kW)
#' @param tol numeric, the level below 0 kept as rounding (kWh)
#'
#' @return list of the slot's rounded `power` and `backup` (kW), the
#'   unrounded power the level asks for, `raw` (`NA` where the COP is 0), and
#'   the `level` at the end of the slot (kWh)
#' @keywords internal
#'
tank_round_step <- function(
  s,
  target,
  heat_demand,
  cop,
  backup,
  b_raw,
  power,
  k,
  dt,
  p_max,
  b_max,
  Smax,
  tol
) {
  out <- 0
  raw <- NA_real_
  need <- target - k * s + (heat_demand - backup) * dt
  empty <- !is.null(power) && target <= 1e-9
  if (cop > 0) {
    p <- need / (cop * dt)
    if (empty) {
      p <- min(p, power + 0.01)
    }
    raw <- p
    out <- min(max(round(p, 2), 0), p_max)
    # At its cap (or off) the compressor cannot make up the backup heater's
    # rounding, so the backup heater tracks the level instead: rounded on
    # its own, a backup power on a rounding tie in every slot of a peak
    # drifted a whole step of heat away
    short <- need - cop * out * dt
    if (
      b_max > 0 &&
        ((out >= p_max && short > 0) || (out <= 0 && short < 0))
    ) {
      b <- backup + short / dt
      if (empty) {
        b <- min(b, b_raw + 0.01)
      }
      backup <- min(max(round(b, 2), 0), b_max)
    }
  } else if (b_max > 0) {
    b <- backup + need / dt
    if (empty) {
      b <- min(b, b_raw + 0.01)
    }
    backup <- min(max(round(b, 2), 0), b_max)
  }
  # Rounding to the nearest step may take the level above the top, by up to
  # half a step of heat (0.0175 kWh for 0.01 kW at COP 7 and 15 minutes):
  # there the compressor, then the backup heater, rounds down instead
  room <- Smax - k * s + heat_demand * dt
  if (cop * out + backup > room / dt + 1e-9) {
    if (cop > 0) {
      out <- max(floor((room / dt - backup) / cop * 100 + 1e-9) / 100, 0)
    }
    if (cop * out + backup > room / dt + 1e-9) {
      backup <- max(floor((room / dt - cop * out) * 100 + 1e-9) / 100, 0)
    }
  }
  s <- k * s + (cop * out + backup - heat_demand) * dt
  # A level below 0 by more than rounding is demand the tank did not serve
  if (s < -tol) {
    s <- 0
  }
  list(power = out, backup = backup, raw = raw, level = s)
}


#' Round the compressor power of several stores sharing one compressor
#'
#' [tank_round_power()] for each store, slot by slot, and one rule more:
#' where the stores' rounded powers sum above `Pc` (at 2 decimals), one store
#' rounds down by 0.01 kW in that slot, until the sum is within `Pc`: a store
#' whose level that step of heat leaves at or above its floor, and among them
#' (or among all, when none can) the one with the largest rounding remainder,
#' its rounded power above the power its level asks for. That store's backup
#' tracks the level in its place, as at the compressor's cap, and its level
#' is made up in the next slots as any rounding is.
#'
#' @param target,heat_demand,cop,backup,power numeric matrices, one column per
#'   store, as the vectors of [tank_round_power()]
#' @param init,k,Pb,Smax numeric vectors, one value per store
#' @param Smin numeric vector, each store's floor (kWh)
#' @inheritParams tank_round_power
#'
#' @return list of the rounded `power` and `backup` (kW), matrices with one
#'   column per store
#' @keywords internal
#'
tank_round_stores <- function(
  target,
  heat_demand,
  cop,
  init,
  k,
  dt,
  Pc,
  backup,
  Pb,
  power,
  Smax,
  Smin
) {
  n <- nrow(target)
  m <- ncol(target)
  p_max <- floor(Pc * 100 + 1e-9) / 100
  b_max <- floor(Pb * 100 + 1e-9) / 100
  b_raw <- backup
  out <- matrix(0, n, m)
  tol <- numeric(m)
  for (s in seq_len(m)) {
    tol[s] <- 0.005 * max(cop[, s], 1) * dt + 1e-9
    backup[, s] <- pmin(pmax(round(backup[, s], 2), 0), b_max[s])
  }
  level <- init
  for (t in seq_len(n)) {
    round_store <- function(s, cap) {
      tank_round_step(
        level[s], target[t, s], heat_demand[t, s], cop[t, s], backup[t, s],
        b_raw[t, s], power[t, s], k[s], dt, cap, b_max[s], Smax[s], tol[s]
      )
    }
    steps <- lapply(seq_len(m), round_store, cap = p_max)
    powers <- vapply(steps, `[[`, numeric(1), "power")
    while (sum(powers) > p_max + 1e-9) {
      remainder <- powers - vapply(steps, `[[`, numeric(1), "raw")
      remainder[is.na(remainder) | powers <= 0] <- -Inf
      # A store with heat to spare above its floor gives the step first: the
      # others would leave it unmet
      spare <- vapply(steps, `[[`, numeric(1), "level") - 0.01 * cop[t, ] * dt >=
        Smin - 1e-9
      if (any(spare & is.finite(remainder))) {
        remainder[!spare] <- -Inf
      }
      s <- which.max(remainder)
      steps[[s]] <- round_store(s, round(powers[s] - 0.01, 2))
      powers[s] <- steps[[s]]$power
    }
    for (s in seq_len(m)) {
      out[t, s] <- steps[[s]]$power
      backup[t, s] <- steps[[s]]$backup
      level[s] <- steps[[s]]$level
    }
  }
  list(power = out, backup = backup)
}


#' House level path (unrounded)
#'
#' The level of the house behind a space-heating tank, in kWh above its
#' setpoint, at the start of each slot and after the last one:
#' `R[t] = kh * R[t-1] + (heat[t] - heat_demand[t]) * dt`. Nothing stops it:
#' the house's floor at the setpoint is held by `heat`, the heat the tank
#' passes to it.
#'
#' @param heat numeric vector, heat from the tank to the house (kW thermal)
#' @param heat_demand numeric vector, heat demand (kW thermal)
#' @param init numeric, house level before the first slot (kWh)
#' @param kh numeric, per-slot retention of the house `(1 - Hloss/100)^dt`
#' @param dt numeric, slot length in hours
#'
#' @return numeric vector of length `length(heat) + 1`
#' @keywords internal
#'
tank_house_path <- function(heat, heat_demand, init, kh, dt) {
  n <- length(heat)
  level <- numeric(n + 1)
  r <- init
  for (t in seq_len(n)) {
    level[t] <- r
    r <- kh * r + (heat[t] - heat_demand[t]) * dt
  }
  level[n + 1] <- r
  level
}


#' Round the heat into the house to 2 decimals while tracking its level
#'
#' As [tank_round_power()] does for the compressor: each slot takes the
#' 2-decimal heat that brings the house's level closest to the level the
#' unrounded solution reaches at the end of that slot, rounded down where
#' that would take the house above its top and up where it would take it
#' below its setpoint, whose floor is hard.
#'
#' @inheritParams tank_house_path
#' @param target numeric vector, house level at the end of each slot under the
#'   unrounded heat (kWh)
#' @param Hcap numeric, the house's capacity (kWh)
#'
#' @return numeric vector, the rounded heat from the tank to the house (kW)
#' @keywords internal
#'
tank_round_house <- function(target, heat_demand, init, kh, dt, Hcap) {
  n <- length(target)
  out <- numeric(n)
  r <- init
  for (t in seq_len(n)) {
    f <- max(round((target[t] - kh * r) / dt + heat_demand[t], 2), 0)
    if (kh * r + (f - heat_demand[t]) * dt > Hcap + 1e-9) {
      f <- max(floor(((Hcap - kh * r) / dt + heat_demand[t]) * 100 + 1e-9) / 100, 0)
    }
    if (kh * r + (f - heat_demand[t]) * dt < -1e-9) {
      f <- max(ceiling((heat_demand[t] - kh * r / dt) * 100 - 1e-9) / 100, 0)
    }
    out[t] <- f
    r <- kh * r + (f - heat_demand[t]) * dt
  }
  out
}


#' Price of a kWh of heat in a window's objective
#'
#' The highest marginal value a kW of electric load can take in the window's
#' objective, divided by the lowest positive COP (when below 1): no kWh of
#' compressor heat is worth more than it. For `"cost"` the marginal value is
#' the highest absolute import or (clipped) export price; for `"grid"` the
#' derivative of the squared net flow at its largest, `2 * (max|L - G| + Pc +
#' Pb)`; a weight `w` mixes both as the objective does, and a ramping penalty
#' adds its largest derivative `4 * lambda * Pc`.
#'
#' @inheritParams tank_solve_window
#' @param price numeric, highest absolute electricity price of the window
#' @param scale numeric, the weighted objective's price scale
#'
#' @return positive number, in the objective's units per kW of heat and slot
#' @keywords internal
#'
tank_heat_price <- function(L, G, cop, Pc, Pb, objective, lambda, price, scale) {
  flow <- 2 * (max(abs(L - G)) + Pc + Pb)
  value <- if (identical(objective, "grid")) {
    flow
  } else if (identical(objective, "cost")) {
    price
  } else {
    objective * scale * flow + (1 - objective) * price
  }
  value <- value + 4 * lambda * Pc
  if (!is.finite(value) || value <= 0) {
    value <- 1
  }
  cop_min <- if (any(cop > 0)) min(cop[cop > 0]) else 1
  value / min(1, cop_min)
}


#' Unmet heat of a schedule
#'
#' Per slot, the heat that would have been required to keep the tank at least
#' at its comfort floor: the heat it gives from below the floor, to the demand
#' or to its standing loss, plus the heat demand it cannot serve at all once it
#' is at the inlet temperature (level 0). A gap within half a rounding step of
#' the power is carried to the next slots, where the level climbing back
#' cancels it, and counted once the gaps add up beyond that step.
#'
#' @inheritParams tank_level_path
#' @param Smin numeric, comfort floor (kWh)
#' @param tol numeric, gap ignored (kWh)
#'
#' @return numeric vector, unmet heat per slot (kW thermal), rounded to 2
#'   decimals
#' @keywords internal
#'
tank_unmet_heat <- function(
  power,
  heat_demand,
  cop,
  init,
  k,
  dt,
  backup,
  Smin,
  tol = 0
) {
  n <- length(power)
  unmet <- numeric(n)
  # Heat given from below the floor and not counted yet: a gap within a
  # rounding step waits here, and the level climbing back takes it back, so a
  # rounding wiggle counts nothing while a slow drain (a standing loss of a
  # few Wh per slot) adds up
  pending <- 0
  s <- init
  for (t in seq_len(n)) {
    s_raw <- k * s + (cop[t] * power[t] + backup[t] - heat_demand[t]) * dt
    # Below the floor, and below the inlet temperature (not served at all).
    # The standing loss below the floor is heat the tank gives too.
    pending <- max(pending + min(Smin, s) - min(Smin, s_raw), 0)
    if (pending > tol) {
      # Down to 2 decimals, the rest carried, so the slots add up
      unmet[t] <- floor(pending / dt * 100 + 1e-9) / 100
      pending <- max(pending - unmet[t] * dt, 0)
    }
    s <- if (s_raw < -tol) 0 else s_raw
  }
  round(unmet, 2)
}


#' Least heat a window cannot give at the inlet temperature
#'
#' The heat demand the tank cannot serve at the inlet temperature (level 0)
#' with the compressor, the backup heater and the gas backup at full power
#' whenever the tank has room, the compressor first: no schedule leaves less.
#'
#' @inheritParams tank_solve_window
#' @param ub_P,ub_B,ub_total numeric vectors, caps of the compressor, the
#'   backup heater and their sum (kW)
#' @param ub_H numeric, cap of the gas backup (kW thermal), outside `ub_total`
#'
#' @return numeric vector, heat not served per slot (kW thermal)
#' @keywords internal
#'
tank_min_unmet <- function(Q, cop, S0, k, dt, Smax, ub_P, ub_B, ub_total, ub_H = 0) {
  n <- length(Q)
  Smax <- rep_len(Smax, n)
  ub_H <- rep_len(ub_H, n)
  U <- numeric(n)
  s <- S0
  for (t in seq_len(n)) {
    P <- min(ub_P[t], ub_total[t])
    B <- min(ub_B[t], ub_total[t] - P)
    heat <- min(cop[t] * P + B + ub_H[t], max((Smax[t] - k * s) / dt + Q[t], 0))
    s_next <- k * s + (heat - Q[t]) * dt
    if (s_next < 0) {
      U[t] <- -s_next / dt
      s_next <- 0
    }
    s <- s_next
  }
  U
}


#' A window with no solution
#'
#' @param status character, why the window has no solution
#'
#' @return list with `power = NULL` and the `status`
#' @keywords internal
#'
tank_failure <- function(status) {
  list(power = NULL, backup = NULL, gas = NULL, house = NULL, status = status)
}


#' Solve one tank window
#'
#' One compressor behind one or more stores. Every per-store argument (`Q`,
#' `cop`, `S0`, `Smin`, `Smax`, `k`, `Pb`, `Pg`, `Hcap`, `kh`, `R0`, `S_ref`,
#' `return_weight`, `level_unit`, `power_unit`) takes one value per store
#' (`Q` and `cop` one column per store), and a single store is given as
#' vectors and numbers, as without `stores`. The stores share `Pc`:
#' their compressor powers sum to at most it in every slot.
#'
#' @param G numeric vector, production (kW)
#' @param L numeric vector, static load (kW)
#' @param Q numeric vector, heat demand (kW thermal), or a matrix with one
#'   column per store
#' @param cop numeric vector, coefficient of performance, or a matrix with one
#'   column per store
#' @param PI,PE numeric vectors, import and export prices (EUR/kWh); unused for
#'   `"grid"`
#' @param import_capacity,export_capacity numeric vectors, net grid flow
#'   capacities (kW)
#' @param S0 numeric, level before the first slot, inside the band (kWh)
#' @param Smin,Smax numeric, band (kWh). `Smax = 0` is a store that holds
#'   nothing: its heat in equals its heat out in every slot.
#' @param Pc numeric, compressor cap (kW), shared by the stores
#' @param k numeric, per-slot retention
#' @param dt numeric, slot length (hours)
#' @param objective `"grid"`, `"cost"` or a number strictly between 0 and 1
#' @param lambda numeric, ramping penalty weight on the stores' summed
#'   compressor power
#' @param Pb numeric, backup heater cap (kW); 0 means no backup heater
#' @param Pg numeric, gas backup cap (kW thermal); 0 means no gas backup. Its
#'   heat enters the tank's balance only: not the net flow, nor the capacities.
#' @param soft logical, whether the comfort floor is soft: the level may go
#'   down to the inlet temperature, below the floor at a price
#' @param slack_import_max,slack_export_max numeric vectors, how far each slot
#'   may miss its import / export capacity (kW), from
#'   [optimization_slack_ceiling()]; the capacities are hard where they are 0
#' @param level_unit numeric, the kWh that one unit of the solver's level
#'   stands for. `NULL` (default) is `Smax` on OSQP, so the solver sees the
#'   level between 0 and 1 whatever the tank's size, and 1 (kWh) on HiGHS.
#'   1 is the formulation in kWh that OSQP used up to flextools 1.18.1. A
#'   store with `Smax = 0` takes `power_unit * dt`.
#' @param power_unit numeric, the kW that one unit of the solver's power
#'   stands for, and the kW of one unit of its rows in kW. `NULL` (default)
#'   is `Pc` on OSQP, or the power that fills the band in one slot (`Smax /
#'   dt`) when smaller, so the solver sees the compressor on the same scale
#'   whatever the heat pump's size, and 1 (kW) on HiGHS. 1 is the formulation
#'   in kW that OSQP used up to flextools 1.19.4. A store with `Smax = 0`
#'   takes `Pc`. The rows shared by the stores take the largest.
#' @param Hcap numeric, the house's capacity behind the tank (kWh); 0 means
#'   no house, and `Q` is then the tank's demand. With a house, `Q` is the
#'   house's demand and the tank's is the heat it passes to the house.
#' @param kh numeric, per-slot retention of the house
#' @param R0 numeric, house level before the first slot, within `[0, Hcap]`
#'   (kWh)
#' @param S_ref numeric, the level the window returns to (kWh): it ends at
#'   least this full, within the band and the room `ub_S` leaves at the top.
#'   Defaults to `S0`, the window's own start.
#' @param return_weight numeric, the price of a kWh the window ends below
#'   `S_ref`, in units of [tank_heat_price()]. `NULL` (default) is `2 / (cop *
#'   dt)` at the window's lowest COP (at least 1), between the compressor's
#'   heat and a missed grid capacity (see the body). `20 / dt`, the comfort
#'   floor's, is the price up to flextools 1.22.1.
#'
#' @return list of the unrounded `power`, `backup` and `gas` (kW), the heat
#'   from the tank to the house `house` (kW, `NULL` without a house) and the
#'   `status`; when the window has no solution, `power` is `NULL` and `status`
#'   says why (see `tank_failure()`). With more than one store, `power`,
#'   `backup`, `gas` and `house` are matrices with one column per store
#'   (`house` 0 in the columns of stores without one).
#' @keywords internal
#'
tank_solve_window <- function(
  G,
  L,
  Q,
  cop,
  PI,
  PE,
  import_capacity,
  export_capacity,
  S0,
  Smin,
  Smax,
  Pc,
  k,
  dt,
  objective,
  lambda = 0,
  Pb = 0,
  Pg = 0,
  soft = FALSE,
  slack_import_max = 0,
  slack_export_max = 0,
  level_unit = NULL,
  power_unit = NULL,
  Hcap = 0,
  kh = 1,
  R0 = 0,
  S_ref = S0,
  return_weight = NULL
) {
  n <- length(G)
  Q <- matrix(as.numeric(Q), nrow = n)
  m <- ncol(Q)
  cop <- matrix(as.numeric(cop), nrow = n, ncol = m)
  per_store <- function(x) rep_len(as.numeric(x), m)
  S_ref <- per_store(S_ref)
  S0 <- per_store(S0)
  Smin <- per_store(Smin)
  Smax <- per_store(Smax)
  k <- per_store(k)
  Pb <- per_store(Pb)
  Pg <- per_store(Pg)
  Hcap <- per_store(Hcap)
  kh <- per_store(kh)
  R0 <- per_store(R0)
  import_capacity <- as.numeric(rep_len(import_capacity, n))
  export_capacity <- as.numeric(rep_len(export_capacity, n))
  slack_import_max <- as.numeric(rep_len(slack_import_max, n))
  slack_export_max <- as.numeric(rep_len(slack_export_max, n))
  with_backup <- Pb > 0
  # The gas backup's heat is no electric load: it is in no row or box of the
  # load below, only in the tank's dynamics
  with_gas <- Pg > 0
  # With a house, the tank's demand is the heat `F` it passes to the house,
  # and `Q` is the house's
  with_house <- Hcap > 0
  soft_caps <- any(slack_import_max > 0) || any(slack_export_max > 0)
  # One store and no backup heater bound the heat pump's load by a box on
  # `P`; otherwise a row on the load bounds it (the stores' `P + B` summed)
  load_row <- !soft_caps && (any(with_backup) || m > 1)
  cap_P <- ifelse(cop > 0, Pc, 0)
  zeros <- matrix(0, n, m)

  # The net flow `L + sum(P + B) - G` must stay within the capacities, which
  # for non-negative loads is a box on `sum(P + B)`. A negative capacity is an
  # obligation to flow the other way and moves the box the same way.
  lb_load <- pmax(0, G - L - export_capacity)
  ub_load <- G - L + import_capacity
  if (soft_caps || load_row) {
    if (soft_caps) {
      # Soft capacities: the heat pump's load may miss them by up to the
      # slack ceilings, through explicit rows on `P + B` with penalised slack
      # (below). The boxes take the relaxed envelope.
      cap_lb <- G - L - export_capacity
      cap_ub <- ub_load
      lb_load <- pmax(0, cap_lb - slack_export_max)
      ub_load <- ub_load + slack_import_max
    }
    lb_P <- zeros
    ub_P <- pmax(pmin(cap_P, ub_load), 0)
    ub_B <- zeros
    for (s in which(with_backup)) {
      ub_B[, s] <- pmax(pmin(Pb[s], ub_load), 0)
    }
    # The stores share the compressor's `Pc`
    ub_heat_load <- if (m == 1) ub_P[, 1] + ub_B[, 1] else pmin(Pc, rowSums(ub_P)) + rowSums(ub_B)
    if (any(lb_load > pmin(ub_load, ub_heat_load) + 1e-9)) {
      return(tank_failure("the heat pump's load cannot meet the grid capacities"))
    }
    if (soft_caps) {
      lb_total <- lb_load
      ub_total <- pmax(pmin(ub_load, ub_heat_load), lb_load)
    } else {
      ub_load <- pmax(pmin(ub_load, ub_heat_load), lb_load)
      lb_total <- lb_load
      ub_total <- ub_load
    }
  } else {
    lb_P <- matrix(lb_load, n, 1)
    ub_P <- pmin(cap_P, ub_load)
    if (any(lb_P > ub_P + 1e-9)) {
      return(tank_failure("the heat pump's load cannot meet the grid capacities"))
    }
    ub_P <- pmax(ub_P, lb_P)
    ub_B <- zeros
    lb_total <- lb_P[, 1]
    ub_total <- ub_P[, 1]
  }

  # The power is rounded to 2 decimals afterwards, which moves the level by up
  # to half a step of heat: the solver keeps that much room below the top, so
  # the rounded level stays within the band. Where the tank cannot get down
  # to that room yet (it starts at the top with no demand to draw it), the
  # top is the lowest level it can reach: there the heat pump runs at its
  # least power, which needs no rounding. With several stores the least load
  # is no store's in particular, and each store's reach is its own decay.
  ub_S <- zeros
  S_end <- numeric(m)
  for (s in seq_len(m)) {
    heat_least <- if (m > 1) {
      0
    } else {
      lb_total * if (with_backup[s]) pmax(cop[, s], 1) else cop[, s]
    }
    heat_least <- rep_len(heat_least, n)
    reach <- S0[s]
    for (t in seq_len(n)) {
      reach <- k[s] * reach + (heat_least[t] - Q[t, s]) * dt
      ub_S[t, s] <- min(
        max(Smax[s] - 0.005 * max(cop[t, s], 1) * dt, Smin[s], reach),
        Smax[s]
      )
    }
    # End condition: the window ends at least as full as the reference level
    # (or at the floor, when that is below it), within that room
    S_end[s] <- min(max(Smin[s], S_ref[s]), ub_S[n, s])
  }
  # The house has no end condition: what it holds at the end of the window
  # only has to be within its band, and does not count towards the tank's.
  # Holding it to the level it started with refilled what it lost every
  # window, with no demand to serve.
  # The least unmet heat with a house, or with stores sharing the compressor,
  # is solved for once the rows are built (below)
  least_lp <- soft && (any(with_house) || m > 1)
  if (soft) {
    # Soft comfort floor: the level may go down to the inlet temperature
    # (level 0), with the depth below the floor `D` penalised, and the end
    # condition missed by `R`, penalised too. Unmet heat is only the heat the
    # tank cannot give at the inlet temperature.
    lb_S <- zeros
    if (least_lp) {
      U_cap <- Inf
    } else {
      U_max <- tank_min_unmet(
        Q[, 1], cop[, 1], S0, k, dt,
        Smax = ub_S[, 1],
        ub_P = ub_P[, 1],
        ub_B = ub_B[, 1],
        ub_total = ub_total,
        ub_H = Pg
      )
      U_cap <- sum(U_max) + 1e-6
    }
  } else {
    lb_S <- matrix(rep(Smin, each = n), n, m)
    lb_S[n, ] <- S_end
  }

  # The level enters the solver in units of `level_unit` kWh. On OSQP that is
  # the top of the band: with the level in kWh next to powers in kW, a tank
  # of under about 1 kWh made OSQP's adaptive step size stall at its
  # iteration limit (with no demand to draw the tank and a COP that moves the
  # rounding room slot by slot), on windows that have a solution. Scaled, the
  # level is between 0 and 1 for any tank. Every row and bound on the level
  # (`S`), its depth below the floor (`D`) and the missed end condition (`R`)
  # is in that unit, and so is the price of `D` and `R`. The heat (`P`, `B`,
  # `H`, `U`) is in `power_unit` (below). The power is the solution; the level
  # in kWh follows from it on the exact dynamics below. HiGHS keeps kWh.
  # A store that holds nothing (`Smax = 0`) takes the heat of `power_unit`
  # over one slot.
  use_highs <- identical(objective, "cost") && lambda == 0
  # The powers enter OSQP in units of `power_unit` kW, and so do the rows in
  # kW. With the level scaled and the powers in kW, OSQP failed on a 2 MW
  # heat pump with a 2624 kWh tank ("maximum iterations reached", and
  # "primal infeasible" larger), on windows that have a solution: the problem
  # it solved was not the same at every size. Scaled by the compressor cap
  # too, a site twice the size is the same problem. Where the cap is more
  # than the power that fills the band in one slot, the unit is that power:
  # by the cap, the 0.58 kWh tank of a 4 kW heat pump stalled OSQP's
  # adaptive step size again.
  if (is.null(power_unit)) {
    power_unit <- if (use_highs) 1 else ifelse(Smax > 0, pmin(Pc, Smax / dt), Pc)
  }
  power_unit <- per_store(power_unit)
  if (is.null(level_unit)) {
    level_unit <- if (use_highs) 1 else ifelse(Smax > 0, Smax, power_unit * dt)
  }
  level_unit <- per_store(level_unit)
  # The rows shared by the stores (the balance, the capacities, the shared
  # compressor and the cap on `U`) take the largest store's unit
  row_power_unit <- max(power_unit)
  # The house's level enters OSQP in units of its capacity, as the tank's
  # does in units of its top: a house holds tens of kWh behind a tank that
  # may hold one
  house_unit <- ifelse(use_highs | !with_house, 1, Hcap)

  with_flows <- !identical(objective, "grid")
  # Per store: `P`, `S`, then `B` with a backup heater, `H` with a gas
  # backup, `F` and `W` with a house. Then per store `U`, `D` and `R` with a
  # soft floor, and the slack and the flows, shared.
  idx_P <- idx_S <- idx_B <- idx_H <- idx_F <- idx_W <- vector("list", m)
  idx_U <- idx_D <- idx_R <- rep(list(integer(0)), m)
  nv <- 0
  take <- function(size) {
    idx <- nv + seq_len(size)
    nv <<- nv + size
    idx
  }
  for (s in seq_len(m)) {
    idx_P[[s]] <- take(n)
    idx_S[[s]] <- take(n)
    idx_B[[s]] <- if (with_backup[s]) take(n) else integer(0)
    idx_H[[s]] <- if (with_gas[s]) take(n) else integer(0)
    # `F`, the heat from the tank to the house, and `W`, the house's level
    idx_F[[s]] <- if (with_house[s]) take(n) else integer(0)
    idx_W[[s]] <- if (with_house[s]) take(n) else integer(0)
  }
  if (soft) {
    for (s in seq_len(m)) {
      idx_U[[s]] <- take(n)
      idx_D[[s]] <- take(n)
      idx_R[[s]] <- take(1)
    }
  }
  idx_si <- integer(0)
  idx_se <- integer(0)
  if (soft_caps) {
    idx_si <- take(n)
    idx_se <- take(n)
  }
  idx_I <- integer(0)
  idx_E <- integer(0)
  if (with_flows) {
    idx_I <- take(n)
    idx_E <- take(n)
  }
  all_P <- unlist(idx_P)
  all_B <- unlist(idx_B)
  all_U <- unlist(idx_U)

  A <- Matrix::Diagonal(nv)
  rhs_dyn <- numeric(0)
  for (s in seq_len(m)) {
    lu <- level_unit[s]
    # Dynamics: S_t - k * S_{t-1} - dt * (cop_t * P_t + B_t + H_t + U_t) =
    # -Q_t * dt (+ k * S0 at t = 1), divided by `level_unit`. With a house,
    # the tank gives `F_t` in place of `Q_t`.
    A_dyn <- Matrix::sparseMatrix(
      i = c(
        seq_len(n),
        seq_len(n),
        seq_len(n - 1) + 1,
        if (with_backup[s]) seq_len(n),
        if (with_gas[s]) seq_len(n),
        if (with_house[s]) seq_len(n),
        if (soft) seq_len(n)
      ),
      j = c(
        idx_S[[s]], idx_P[[s]], idx_S[[s]][seq_len(n - 1)],
        idx_B[[s]], idx_H[[s]], idx_F[[s]], idx_U[[s]]
      ),
      x = c(
        rep(1, n),
        -cop[, s] * dt / lu,
        rep(-k[s], n - 1),
        if (with_backup[s]) rep(-dt / lu, n),
        if (with_gas[s]) rep(-dt / lu, n),
        if (with_house[s]) rep(dt / lu, n),
        if (soft) rep(-dt / lu, n)
      ),
      dims = c(n, nv)
    )
    rhs <- if (with_house[s]) numeric(n) else -Q[, s] * dt
    rhs[1] <- rhs[1] + k[s] * S0[s]
    rhs_dyn <- c(rhs_dyn, rhs / lu)
    A <- rbind(A, A_dyn)
    if (with_house[s]) {
      # House: W_t - kh * W_{t-1} - dt * F_t = -Q_t * dt (+ kh * R0 at t = 1),
      # divided by `house_unit`
      A_house <- Matrix::sparseMatrix(
        i = c(seq_len(n), seq_len(n - 1) + 1, seq_len(n)),
        j = c(idx_W[[s]], idx_W[[s]][seq_len(n - 1)], idx_F[[s]]),
        x = c(rep(1, n), rep(-kh[s], n - 1), rep(-dt / house_unit[s], n)),
        dims = c(n, nv)
      )
      rhs_house <- -Q[, s] * dt
      rhs_house[1] <- rhs_house[1] + kh[s] * R0[s]
      rhs_dyn <- c(rhs_dyn, rhs_house / house_unit[s])
      A <- rbind(A, A_house)
    }
  }

  lower <- numeric(0)
  upper <- numeric(0)
  for (s in seq_len(m)) {
    lower <- c(lower, lb_P[, s], lb_S[, s] / level_unit[s])
    upper <- c(upper, ub_P[, s], ub_S[, s] / level_unit[s])
    if (with_backup[s]) {
      lower <- c(lower, rep(0, n))
      upper <- c(upper, ub_B[, s])
    }
    if (with_gas[s]) {
      lower <- c(lower, rep(0, n))
      upper <- c(upper, rep(Pg[s], n))
    }
    if (with_house[s]) {
      # F: no slot passes more than its demand plus filling the whole house,
      # which has no cap of its own. W: between the setpoint and the top, the
      # floor hard.
      lower <- c(lower, rep(0, n), rep(0, n))
      upper <- c(upper, Q[, s] + Hcap[s] / dt, rep(Hcap[s], n) / house_unit[s])
    }
  }
  if (soft) {
    for (s in seq_len(m)) {
      # U: no slot needs more than its demand plus refilling the whole band
      # (and the house). D: at most the floor. R: at most the end level.
      lower <- c(lower, rep(0, n), rep(0, n), 0)
      upper <- c(
        upper,
        if (with_house[s]) Q[, s] + (Smax[s] + Hcap[s]) / dt else Q[, s] + Smax[s] / dt,
        rep(Smin[s], n) / level_unit[s],
        S_end[s] / level_unit[s]
      )
    }
  }
  if (soft_caps) {
    lower <- c(lower, rep(0, 2 * n))
    upper <- c(upper, slack_import_max, slack_export_max)
  }

  # The heat pump's electric load in each slot: every store's `P` and `B`
  load_rows <- function(sign = 1, offset = 0) {
    cols <- c(all_P, all_B)
    list(
      i = offset + rep(seq_len(n), length(cols) / n),
      j = cols,
      x = rep(sign, length(cols))
    )
  }

  if (with_flows) {
    # Balance: I_t - E_t - sum(P_t + B_t) = L_t - G_t
    load <- load_rows(-1)
    A_bal <- Matrix::sparseMatrix(
      i = c(rep(seq_len(n), 2), load$i),
      j = c(idx_I, idx_E, load$j),
      x = c(rep(1, n), rep(-1, n), load$x),
      dims = c(n, nv)
    )
    A <- rbind(A, A_bal)
    lower <- c(lower, rep(0, n), rep(0, n))
    upper <- c(upper, pmax(L - G + ub_total, 0), pmax(G - L - lb_total, 0))
    lower <- c(lower, rhs_dyn, L - G)
    upper <- c(upper, rhs_dyn, L - G)

    # Importing and exporting in the same slot cancels out on the balance, so
    # it is only free while exporting pays no more than importing costs.
    # Clip the export price as the battery's QP does, keeping the problem
    # convex and bounded.
    PE_clipped <- pmin(PE, PI)
    if (any(PE_clipped != PE)) {
      message_once(
        "\u26a0\ufe0f Optimization: export price exceeds import price; clipping for bounded problem."
      )
    }
  } else {
    lower <- c(lower, rhs_dyn)
    upper <- c(upper, rhs_dyn)
  }

  if (soft_caps) {
    # sum(P_t + B_t) - s_i,t <= G_t - L_t + C_imp,t   import capacity, soft
    # sum(P_t + B_t) + s_e,t >= G_t - L_t - C_exp,t   export capacity, soft
    imp <- load_rows()
    ex <- load_rows(offset = n)
    A_cap <- Matrix::sparseMatrix(
      i = c(imp$i, seq_len(n), ex$i, n + seq_len(n)),
      j = c(imp$j, idx_si, ex$j, idx_se),
      x = c(imp$x, rep(-1, n), ex$x, rep(1, n)),
      dims = c(2 * n, nv)
    )
    A <- rbind(A, A_cap)
    lower <- c(lower, rep(-Inf, n), cap_lb)
    upper <- c(upper, cap_ub, rep(Inf, n))
  } else if (load_row) {
    # Capacity box on the heat pump's total load: lb <= sum(P_t + B_t) <= ub
    load <- load_rows()
    A_load <- Matrix::sparseMatrix(
      i = load$i,
      j = load$j,
      x = load$x,
      dims = c(n, nv)
    )
    A <- rbind(A, A_load)
    lower <- c(lower, lb_load)
    upper <- c(upper, ub_load)
  }

  if (m > 1) {
    # The one compressor: sum(P_t) <= Pc, the valve sharing its time between
    # the stores within the slot
    A_pc <- Matrix::sparseMatrix(
      i = rep(seq_len(n), m),
      j = all_P,
      x = rep(1, n * m),
      dims = c(n, nv)
    )
    A <- rbind(A, A_pc)
    lower <- c(lower, rep(0, n))
    upper <- c(upper, rep(Pc, n))
  }

  if (soft) {
    # Per store:
    # S_t + D_t >= Smin            depth below the comfort floor
    # S_n + R >= S_end             end condition, soft, the tank's alone
    # Then, over every store: sum(U) <= the least unmet heat any schedule
    # needs
    for (s in seq_len(m)) {
      A_soft <- Matrix::sparseMatrix(
        i = c(seq_len(n), seq_len(n), n + 1, n + 1),
        j = c(idx_S[[s]], idx_D[[s]], idx_S[[s]][n], idx_R[[s]]),
        x = rep(1, 2 * n + 2),
        dims = c(n + 1, nv)
      )
      A <- rbind(A, A_soft)
      lower <- c(lower, rep(Smin[s], n) / level_unit[s], S_end[s] / level_unit[s])
      upper <- c(upper, rep(Inf, n), Inf)
    }
    A_unmet <- Matrix::sparseMatrix(
      i = rep(1, n * m),
      j = all_U,
      x = rep(1, n * m),
      dims = c(1, nv)
    )
    A <- rbind(A, A_unmet)
    lower <- c(lower, 0)
    upper <- c(upper, U_cap)
  }

  if (least_lp) {
    # The least unmet heat with a house, or with stores sharing the
    # compressor: the greedy schedule of `tank_min_unmet()` has a second store
    # to choose for, so solve for it on the same rows (an LP on HiGHS), and
    # cap `U` at it
    lin <- numeric(nv)
    lin[all_U] <- 1
    least <- highs::highs_solve(
      Q = NULL,
      L = lin,
      lower = lower[seq_len(nv)],
      upper = upper[seq_len(nv)],
      A = A[-seq_len(nv), , drop = FALSE],
      lhs = lower[-seq_len(nv)],
      rhs = upper[-seq_len(nv)],
      control = optimization_highs_options()
    )
    if (
      !identical(least$status_message, "Optimal") ||
        is.null(least$primal_solution)
    ) {
      return(tank_failure(paste0("HiGHS: ", least$status_message)))
    }
    U_least <- sum(pmax(least$primal_solution[all_U], 0))
    upper[length(upper)] <- U_least * (1 + 1e-6) + 1e-6
  }

  scale <- if (with_flows) mean(PI)^2 else 1
  if (!is.finite(scale) || scale <= 0) {
    scale <- 1
  }

  # Heat from the backup heater or the gas backup costs more than any
  # compressor heat, and missing a grid capacity more still. Going below the
  # comfort floor costs more per kWh and slot than any of them per kWh of
  # heat, so the solver runs the compressor first, then the backup heater (or
  # the gas backup), then misses the capacity, and only then lets the tank
  # cool below its floor. The gas backup pays the backup heater's price
  # without its electricity, which it does not draw: the gas itself is not
  # priced. The demand the tank cannot serve at the inlet temperature (`U`)
  # is capped at the least any schedule leaves instead of priced. The heat
  # price takes every store's COP and backup heater.
  if (any(with_backup) || any(with_gas) || soft || soft_caps) {
    heat_price <- tank_heat_price(
      L,
      G,
      as.numeric(cop),
      Pc,
      sum(Pb),
      objective,
      lambda,
      price = if (with_flows) max(abs(c(PI, PE_clipped))) else 0,
      scale = scale
    )
  }
  c_B <- if (any(with_backup)) 2 * heat_price else 0
  c_H <- if (any(with_gas)) 2 * heat_price else 0
  c_U <- 0
  c_D <- if (soft) 20 * heat_price / dt * level_unit else rep(0, m)
  # Missing the end condition costs twice the dearest compressor heat per kWh
  # (`heat_price / (cop * dt)`), so price alone never leaves the tank below
  # its reference level, and less than missing a grid capacity: a kWh drawn
  # from the tank lowers one slot's breach by `1 / (cop * dt)` kW, worth
  # `c_S / (cop * dt)`, five times as much where the COP varies less than
  # fivefold in the window. Sharing the floor's `c_D` instead (80 x
  # `heat_price` per kWh at 15 minutes) kept a full tank closed through a
  # breach. Each store takes its own lowest COP.
  if (soft && is.null(return_weight)) {
    return_weight <- numeric(m)
    for (s in seq_len(m)) {
      cop_low <- if (any(cop[, s] > 0)) max(1, min(cop[cop[, s] > 0, s])) else 1
      return_weight[s] <- 2 / (cop_low * dt)
    }
  }
  c_R <- if (soft) per_store(return_weight) * heat_price * level_unit else rep(0, m)
  c_S <- if (soft_caps) 10 * heat_price else 0

  lambdaMat <- if (lambda > 0) get_lambda_matrix(n) else NULL

  if (use_highs) {
    # Pure LP on HiGHS
    lin <- numeric(nv)
    lin[all_B] <- c_B
    lin[unlist(idx_H)] <- c_H
    lin[all_U] <- c_U
    for (s in seq_len(m)) {
      lin[idx_D[[s]]] <- c_D[s]
      lin[idx_R[[s]]] <- c_R[s]
    }
    lin[c(idx_si, idx_se)] <- c_S
    lin[idx_I] <- PI
    lin[idx_E] <- -PE_clipped
    result <- highs::highs_solve(
      Q = NULL,
      L = lin,
      lower = lower[seq_len(nv)],
      upper = upper[seq_len(nv)],
      A = A[-seq_len(nv), , drop = FALSE],
      lhs = lower[-seq_len(nv)],
      rhs = upper[-seq_len(nv)],
      control = optimization_highs_options()
    )
    if (
      !identical(result$status_message, "Optimal") ||
        is.null(result$primal_solution)
    ) {
      return(tank_failure(paste0("HiGHS: ", result$status_message)))
    }
    x <- as.numeric(result$primal_solution)
  } else {
    P_quad <- Matrix::Matrix(0, nv, nv, sparse = TRUE)
    q <- numeric(nv)
    if (identical(objective, "grid")) {
      # min sum((L - G + sum(P + B))^2) + lambda * sum((sum P_t - sum P_{t-1})^2)
      w_flow <- 1
      q_flow <- 2 * (L - G)
    } else {
      w <- if (identical(objective, "cost")) 0 else objective
      w_flow <- w * scale
      q_flow <- 2 * w * scale * (L - G)
      q[idx_I] <- (1 - w) * PI
      q[idx_E] <- -(1 - w) * PE_clipped
    }
    # The squared net flow couples every electric load (each store's `P` and
    # `B`), and the ramping penalty every store's `P`
    flow_block <- 2 * w_flow * diag(n)
    for (s in seq_len(m)) {
      for (r in seq_len(m)) {
        P_block <- flow_block
        if (lambda > 0) {
          P_block <- P_block + 2 * lambda * if (identical(objective, "grid") || s != r) {
            lambdaMat
          } else {
            lambdaMat + 1e-6 * diag(n)
          }
        }
        P_quad[idx_P[[s]], idx_P[[r]]] <- P_block
        if (with_backup[r]) {
          # The backup heater's load enters the squared net flow like `P`'s
          P_quad[idx_P[[s]], idx_B[[r]]] <- flow_block
          P_quad[idx_B[[r]], idx_P[[s]]] <- flow_block
        }
        if (with_backup[s] && with_backup[r]) {
          P_quad[idx_B[[s]], idx_B[[r]]] <- flow_block
        }
      }
      q[idx_P[[s]]] <- q_flow
      if (with_backup[s]) {
        q[idx_B[[s]]] <- 2 * w_flow * (L - G) + c_B
      }
      q[idx_D[[s]]] <- c_D[s]
      q[idx_R[[s]]] <- c_R[s]
    }
    # The gas backup is no electric load: its price only
    q[unlist(idx_H)] <- c_H
    q[all_U] <- c_U
    q[c(idx_si, idx_se)] <- c_S

    # Every power in units of its store's `power_unit`: its column, its bound
    # and the rows in kW (the balance, the capacities, the shared compressor
    # and the cap on `U`) are divided by it. The level's rows (dynamics,
    # floor, end) are in `level_unit`.
    col_unit <- rep(1, nv)
    for (s in seq_len(m)) {
      col_unit[c(idx_P[[s]], idx_B[[s]], idx_H[[s]], idx_F[[s]], idx_U[[s]])] <- power_unit[s]
    }
    col_unit[c(idx_si, idx_se, idx_I, idx_E)] <- row_power_unit
    row_unit <- c(
      col_unit,
      unlist(lapply(seq_len(m), function(s) rep(1, if (with_house[s]) 2 * n else n))),
      if (with_flows) rep(row_power_unit, n),
      if (soft_caps) rep(row_power_unit, 2 * n) else if (load_row) rep(row_power_unit, n),
      if (m > 1) rep(row_power_unit, n),
      if (soft) c(rep(1, m * (n + 1)), row_power_unit)
    )
    A <- Matrix::Diagonal(x = 1 / row_unit) %*% A %*% Matrix::Diagonal(x = col_unit)
    lower <- lower / row_unit
    upper <- upper / row_unit
    P_quad <- Matrix::Diagonal(x = col_unit) %*% P_quad %*% Matrix::Diagonal(x = col_unit)
    q <- q * col_unit

    # The objective is normalised as with the level in units of
    # `power_unit` kWh, so the price of `D` and `R` per unit of `level_unit`
    # does not set it
    q_ref <- q
    for (s in seq_len(m)) {
      idx_DR <- c(idx_D[[s]], idx_R[[s]])
      q_ref[idx_DR] <- q_ref[idx_DR] / level_unit[s] * power_unit[s]
    }
    obj_scale <- max(max(abs(P_quad)), max(abs(q_ref)), 1e-6)
    sol <- solve_osqp(P_quad / obj_scale, q / obj_scale, A, lower, upper)
    if (
      is.null(sol$x) &&
        identical(sol$result$status_message, "maximum iterations reached")
    ) {
      # A safety net for a window the scaled level still stalls on: OSQP's
      # adaptive step size is what stalled, so solve it once more with a
      # fixed one before the window counts as failed.
      sol <- solve_osqp(
        P_quad / obj_scale, q / obj_scale, A, lower, upper,
        adaptive_rho = FALSE
      )
    }
    if (is.null(sol$x)) {
      return(tank_failure(paste0("OSQP: ", sol$result$status_message)))
    }
    x <- as.numeric(sol$x) * col_unit
  }

  P <- zeros
  B <- zeros
  H <- zeros
  F_house <- zeros
  for (s in seq_len(m)) {
    P[, s] <- pmin(pmax(x[idx_P[[s]]], lb_P[, s]), ub_P[, s])
    if (with_backup[s]) {
      B[, s] <- pmin(pmax(x[idx_B[[s]]], 0), ub_B[, s])
    }
    if (with_gas[s]) {
      H[, s] <- pmin(pmax(x[idx_H[[s]]], 0), Pg[s])
    }
    # The tank's demand: the heat it passes to the house, or else `Q`
    if (with_house[s]) {
      F_house[, s] <- pmax(x[idx_F[[s]]], 0)
    }
    tank_out <- if (with_house[s]) F_house[, s] else Q[, s]

    # OSQP declares an infeasible problem as such, but its accuracy is
    # absolute, so check the power it returns against the band once more on
    # the exact dynamics before trusting it.
    # The level stops at the inlet temperature: the heat `U` the solver lets
    # in there is demand the tank does not serve. On the path it stops there
    # once it would go below by more than half a rounding step, and is kept as
    # rounding within that, so a soft floor has no lower bound left to check:
    # a tank drained from the rounding room below its top lands exactly half a
    # step below 0.
    # OSQP's accuracy is relative on rows scaled by `Smax`, so its overshoot
    # grows with the tank: the check allows 1e-4 of the top (0.01 % SOC, the
    # smallest step of the reported SOC) and at least 0.001 kWh.
    # The house's level is checked the same way, against its own capacity.
    level <- tank_level_path(
      P[, s], tank_out, cop[, s], S0[s], k[s], dt,
      backup = B[, s] + H[, s]
    )[-1]
    tol <- max(1e-3, 1e-4 * Smax[s])
    if (
      (!soft && any(level < Smin[s] - tol)) ||
        any(level > Smax[s] + tol) ||
        (!soft && level[n] < S_end[s] - tol)
    ) {
      return(tank_failure("the solution leaves the tank's band on the exact dynamics"))
    }
    if (with_house[s]) {
      house <- tank_house_path(F_house[, s], Q[, s], R0[s], kh[s], dt)[-1]
      tol_house <- max(1e-3, 1e-4 * Hcap[s])
      if (any(house < -tol_house) || any(house > Hcap[s] + tol_house)) {
        return(tank_failure("the solution leaves the house's band on the exact dynamics"))
      }
    }
  }
  if (m == 1) {
    return(list(
      power = P[, 1],
      backup = B[, 1],
      gas = H[, 1],
      house = if (with_house) F_house[, 1] else NULL,
      status = "optimal"
    ))
  }
  list(power = P, backup = B, gas = H, house = F_house, status = "optimal")
}


#' Optimization windows of the tank, partial ones at both ends included
#'
#' The full windows of `get_flex_windows()`, plus a leading window over the
#' slots before the first window start and a trailing window over the slots
#' after the last full window, which the run cuts short. Each partial window
#' keeps the flexible part (the first `flex_window_hours` of its window) that
#' falls inside the run, so with `flex_window_hours = 24 * window_days` every
#' slot lies in a window.
#'
#' @param dttm_seq datetime vector of the run
#' @param window_days,window_start_hour,flex_window_hours numeric, as in
#'   [add_tank_optimization()]
#'
#' @return tibble with the columns `start` (first optimised slot) and
#'   `flex_idx` (list of the optimised slots), ordered by `start`
#' @keywords internal
#'
tank_flex_windows <- function(
  dttm_seq,
  window_days,
  window_start_hour,
  flex_window_hours
) {
  n <- length(dttm_seq)
  full <- get_flex_windows(
    dttm_seq = dttm_seq,
    window_days = window_days,
    window_start_hour = window_start_hour,
    flex_window_hours = flex_window_hours
  )
  resolution <- as.numeric(dttm_seq[2] - dttm_seq[1], units = "mins")
  window_length <- round(window_days * 24 * 60 / resolution)
  flex_length <- round(
    min(flex_window_hours, 24 * window_days) * 60 / resolution
  )

  # First window start: the first full window's, or else the next
  # `window_start_hour` at or after the first slot
  if (nrow(full)) {
    first_start <- full$start[1]
    last_end <- max(full$end)
  } else {
    # On the clock: the day's date and hour are counted on the local clock
    # (as UTC, which has no DST), and a start hour in the spring-forward gap
    # moves to the gap's end
    t0 <- dttm_seq[1]
    clock_start <- lubridate::floor_date(lubridate::force_tz(t0, "UTC"), "day") +
      lubridate::dhours(window_start_hour)
    to_local <- function(clock) {
      lubridate::force_tz(
        clock, lubridate::tz(t0),
        roll_dst = c("boundary", "pre")
      )
    }
    next_start <- to_local(clock_start)
    if (next_start < t0) {
      next_start <- to_local(clock_start + lubridate::ddays(1))
    }
    first_start <- 1 + round(
      as.numeric(difftime(next_start, t0, units = "mins")) / resolution
    )
    last_end <- min(first_start - 1, n)
  }

  partial <- list()
  # Leading window: started before the run, ends at `first_start - 1`
  if (first_start > 1) {
    window_start <- first_start - window_length
    flex_end <- min(window_start + flex_length - 1, first_start - 1, n)
    if (flex_end >= 1) {
      partial[[length(partial) + 1]] <- seq(max(window_start, 1), flex_end)
    }
  }
  # Trailing window: starts after the last full window, cut by the run's end
  if (last_end < n) {
    window_start <- last_end + 1
    partial[[length(partial) + 1]] <- seq(
      window_start,
      min(window_start + flex_length - 1, n)
    )
  }

  windows <- dplyr::bind_rows(
    dplyr::tibble(start = full$start, flex_idx = full$flex_idx),
    dplyr::tibble(
      start = purrr::map_int(partial, ~ as.integer(min(.x))),
      flex_idx = partial
    )
  )
  windows[order(windows$start), ]
}


#' Check a single number argument
#' @keywords internal
tank_check_number <- function(
  x,
  name,
  min = -Inf,
  max = Inf,
  min_open = FALSE,
  where = ""
) {
  ok <- is.numeric(x) && length(x) == 1 && !is.na(x) && is.finite(x) &&
    x <= max && (if (min_open) x > min else x >= min)
  if (!ok) {
    range <- paste0(
      if (min_open) "greater than " else "at least ",
      min,
      if (is.finite(max)) paste0(" and at most ", max) else ""
    )
    stop(sprintf("Error: `%s` must be a number %s%s.", name, range, where), call. = FALSE)
  }
  invisible(x)
}


#' Check a numeric column of `opt_data`
#' @keywords internal
tank_check_column <- function(opt_data, name, non_negative = TRUE) {
  if (!(name %in% names(opt_data))) {
    stop(
      sprintf("Error: `opt_data` must contain a `%s` column.", name),
      call. = FALSE
    )
  }
  x <- opt_data[[name]]
  if (!is.numeric(x) || anyNA(x) || any(!is.finite(x))) {
    stop(
      sprintf("Error: `%s` in `opt_data` must be numeric without NA.", name),
      call. = FALSE
    )
  }
  if (non_negative && any(x < 0)) {
    stop(
      sprintf("Error: `%s` in `opt_data` must not be negative.", name),
      call. = FALSE
    )
  }
  invisible(x)
}


# Public API -------------------------------------------------------------------

#' Heat pump with a thermal tank: optimal compressor profile
#'
#' Optimises the electric power of a heat pump's compressor that charges one
#' thermal tank, which the heat demand discharges, with an optional backup
#' next to it: a resistive heater or a gas boiler. It mirrors
#' [add_battery_optimization()]: the same `opt_data` conventions, objectives
#' (except `"capacity"`), optimization windows and solvers, with the tank in
#' place of the battery. The compressor's
#' heat is `cop * P`, so there is no conversion efficiency: the COP is the
#' conversion. The backup heater turns `B` kW of electricity into `B` kW of heat.
#' The gas backup (a hybrid heat pump's boiler) adds `H` kW of heat that is not
#' electricity. A space-heating tank may have the house it heats behind it, as
#' a second, lossy store (see *The house behind the tank*). One compressor may
#' serve several stores, such as a hot-water cylinder and a heating buffer
#' (see *Stores behind one compressor*).
#'
#' Per slot of `dt` hours, with `S` the level in kWh thermal above the inlet
#' water temperature (level 0, `SOC = 0`) and `k = (1 - loss/100)^dt`:
#'
#' - `S[t] = k * S[t-1] + (cop[t] * P[t] + B[t] + H[t] - heat_demand[t]) * dt`
#' - `Tcap * SOCmin/100 <= S[t] <= Tcap * SOCmax/100`
#' - `0 <= P[t] <= Pc`, and `P[t] = 0` where `cop[t] = 0`
#' - `0 <= B[t] <= Pb`, also where `cop[t] = 0`
#' - `0 <= H[t] <= Pg`, also where `cop[t] = 0`
#' - every window ends at least as full as `Tcap * SOCini/100`, the reference
#'   level (or as full as its top allows).
#'
#' `B` (the backup heater) is a variable only with `Pb > 0`, and `H` (the gas
#' backup) only with `Pg > 0`; a tank has one backup at most. The comfort floor
#' `SOCmin` is **soft**: when the compressor and the backup cannot hold
#' it, the tank keeps giving heat below it, cooling down to the inlet
#' temperature (level 0) at most, as a real tank would. The **unmet heat** is
#' the heat that would have been required to keep the tank at least at its
#' comfort floor: the heat it gives from below the floor, plus the demand it
#' cannot serve at all once it is at the inlet temperature. It is positive
#' while the level falls below the floor and 0 while it recovers, so it is a
#' missing energy, not a measure of how long or how far the tank stays below
#' its floor. Every window therefore has a schedule.
#'
#' The run starts from `Tcap * SOCini/100`, and the level carries from one
#' window to the next. The windows are those of the other optimization
#' functions (one every `window_days` from `window_start_hour`), plus the
#' partial windows the run cuts at both ends: when the first slot is not a
#' window start, the slots up to the first window start form a **leading
#' window**, and the slots after the last full window a **trailing window**,
#' both optimised like any other window (the leading one from `SOCini`, both
#' with the end-of-window rule at their last slot). Where a window cannot
#' meet a grid capacity, the tank may end it below the reference level, at a
#' price above any compressor heat and below the capacity's, and the next
#' windows fill it back. Only the first
#' `flex_window_hours` of every window are optimised, so with
#' `flex_window_hours = 24 * window_days` every slot is optimised. The slots
#' past `flex_window_hours` run the `flexible` schedule, without backup.
#'
#' @section The house behind the tank:
#' With `Hcap` and `Hloss`, the building the tank heats is a second store
#' behind it, a "virtual RC battery": `R` is the heat its interior holds above
#' its setpoint (kWh, 0 at the setpoint, `Hcap` at the top of its comfort
#' band), `F >= 0` the heat the tank passes to it (kW), and the heat demand
#' is the house's. Per slot, with `kh = (1 - Hloss/100)^dt`:
#'
#' - `S[t] = k * S[t-1] + (cop[t] * P[t] + B[t] + H[t] - F[t]) * dt`
#' - `R[t] = kh * R[t-1] + (F[t] - heat_demand[t]) * dt`
#' - `0 <= R[t] <= Hcap`, the floor at the setpoint hard; `F` has no cap of its
#'   own
#' - the house has no end condition: every window ends with the tank at least
#'   at its reference level as without a house, and the heat left in the
#'   house does not count towards it.
#'
#' The tank's band, its soft floor, its end condition, the backups and their
#' order are those without a house. The house starts the run at its setpoint
#' (`R = 0`) and its level carries from one window to the next like the
#' tank's, so a window uses the heat the last one left there, but no window
#' has to put it back. The schedule heats the house ahead when that costs
#' less than the heat it loses before the demand needs it, within the window:
#' the heat it holds at the window's end is worth nothing to that window.
#' The demand the tank cannot pass to the house is unmet heat, as without a
#' house. The slots that run `flexible` never heat the house: it serves the
#' demand while it is above its setpoint, and the tank gives the rest.
#'
#' @section A store that holds nothing:
#' `Tcap = 0` is a heat pump without a tank (or a store without a buffer): its
#' level is 0 in every slot and `SOCmin`, `SOCmax`, `SOCini` and `loss` have
#' nothing to act on. Its heat in equals its heat out in every slot. Without
#' a house the compressor follows the demand, `cop * P + B + H = heat_demand -
#' U`, up to what `Pc` delivers, and the rest is unmet heat `U`. With a house
#' it heats the house directly, `cop * P + B + H = F`, which is how a heat
#' pump without a buffer pre-heats a building; the house keeps its floor at
#' its setpoint, and what it cannot get is unmet heat. Its 2-decimal power
#' keeps the level within half a rounding step of heat of 0 (`0.005 * cop *
#' dt` kWh, above or below), as a tank keeps it at its floor, so the rounding
#' leaves no heat unmet.
#'
#' @section Stores behind one compressor:
#' With `stores`, a character vector of store names (each matching
#' `^[a-z][a-z0-9_]*$`; flextools gives them no meaning), the compressor
#' serves one store per name, through a valve that serves one at a time and
#' shares its time within the slot. Each store has its own balance as above
#' (its heat demand, COP, band, loss, backup, house and unmet heat), and the
#' stores share the compressor:
#'
#' - `S_s[t] = k_s * S_s[t-1] + (cop_s[t] * P_s[t] + B_s[t] + H_s[t] -
#'   heat_demand_s[t]) * dt` for every store `s`
#' - `sum_s P_s[t] <= Pc`, the one compressor's nameplate; the backups are
#'   not part of it.
#'
#' `opt_data` then carries `heat_demand_<store>`, `cop_<store>` and
#' `flexible_<store>` for every store, and not `heat_demand`, `cop` or
#' `flexible`. `flexible_<store>` is that store's static compressor power,
#' without its backup; the stores' `flexible_<store>` must not sum above
#' `Pc + 0.01` in any slot.
#'
#' `Tcap`, `SOCmin`, `SOCmax`, `SOCini`, `loss`, `Pb`, `Pg`, `Hcap` and
#' `Hloss` take one value per store, in the order of `stores`, or one value
#' for all of them. `NA` in `Pb` or `Pg` is no such backup on that store, `NA`
#' in both `Hcap` and `Hloss` no house behind it, and `SOCmin`, `SOCmax`,
#' `SOCini` and `loss` may be `NA` where `Tcap` is 0, and only there. `Pc`
#' stays one number.
#'
#' The objective sees the heat pump's electric load, every store's compressor
#' and backup heater, `sum_s (P_s + B_s)`; `lambda` penalises changes of
#' `sum_s P_s`. The heat price `h` (below) takes the lowest positive COP of
#' every store, and `Pc + sum(Pb)` in its `"grid"` form. Windows, the levels
#' they carry, `on_failure` and the slots that run `flexible` work per store;
#' in a slot that runs `flexible`, each store runs its own `flexible_<store>`
#' and stops at its own top. One store in this form gives the results of the
#' form without `stores`.
#'
#' The unmet heat of every store counts alike: where the compressor cannot
#' serve every store, the solver may leave either short.
#'
#' @section Compressor first, backup heater second, unmet heat last:
#' The backup heater's electricity is part of the load the objective sees, like
#' the compressor's. To use it only after the compressor, and to cool the tank
#' below its floor only after both, the objective prices them from the
#' window's **heat price** `h`: the highest marginal value a kW of
#' electric load can take in the window's objective, divided by the lowest
#' positive COP when it is below 1, so that no kWh of compressor heat is worth
#' more than `h`:
#'
#' - `"cost"`: the highest absolute import or export price of the window
#'   (EUR/kWh);
#' - `"grid"`: the largest derivative of the squared net flow,
#'   `2 * (max|static - production| + Pc + Pb)` (kW);
#' - a weight `w`: `w` times the grid value (scaled as the grid term of the
#'   objective is) plus `1 - w` times the cost value;
#' - with `lambda > 0`, plus the largest derivative of the ramping penalty,
#'   `4 * lambda * Pc`.
#'
#' The backup heater pays `2 * h` per kW on top of its electricity, so it only
#' runs for heat the compressor cannot deliver (it never absorbs a PV surplus
#' the compressor could).
#'
#' The gas backup pays the same `2 * h` per kW of heat, at the same rank: only
#' for heat the compressor cannot deliver, before a missed grid capacity and
#' before the comfort floor. Its heat is not electricity: it is not part of the
#' net flow that any objective sees, nor of the grid capacities and their
#' slack, and `h` does not count it. The gas it burns is not priced (its cost
#' is not weighed against electricity), so it never runs where the compressor
#' could heat instead within the grid capacities, however expensive the
#' electricity.
#'
#' When a window has no schedule that holds the band within the grid
#' capacities, it is solved again with a soft floor: the level may go down to
#' 0, and every kWh below `SOCmin` costs `20 * h / dt` per slot, more than
#' any kWh of heat the compressor, the backup heater or a missed grid capacity
#' (below) can deliver. So the tank only cools below its floor when they
#' cannot hold it, and is brought back above it as soon as they can. The end
#' condition becomes soft at the same price, so a drained tank does not have
#' to be refilled by the end of the window. Without a backup heater and with a
#' band the compressor can hold, the problem, and so the result, is the one
#' without any of this.
#'
#' @section Soft grid capacities:
#' As for [add_battery_optimization()], the grid capacities are soft: a window
#' that has no schedule holding the band within them is solved with the soft
#' comfort floor and with a penalised slack on each capacity. The slack is bounded by
#' how far the site already misses the capacity without optimisation (see
#' `optimization_slack_ceiling()`): for import, with the heat pump running
#' `flexible`; for export, with the heat pump off, since a full tank cannot
#' absorb a surplus. It costs `10 * h` per kW (electric), between the backup
#' and the comfort floor: the solver uses the compressor and the backup to
#' their limits before it misses a capacity, and misses an import capacity
#' before it lets the tank cool below its floor. The gas backup is not bound
#' by the capacities at all. The heat pump off with the tank cooling always
#' fits these bounds, so **every window has a schedule**.
#' A capacity left missed is reported with a message; compare the net flow
#' `static + P + attr(, "backup_kw") - production` with the capacities to find
#' where.
#'
#' The slots that run `flexible` (past `flex_window_hours`, or in a window the
#' solver failed on, reported with a message, unless `on_failure = "error"`
#' stops the run there instead) keep the tank below its top: the
#' compressor stops as a thermostat would, its power there being `flexible`
#' reduced to what keeps the level at `SOCmax`. Below the floor they cool the
#' tank down to the inlet temperature at most, with unmet heat as above. So the
#' level stays between 0 and `SOCmax` everywhere, and [get_tank_level()]
#' reproduces it.
#'
#' Inputs out of range stop with an error naming the argument; nothing is
#' clamped into range, and no zero profile is returned in their place.
#'
#' @param opt_data tibble, optimization contextual data. The first column must
#'   be named `datetime` (mandatory). Required columns:
#'
#' - `heat_demand`: heat the tank must deliver (kW thermal, >= 0)
#' - `cop`: heat per electric kWh (>= 0); 0 means the compressor cannot serve
#'   the tank in that slot
#' - `flexible`: the static electric schedule of the heat pump (kW, >= 0),
#'   returned wherever the function does not optimise
#'
#' With `stores`, these three columns once per store instead, suffixed with
#' its name: `heat_demand_<store>`, `cop_<store>` and `flexible_<store>`.
#'
#' Optional columns, as in [add_battery_optimization()]: `static`, `production`,
#' `import_capacity`, `export_capacity`, `price_imported`, `price_exported`.
#' @param opt_objective character or numeric. `"grid"` (default), `"cost"`, or a
#'   numeric weight `w` in `[0, 1]` where `w = 1` is pure grid and `w = 0` is
#'   pure cost. `"grid"` minimises the squared net grid flow
#'   `static + P + B - production`; `"cost"` minimises the energy cost of it.
#'   `"capacity"` has no tank form and stops with an error.
#' @param Tcap numeric, tank capacity (kWh thermal, >= 0): the heat between the
#'   inlet water temperature (`SOC = 0`, where the tank gives no more heat) and
#'   the top. 0 is no tank (see *A store that holds nothing*). For a mixed tank of `V` litres between the inlet temperature
#'   `T_in` and its top temperature `T_max` (degrees C), it is
#'   `V * 4.186 / 3600 * (T_max - T_in)`, and a comfort floor at `T_floor` is
#'   `SOCmin = 100 * (T_floor - T_in) / (T_max - T_in)`.
#' @param Pc numeric, compressor cap (kW electric, > 0). With `stores`, the
#'   one compressor's nameplate, which the stores share.
#' @param SOCmin numeric, minimum State-of-Charge (%), the comfort floor.
#' @param SOCmax numeric, maximum State-of-Charge (%).
#' @param SOCini numeric, State-of-Charge (%) at the start of the run, within
#'   `[SOCmin, SOCmax]`, and the reference level every window ends at least
#'   as full as. `NULL` (default) means `SOCmax`, unlike the battery: a tank
#'   starting at its floor has no reserve for its first window.
#' @param window_days integer, optimization window length in days.
#' @param window_start_hour integer, start hour of each optimization window.
#' @param flex_window_hours numeric, flexibility window length (hours).
#' @param lambda numeric, ramping penalty weight on changes of `P` between
#'   consecutive slots.
#' @param loss numeric, standing loss of the tank's level (% per hour, >= 0), as
#'   in [get_storage_losses()].
#' @param on_failure character, what a window the solver fails on does:
#'   `"flexible"` (default) runs the `flexible` schedule there, as described in
#'   *Soft grid capacities*, with a message; `"error"` stops with an error
#'   naming the window's first `datetime` and the solver status.
#' @param Pb numeric, rating of the resistive backup heater (kW electric, which
#'   is also its kW thermal, >= 0). `NULL` (default) or 0 means no backup
#'   heater.
#' @param Pg numeric, rating of the gas backup, such as the boiler of a hybrid
#'   heat pump (kW thermal, >= 0). Its heat is not electric load: it is in the
#'   tank's balance only. `NULL` (default) or 0 means no gas backup, and the
#'   result is the one without the argument. `Pb` and `Pg` cannot both be
#'   above 0.
#' @param Hcap numeric, capacity of the house behind a space-heating tank (kWh
#'   thermal, > 0): the heat its interior holds between its setpoint and the
#'   top of its comfort band. For an interior heat capacity `C` (J/K) and a
#'   comfort band of `dT` (K), `C * dT / 3.6e6`. `NULL` (default) means no
#'   house, and the result is the one without the argument. Given with `Hloss`
#'   or not at all.
#' @param Hloss numeric, loss of the house's heat above its setpoint (% per
#'   hour, >= 0 and < 100). A house of time constant `tau` hours loses
#'   `100 * (1 - exp(-1 / tau))`. Given with `Hcap` or not at all.
#' @param stores character vector, the names of the stores behind the one
#'   compressor (see *Stores behind one compressor*). `NULL` (default) is one
#'   tank, with the unsuffixed columns and one value per argument.
#'
#' @return numeric vector, the compressor's electric power (kW), one value per
#'   `opt_data` row, rounded to 2 decimals: the optimised power in optimised
#'   windows and `flexible` everywhere else, reduced where it would heat the
#'   tank above `SOCmax`. Attributes:
#'
#' - `backup_kw`: numeric vector, one value per row, rounded to 2 decimals: the
#'   backup heater's power (kW electric = kW thermal); 0 outside optimised
#'   windows and everywhere without a backup heater (also with a gas backup).
#' - `gas_backup_kw`: only with `Pg > 0`. Numeric vector, one value per row,
#'   rounded to 2 decimals: the gas backup's heat (kW thermal, not
#'   electricity); 0 outside optimised windows.
#' - `unmet_heat_kw`: numeric vector, one value per row, rounded to 2 decimals:
#'   the unmet heat (kW thermal, not electricity): the heat that would have
#'   been required in that slot to keep the tank at least at its comfort floor
#'   (the heat it gave from below the floor, plus the demand it could not
#'   serve at the inlet temperature). 0 wherever the level stays at or above
#'   the floor, or rises back towards it. It is a report, not part of the
#'   balance.
#' - `house_heat_kw`: only with a house. Numeric vector, one value per row,
#'   rounded to 2 decimals: the heat the tank passes to the house (kW
#'   thermal), which is the tank's demand.
#' - `house_level_kwh`: only with a house. Numeric vector, one value per row,
#'   rounded to 2 decimals: the house's level above its setpoint at the start
#'   of the slot (kWh thermal), between 0 and `Hcap`.
#'
#' The heat pump's electric load is the power plus `attr(, "backup_kw")`. Its
#' heat is `cop` times the power, plus `attr(, "backup_kw")` and
#' `attr(, "gas_backup_kw")`: pass their sum as the `backup` of
#' [get_tank_level()] and [get_tank_unmet_heat()], and with a house
#' `attr(, "house_heat_kw")` as their `heat_demand`. The house's level follows
#' exactly from it, from 0: `R[t] = kh * R[t-1] + (house_heat_kw[t] -
#' heat_demand[t]) * dt`, nothing cut off, so the heat the house loses over the
#' run is `(1 - kh)` times the sum of `R` at the start of every slot.
#'
#' With `stores`, the value is the compressor's total power `sum_s P_s` (kW
#' electric), rounded to 2 decimals and at or below `Pc` in every row, and
#' every attribute is a named list with one numeric vector per store, keyed
#' by its name:
#'
#' - `power_kw`: every store. The store's compressor power (kW electric),
#'   rounded to 2 decimals; they sum to the value. Where the stores' rounded
#'   powers would sum above `Pc`, one store rounds down by 0.01 kW in that
#'   slot: one whose level keeps that heat above its floor, and among them
#'   the one rounded up the most.
#' - `level_kwh`: every store. The store's level at the start of the slot
#'   (kWh thermal), rounded to 2 decimals: what [get_tank_level()] gives from
#'   the store's attributes, so within half a rounding step of heat of 0
#'   where `Tcap` is 0.
#' - `backup_kw`, `unmet_heat_kw`: every store, as above.
#' - `gas_backup_kw`: the stores with `Pg > 0`, as above.
#' - `house_heat_kw`, `house_level_kwh`: the stores with a house, as above.
#'
#' @export
#'
#' @importFrom dplyr %>% mutate
#' @importFrom timefully get_time_resolution
#'
#' @examples
#' library(dplyr)
#' opt_data <- tibble(
#'   datetime = as.POSIXct("2024-01-01", tz = "UTC") + 3600 * 0:23,
#'   production = 0,
#'   heat_demand = c(rep(1, 17), rep(6, 3), rep(1, 4)),
#'   cop = 3,
#'   flexible = 1 / 3,
#'   price_imported = ifelse(0:23 < 6, 0.1, 0.3),
#'   price_exported = 0
#' )
#' P <- add_tank_optimization(
#'   opt_data, opt_objective = "cost", Tcap = 3, Pc = 1, Pb = 2
#' )
#' attr(P, "backup_kw")
#' attr(P, "unmet_heat_kw")
#' get_tank_level(
#'   P, opt_data$heat_demand, opt_data$cop, init = 3,
#'   backup = attr(P, "backup_kw")
#' )
#'
add_tank_optimization <- function(
  opt_data,
  opt_objective = "grid",
  Tcap,
  Pc,
  SOCmin = 0,
  SOCmax = 100,
  SOCini = NULL,
  window_days = 1,
  window_start_hour = 0,
  flex_window_hours = 24,
  lambda = 0,
  loss = 0,
  Pb = NULL,
  Pg = NULL,
  Hcap = NULL,
  Hloss = NULL,
  on_failure = c("flexible", "error"),
  stores = NULL
) {
  if (is.null(opt_data)) {
    stop("Error: `opt_data` parameter is empty.", call. = FALSE)
  }
  if (!("datetime" %in% names(opt_data))) {
    stop("Error: `datetime` variable must exist in `opt_data`", call. = FALSE)
  }
  tank_columns <- c("heat_demand", "cop", "flexible")
  if (is.null(stores)) {
    m <- 1L
    columns <- matrix(tank_columns, nrow = 1)
  } else {
    if (
      !is.character(stores) || !length(stores) || anyNA(stores) ||
        !all(grepl("^[a-z][a-z0-9_]*$", stores))
    ) {
      stop(
        "Error: `stores` must be a character vector of store names, each matching `^[a-z][a-z0-9_]*$`.",
        call. = FALSE
      )
    }
    if (anyDuplicated(stores)) {
      stop(
        sprintf(
          "Error: `stores` names each store once; `%s` is repeated.",
          stores[anyDuplicated(stores)]
        ),
        call. = FALSE
      )
    }
    unsuffixed <- intersect(tank_columns, names(opt_data))
    if (length(unsuffixed)) {
      stop(
        sprintf(
          "Error: with `stores`, `opt_data` takes one `%s_<store>` column per store, not `%s`.",
          unsuffixed[1],
          unsuffixed[1]
        ),
        call. = FALSE
      )
    }
    m <- length(stores)
    columns <- outer(stores, tank_columns, function(s, col) paste0(col, "_", s))
  }
  for (col in t(columns)) {
    tank_check_column(opt_data, col)
  }
  for (col in intersect(c("static", "production"), names(opt_data))) {
    tank_check_column(opt_data, col, non_negative = FALSE)
  }

  if (identical(opt_objective, "capacity")) {
    stop(
      "Error: `opt_objective = \"capacity\"` has no tank form; use \"grid\", \"cost\" or a number in [0, 1].",
      call. = FALSE
    )
  }
  if (is.numeric(opt_objective)) {
    tank_check_number(opt_objective, "opt_objective", 0, 1)
    if (opt_objective <= 0) {
      opt_objective <- "cost"
    } else if (opt_objective >= 1) {
      opt_objective <- "grid"
    }
  } else if (
    !(is.character(opt_objective) &&
      length(opt_objective) == 1 &&
      opt_objective %in% c("grid", "cost"))
  ) {
    stop(
      "Error: `opt_objective` must be \"grid\", \"cost\" or a number in [0, 1].",
      call. = FALSE
    )
  }

  # With `stores`, every tank argument takes one value per store (one for
  # all of them is recycled), and `NA` is no backup or no house on that
  # store, or a band a store that holds nothing does not read
  where <- if (is.null(stores)) rep("", m) else sprintf(" (store `%s`)", stores)
  per_store <- function(x, name) {
    if (is.null(stores) || is.null(x)) {
      return(x)
    }
    if (!(length(x) %in% c(1, m))) {
      stop(
        sprintf(
          "Error: `%s` takes one value per store in `stores` (%d), or one for all of them.",
          name,
          m
        ),
        call. = FALSE
      )
    }
    rep_len(x, m)
  }
  Tcap <- per_store(Tcap, "Tcap")
  SOCmin <- per_store(SOCmin, "SOCmin")
  SOCmax <- per_store(SOCmax, "SOCmax")
  SOCini <- per_store(SOCini, "SOCini")
  loss <- per_store(loss, "loss")
  Pb <- per_store(Pb, "Pb")
  Pg <- per_store(Pg, "Pg")
  Hcap <- per_store(Hcap, "Hcap")
  Hloss <- per_store(Hloss, "Hloss")

  if (is.null(stores)) {
    tank_check_number(Tcap, "Tcap", 0)
  } else {
    for (s in seq_len(m)) {
      tank_check_number(Tcap[s], "Tcap", 0, where = where[s])
    }
  }
  tank_check_number(Pc, "Pc", 0, min_open = TRUE)
  if (is.null(Pb)) {
    Pb <- rep(0, m)
  }
  if (is.null(Pg)) {
    Pg <- rep(0, m)
  }
  if (!is.null(stores)) {
    Pb[is.na(Pb)] <- 0
    Pg[is.na(Pg)] <- 0
  }
  if (identical(on_failure, c("flexible", "error"))) {
    on_failure <- "flexible"
  }
  if (
    !(is.character(on_failure) &&
      length(on_failure) == 1 &&
      on_failure %in% c("flexible", "error"))
  ) {
    stop(
      "Error: `on_failure` must be \"flexible\" or \"error\".",
      call. = FALSE
    )
  }
  if (is.null(stores)) {
    tank_check_number(Pb, "Pb", 0)
    tank_check_number(Pg, "Pg", 0)
  }
  if (is.null(stores)) {
    # One value each, as up to flextools 1.24.0
    for (arg in c("SOCmin", "SOCmax", "SOCini", "loss")) {
      value <- get(arg)
      if (!is.null(value) && length(value) != 1) {
        tank_check_number(value, arg)
      }
    }
  }
  if (is.null(SOCini)) {
    SOCini <- SOCmax
  }
  for (s in seq_len(m)) {
    if (!is.null(stores)) {
      tank_check_number(Pb[s], "Pb", 0, where = where[s])
      tank_check_number(Pg[s], "Pg", 0, where = where[s])
    }
    if (Pb[s] > 0 && Pg[s] > 0) {
      stop(
        paste0(
          "Error: a tank has one backup: `Pb` (resistive) and `Pg` (gas) cannot both be above 0",
          where[s],
          "."
        ),
        call. = FALSE
      )
    }
    # A store that holds nothing has no band to read
    if (Tcap[s] == 0) {
      if (is.na(SOCmin[s])) SOCmin[s] <- 0
      if (is.na(SOCmax[s])) SOCmax[s] <- 100
      if (is.na(SOCini[s])) SOCini[s] <- SOCmax[s]
      if (is.na(loss[s])) loss[s] <- 0
    }
    tank_check_number(SOCmin[s], "SOCmin", 0, 100, where = where[s])
    tank_check_number(SOCmax[s], "SOCmax", 0, 100, where = where[s])
    if (Tcap[s] > 0 && SOCmin[s] >= SOCmax[s]) {
      stop(
        paste0("Error: `SOCmin` must be lower than `SOCmax`", where[s], "."),
        call. = FALSE
      )
    }
    if (Tcap[s] > 0) {
      tank_check_number(SOCini[s], "SOCini", SOCmin[s], SOCmax[s], where = where[s])
    } else {
      tank_check_number(SOCini[s], "SOCini", 0, 100, where = where[s])
    }
    tank_check_number(loss[s], "loss", 0, 100, where = where[s])
    if (loss[s] >= 100) {
      stop(
        paste0("Error: `loss` must be lower than 100 (%/hour)", where[s], "."),
        call. = FALSE
      )
    }
  }
  tank_check_number(lambda, "lambda", 0)
  if (is.null(Hcap) != is.null(Hloss)) {
    stop(
      sprintf(
        "Error: `%s` needs `%s`: a house behind the tank takes `Hcap` and `Hloss` together.",
        if (is.null(Hcap)) "Hloss" else "Hcap",
        if (is.null(Hcap)) "Hcap" else "Hloss"
      ),
      call. = FALSE
    )
  }
  house <- rep(!is.null(Hcap), m)
  if (house[1] && !is.null(stores)) {
    house <- !is.na(Hcap)
    for (s in which(is.na(Hcap) != is.na(Hloss))) {
      stop(
        sprintf(
          "Error: `%s` needs `%s`: a house behind the tank takes `Hcap` and `Hloss` together%s.",
          if (is.na(Hcap[s])) "Hloss" else "Hcap",
          if (is.na(Hcap[s])) "Hcap" else "Hloss",
          where[s]
        ),
        call. = FALSE
      )
    }
  }
  for (s in which(house)) {
    if (is.null(stores)) {
      tank_check_number(Hcap, "Hcap", 0, min_open = TRUE)
      tank_check_number(Hloss, "Hloss", 0, 100)
    }
    tank_check_number(Hcap[s], "Hcap", 0, min_open = TRUE, where = where[s])
    tank_check_number(Hloss[s], "Hloss", 0, 100, where = where[s])
    if (Hloss[s] >= 100) {
      stop(
        paste0("Error: `Hloss` must be lower than 100 (%/hour)", where[s], "."),
        call. = FALSE
      )
    }
  }

  column_matrix <- function(j) {
    matrix(
      unlist(lapply(columns[, j], function(col) opt_data[[col]]), use.names = FALSE),
      ncol = m
    )
  }
  Q <- column_matrix(1)
  cop <- column_matrix(2)
  flexible <- column_matrix(3)
  if (!is.null(stores)) {
    # One compressor's static schedule cannot pass its nameplate (up to the
    # rounding of the stores' schedules)
    over <- which(rowSums(flexible) > Pc + 0.01 + 1e-9)
    if (length(over)) {
      stop(
        sprintf(
          "Error: the stores' `flexible_<store>` sum above `Pc` (%s kW) at %s.",
          Pc,
          format(opt_data$datetime[over[1]], "%Y-%m-%d %H:%M:%S %Z")
        ),
        call. = FALSE
      )
    }
    opt_data$flexible <- rowSums(flexible)
  }

  opt_data <- check_optimization_data(opt_data, opt_objective)

  dttm_seq <- opt_data$datetime
  n_rows <- nrow(opt_data)
  time_resolution <- get_time_resolution(dttm_seq)
  dt <- time_resolution / 60
  k <- (1 - loss / 100)^dt
  Smin <- Tcap * SOCmin / 100
  Smax <- Tcap * SOCmax / 100
  if (!any(house)) {
    Hcap <- rep(0, m)
    Hloss <- rep(0, m)
  }
  Hcap[!house] <- 0
  Hloss[!house] <- 0
  kh <- ifelse(house, (1 - Hloss / 100)^dt, 1)
  # The top the 2-decimal power keeps the level under. A store that holds
  # nothing has no room below its top for the rounding, which then could only
  # round down and leave a step of heat unmet in every other slot: it may
  # hold half a rounding step of heat, above or below 0, as a tank may at its
  # floor
  rounding_top <- function(s, idx) {
    if (Smax[s] > 0) Smax[s] else 0.005 * max(cop[idx, s], 1) * dt
  }

  G <- opt_data$production
  L <- opt_data$static
  if (is.null(opt_data$price_imported)) {
    opt_data$price_imported <- 0
  }
  if (is.null(opt_data$price_exported)) {
    opt_data$price_exported <- 0
  }
  import_capacity <- as.numeric(rep_len(opt_data$import_capacity, n_rows))
  export_capacity <- as.numeric(rep_len(opt_data$export_capacity, n_rows))

  flex_windows <- tank_flex_windows(
    dttm_seq = dttm_seq,
    window_days = window_days,
    window_start_hour = window_start_hour,
    flex_window_hours = flex_window_hours
  )

  reset_message_once()

  # One column per store
  P_out <- round(flexible, 2)
  if (m > 1) {
    # The stores' static schedules may pass `Pc` by their rounding: the
    # largest gives the excess back
    p_max <- floor(Pc * 100 + 1e-9) / 100
    for (t in which(rowSums(P_out) > p_max + 1e-9)) {
      s <- which.max(P_out[t, ])
      P_out[t, s] <- round(P_out[t, s] - (sum(P_out[t, ]) - p_max), 2)
    }
  }
  B_out <- matrix(0, n_rows, m)
  H_out <- matrix(0, n_rows, m)
  # The tank's demand: with a house, the heat it passes to the house
  F_out <- Q
  level <- Tcap * SOCini / 100 # level at the start of slot `pos`
  house_level <- numeric(m) # the house's, which starts the run at its setpoint
  pos <- 1L

  advance <- function(level, idx) {
    if (!length(idx)) {
      return(level)
    }
    for (s in seq_len(m)) {
      if (house[s]) {
        path <- tank_house_path(F_out[idx, s], Q[idx, s], house_level[s], kh[s], dt)
        house_level[s] <<- path[length(path)]
      }
      path <- tank_level_path(
        P_out[idx, s],
        F_out[idx, s],
        cop[idx, s],
        level[s],
        k[s],
        dt,
        backup = B_out[idx, s] + H_out[idx, s]
      )
      level[s] <- path[length(path)]
    }
    level
  }

  # Slots that run `flexible` keep the tank below its top too: the compressor
  # stops as a thermostat would, at the 2-decimal power that keeps the level
  # at the top. Below, the tank cools down to the inlet temperature at most.
  # They never heat the house: it serves the demand while it is above its
  # setpoint, and the tank gives the rest. Each store runs its own.
  run_flexible <- function(level, idx) {
    for (t in idx) {
      for (s in seq_len(m)) {
        if (house[s]) {
          F_out[t, s] <<- max(ceiling((Q[t, s] - kh[s] * house_level[s] / dt) * 100 - 1e-9) / 100, 0)
          house_level[s] <<- kh[s] * house_level[s] + (F_out[t, s] - Q[t, s]) * dt
        }
        top <- rounding_top(s, t)
        s_next <- k[s] * level[s] + (cop[t, s] * P_out[t, s] - F_out[t, s]) * dt
        if (s_next > top + 1e-9 && cop[t, s] > 0) {
          p_fit <- (top - k[s] * level[s] + F_out[t, s] * dt) / (cop[t, s] * dt)
          P_out[t, s] <<- max(floor(p_fit * 100 + 1e-9) / 100, 0)
          s_next <- k[s] * level[s] + (cop[t, s] * P_out[t, s] - F_out[t, s]) * dt
        }
        level[s] <- if (s_next < -1e-9) 0 else s_next
      }
    }
    level
  }

  for (w in seq_len(nrow(flex_windows))) {
    start <- flex_windows$start[w]
    idx <- flex_windows$flex_idx[[w]]

    # Slots before the window run `flexible`
    if (start > pos) {
      level <- run_flexible(level, seq(pos, start - 1))
    }

    # Every slot keeps the level between the inlet temperature and the top,
    # up to rounding, which the solver does not need to see
    S0 <- pmin(pmax(level, 0), Smax)

    # One store goes to the solver as vectors and numbers
    window_cols <- function(x) if (m == 1) x[idx, 1] else x[idx, , drop = FALSE]
    window_args <- list(
      G = G[idx],
      L = L[idx],
      Q = window_cols(Q),
      cop = window_cols(cop),
      PI = opt_data$price_imported[idx],
      PE = opt_data$price_exported[idx],
      import_capacity = import_capacity[idx],
      export_capacity = export_capacity[idx],
      S0 = S0,
      Smin = Smin,
      Smax = Smax,
      Pc = Pc,
      k = k,
      dt = dt,
      objective = opt_objective,
      lambda = lambda,
      Pb = Pb,
      Pg = Pg,
      S_ref = Tcap * SOCini / 100
    )
    if (any(house)) {
      window_args$Hcap <- Hcap
      window_args$kh <- kh
      window_args$R0 <- pmin(pmax(house_level, 0), Hcap)
    }
    sol <- do.call(tank_solve_window, window_args)

    if (is.null(sol$power)) {
      # The window has no schedule that holds the band within the grid
      # capacities. Solve it again with the unmet heat, and with the
      # capacities soft as for the battery: penalised slack, up to how far
      # the static schedule (import) or the heat pump switched off (export)
      # already misses them. Then the heat pump off, with the floor held by
      # unmet heat, is a solution, so this problem always has one.
      tol <- optimization_solution_tolerance()
      slack_import_max <- optimization_slack_ceiling(
        import_capacity[idx],
        L[idx] + rowSums(flexible)[idx] - G[idx]
      )
      slack_export_max <- optimization_slack_ceiling(
        export_capacity[idx],
        G[idx] - L[idx]
      )
      slack_import_max[slack_import_max > 0] <-
        slack_import_max[slack_import_max > 0] + tol
      slack_export_max[slack_export_max > 0] <-
        slack_export_max[slack_export_max > 0] + tol
      window_args$soft <- TRUE
      window_args$slack_import_max <- slack_import_max
      window_args$slack_export_max <- slack_export_max
      sol <- do.call(tank_solve_window, window_args)
    }

    if (is.null(sol$power)) {
      # Only a solver failure gets here
      if (on_failure == "error") {
        stop(
          sprintf(
            "Error: the solver failed on the window starting at %s (%s).",
            format(dttm_seq[start], "%Y-%m-%d %H:%M:%S %Z"),
            sol$status
          ),
          call. = FALSE
        )
      }
      message_once(
        "\u26a0\ufe0f Optimization warning: the solver failed in some windows. Using the `flexible` schedule there."
      )
      level <- run_flexible(level, idx)
    } else {
      n_idx <- length(idx)
      sol_power <- matrix(sol$power, n_idx, m)
      # The backup heater and the gas backup add heat to the tank alike, and a
      # tank has one of them at most: they are rounded as one backup
      sol_backup <- matrix(sol$backup + sol$gas, n_idx, m)
      sol_house <- if (any(house)) matrix(sol$house, n_idx, m) else NULL
      target <- matrix(0, n_idx, m)
      for (s in seq_len(m)) {
        if (house[s]) {
          # The heat into the house first: the tank's rounding then tracks its
          # level with the rounded heat as its demand
          house_target <- tank_house_path(
            sol_house[, s], Q[idx, s], window_args$R0[s], kh[s], dt
          )[-1]
          F_out[idx, s] <- tank_round_house(
            house_target, Q[idx, s], house_level[s], kh[s], dt, Hcap[s]
          )
        }
        target[, s] <- tank_level_path(
          sol_power[, s],
          if (house[s]) sol_house[, s] else Q[idx, s],
          cop[idx, s],
          S0[s],
          k[s],
          dt,
          backup = sol_backup[, s]
        )[-1]
      }
      if (m == 1) {
        rounded <- tank_round_power(
          target[, 1],
          F_out[idx, 1],
          cop[idx, 1],
          level,
          k,
          dt,
          Pc,
          backup = sol_backup[, 1],
          Pb = Pb + Pg,
          power = sol_power[, 1],
          Smax = rounding_top(1, idx)
        )
        rounded <- lapply(rounded, matrix, ncol = 1)
      } else {
        rounded <- tank_round_stores(
          target,
          F_out[idx, , drop = FALSE],
          cop[idx, , drop = FALSE],
          level,
          k,
          dt,
          Pc,
          backup = sol_backup,
          Pb = Pb + Pg,
          power = sol_power,
          Smax = vapply(seq_len(m), rounding_top, numeric(1), idx = idx),
          Smin = Smin
        )
      }
      P_out[idx, ] <- rounded$power
      for (s in seq_len(m)) {
        if (Pg[s] > 0) {
          H_out[idx, s] <- rounded$backup[, s]
        } else {
          B_out[idx, s] <- rounded$backup[, s]
        }
      }
      # The gas backup is not in the net flow
      net <- L[idx] + rowSums(P_out[idx, , drop = FALSE]) +
        rowSums(B_out[idx, , drop = FALSE]) - G[idx]
      if (
        any(net > import_capacity[idx] + 0.01) ||
          any(-net > export_capacity[idx] + 0.01)
      ) {
        message_once(
          "\u26a0\ufe0f Optimization warning: grid capacity not reachable in some slots. The heat pump is operating at its limits there, so the capacity is approached but not met."
        )
      }
      level <- advance(level, idx)
    }

    pos <- max(idx) + 1L
  }
  # Slots after the last window run `flexible`
  if (pos <= n_rows) {
    run_flexible(level, seq(pos, n_rows))
  }

  # Unmet heat: the heat that would have kept the tank at its comfort floor
  U_out <- matrix(0, n_rows, m)
  for (s in seq_len(m)) {
    U_out[, s] <- get_tank_unmet_heat(
      P_out[, s],
      F_out[, s],
      cop[, s],
      init = Tcap[s] * SOCini[s] / 100,
      Tcap = Tcap[s],
      SOCmin = SOCmin[s],
      loss = loss[s],
      time_resolution = time_resolution,
      backup = B_out[, s] + H_out[, s]
    )
  }
  if (any(U_out > 0)) {
    message(
      "\u26a0\ufe0f Optimization warning: the tank goes below its comfort floor in some slots. That heat is unmet (attribute `unmet_heat_kw`)."
    )
  }
  house_level_out <- function(s) {
    round(tank_house_path(F_out[, s], Q[, s], 0, kh[s], dt)[seq_len(n_rows)], 2)
  }

  if (is.null(stores)) {
    out <- P_out[, 1]
    attr(out, "backup_kw") <- B_out[, 1]
    if (Pg > 0) {
      attr(out, "gas_backup_kw") <- H_out[, 1]
    }
    if (house) {
      attr(out, "house_heat_kw") <- F_out[, 1]
      attr(out, "house_level_kwh") <- house_level_out(1)
    }
    attr(out, "unmet_heat_kw") <- U_out[, 1]
    return(out)
  }

  # The stores form: the compressor's total power, and every store's own in
  # named lists
  by_store <- function(x, which = seq_len(m)) {
    if (is.function(x)) {
      out <- lapply(which, x)
    } else {
      out <- lapply(which, function(s) x[, s])
    }
    names(out) <- stores[which]
    out
  }
  out <- round(rowSums(P_out), 2)
  attr(out, "power_kw") <- by_store(P_out)
  # The level `get_tank_level()` gives from the same power
  attr(out, "level_kwh") <- by_store(function(s) {
    round(
      tank_level_path(
        P_out[, s], F_out[, s], cop[, s], Tcap[s] * SOCini[s] / 100, k[s], dt,
        backup = B_out[, s] + H_out[, s]
      )[seq_len(n_rows)],
      2
    )
  })
  attr(out, "backup_kw") <- by_store(B_out)
  if (any(Pg > 0)) {
    attr(out, "gas_backup_kw") <- by_store(H_out, which(Pg > 0))
  }
  attr(out, "unmet_heat_kw") <- by_store(U_out)
  if (any(house)) {
    attr(out, "house_heat_kw") <- by_store(F_out, which(house))
    attr(out, "house_level_kwh") <- by_store(house_level_out, which(house))
  }
  out
}


#' Tank level (thermal energy)
#'
#' The tank's counterpart of [get_storage_level()]: the level in kWh thermal at
#' the start of each slot, starting from `init`, with
#' `S[t+1] = max(k * S[t] + (cop[t] * power[t] + backup[t] - heat_demand[t]) *
#' dt, 0)` and `k = (1 - loss/100)^dt`, `dt` being the time resolution in
#' hours. Level 0 is the inlet temperature: an empty tank gives no more heat,
#' and the demand it cannot serve is unmet heat.
#'
#' @param power numeric vector, compressor electric power (kW), such as the
#'   result of [add_tank_optimization()]
#' @param heat_demand numeric vector, heat demand (kW thermal)
#' @param cop numeric vector, coefficient of performance (heat per electric
#'   kWh)
#' @param init numeric, level before the first slot (in kWh, not %)
#' @param loss numeric, standing loss of the level (% per hour)
#' @param time_resolution numeric, time resolution of the time-series (in
#'   minutes)
#' @param backup numeric, backup heat (kW thermal): `attr(result,
#'   "backup_kw")` of [add_tank_optimization()] (a resistive heater, kW
#'   electric = kW thermal), or its `attr(result, "gas_backup_kw")` with a gas
#'   backup, or 0 without a backup
#'
#' @return numeric vector of the level (kWh), rounded to 2 decimals
#' @export
#'
get_tank_level <- function(
  power,
  heat_demand,
  cop,
  init,
  loss = 0,
  time_resolution = 60,
  backup = 0
) {
  n <- length(power)
  for (arg in c("heat_demand", "cop", "backup")) {
    value <- get(arg)
    if (!is.numeric(value) || !(length(value) %in% c(1, n)) || anyNA(value)) {
      stop(
        sprintf(
          "Error: `%s` must be numeric without NA, of length 1 or `length(power)`.",
          arg
        ),
        call. = FALSE
      )
    }
  }
  if (!is.numeric(power) || anyNA(power)) {
    stop("Error: `power` must be numeric without NA.", call. = FALSE)
  }
  tank_check_number(init, "init")
  tank_check_number(loss, "loss", 0, 100)
  if (loss >= 100) {
    stop("Error: `loss` must be lower than 100 (%/hour).", call. = FALSE)
  }
  tank_check_number(time_resolution, "time_resolution", 0, min_open = TRUE)

  dt <- time_resolution / 60
  k <- (1 - loss / 100)^dt
  level <- tank_level_path(
    power,
    rep_len(heat_demand, n),
    rep_len(cop, n),
    init,
    k,
    dt,
    rep_len(backup, n)
  )
  round(level[seq_len(n)], 2)
}


#' Unmet heat of a tank schedule
#'
#' The heat that would have been required in each slot to keep the tank at
#' least at its comfort floor, for any compressor schedule: the static
#' schedule's counterpart of `attr(result, "unmet_heat_kw")` of
#' [add_tank_optimization()], which uses this same function. Per slot of `dt`
#' hours, with the level `S` following [get_tank_level()] (it stops at 0, the
#' inlet temperature), `Smin = Tcap * SOCmin / 100`, `k = (1 - loss/100)^dt`
#' and `S~[t] = k * S[t-1] + (cop[t] * power[t] + backup[t] -
#' heat_demand[t]) * dt` the level the balance asks for:
#'
#' `unmet[t] * dt = max(0, min(Smin, S[t-1]) - min(Smin, S~[t]))`
#'
#' that is the heat the tank gives from below its comfort floor, to the demand
#' or to its standing loss, plus the demand it cannot serve at all at the inlet
#' temperature. It is positive while the level falls below the floor, and 0
#' while the level stays at or above it or rises back towards it. A gap within
#' half a rounding step of the power (`0.005 * max(cop, 1) * dt` kWh) is
#' carried over: the level climbing back cancels it, and it counts in the
#' slot where the gaps carried add up beyond that step. So a standing loss of
#' a few Wh per slot that drains the tank below its floor is unmet heat, while
#' the level wiggling by a rounding step around the floor is not.
#'
#' @inheritParams get_tank_level
#' @param Tcap numeric, tank capacity (kWh thermal, >= 0), as in
#'   [add_tank_optimization()]: the heat between the inlet water temperature
#'   and the top of the tank. At 0 (and `init = 0`), the unmet heat is the
#'   heat demand the compressor and the backup do not serve in the slot, up to
#'   the rounding carried as above.
#' @param SOCmin numeric, comfort floor (%, between 0 and 100)
#'
#' @return numeric vector of the unmet heat (kW thermal), one value per slot,
#'   rounded to 2 decimals
#' @export
#'
#' @examples
#' # A 3 kWh tank with its floor at 1 kWh, emptied by 2 kW for 2 hours
#' get_tank_unmet_heat(
#'   power = c(0, 0, 0), heat_demand = c(2, 2, 0), cop = 3,
#'   init = 3, Tcap = 3, SOCmin = 100 / 3
#' )
#'
get_tank_unmet_heat <- function(
  power,
  heat_demand,
  cop,
  init,
  Tcap,
  SOCmin,
  loss = 0,
  time_resolution = 60,
  backup = 0
) {
  n <- length(power)
  for (arg in c("heat_demand", "cop", "backup")) {
    value <- get(arg)
    if (!is.numeric(value) || !(length(value) %in% c(1, n)) || anyNA(value)) {
      stop(
        sprintf(
          "Error: `%s` must be numeric without NA, of length 1 or `length(power)`.",
          arg
        ),
        call. = FALSE
      )
    }
  }
  if (!is.numeric(power) || anyNA(power)) {
    stop("Error: `power` must be numeric without NA.", call. = FALSE)
  }
  tank_check_number(init, "init")
  tank_check_number(Tcap, "Tcap", 0)
  tank_check_number(SOCmin, "SOCmin", 0, 100)
  tank_check_number(loss, "loss", 0, 100)
  if (loss >= 100) {
    stop("Error: `loss` must be lower than 100 (%/hour).", call. = FALSE)
  }
  tank_check_number(time_resolution, "time_resolution", 0, min_open = TRUE)

  dt <- time_resolution / 60
  cop <- rep_len(cop, n)
  tank_unmet_heat(
    power,
    rep_len(heat_demand, n),
    cop,
    init,
    (1 - loss / 100)^dt,
    dt,
    backup = rep_len(backup, n),
    Smin = Tcap * SOCmin / 100,
    tol = 0.005 * max(cop, 1) * dt
  )
}
