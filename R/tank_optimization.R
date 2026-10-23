# Tank optimization (OSQP / HiGHS) ---------------------------------------------
#
# A heat pump charging one thermal tank. The compressor's electric power `P`
# is the decision, the heat demand `Q` discharges the tank, and the COP `cop`
# converts one into the other. A resistive backup heater `B` (COP 1) can add
# heat next to the compressor. Per slot of `dt` hours, with the level `S` in
# kWh thermal above the inlet temperature (level 0):
#
#   S[t] = k * S[t-1] + (cop[t] * P[t] + B[t] - Q[t]) * dt
#   Smin <= S[t] <= Smax                              k = (1 - loss/100)^dt
#   0 <= P[t] <= Pc,  P[t] = 0 where cop[t] = 0
#   0 <= B[t] <= Pb   (also where cop[t] = 0)
#   S[last slot of a window] >= S[slot before its first]
#
# A window with no schedule that holds this is solved again with a soft
# comfort floor (the level may go down to 0, with the depth below `Smin`
# penalised, plus `U` for the demand the tank cannot serve at 0) and soft grid
# capacities. `B` is a variable only with a backup heater (`Pb > 0`), so
# without one, and where the band holds, the problem is the one without them.
#
# The level carries from one window to the next, so windows are solved in
# order, and every slot no window optimises runs the static `flexible`
# schedule.


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
#' of heat. The backup heater is rounded on its own and the compressor makes up
#' its rounding; where the COP is 0 the backup heater tracks the level in the
#' compressor's place.
#'
#' @inheritParams tank_level_path
#' @param target numeric vector, level at the end of each slot under the
#'   unrounded power (kWh)
#' @param Pc numeric, compressor cap (kW)
#' @param Pb numeric, backup heater cap (kW), 0 without one
#' @param power numeric vector, the unrounded power (kW). Where the tank ends a
#'   slot empty, the demand it leaves unserved is not for the rounding to make
#'   up: the power there stays within a rounding step of it.
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
  power = NULL
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
    need <- target[t] - k * s + (heat_demand[t] - backup[t]) * dt
    empty <- !is.null(power) && target[t] <= 1e-9
    if (cop[t] > 0) {
      p <- need / (cop[t] * dt)
      if (empty) {
        p <- min(p, power[t] + 0.01)
      }
      out[t] <- min(max(round(p, 2), 0), p_max)
    } else if (b_max > 0) {
      b <- backup[t] + need / dt
      if (empty) {
        b <- min(b, b_raw[t] + 0.01)
      }
      backup[t] <- min(max(round(b, 2), 0), b_max)
    }
    s <- k * s + (cop[t] * out[t] + backup[t] - heat_demand[t]) * dt
    # A level below 0 by more than rounding is demand the tank did not serve
    if (s < -tol) {
      s <- 0
    }
  }
  list(power = out, backup = backup)
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
#' with the compressor and the backup heater at full power whenever the tank
#' has room, the compressor first: no schedule leaves less.
#'
#' @inheritParams tank_solve_window
#' @param ub_P,ub_B,ub_total numeric vectors, caps of the compressor, the
#'   backup heater and their sum (kW)
#'
#' @return numeric vector, heat not served per slot (kW thermal)
#' @keywords internal
#'
tank_min_unmet <- function(Q, cop, S0, k, dt, Smax, ub_P, ub_B, ub_total) {
  n <- length(Q)
  U <- numeric(n)
  s <- S0
  for (t in seq_len(n)) {
    P <- min(ub_P[t], ub_total[t])
    B <- min(ub_B[t], ub_total[t] - P)
    heat <- min(cop[t] * P + B, max(Smax - k * s, 0) / dt + Q[t])
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
  list(power = NULL, backup = NULL, status = status)
}


#' Solve one tank window
#'
#' @param G numeric vector, production (kW)
#' @param L numeric vector, static load (kW)
#' @param Q numeric vector, heat demand (kW thermal)
#' @param cop numeric vector, coefficient of performance
#' @param PI,PE numeric vectors, import and export prices (EUR/kWh); unused for
#'   `"grid"`
#' @param import_capacity,export_capacity numeric vectors, net grid flow
#'   capacities (kW)
#' @param S0 numeric, level before the first slot, inside the band (kWh)
#' @param Smin,Smax numeric, band (kWh)
#' @param Pc numeric, compressor cap (kW)
#' @param k numeric, per-slot retention
#' @param dt numeric, slot length (hours)
#' @param objective `"grid"`, `"cost"` or a number strictly between 0 and 1
#' @param lambda numeric, ramping penalty weight
#' @param Pb numeric, backup heater cap (kW); 0 means no backup heater
#' @param soft logical, whether the comfort floor is soft: the level may go
#'   down to the inlet temperature, below the floor at a price
#' @param slack_import_max,slack_export_max numeric vectors, how far each slot
#'   may miss its import / export capacity (kW), from
#'   [optimization_slack_ceiling()]; the capacities are hard where they are 0
#'
#' @return list of the unrounded `power` and `backup` (kW) and the `status`;
#'   when the window has no solution, `power` is `NULL` and `status` says why
#'   (see `tank_failure()`)
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
  soft = FALSE,
  slack_import_max = 0,
  slack_export_max = 0
) {
  n <- length(G)
  import_capacity <- as.numeric(rep_len(import_capacity, n))
  export_capacity <- as.numeric(rep_len(export_capacity, n))
  slack_import_max <- as.numeric(rep_len(slack_import_max, n))
  slack_export_max <- as.numeric(rep_len(slack_export_max, n))
  with_backup <- Pb > 0
  soft_caps <- any(slack_import_max > 0) || any(slack_export_max > 0)

  # The net flow `L + P + B - G` must stay within the capacities, which for
  # non-negative loads is a box on `P + B`. A negative capacity is an
  # obligation to flow the other way and moves the box the same way.
  lb_load <- pmax(0, G - L - export_capacity)
  ub_load <- G - L + import_capacity
  if (soft_caps) {
    # Soft capacities: the heat pump's load may miss them by up to the slack
    # ceilings, through explicit rows on `P + B` with penalised slack (below).
    # The boxes take the relaxed envelope.
    cap_lb <- G - L - export_capacity
    cap_ub <- ub_load
    lb_load <- pmax(0, cap_lb - slack_export_max)
    ub_load <- ub_load + slack_import_max
    lb_P <- rep(0, n)
    ub_P <- pmax(pmin(ifelse(cop > 0, Pc, 0), ub_load), 0)
    ub_B <- if (with_backup) pmax(pmin(Pb, ub_load), 0) else rep(0, n)
    if (any(lb_load > pmin(ub_load, ub_P + ub_B) + 1e-9)) {
      return(tank_failure("the heat pump's load cannot meet the grid capacities"))
    }
    lb_total <- lb_load
    ub_total <- pmax(pmin(ub_load, ub_P + ub_B), lb_load)
  } else if (with_backup) {
    lb_P <- rep(0, n)
    ub_P <- pmax(pmin(ifelse(cop > 0, Pc, 0), ub_load), 0)
    ub_B <- pmax(pmin(Pb, ub_load), 0)
    if (any(lb_load > pmin(ub_load, ub_P + ub_B) + 1e-9)) {
      return(tank_failure("the heat pump's load cannot meet the grid capacities"))
    }
    ub_load <- pmax(pmin(ub_load, ub_P + ub_B), lb_load)
    lb_total <- lb_load
    ub_total <- ub_load
  } else {
    lb_P <- lb_load
    ub_P <- pmin(ifelse(cop > 0, Pc, 0), ub_load)
    if (any(lb_P > ub_P + 1e-9)) {
      return(tank_failure("the heat pump's load cannot meet the grid capacities"))
    }
    ub_P <- pmax(ub_P, lb_P)
    lb_total <- lb_P
    ub_total <- ub_P
  }

  ub_S <- rep(Smax, n)
  # End condition: the window ends at least as full as it started (or at the
  # floor, when it started below it)
  S_end <- max(Smin, S0)
  if (soft) {
    # Soft comfort floor: the level may go down to the inlet temperature
    # (level 0), with the depth below the floor `D` penalised, and the end
    # condition missed by `R`, penalised too. Unmet heat is only the heat the
    # tank cannot give at the inlet temperature.
    lb_S <- rep(0, n)
    U_max <- tank_min_unmet(
      Q, cop, S0, k, dt,
      Smax = Smax,
      ub_P = ub_P,
      ub_B = if (with_backup) ub_B else rep(0, n),
      ub_total = ub_total
    )
  } else {
    lb_S <- rep(Smin, n)
    lb_S[n] <- S_end
  }

  with_flows <- !identical(objective, "grid")
  idx_P <- seq_len(n)
  idx_S <- n + seq_len(n)
  nv <- 2 * n
  idx_B <- integer(0)
  idx_U <- integer(0)
  if (with_backup) {
    idx_B <- nv + seq_len(n)
    nv <- nv + n
  }
  idx_D <- integer(0)
  idx_R <- integer(0)
  if (soft) {
    idx_U <- nv + seq_len(n)
    idx_D <- nv + n + seq_len(n)
    idx_R <- nv + 2 * n + 1
    nv <- nv + 2 * n + 1
  }
  idx_si <- integer(0)
  idx_se <- integer(0)
  if (soft_caps) {
    idx_si <- nv + seq_len(n)
    idx_se <- nv + n + seq_len(n)
    nv <- nv + 2 * n
  }
  idx_I <- integer(0)
  idx_E <- integer(0)
  if (with_flows) {
    idx_I <- nv + seq_len(n)
    idx_E <- nv + n + seq_len(n)
    nv <- nv + 2 * n
  }

  # Dynamics: S_t - k * S_{t-1} - dt * (cop_t * P_t + B_t + U_t) = -Q_t * dt
  # (+ k * S0 at t = 1)
  A_dyn <- Matrix::sparseMatrix(
    i = c(
      seq_len(n),
      seq_len(n),
      seq_len(n - 1) + 1,
      if (with_backup) seq_len(n),
      if (soft) seq_len(n)
    ),
    j = c(idx_S, idx_P, idx_S[seq_len(n - 1)], idx_B, idx_U),
    x = c(
      rep(1, n),
      -cop * dt,
      rep(-k, n - 1),
      if (with_backup) rep(-dt, n),
      if (soft) rep(-dt, n)
    ),
    dims = c(n, nv)
  )
  rhs_dyn <- -Q * dt
  rhs_dyn[1] <- rhs_dyn[1] + k * S0

  A <- rbind(Matrix::Diagonal(nv), A_dyn)
  lower <- c(lb_P, lb_S)
  upper <- c(ub_P, ub_S)
  if (with_backup) {
    lower <- c(lower, rep(0, n))
    upper <- c(upper, ub_B)
  }
  if (soft) {
    # U: no slot needs more than its demand plus refilling the whole band.
    # D: at most the floor. R: at most the end level.
    lower <- c(lower, rep(0, n), rep(0, n), 0)
    upper <- c(upper, Q + Smax / dt, rep(Smin, n), S_end)
  }
  if (soft_caps) {
    lower <- c(lower, rep(0, 2 * n))
    upper <- c(upper, slack_import_max, slack_export_max)
  }

  if (with_flows) {
    # Balance: I_t - E_t - P_t - B_t = L_t - G_t
    A_bal <- Matrix::sparseMatrix(
      i = c(rep(seq_len(n), 3), if (with_backup) seq_len(n)),
      j = c(idx_I, idx_E, idx_P, idx_B),
      x = c(rep(1, n), rep(-1, n), rep(-1, n), if (with_backup) rep(-1, n)),
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
    # P_t + B_t - s_i,t <= G_t - L_t + C_imp,t   import capacity, soft
    # P_t + B_t + s_e,t >= G_t - L_t - C_exp,t   export capacity, soft
    A_cap <- Matrix::sparseMatrix(
      i = c(
        seq_len(n),
        if (with_backup) seq_len(n),
        seq_len(n),
        n + seq_len(n),
        if (with_backup) n + seq_len(n),
        n + seq_len(n)
      ),
      j = c(idx_P, idx_B, idx_si, idx_P, idx_B, idx_se),
      x = c(
        rep(1, n),
        if (with_backup) rep(1, n),
        rep(-1, n),
        rep(1, n),
        if (with_backup) rep(1, n),
        rep(1, n)
      ),
      dims = c(2 * n, nv)
    )
    A <- rbind(A, A_cap)
    lower <- c(lower, rep(-Inf, n), cap_lb)
    upper <- c(upper, cap_ub, rep(Inf, n))
  } else if (with_backup) {
    # Capacity box on the heat pump's total load: lb <= P_t + B_t <= ub
    A_load <- Matrix::sparseMatrix(
      i = rep(seq_len(n), 2),
      j = c(idx_P, idx_B),
      x = rep(1, 2 * n),
      dims = c(n, nv)
    )
    A <- rbind(A, A_load)
    lower <- c(lower, lb_load)
    upper <- c(upper, ub_load)
  }

  if (soft) {
    # S_t + D_t >= Smin            depth below the comfort floor
    # S_n + R >= S_end             end condition, soft
    # sum(U) <= the least unmet heat any schedule needs
    A_soft <- Matrix::sparseMatrix(
      i = c(seq_len(n), seq_len(n), n + 1, n + 1, rep(n + 2, n)),
      j = c(idx_S, idx_D, idx_S[n], idx_R, idx_U),
      x = rep(1, 3 * n + 2),
      dims = c(n + 2, nv)
    )
    A <- rbind(A, A_soft)
    lower <- c(lower, rep(Smin, n), S_end, 0)
    upper <- c(upper, rep(Inf, n), Inf, sum(U_max) + 1e-6)
  }

  scale <- if (with_flows) mean(PI)^2 else 1
  if (!is.finite(scale) || scale <= 0) {
    scale <- 1
  }

  # Heat from the backup heater costs more than any compressor heat, and
  # missing a grid capacity more still. Going below the comfort floor costs
  # more per kWh and slot than any of them per kWh of heat, so the solver
  # runs the compressor first, then the backup heater, then misses the
  # capacity, and only then lets the tank cool below its floor. The demand the
  # tank cannot serve at the inlet temperature (`U`) is capped at the least
  # any schedule leaves instead of priced.
  if (with_backup || soft || soft_caps) {
    heat_price <- tank_heat_price(
      L,
      G,
      cop,
      Pc,
      Pb,
      objective,
      lambda,
      price = if (with_flows) max(abs(c(PI, PE_clipped))) else 0,
      scale = scale
    )
  }
  c_B <- if (with_backup) 2 * heat_price else 0
  c_U <- 0
  c_D <- if (soft) 20 * heat_price / dt else 0
  c_S <- if (soft_caps) 10 * heat_price else 0

  lambdaMat <- if (lambda > 0) get_lambda_matrix(n) else NULL

  if (identical(objective, "cost") && lambda == 0) {
    # Pure LP on HiGHS
    lin <- numeric(nv)
    lin[idx_B] <- c_B
    lin[idx_U] <- c_U
    lin[c(idx_D, idx_R)] <- c_D
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
      # min sum((L - G + P + B)^2) + lambda * sum((P_t - P_{t-1})^2)
      w_flow <- 1
      P_block <- 2 * diag(n)
      if (lambda > 0) {
        P_block <- P_block + 2 * lambda * lambdaMat
      }
      q[idx_P] <- 2 * (L - G)
    } else {
      w <- if (identical(objective, "cost")) 0 else objective
      w_flow <- w * scale
      P_block <- 2 * w * scale * diag(n)
      if (lambda > 0) {
        P_block <- P_block + 2 * lambda * (lambdaMat + 1e-6 * diag(n))
      }
      q[idx_P] <- 2 * w * scale * (L - G)
      q[idx_I] <- (1 - w) * PI
      q[idx_E] <- -(1 - w) * PE_clipped
    }
    P_quad[idx_P, idx_P] <- P_block
    if (with_backup) {
      # The backup heater's load enters the squared net flow like `P`'s
      flow_block <- 2 * w_flow * diag(n)
      P_quad[idx_P, idx_B] <- flow_block
      P_quad[idx_B, idx_P] <- flow_block
      P_quad[idx_B, idx_B] <- flow_block
      q[idx_B] <- 2 * w_flow * (L - G) + c_B
    }
    q[idx_U] <- c_U
    q[c(idx_D, idx_R)] <- c_D
    q[c(idx_si, idx_se)] <- c_S
    obj_scale <- max(max(abs(P_quad)), max(abs(q)), 1e-6)
    sol <- solve_osqp(P_quad / obj_scale, q / obj_scale, A, lower, upper)
    if (is.null(sol$x)) {
      return(tank_failure(paste0("OSQP: ", sol$result$status_message)))
    }
    x <- as.numeric(sol$x)
  }

  if (with_backup || soft_caps) {
    P <- pmin(pmax(x[idx_P], 0), ub_P)
    B <- if (with_backup) pmin(pmax(x[idx_B], 0), ub_B) else rep(0, n)
  } else {
    P <- pmin(pmax(x[idx_P], lb_P), ub_P)
    B <- rep(0, n)
  }

  # OSQP declares an infeasible problem as such, but its accuracy is absolute,
  # so check the power it returns against the band once more on the exact
  # dynamics before trusting it.
  # The level stops at the inlet temperature: the heat `U` the solver lets in
  # there is demand the tank does not serve
  level <- tank_level_path(P, Q, cop, S0, k, dt, backup = B)[-1]
  tol <- 1e-3
  if (
    any(level < (if (soft) 0 else Smin) - tol) ||
      any(level > Smax + tol) ||
      (!soft && level[n] < S0 - tol)
  ) {
    return(tank_failure("the solution leaves the tank's band on the exact dynamics"))
  }
  list(power = P, backup = B, status = "optimal")
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
    t0 <- dttm_seq[1]
    next_start <- lubridate::floor_date(t0, "day") +
      lubridate::hours(window_start_hour)
    if (next_start < t0) {
      next_start <- next_start + lubridate::days(1)
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
tank_check_number <- function(x, name, min = -Inf, max = Inf, min_open = FALSE) {
  ok <- is.numeric(x) && length(x) == 1 && !is.na(x) && is.finite(x) &&
    x <= max && (if (min_open) x > min else x >= min)
  if (!ok) {
    range <- paste0(
      if (min_open) "greater than " else "at least ",
      min,
      if (is.finite(max)) paste0(" and at most ", max) else ""
    )
    stop(sprintf("Error: `%s` must be a number %s.", name, range), call. = FALSE)
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
#' thermal tank, which the heat demand discharges, with an optional resistive
#' backup heater next to it. It mirrors [add_battery_optimization()]: the same
#' `opt_data` conventions, objectives (except `"capacity"`), optimization
#' windows and solvers, with the tank in place of the battery. The compressor's
#' heat is `cop * P`, so there is no conversion efficiency: the COP is the
#' conversion. The backup heater turns `B` kW of electricity into `B` kW of heat.
#'
#' Per slot of `dt` hours, with `S` the level in kWh thermal above the inlet
#' water temperature (level 0, `SOC = 0`) and `k = (1 - loss/100)^dt`:
#'
#' - `S[t] = k * S[t-1] + (cop[t] * P[t] + B[t] - heat_demand[t]) * dt`
#' - `Tcap * SOCmin/100 <= S[t] <= Tcap * SOCmax/100`
#' - `0 <= P[t] <= Pc`, and `P[t] = 0` where `cop[t] = 0`
#' - `0 <= B[t] <= Pb`, also where `cop[t] = 0`
#' - every window ends at least as full as it started.
#'
#' `B` (the backup heater) is a variable only with `Pb > 0`. The comfort floor
#' `SOCmin` is **soft**: when the compressor and the backup heater cannot hold
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
#' with the end-of-window rule at their last slot). Only the first
#' `flex_window_hours` of every window are optimised, so with
#' `flex_window_hours = 24 * window_days` every slot is optimised. The slots
#' past `flex_window_hours` run the `flexible` schedule, without backup heater.
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
#' heater and the comfort floor: the solver uses the compressor and the backup
#' heater to their limits before it misses a capacity, and misses an import
#' capacity before it lets the tank cool below its floor. The heat pump off
#' with the tank cooling always fits these bounds, so **every window has a
#' schedule**.
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
#' Optional columns, as in [add_battery_optimization()]: `static`, `production`,
#' `import_capacity`, `export_capacity`, `price_imported`, `price_exported`.
#' @param opt_objective character or numeric. `"grid"` (default), `"cost"`, or a
#'   numeric weight `w` in `[0, 1]` where `w = 1` is pure grid and `w = 0` is
#'   pure cost. `"grid"` minimises the squared net grid flow
#'   `static + P + B - production`; `"cost"` minimises the energy cost of it.
#'   `"capacity"` has no tank form and stops with an error.
#' @param Tcap numeric, tank capacity (kWh thermal, > 0): the heat between the
#'   inlet water temperature (`SOC = 0`, where the tank gives no more heat) and
#'   the top. For a mixed tank of `V` litres between the inlet temperature
#'   `T_in` and its top temperature `T_max` (degrees C), it is
#'   `V * 4.186 / 3600 * (T_max - T_in)`, and a comfort floor at `T_floor` is
#'   `SOCmin = 100 * (T_floor - T_in) / (T_max - T_in)`.
#' @param Pc numeric, compressor cap (kW electric, > 0).
#' @param SOCmin numeric, minimum State-of-Charge (%), the comfort floor.
#' @param SOCmax numeric, maximum State-of-Charge (%).
#' @param SOCini numeric, State-of-Charge (%) at the start of the run, within
#'   `[SOCmin, SOCmax]`. `NULL` (default) means `SOCmax`, unlike the battery: a
#'   tank starting at its floor has no reserve for its first window.
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
#'
#' @return numeric vector, the compressor's electric power (kW), one value per
#'   `opt_data` row, rounded to 2 decimals: the optimised power in optimised
#'   windows and `flexible` everywhere else, reduced where it would heat the
#'   tank above `SOCmax`. Attributes:
#'
#' - `backup_kw`: numeric vector, one value per row, rounded to 2 decimals: the
#'   backup heater's power (kW electric = kW thermal); 0 outside optimised
#'   windows and everywhere without a backup heater.
#' - `unmet_heat_kw`: numeric vector, one value per row, rounded to 2 decimals:
#'   the unmet heat (kW thermal, not electricity): the heat that would have
#'   been required in that slot to keep the tank at least at its comfort floor
#'   (the heat it gave from below the floor, plus the demand it could not
#'   serve at the inlet temperature). 0 wherever the level stays at or above
#'   the floor, or rises back towards it. It is a report, not part of the
#'   balance.
#'
#' The heat pump's electric load is the power plus `attr(, "backup_kw")`.
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
  on_failure = c("flexible", "error")
) {
  if (is.null(opt_data)) {
    stop("Error: `opt_data` parameter is empty.", call. = FALSE)
  }
  if (!("datetime" %in% names(opt_data))) {
    stop("Error: `datetime` variable must exist in `opt_data`", call. = FALSE)
  }
  for (col in c("heat_demand", "cop", "flexible")) {
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

  tank_check_number(Tcap, "Tcap", 0, min_open = TRUE)
  tank_check_number(Pc, "Pc", 0, min_open = TRUE)
  if (is.null(Pb)) {
    Pb <- 0
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
  tank_check_number(Pb, "Pb", 0)
  tank_check_number(SOCmin, "SOCmin", 0, 100)
  tank_check_number(SOCmax, "SOCmax", 0, 100)
  if (SOCmin >= SOCmax) {
    stop("Error: `SOCmin` must be lower than `SOCmax`.", call. = FALSE)
  }
  if (is.null(SOCini)) {
    SOCini <- SOCmax
  }
  tank_check_number(SOCini, "SOCini", SOCmin, SOCmax)
  tank_check_number(loss, "loss", 0, 100)
  if (loss >= 100) {
    stop("Error: `loss` must be lower than 100 (%/hour).", call. = FALSE)
  }
  tank_check_number(lambda, "lambda", 0)

  opt_data <- check_optimization_data(opt_data, opt_objective)

  dttm_seq <- opt_data$datetime
  n_rows <- nrow(opt_data)
  time_resolution <- get_time_resolution(dttm_seq)
  dt <- time_resolution / 60
  k <- (1 - loss / 100)^dt
  Smin <- Tcap * SOCmin / 100
  Smax <- Tcap * SOCmax / 100

  Q <- opt_data$heat_demand
  cop <- opt_data$cop
  flexible <- opt_data$flexible
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

  P_out <- round(flexible, 2)
  B_out <- numeric(n_rows)
  level <- Tcap * SOCini / 100 # level at the start of slot `pos`
  pos <- 1L

  advance <- function(level, idx) {
    if (!length(idx)) {
      return(level)
    }
    path <- tank_level_path(
      P_out[idx],
      Q[idx],
      cop[idx],
      level,
      k,
      dt,
      backup = B_out[idx]
    )
    path[length(path)]
  }

  # Slots that run `flexible` keep the tank below its top too: the compressor
  # stops as a thermostat would, at the 2-decimal power that keeps the level
  # at the top. Below, the tank cools down to the inlet temperature at most.
  run_flexible <- function(level, idx) {
    for (t in idx) {
      s_next <- k * level + (cop[t] * P_out[t] - Q[t]) * dt
      if (s_next > Smax + 1e-9 && cop[t] > 0) {
        p_fit <- (Smax - k * level + Q[t] * dt) / (cop[t] * dt)
        P_out[t] <<- max(floor(p_fit * 100 + 1e-9) / 100, 0)
        s_next <- k * level + (cop[t] * P_out[t] - Q[t]) * dt
      }
      level <- if (s_next < -1e-9) 0 else s_next
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
    S0 <- min(max(level, 0), Smax)

    window_args <- list(
      G = G[idx],
      L = L[idx],
      Q = Q[idx],
      cop = cop[idx],
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
      Pb = Pb
    )
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
        L[idx] + flexible[idx] - G[idx]
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
      target <- tank_level_path(
        sol$power,
        Q[idx],
        cop[idx],
        S0,
        k,
        dt,
        backup = sol$backup
      )[-1]
      rounded <- tank_round_power(
        target,
        Q[idx],
        cop[idx],
        level,
        k,
        dt,
        Pc,
        backup = sol$backup,
        Pb = Pb,
        power = sol$power
      )
      P_out[idx] <- rounded$power
      B_out[idx] <- rounded$backup
      net <- L[idx] + P_out[idx] + B_out[idx] - G[idx]
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
  U_out <- get_tank_unmet_heat(
    P_out,
    Q,
    cop,
    init = Tcap * SOCini / 100,
    Tcap = Tcap,
    SOCmin = SOCmin,
    loss = loss,
    time_resolution = time_resolution,
    backup = B_out
  )
  if (any(U_out > 0)) {
    message(
      "\u26a0\ufe0f Optimization warning: the tank goes below its comfort floor in some slots. That heat is unmet (attribute `unmet_heat_kw`)."
    )
  }

  attr(P_out, "backup_kw") <- B_out
  attr(P_out, "unmet_heat_kw") <- U_out
  P_out
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
#' @param backup numeric, backup heater power (kW electric = kW thermal):
#'   `attr(result, "backup_kw")` of [add_tank_optimization()], or 0 without a
#'   backup heater
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
#' @param Tcap numeric, tank capacity (kWh thermal, > 0), as in
#'   [add_tank_optimization()]: the heat between the inlet water temperature
#'   and the top of the tank
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
  tank_check_number(Tcap, "Tcap", 0, min_open = TRUE)
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
