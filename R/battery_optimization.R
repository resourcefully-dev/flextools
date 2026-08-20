# Battery optimization (OSQP / HiGHS) ----------------------------------------

battery_qp_try_heuristic <- function(
  target,
  lower,
  upper,
  Bcap,
  Bc,
  Bd,
  SOCmin,
  SOCmax,
  SOCini
) {
  time_slots <- length(target)
  storage <- 0
  profile <- numeric(time_slots)
  lower_storage <- rep((SOCmin - SOCini) / 100 * Bcap, time_slots)
  upper_storage <- rep((SOCmax - SOCini) / 100 * Bcap, time_slots)

  for (i in seq_len(time_slots)) {
    remaining_slots <- time_slots - i
    future_storage_lower <- -remaining_slots * Bc
    future_storage_upper <- remaining_slots * Bd
    storage_lower <- max(lower_storage[i], future_storage_lower)
    storage_upper <- min(upper_storage[i], future_storage_upper)
    step_lower <- max(lower[i], storage_lower - storage)
    step_upper <- min(upper[i], storage_upper - storage)

    if (step_lower > step_upper + 1e-8) {
      return(NULL)
    }

    profile[i] <- pmin(pmax(target[i], step_lower), step_upper)
    storage <- storage + profile[i]
  }

  profile
}


battery_solve_osqp <- function(P, q, A, lower, upper) {
  sol <- solve_osqp(P, q, A, lower, upper)
  list(
    result = sol$result,
    profile = if (!is.null(sol$x)) as.numeric(sol$x) else NULL
  )
}


# Core window solver for grid/capacity: X = [B] --------------------------------

#' Perform battery optimization for a single window (grid/capacity objective)
#'
#' @param G numeric vector, renewable generation profile
#' @param L numeric vector, static load profile
#' @param Bcap numeric, battery capacity in energy units (kWh * slots/h)
#' @param Bc numeric, maximum charging power (kW)
#' @param Bd numeric, maximum discharging power (kW)
#' @param SOCmin numeric, minimum State-of-Charge (%)
#' @param SOCmax numeric, maximum State-of-Charge (%)
#' @param SOCini numeric, initial State-of-Charge (%)
#' @param import_capacity numeric vector, maximum grid import (kW)
#' @param export_capacity numeric vector, maximum grid export (kW)
#' @param P numeric matrix, quadratic objective term
#' @param q numeric vector, linear objective term
#'
#' @return numeric vector
#' @keywords internal
#'
battery_solve_grid_window <- function(
  G,
  L,
  Bcap,
  Bc,
  Bd,
  SOCmin,
  SOCmax,
  SOCini,
  import_capacity,
  export_capacity,
  P,
  q
) {
  G <- round(G, 2)
  L <- round(L, 2)

  time_slots <- length(G)
  import_capacity <- as.numeric(rep_len(import_capacity, time_slots))
  export_capacity <- as.numeric(rep_len(export_capacity, time_slots))
  identityMat <- diag(time_slots)
  cumsumMat <- triangulate_matrix(matrix(1, time_slots, time_slots), "l")

  lb_cumsum <- rep((SOCmin - SOCini) / 100 * Bcap, time_slots)
  ub_cumsum <- rep((SOCmax - SOCini) / 100 * Bcap, time_slots)

  # How far each capacity may be missed: only as far as the pre-battery net
  # flow already needed. Zero wherever the site was within its capacity, so
  # those slots stay hard capped and the battery can never create a grid
  # violation worse than the pre-battery profile.
  tol <- optimization_solution_tolerance()
  slack_import_max <- optimization_slack_ceiling(import_capacity, L - G)
  slack_export_max <- optimization_slack_ceiling(export_capacity, G - L)
  soft_caps <- any(slack_import_max > 0) || any(slack_export_max > 0)

  # The tolerance absorbs the 2-decimal rounding of the inputs, but only where
  # a capacity is already missed. Slots that can meet their capacity keep a
  # ceiling of exactly zero, so they stay as hard as they are on the fast path.
  slack_import_max[slack_import_max > 0] <-
    slack_import_max[slack_import_max > 0] + tol
  slack_export_max[slack_export_max > 0] <-
    slack_export_max[slack_export_max > 0] + tol

  # Relaxed envelope: the loosest net flow any solution may reach. Used as a
  # (redundant but tightening) box on B in the soft problem, and as the bounds
  # for the heuristic fallback.
  lb_B_relaxed <- pmax(-Bd, G - L - export_capacity - slack_export_max)
  ub_B_relaxed <- pmin(Bc, G - L + import_capacity + slack_import_max)

  if (!soft_caps) {
    # Fast path: every slot can meet its capacity, so no slack is needed and
    # the problem is exactly the hard-constrained one. This keeps the
    # unconstrained case (import_capacity = export_capacity = Inf) at its
    # original size and cost.
    Amat <- rbind(
      identityMat,
      cumsumMat,
      matrix(1, nrow = 1, ncol = time_slots)
    )
    lower <- round(
      c(pmax(-Bd, G - L - export_capacity), lb_cumsum, 0),
      2
    )
    upper <- round(
      c(pmin(Bc, G - L + import_capacity), ub_cumsum, 0),
      2
    )
    solution <- battery_solve_osqp(P, q, Amat, lower, upper)
  } else {
    # Soft path. Variables X = [B, s_i, s_e] (3n): s_i and s_e measure by how
    # much a slot misses its import / export capacity.
    #
    #   -Bd <= B_t <= Bc                          hard physics
    #   B_t - s_i,t <= G_t - L_t + C_imp,t        import capacity, soft
    #   B_t + s_e,t >= G_t - L_t - C_exp,t        export capacity, soft
    #   0 <= s_i,t <= slack_import_max_t          relaxation ceiling
    #   0 <= s_e,t <= slack_export_max_t
    #   lb_cumsum <= cumsum(B) <= ub_cumsum       state of charge
    #   sum(B) = 0                                energy neutrality
    #
    # X = (0, slack_import_max, slack_export_max) satisfies all of the above,
    # so the problem is feasible by construction and no relaxation retry is
    # needed. Penalising the slack makes the optimizer spend the battery's
    # full physical capability on approaching an unreachable capacity instead
    # of abandoning it, while the ceiling keeps the guarantee that the result
    # is never worse than the pre-battery profile.
    zeroMat <- matrix(0, time_slots, time_slots)
    # The penalty has to dominate the gradient of whatever objective the caller
    # handed in: `sum(net^2)` from `battery_grid_window()`, whose gradient is
    # bounded by the net flow, or `sum(B^2)` from `battery_usage_window()`,
    # bounded by the battery power. The relaxed box keeps `|B| <= max(Bc, Bd)`,
    # so `max|L - G| + max(Bc, Bd)` bounds the net flow and, being no smaller,
    # the battery power too - one envelope covers both objectives.
    #
    # It is derived from the flows the solution can actually reach rather than
    # from the capacities, which bound nothing achievable once a capacity is set
    # loosely: a nominal 1e6 standing in for "unlimited" (rather than `Inf`,
    # which is filtered out) produced a penalty of 2e7, and at that
    # conditioning OSQP returned no solution at all, so the battery was
    # disabled for the whole window.
    penalty <- optimization_slack_penalty(max(abs(L - G)) + max(Bc, Bd))
    # Small quadratic ridge on the slack block: keeps the objective strictly
    # convex in every variable so OSQP has a unique optimum to converge to.
    ridge <- 1e-6 * penalty

    P <- rbind(
      cbind(P, zeroMat, zeroMat),
      cbind(zeroMat, ridge * identityMat, zeroMat),
      cbind(zeroMat, zeroMat, ridge * identityMat)
    )
    q <- c(q, rep(penalty, time_slots), rep(penalty, time_slots))

    Amat <- rbind(
      cbind(identityMat, zeroMat, zeroMat), # B box
      cbind(zeroMat, identityMat, zeroMat), # import slack box
      cbind(zeroMat, zeroMat, identityMat), # export slack box
      cbind(identityMat, -identityMat, zeroMat), # import capacity
      cbind(identityMat, zeroMat, identityMat), # export capacity
      cbind(cumsumMat, zeroMat, zeroMat), # state of charge
      matrix(c(rep(1, time_slots), rep(0, 2 * time_slots)), nrow = 1)
    )
    lower <- c(
      lb_B_relaxed,
      rep(0, time_slots),
      rep(0, time_slots),
      rep(-Inf, time_slots),
      G - L - export_capacity,
      lb_cumsum,
      0
    )
    upper <- c(
      ub_B_relaxed,
      slack_import_max,
      slack_export_max,
      G - L + import_capacity,
      rep(Inf, time_slots),
      ub_cumsum,
      0
    )

    # No manual objective scaling here: OSQP already equilibrates the problem
    # internally, and pre-dividing by the (large) penalty only shrinks the
    # objective against OSQP's absolute termination tolerance. Measured on a
    # one-year benchmark, dropping it improves the accuracy of near-zero
    # solutions by ~40x at identical optimality (0/365 windows suboptimal
    # either way).
    solution <- battery_solve_osqp(P, q, Amat, lower, upper)
  }

  if (!is.null(solution$profile)) {
    if (!soft_caps) {
      return(solution$profile)
    }
    slack <- solution$profile[-seq_len(time_slots)]
    if (any(slack > tol + 1e-6)) {
      message_once(
        "\u26a0\ufe0f Optimization warning: grid capacity not reachable in some slots. The battery is operating at its physical limit there, so the capacity is approached but not met."
      )
    }
    # Drop digits the solver does not guarantee: OSQP terminates at an absolute
    # tolerance of 1e-6, so anything below that is convergence residue rather
    # than a battery setpoint. Only the soft path needs this - the fast path is
    # left untouched so it stays bit-for-bit identical to the hard-constrained
    # solver it replaces.
    profile <- round(solution$profile[seq_len(time_slots)], 6)

    # The slack penalty is linear in the volume missed, so the solver is
    # indifferent to how an unreachable capacity is missed and the quadratic
    # term flattens it across every affected slot. Spend the same energy on
    # meeting the capacity for as many slots as it covers instead.
    return(
      optimization_concentrate_slack(
        B = profile,
        net0 = L - G,
        import_capacity = import_capacity,
        export_capacity = export_capacity,
        lb_B = lb_B_relaxed,
        ub_B = ub_B_relaxed,
        lb_cumsum = lb_cumsum,
        ub_cumsum = ub_cumsum
      )
    )
  }

  # Solver crash guard. Aim the heuristic at the capacity when a capacity is
  # binding (a negative `import_capacity` is a forced export, so the target
  # moves below the zero-net-flow point) and clamp it into the relaxed
  # envelope.
  heuristic <- battery_qp_try_heuristic(
    target = pmax(
      pmin(G - L, G - L + import_capacity),
      G - L - export_capacity
    ),
    lower = lb_B_relaxed,
    upper = ub_B_relaxed,
    Bcap = Bcap,
    Bc = Bc,
    Bd = Bd,
    SOCmin = SOCmin,
    SOCmax = SOCmax,
    SOCini = SOCini
  )
  if (!is.null(heuristic)) {
    message_once(paste0(
      "\u26a0\ufe0f Optimization warning: ",
      solution$result$status_message,
      ". Using heuristic battery profile for some windows."
    ))
    return(heuristic)
  }

  # Unreachable for well-formed inputs: B = 0 is feasible in both branches, so
  # reaching here means the solver crashed. Surface it loudly and fall back to
  # the do-nothing profile, which leaves the site untouched.
  message_once(paste0(
    "\u26a0\ufe0f Optimization warning: ",
    solution$result$status_message,
    ". Disabling battery for some windows."
  ))
  rep(0, time_slots)
}


#' @keywords internal
battery_grid_window <- function(
  G,
  L,
  Bcap,
  Bc,
  Bd,
  SOCmin,
  SOCmax,
  SOCini,
  import_capacity,
  export_capacity,
  lambda = 0
) {
  time_slots <- length(G)
  lambdaMat <- get_lambda_matrix(time_slots)
  P <- 2 * (diag(time_slots) + lambda * lambdaMat)
  q <- 2 * (L - G)

  battery_solve_grid_window(
    G,
    L,
    Bcap,
    Bc,
    Bd,
    SOCmin,
    SOCmax,
    SOCini,
    import_capacity,
    export_capacity,
    P,
    q
  )
}


#' Minimize battery usage (just a window)
#'
#' The capacity is enforced by `battery_solve_grid_window()`'s constraints, so
#' the objective is free to ask for the *least battery* that satisfies them:
#' `min sum(B^2)`, with no linear term pulling the net flow anywhere. The
#' quadratic penalises power, so the discharge settles exactly on the binding
#' capacity rather than below it, and the energy neutrality it has to give back
#' is recharged at the lowest power the window allows.
#'
#' This is the opposite reading of the same constraints from
#' `battery_grid_window()`, whose `q = 2 * (L - G)` makes the objective
#' `sum((L - G + B)^2)` and therefore flattens the whole profile.
#'
#' @inheritParams battery_solve_grid_window
#' @param lambda numeric, ramping penalty weight.
#'
#' @return numeric vector
#' @keywords internal
#'
battery_usage_window <- function(
  G,
  L,
  Bcap,
  Bc,
  Bd,
  SOCmin,
  SOCmax,
  SOCini,
  import_capacity,
  export_capacity,
  lambda = 0
) {
  time_slots <- length(G)
  lambdaMat <- get_lambda_matrix(time_slots)

  # min sum(B^2) + lambda * sum((B_t - B_{t-1})^2)
  P <- 2 * (diag(time_slots) + lambda * lambdaMat)
  q <- rep(0, time_slots)

  battery_solve_grid_window(
    G,
    L,
    Bcap,
    Bc,
    Bd,
    SOCmin,
    SOCmax,
    SOCini,
    import_capacity,
    export_capacity,
    P,
    q
  )
}


#' @keywords internal
battery_capacity_window <- function(
  G,
  L,
  Bcap,
  Bc,
  Bd,
  SOCmin,
  SOCmax,
  SOCini,
  import_capacity,
  export_capacity,
  lambda = 0
) {
  # Overshoot is measured on the NET flow, not on the one-sided imported /
  # exported series. The two agree for non-negative capacities, but a negative
  # `import_capacity` is a forced-export obligation: a net import of 60 kW
  # against a -100 kW capacity is a 160 kW overshoot, which the one-sided
  # `imported - import_capacity` would also report as 160 while treating any
  # slot that already exports as compliant. Working from the net flow keeps the
  # overshoot test right in both directions.
  #
  # Rounded to 2 decimals to match `battery_solve_grid_window()`, which rounds
  # its inputs before deriving the very capacities this test anticipates: an
  # overshoot too small to survive that rounding is not one the solver would
  # act on either.
  net <- round(L, 2) - round(G, 2)
  imported_over <- pmax(net - import_capacity, 0)
  exported_over <- pmax(-net - export_capacity, 0)
  imported_over[!is.finite(imported_over)] <- 0
  exported_over[!is.finite(exported_over)] <- 0

  # Nothing to solve for: the window is within both capacities, so the least
  # battery that keeps it there is no battery at all. Short-circuiting here is
  # what keeps the battery idle outside congestion instead of cycling it for a
  # marginal gain the objective would otherwise still find.
  tol <- optimization_solution_tolerance()
  if (!any(imported_over > tol) && !any(exported_over > tol)) {
    return(rep(0, length(G)))
  }

  # The whole battery is offered, not a reserve sized to the overshoot. Sizing
  # a reserve was a proxy for "use as little as possible" needed only because
  # the objective underneath was `battery_grid_window()`, which spends whatever
  # it is given on flattening the profile. `battery_usage_window()` asks for
  # the minimum directly, so the nameplate no longer decides how hard the
  # battery works - the capacity does - and a large battery on a small
  # overshoot now behaves like a small one instead of being throttled by a
  # reserve that could also be sized too small to clear the window.
  battery_usage_window(
    G,
    L,
    Bcap,
    Bc,
    Bd,
    SOCmin,
    SOCmax,
    SOCini,
    import_capacity,
    export_capacity,
    lambda
  )
}


# Unified cost solver: X = [B_c, B_d, I, E] (+ m when cycle_cost == 0) ---------

#' @keywords internal
battery_solve_cost_unified_window <- function(
  G,
  L,
  PI,
  PE,
  Bcap,
  Bc,
  Bd,
  SOCmin,
  SOCmax,
  SOCini,
  import_capacity,
  export_capacity,
  charge_eff = 1,
  discharge_eff = 1,
  cycle_cost = 0,
  P_B = NULL,
  q_B = NULL
) {
  G <- round(G, 2)
  L <- round(L, 2)
  n <- length(G)
  cumsumMat <- triangulate_matrix(matrix(1, n, n), "l")
  zeroMat <- matrix(0, n, n)

  lb_soc <- rep((SOCmin - SOCini) / 100 * Bcap, n)
  ub_soc <- rep((SOCmax - SOCini) / 100 * Bcap, n)

  import_capacity <- as.numeric(rep_len(import_capacity, n))
  export_capacity <- as.numeric(rep_len(export_capacity, n))

  # `import_capacity` / `export_capacity` limit the NET grid flow, whereas I
  # and E are one-sided and non-negative. A negative capacity is therefore an
  # obligation to flow the other way: it caps this side at zero - hence the
  # pmax(., 0), without which the variable box becomes inconsistent
  # ("Col N has inconsistent bounds [0, -100]") and the whole window is
  # abandoned - and is enforced on the net flow by the capacity rows below.
  import_mode_ub <- pmax(L - G + Bc, 0)
  export_mode_ub <- pmax(G - L + Bd, 0)
  import_mode_ub[import_mode_ub < 1e-9] <- 0
  export_mode_ub[export_mode_ub < 1e-9] <- 0
  import_mode_ub[is.finite(import_capacity)] <- pmin(
    import_mode_ub[is.finite(import_capacity)],
    pmax(import_capacity[is.finite(import_capacity)], 0)
  )
  export_mode_ub[is.finite(export_capacity)] <- pmin(
    export_mode_ub[is.finite(export_capacity)],
    pmax(export_capacity[is.finite(export_capacity)], 0)
  )

  # How far each capacity may be missed (see optimization_slack_ceiling()).
  tol <- optimization_solution_tolerance()
  slack_import_max <- optimization_slack_ceiling(import_capacity, L - G)
  slack_export_max <- optimization_slack_ceiling(export_capacity, G - L)
  soft_caps <- any(slack_import_max > 0) || any(slack_export_max > 0)
  slack_import_max[slack_import_max > 0] <-
    slack_import_max[slack_import_max > 0] + tol
  slack_export_max[slack_export_max > 0] <-
    slack_export_max[slack_export_max > 0] + tol

  # Explicit rows on the net flow are needed only when a capacity can be
  # missed, or is negative and so cannot be expressed as a one-sided box. With
  # the default infinite capacities this is FALSE and the problem below is
  # exactly the one solved before soft capacities existed.
  capacity_rows <- soft_caps ||
    any(import_capacity < 0) ||
    any(export_capacity < 0)
  n_slack <- if (capacity_rows) 2 * n else 0
  penalty <- optimization_slack_penalty(
    c(pmax(import_capacity, L - G), pmax(export_capacity, G - L))
  )

  # I_t - E_t - s_i,t <= import_capacity_t   (net import, soft)
  # E_t - I_t - s_e,t <= export_capacity_t   (net export, soft)
  # `lead` is the number of variables preceding the slack block.
  capacity_row_block <- function(lead) {
    rows <- matrix(0, 2 * n, lead + 2 * n)
    i1 <- seq_len(n)
    i2 <- n + seq_len(n)
    rows[cbind(i1, 2 * n + seq_len(n))] <- 1
    rows[cbind(i1, 3 * n + seq_len(n))] <- -1
    rows[cbind(i1, lead + seq_len(n))] <- -1
    rows[cbind(i2, 3 * n + seq_len(n))] <- 1
    rows[cbind(i2, 2 * n + seq_len(n))] <- -1
    rows[cbind(i2, lead + n + seq_len(n))] <- -1
    rows
  }
  capacity_rhs <- c(import_capacity, export_capacity)
  slack_lb <- rep(0, n_slack)
  slack_ub <- if (capacity_rows) {
    c(slack_import_max, slack_export_max)
  } else {
    numeric(0)
  }

  # X = [B_c_1..B_c_n, B_d_1..B_d_n, I_1..I_n, E_1..E_n]
  # (+ m_1..m_n appended in the MILP branch when cycle_cost == 0)
  #
  # B_c: grid-side charge power  (stored energy = B_c * charge_eff per slot)
  # B_d: grid-side discharge power (released energy = B_d / discharge_eff per slot)
  #
  # Balance:             B_c - B_d - I + E = G - L
  # SOC evolution:       cumsum(\u03b7_c * B_c - (1/\u03b7_d) * B_d) in [lb_soc, ub_soc]
  # Energy conservation: sum(\u03b7_c * B_c - (1/\u03b7_d) * B_d) = 0

  A_balance <- cbind(diag(n), -diag(n), -diag(n), diag(n))
  A_soc <- cbind(
    charge_eff * cumsumMat,
    -(1 / discharge_eff) * cumsumMat,
    zeroMat,
    zeroMat
  )
  A_energy <- matrix(
    c(rep(charge_eff, n), rep(-1 / discharge_eff, n), rep(0, 2 * n)),
    nrow = 1
  )

  A_vars <- rbind(A_balance, A_soc, A_energy)
  lb_vars <- c(G - L, lb_soc, 0)
  ub_vars <- c(G - L, ub_soc, 0)

  # Penalise each kW\u00b7slot of discharge: 1 kW\u00b7slot / Bcap = one full-cycle fraction.
  cycle_coef <- cycle_cost / Bcap

  if (cycle_cost > 0 || !is.null(P_B)) {
    # LP/QP path \u2014 binary mode variable is skipped because:
    #   cycle_cost > 0: makes simultaneous B_c, B_d > 0 economically
    #     self-defeating
    #   P_B != NULL: the quadratic term already makes simultaneous
    #     charge+discharge suboptimal
    q_Bc <- if (!is.null(q_B)) q_B else rep(0, n)
    q_Bd <- if (!is.null(q_B)) -q_B else rep(0, n)

    if (!is.null(P_B)) {
      # QP path via OSQP with 5n incremental SOC formulation.
      # Variables: [B_c_1..n, B_d_1..n, I_1..n, E_1..n, S_1..n] (5n)
      # S_t tracks cumulative SOC change; bidiagonal A_soc_incr gives O(n) nnz.
      #
      # Clip PE so the QP stays bounded when export price >= import price.
      PE_clipped <- pmin(PE, PI)
      if (any(PE_clipped != PE)) {
        message_once(
          "\u26a0\ufe0f Optimization: export price exceeds import price; clipping for bounded QP."
        )
      }
      ub_I <- import_mode_ub
      ub_E <- export_mode_ub

      nv <- 5 * n + n_slack
      idx_Bc <- seq_len(n)
      idx_Bd <- seq(n + 1, 2 * n)
      idx_I <- seq(2 * n + 1, 3 * n)
      idx_E <- seq(3 * n + 1, 4 * n)
      idx_S <- seq(4 * n + 1, 5 * n)

      # Sparse P on [B_c, B_d, I, E, S] (+ slack): quadratic on (B_c - B_d)
      P_5n <- Matrix::sparseMatrix(
        i = integer(),
        j = integer(),
        x = numeric(),
        dims = c(nv, nv)
      )
      P_5n[idx_Bc, idx_Bc] <- P_B
      P_5n[idx_Bc, idx_Bd] <- -P_B
      P_5n[idx_Bd, idx_Bc] <- -P_B
      P_5n[idx_Bd, idx_Bd] <- P_B
      P_5n <- (P_5n + Matrix::t(P_5n)) / 2

      if (n_slack > 0) {
        idx_slack <- seq(5 * n + 1, nv)
        # Ridge on the slack block keeps the objective strictly convex.
        P_5n[idx_slack, idx_slack] <- (1e-6 * penalty) * diag(n_slack)
      }

      # Balance: B_c_t - B_d_t - I_t + E_t = G_t - L_t
      A_bal <- Matrix::sparseMatrix(
        i = rep(seq_len(n), 4),
        j = c(idx_Bc, idx_Bd, idx_I, idx_E),
        x = c(rep(1, n), rep(-1, n), rep(-1, n), rep(1, n)),
        dims = c(n, nv)
      )

      # Incremental SOC: S_t - S_{t-1} - \u03b7_c*B_c_t + (1/\u03b7_d)*B_d_t = 0
      soc_rows <- c(seq_len(n), seq_len(n), seq_len(n), seq(2, n))
      soc_cols <- c(idx_Bc, idx_Bd, idx_S, idx_S[seq_len(n - 1)])
      soc_vals <- c(
        rep(-charge_eff, n),
        rep(1 / discharge_eff, n),
        rep(1, n),
        rep(-1, n - 1)
      )
      A_soc_incr <- Matrix::sparseMatrix(
        i = soc_rows,
        j = soc_cols,
        x = soc_vals,
        dims = c(n, nv)
      )

      # Energy conservation: S_n = 0 (battery returns to initial SOC)
      A_energy_5n <- Matrix::sparseMatrix(
        i = 1L,
        j = idx_S[n],
        x = 1.0,
        dims = c(1, nv)
      )

      A_osqp <- rbind(
        Matrix::Diagonal(n = nv), # variable bounds
        A_bal,
        A_soc_incr,
        A_energy_5n
      )
      lower_osqp <- c(
        rep(0, n),
        rep(0, n),
        rep(0, n),
        rep(0, n),
        lb_soc,
        slack_lb,
        G - L,
        rep(0, n),
        0
      )
      upper_osqp <- c(
        rep(Bc, n),
        rep(Bd, n),
        ub_I,
        ub_E,
        ub_soc,
        slack_ub,
        G - L,
        rep(0, n),
        0
      )

      if (capacity_rows) {
        A_osqp <- rbind(A_osqp, Matrix::Matrix(capacity_row_block(5 * n)))
        lower_osqp <- c(lower_osqp, rep(-Inf, 2 * n))
        upper_osqp <- c(upper_osqp, capacity_rhs)
      }

      # Normalize objective so OSQP's ADMM rho is well-matched regardless of w.
      q_osqp <- c(
        q_Bc,
        rep(cycle_coef, n) + q_Bd,
        PI,
        -PE_clipped,
        rep(0, n),
        rep(penalty, n_slack)
      )
      obj_scale <- max(max(abs(P_5n@x)), max(abs(q_osqp)), 1e-6)
      sol <- solve_osqp(
        P_5n / obj_scale,
        q_osqp / obj_scale,
        A_osqp,
        lower_osqp,
        upper_osqp
      )

      if (!is.null(sol$x)) {
        x <- as.numeric(sol$x)
        B_c <- x[idx_Bc]
        B_d <- x[idx_Bd]
        return(round(B_c * charge_eff - B_d / discharge_eff, 10))
      }

      # Fallback to HiGHS LP on OSQP failure (loses quadratic grid term)
      message_once(
        "\u26a0\ufe0f Optimization warning: OSQP failed for cost/combined. Falling back to HiGHS."
      )
    }

    # LP layout: [B_c, B_d, I, E] (+ s_i, s_e when a capacity can be missed)
    A_lp <- cbind(A_vars, matrix(0, nrow(A_vars), n_slack))
    lhs_lp <- lb_vars
    rhs_lp <- ub_vars
    if (capacity_rows) {
      A_lp <- rbind(A_lp, capacity_row_block(4 * n))
      lhs_lp <- c(lhs_lp, rep(-Inf, 2 * n))
      rhs_lp <- c(rhs_lp, capacity_rhs)
    }

    result <- highs::highs_solve(
      Q = NULL,
      L = c(
        q_Bc,
        rep(cycle_coef, n) + q_Bd,
        PI,
        -PE,
        rep(penalty, n_slack)
      ),
      lower = c(rep(0, n), rep(0, n), rep(0, n), rep(0, n), slack_lb),
      upper = c(
        rep(Bc, n),
        rep(Bd, n),
        pmin(pmax(import_capacity, 0), import_mode_ub),
        pmin(pmax(export_capacity, 0), export_mode_ub),
        slack_ub
      ),
      A = A_lp,
      lhs = lhs_lp,
      rhs = rhs_lp,
      types = rep(1L, 4 * n + n_slack),
      control = optimization_highs_options()
    )
  } else {
    # MILP path \u2014 binary mode variable m prevents simultaneous charge+discharge.
    # Layout: [B_c, B_d, I, E, m] (+ s_i, s_e when a capacity can be missed)
    A_base <- cbind(A_vars, matrix(0, nrow(A_vars), n + n_slack))
    # Import mode: I_t <= import_mode_ub_t * m_t
    A_import_mode <- cbind(
      matrix(0, n, n), # B_c
      matrix(0, n, n), # B_d
      diag(n), # I
      matrix(0, n, n), # E
      -diag(import_mode_ub), # -M_I * m
      matrix(0, n, n_slack) # slack
    )
    # Export mode: E_t <= export_mode_ub_t * (1 - m_t)
    A_export_mode <- cbind(
      matrix(0, n, n), # B_c
      matrix(0, n, n), # B_d
      matrix(0, n, n), # I
      diag(n), # E
      diag(export_mode_ub), # M_E * m
      matrix(0, n, n_slack) # slack
    )
    A_full <- rbind(A_base, A_import_mode, A_export_mode)
    lhs_full <- c(lb_vars, rep(-Inf, 2 * n))
    rhs_full <- c(ub_vars, rep(0, n), export_mode_ub)
    if (capacity_rows) {
      A_full <- rbind(A_full, capacity_row_block(5 * n))
      lhs_full <- c(lhs_full, rep(-Inf, 2 * n))
      rhs_full <- c(rhs_full, capacity_rhs)
    }

    result <- highs::highs_solve(
      Q = NULL,
      L = c(
        rep(0, n),
        rep(0, n),
        PI,
        -PE,
        rep(0, n),
        rep(penalty, n_slack)
      ),
      lower = c(
        rep(0, n),
        rep(0, n),
        rep(0, n),
        rep(0, n),
        rep(0, n),
        slack_lb
      ),
      upper = c(
        rep(Bc, n),
        rep(Bd, n),
        # pmax(., 0) only: keeping the box otherwise untouched leaves the
        # unconstrained problem bit-for-bit identical to the pre-slack solver,
        # which matters because the MILP has ties and a tighter (still valid)
        # box makes HiGHS pick a different optimum of equal cost.
        pmax(import_capacity, 0),
        pmax(export_capacity, 0),
        rep(1, n),
        slack_ub
      ),
      A = A_full,
      lhs = lhs_full,
      rhs = rhs_full,
      types = c(rep(1L, 4 * n), rep(2L, n), rep(1L, n_slack)),
      control = optimization_highs_options(include_mip_gap = TRUE)
    )
  }

  if (
    identical(result$status_message, "Optimal") &&
      !is.null(result$primal_solution)
  ) {
    sol <- as.numeric(result$primal_solution)
    B_c <- sol[seq_len(n)]
    B_d <- sol[seq(n + 1, 2 * n)]
    # Return storage-side power (consistent with all other objectives) so that
    # get_conversion_losses() can be applied uniformly to get grid-side flows.
    return(round(B_c * charge_eff - B_d / discharge_eff, 10))
  }

  rep(0, n)
}


#' @keywords internal
battery_cost_window <- function(
  G,
  L,
  PI,
  PE,
  Bcap,
  Bc,
  Bd,
  SOCmin,
  SOCmax,
  SOCini,
  import_capacity,
  export_capacity,
  lambda = 0,
  charge_eff = 1,
  discharge_eff = 1,
  cycle_cost = 0
) {
  n <- length(G)

  P_B <- if (lambda > 0) {
    lambdaMat <- get_lambda_matrix(n)
    2 * lambda * (lambdaMat + 1e-6 * diag(n))
  } else {
    NULL
  }

  battery_solve_cost_unified_window(
    G,
    L,
    PI,
    PE,
    Bcap,
    Bc,
    Bd,
    SOCmin,
    SOCmax,
    SOCini,
    import_capacity,
    export_capacity,
    charge_eff = charge_eff,
    discharge_eff = discharge_eff,
    cycle_cost = cycle_cost,
    P_B = P_B,
    q_B = if (!is.null(P_B)) rep(0, n) else NULL
  )
}


#' @keywords internal
battery_combined_window <- function(
  G,
  L,
  PI,
  PE,
  Bcap,
  Bc,
  Bd,
  SOCmin,
  SOCmax,
  SOCini,
  import_capacity,
  export_capacity,
  w,
  lambda = 0
) {
  G <- round(G, 2)
  L <- round(L, 2)
  n <- length(G)
  scale <- mean(PI)^2

  # Quadratic grid term on net battery power B
  P_grid <- 2 * w * scale * diag(n)
  q_grid <- 2 * w * scale * (L - G)

  if (lambda > 0) {
    lambdaMat <- get_lambda_matrix(n)
    P_B <- P_grid + 2 * lambda * (lambdaMat + 1e-6 * diag(n))
  } else {
    P_B <- P_grid
  }

  # Clip PE so the QP stays bounded when export price >= import price.
  PE_clipped <- pmin(PE, PI)
  if (any(PE_clipped != PE)) {
    message_once(
      "\u26a0\ufe0f Optimization: export price exceeds import price; clipping for bounded QP."
    )
  }

  lb_soc <- rep((SOCmin - SOCini) / 100 * Bcap, n)
  ub_soc <- rep((SOCmax - SOCini) / 100 * Bcap, n)

  import_capacity <- as.numeric(rep_len(import_capacity, n))
  export_capacity <- as.numeric(rep_len(export_capacity, n))

  # A negative capacity is an obligation to flow the other way: it caps this
  # one-sided variable at zero and is enforced on the net flow below.
  ub_I <- pmin(pmax(import_capacity, 0), pmax(L - G + Bc, 0))
  ub_E <- pmin(pmax(export_capacity, 0), pmax(G - L + Bd, 0))

  tol <- optimization_solution_tolerance()
  slack_import_max <- optimization_slack_ceiling(import_capacity, L - G)
  slack_export_max <- optimization_slack_ceiling(export_capacity, G - L)
  soft_caps <- any(slack_import_max > 0) || any(slack_export_max > 0)
  slack_import_max[slack_import_max > 0] <-
    slack_import_max[slack_import_max > 0] + tol
  slack_export_max[slack_export_max > 0] <-
    slack_export_max[slack_export_max > 0] + tol
  capacity_rows <- soft_caps ||
    any(import_capacity < 0) ||
    any(export_capacity < 0)
  n_slack <- if (capacity_rows) 2 * n else 0
  nv <- 3 * n + n_slack
  penalty <- optimization_slack_penalty(
    c(pmax(import_capacity, L - G), pmax(export_capacity, G - L))
  )

  cumsumMat <- triangulate_matrix(matrix(1, n, n), "l")
  zeroMat <- matrix(0, n, n)
  slackMat <- matrix(0, n, n_slack)

  # Sparse P on [B, I, E] (+ slack): quadratic only on B block (O(n) nnz)
  P_3n <- Matrix::sparseMatrix(
    i = integer(),
    j = integer(),
    x = numeric(),
    dims = c(nv, nv)
  )
  P_3n[seq_len(n), seq_len(n)] <- P_B
  if (n_slack > 0) {
    idx_slack <- seq(3 * n + 1, nv)
    P_3n[idx_slack, idx_slack] <- (1e-6 * penalty) * diag(n_slack)
  }
  P_3n <- (P_3n + Matrix::t(P_3n)) / 2

  # OSQP constraint matrix: variable bounds + balance + SOC + energy conservation
  A_osqp <- rbind(
    cbind(diag(n), zeroMat, zeroMat, slackMat),
    cbind(zeroMat, diag(n), zeroMat, slackMat),
    cbind(zeroMat, zeroMat, diag(n), slackMat),
    cbind(diag(n), -diag(n), diag(n), slackMat),
    cbind(cumsumMat, zeroMat, zeroMat, slackMat),
    matrix(c(rep(1, n), rep(0, 2 * n + n_slack)), nrow = 1)
  )
  lower_osqp <- c(rep(-Bd, n), rep(0, n), rep(0, n), G - L, lb_soc, 0)
  upper_osqp <- c(rep(Bc, n), ub_I, ub_E, G - L, ub_soc, 0)

  if (capacity_rows) {
    # I_t - E_t - s_i,t <= import_capacity_t
    # E_t - I_t - s_e,t <= export_capacity_t
    slack_box <- cbind(matrix(0, n_slack, 3 * n), diag(n_slack))
    cap_rows <- rbind(
      cbind(zeroMat, diag(n), -diag(n), -diag(n), zeroMat),
      cbind(zeroMat, -diag(n), diag(n), zeroMat, -diag(n))
    )
    A_osqp <- rbind(A_osqp, slack_box, cap_rows)
    upper_osqp <- c(
      upper_osqp,
      slack_import_max,
      slack_export_max,
      import_capacity,
      export_capacity
    )
    lower_osqp <- c(lower_osqp, rep(0, n_slack), rep(-Inf, 2 * n))
  }

  sol <- solve_osqp(
    P_3n,
    c(
      q_grid,
      (1 - w) * PI,
      -(1 - w) * PE_clipped,
      rep(penalty, n_slack)
    ),
    A_osqp,
    lower_osqp,
    upper_osqp
  )

  if (!is.null(sol$x)) {
    profile <- round(sol$x[seq_len(n)], 10)
    if (!soft_caps) {
      return(profile)
    }
    # Same linear-slack indifference as in `battery_solve_grid_window()`: spend
    # the energy on meeting the capacity for as many slots as it covers rather
    # than missing it marginally everywhere. Only `B` is returned, so the I / E
    # variables need no repair of their own.
    return(
      optimization_concentrate_slack(
        B = profile,
        net0 = L - G,
        import_capacity = import_capacity,
        export_capacity = export_capacity,
        lb_B = rep(-Bd, n),
        ub_B = rep(Bc, n),
        lb_cumsum = lb_soc,
        ub_cumsum = ub_soc
      )
    )
  }

  message_once(
    "\u26a0\ufe0f Optimization warning: OSQP failed for combined objective."
  )
  rep(0, n)
}


# Public API -------------------------------------------------------------------

#' Battery optimal charging/discharging profile
#'
#' See the formulation of the optimization problems in the
#' [documentation website](https://resourcefully-dev.github.io/flextools/).
#'
#' @param opt_data tibble, optimization contextual data.
#' The first column must be named `datetime` (mandatory).
#' Optional columns:
#'
#' - `static`: static power demand (kW)
#' - `production`: local generation (kW)
#' - `import_capacity`: max grid import (kW)
#' - `export_capacity`: max grid export (kW)
#' - `price_imported`: energy import price (required for cost/combined)
#' - `price_exported`: energy export price (required for cost/combined)
#'
#' `import_capacity` and `export_capacity` constrain the **net** grid flow. A
#' negative value is therefore an obligation to flow the other way:
#' `import_capacity = -100` requires at least 100 kW of export in that slot, as
#' a congestion contract might. Such an obligation outranks `opt_objective` —
#' the battery meets the capacity first and optimises second.
#'
#' A capacity the battery cannot physically reach is *approached*, not dropped:
#' the battery operates at its limit and a warning is emitted once. The result
#' is still guaranteed never to be worse than the profile without a battery, and
#' slots that can meet their capacity remain strictly capped. The unavoidable
#' miss is *concentrated*: the capacity is met exactly for as many slots as the
#' battery's energy covers, rather than spread thinly so that every slot ends
#' marginally over. Both leave the same volume unserved, but only the former
#' reduces the number of slots in violation (see `get_energy_kpis()`'s
#' `congestion_time`).
#'
#' @param opt_objective character or numeric.
#' `"grid"` (default), `"capacity"`, `"cost"`, or a numeric weight `w`
#' where `w=1` is pure grid and `w=0` is pure cost.
#'
#' `"grid"` minimises the net grid flow, so it flattens the profile with
#' whatever battery it is given. `"capacity"` instead minimises the *battery
#' usage* that keeps the net flow inside `import_capacity` and
#' `export_capacity`: windows within both capacities are left untouched, and in
#' the windows that are not, the battery discharges onto the capacity line
#' rather than below it and recharges at the lowest power the window allows.
#' Use it to size the minimum battery a congestion limit needs; use `"grid"` to
#' minimise the peak itself.
#' @param Bcap numeric, battery capacity (kWh)
#' @param Bc numeric, maximum charging power (kW)
#' @param Bd numeric, maximum discharging power (kW)
#' @param SOCmin numeric, minimum State-of-Charge (%)
#' @param SOCmax numeric, maximum State-of-Charge (%)
#' @param SOCini numeric, initial State-of-Charge (%). Defaults to `SOCmin`.
#' @param window_days integer, optimization window length in days.
#' @param window_start_hour integer, start hour of each optimization window.
#' @param flex_window_hours numeric, flexibility window length (hours).
#' @param lambda numeric, ramping penalty weight. Penalises rapid changes in
#'   battery power between consecutive time slots.
#' @param charge_eff numeric, charging efficiency in (0, 1]. Default 1 (lossless).
#'   Embeds round-trip losses in the SOC constraints for accurate energy accounting.
#' @param discharge_eff numeric, discharging efficiency in (0, 1]. Default 1 (lossless).
#'   See `charge_eff`.
#' @param cycle_cost numeric, degradation cost per kWh cycled (Euro/kWh). Default 0.
#'   Adds a linear penalty on battery discharge so the optimizer trades off energy
#'   cost savings against battery wear. When positive, the problem is solved as a
#'   pure LP (no binary variables) which is substantially faster than the default MILP.
#'
#' @return numeric vector
#' @export
#'
#' @importFrom dplyr tibble %>% mutate left_join arrange
#' @importFrom purrr map
#'
#' @examples
#' library(dplyr)
#' opt_data <- flextools::energy_profiles %>%
#'   filter(lubridate::isoweek(datetime) == 18) %>%
#'   rename(production = "solar", static = "building") %>%
#'   select(any_of(c(
#'     "datetime", "production", "static", "price_imported", "price_exported"
#'   )))
#' opt_battery <- opt_data %>%
#'   add_battery_optimization(
#'     opt_objective = "grid",
#'     Bcap = 50, Bc = 4, Bd = 4,
#'     window_start_hour = 5
#'   )
#'
add_battery_optimization <- function(
  opt_data,
  opt_objective = "grid",
  Bcap,
  Bc,
  Bd,
  SOCmin = 0,
  SOCmax = 100,
  SOCini = NULL,
  window_days = 1,
  window_start_hour = 0,
  flex_window_hours = 24,
  lambda = 0,
  charge_eff = 1,
  discharge_eff = 1,
  cycle_cost = 0
) {
  if (is.null(opt_data)) {
    stop("Error: `opt_data` parameter is empty.")
  }
  opt_data <- opt_data %>% mutate(flexible = 0)
  opt_data <- check_optimization_data(opt_data, opt_objective)

  if (Bcap == 0 || Bc == 0 || Bd == 0 || SOCmin == SOCmax) {
    message(
      "\u26a0\ufe0f Optimization warning: battery parameters don't allow optimization."
    )
    return(rep(0, nrow(opt_data)))
  }

  if (is.null(SOCini)) {
    SOCini <- 0
  }
  if (SOCini < SOCmin) {
    SOCini <- SOCmin
  }
  if (SOCini > SOCmax) {
    SOCini <- SOCmax
  }

  if (charge_eff <= 0 || charge_eff > 1) {
    stop("Error: charge_eff must be in (0, 1]")
  }
  if (discharge_eff <= 0 || discharge_eff > 1) {
    stop("Error: discharge_eff must be in (0, 1]")
  }
  if (!is.numeric(cycle_cost) || length(cycle_cost) != 1 || cycle_cost < 0) {
    stop("Error: cycle_cost must be a non-negative number.")
  }

  # Collapse numeric endpoints to named objectives for simpler dispatch
  if (is.numeric(opt_objective)) {
    if (opt_objective <= 0) {
      opt_objective <- "cost"
    } else if (opt_objective >= 1) {
      opt_objective <- "grid"
    }
  }

  dttm_seq <- opt_data$datetime
  time_resolution <- get_time_resolution(dttm_seq)
  flex_windows_idxs <- get_flex_windows(
    dttm_seq = dttm_seq,
    window_days = window_days,
    window_start_hour = window_start_hour,
    flex_window_hours = flex_window_hours
  )
  flex_windows_idxs_seq <- as.numeric(unlist(flex_windows_idxs$flex_idx))
  windows_data <- map(flex_windows_idxs$flex_idx, ~ opt_data[.x, ])
  Bcap_scaled <- Bcap * 60 / time_resolution

  reset_message_once()

  if (opt_objective == "grid") {
    B_windows <- map(
      windows_data,
      ~ battery_grid_window(
        G = .x$production,
        L = .x$static,
        Bcap = Bcap_scaled,
        Bc = Bc,
        Bd = Bd,
        SOCmin = SOCmin,
        SOCmax = SOCmax,
        SOCini = SOCini,
        import_capacity = .x$import_capacity,
        export_capacity = .x$export_capacity,
        lambda = lambda
      )
    )
  } else if (opt_objective == "capacity") {
    B_windows <- map(
      windows_data,
      ~ battery_capacity_window(
        G = .x$production,
        L = .x$static,
        Bcap = Bcap_scaled,
        Bc = Bc,
        Bd = Bd,
        SOCmin = SOCmin,
        SOCmax = SOCmax,
        SOCini = SOCini,
        import_capacity = .x$import_capacity,
        export_capacity = .x$export_capacity,
        lambda = lambda
      )
    )
  } else if (opt_objective == "cost") {
    B_windows <- map(
      windows_data,
      ~ battery_cost_window(
        G = .x$production,
        L = .x$static,
        PI = .x$price_imported,
        PE = .x$price_exported,
        Bcap = Bcap_scaled,
        Bc = Bc,
        Bd = Bd,
        SOCmin = SOCmin,
        SOCmax = SOCmax,
        SOCini = SOCini,
        import_capacity = .x$import_capacity,
        export_capacity = .x$export_capacity,
        lambda = lambda,
        charge_eff = charge_eff,
        discharge_eff = discharge_eff,
        cycle_cost = cycle_cost
      )
    )
  } else if (is.numeric(opt_objective)) {
    B_windows <- map(
      windows_data,
      ~ battery_combined_window(
        G = .x$production,
        L = .x$static,
        PI = .x$price_imported,
        PE = .x$price_exported,
        Bcap = Bcap_scaled,
        Bc = Bc,
        Bd = Bd,
        SOCmin = SOCmin,
        SOCmax = SOCmax,
        SOCini = SOCini,
        import_capacity = .x$import_capacity,
        export_capacity = .x$export_capacity,
        w = opt_objective,
        lambda = lambda
      )
    )
  } else {
    stop("Error: invalid `opt_objective`")
  }

  B <- as.numeric(unlist(B_windows))

  if (length(flex_windows_idxs_seq) == length(dttm_seq)) {
    return(B)
  }

  B_flex <- left_join(
    tibble(idx = seq_len(length(dttm_seq))),
    tibble(idx = flex_windows_idxs_seq, B = B),
    by = "idx"
  ) %>%
    arrange(.data$idx)

  B_flex$B[is.na(B_flex$B)] <- 0
  B_flex$B
}
