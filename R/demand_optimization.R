# General functions -------------------------------------------------------

# Active shared helpers now live in `optimization.R`. Demand keeps only the
# demand-specific wrappers and formulations here so the common backend logic is
# defined once for demand, battery and V2G.

get_bounds <- function(
  time_slots,
  G,
  LF,
  LS,
  direction,
  time_horizon,
  LFmax,
  import_capacity,
  export_capacity,
  cumsum_min = NULL
) {
  identityMat <- diag(time_slots)
  cumsumMat <- triangulate_matrix(matrix(1, time_slots, time_slots), "l")

  # The general demand bounds reflect three physical restrictions:
  # 1. optimized flexible load cannot be negative,
  # 2. it cannot exceed its technical maximum,
  # 3. it must respect grid import/export constraints after accounting for
  #    production and static load.
  LFmax_vct <- round(pmin(pmax(G - LS + import_capacity, 0), LFmax), 2)
  lb_O <- round(
    pmin(pmax(G - LS - export_capacity, 0), LFmax_vct),
    2
  )

  # Forward and backward shifting are represented by different triangular
  # matrices, but the rest of the algebra is kept exactly as in the original
  # derivation so the time-horizon meaning does not change.
  Amat_cumsum <- cumsumMat
  if (direction == "forward") {
    if (time_horizon == time_slots) {
      horizonMat_cumsum <- matrix(0, time_slots, time_slots)
    } else {
      horizonMat_cumsum <- triangulate_matrix(
        matrix(1, time_slots, time_slots),
        "l",
        -time_horizon
      )
    }
    horizonMat_identity <- triangulate_matrix(
      triangulate_matrix(matrix(1, time_slots, time_slots), "l"),
      "u",
      -time_horizon
    )

    lb_cumsum <- horizonMat_cumsum %*% LF
    ub_cumsum <- cumsumMat %*% LF

    # Departure deadlines (`cumsum_min`, see get_flex_envelope()): the energy
    # owed to the sessions that have left by each slot must already be in
    # `cumsum(O)`. Capped at `cumsum(LF)`, so the original profile stays a
    # feasible point (the fallback in demand_solve_window() relies on it).
    if (!is.null(cumsum_min)) {
      lb_cumsum <- pmin(
        pmax(lb_cumsum, rep_len(as.numeric(cumsum_min), time_slots)),
        ub_cumsum
      )
    }

    ub_shift <- horizonMat_identity %*% LF
    ub_O <- pmin(pmax(ub_shift, lb_O), LFmax_vct)
  } else {
    horizonMat_cumsum <- triangulate_matrix(
      matrix(1, time_slots, time_slots),
      "l",
      time_horizon
    )
    horizonMat_identity <- triangulate_matrix(
      triangulate_matrix(matrix(1, time_slots, time_slots), "u"),
      "l",
      time_horizon
    )

    lb_cumsum <- cumsumMat %*% LF
    ub_cumsum <- horizonMat_cumsum %*% LF

    ub_shift <- horizonMat_identity %*% LF
    ub_O <- pmin(pmax(ub_shift, lb_O), LFmax_vct)
  }

  list(
    Amat_O = identityMat,
    lb_O = lb_O,
    ub_O = ub_O,
    Amat_cumsum = Amat_cumsum,
    lb_cumsum = lb_cumsum,
    ub_cumsum = ub_cumsum
  )
}


demand_highs_is_optimal <- function(result) {
  identical(result$status_message, "Optimal")
}


#' Validate the energy range of a demand window
#'
#' The optimized load carries, by default, exactly the energy of the flexible
#' profile it starts from. `energy_ratio = c(min, max)` widens that to a range
#' `[min, max] * sum(LF)`. A range below 1 only makes sense when energy may be
#' left undelivered, which the cumulative-shift constraints admit only for
#' forward shifting over the whole window: any finite horizon, and the
#' backward direction, pin `cumsum(O)` from below to the original profile.
#'
#' @param energy_ratio numeric vector `c(min, max)`, or `NULL` for `c(1, 1)`.
#' @param direction character, `forward` or `backward`.
#' @param time_horizon integer or `NULL` (whole window).
#' @param time_slots integer, number of slots in the window.
#'
#' @return the validated `c(min, max)`
#' @keywords internal
#'
demand_check_energy_ratio <- function(
  energy_ratio,
  direction,
  time_horizon,
  time_slots
) {
  if (is.null(energy_ratio)) {
    return(c(1, 1))
  }
  if (
    !is.numeric(energy_ratio) ||
      length(energy_ratio) != 2 ||
      any(!is.finite(energy_ratio))
  ) {
    stop("Error: `energy_ratio` must be two finite numbers, `c(min, max)`")
  }
  if (
    energy_ratio[1] < 0 ||
      energy_ratio[2] > 1 ||
      energy_ratio[1] > energy_ratio[2]
  ) {
    stop("Error: `energy_ratio` must satisfy `0 <= min <= max <= 1`")
  }
  if (energy_ratio[2] <= 0) {
    stop("Error: the maximum energy ratio must be higher than 0")
  }
  if (energy_ratio[1] < 1) {
    if (is.null(time_horizon)) {
      time_horizon <- time_slots
    }
    if (direction != "forward" || time_horizon != time_slots) {
      stop(
        "Error: an energy ratio below 1 requires `direction = \"forward\"` over the whole window (`time_horizon = NULL`)"
      )
    }
  }
  energy_ratio
}


#' Reward per unit of optimized load kept
#'
#' When the energy row is a range, the optimizer must be told to keep energy
#' rather than drop it: every objective in this file improves when load is
#' removed (less net flow, less cost). A uniform linear reward on `O` that
#' strictly dominates the objective's marginal gain from removing one unit —
#' `|d/dO| <= max row sum of |P| * max |X| + max |q|` anywhere in the feasible
#' box — makes the solver keep as much energy as the constraints admit and
#' shape it afterwards. Being the same in every slot it does not bias where the
#' energy goes, only how much of it stays.
#'
#' @param P numeric matrix or `NULL`, normalised quadratic term.
#' @param q numeric vector, linear term.
#' @param envelope numeric, bound on the absolute value of any decision
#'   variable in the feasible box (kW).
#'
#' @return numeric scalar
#' @keywords internal
#'
demand_energy_reward <- function(P, q, envelope) {
  envelope <- abs(envelope[is.finite(envelope)])
  envelope <- if (length(envelope)) max(envelope) else 0
  gradient_quadratic <- if (is.null(P)) 0 else max(rowSums(abs(P))) * envelope
  gradient_linear <- max(abs(q[is.finite(q)]), 0)
  10 * (gradient_quadratic + gradient_linear + 1)
}


demand_attach_profile <- function(
  optimized_load,
  imported = NULL,
  exported = NULL
) {
  if (!is.null(imported)) {
    attr(optimized_load, "import") <- as.numeric(imported)
  }
  if (!is.null(exported)) {
    attr(optimized_load, "export") <- as.numeric(exported)
  }

  optimized_load
}


demand_build_mode_constraints <- function(solver_data, bounds) {
  time_slots <- solver_data$time_slots
  total_variables <- solver_data$n_variables + time_slots

  # Import is allowed only when the grid mode is "import".
  A_import_mode <- matrix(0, nrow = time_slots, ncol = total_variables)
  A_import_mode[cbind(seq_len(time_slots), solver_data$import_idx)] <- 1
  A_import_mode[cbind(seq_len(time_slots), solver_data$grid_mode_idx)] <-
    -bounds$import_mode_ub

  # Export is allowed only when the grid mode is "export".
  A_export_mode <- matrix(0, nrow = time_slots, ncol = total_variables)
  A_export_mode[cbind(seq_len(time_slots), solver_data$export_idx)] <- 1
  A_export_mode[cbind(seq_len(time_slots), solver_data$grid_mode_idx)] <-
    bounds$export_mode_ub

  list(
    A = rbind(A_import_mode, A_export_mode),
    lhs = c(rep(-Inf, time_slots), rep(-Inf, time_slots)),
    rhs = c(rep(0, time_slots), bounds$export_mode_ub)
  )
}


demand_build_highs_problem <- function(
  solver_data,
  bounds,
  relax_binaries = TRUE,
  mode_lower = NULL,
  mode_upper = NULL
) {
  time_slots <- solver_data$time_slots
  total_variables <- solver_data$n_variables + time_slots

  if (is.null(mode_lower)) {
    mode_lower <- rep(0, time_slots)
  }
  if (is.null(mode_upper)) {
    mode_upper <- rep(1, time_slots)
  }

  mode_constraints <- demand_build_mode_constraints(solver_data, bounds)
  A_base <- cbind(
    solver_data$A,
    matrix(0, nrow = nrow(solver_data$A), ncol = time_slots)
  )
  Q <- NULL
  if (!is.null(solver_data$P)) {
    Q <- matrix(0, nrow = total_variables, ncol = total_variables)
    Q[
      seq_len(solver_data$n_variables),
      seq_len(solver_data$n_variables)
    ] <- solver_data$P
  }

  list(
    Q = Q,
    L = c(solver_data$q, rep(0, time_slots)),
    lower = c(rep(-Inf, solver_data$n_variables), mode_lower),
    upper = c(rep(Inf, solver_data$n_variables), mode_upper),
    A = rbind(A_base, mode_constraints$A),
    lhs = c(bounds$lb, mode_constraints$lhs),
    rhs = c(bounds$ub, mode_constraints$rhs),
    types = c(
      rep(1L, solver_data$n_variables),
      if (relax_binaries) rep(1L, time_slots) else rep(2L, time_slots)
    )
  )
}


demand_extract_solution <- function(x_value, solver_data) {
  optimized_load <- round(x_value[solver_data$optimized_idx], 2)

  if (!solver_data$has_grid_flows) {
    return(demand_attach_profile(optimized_load))
  }

  imported <- pmax(x_value[solver_data$import_idx], 0)
  exported <- pmax(x_value[solver_data$export_idx], 0)
  tolerance <- optimization_solution_tolerance()
  imported[imported < tolerance] <- 0
  exported[exported < tolerance] <- 0

  # Enforce physical exclusivity: continuous QP solvers (OSQP) may produce
  # small simultaneous I and E. Net = I - E is preserved by collapsing to one direction.
  both_positive <- imported > 0 & exported > 0
  if (any(both_positive)) {
    net <- imported - exported
    imported[both_positive] <- pmax(net[both_positive], 0)
    exported[both_positive] <- pmax(-net[both_positive], 0)
  }

  demand_attach_profile(optimized_load, imported, exported)
}


demand_solve_milp_window <- function(solver_data, bounds) {
  problem <- demand_build_highs_problem(
    solver_data = solver_data,
    bounds = bounds,
    relax_binaries = FALSE
  )
  result <- highs::highs_solve(
    Q = problem$Q,
    L = problem$L,
    lower = problem$lower,
    upper = problem$upper,
    A = problem$A,
    lhs = problem$lhs,
    rhs = problem$rhs,
    types = problem$types,
    control = optimization_highs_options(include_mip_gap = FALSE)
  )

  list(result = result, x = result$primal_solution)
}


demand_solve_qp_window <- function(solver_data, bounds) {
  q <- solver_data$q
  ub <- bounds$ub
  n <- solver_data$time_slots

  # When export price exceeds import price in some slots the QP objective is
  # unbounded (both I and E can grow without limit). Clip export prices to
  # import prices to restore convexity. Physical import/export are re-derived
  # from O by callers, so the optimal O profile is unaffected.
  export_q_clipped <- pmax(
    q[solver_data$export_idx],
    -q[solver_data$import_idx]
  )
  if (any(export_q_clipped != q[solver_data$export_idx])) {
    message_once(
      "\u26a0\ufe0f Optimization: export price exceeds import price in some slots; clipping for bounded QP."
    )
    q[solver_data$export_idx] <- export_q_clipped
  }

  # Cap I and E upper bounds at their physical limits (max possible import/export
  # given O bounds and site balance). Without this cap, infinite capacity data
  # yields an unbounded QP whenever clipped PE == PI.
  if (!is.null(bounds$import_mode_ub)) {
    ub[n + seq_len(n)] <- pmin(ub[n + seq_len(n)], bounds$import_mode_ub)
  }
  if (!is.null(bounds$export_mode_ub)) {
    ub[2 * n + seq_len(n)] <- pmin(
      ub[2 * n + seq_len(n)],
      bounds$export_mode_ub
    )
  }

  solve_osqp(
    P = solver_data$P,
    q = q,
    A = solver_data$A,
    lb = bounds$lb,
    ub = ub
  )
}


demand_select_window_solver <- function(solver_data) {
  if (!solver_data$has_grid_flows) {
    return(function(solver_data, bounds) {
      solve_osqp(
        P = solver_data$P,
        q = solver_data$q,
        A = solver_data$A,
        lb = bounds$lb,
        ub = bounds$ub
      )
    })
  }

  if (is.null(solver_data$P)) {
    # lambda = 0: use MILP to enforce import/export exclusivity exactly
    return(demand_solve_milp_window)
  }

  # lambda > 0: use OSQP (continuous QP, no binary variables). The quadratic
  # smoothing term ensures a well-posed bounded problem after PE clipping;
  # OSQP with polishing converges reliably where HiGHS QP ASM is too slow.
  demand_solve_qp_window
}


# Capacity objective helpers ------------------------------------------------------

# The capacity slice: the part of the flexible load that has to move for the
# caps. The load is written as `O = LF - slice + added`, and one problem over
# [slice, added, import_excess, export_excess, shift, discharge] is solved
# lexicographically (see select_capacity_slice()):
#   1. the breach left above the caps, lexicographically from the largest
#      excess (see capacity_minmax_excess()),
#   2. the slice: the energy moved (or dropped) to get there,
#   3. (V2G) the energy discharged, `sum(max(-O, 0))`,
#   4. how far in time it moves, `sum(|cumsum(added - slice)|)`.
# 3 and 4 only break ties between slices of the same energy. The caps are soft
# only up to the original profile's own breach in each slot, so a breach that
# cannot be removed is still brought down as far as it can, and a slot within
# its caps stays within them. capacity_window() then places the slice again
# with the grid objective; `added` only proves that a placement exists.
#
# `LFmin` is the per-slot floor of the optimized load: 0 for demand, and below
# 0 (discharge) for V2G, where capacity_v2g_window() passes the connected power
# and `v2g_discharge_floor()`, so a discharge only ever cuts import.
#
# `headroom` (V2G with `soc_max`, see get_flex_envelope()) lets the cumulative
# optimized load run above cumsum(LF) by that much: the fleet charges ahead.
capacity_slice_problem <- function(
  G,
  LF,
  LS,
  direction,
  time_horizon,
  LFmax,
  import_capacity,
  export_capacity,
  energy_lb = NULL,
  cumsum_min = NULL,
  LFmin = 0,
  headroom = NULL
) {
  time_slots <- length(LF)
  LFmin <- rep_len(as.numeric(LFmin), time_slots)
  identityMat <- diag(time_slots)
  zeroMat <- matrix(0, nrow = time_slots, ncol = time_slots)
  cumsumMat <- triangulate_matrix(matrix(1, time_slots, time_slots), "l")
  # Columns: slice, added, import excess, export excess, shift, discharge.
  block <- function(slice = zeroMat, added = zeroMat, import = zeroMat,
                    export = zeroMat, shift = zeroMat, discharge = zeroMat) {
    cbind(slice, added, import, export, shift, discharge)
  }

  if (direction == "forward") {
    if (time_horizon == time_slots) {
      horizonMat_cumsum <- matrix(0, nrow = time_slots, ncol = time_slots)
    } else {
      horizonMat_cumsum <- triangulate_matrix(
        matrix(1, time_slots, time_slots),
        "l",
        -time_horizon
      )
    }
    horizonMat_identity <- triangulate_matrix(
      triangulate_matrix(matrix(1, time_slots, time_slots), "l"),
      "u",
      -time_horizon
    )

    A_cumsum_lb <- block(-horizonMat_cumsum, cumsumMat)
    lhs_cumsum_lb <- rep(0, time_slots)
    rhs_cumsum_lb <- rep(Inf, time_slots)

    A_cumsum_ub <- block(-cumsumMat, cumsumMat)
    lhs_cumsum_ub <- rep(-Inf, time_slots)
    rhs_cumsum_ub <- rep(0, time_slots)
    # Charging ahead (V2G `soc_max`): cumsum(LF - slice + added) may run above
    # cumsum(LF) by the headroom of the connected sessions.
    if (!is.null(headroom)) {
      rhs_cumsum_ub <- round(pmax(rep_len(as.numeric(headroom), time_slots), 0), 2)
    }
    # Departure deadlines: cumsum(LF - slice + added) >= cumsum_min, capped at
    # cumsum(LF) as in get_bounds().
    if (!is.null(cumsum_min)) {
      LF_cumsum <- cumsum(LF)
      lhs_cumsum_ub <- round(
        pmin(rep_len(as.numeric(cumsum_min), time_slots), LF_cumsum) -
          LF_cumsum,
        2
      )
    }
  } else {
    horizonMat_cumsum <- triangulate_matrix(
      matrix(1, time_slots, time_slots),
      "l",
      time_horizon
    )
    horizonMat_identity <- triangulate_matrix(
      triangulate_matrix(matrix(1, time_slots, time_slots), "u"),
      "l",
      time_horizon
    )

    A_cumsum_lb <- block(-cumsumMat, cumsumMat)
    lhs_cumsum_lb <- rep(0, time_slots)
    rhs_cumsum_lb <- rep(Inf, time_slots)

    A_cumsum_ub <- block(-horizonMat_cumsum, cumsumMat)
    lhs_cumsum_ub <- rep(-Inf, time_slots)
    rhs_cumsum_ub <- rep(0, time_slots)
  }

  # The breach of the original profile in each slot: the most the optimized
  # load may keep there, and nothing where the caps hold.
  import_limit <- round(G - LS + import_capacity - LF, 2)
  export_limit <- round(G - LS - export_capacity - LF, 2)
  import_excess <- pmax(-import_limit, 0)
  export_excess <- pmax(export_limit, 0)

  # Energy row: -sum(slice) + sum(added) = sum(final) - sum(LF). By default an
  # equality, so the slice is re-added in full. When `energy_lb` admits less
  # energy the row becomes a range and the moved-energy objective charges 2 per
  # unit removed and refunds 1 per unit re-added: a shifted unit still costs 1,
  # a dropped unit costs 2 and doing nothing costs 0 — so the LP shifts before
  # it drops, and drops only what the caps leave no room for.
  n <- time_slots
  A_energy <- matrix(c(rep(-1, n), rep(1, n), rep(0, 4 * n)), nrow = 1)
  LF_energy <- sum(LF)
  if (
    is.null(energy_lb) ||
      energy_lb >= LF_energy - optimization_solution_tolerance()
  ) {
    L_moved <- c(rep(1, n), rep(0, 5 * n))
    lhs_energy <- 0
  } else {
    L_moved <- c(rep(2, n), rep(-1, n), rep(0, 4 * n))
    lhs_energy <- round(energy_lb - LF_energy, 2)
  }

  list(
    time_slots = time_slots,
    # The columns and rows of the load and its excess (see
    # select_capacity_slice()); the shift and discharge columns and their
    # rows come after them.
    core_cols = seq_len(4 * n),
    core_rows = seq_len(6 * n + 1),
    L_moved = L_moved,
    L_shift = c(rep(0, 4 * n), rep(1, n), rep(0, n)),
    L_discharge = c(rep(0, 5 * n), rep(1, n)),
    has_discharge = any(LFmin < 0),
    import_excess = import_excess,
    export_excess = export_excess,
    lower = rep(0, 6 * n),
    upper = c(
      round(LF - pmin(LFmin, LF), 2),
      rep(Inf, n),
      import_excess,
      export_excess,
      rep(Inf, n),
      rep(Inf, n)
    ),
    A = rbind(
      # Optimized load within [LFmin, LFmax]
      block(-identityMat, identityMat),
      # Import: O - import_excess <= G - LS + import_capacity
      block(-identityMat, identityMat, import = -identityMat),
      # Export: O + export_excess >= G - LS - export_capacity
      block(-identityMat, identityMat, export = identityMat),
      A_cumsum_lb,
      A_cumsum_ub,
      block(-horizonMat_identity, identityMat),
      A_energy,
      # shift >= |cumsum(added - slice)|
      block(cumsumMat, -cumsumMat, shift = identityMat),
      block(-cumsumMat, cumsumMat, shift = identityMat),
      # discharge >= -O
      block(-identityMat, identityMat, discharge = identityMat)
    ),
    lhs = c(
      round(pmin(LFmin, LF) - LF, 2),
      rep(-Inf, n),
      export_limit,
      lhs_cumsum_lb,
      lhs_cumsum_ub,
      rep(-Inf, n),
      lhs_energy,
      rep(0, 2 * n),
      -LF
    ),
    rhs = c(
      round(pmax(LFmax, LF) - LF, 2),
      import_limit,
      rep(Inf, n),
      rhs_cumsum_lb,
      rhs_cumsum_ub,
      rep(0, n),
      0,
      rep(Inf, 3 * n)
    )
  )
}


# Solves the problem of capacity_slice_problem() lexicographically, with LPs:
# the breach first, then the energy moved, the energy discharged (V2G) and how
# far it moves. Each stage keeps the optimum of the previous one as a
# constraint. Returns the optimized load with its energy and the breach left
# (`breach`), or NULL when HiGHS fails.
select_capacity_slice <- function(
  G,
  LF,
  LS,
  direction,
  time_horizon,
  LFmax,
  import_capacity,
  export_capacity,
  energy_lb = NULL,
  cumsum_min = NULL,
  LFmin = 0,
  headroom = NULL
) {
  G <- round(as.numeric(G), 2)
  LF <- round(as.numeric(LF), 2)
  LS <- round(as.numeric(LS), 2)

  time_slots <- length(LF)
  LFmax <- as.numeric(rep_len(LFmax, time_slots))
  import_capacity <- as.numeric(rep_len(import_capacity, time_slots))
  export_capacity <- as.numeric(rep_len(export_capacity, time_slots))

  problem <- capacity_slice_problem(
    G = G,
    LF = LF,
    LS = LS,
    direction = direction,
    time_horizon = time_horizon,
    LFmax = LFmax,
    import_capacity = import_capacity,
    export_capacity = export_capacity,
    energy_lb = energy_lb,
    cumsum_min = cumsum_min,
    LFmin = LFmin,
    headroom = headroom
  )

  solve <- function(L, lower, upper, A, lhs, rhs) {
    result <- highs::highs_solve(
      L = L,
      lower = lower,
      upper = upper,
      A = A,
      lhs = lhs,
      rhs = rhs,
      control = optimization_highs_options(include_mip_gap = FALSE)
    )
    if (!demand_highs_is_optimal(result) || is.null(result$primal_solution)) {
      return(NULL)
    }
    result
  }
  excess_idx <- 2 * time_slots + seq_len(2 * time_slots)

  # Stages 1 and 2 solve on the core of the problem: the slice, the added
  # energy and the excess, without the shift and discharge columns. Those only
  # measure the tie-breaks of stages 3 and 4; before then they are free (no
  # cost, no upper bound, only held from below by their rows), so the optimum
  # set of stage 1 would be unbounded. On the core every column is bounded
  # (`added` through the load's upper bound).
  core <- problem$core_cols
  core_rows <- problem$core_rows
  A_core <- problem$A[core_rows, core, drop = FALSE]
  lhs_core <- problem$lhs[core_rows]
  rhs_core <- problem$rhs[core_rows]

  # 1. The breach: the largest excess first (see capacity_minmax_excess()).
  excess <- capacity_minmax_excess(
    solve = solve,
    lower = problem$lower[core],
    upper = problem$upper[core],
    A = A_core,
    lhs = lhs_core,
    rhs = rhs_core,
    excess_idx = excess_idx
  )
  if (is.null(excess)) {
    return(NULL)
  }

  # 2. The energy moved to get there, keeping each slot's excess at the
  # optimum of 1 (up to the solver tolerance).
  tolerance <- optimization_solution_tolerance()
  upper <- problem$upper
  upper[excess_idx] <- pmin(upper[excess_idx], excess + tolerance)
  result <- solve(
    problem$L_moved[core], problem$lower[core], upper[core], A_core,
    lhs_core, rhs_core
  )
  if (is.null(result)) {
    return(NULL)
  }

  # 3. and 4. Each keeping the optimum of the stage before: the energy
  # discharged (V2G: among equal moves, the one that cycles the batteries
  # least), then how far in time the energy moves.
  A <- problem$A
  lhs <- problem$lhs
  rhs <- problem$rhs
  previous_L <- problem$L_moved
  stages <- if (problem$has_discharge) {
    list(problem$L_discharge, problem$L_shift)
  } else {
    list(problem$L_shift)
  }
  for (L in stages) {
    optimum <- result$objective_value
    A <- rbind(A, matrix(previous_L, nrow = 1))
    lhs <- c(lhs, -Inf)
    rhs <- c(rhs, optimum + tolerance * max(1, abs(optimum)))
    result <- solve(L, problem$lower, upper, A, lhs, rhs)
    if (is.null(result)) {
      return(NULL)
    }
    previous_L <- L
  }

  x <- result$primal_solution
  # Opposite moves in the same slot cancel out in the load.
  delta <- x[time_slots + seq_len(time_slots)] - x[seq_len(time_slots)]
  delta[abs(delta) < tolerance] <- 0
  slice <- round(pmax(-delta, 0), 2)
  added <- round(pmax(delta, 0), 2)
  import_excess <- pmax(x[2 * time_slots + seq_len(time_slots)], 0)
  export_excess <- pmax(x[3 * time_slots + seq_len(time_slots)], 0)
  breach <- sum(import_excess) + sum(export_excess)

  list(
    slice = slice,
    added = added,
    energy = sum(LF) + sum(delta),
    import_excess = import_excess,
    export_excess = export_excess,
    breach = if (breach < 0.01) 0 else breach,
    result = result
  )
}


# Stage 1 of select_capacity_slice(): the excess over the caps, brought down
# lexicographically from the largest, as a sequence of LPs. Each LP minimises
# `z`, the largest excess among the slots still free (`excess <= z`). A slot
# whose row `excess <= z` has a nonzero dual is at `z` in every optimum
# (complementary slackness), so it is fixed there and the next LP brings the
# rest further down. Every LP fixes at least one slot (the duals of those rows
# sum to 1), and the sequence ends when the largest free excess is 0. Returns
# the excess of each slot (the `excess_idx` columns), or NULL when HiGHS
# fails.
#
# The breach used to be a QP on the squared excess. Its optimum is a
# degenerate vertex (many slots exactly on their cap with no excess), on which
# HiGHS's active-set QP solver can cycle without end; the simplex of the LPs
# does not.
capacity_minmax_excess <- function(
  solve,
  lower,
  upper,
  A,
  lhs,
  rhs,
  excess_idx
) {
  n_cols <- length(lower)
  # The most excess each slot may keep: the original profile's own breach
  bound <- upper[excess_idx]
  level <- bound
  free <- which(level > 0)
  # The margin above a fixed level, so that the next LPs and stage 2 stay
  # feasible to the precision of the simplex; well below the 0.01 grid of the
  # data.
  margin <- 1e-4

  while (length(free) > 0) {
    upper_lp <- upper
    upper_lp[excess_idx] <- level
    # excess_j - z <= 0 for every free slot j
    A_level <- matrix(0, nrow = length(free), ncol = n_cols + 1)
    A_level[cbind(seq_along(free), excess_idx[free])] <- 1
    A_level[, n_cols + 1] <- -1
    result <- solve(
      c(rep(0, n_cols), 1),
      c(lower, 0),
      c(upper_lp, Inf),
      rbind(cbind(A, 0), A_level),
      c(lhs, rep(-Inf, length(free))),
      c(rhs, rep(0, length(free)))
    )
    if (is.null(result)) {
      return(NULL)
    }

    z <- max(result$objective_value, 0)
    if (z <= optimization_solution_tolerance()) {
      level[free] <- 0
      break
    }
    dual <- result$solver_msg$row_dual[nrow(A) + seq_along(free)]
    fixed <- free[abs(dual) > 1e-9]
    if (length(fixed) == 0) {
      excess <- result$primal_solution[excess_idx[free]]
      fixed <- free[excess >= z - margin]
    }
    level[fixed] <- pmin(z + margin, bound[fixed])
    free <- setdiff(free, fixed)
  }

  level
}


# The capacity objective for one window, shared by demand_capacity_window()
# and capacity_v2g_window(). A window with no breach is returned as it is.
# Otherwise select_capacity_slice() picks the slice: the least energy whose
# move removes the breach - or, where the flexibility cannot remove it, brings
# the excess down as far as it can. Only that slice is placed again, by
# `place_slice` (the grid objective), on top of the untouched rest of the
# load. The placement keeps the caps, raised only in the slots, and only as
# far, as the selected slice leaves an excess, so it is feasible by
# construction (the slice LP's own placement is a feasible point).
capacity_window <- function(
  G,
  LF,
  LS,
  direction,
  time_horizon,
  LFmax,
  import_capacity,
  export_capacity,
  place_slice,
  energy_ratio = c(1, 1),
  cumsum_min = NULL,
  LFmin = 0,
  headroom = NULL
) {
  G <- round(as.numeric(G), 2)
  LF <- round(as.numeric(LF), 2)
  LS <- round(as.numeric(LS), 2)

  time_slots <- length(LF)
  if (is.null(time_horizon)) {
    time_horizon <- time_slots
  }
  LFmax <- as.numeric(rep_len(LFmax, time_slots))
  import_capacity <- as.numeric(rep_len(import_capacity, time_slots))
  export_capacity <- as.numeric(rep_len(export_capacity, time_slots))
  energy_ratio <- demand_check_energy_ratio(
    energy_ratio,
    direction,
    time_horizon,
    time_slots
  )
  tol <- optimization_solution_tolerance()

  # The capacity objective leaves the profile alone wherever the caps hold, so
  # an energy target below the requirement is applied as a uniform scaling of
  # the whole profile before the slice is chosen: same shape, less energy. The
  # floor stays relative to the ORIGINAL profile, since that is what the caller
  # promised to deliver at least.
  LF_target <- if (energy_ratio[2] < 1) round(LF * energy_ratio[2], 2) else LF
  energy_lb <- if (energy_ratio[1] < 1) {
    round(energy_ratio[1] * sum(LF), 2)
  } else {
    NULL
  }

  # No breach: nothing to move.
  net <- LS + LF_target - G
  if (all(net <= import_capacity + 0.005 & -net <= export_capacity + 0.005)) {
    return(LF_target)
  }

  slice_solution <- select_capacity_slice(
    G = G,
    LF = LF_target,
    LS = LS,
    direction = direction,
    time_horizon = time_horizon,
    LFmax = LFmax,
    import_capacity = import_capacity,
    export_capacity = export_capacity,
    energy_lb = energy_lb,
    cumsum_min = cumsum_min,
    LFmin = LFmin,
    headroom = headroom
  )

  if (is.null(slice_solution)) {
    # Unreachable for well-formed inputs: the original profile is a feasible
    # point of the slice LP (no move, the breach it already has).
    message_once(
      "\u26a0\ufe0f Optimization warning: capacity optimization failed in some windows. No optimization provided."
    )
    return(LF_target)
  }

  if (slice_solution$breach > 0) {
    if (!is.null(energy_lb) && slice_solution$energy <= energy_lb + 0.01) {
      message_once(
        "\u26a0\ufe0f Optimization warning: even the minimum energy does not fit under the grid capacity in some windows. Reducing the excess as far as the minimum energy allows, never beyond the original profile."
      )
    } else {
      message_once(
        "\u26a0\ufe0f Optimization warning: optimization not feasible in some windows. Reducing the excess over the grid capacity as far as the flexibility allows, never beyond the original profile."
      )
    }
  }

  moved_slice <- slice_solution$slice
  if (all(moved_slice == 0)) {
    return(LF_target)
  }

  # The slice LP decided how much of the removed energy is re-added (all of it
  # unless the caps left no room). The grid LP that places it must carry
  # exactly that energy, expressed as a ratio of the rounded slice so both
  # sides round to the same 2-decimal figure.
  moved_energy <- sum(moved_slice)
  added_energy <- sum(slice_solution$added)
  fixed_load <- round(LF_target - moved_slice, 2)
  if (added_energy <= tol) {
    # The caps left no room for any of the removed energy: with a minimum
    # below 100% the LP dropped all of it, and there is nothing left to place.
    return(fixed_load)
  }
  slice_ratio <- if (added_energy < moved_energy - tol) {
    rep(added_energy / moved_energy, 2)
  } else {
    c(1, 1)
  }

  # The excess the selected slice leaves, rounded up to the 2-decimal grid of
  # the bounds: the placement may keep it, and no more.
  excess_margin <- function(excess) {
    ifelse(excess > tol, ceiling(round(excess * 100, 6)) / 100 + 0.01, 0)
  }

  optimized_slice <- place_slice(
    LF = moved_slice,
    time_horizon = time_horizon,
    LS = LS + fixed_load,
    LFmax = round(pmax(LFmax - fixed_load, 0), 2),
    import_capacity = import_capacity +
      excess_margin(slice_solution$import_excess),
    export_capacity = export_capacity +
      excess_margin(slice_solution$export_excess),
    energy_ratio = slice_ratio,
    # The fixed part already covers some of the departure deadlines; the slice
    # has to cover the rest (the slice LP above found such a placement).
    cumsum_min = if (is.null(cumsum_min)) {
      NULL
    } else {
      rep_len(as.numeric(cumsum_min), time_slots) - cumsum(fixed_load)
    },
    # cumsum(fixed_load + placed) <= cumsum(LF) + headroom is the same
    # headroom above cumsum(moved_slice).
    headroom = headroom
  )

  round(fixed_load + as.numeric(optimized_slice), 2)
}


#' Capacity optimisation of a flexible demand (single window)
#'
#' A window within `import_capacity` and `export_capacity` is returned
#' unchanged. Otherwise the slice of the flexible load that has to move is
#' chosen with `select_capacity_slice()`: the least energy whose move removes
#' the breach, or, where the flexibility cannot remove it, brings the excess
#' over the caps down as far as it can (largest excess first). Only that slice
#' is placed again with the grid objective (`demand_grid_window()`), on top of
#' the rest of the load, which stays untouched. No slot ends up beyond its cap
#' by more than the selected slice leaves, nor beyond the original profile's
#' excess.
#'
#' @param lambda numeric, penalty on change of the placed slice (see
#'   `demand_grid_window()`).
#' @param energy_ratio numeric vector `c(min, max)`, share of the flexible
#'   load's energy the optimized load must carry. The maximum scales the whole
#'   profile; energy is dropped down to the minimum only where the caps leave
#'   no room for it.
#'
#' @keywords internal
#'
demand_capacity_window <- function(
  G,
  LF,
  LS,
  direction,
  time_horizon,
  LFmax,
  import_capacity,
  export_capacity,
  lambda = 0,
  energy_ratio = c(1, 1),
  cumsum_min = NULL
) {
  capacity_window(
    G = G,
    LF = LF,
    LS = LS,
    direction = direction,
    time_horizon = time_horizon,
    LFmax = LFmax,
    import_capacity = import_capacity,
    export_capacity = export_capacity,
    place_slice = function(LF, time_horizon, LS, LFmax, import_capacity,
                           export_capacity, energy_ratio, cumsum_min,
                           headroom = NULL) {
      demand_grid_window(
        G = G,
        LF = LF,
        LS = LS,
        direction = direction,
        time_horizon = time_horizon,
        LFmax = LFmax,
        import_capacity = import_capacity,
        export_capacity = export_capacity,
        lambda = lambda,
        energy_ratio = energy_ratio,
        cumsum_min = cumsum_min
      )
    },
    energy_ratio = energy_ratio,
    cumsum_min = cumsum_min
  )
}


# Optimization of load ------------------------------------------------------------

#' Optimize a vector of flexible demand
#'
#' See the formulation of the optimization problems in the
#' [documentation website](https://resourcefully-dev.github.io/flextools/).
#'
#' @param opt_data tibble, optimization contextual data.
#' The first column must be named `datetime` (mandatory) containing the
#' date time sequence where the optimization algorithm is applied.
#'
#' The second column must be named `flexible` (mandatory), being the
#' power demand (in kW) vector that will be optimized.
#'
#' The other columns can be (optional):
#'
#' - `static`: static power demand (in kW) from other sectors like buildings,
#' offices, etc.
#'
#' - `import_capacity`: maximum imported power from the grid (in kW),
#' for example the contracted power with the energy company.
#'
#' - `export_capacity`: maximum exported power from the grid (in kW),
#' for example the contracted power with the energy company.
#'
#' - `load_capacity`: maximum power that the `flexible` load
#' can consume (in kW).
#'
#' - `production`: local power generation (in kW).
#' This is used when `opt_objective = "grid"`.
#'
#' - `price_imported`: price for imported energy (Euro/kWh).
#' This is used when `opt_objective = "cost"`.
#'
#' - `price_exported`: price for exported energy (Euro/kWh).
#' This is used when `opt_objective = "cost"`.
#'
#' - `price_turn_down`: price for turn-down energy use (Euro/kWh).
#' This is used when `opt_objective = "cost"`.
#'
#' - `price_turn_up`: price for turn-up energy use (Euro/kWh).
#' This is used when `opt_objective = "cost"`.
#'
#' @param opt_objective character or numeric.
#' Optimization objective can be `"grid"` (default), `"cost"` or `"capacity"`, or
#' a number between `0` and `1` to perform combined optimization
#' where `0 == "cost"` and `1 == "grid"`.
#' The `"capacity"` objective minimizes the amount of flexible demand that
#' needs to be moved to respect `import_capacity` and `export_capacity`,
#' then applies the grid-minimizing formulation only to that moved slice. A
#' window within its capacities is left unchanged. When a breach cannot be
#' removed, the slice brings the excess down as far as the flexibility allows,
#' never beyond the original profile's.
#' @param direction character, being `forward` or `backward`. The direction where energy can be shifted
#' @param time_horizon integer, maximum number of time slots to shift energy from.
#'  If `NULL`, the `time_horizon` will be the total optimization window length.
#' @param window_days integer, number of days to consider as optimization window.
#' @param window_start_hour integer, starting hour of the optimization window.
#' @param flex_window_hours integer, flexibility window length, in hours.
#' This optional feature lets you apply flexibility only during few hours from the `window_start_hour`.
#' It must be lower than `window_days*24` hours.
#' @param lambda numeric, penalty on change for the flexible load.
#'
#' @return numeric vector
#' @export
#'
#' @importFrom dplyr tibble %>% left_join arrange
#' @importFrom purrr map
#' @importFrom rlang .data
#'
optimize_demand <- function(
  opt_data,
  opt_objective = "grid",
  direction = "forward",
  time_horizon = NULL,
  window_days = 1,
  window_start_hour = 0,
  flex_window_hours = NULL,
  lambda = 0
) {
  # Validate and complete the contextual input data.
  opt_data <- check_optimization_data(opt_data, opt_objective)
  if (is.null(opt_data)) {
    stop("Error: `opt_data` parameter is empty.")
  }

  # The directional shifting contract is unchanged.
  if (((direction != "forward") && (direction != "backward"))) {
    stop("Error: `direction` must be 'forward' or 'backward'")
  }

  # Split the input time series into optimization windows.
  dttm_seq <- opt_data$datetime
  flex_windows_idxs <- get_flex_windows(
    dttm_seq = dttm_seq,
    window_days = window_days,
    window_start_hour = window_start_hour,
    flex_window_hours = flex_window_hours
  )
  flex_windows_idxs_seq <- as.numeric(unlist(flex_windows_idxs$flex_idx))

  windows_data <- map(
    flex_windows_idxs$flex_idx,
    ~ opt_data[.x, ]
  )

  # Solve each window independently with the same objective-specific formulas
  # used in the original implementation.
  reset_message_once()

  if (is.numeric(opt_objective)) {
    # The combined demand formulation is only needed strictly inside (0, 1).
    # Endpoint weights are aliases of the pure formulations and are routed
    # directly to those smaller, more stable models.
    if (opt_objective <= 0) {
      opt_objective <- "cost"
    } else if (opt_objective >= 1) {
      opt_objective <- "grid"
    }
  }

  if (opt_objective == "grid") {
    O_windows <- map(
      windows_data,
      ~ demand_grid_window(
        G = .x$production,
        LF = .x$flexible,
        LS = .x$static,
        direction = direction,
        time_horizon = time_horizon,
        LFmax = .x$load_capacity,
        import_capacity = .x$import_capacity,
        export_capacity = .x$export_capacity,
        lambda = lambda
      )
    )
  } else if (opt_objective == "capacity") {
    O_windows <- map(
      windows_data,
      ~ demand_capacity_window(
        G = .x$production,
        LF = .x$flexible,
        LS = .x$static,
        direction = direction,
        time_horizon = time_horizon,
        LFmax = .x$load_capacity,
        import_capacity = .x$import_capacity,
        export_capacity = .x$export_capacity,
        lambda = lambda
      )
    )
  } else if (opt_objective == "cost") {
    O_windows <- map(
      windows_data,
      ~ demand_cost_window(
        G = .x$production,
        LF = .x$flexible,
        LS = .x$static,
        PI = .x$price_imported,
        PE = .x$price_exported,
        PTU = .x$price_turn_up,
        PTD = .x$price_turn_down,
        direction = direction,
        time_horizon = time_horizon,
        LFmax = .x$load_capacity,
        import_capacity = .x$import_capacity,
        export_capacity = .x$export_capacity,
        lambda = lambda
      )
    )
  } else if (is.numeric(opt_objective)) {
    O_windows <- map(
      windows_data,
      ~ demand_combined_window(
        G = .x$production,
        LF = .x$flexible,
        LS = .x$static,
        PI = .x$price_imported,
        PE = .x$price_exported,
        PTU = .x$price_turn_up,
        PTD = .x$price_turn_down,
        direction = direction,
        time_horizon = time_horizon,
        LFmax = .x$load_capacity,
        import_capacity = .x$import_capacity,
        export_capacity = .x$export_capacity,
        lambda = lambda,
        w = opt_objective
      )
    )
  } else {
    stop("Error: invalid `opt_objective`")
  }

  O <- as.numeric(unlist(O_windows))

  if (length(flex_windows_idxs_seq) == length(dttm_seq)) {
    return(O)
  }

  # Reinsert the original flexible load outside the flexibility windows.
  O_flex <- left_join(
    tibble(idx = seq_len(length(dttm_seq))),
    tibble(
      idx = flex_windows_idxs_seq,
      O = O
    ),
    by = "idx"
  ) %>%
    arrange(.data$idx)

  O_flex$O[is.na(O_flex$O)] <- opt_data$flexible[is.na(O_flex$O)]
  O_flex$O
}


#' Perform demand optimization (just a window)
#'
#' @param G numeric vector, being the renewable generation power profile
#' @param LF numeric vector, being the flexible load power profile
#' @param LS numeric vector, being the static load power profile
#' @param direction character, being `forward` or `backward`. The direction where energy can be shifted
#' @param time_horizon integer, maximum number of positions to shift energy from
#' @param LFmax numeric, value of maximum power (in kW) of the flexible load `LF`
#' @param import_capacity numeric or numeric vector, grid maximum import capacity that will limit the maximum optimized demand
#' @param export_capacity numeric or numeric vector, grid maximum export capacity that will limit the maximum optimized demand
#' @param P numeric matrix, optimization objective parameter
#' @param q numeric vector, optimization objective parameter
#' @param energy_ratio numeric vector `c(min, max)`, share of the flexible
#'   load's energy the optimized load must carry. `c(1, 1)` (default) preserves
#'   the energy exactly; a range lets the optimizer drop energy — down to `min`
#'   — when the grid capacity cannot fit `max`.
#' @param cumsum_min numeric vector or `NULL`, minimum cumulative optimized
#'   load at each slot (in the units of `LF`): the energy owed to the sessions
#'   that have departed by then (see `get_flex_envelope()`). Only applies to
#'   `direction = "forward"`, and is capped at `cumsum(LF)` so the original
#'   profile stays feasible.
#'
#' @return numeric vector
#' @keywords internal
#'
demand_solve_window <- function(
  G,
  LF,
  LS,
  direction,
  time_horizon,
  LFmax,
  import_capacity,
  export_capacity,
  P,
  q,
  energy_ratio = c(1, 1),
  cumsum_min = NULL
) {
  # The original implementation rounded the inputs to avoid tiny numerical
  # inconsistencies between bounds. That behavior is kept unchanged.
  G <- round(G, 2)
  LF <- round(LF, 2)
  LS <- round(LS, 2)

  time_slots <- length(G)
  if (is.null(time_horizon)) {
    time_horizon <- time_slots
  }
  identityMat <- diag(time_slots)
  has_grid_flows <- nrow(P) > time_slots
  P_normalized <- optimization_normalize_quadratic(
    P,
    problem_name = "demand optimization"
  )

  # Build the same physical bounds as before. The continuous `grid` objective
  # still solves directly, while cost/combined add a binary grid mode to avoid
  # simultaneous import and export in the same timestep.
  base_bounds <- get_bounds(
    time_slots,
    G,
    LF,
    LS,
    direction,
    time_horizon,
    LFmax,
    import_capacity,
    export_capacity,
    cumsum_min
  )

  # Energy the optimized load must carry: by default exactly the profile's own
  # (`energy_ratio = c(1, 1)`), otherwise the range `[min, max] * sum(LF)`.
  energy_ratio <- demand_check_energy_ratio(
    energy_ratio,
    direction,
    time_horizon,
    time_slots
  )
  LF_energy <- sum(LF)
  energy_lb <- round(energy_ratio[1] * LF_energy, 2)
  energy_ub <- round(energy_ratio[2] * LF_energy, 2)
  if (energy_lb < energy_ub) {
    # Every objective here improves when load is removed, so a range would be
    # read as "deliver the minimum". Reward each unit of optimized load kept by
    # more than the objective can gain from dropping it: the solver then keeps
    # as much energy as the caps admit and shapes it afterwards. The reward is
    # the same in every slot, so it decides how much energy stays, not where.
    reward <- demand_energy_reward(
      P_normalized,
      q,
      envelope = max(base_bounds$ub_O[is.finite(base_bounds$ub_O)], 0) +
        max(abs(LS - G))
    )
    q[seq_len(time_slots)] <- q[seq_len(time_slots)] - reward
  }

  if (has_grid_flows) {
    # Unknown variable: X = [O, I, E]
    # `O` is optimized flexible load, `I` imported energy and `E` exported
    # energy. The block structure is left untouched to keep the original model
    # easy to compare with the previous OSQP version.
    Amat_O <- cbind(identityMat, identityMat * 0, identityMat * 0)

    Amat_I <- cbind(
      identityMat * 0,
      identityMat * 1,
      identityMat * 0
    )

    Amat_E <- cbind(
      identityMat * 0,
      identityMat * 0,
      identityMat * 1
    )

    # Grid balance ties optimized demand and grid flows to the net site power.
    Amat_balance <- cbind(
      identityMat * 1,
      identityMat * -1,
      identityMat * 1
    )
    lb_balance <- G - LS
    ub_balance <- G - LS

    # Cumulative-shift constraints implement the forward/backward time horizon.
    Amat_cumsum <- cbind(
      base_bounds$Amat_cumsum,
      identityMat * 0,
      identityMat * 0
    )
    lb_cumsum <- base_bounds$lb_cumsum
    ub_cumsum <- base_bounds$ub_cumsum

    # Total flexible energy: preserved by default, a range otherwise.
    Amat_energy <- cbind(
      matrix(1, ncol = time_slots),
      matrix(0, ncol = time_slots),
      matrix(0, ncol = time_slots)
    )
    lb_energy <- energy_lb
    ub_energy <- energy_ub

    Amat <- rbind(
      Amat_O,
      Amat_I,
      Amat_E,
      Amat_balance,
      Amat_cumsum,
      Amat_energy
    )

    bounds_with_capacities <- function(
      import_cap,
      export_cap,
      clamp_to_lf = FALSE,
      energy_floor = lb_energy,
      energy_ceiling = ub_energy
    ) {
      import_cap <- as.numeric(rep_len(import_cap, time_slots))
      export_cap <- as.numeric(rep_len(export_cap, time_slots))
      L_bounds <- get_bounds(
        time_slots,
        G,
        LF,
        LS,
        direction,
        time_horizon,
        LFmax,
        import_cap,
        export_cap,
        cumsum_min
      )

      # Minimal-relaxation retry: guarantee the original profile O = LF is a
      # feasible point. LFmax can sit below LF (e.g. when a caller pre-shrinks
      # the max power for conversion losses), which would pull ub_O under LF and
      # make O = LF infeasible, so widen the optimized-load box to admit it.
      if (clamp_to_lf) {
        L_bounds$ub_O <- pmax(L_bounds$ub_O, LF)
        L_bounds$lb_O <- pmin(L_bounds$lb_O, LF)
      }

      # These are the tightest per-slot grid-flow bounds implied by the
      # optimized-load bounds and the site balance equation.
      import_mode_ub <- pmax(L_bounds$ub_O + LS - G, 0)
      export_mode_ub <- pmax(G - LS - L_bounds$lb_O, 0)
      import_mode_ub[is.finite(import_cap)] <- pmin(
        import_mode_ub[is.finite(import_cap)],
        import_cap[is.finite(import_cap)]
      )
      export_mode_ub[is.finite(export_cap)] <- pmin(
        export_mode_ub[is.finite(export_cap)],
        export_cap[is.finite(export_cap)]
      )

      list(
        lb = round(
          c(
            L_bounds$lb_O,
            rep(0, time_slots),
            rep(0, time_slots),
            lb_balance,
            # Never above the energy row: the deadlines and the energy are
            # rounded separately and must not contradict each other.
            pmin(lb_cumsum, energy_floor),
            energy_floor
          ),
          2
        ),
        ub = round(
          c(
            L_bounds$ub_O,
            import_cap,
            export_cap,
            ub_balance,
            ub_cumsum,
            energy_ceiling
          ),
          2
        ),
        import_mode_ub = import_mode_ub,
        export_mode_ub = export_mode_ub
      )
    }

    solver_data <- list(
      time_slots = time_slots,
      n_variables = 3 * time_slots,
      optimized_idx = seq_len(time_slots),
      import_idx = seq_len(time_slots) + time_slots,
      export_idx = seq_len(time_slots) + 2 * time_slots,
      grid_mode_idx = 3 * time_slots + seq_len(time_slots),
      A = Amat,
      P = P_normalized,
      q = q,
      has_grid_flows = TRUE,
      bounds_with_capacities = bounds_with_capacities
    )
  } else {
    # Unknown variable: X = [O]
    # This smaller model is used when the objective does not require explicit
    # import/export variables.
    Amat_O <- base_bounds$Amat_O
    Amat_cumsum <- base_bounds$Amat_cumsum
    lb_cumsum <- base_bounds$lb_cumsum
    ub_cumsum <- base_bounds$ub_cumsum

    Amat_energy <- matrix(1, ncol = time_slots)
    lb_energy <- energy_lb
    ub_energy <- energy_ub

    Amat <- rbind(Amat_O, Amat_cumsum, Amat_energy)

    bounds_with_capacities <- function(
      import_cap,
      export_cap,
      clamp_to_lf = FALSE,
      energy_floor = lb_energy,
      energy_ceiling = ub_energy
    ) {
      L_bounds <- get_bounds(
        time_slots,
        G,
        LF,
        LS,
        direction,
        time_horizon,
        LFmax,
        import_cap,
        export_cap,
        cumsum_min
      )

      # Minimal-relaxation retry: guarantee O = LF is a feasible point even when
      # LFmax sits below LF (see the grid-flow branch for the full argument).
      if (clamp_to_lf) {
        L_bounds$ub_O <- pmax(L_bounds$ub_O, LF)
        L_bounds$lb_O <- pmin(L_bounds$lb_O, LF)
      }

      list(
        lb = round(
          c(L_bounds$lb_O, pmin(lb_cumsum, energy_floor), energy_floor),
          2
        ),
        ub = round(c(L_bounds$ub_O, ub_cumsum, energy_ceiling), 2)
      )
    }

    solver_data <- list(
      time_slots = time_slots,
      n_variables = time_slots,
      optimized_idx = seq_len(time_slots),
      A = Amat,
      P = P_normalized,
      q = q,
      has_grid_flows = FALSE,
      bounds_with_capacities = bounds_with_capacities
    )
  }

  solve_window_problem <- demand_select_window_solver(solver_data)

  solve_with_capacities <- function(
    import_cap,
    export_cap,
    clamp_to_lf = FALSE,
    energy_floor = energy_lb,
    energy_ceiling = energy_ub
  ) {
    bounds <- solver_data$bounds_with_capacities(
      import_cap,
      export_cap,
      clamp_to_lf,
      energy_floor,
      energy_ceiling
    )
    solve_window_problem(solver_data, bounds)
  }

  # First solve: keep the original grid limits.
  O <- solve_with_capacities(import_capacity, export_capacity)
  if (demand_highs_is_optimal(O$result)) {
    return(demand_extract_solution(O$x, solver_data))
  }

  # Fallback solves: the original grid limits make the problem infeasible.
  # Instead of removing the grid constraints entirely (an unconstrained solve
  # that could shift energy into new, worse violations), raise the per-slot
  # caps only as far as a reference profile already needs. Slots that were
  # within their caps keep the true capacity, so the optimizer can never
  # create a violation worse than that profile had.
  #
  # The retry with the original profile O = LF is feasible BY CONSTRUCTION:
  # with the induced grid flows I = pmax(LS + LF - G, 0), E = pmax(G - LS -
  # LF, 0) it satisfies every constraint:
  #   * energy: pinned to sum(LF);
  #   * cumsum bounds: they are derived from LF itself, so cumsum(LF) lies
  #     inside [lb_cumsum, ub_cumsum] (one side is exactly cumsum(LF));
  #   * grid balance: O - I + E = G - LS by definition;
  #   * capacity: the relaxed caps equal at least the reference net flows, so
  #     I <= import_cap_relaxed and E <= export_cap_relaxed;
  #   * optimized-load box: lb_O <= LF is always true, and clamp_to_lf lifts
  #     ub_O up to LF when LFmax would otherwise pull it below.
  # The minimum-energy retry tried first is the same construction on the scaled
  # profile (a range below 1 is only admitted for forward shifting over the
  # whole window, where lb_cumsum = 0), except that the export-side lower bound
  # lb_O can exceed a scaled profile — which is why it may fail and is followed
  # by the original-profile retry rather than replacing it.
  # A small tolerance absorbs the 2-decimal rounding of the bounds.
  tol <- optimization_solution_tolerance()
  relax_to <- function(LF_ref, energy) {
    solve_with_capacities(
      pmax(import_capacity, LS + LF_ref - G) + tol,
      pmax(export_capacity, G - LS - LF_ref) + tol,
      clamp_to_lf = TRUE,
      energy_floor = energy,
      energy_ceiling = energy
    )
  }

  # Energy first: when the range admits less than the full profile, relax the
  # caps only as far as the minimum-energy profile needs and deliver exactly
  # that minimum. A minimum of 0 is skipped \u2014 a zero profile has no headroom to
  # relax towards, and an infeasibility that survives dropping every unit of
  # energy is not about energy (export side, load capacity).
  LF_min <- if (energy_lb < round(LF_energy, 2) && LF_energy > 0) {
    round(LF * energy_lb / LF_energy, 2)
  } else {
    LF
  }
  if (!identical(LF_min, LF) && sum(LF_min) > 0) {
    message_once(
      "\u26A0\uFE0F Optimization warning: even the minimum energy does not fit under the grid capacity in some windows. Relaxing grid capacity to the minimum-energy profile in the affected windows."
    )
    O <- relax_to(LF_min, sum(LF_min))
    if (demand_highs_is_optimal(O$result)) {
      return(demand_extract_solution(O$x, solver_data))
    }
  }

  # Capacity second: the original, unshifted profile, feasible by construction
  # (the 1.6.0 behaviour). The energy is pinned to the profile's own for the
  # same reason: it is the one point known to satisfy every other constraint.
  message_once(
    "\u26A0\uFE0F Optimization warning: optimization not feasible in some windows. Relaxing grid capacity to the original profile in the affected windows."
  )
  O <- relax_to(LF, round(LF_energy, 2))
  if (demand_highs_is_optimal(O$result)) {
    return(demand_extract_solution(O$x, solver_data))
  }

  # Unreachable for well-formed inputs: the relaxation above is feasible by
  # construction, so reaching here means the solver crashed. Surface it loudly
  # and fall back to the untouched input profile as a last resort.
  message_once(paste0(
    "\u26A0\uFE0F Optimization warning: ",
    O$result$status_message,
    ". No optimization provided."
  ))
  LF
}


#' Minimization of net power (just a window)
#'
#' @param G numeric vector, being the renewable generation profile
#' @param LF numeric vector, being the flexible load profile
#' @param LS numeric vector, being the static load profile
#' @param direction character, being `forward` or `backward`. The direction where energy can be shifted
#' @param time_horizon integer, maximum number of positions to shift energy from
#' @param LFmax numeric, value of maximum power (in kW) of the flexible load `LF`
#' @param import_capacity numeric or numeric vector, grid maximum import capacity that will limit the maximum optimized demand
#' @param export_capacity numeric or numeric vector, grid maximum export capacity that will limit the maximum optimized demand
#' @param lambda numeric, penalty on change for the flexible load.
#' @inheritParams demand_solve_window
#'
#' @return numeric vector
#' @keywords internal
#'
demand_grid_window <- function(
  G,
  LF,
  LS,
  direction,
  time_horizon,
  LFmax,
  import_capacity,
  export_capacity,
  lambda = 0,
  energy_ratio = c(1, 1),
  cumsum_min = NULL
) {
  time_slots <- length(LF)
  identityMat <- diag(time_slots)
  LambdaMat <- get_lambda_matrix(time_slots)

  # min sum((O + LS - G)^2) + lambda * sum((O_t - O_{t-1})^2)
  P <- 2 * (identityMat + lambda * LambdaMat)
  q <- 2 * (LS - G)

  demand_solve_window(
    G,
    LF,
    LS,
    direction,
    time_horizon,
    LFmax,
    import_capacity,
    export_capacity,
    P,
    q,
    energy_ratio,
    cumsum_min
  )
}


#' Minimization of the cost (just a window)
#'
#' @param G numeric vector, being the renewable generation power profile
#' @param LF numeric vector, being the flexible load power profile
#' @param LS numeric vector, being the static load power profile
#' @param PI numeric vector, electricity prices for imported energy
#' @param PE numeric vector, electricity prices for exported energy
#' @param PTD numeric vector, prices for turn-down energy use
#' @param PTU numeric vector, prices for turn-up energy use
#' @param direction character, being `forward` or `backward`. The direction where energy can be shifted
#' @param time_horizon integer, maximum number of positions to shift energy from
#' @param LFmax numeric, value of maximum power (in kW) of the flexible load `LF`
#' @param import_capacity numeric or numeric vector, grid maximum import capacity that will limit the maximum optimized demand
#' @param export_capacity numeric or numeric vector, grid maximum export capacity that will limit the maximum optimized demand
#' @param lambda numeric, penalty on change for the flexible load.
#' @inheritParams demand_solve_window
#'
#' @return numeric vector
#' @keywords internal
#'
demand_cost_window <- function(
  G,
  LF,
  LS,
  PI,
  PE,
  PTD,
  PTU,
  direction,
  time_horizon,
  LFmax,
  import_capacity,
  export_capacity,
  lambda = 0,
  energy_ratio = c(1, 1),
  cumsum_min = NULL
) {
  time_slots <- length(LF)
  identityMat <- diag(time_slots)
  LambdaMat <- get_lambda_matrix(time_slots)

  # Unknown variable: X = [O, I, E]
  # Quadratic term penalizes ramping: lambda * sum((O_t - O_{t-1})^2).
  # A tiny diagonal term (1e-6 * lambda * I) regularises the PSD LambdaMat to
  # PD so that OSQP converges reliably for large lambda values.
  P <- rbind(
    cbind(
      2 * lambda * (LambdaMat + 1e-6 * identityMat),
      identityMat * 0,
      identityMat * 0
    ),
    cbind(
      identityMat * 0,
      identityMat * 0,
      identityMat * 0
    ),
    cbind(
      identityMat * 0,
      identityMat * 0,
      identityMat * 0
    )
  )
  q <- c(
    PTD - PTU,
    PI,
    -PE
  )

  demand_solve_window(
    G,
    LF,
    LS,
    direction,
    time_horizon,
    LFmax,
    import_capacity,
    export_capacity,
    P,
    q,
    energy_ratio,
    cumsum_min
  )
}


#' Combined optimization (just a window)
#'
#' @param G numeric vector, being the renewable generation power profile
#' @param LF numeric vector, being the flexible load power profile
#' @param LS numeric vector, being the static load power profile
#' @param PI numeric vector, electricity prices for imported energy
#' @param PE numeric vector, electricity prices for exported energy
#' @param PTD numeric vector, prices for turn-down energy use
#' @param PTU numeric vector, prices for turn-up energy use
#' @param direction character, being `forward` or `backward`. The direction where energy can be shifted
#' @param time_horizon integer, maximum number of positions to shift energy from
#' @param LFmax numeric, value of maximum power (in kW) of the flexible load `LF`
#' @param import_capacity numeric or numeric vector, grid maximum import capacity that will limit the maximum optimized demand
#' @param export_capacity numeric or numeric vector, grid maximum export capacity that will limit the maximum optimized demand
#' @param w numeric, optimization objective weight (`w=1` minimizes net power while `w=0` minimizes cost).
#' @param lambda numeric, penalty on change for the flexible load.
#' @inheritParams demand_solve_window
#'
#' @return numeric vector
#' @keywords internal
#'
demand_combined_window <- function(
  G,
  LF,
  LS,
  PI,
  PE,
  PTD,
  PTU,
  direction,
  time_horizon,
  LFmax,
  import_capacity,
  export_capacity,
  w,
  lambda,
  energy_ratio = c(1, 1),
  cumsum_min = NULL
) {
  time_slots <- length(LF)
  identityMat <- diag(time_slots)
  LambdaMat <- get_lambda_matrix(time_slots)

  # Unknown variable: X = [O, I, E]
  # Grid term uses diagonal quadratic (normalized by mean price squared).
  # Ramping penalty uses LambdaMat = D'D so lambda penalizes slot-to-slot changes.
  P <- rbind(
    cbind(
      2 * w * mean(PI)^2 * identityMat + 2 * lambda * LambdaMat,
      identityMat * 0,
      identityMat * 0
    ),
    cbind(
      identityMat * 0,
      identityMat * 0,
      identityMat * 0
    ),
    cbind(
      identityMat * 0,
      identityMat * 0,
      identityMat * 0
    )
  )
  q <- c(
    (1 - w) * (PTD - PTU) - 2 * w * mean(PI)^2 * (G - LS),
    (1 - w) * PI,
    -(1 - w) * PE
  )

  demand_solve_window(
    G,
    LF,
    LS,
    direction,
    time_horizon,
    LFmax,
    import_capacity,
    export_capacity,
    P,
    q,
    energy_ratio,
    cumsum_min
  )
}
