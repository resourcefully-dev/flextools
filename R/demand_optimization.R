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
  export_capacity
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

capacity_slice_problem <- function(
  G,
  LF,
  LS,
  direction,
  time_horizon,
  LFmax,
  import_capacity,
  export_capacity,
  energy_lb = NULL
) {
  time_slots <- length(LF)
  identityMat <- diag(time_slots)
  zeroMat <- matrix(0, nrow = time_slots, ncol = time_slots)
  cumsumMat <- triangulate_matrix(matrix(1, time_slots, time_slots), "l")

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

    A_cumsum_lb <- cbind(-horizonMat_cumsum, cumsumMat)
    lhs_cumsum_lb <- rep(0, time_slots)
    rhs_cumsum_lb <- rep(Inf, time_slots)

    A_cumsum_ub <- cbind(-cumsumMat, cumsumMat)
    lhs_cumsum_ub <- rep(-Inf, time_slots)
    rhs_cumsum_ub <- rep(0, time_slots)
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

    A_cumsum_lb <- cbind(-cumsumMat, cumsumMat)
    lhs_cumsum_lb <- rep(0, time_slots)
    rhs_cumsum_lb <- rep(Inf, time_slots)

    A_cumsum_ub <- cbind(-horizonMat_cumsum, cumsumMat)
    lhs_cumsum_ub <- rep(-Inf, time_slots)
    rhs_cumsum_ub <- rep(0, time_slots)
  }

  final_lb <- round(pmax(G - LS - export_capacity, 0), 2)
  final_ub <- round(pmin(pmax(G - LS + import_capacity, 0), LFmax), 2)

  A_slice_bounds <- cbind(identityMat, zeroMat)
  A_final_bounds <- cbind(-identityMat, identityMat)
  A_shift_identity <- cbind(-horizonMat_identity, identityMat)
  # Energy row: -sum(slice) + sum(added) = sum(final) - sum(LF). By default an
  # equality, so the slice is re-added in full. When `energy_lb` admits less
  # energy the row becomes a range and the objective charges 2 per unit
  # removed and refunds 1 per unit re-added: a shifted unit still costs 1
  # (as before), a dropped unit costs 2 and doing nothing costs 0 — so the LP
  # shifts before it drops, and drops only what the caps leave no room for.
  A_energy <- matrix(0, nrow = 1, ncol = 2 * time_slots)
  A_energy[1, seq_len(time_slots)] <- -1
  A_energy[1, time_slots + seq_len(time_slots)] <- 1
  LF_energy <- sum(LF)
  if (
    is.null(energy_lb) ||
      energy_lb >= LF_energy - optimization_solution_tolerance()
  ) {
    L <- c(rep(1, time_slots), rep(0, time_slots))
    lhs_energy <- 0
  } else {
    L <- c(rep(2, time_slots), rep(-1, time_slots))
    lhs_energy <- round(energy_lb - LF_energy, 2)
  }

  list(
    L = L,
    lower = c(rep(0, time_slots), rep(0, time_slots)),
    upper = c(LF, rep(Inf, time_slots)),
    A = rbind(
      A_slice_bounds,
      A_final_bounds,
      A_cumsum_lb,
      A_cumsum_ub,
      A_shift_identity,
      A_energy
    ),
    lhs = c(
      rep(0, time_slots),
      final_lb - LF,
      lhs_cumsum_lb,
      lhs_cumsum_ub,
      rep(-Inf, time_slots),
      lhs_energy
    ),
    rhs = c(
      LF,
      final_ub - LF,
      rhs_cumsum_lb,
      rhs_cumsum_ub,
      rep(0, time_slots),
      0
    )
  )
}


select_capacity_slice <- function(
  G,
  LF,
  LS,
  direction,
  time_horizon,
  LFmax,
  import_capacity,
  export_capacity,
  energy_lb = NULL
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
    energy_lb = energy_lb
  )

  result <- highs::highs_solve(
    Q = NULL,
    L = problem$L,
    lower = problem$lower,
    upper = problem$upper,
    A = problem$A,
    lhs = problem$lhs,
    rhs = problem$rhs,
    types = rep(1L, ncol(problem$A)),
    control = optimization_highs_options(include_mip_gap = FALSE)
  )

  if (!demand_highs_is_optimal(result) || is.null(result$primal_solution)) {
    return(NULL)
  }

  tolerance <- optimization_solution_tolerance()
  slice <- pmax(result$primal_solution[seq_len(time_slots)], 0)
  slice[slice < tolerance] <- 0
  added <- pmax(result$primal_solution[time_slots + seq_len(time_slots)], 0)
  added[added < tolerance] <- 0

  list(
    slice = round(slice, 2),
    added = round(added, 2),
    result = result
  )
}


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
  energy_ratio = c(1, 1)
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

  slice_solution <- select_capacity_slice(
    G = G,
    LF = LF_target,
    LS = LS,
    direction = direction,
    time_horizon = time_horizon,
    LFmax = LFmax,
    import_capacity = import_capacity,
    export_capacity = export_capacity,
    energy_lb = energy_lb
  )

  if (is.null(slice_solution)) {
    # The capacity slice LP is infeasible under the true caps, even after
    # dropping energy down to the floor. Rather than dropping the grid
    # constraints entirely, relax each cap only as far as a reference profile
    # already needs (import LS + LF_ref - G, export G - LS - LF_ref). Slots
    # within their caps keep the true capacity, so the result can never be
    # worse than that profile. The reference is the original, unshifted
    # profile - or, when the energy range admits less, the original profile
    # scaled down to the minimum energy, since the minimum is all that has to
    # be forced through the capacity. demand_grid_window() (via
    # demand_solve_window) is feasible by construction with these caps because
    # O = LF_ref is a feasible point; its energy row is pinned to sum(LF_ref)
    # through a ratio of the rounded reference, so the two agree exactly.
    LF_ref <- LF
    fallback_ratio <- c(1, 1)
    if (energy_ratio[1] < 1 && sum(LF) > 0) {
      LF_ref <- round(LF * energy_ratio[1], 2)
      fallback_ratio <- rep(min(sum(LF_ref) / sum(LF), 1), 2)
    }
    import_cap_relaxed <- pmax(import_capacity, LS + LF_ref - G) + tol
    export_cap_relaxed <- pmax(export_capacity, G - LS - LF_ref) + tol
    if (identical(LF_ref, LF)) {
      message_once(
        "\u26a0\ufe0f Optimization warning: optimization not feasible in some windows. Relaxing grid capacity to the original profile in the affected windows."
      )
    } else {
      message_once(
        "\u26a0\ufe0f Optimization warning: even the minimum energy does not fit under the grid capacity in some windows. Relaxing grid capacity to the minimum-energy profile in the affected windows."
      )
    }
    return(
      demand_grid_window(
        G = G,
        LF = LF,
        LS = LS,
        direction = direction,
        time_horizon = time_horizon,
        LFmax = LFmax,
        import_capacity = import_cap_relaxed,
        export_capacity = export_cap_relaxed,
        lambda = lambda,
        energy_ratio = fallback_ratio
      )
    )
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
  slice_ratio <- if (added_energy < moved_energy - tol) {
    rep(added_energy / moved_energy, 2)
  } else {
    c(1, 1)
  }

  fixed_load <- round(LF_target - moved_slice, 2)
  optimized_slice <- demand_grid_window(
    G = G,
    LF = moved_slice,
    LS = LS + fixed_load,
    direction = direction,
    time_horizon = time_horizon,
    LFmax = round(pmax(LFmax - fixed_load, 0), 2),
    import_capacity = import_capacity,
    export_capacity = export_capacity,
    lambda = lambda,
    energy_ratio = slice_ratio
  )

  round(fixed_load + as.numeric(optimized_slice), 2)
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
#' then applies the grid-minimizing formulation only to that moved slice.
#' If that constrained problem is infeasible, grid limits are removed for the
#' affected optimization window.
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
  energy_ratio = c(1, 1)
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
    export_capacity
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
        export_cap
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
            lb_cumsum,
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
        export_cap
      )

      # Minimal-relaxation retry: guarantee O = LF is a feasible point even when
      # LFmax sits below LF (see the grid-flow branch for the full argument).
      if (clamp_to_lf) {
        L_bounds$ub_O <- pmax(L_bounds$ub_O, LF)
        L_bounds$lb_O <- pmin(L_bounds$lb_O, LF)
      }

      list(
        lb = round(c(L_bounds$lb_O, lb_cumsum, energy_floor), 2),
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

  # Fallback solve: the original grid limits make the problem infeasible.
  # Instead of removing the grid constraints entirely (an unconstrained solve
  # that could shift energy into new, worse violations), raise the per-slot
  # caps only as far as a reference profile already needs. Slots that were
  # within their caps keep the true capacity, so the optimizer can never
  # create a violation worse than that profile had.
  #
  # The reference is the ORIGINAL, unshifted profile O = LF - unless the energy
  # range admits less: then it is LF scaled down to the minimum energy, since
  # the minimum is all that has to be forced through the capacity.
  #
  # This retry is feasible BY CONSTRUCTION: the reference point O = LF_ref
  # (with the induced grid flows I = pmax(LS + LF_ref - G, 0),
  # E = pmax(G - LS - LF_ref, 0)) satisfies every constraint:
  #   * energy: pinned to sum(LF_ref) — when the minimum is all that fits the
  #     capacity is exceeded by exactly what the minimum needs, no more;
  #   * cumsum bounds: they are derived from LF itself, so cumsum(LF) lies
  #     inside [lb_cumsum, ub_cumsum] (one side is exactly cumsum(LF)), and a
  #     scaled LF_ref stays inside because a range below 1 is only admitted
  #     for forward shifting over the whole window (lb_cumsum = 0);
  #   * grid balance: O - I + E = G - LS by definition;
  #   * capacity: the relaxed caps equal at least the reference net flows, so
  #     I <= import_cap_relaxed and E <= export_cap_relaxed;
  #   * optimized-load box: lb_O <= LF_ref is always true, and clamp_to_lf
  #     lifts ub_O up to LF (>= LF_ref) when LFmax would otherwise pull it
  #     below.
  # A small tolerance absorbs the 2-decimal rounding of the bounds.
  tol <- optimization_solution_tolerance()
  LF_ref <- LF
  energy_floor <- energy_lb
  energy_ceiling <- energy_ub
  if (energy_lb < round(LF_energy, 2) && LF_energy > 0) {
    LF_ref <- round(LF * energy_lb / LF_energy, 2)
    energy_floor <- sum(LF_ref)
    energy_ceiling <- sum(LF_ref)
  }
  import_cap_relaxed <- pmax(import_capacity, LS + LF_ref - G) + tol
  export_cap_relaxed <- pmax(export_capacity, G - LS - LF_ref) + tol
  if (identical(LF_ref, LF)) {
    message_once(
      "\u26A0\uFE0F Optimization warning: optimization not feasible in some windows. Relaxing grid capacity to the original profile in the affected windows."
    )
  } else {
    message_once(
      "\u26A0\uFE0F Optimization warning: even the minimum energy does not fit under the grid capacity in some windows. Relaxing grid capacity to the minimum-energy profile in the affected windows."
    )
  }
  O <- solve_with_capacities(
    import_cap_relaxed,
    export_cap_relaxed,
    clamp_to_lf = TRUE,
    energy_floor = energy_floor,
    energy_ceiling = energy_ceiling
  )
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
  energy_ratio = c(1, 1)
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
    energy_ratio
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
  energy_ratio = c(1, 1)
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
    energy_ratio
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
  energy_ratio = c(1, 1)
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
    energy_ratio
  )
}
