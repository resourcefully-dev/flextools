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
    ub_O <- pmin(pmax(ub_shift, lb_O), LFmax)
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


demand_highs_options <- function() {
  optimization_highs_options(include_mip_gap = FALSE)
}


demand_highs_is_optimal <- function(result) {
  identical(result$status_message, "Optimal")
}


demand_normalize_quadratic <- function(P, tolerance = 1e-8) {
  optimization_normalize_quadratic(
    P = P,
    tolerance = tolerance,
    problem_name = "demand optimization"
  )
}


demand_solve_highs_problem <- function(P, q, Amat, lb, ub) {
  # OSQP solved `1/2 x'Px + q'x` subject to `lb <= Ax <= ub`.
  # HiGHS uses the same QP convention, so the original formulation can be
  # reused directly once the constraint matrix is mapped to `lhs/rhs`.
  highs::highs_solve(
    Q = demand_normalize_quadratic(P),
    L = q,
    lower = rep(-Inf, ncol(Amat)),
    upper = rep(Inf, ncol(Amat)),
    A = Amat,
    lhs = lb,
    rhs = ub,
    types = rep(1L, ncol(Amat)),
    control = demand_highs_options()
  )
}


demand_attach_profile <- function(optimized_load, imported = NULL, exported = NULL) {
  if (!is.null(imported)) {
    attr(optimized_load, "import") <- as.numeric(imported)
  }
  if (!is.null(exported)) {
    attr(optimized_load, "export") <- as.numeric(exported)
  }

  optimized_load
}


demand_solution_tolerance <- function() {
  optimization_solution_tolerance()
}


demand_objective_tolerance <- function() {
  optimization_objective_tolerance()
}


demand_relative_gap_tolerance <- function() {
  optimization_relative_gap_tolerance()
}


demand_objective_gap <- function(lower_bound, incumbent) {
  optimization_objective_gap(lower_bound, incumbent)
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


demand_mode_values <- function(x_value, solver_data) {
  x_value[solver_data$grid_mode_idx]
}


demand_round_mode_guess <- function(x_value, solver_data) {
  import <- x_value[solver_data$import_idx]
  export <- x_value[solver_data$export_idx]

  as.numeric(import >= export)
}


demand_branch_index <- function(x_value, solver_data, tolerance = 1e-6) {
  import <- x_value[solver_data$import_idx]
  export <- x_value[solver_data$export_idx]
  simultaneous <- which(import > tolerance & export > tolerance)

  if (length(simultaneous) > 0) {
    scores <- pmin(import[simultaneous], export[simultaneous])
    return(simultaneous[which.max(scores)])
  }

  z_value <- demand_mode_values(x_value, solver_data)
  fractional <- which(z_value > tolerance & z_value < 1 - tolerance)
  if (length(fractional) == 0) {
    return(NA_integer_)
  }

  scores <- pmin(z_value[fractional], 1 - z_value[fractional])
  fractional[which.max(scores)]
}


demand_has_simultaneous_flows <- function(
  x_value,
  solver_data,
  tolerance = 1e-6
) {
  import <- x_value[solver_data$import_idx]
  export <- x_value[solver_data$export_idx]

  any(import > tolerance & export > tolerance)
}


demand_extract_solution <- function(x_value, solver_data) {
  optimized_load <- round(x_value[solver_data$optimized_idx], 2)

  if (!solver_data$has_grid_flows) {
    return(demand_attach_profile(optimized_load))
  }

  imported <- pmax(x_value[solver_data$import_idx], 0)
  exported <- pmax(x_value[solver_data$export_idx], 0)
  tolerance <- demand_solution_tolerance()
  imported[imported < tolerance] <- 0
  exported[exported < tolerance] <- 0

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
    control = demand_highs_options()
  )

  list(result = result, x = result$primal_solution)
}


demand_solve_qp_relaxation <- function(
  solver_data,
  bounds,
  mode_lower,
  mode_upper
) {
  problem <- demand_build_highs_problem(
    solver_data = solver_data,
    bounds = bounds,
    relax_binaries = TRUE,
    mode_lower = mode_lower,
    mode_upper = mode_upper
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
    control = demand_highs_options()
  )

  list(result = result, x = result$primal_solution)
}


demand_solve_miqp_window <- function(solver_data, bounds) {
  time_slots <- solver_data$time_slots
  objective_tolerance <- demand_objective_tolerance()
  relative_gap_tolerance <- demand_relative_gap_tolerance()

  root_relaxation <- demand_solve_qp_relaxation(
    solver_data = solver_data,
    bounds = bounds,
    mode_lower = rep(0, time_slots),
    mode_upper = rep(1, time_slots)
  )

  if (!demand_highs_is_optimal(root_relaxation$result)) {
    return(list(
      result = root_relaxation$result,
      x = root_relaxation$x
    ))
  }

  # Most quadratic demand windows already satisfy import/export exclusivity in
  # the continuous relaxation. Accepting those windows directly avoids the
  # expensive custom branch-and-bound search with no change in feasibility.
  if (!demand_has_simultaneous_flows(root_relaxation$x, solver_data)) {
    return(root_relaxation)
  }

  best_objective <- Inf
  best_x <- NULL
  stack <- list(list(
    mode_lower = rep(0, time_slots),
    mode_upper = rep(1, time_slots)
  ))

  update_incumbent <- function(mode_fixed) {
    heuristic <- demand_solve_qp_relaxation(
      solver_data = solver_data,
      bounds = bounds,
      mode_lower = mode_fixed,
      mode_upper = mode_fixed
    )

    if (!demand_highs_is_optimal(heuristic$result)) {
      return()
    }

    if (heuristic$result$objective_value + objective_tolerance < best_objective) {
      best_objective <<- heuristic$result$objective_value
      best_x <<- heuristic$x
    }
  }

  root_mode_guess <- demand_round_mode_guess(root_relaxation$x, solver_data)
  update_incumbent(root_mode_guess)

  if (
    !is.null(best_x) &&
      demand_objective_gap(
        lower_bound = root_relaxation$result$objective_value,
        incumbent = best_objective
      ) <= relative_gap_tolerance
  ) {
    return(list(
      result = list(
        status_message = "Optimal",
        objective_value = best_objective
      ),
      x = best_x
    ))
  }

  while (length(stack) > 0) {
    node <- stack[[length(stack)]]
    stack <- stack[-length(stack)]

    if (
      identical(node$mode_lower, rep(0, time_slots)) &&
        identical(node$mode_upper, rep(1, time_slots))
    ) {
      relaxation <- root_relaxation
    } else {
      relaxation <- demand_solve_qp_relaxation(
        solver_data = solver_data,
        bounds = bounds,
        mode_lower = node$mode_lower,
        mode_upper = node$mode_upper
      )
    }
    if (!demand_highs_is_optimal(relaxation$result)) {
      next
    }

    if (relaxation$result$objective_value >= best_objective - objective_tolerance) {
      next
    }

    if (
      !is.null(best_x) &&
        demand_objective_gap(
          lower_bound = relaxation$result$objective_value,
          incumbent = best_objective
        ) <= relative_gap_tolerance
    ) {
      next
    }

    branch_idx <- demand_branch_index(relaxation$x, solver_data)
    if (is.na(branch_idx)) {
      best_objective <- relaxation$result$objective_value
      best_x <- relaxation$x
      next
    }

    mode_guess <- demand_round_mode_guess(relaxation$x, solver_data)
    mode_guess <- pmin(pmax(mode_guess, node$mode_lower), node$mode_upper)
    update_incumbent(mode_guess)

    left_node <- list(
      mode_lower = node$mode_lower,
      mode_upper = node$mode_upper
    )
    left_node$mode_lower[branch_idx] <- 0
    left_node$mode_upper[branch_idx] <- 0

    right_node <- list(
      mode_lower = node$mode_lower,
      mode_upper = node$mode_upper
    )
    right_node$mode_lower[branch_idx] <- 1
    right_node$mode_upper[branch_idx] <- 1

    mode_value <- demand_mode_values(relaxation$x, solver_data)[branch_idx]
    if (mode_value >= 0.5) {
      stack <- c(stack, list(left_node, right_node))
    } else {
      stack <- c(stack, list(right_node, left_node))
    }
  }

  if (is.null(best_x)) {
    return(list(
      result = list(status_message = "Primal infeasible or unbounded"),
      x = NULL
    ))
  }

  list(
    result = list(
      status_message = "Optimal",
      objective_value = best_objective
    ),
    x = best_x
  )
}


demand_select_window_solver <- function(solver_data) {
  if (!solver_data$has_grid_flows) {
    return(function(solver_data, bounds) {
      result <- demand_solve_highs_problem(
        P = solver_data$P,
        q = solver_data$q,
        Amat = solver_data$A,
        lb = bounds$lb,
        ub = bounds$ub
      )

      list(result = result, x = result$primal_solution)
    })
  }

  if (is.null(solver_data$P)) {
    return(demand_solve_milp_window)
  }

  demand_solve_miqp_window
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
#' - `price_imported`: price for imported energy (€/kWh).
#' This is used when `opt_objective = "cost"`.
#'
#' - `price_exported`: price for exported energy (€/kWh).
#' This is used when `opt_objective = "cost"`.
#'
#' - `price_turn_down`: price for turn-down energy use (€/kWh).
#' This is used when `opt_objective = "cost"`.
#'
#' - `price_turn_up`: price for turn-up energy use (€/kWh).
#' This is used when `opt_objective = "cost"`.
#'
#' @param opt_objective character or numeric.
#' Optimization objective can be `"grid"` (default) or `"cost"`, or
#' a number between `0` and `1` to perform combined optimization
#' where `0 == "cost"` and `1 == "grid"`.
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
      ~ minimize_net_power_window(
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
      ~ minimize_cost_window(
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
      ~ optimize_demand_window(
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
#'
#' @return numeric vector
#' @keywords internal
#'
solve_optimization_window <- function(
  G,
  LF,
  LS,
  direction,
  time_horizon,
  LFmax,
  import_capacity,
  export_capacity,
  P,
  q
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
  P_normalized <- demand_normalize_quadratic(P)

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

    # Total flexible energy must be preserved.
    Amat_energy <- cbind(
      matrix(1, ncol = time_slots),
      matrix(0, ncol = time_slots),
      matrix(0, ncol = time_slots)
    )
    lb_energy <- sum(LF)
    ub_energy <- sum(LF)

    Amat <- rbind(
      Amat_O,
      Amat_I,
      Amat_E,
      Amat_balance,
      Amat_cumsum,
      Amat_energy
    )

    bounds_with_capacities <- function(import_cap, export_cap) {
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
            lb_energy
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
            ub_energy
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
    lb_energy <- sum(LF)
    ub_energy <- sum(LF)

    Amat <- rbind(Amat_O, Amat_cumsum, Amat_energy)

    bounds_with_capacities <- function(import_cap, export_cap) {
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

      list(
        lb = round(c(L_bounds$lb_O, lb_cumsum, lb_energy), 2),
        ub = round(c(L_bounds$ub_O, ub_cumsum, ub_energy), 2)
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

  solve_with_capacities <- function(import_cap, export_cap) {
    bounds <- solver_data$bounds_with_capacities(import_cap, export_cap)
    solve_window_problem(solver_data, bounds)
  }

  # First solve: keep the original grid limits.
  O <- solve_with_capacities(import_capacity, export_capacity)
  if (demand_highs_is_optimal(O$result)) {
    return(demand_extract_solution(O$x, solver_data))
  }

  # Fallback solve: if the original grid limits make the problem infeasible,
  # the legacy implementation removed them and retried. The same fallback is
  # preserved here so public behavior remains stable.
  message_once(
    "\u26A0\uFE0F Optimization warning: optimization not feasible in some windows. Removing grid constraints."
  )
  O <- solve_with_capacities(rep(Inf, time_slots), rep(Inf, time_slots))
  if (demand_highs_is_optimal(O$result)) {
    return(demand_extract_solution(O$x, solver_data))
  }

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
#'
#' @return numeric vector
#' @keywords internal
#'
minimize_net_power_window <- function(
  G,
  LF,
  LS,
  direction,
  time_horizon,
  LFmax,
  import_capacity,
  export_capacity,
  lambda = 0
) {
  time_slots <- length(LF)
  identityMat <- diag(time_slots)

  # Same net-power objective as before:
  #   min sum((O + LS - G)^2) + lambda * sum((O - LF)^2)
  P <- 2 * identityMat * (1 + lambda)
  q <- 2 * (LS - G - lambda * LF)

  solve_optimization_window(
    G,
    LF,
    LS,
    direction,
    time_horizon,
    LFmax,
    import_capacity,
    export_capacity,
    P,
    q
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
#'
#' @return numeric vector
#' @keywords internal
#'
minimize_cost_window <- function(
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
  lambda = 0
) {
  time_slots <- length(LF)
  identityMat <- diag(time_slots)

  # Unknown variable: X = [O, I, E]
  # The linear part is the same economic objective used before. The only
  # quadratic term is the optional smoothing penalty on deviations from LF.
  P <- rbind(
    cbind(
      2 * lambda * identityMat,
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
    PTD - PTU - 2 * lambda * LF,
    PI,
    -PE
  )

  solve_optimization_window(
    G,
    LF,
    LS,
    direction,
    time_horizon,
    LFmax,
    import_capacity,
    export_capacity,
    P,
    q
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
#'
#' @return numeric vector
#' @keywords internal
#'
optimize_demand_window <- function(
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
  lambda
) {
  time_slots <- length(LF)
  identityMat <- diag(time_slots)

  # Unknown variable: X = [O, I, E]
  # This preserves the original weighted combination of cost and grid terms.
  P <- rbind(
    cbind(
      2 * (w * mean(PI)^2 + lambda) * identityMat,
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
    (1 - w) * (PTD - PTU) - 2 * lambda * LF - 2 * w * mean(PI)^2 * (G - LS),
    (1 - w) * PI,
    -(1 - w) * PE
  )

  solve_optimization_window(
    G,
    LF,
    LS,
    direction,
    time_horizon,
    LFmax,
    import_capacity,
    export_capacity,
    P,
    q
  )
}
