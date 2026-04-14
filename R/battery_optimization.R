# Mixed-integer battery optimization backend built on HiGHS.
# The active formulation uses one signed battery power variable per timestep:
# positive values charge the battery and negative values discharge it. Exact
# charging/discharging efficiencies are enforced with one auxiliary energy
# change variable and, when needed, one battery-mode binary per timestep.

battery_attach_profile <- function(battery, charge, discharge) {
  attr(battery, "charge") <- as.numeric(charge)
  attr(battery, "discharge") <- as.numeric(discharge)
  battery
}


battery_solution_tolerance <- function() {
  optimization_solution_tolerance()
}


battery_highs_options <- function() {
  optimization_highs_options(include_mip_gap = TRUE)
}


battery_highs_is_optimal <- function(result) {
  identical(result$status_message, "Optimal")
}


battery_objective_tolerance <- function() {
  optimization_objective_tolerance()
}


battery_relative_gap_tolerance <- function() {
  optimization_relative_gap_tolerance()
}


battery_branch_and_bound_node_limit <- function() {
  1000L
}


battery_branch_and_bound_time_limit_seconds <- function() {
  1
}


battery_objective_gap <- function(lower_bound, incumbent) {
  optimization_objective_gap(lower_bound, incumbent)
}


battery_solver_status_message <- function(result) {
  if (!is.null(result$status_message)) {
    return(result$status_message)
  }

  result$status
}


battery_solution_is_acceptable <- function(result) {
  battery_solver_status_message(result) %in%
    c(
      "Optimal",
      "Heuristic",
      "Node limit reached",
      "Time limit reached"
    )
}


battery_normalize_quadratic <- function(P, tolerance = 1e-8) {
  if (is.null(P)) {
    return(NULL)
  }

  optimization_normalize_quadratic(
    P = P,
    tolerance = tolerance,
    problem_name = "battery optimization"
  )
}


battery_build_mode_constraints <- function(solver_data, bounds) {
  time_slots <- solver_data$time_slots
  total_variables <- solver_data$n_variables + solver_data$total_mode_variables
  A_list <- list()
  lhs <- numeric()
  rhs <- numeric()

  if (solver_data$has_battery_mode) {
    A_charge <- matrix(0, nrow = time_slots, ncol = total_variables)
    A_charge[cbind(seq_len(time_slots), solver_data$power_idx)] <- 1
    A_charge[cbind(
      seq_len(time_slots),
      solver_data$battery_mode_idx
    )] <- -solver_data$Bc

    A_discharge <- matrix(0, nrow = time_slots, ncol = total_variables)
    A_discharge[cbind(seq_len(time_slots), solver_data$power_idx)] <- -1
    A_discharge[cbind(
      seq_len(time_slots),
      solver_data$battery_mode_idx
    )] <- solver_data$Bd

    A_charge_delta <- matrix(0, nrow = time_slots, ncol = total_variables)
    A_charge_delta[cbind(seq_len(time_slots), solver_data$delta_idx)] <- 1
    A_charge_delta[cbind(
      seq_len(time_slots),
      solver_data$power_idx
    )] <- -solver_data$charge_eff
    A_charge_delta[cbind(
      seq_len(time_slots),
      solver_data$battery_mode_idx
    )] <- solver_data$charge_branch_M

    A_charge_delta_neg <- matrix(0, nrow = time_slots, ncol = total_variables)
    A_charge_delta_neg[cbind(seq_len(time_slots), solver_data$delta_idx)] <- -1
    A_charge_delta_neg[cbind(
      seq_len(time_slots),
      solver_data$power_idx
    )] <- solver_data$charge_eff
    A_charge_delta_neg[cbind(
      seq_len(time_slots),
      solver_data$battery_mode_idx
    )] <- solver_data$charge_branch_M

    A_discharge_delta <- matrix(0, nrow = time_slots, ncol = total_variables)
    A_discharge_delta[cbind(seq_len(time_slots), solver_data$delta_idx)] <- 1
    A_discharge_delta[cbind(
      seq_len(time_slots),
      solver_data$power_idx
    )] <- -(1 / solver_data$discharge_eff)
    A_discharge_delta[cbind(
      seq_len(time_slots),
      solver_data$battery_mode_idx
    )] <- -solver_data$discharge_branch_M

    A_discharge_delta_neg <- matrix(
      0,
      nrow = time_slots,
      ncol = total_variables
    )
    A_discharge_delta_neg[cbind(
      seq_len(time_slots),
      solver_data$delta_idx
    )] <- -1
    A_discharge_delta_neg[cbind(
      seq_len(time_slots),
      solver_data$power_idx
    )] <- 1 / solver_data$discharge_eff
    A_discharge_delta_neg[cbind(
      seq_len(time_slots),
      solver_data$battery_mode_idx
    )] <- -solver_data$discharge_branch_M

    A_list <- c(
      A_list,
      list(
        A_charge,
        A_discharge,
        A_charge_delta,
        A_charge_delta_neg,
        A_discharge_delta,
        A_discharge_delta_neg
      )
    )
    lhs <- c(
      lhs,
      rep(-Inf, time_slots),
      rep(-Inf, time_slots),
      rep(-Inf, time_slots),
      rep(-Inf, time_slots),
      rep(-Inf, time_slots),
      rep(-Inf, time_slots)
    )
    rhs <- c(
      rhs,
      rep(0, time_slots),
      rep(solver_data$Bd, time_slots),
      rep(solver_data$charge_branch_M, time_slots),
      rep(solver_data$charge_branch_M, time_slots),
      rep(0, time_slots),
      rep(0, time_slots)
    )
  }

  if (solver_data$has_grid_mode) {
    A_import_mode <- matrix(0, nrow = time_slots, ncol = total_variables)
    A_import_mode[cbind(seq_len(time_slots), solver_data$import_idx)] <- 1
    A_import_mode[cbind(seq_len(time_slots), solver_data$grid_mode_idx)] <-
      -bounds$import_mode_ub

    A_export_mode <- matrix(0, nrow = time_slots, ncol = total_variables)
    A_export_mode[cbind(seq_len(time_slots), solver_data$export_idx)] <- 1
    A_export_mode[cbind(seq_len(time_slots), solver_data$grid_mode_idx)] <-
      bounds$export_mode_ub

    A_list <- c(A_list, list(A_import_mode, A_export_mode))
    lhs <- c(lhs, rep(-Inf, time_slots), rep(-Inf, time_slots))
    rhs <- c(rhs, rep(0, time_slots), bounds$export_mode_ub)
  }

  if (length(A_list) == 0) {
    return(list(
      A = matrix(0, nrow = 0, ncol = total_variables),
      lhs = numeric(),
      rhs = numeric()
    ))
  }

  list(
    A = do.call(rbind, A_list),
    lhs = lhs,
    rhs = rhs
  )
}


battery_build_highs_problem <- function(
  solver_data,
  bounds,
  relax_binaries = TRUE,
  mode_lower = NULL,
  mode_upper = NULL
) {
  total_mode_variables <- solver_data$total_mode_variables
  total_variables <- solver_data$n_variables + total_mode_variables

  if (is.null(mode_lower)) {
    mode_lower <- rep(0, total_mode_variables)
  }
  if (is.null(mode_upper)) {
    mode_upper <- rep(1, total_mode_variables)
  }

  mode_constraints <- battery_build_mode_constraints(solver_data, bounds)
  A_base <- cbind(
    solver_data$A,
    matrix(0, nrow = nrow(solver_data$A), ncol = total_mode_variables)
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
    L = c(solver_data$q, rep(0, total_mode_variables)),
    lower = c(solver_data$continuous_lower, mode_lower),
    upper = c(solver_data$continuous_upper, mode_upper),
    A = rbind(A_base, mode_constraints$A),
    lhs = c(bounds$lb, mode_constraints$lhs),
    rhs = c(bounds$ub, mode_constraints$rhs),
    types = c(
      rep(1L, solver_data$n_variables),
      if (relax_binaries) {
        rep(1L, total_mode_variables)
      } else {
        rep(2L, total_mode_variables)
      }
    )
  )
}


battery_solve_highs_problem <- function(problem) {
  highs::highs_solve(
    Q = problem$Q,
    L = problem$L,
    lower = problem$lower,
    upper = problem$upper,
    A = problem$A,
    lhs = problem$lhs,
    rhs = problem$rhs,
    types = problem$types,
    control = battery_highs_options()
  )
}


battery_mode_values <- function(x_value, solver_data) {
  if (solver_data$total_mode_variables == 0) {
    return(numeric())
  }

  x_value[solver_data$mode_variable_idx]
}


battery_round_mode_guess <- function(x_value, solver_data) {
  mode_guess <- numeric()

  if (solver_data$has_battery_mode) {
    power <- x_value[solver_data$power_idx]
    delta <- x_value[solver_data$delta_idx]
    charge_residual <- abs(delta - solver_data$charge_eff * power)
    discharge_residual <- abs(delta - power / solver_data$discharge_eff)
    battery_mode <- as.numeric(charge_residual <= discharge_residual)
    battery_mode[abs(power) <= battery_solution_tolerance()] <- 1
    mode_guess <- c(mode_guess, battery_mode)
  }

  if (solver_data$has_grid_mode) {
    net_exchange <- solver_data$L -
      solver_data$G +
      x_value[solver_data$power_idx]
    grid_mode <- as.numeric(net_exchange >= 0)
    near_zero <- abs(net_exchange) <= battery_solution_tolerance()
    if (any(near_zero)) {
      import <- x_value[solver_data$import_idx]
      export <- x_value[solver_data$export_idx]
      grid_mode[near_zero] <- as.numeric(import[near_zero] >= export[near_zero])
    }
    mode_guess <- c(mode_guess, grid_mode)
  }

  mode_guess
}


battery_solution_is_physically_valid <- function(
  x_value,
  solver_data,
  tolerance = 1e-6
) {
  power <- x_value[solver_data$power_idx]
  delta <- x_value[solver_data$delta_idx]

  if (solver_data$has_battery_mode) {
    charge_ok <- (power >= -tolerance) &
      (abs(delta - solver_data$charge_eff * power) <= tolerance)
    discharge_ok <- (power <= tolerance) &
      (abs(delta - power / solver_data$discharge_eff) <= tolerance)

    if (any(!(charge_ok | discharge_ok))) {
      return(FALSE)
    }
  } else {
    if (any(abs(delta - power) > tolerance)) {
      return(FALSE)
    }
  }

  if (!solver_data$has_grid_mode) {
    return(TRUE)
  }

  import <- x_value[solver_data$import_idx]
  export <- x_value[solver_data$export_idx]
  !any(import > tolerance & export > tolerance)
}


battery_branch_index <- function(x_value, solver_data, tolerance = 1e-6) {
  if (solver_data$has_grid_mode) {
    import <- x_value[solver_data$import_idx]
    export <- x_value[solver_data$export_idx]
    simultaneous_grid <- which(import > tolerance & export > tolerance)

    if (length(simultaneous_grid) > 0) {
      scores <- pmin(import[simultaneous_grid], export[simultaneous_grid])
      return(
        length(solver_data$battery_mode_idx) +
          simultaneous_grid[which.max(scores)]
      )
    }
  }

  if (!solver_data$has_battery_mode) {
    return(NA_integer_)
  }

  power <- x_value[solver_data$power_idx]
  delta <- x_value[solver_data$delta_idx]
  charge_residual <- abs(delta - solver_data$charge_eff * power)
  discharge_residual <- abs(delta - power / solver_data$discharge_eff)
  valid <- ((power >= -tolerance) & (charge_residual <= tolerance)) |
    ((power <= tolerance) & (discharge_residual <= tolerance))

  invalid_idx <- which(!valid)
  if (length(invalid_idx) == 0) {
    return(NA_integer_)
  }

  scores <- pmin(charge_residual[invalid_idx], discharge_residual[invalid_idx])
  invalid_idx[which.max(scores)]
}


battery_attach_solution <- function(x_value, solver_data) {
  power <- x_value[solver_data$power_idx]
  charge <- pmax(power, 0)
  discharge <- pmax(-power, 0)
  tolerance <- battery_solution_tolerance()
  charge[charge < tolerance] <- 0
  discharge[discharge < tolerance] <- 0
  battery <- charge - discharge

  battery_attach_profile(battery, charge, discharge)
}


battery_solve_milp_window <- function(solver_data, bounds) {
  if (solver_data$total_mode_variables == 0) {
    return(battery_solve_qp_relaxation(
      solver_data = solver_data,
      bounds = bounds,
      mode_lower = numeric(),
      mode_upper = numeric()
    ))
  }

  root_relaxation <- battery_solve_qp_relaxation(
    solver_data = solver_data,
    bounds = bounds,
    mode_lower = rep(0, solver_data$total_mode_variables),
    mode_upper = rep(1, solver_data$total_mode_variables)
  )

  if (
    battery_highs_is_optimal(root_relaxation$result) &&
      battery_solution_is_physically_valid(root_relaxation$x, solver_data)
  ) {
    return(root_relaxation)
  }

  heuristic <- battery_try_mode_heuristic(
    solver_data = solver_data,
    bounds = bounds,
    root_relaxation = root_relaxation
  )
  if (!is.null(heuristic)) {
    return(heuristic)
  }

  problem <- battery_build_highs_problem(
    solver_data = solver_data,
    bounds = bounds,
    relax_binaries = FALSE
  )
  result <- battery_solve_highs_problem(problem)

  list(result = result, x = result$primal_solution)
}


battery_solve_qp_relaxation <- function(
  solver_data,
  bounds,
  mode_lower,
  mode_upper
) {
  problem <- battery_build_highs_problem(
    solver_data = solver_data,
    bounds = bounds,
    relax_binaries = TRUE,
    mode_lower = mode_lower,
    mode_upper = mode_upper
  )
  result <- battery_solve_highs_problem(problem)

  list(result = result, x = result$primal_solution)
}


battery_solve_fixed_battery_modes <- function(
  solver_data,
  bounds,
  battery_mode
) {
  if (solver_data$has_grid_flows || !solver_data$has_battery_mode) {
    stop(
      "Error: fixed battery mode solver only supports battery-only mode problems"
    )
  }

  time_slots <- solver_data$time_slots
  lower <- solver_data$continuous_lower
  upper <- solver_data$continuous_upper
  lower[solver_data$power_idx] <- ifelse(battery_mode > 0.5, 0, -solver_data$Bd)
  upper[solver_data$power_idx] <- ifelse(battery_mode > 0.5, solver_data$Bc, 0)

  A_delta <- cbind(
    diag(ifelse(
      battery_mode > 0.5,
      -solver_data$charge_eff,
      -(1 / solver_data$discharge_eff)
    )),
    diag(time_slots)
  )

  problem <- list(
    Q = solver_data$P,
    L = solver_data$q,
    lower = lower,
    upper = upper,
    A = rbind(solver_data$A, A_delta),
    lhs = c(bounds$lb, rep(0, time_slots)),
    rhs = c(bounds$ub, rep(0, time_slots)),
    types = rep(1L, solver_data$n_variables)
  )
  result <- battery_solve_highs_problem(problem)

  list(result = result, x = result$primal_solution)
}


battery_solve_fixed_modes <- function(solver_data, bounds, mode_values) {
  if (length(mode_values) != solver_data$total_mode_variables) {
    stop("Error: fixed mode vector has invalid length")
  }

  if (solver_data$has_battery_mode && !solver_data$has_grid_flows) {
    return(battery_solve_fixed_battery_modes(
      solver_data = solver_data,
      bounds = bounds,
      battery_mode = mode_values[seq_along(solver_data$battery_mode_idx)]
    ))
  }

  battery_solve_qp_relaxation(
    solver_data = solver_data,
    bounds = bounds,
    mode_lower = mode_values,
    mode_upper = mode_values
  )
}


battery_guess_modes_fast <- function(solver_data, bounds) {
  if (!solver_data$has_battery_mode || solver_data$has_grid_flows) {
    return(NULL)
  }

  approx_solver_data <- battery_build_solver_data(
    P = solver_data$P,
    q = solver_data$q,
    G = solver_data$G,
    L = solver_data$L,
    Bcap = solver_data$Bcap,
    Bc = solver_data$Bc,
    Bd = solver_data$Bd,
    SOCmin = solver_data$SOCmin,
    SOCmax = solver_data$SOCmax,
    SOCini = solver_data$SOCini,
    charge_eff = 1,
    discharge_eff = 1
  )
  approx_bounds <- approx_solver_data$bounds_with_capacities(
    bounds$import_cap,
    bounds$export_cap
  )
  approx_solution <- battery_solve_qp_relaxation(
    solver_data = approx_solver_data,
    bounds = approx_bounds,
    mode_lower = numeric(),
    mode_upper = numeric()
  )

  if (!battery_highs_is_optimal(approx_solution$result)) {
    return(NULL)
  }

  as.numeric(approx_solution$x[approx_solver_data$power_idx] >= 0)
}


battery_build_mode_guess <- function(
  solver_data,
  bounds,
  root_relaxation = NULL
) {
  if (solver_data$total_mode_variables == 0) {
    return(numeric())
  }

  if (is.null(root_relaxation)) {
    root_relaxation <- battery_solve_qp_relaxation(
      solver_data = solver_data,
      bounds = bounds,
      mode_lower = rep(0, solver_data$total_mode_variables),
      mode_upper = rep(1, solver_data$total_mode_variables)
    )
  }

  if (!battery_highs_is_optimal(root_relaxation$result)) {
    return(NULL)
  }

  mode_guess <- battery_round_mode_guess(root_relaxation$x, solver_data)

  if (!solver_data$has_grid_flows) {
    fast_battery_mode <- battery_guess_modes_fast(solver_data, bounds)
    if (!is.null(fast_battery_mode)) {
      mode_guess[seq_along(fast_battery_mode)] <- fast_battery_mode
    }
  }

  mode_guess
}


battery_try_mode_heuristic <- function(
  solver_data,
  bounds,
  root_relaxation = NULL
) {
  mode_guess <- battery_build_mode_guess(
    solver_data = solver_data,
    bounds = bounds,
    root_relaxation = root_relaxation
  )

  if (is.null(mode_guess)) {
    return(NULL)
  }

  if (solver_data$has_battery_mode && solver_data$has_grid_mode) {
    partial_mode_lower <- c(
      mode_guess[seq_along(solver_data$battery_mode_idx)],
      rep(0, length(solver_data$grid_mode_idx))
    )
    partial_mode_upper <- c(
      mode_guess[seq_along(solver_data$battery_mode_idx)],
      rep(1, length(solver_data$grid_mode_idx))
    )
    partial_solution <- battery_solve_qp_relaxation(
      solver_data = solver_data,
      bounds = bounds,
      mode_lower = partial_mode_lower,
      mode_upper = partial_mode_upper
    )

    if (battery_highs_is_optimal(partial_solution$result)) {
      mode_guess <- battery_round_mode_guess(partial_solution$x, solver_data)
    }
  }

  heuristic <- battery_solve_fixed_modes(
    solver_data = solver_data,
    bounds = bounds,
    mode_values = mode_guess
  )

  if (!battery_highs_is_optimal(heuristic$result)) {
    return(NULL)
  }

  if (!battery_solution_is_physically_valid(heuristic$x, solver_data)) {
    return(NULL)
  }

  list(
    result = list(
      status_message = "Heuristic",
      objective_value = heuristic$result$objective_value
    ),
    x = heuristic$x
  )
}


battery_solve_miqp_window <- function(solver_data, bounds) {
  if (solver_data$total_mode_variables == 0) {
    return(battery_solve_qp_relaxation(
      solver_data = solver_data,
      bounds = bounds,
      mode_lower = numeric(),
      mode_upper = numeric()
    ))
  }

  total_mode_variables <- solver_data$total_mode_variables
  objective_tolerance <- battery_objective_tolerance()
  relative_gap_tolerance <- battery_relative_gap_tolerance()
  node_limit <- battery_branch_and_bound_node_limit()
  time_limit <- battery_branch_and_bound_time_limit_seconds()
  start_elapsed <- proc.time()[["elapsed"]]
  nodes_visited <- 0L

  if (!solver_data$has_grid_flows && solver_data$has_battery_mode) {
    fast_mode_guess <- battery_guess_modes_fast(solver_data, bounds)
    if (!is.null(fast_mode_guess)) {
      heuristic <- battery_solve_fixed_battery_modes(
        solver_data = solver_data,
        bounds = bounds,
        battery_mode = fast_mode_guess
      )

      if (
        battery_highs_is_optimal(heuristic$result) &&
          battery_solution_is_physically_valid(heuristic$x, solver_data)
      ) {
        return(list(
          result = list(
            status_message = "Heuristic",
            objective_value = heuristic$result$objective_value
          ),
          x = heuristic$x
        ))
      }
    }
  }

  root_relaxation <- battery_solve_qp_relaxation(
    solver_data = solver_data,
    bounds = bounds,
    mode_lower = rep(0, total_mode_variables),
    mode_upper = rep(1, total_mode_variables)
  )

  if (!battery_highs_is_optimal(root_relaxation$result)) {
    return(list(
      result = root_relaxation$result,
      x = root_relaxation$x
    ))
  }

  if (battery_solution_is_physically_valid(root_relaxation$x, solver_data)) {
    return(root_relaxation)
  }

  heuristic <- battery_try_mode_heuristic(
    solver_data = solver_data,
    bounds = bounds,
    root_relaxation = root_relaxation
  )
  if (!is.null(heuristic)) {
    return(heuristic)
  }

  best_objective <- Inf
  best_x <- NULL
  stack <- list(list(
    mode_lower = rep(0, total_mode_variables),
    mode_upper = rep(1, total_mode_variables)
  ))

  update_incumbent <- function(mode_fixed) {
    if (is.null(mode_fixed)) {
      return()
    }

    heuristic <- battery_solve_qp_relaxation(
      solver_data = solver_data,
      bounds = bounds,
      mode_lower = mode_fixed,
      mode_upper = mode_fixed
    )

    if (!battery_highs_is_optimal(heuristic$result)) {
      return()
    }

    if (!battery_solution_is_physically_valid(heuristic$x, solver_data)) {
      return()
    }

    if (
      heuristic$result$objective_value + objective_tolerance < best_objective
    ) {
      best_objective <<- heuristic$result$objective_value
      best_x <<- heuristic$x
    }
  }

  root_mode_guess <- battery_build_mode_guess(
    solver_data = solver_data,
    bounds = bounds,
    root_relaxation = root_relaxation
  )
  update_incumbent(root_mode_guess)

  if (
    !is.null(best_x) &&
      battery_objective_gap(
        lower_bound = root_relaxation$result$objective_value,
        incumbent = best_objective
      ) <=
        relative_gap_tolerance
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
    nodes_visited <- nodes_visited + 1L
    elapsed <- proc.time()[["elapsed"]] - start_elapsed
    if (!is.null(best_x) && nodes_visited > node_limit) {
      return(list(
        result = list(
          status_message = "Node limit reached",
          objective_value = best_objective,
          nodes_visited = nodes_visited,
          elapsed = elapsed
        ),
        x = best_x
      ))
    }
    if (!is.null(best_x) && elapsed > time_limit) {
      return(list(
        result = list(
          status_message = "Time limit reached",
          objective_value = best_objective,
          nodes_visited = nodes_visited,
          elapsed = elapsed
        ),
        x = best_x
      ))
    }

    node <- stack[[length(stack)]]
    stack <- stack[-length(stack)]

    if (
      identical(node$mode_lower, rep(0, total_mode_variables)) &&
        identical(node$mode_upper, rep(1, total_mode_variables))
    ) {
      relaxation <- root_relaxation
    } else {
      relaxation <- battery_solve_qp_relaxation(
        solver_data = solver_data,
        bounds = bounds,
        mode_lower = node$mode_lower,
        mode_upper = node$mode_upper
      )
    }

    if (!battery_highs_is_optimal(relaxation$result)) {
      next
    }

    if (
      relaxation$result$objective_value >= best_objective - objective_tolerance
    ) {
      next
    }

    if (
      !is.null(best_x) &&
        battery_objective_gap(
          lower_bound = relaxation$result$objective_value,
          incumbent = best_objective
        ) <=
          relative_gap_tolerance
    ) {
      next
    }

    if (battery_solution_is_physically_valid(relaxation$x, solver_data)) {
      best_objective <- relaxation$result$objective_value
      best_x <- relaxation$x
      next
    }

    branch_idx <- battery_branch_index(relaxation$x, solver_data)
    if (is.na(branch_idx)) {
      next
    }

    mode_guess <- battery_round_mode_guess(relaxation$x, solver_data)
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

    mode_value <- battery_mode_values(relaxation$x, solver_data)[branch_idx]
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


battery_quadratic_problem <- function(solver_data) {
  !is.null(solver_data$P)
}


battery_select_window_solver <- function(solver_data) {
  if (battery_quadratic_problem(solver_data)) {
    return(battery_solve_miqp_window)
  }

  battery_solve_milp_window
}


battery_build_solver_data <- function(
  P,
  q,
  G,
  L,
  Bcap,
  Bc,
  Bd,
  SOCmin,
  SOCmax,
  SOCini,
  charge_eff,
  discharge_eff
) {
  P_symmetric <- if (is.null(P)) {
    NULL
  } else {
    battery_normalize_quadratic((P + t(P)) / 2)
  }
  time_slots <- length(G)
  cumsumMat <- triangulate_matrix(matrix(1, time_slots, time_slots), "l")

  if (charge_eff <= 0 || discharge_eff <= 0) {
    stop("Error: charge and discharge efficiencies must be positive")
  }

  has_grid_flows <- length(q) > 2 * time_slots
  has_battery_mode <- !(abs(charge_eff - 1) <= 1e-12 &&
    abs(discharge_eff - 1) <= 1e-12)
  n_grid_variables <- if (has_grid_flows) 2 * time_slots else 0L
  n_variables <- 2 * time_slots + n_grid_variables

  power_idx <- seq_len(time_slots)
  delta_idx <- seq_len(time_slots) + time_slots
  import_idx <- if (has_grid_flows) {
    seq_len(time_slots) + 2 * time_slots
  } else {
    integer()
  }
  export_idx <- if (has_grid_flows) {
    seq_len(time_slots) + 3 * time_slots
  } else {
    integer()
  }

  continuous_lower <- c(
    rep(-Bd, time_slots),
    rep(-Bd / discharge_eff, time_slots),
    if (has_grid_flows) rep(0, 2 * time_slots) else numeric()
  )
  continuous_upper <- c(
    rep(Bc, time_slots),
    rep(charge_eff * Bc, time_slots),
    if (has_grid_flows) rep(Inf, 2 * time_slots) else numeric()
  )

  lb_cumsum <- rep((SOCmin - SOCini) / 100 * Bcap, time_slots)
  ub_cumsum <- rep((SOCmax - SOCini) / 100 * Bcap, time_slots)

  if (has_grid_flows) {
    zeroMat <- matrix(0, nrow = time_slots, ncol = time_slots)

    A_import <- cbind(zeroMat, zeroMat, diag(time_slots), zeroMat)
    A_export <- cbind(zeroMat, zeroMat, zeroMat, diag(time_slots))
    A_balance <- cbind(
      diag(time_slots),
      zeroMat,
      -diag(time_slots),
      diag(time_slots)
    )
    A_soc <- cbind(zeroMat, cumsumMat, zeroMat, zeroMat)
    A_energy <- cbind(
      matrix(0, nrow = 1, ncol = time_slots),
      matrix(1, nrow = 1, ncol = time_slots),
      matrix(0, nrow = 1, ncol = time_slots),
      matrix(0, nrow = 1, ncol = time_slots)
    )

    bounds_with_capacities <- function(import_cap, export_cap) {
      import_cap <- as.numeric(rep_len(import_cap, time_slots))
      export_cap <- as.numeric(rep_len(export_cap, time_slots))
      import_mode_ub <- pmax(L - G + Bc, 0)
      export_mode_ub <- pmax(G - L + Bd, 0)
      import_mode_ub[is.finite(import_cap)] <- pmin(
        import_mode_ub[is.finite(import_cap)],
        import_cap[is.finite(import_cap)]
      )
      export_mode_ub[is.finite(export_cap)] <- pmin(
        export_mode_ub[is.finite(export_cap)],
        export_cap[is.finite(export_cap)]
      )

      list(
        lb = c(
          rep(0, time_slots),
          rep(0, time_slots),
          G - L,
          lb_cumsum,
          0
        ),
        ub = c(
          import_cap,
          export_cap,
          G - L,
          ub_cumsum,
          0
        ),
        import_cap = import_cap,
        export_cap = export_cap,
        import_mode_ub = import_mode_ub,
        export_mode_ub = export_mode_ub
      )
    }

    A <- rbind(A_import, A_export, A_balance, A_soc, A_energy)
  } else {
    zeroMat <- matrix(0, nrow = time_slots, ncol = time_slots)
    A_grid <- cbind(diag(time_slots), zeroMat)
    A_soc <- cbind(zeroMat, cumsumMat)
    A_energy <- cbind(
      matrix(0, nrow = 1, ncol = time_slots),
      matrix(1, nrow = 1, ncol = time_slots)
    )

    bounds_with_capacities <- function(import_cap, export_cap) {
      import_cap <- as.numeric(rep_len(import_cap, time_slots))
      export_cap <- as.numeric(rep_len(export_cap, time_slots))

      list(
        lb = c(
          G - L - export_cap,
          lb_cumsum,
          0
        ),
        ub = c(
          G - L + import_cap,
          ub_cumsum,
          0
        ),
        import_cap = import_cap,
        export_cap = export_cap
      )
    }

    A <- rbind(A_grid, A_soc, A_energy)
  }

  mode_offset <- n_variables
  battery_mode_idx <- if (has_battery_mode) {
    mode_offset + seq_len(time_slots)
  } else {
    integer()
  }
  grid_mode_idx <- if (has_grid_flows) {
    mode_offset + length(battery_mode_idx) + seq_len(time_slots)
  } else {
    integer()
  }
  total_mode_variables <- length(battery_mode_idx) + length(grid_mode_idx)
  mode_variable_idx <- c(battery_mode_idx, grid_mode_idx)

  branch_factor <- (1 / discharge_eff) - charge_eff

  list(
    time_slots = time_slots,
    n_variables = n_variables,
    power_idx = power_idx,
    delta_idx = delta_idx,
    import_idx = import_idx,
    export_idx = export_idx,
    A = A,
    bounds_with_capacities = bounds_with_capacities,
    P = P_symmetric,
    q = as.numeric(q),
    continuous_lower = continuous_lower,
    continuous_upper = continuous_upper,
    Bc = Bc,
    Bd = Bd,
    G = G,
    L = L,
    Bcap = Bcap,
    SOCmin = SOCmin,
    SOCmax = SOCmax,
    SOCini = SOCini,
    charge_eff = charge_eff,
    discharge_eff = discharge_eff,
    has_grid_flows = has_grid_flows,
    has_battery_mode = has_battery_mode,
    has_grid_mode = has_grid_flows,
    charge_branch_M = Bd * branch_factor,
    discharge_branch_M = Bc * branch_factor,
    total_mode_variables = total_mode_variables,
    battery_mode_idx = battery_mode_idx,
    grid_mode_idx = grid_mode_idx,
    mode_variable_idx = mode_variable_idx
  )
}


battery_solve_window <- function(
  P,
  q,
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
  charge_eff,
  discharge_eff
) {
  solver_data <- battery_build_solver_data(
    P = P,
    q = q,
    G = G,
    L = L,
    Bcap = Bcap,
    Bc = Bc,
    Bd = Bd,
    SOCmin = SOCmin,
    SOCmax = SOCmax,
    SOCini = SOCini,
    charge_eff = charge_eff,
    discharge_eff = discharge_eff
  )
  solve_window_problem <- battery_select_window_solver(solver_data)

  solve_with_capacities <- function(import_cap, export_cap) {
    bounds <- solver_data$bounds_with_capacities(import_cap, export_cap)
    solve_window_problem(solver_data, bounds)
  }

  solution <- solve_with_capacities(import_capacity, export_capacity)
  if (battery_solution_is_acceptable(solution$result)) {
    return(battery_attach_solution(solution$x, solver_data))
  }

  message_once(
    "\u26A0\uFE0F Optimization warning: optimization not feasible in some windows. Removing grid constraints."
  )

  solution <- solve_with_capacities(
    rep(Inf, solver_data$time_slots),
    rep(Inf, solver_data$time_slots)
  )
  if (battery_solution_is_acceptable(solution$result)) {
    return(battery_attach_solution(solution$x, solver_data))
  }

  message_once(paste0(
    "\u26A0\uFE0F Optimization warning: ",
    battery_solver_status_message(solution$result),
    ". Disabling battery for some windows."
  ))
  zero_profile <- rep(0, solver_data$time_slots)
  battery_attach_profile(zero_profile, zero_profile, zero_profile)
}


#' Battery optimal charging/discharging profile
#'
#' See the formulation of the optimization problems in the
#' [documentation website](https://resourcefully-dev.github.io/flextools/).
#'
#' @param opt_data tibble, optimization contextual data.
#' The first column must be named `datetime` (mandatory) containing the
#' date time sequence where the optimization algorithm is applied.
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
#' Optimization objective can be `"grid"` (default), `"cost"` or `"capacity"`, or
#' a number between `0` and `1` to perform combined optimization
#' where `0 == "cost"` and `1 == "grid"`.
#' @param Bcap numeric, capacity of the battery (in kWh)
#' @param Bc numeric, maximum charging power (in kW)
#' @param Bd numeric, maximum discharging power (in kW)
#' @param SOCmin numeric, minimum State-of-Charge of the battery
#' @param SOCmax numeric, maximum State-of-Charge of the battery
#' @param SOCini numeric, required State-of-Charge at the beginning/end of optimization window
#' @param window_days integer, number of days to consider as optimization window.
#' @param window_start_hour integer, starting hour of the optimization window.
#' @param flex_window_hours integer, flexibility window length, in hours.
#' This optional feature lets you apply flexibility only during few hours from the `window_start_hour`.
#' It must be lower than `window_days*24` hours.
#' @param charge_eff numeric, battery charging efficiency (from 0 to 1, default 1).
#' @param discharge_eff numeric, battery discharging efficiency (from 0 to 1, default 1).
#'
#' @return numeric vector
#' @export
#'
#' @importFrom dplyr tibble %>%
#' @importFrom purrr map
#' @importFrom timefully get_time_resolution
#'
#' @examples
#' library(dplyr)
#' opt_data <- flextools::energy_profiles %>%
#'   filter(lubridate::isoweek(datetime) == 18) %>%
#'   rename(
#'     production = "solar",
#'     static = "building",
#'   ) %>%
#'   select(any_of(c(
#'     "datetime", "production", "static", "price_imported", "price_exported"
#'   )))
#'   opt_battery <- opt_data %>%
#'     add_battery_optimization(
#'       opt_objective = 0.5,
#'       Bcap = 50, Bc = 4, Bd = 4,
#'       window_start_hour = 5
#'     )
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
  charge_eff = 1,
  discharge_eff = 1
) {
  if (is.null(opt_data)) {
    stop("Error: `opt_data` parameter is empty.")
  }
  opt_data <- opt_data %>% mutate(flexible = 0)
  opt_data <- check_optimization_data(opt_data, opt_objective)

  if (charge_eff <= 0 || discharge_eff <= 0) {
    stop("Error: efficiencies must be greater than 0")
  }
  if (charge_eff > 1 || discharge_eff > 1) {
    stop("Error: efficiencies must be lower or equal to 1")
  }

  if (Bcap == 0 || Bc == 0 || Bd == 0 || SOCmin == SOCmax) {
    message(
      "\u26A0\uFE0F Optimization warning: battery parameters don't allow optimization."
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

  dttm_seq <- opt_data$datetime
  time_resolution <- get_time_resolution(dttm_seq)
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

  flatten_profiles <- function(results) {
    charge_list <- purrr::map(results, function(x) {
      val <- attr(x, "charge")
      if (is.null(val)) rep(0, length(x)) else val
    })
    discharge_list <- purrr::map(results, function(x) {
      val <- attr(x, "discharge")
      if (is.null(val)) rep(0, length(x)) else val
    })

    list(
      battery = as.numeric(unlist(results)),
      charge = as.numeric(unlist(charge_list)),
      discharge = as.numeric(unlist(discharge_list))
    )
  }

  reset_message_once()

  if (is.numeric(opt_objective)) {
    # The combined formulation should only be used strictly inside (0, 1).
    # At the endpoints it degenerates to the dedicated pure-objective models,
    # which are smaller, faster and numerically more robust.
    if (opt_objective <= 0) {
      opt_objective <- "cost"
    } else if (opt_objective >= 1) {
      opt_objective <- "grid"
    }
  }

  if (opt_objective == "grid") {
    B_windows <- map(
      windows_data,
      ~ minimize_net_power_window_battery(
        G = .x$production,
        L = .x$static,
        Bcap = Bcap * 60 / time_resolution,
        Bc = Bc,
        Bd = Bd,
        SOCmin = SOCmin,
        SOCmax = SOCmax,
        SOCini = SOCini,
        import_capacity = .x$import_capacity,
        export_capacity = .x$export_capacity,
        charge_eff = charge_eff,
        discharge_eff = discharge_eff
      )
    )
  } else if (opt_objective == "capacity") {
    B_windows <- map(
      windows_data,
      ~ curtail_capacity_window_battery(
        G = .x$production,
        L = .x$static,
        Bcap = Bcap * 60 / time_resolution,
        Bc = Bc,
        Bd = Bd,
        SOCmin = SOCmin,
        SOCmax = SOCmax,
        SOCini = SOCini,
        import_capacity = .x$import_capacity,
        export_capacity = .x$export_capacity,
        charge_eff = charge_eff,
        discharge_eff = discharge_eff
      )
    )
  } else if (opt_objective == "cost") {
    B_windows <- map(
      windows_data,
      ~ minimize_cost_window_battery(
        G = .x$production,
        L = .x$static,
        PI = .x$price_imported,
        PE = .x$price_exported,
        PTU = .x$price_turn_up,
        PTD = .x$price_turn_down,
        Bcap = Bcap * 60 / time_resolution,
        Bc = Bc,
        Bd = Bd,
        SOCmin = SOCmin,
        SOCmax = SOCmax,
        SOCini = SOCini,
        import_capacity = .x$import_capacity,
        export_capacity = .x$export_capacity,
        charge_eff = charge_eff,
        discharge_eff = discharge_eff
      )
    )
  } else if (is.numeric(opt_objective)) {
    B_windows <- map(
      windows_data,
      ~ optimize_battery_window(
        G = .x$production,
        L = .x$static,
        PI = .x$price_imported,
        PE = .x$price_exported,
        PTU = .x$price_turn_up,
        PTD = .x$price_turn_down,
        Bcap = Bcap * 60 / time_resolution,
        Bc = Bc,
        Bd = Bd,
        SOCmin = SOCmin,
        SOCmax = SOCmax,
        SOCini = SOCini,
        import_capacity = .x$import_capacity,
        export_capacity = .x$export_capacity,
        w = opt_objective,
        charge_eff = charge_eff,
        discharge_eff = discharge_eff
      )
    )
  } else {
    stop("Error: invalid `opt_objective`")
  }

  profiles <- flatten_profiles(B_windows)
  battery <- profiles$battery
  charge <- profiles$charge
  discharge <- profiles$discharge

  if (length(flex_windows_idxs_seq) == length(dttm_seq)) {
    attr(battery, "charge") <- charge
    attr(battery, "discharge") <- discharge
    return(battery)
  }

  B_flex <- left_join(
    tibble(idx = seq_len(length(dttm_seq))),
    tibble(
      idx = flex_windows_idxs_seq,
      battery = battery,
      charge = charge,
      discharge = discharge
    ),
    by = "idx"
  ) %>%
    arrange(.data$idx)

  B_flex[is.na(B_flex)] <- 0
  battery_full <- B_flex$battery
  attr(battery_full, "charge") <- B_flex$charge
  attr(battery_full, "discharge") <- B_flex$discharge
  battery_full
}


#' Battery optimal charging/discharging profile to minimize grid interaction (just a window)
#'
#' @param G numeric vector, being the renewable generation profile
#' @param L numeric vector, being the load profile
#' @param Bcap numeric, capacity of the battery (NOT in kWh but in energy units according to time resolution)
#' @param Bc numeric, maximum charging power (in kW)
#' @param Bd numeric, maximum discharging power (in kW)
#' @param SOCmin numeric, minimum State-of-Charge of the battery
#' @param SOCmax numeric, maximum State-of-Charge of the battery
#' @param SOCini numeric, required State-of-Charge at the beginning/end of optimization window
#' @param import_capacity numeric or numeric vector, grid maximum import power capacity that will limit the maximum charging power
#' @param export_capacity numeric or numeric vector, grid maximum export power capacity that will limit the maximum discharging power
#' @param charge_eff numeric, battery charging efficiency (from 0 to 1)
#' @param discharge_eff numeric, battery discharging efficiency (from 0 to 1)
#'
#' @importFrom dplyr  %>% tibble mutate summarise_all
#'
#' @return numeric vector
#' @keywords internal
#'
curtail_capacity_window_battery <- function(
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
  charge_eff,
  discharge_eff
) {
  balance_sum <- tibble(
    consumption = L,
    production = G
  ) %>%
    get_energy_balance() %>%
    mutate(
      export_capacity = export_capacity,
      import_capacity = import_capacity,
      exported_over = pmax(.data$exported - .data$export_capacity, 0),
      imported_over = pmax(.data$imported - .data$import_capacity, 0)
    ) %>%
    summarise_all(sum)

  Bcap_curtail <- min(
    max(balance_sum$exported_over, balance_sum$imported_over),
    Bcap
  )

  if (Bcap_curtail == 0) {
    return(rep(0, length(G)))
  }

  minimize_net_power_window_battery(
    G = G,
    L = L,
    Bcap = Bcap_curtail,
    Bc = Bc,
    Bd = Bd,
    SOCmin = SOCmin,
    SOCmax = SOCmax,
    SOCini = SOCini,
    import_capacity = import_capacity,
    export_capacity = export_capacity,
    charge_eff = charge_eff,
    discharge_eff = discharge_eff
  )
}


#' Perform battery optimization (just a window)
#'
#' @param P numeric matrix, optimization objective parameter
#' @param q numeric vector, optimization objective parameter
#' @param G numeric vector, being the renewable generation profile
#' @param L numeric vector, being the load profile
#' @param Bcap numeric, capacity of the battery (NOT in kWh but in energy units according to time resolution)
#' @param Bc numeric, maximum charging power (in kW)
#' @param Bd numeric, maximum discharging power (in kW)
#' @param SOCmin numeric, minimum State-of-Charge of the battery
#' @param SOCmax numeric, maximum State-of-Charge of the battery
#' @param SOCini numeric, required State-of-Charge at the beginning/end of optimization window
#' @param import_capacity numeric vector, grid maximum import power capacity that will limit the maximum charging power
#' @param export_capacity numeric vector, grid maximum export power capacity that will limit the maximum discharging power
#' @param charge_eff numeric, battery charging efficiency (from 0 to 1)
#' @param discharge_eff numeric, battery discharging efficiency (from 0 to 1)
#'
#' @return numeric vector
#' @keywords internal
#'
solve_optimization_battery_window <- function(
  P,
  q,
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
  charge_eff,
  discharge_eff
) {
  battery_solve_window(
    P = P,
    q = q,
    G = G,
    L = L,
    Bcap = Bcap,
    Bc = Bc,
    Bd = Bd,
    SOCmin = SOCmin,
    SOCmax = SOCmax,
    SOCini = SOCini,
    import_capacity = import_capacity,
    export_capacity = export_capacity,
    charge_eff = charge_eff,
    discharge_eff = discharge_eff
  )
}


#' Battery optimal charging/discharging profile to minimize grid interaction (just a window)
#'
#' @param G numeric vector, being the renewable generation profile
#' @param L numeric vector, being the load profile
#' @param Bcap numeric, capacity of the battery (NOT in kWh but in energy units according to time resolution)
#' @param Bc numeric, maximum charging power (in kW)
#' @param Bd numeric, maximum discharging power (in kW)
#' @param SOCmin numeric, minimum State-of-Charge of the battery
#' @param SOCmax numeric, maximum State-of-Charge of the battery
#' @param SOCini numeric, required State-of-Charge at the beginning/end of optimization window
#' @param import_capacity numeric vector, grid maximum import power capacity that will limit the maximum charging power
#' @param export_capacity numeric vector, grid maximum export power capacity that will limit the maximum discharging power
#' @param charge_eff numeric, battery charging efficiency (from 0 to 1)
#' @param discharge_eff numeric, battery discharging efficiency (from 0 to 1)
#'
#' @return numeric vector
#' @keywords internal
#'
minimize_net_power_window_battery <- function(
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
  charge_eff,
  discharge_eff
) {
  time_slots <- length(G)
  zero_square <- matrix(0, nrow = time_slots, ncol = time_slots)
  P <- rbind(
    cbind(2 * diag(time_slots), zero_square),
    cbind(zero_square, zero_square)
  )
  q <- c(2 * (L - G), rep(0, time_slots))

  solve_optimization_battery_window(
    P = P,
    q = q,
    G = G,
    L = L,
    Bcap = Bcap,
    Bc = Bc,
    Bd = Bd,
    SOCmin = SOCmin,
    SOCmax = SOCmax,
    SOCini = SOCini,
    import_capacity = import_capacity,
    export_capacity = export_capacity,
    charge_eff = charge_eff,
    discharge_eff = discharge_eff
  )
}


#' Battery optimal charging/discharging profile to minimize cost (just a window)
#'
#' @param G numeric vector, being the renewable generation profile
#' @param L numeric vector, being the load profile
#' @param PI numeric vector, electricity prices for imported energy
#' @param PE numeric vector, electricity prices for exported energy
#' @param PTD numeric vector, prices for turn-down energy use
#' @param PTU numeric vector, prices for turn-up energy use
#' @param Bcap numeric, capacity of the battery (NOT in kWh but in energy units according to time resolution)
#' @param Bc numeric, maximum charging power (in kW)
#' @param Bd numeric, maximum discharging power (in kW)
#' @param SOCmin numeric, minimum State-of-Charge of the battery
#' @param SOCmax numeric, maximum State-of-Charge of the battery
#' @param SOCini numeric, required State-of-Charge at the beginning/end of optimization window
#' @param import_capacity numeric vector, grid maximum import power capacity that will limit the maximum charging power
#' @param export_capacity numeric vector, grid maximum export power capacity that will limit the maximum discharging power
#' @param charge_eff numeric, battery charging efficiency (from 0 to 1)
#' @param discharge_eff numeric, battery discharging efficiency (from 0 to 1)
#'
#' @return numeric vector
#' @keywords internal
#'
minimize_cost_window_battery <- function(
  G,
  L,
  PE,
  PI,
  PTD,
  PTU,
  Bcap,
  Bc,
  Bd,
  SOCmin,
  SOCmax,
  SOCini,
  import_capacity,
  export_capacity,
  charge_eff,
  discharge_eff
) {
  time_slots <- length(G)
  q_block <- PTD - PTU
  q <- c(
    q_block,
    rep(0, time_slots),
    PI,
    -PE
  )

  solve_optimization_battery_window(
    P = NULL,
    q = q,
    G = G,
    L = L,
    Bcap = Bcap,
    Bc = Bc,
    Bd = Bd,
    SOCmin = SOCmin,
    SOCmax = SOCmax,
    SOCini = SOCini,
    import_capacity = import_capacity,
    export_capacity = export_capacity,
    charge_eff = charge_eff,
    discharge_eff = discharge_eff
  )
}


#' Battery optimal charging/discharging profile to minimize net power and cost (just a window)
#'
#' @param G numeric vector, being the renewable generation profile
#' @param L numeric vector, being the load profile
#' @param PI numeric vector, electricity prices for imported energy
#' @param PE numeric vector, electricity prices for exported energy
#' @param PTD numeric vector, prices for turn-down energy use
#' @param PTU numeric vector, prices for turn-up energy use
#' @param Bcap numeric, capacity of the battery (NOT in kWh but in energy units according to time resolution)
#' @param Bc numeric, maximum charging power (in kW)
#' @param Bd numeric, maximum discharging power (in kW)
#' @param SOCmin numeric, minimum State-of-Charge of the battery
#' @param SOCmax numeric, maximum State-of-Charge of the battery
#' @param SOCini numeric, required State-of-Charge at the beginning/end of optimization window
#' @param import_capacity numeric vector, grid maximum import power capacity that will limit the maximum charging power
#' @param export_capacity numeric vector, grid maximum export power capacity that will limit the maximum discharging power
#' @param w numeric, optimization objective weight (`w=1` minimizes net power while `w=0` minimizes cost)
#' @param charge_eff numeric, battery charging efficiency (from 0 to 1)
#' @param discharge_eff numeric, battery discharging efficiency (from 0 to 1)
#'
#' @return numeric vector
#' @keywords internal
#'
optimize_battery_window <- function(
  G,
  L,
  PE,
  PI,
  PTD,
  PTU,
  Bcap,
  Bc,
  Bd,
  SOCmin,
  SOCmax,
  SOCini,
  import_capacity,
  export_capacity,
  w,
  charge_eff,
  discharge_eff
) {
  time_slots <- length(G)
  P_net <- 2 * w * mean(PI)^2 * diag(time_slots)
  P <- matrix(0, nrow = 4 * time_slots, ncol = 4 * time_slots)
  P[seq_len(time_slots), seq_len(time_slots)] <- P_net
  q_block <- (1 - w) * (PTD - PTU) + 2 * w * mean(PI)^2 * (L - G)
  q <- c(
    q_block,
    rep(0, time_slots),
    (1 - w) * PI,
    -(1 - w) * PE
  )

  solve_optimization_battery_window(
    P = P,
    q = q,
    G = G,
    L = L,
    Bcap = Bcap,
    Bc = Bc,
    Bd = Bd,
    SOCmin = SOCmin,
    SOCmax = SOCmax,
    SOCini = SOCini,
    import_capacity = import_capacity,
    export_capacity = export_capacity,
    charge_eff = charge_eff,
    discharge_eff = discharge_eff
  )
}
