# Mixed-integer battery optimization backend built on HiGHS.
# Linear objectives are solved directly as MILP problems. Quadratic objectives
# are solved with continuous HiGHS QP relaxations inside a branch-and-bound
# search over the binary battery and grid operating modes.

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
  battery_solver_status_message(result) %in% c(
    "Optimal",
    "Node limit reached",
    "Time limit reached"
  )
}


battery_normalize_quadratic <- function(P, tolerance = 1e-8) {
  optimization_normalize_quadratic(
    P = P,
    tolerance = tolerance,
    problem_name = "battery optimization"
  )
}


battery_build_mode_constraints <- function(solver_data, bounds) {
  time_slots <- solver_data$time_slots
  total_variables <- solver_data$n_variables + solver_data$total_mode_variables

  A_charge_mode <- matrix(0, nrow = time_slots, ncol = total_variables)
  A_charge_mode[cbind(seq_len(time_slots), solver_data$charge_idx)] <- 1
  A_charge_mode[cbind(seq_len(time_slots), solver_data$battery_mode_idx)] <- -solver_data$Bc

  A_discharge_mode <- matrix(0, nrow = time_slots, ncol = total_variables)
  A_discharge_mode[cbind(seq_len(time_slots), solver_data$discharge_idx)] <- 1
  A_discharge_mode[cbind(
    seq_len(time_slots),
    solver_data$battery_mode_idx
  )] <- solver_data$Bd

  A <- rbind(A_charge_mode, A_discharge_mode)
  lhs <- c(rep(-Inf, time_slots), rep(-Inf, time_slots))
  rhs <- c(rep(0, time_slots), rep(solver_data$Bd, time_slots))

  if (solver_data$has_grid_flows) {
    # Grid-mode binaries use the existing import/export formulation and only
    # remove the nonphysical degree of freedom where both can be positive.
    A_import_mode <- matrix(0, nrow = time_slots, ncol = total_variables)
    A_import_mode[cbind(seq_len(time_slots), solver_data$import_idx)] <- 1
    A_import_mode[cbind(seq_len(time_slots), solver_data$grid_mode_idx)] <-
      -bounds$import_mode_ub

    A_export_mode <- matrix(0, nrow = time_slots, ncol = total_variables)
    A_export_mode[cbind(seq_len(time_slots), solver_data$export_idx)] <- 1
    A_export_mode[cbind(seq_len(time_slots), solver_data$grid_mode_idx)] <-
      bounds$export_mode_ub

    A <- rbind(A, A_import_mode, A_export_mode)
    lhs <- c(lhs, rep(-Inf, time_slots), rep(-Inf, time_slots))
    rhs <- c(rhs, rep(0, time_slots), bounds$export_mode_ub)
  }

  list(
    A = A,
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
    lower = c(rep(0, solver_data$n_variables), mode_lower),
    upper = c(rep(Inf, solver_data$n_variables), mode_upper),
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
  x_value[solver_data$mode_variable_idx]
}


battery_round_mode_guess <- function(x_value, solver_data) {
  charge <- x_value[solver_data$charge_idx]
  discharge <- x_value[solver_data$discharge_idx]
  battery_mode <- as.numeric(charge >= discharge)

  if (!solver_data$has_grid_flows) {
    return(battery_mode)
  }

  import <- x_value[solver_data$import_idx]
  export <- x_value[solver_data$export_idx]
  grid_mode <- as.numeric(import >= export)

  c(battery_mode, grid_mode)
}


battery_branch_index <- function(x_value, solver_data, tolerance = 1e-6) {
  charge <- x_value[solver_data$charge_idx]
  discharge <- x_value[solver_data$discharge_idx]
  simultaneous <- which(charge > tolerance & discharge > tolerance)

  if (length(simultaneous) > 0) {
    scores <- pmin(charge[simultaneous], discharge[simultaneous])
    return(simultaneous[which.max(scores)])
  }

  if (solver_data$has_grid_flows) {
    import <- x_value[solver_data$import_idx]
    export <- x_value[solver_data$export_idx]
    simultaneous_grid <- which(import > tolerance & export > tolerance)

    if (length(simultaneous_grid) > 0) {
      scores <- pmin(import[simultaneous_grid], export[simultaneous_grid])
      return(solver_data$time_slots + simultaneous_grid[which.max(scores)])
    }
  }

  z_value <- battery_mode_values(x_value, solver_data)
  fractional <- which(z_value > tolerance & z_value < 1 - tolerance)
  if (length(fractional) == 0) {
    return(NA_integer_)
  }

  scores <- pmin(z_value[fractional], 1 - z_value[fractional])
  fractional[which.max(scores)]
}


battery_has_simultaneous_flows <- function(
  x_value,
  solver_data,
  tolerance = 1e-6
) {
  charge <- x_value[solver_data$charge_idx]
  discharge <- x_value[solver_data$discharge_idx]

  if (any(charge > tolerance & discharge > tolerance)) {
    return(TRUE)
  }

  if (!solver_data$has_grid_flows) {
    return(FALSE)
  }

  import <- x_value[solver_data$import_idx]
  export <- x_value[solver_data$export_idx]
  any(import > tolerance & export > tolerance)
}


battery_attach_solution <- function(x_value, time_slots) {
  charge <- pmax(x_value[seq_len(time_slots)], 0)
  discharge <- pmax(x_value[seq_len(time_slots) + time_slots], 0)
  tolerance <- battery_solution_tolerance()
  charge[charge < tolerance] <- 0
  discharge[discharge < tolerance] <- 0

  battery_attach_profile(charge - discharge, charge, discharge)
}


battery_solve_milp_window <- function(solver_data, bounds) {
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


battery_solve_miqp_window <- function(solver_data, bounds) {
  total_mode_variables <- solver_data$total_mode_variables
  objective_tolerance <- battery_objective_tolerance()
  relative_gap_tolerance <- battery_relative_gap_tolerance()
  node_limit <- battery_branch_and_bound_node_limit()
  time_limit <- battery_branch_and_bound_time_limit_seconds()
  start_elapsed <- proc.time()[["elapsed"]]
  nodes_visited <- 0L

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

  # Most practical windows already satisfy the exact operating modes in the
  # continuous QP relaxation. In those cases we can accept the relaxation
  # directly and avoid the much slower custom branch-and-bound search.
  if (!battery_has_simultaneous_flows(root_relaxation$x, solver_data)) {
    return(root_relaxation)
  }

  best_objective <- Inf
  best_x <- NULL
  stack <- list(list(
    mode_lower = rep(0, total_mode_variables),
    mode_upper = rep(1, total_mode_variables)
  ))

  update_incumbent <- function(mode_fixed) {
    heuristic <- battery_solve_qp_relaxation(
      solver_data = solver_data,
      bounds = bounds,
      mode_lower = mode_fixed,
      mode_upper = mode_fixed
    )

    if (!battery_highs_is_optimal(heuristic$result)) {
      return()
    }

    if (
      heuristic$result$objective_value + objective_tolerance < best_objective
    ) {
      best_objective <<- heuristic$result$objective_value
      best_x <<- heuristic$x
    }
  }

  root_mode_guess <- battery_round_mode_guess(root_relaxation$x, solver_data)
  update_incumbent(root_mode_guess)

  if (
    !is.null(best_x) &&
      battery_objective_gap(
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
        ) <= relative_gap_tolerance
    ) {
      next
    }

    branch_idx <- battery_branch_index(relaxation$x, solver_data)
    if (is.na(branch_idx)) {
      best_objective <- relaxation$result$objective_value
      best_x <- relaxation$x
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
  P_symmetric <- (P + t(P)) / 2
  time_slots <- length(G)
  identityMat <- diag(time_slots)
  cumsumMat <- triangulate_matrix(matrix(1, time_slots, time_slots), "l")

  if (charge_eff <= 0 || discharge_eff <= 0) {
    stop("Error: charge and discharge efficiencies must be positive")
  }

  n_variables <- length(q)
  has_grid_flows <- n_variables > 2 * time_slots

  if (has_grid_flows) {
    zeroMat <- identityMat * 0

    Amat_C <- cbind(identityMat, zeroMat, zeroMat, zeroMat)
    ub_C <- rep(Bc, time_slots)

    Amat_D <- cbind(zeroMat, identityMat, zeroMat, zeroMat)
    ub_D <- rep(Bd, time_slots)

    Amat_I <- cbind(zeroMat, zeroMat, identityMat, zeroMat)
    Amat_E <- cbind(zeroMat, zeroMat, zeroMat, identityMat)

    Amat_balance <- cbind(identityMat, -identityMat, -identityMat, identityMat)
    eq_balance <- G - L

    Amat_cumsum <- cbind(
      charge_eff * cumsumMat,
      -(1 / discharge_eff) * cumsumMat,
      zeroMat,
      zeroMat
    )
    lb_cumsum <- rep((SOCmin - SOCini) / 100 * Bcap, time_slots)
    ub_cumsum <- rep((SOCmax - SOCini) / 100 * Bcap, time_slots)

    Amat_energy <- cbind(
      matrix(charge_eff, nrow = 1, ncol = time_slots),
      matrix(-1 / discharge_eff, nrow = 1, ncol = time_slots),
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
          rep(0, time_slots),
          rep(0, time_slots),
          eq_balance,
          lb_cumsum,
          0
        ),
        ub = c(
          ub_C,
          ub_D,
          import_cap,
          export_cap,
          eq_balance,
          ub_cumsum,
          0
        ),
        import_mode_ub = import_mode_ub,
        export_mode_ub = export_mode_ub
      )
    }

    Amat <- rbind(
      Amat_C,
      Amat_D,
      Amat_I,
      Amat_E,
      Amat_balance,
      Amat_cumsum,
      Amat_energy
    )
  } else {
    zeroMat <- identityMat * 0

    Amat_C <- cbind(identityMat, zeroMat)
    ub_C <- rep(Bc, time_slots)

    Amat_D <- cbind(zeroMat, identityMat)
    ub_D <- rep(Bd, time_slots)

    Amat_grid <- cbind(identityMat, -identityMat)

    Amat_cumsum <- cbind(
      charge_eff * cumsumMat,
      -(1 / discharge_eff) * cumsumMat
    )
    lb_cumsum <- rep((SOCmin - SOCini) / 100 * Bcap, time_slots)
    ub_cumsum <- rep((SOCmax - SOCini) / 100 * Bcap, time_slots)

    Amat_energy <- cbind(
      matrix(charge_eff, nrow = 1, ncol = time_slots),
      matrix(-1 / discharge_eff, nrow = 1, ncol = time_slots)
    )

    bounds_with_capacities <- function(import_cap, export_cap) {
      import_cap <- as.numeric(rep_len(import_cap, time_slots))
      export_cap <- as.numeric(rep_len(export_cap, time_slots))

      list(
        lb = c(
          rep(0, time_slots),
          rep(0, time_slots),
          G - L - export_cap,
          lb_cumsum,
          0
        ),
        ub = c(
          ub_C,
          ub_D,
          G - L + import_cap,
          ub_cumsum,
          0
        )
      )
    }

    Amat <- rbind(Amat_C, Amat_D, Amat_grid, Amat_cumsum, Amat_energy)
  }

  list(
    time_slots = time_slots,
    n_variables = n_variables,
    charge_idx = seq_len(time_slots),
    discharge_idx = seq_len(time_slots) + time_slots,
    import_idx = if (has_grid_flows) seq_len(time_slots) + 2 * time_slots else NULL,
    export_idx = if (has_grid_flows) seq_len(time_slots) + 3 * time_slots else NULL,
    A = Amat,
    bounds_with_capacities = bounds_with_capacities,
    P = battery_normalize_quadratic(P_symmetric),
    q = as.numeric(q),
    Bc = Bc,
    Bd = Bd,
    has_grid_flows = has_grid_flows,
    total_mode_variables = if (has_grid_flows) 2 * time_slots else time_slots,
    battery_mode_idx = n_variables + seq_len(time_slots),
    grid_mode_idx = if (has_grid_flows) {
      n_variables + time_slots + seq_len(time_slots)
    } else {
      integer()
    },
    mode_variable_idx = if (has_grid_flows) {
      n_variables + seq_len(2 * time_slots)
    } else {
      n_variables + seq_len(time_slots)
    }
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
    if (!battery_highs_is_optimal(solution$result)) {
      message_once(
        "\u26A0\uFE0F Optimization warning: battery branch-and-bound limit reached in some windows. Returning best feasible incumbent."
      )
    }
    return(battery_attach_solution(solution$x, solver_data$time_slots))
  }

  message_once(
    "\u26A0\uFE0F Optimization warning: optimization not feasible in some windows. Removing grid constraints."
  )

  solution <- solve_with_capacities(
    rep(Inf, solver_data$time_slots),
    rep(Inf, solver_data$time_slots)
  )
  if (battery_solution_is_acceptable(solution$result)) {
    if (!battery_highs_is_optimal(solution$result)) {
      message_once(
        "\u26A0\uFE0F Optimization warning: battery branch-and-bound limit reached in some windows. Returning best feasible incumbent."
      )
    }
    return(battery_attach_solution(solution$x, solver_data$time_slots))
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
#' @param lambda numeric, penalty on change for the battery compared to the previous time slot.
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
  lambda = 0,
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
        lambda = lambda,
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
        lambda = lambda,
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
        lambda = lambda,
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
        lambda = lambda,
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
#' @param lambda numeric, penalty on change for the flexible load
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
  lambda,
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
    lambda = lambda,
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
#' @param lambda numeric, penalty on change for the flexible load.
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
  lambda,
  charge_eff,
  discharge_eff
) {
  time_slots <- length(G)
  identityMat <- diag(time_slots)
  lambdaMat <- get_lambda_matrix(time_slots)

  penaltyMat <- identityMat + lambda * lambdaMat
  P_block <- 2 * penaltyMat
  P <- rbind(
    cbind(P_block, -P_block),
    cbind(-P_block, P_block)
  )
  q_block <- 2 * (L - G)
  q <- c(q_block, -q_block)

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
#' @param lambda numeric, penalty on change for the flexible load
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
  lambda,
  charge_eff,
  discharge_eff
) {
  time_slots <- length(G)
  lambdaMat <- get_lambda_matrix(time_slots)

  smoothing <- 2 * lambda * lambdaMat
  zero_block <- matrix(0, nrow = 2 * time_slots, ncol = time_slots)
  zero_square <- matrix(0, nrow = time_slots, ncol = time_slots)
  P <- rbind(
    cbind(
      rbind(
        cbind(smoothing, -smoothing),
        cbind(-smoothing, smoothing)
      ),
      zero_block,
      zero_block
    ),
    cbind(
      matrix(0, nrow = time_slots, ncol = 2 * time_slots),
      zero_square,
      zero_square
    ),
    cbind(
      matrix(0, nrow = time_slots, ncol = 2 * time_slots),
      zero_square,
      zero_square
    )
  )
  q_block <- PTD - PTU
  q <- c(
    q_block,
    -q_block,
    PI,
    -PE
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
#' @param lambda numeric, penalty on change for the flexible load
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
  lambda,
  charge_eff,
  discharge_eff
) {
  time_slots <- length(G)
  identityMat <- diag(time_slots)
  lambdaMat <- get_lambda_matrix(time_slots)

  penaltyMat <- 2 * (lambda * lambdaMat + w * mean(PI)^2 * identityMat)
  zero_block <- matrix(0, nrow = 2 * time_slots, ncol = time_slots)
  zero_square <- matrix(0, nrow = time_slots, ncol = time_slots)
  P <- rbind(
    cbind(
      rbind(
        cbind(penaltyMat, -penaltyMat),
        cbind(-penaltyMat, penaltyMat)
      ),
      zero_block,
      zero_block
    ),
    cbind(
      matrix(0, nrow = time_slots, ncol = 2 * time_slots),
      zero_square,
      zero_square
    ),
    cbind(
      matrix(0, nrow = time_slots, ncol = 2 * time_slots),
      zero_square,
      zero_square
    )
  )
  q_block <- (1 - w) * (PTD - PTU) - 2 * w * mean(PI)^2 * (G - L)
  q <- c(
    q_block,
    -q_block,
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
