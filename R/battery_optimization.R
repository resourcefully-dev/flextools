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
  target <- G - L

  lb_B <- pmax(-Bd, G - L - export_capacity)
  ub_B <- pmin(Bc, G - L + import_capacity)
  relaxed_bounds <- FALSE

  if (any(lb_B > ub_B + 1e-8)) {
    message_once(
      "\u26a0\ufe0f Optimization warning: infeasible battery QP bounds. Removing grid constraints."
    )
    lb_B <- rep(-Bd, time_slots)
    ub_B <- rep(Bc, time_slots)
    relaxed_bounds <- TRUE
  }

  lb_cumsum <- rep((SOCmin - SOCini) / 100 * Bcap, time_slots)
  ub_cumsum <- rep((SOCmax - SOCini) / 100 * Bcap, time_slots)

  Amat <- rbind(identityMat, cumsumMat, matrix(1, nrow = 1, ncol = time_slots))
  lower <- round(c(lb_B, lb_cumsum, 0), 2)
  upper <- round(c(ub_B, ub_cumsum, 0), 2)
  solution <- battery_solve_osqp(P, q, Amat, lower, upper)

  if (!is.null(solution$profile)) {
    return(solution$profile)
  }

  heuristic <- battery_qp_try_heuristic(
    target = target,
    lower = lb_B,
    upper = ub_B,
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
      solution$result$info$status,
      ". Using heuristic battery profile for some windows."
    ))
    return(heuristic)
  }

  if (!relaxed_bounds) {
    message_once(
      "\u26a0\ufe0f Optimization warning: optimization not feasible for some windows. Removing grid constraints."
    )
    lb_B <- rep(-Bd, time_slots)
    ub_B <- rep(Bc, time_slots)
    lower <- round(c(lb_B, lb_cumsum, 0), 2)
    upper <- round(c(ub_B, ub_cumsum, 0), 2)
    solution <- battery_solve_osqp(P, q, Amat, lower, upper)

    if (!is.null(solution$profile)) {
      return(solution$profile)
    }

    heuristic <- battery_qp_try_heuristic(
      target = target,
      lower = lb_B,
      upper = ub_B,
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
        solution$result$info$status,
        ". Using heuristic battery profile for some windows."
      ))
      return(heuristic)
    }
  }

  message_once(paste0(
    "\u26a0\ufe0f Optimization warning: ",
    solution$result$info$status,
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
    max(balance_sum$exported_over, balance_sum$imported_over) * 1.01, # add 1% headroom to ensure feasibility
    Bcap
  )

  if (Bcap_curtail == 0) {
    return(rep(0, length(G)))
  }

  battery_grid_window(
    G,
    L,
    Bcap_curtail,
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

  import_mode_ub <- pmax(L - G + Bc, 0)
  export_mode_ub <- pmax(G - L + Bd, 0)
  import_mode_ub[import_mode_ub < 1e-9] <- 0
  export_mode_ub[export_mode_ub < 1e-9] <- 0
  import_mode_ub[is.finite(import_capacity)] <- pmin(
    import_mode_ub[is.finite(import_capacity)],
    import_capacity[is.finite(import_capacity)]
  )
  export_mode_ub[is.finite(export_capacity)] <- pmin(
    export_mode_ub[is.finite(export_capacity)],
    export_capacity[is.finite(export_capacity)]
  )

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
      ub_I <- pmin(import_capacity, import_mode_ub)
      ub_E <- pmin(export_capacity, export_mode_ub)

      idx_Bc <- seq_len(n)
      idx_Bd <- seq(n + 1, 2 * n)
      idx_I <- seq(2 * n + 1, 3 * n)
      idx_E <- seq(3 * n + 1, 4 * n)
      idx_S <- seq(4 * n + 1, 5 * n)

      # Sparse P on [B_c, B_d, I, E, S]: quadratic on (B_c - B_d)
      P_5n <- Matrix::sparseMatrix(
        i = integer(),
        j = integer(),
        x = numeric(),
        dims = c(5 * n, 5 * n)
      )
      P_5n[idx_Bc, idx_Bc] <- P_B
      P_5n[idx_Bc, idx_Bd] <- -P_B
      P_5n[idx_Bd, idx_Bc] <- -P_B
      P_5n[idx_Bd, idx_Bd] <- P_B
      P_5n <- (P_5n + Matrix::t(P_5n)) / 2

      # Balance: B_c_t - B_d_t - I_t + E_t = G_t - L_t
      A_bal <- Matrix::sparseMatrix(
        i = rep(seq_len(n), 4),
        j = c(idx_Bc, idx_Bd, idx_I, idx_E),
        x = c(rep(1, n), rep(-1, n), rep(-1, n), rep(1, n)),
        dims = c(n, 5 * n)
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
        dims = c(n, 5 * n)
      )

      # Energy conservation: S_n = 0 (battery returns to initial SOC)
      A_energy_5n <- Matrix::sparseMatrix(
        i = 1L,
        j = idx_S[n],
        x = 1.0,
        dims = c(1, 5 * n)
      )

      A_osqp <- rbind(
        Matrix::Diagonal(n = 5 * n), # variable bounds
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
        G - L,
        rep(0, n),
        0
      )

      # Normalize objective so OSQP's ADMM rho is well-matched regardless of w.
      q_osqp <- c(q_Bc, rep(cycle_coef, n) + q_Bd, PI, -PE_clipped, rep(0, n))
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

    result <- highs::highs_solve(
      Q = NULL,
      L = c(q_Bc, rep(cycle_coef, n) + q_Bd, PI, -PE),
      lower = c(rep(0, n), rep(0, n), rep(0, n), rep(0, n)),
      upper = c(rep(Bc, n), rep(Bd, n), pmin(import_capacity, import_mode_ub), pmin(export_capacity, export_mode_ub)),
      A = A_vars,
      lhs = lb_vars,
      rhs = ub_vars,
      types = rep(1L, 4 * n),
      control = optimization_highs_options()
    )
  } else {
    # MILP path \u2014 binary mode variable m prevents simultaneous charge+discharge.
    A_base <- cbind(A_vars, matrix(0, nrow(A_vars), n))
    # Import mode: I_t <= import_mode_ub_t * m_t
    A_import_mode <- cbind(
      matrix(0, n, n), # B_c
      matrix(0, n, n), # B_d
      diag(n), # I
      matrix(0, n, n), # E
      -diag(import_mode_ub) # -M_I * m
    )
    # Export mode: E_t <= export_mode_ub_t * (1 - m_t)
    A_export_mode <- cbind(
      matrix(0, n, n), # B_c
      matrix(0, n, n), # B_d
      matrix(0, n, n), # I
      diag(n), # E
      diag(export_mode_ub) # M_E * m
    )
    A_full <- rbind(A_base, A_import_mode, A_export_mode)
    lhs_full <- c(lb_vars, rep(-Inf, 2 * n))
    rhs_full <- c(ub_vars, rep(0, n), export_mode_ub)

    result <- highs::highs_solve(
      Q = NULL,
      L = c(rep(0, n), rep(0, n), PI, -PE, rep(0, n)),
      lower = c(rep(0, n), rep(0, n), rep(0, n), rep(0, n), rep(0, n)),
      upper = c(
        rep(Bc, n),
        rep(Bd, n),
        import_capacity,
        export_capacity,
        rep(1, n)
      ),
      A = A_full,
      lhs = lhs_full,
      rhs = rhs_full,
      types = c(rep(1L, 4 * n), rep(2L, n)),
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

  ub_I <- pmin(import_capacity, pmax(L - G + Bc, 0))
  ub_E <- pmin(export_capacity, pmax(G - L + Bd, 0))

  cumsumMat <- triangulate_matrix(matrix(1, n, n), "l")
  zeroMat <- matrix(0, n, n)

  # Sparse P on [B, I, E]: quadratic only on B block (O(n) nnz)
  P_3n <- Matrix::sparseMatrix(
    i = integer(),
    j = integer(),
    x = numeric(),
    dims = c(3 * n, 3 * n)
  )
  P_3n[seq_len(n), seq_len(n)] <- P_B
  P_3n <- (P_3n + Matrix::t(P_3n)) / 2

  # OSQP constraint matrix: variable bounds + balance + SOC + energy conservation
  A_osqp <- rbind(
    cbind(diag(n), zeroMat, zeroMat),
    cbind(zeroMat, diag(n), zeroMat),
    cbind(zeroMat, zeroMat, diag(n)),
    cbind(diag(n), -diag(n), diag(n)),
    cbind(cumsumMat, zeroMat, zeroMat),
    matrix(c(rep(1, n), rep(0, 2 * n)), nrow = 1)
  )
  lower_osqp <- c(rep(-Bd, n), rep(0, n), rep(0, n), G - L, lb_soc, 0)
  upper_osqp <- c(rep(Bc, n), ub_I, ub_E, G - L, ub_soc, 0)

  sol <- solve_osqp(
    P_3n,
    c(q_grid, (1 - w) * PI, -(1 - w) * PE_clipped),
    A_osqp,
    lower_osqp,
    upper_osqp
  )

  if (!is.null(sol$x)) {
    return(round(sol$x[seq_len(n)], 10))
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
#' @param opt_objective character or numeric.
#' `"grid"` (default), `"capacity"`, `"cost"`, or a numeric weight `w`
#' where `w=1` is pure grid and `w=0` is pure cost.
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
