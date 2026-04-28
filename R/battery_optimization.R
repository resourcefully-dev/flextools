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
    max(balance_sum$exported_over, balance_sum$imported_over),
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


# Cost/combined solvers: X = [B, I, E] ----------------------------------------

battery_cost_build_constraints <- function(
  G,
  L,
  Bcap,
  Bc,
  Bd,
  SOCmin,
  SOCmax,
  SOCini,
  import_capacity,
  export_capacity
) {
  n <- length(G)
  cumsumMat <- triangulate_matrix(matrix(1, n, n), "l")
  zeroMat <- matrix(0, n, n)

  lb_soc <- rep((SOCmin - SOCini) / 100 * Bcap, n)
  ub_soc <- rep((SOCmax - SOCini) / 100 * Bcap, n)

  import_capacity <- as.numeric(rep_len(import_capacity, n))
  export_capacity <- as.numeric(rep_len(export_capacity, n))

  # Physical per-slot mode upper bounds for MILP and OSQP cap
  import_mode_ub <- pmax(L - G + Bc, 0)
  export_mode_ub <- pmax(G - L + Bd, 0)
  import_mode_ub[is.finite(import_capacity)] <- pmin(
    import_mode_ub[is.finite(import_capacity)],
    import_capacity[is.finite(import_capacity)]
  )
  export_mode_ub[is.finite(export_capacity)] <- pmin(
    export_mode_ub[is.finite(export_capacity)],
    export_capacity[is.finite(export_capacity)]
  )

  # X = [B_1..B_n, I_1..I_n, E_1..E_n]
  # Balance: B - I + E = G - L  (equality)
  A_balance <- cbind(diag(n), -diag(n), diag(n))
  # SOC: cumsum(B) in [lb_soc, ub_soc]
  A_soc <- cbind(cumsumMat, zeroMat, zeroMat)
  # Energy: sum(B) = 0
  A_energy <- matrix(c(rep(1, n), rep(0, 2 * n)), nrow = 1)

  A <- rbind(A_balance, A_soc, A_energy)
  lb <- c(G - L, lb_soc, 0)
  ub <- c(G - L, ub_soc, 0)

  list(
    n = n,
    A = A,
    lb = lb,
    ub = ub,
    lb_B = rep(-Bd, n),
    ub_B = rep(Bc, n),
    lb_I = rep(0, n),
    ub_I = import_capacity,
    lb_E = rep(0, n),
    ub_E = export_capacity,
    import_mode_ub = import_mode_ub,
    export_mode_ub = export_mode_ub
  )
}


battery_solve_cost_milp_window <- function(
  G,
  L,
  PI,
  PE,
  P_B,
  q_B,
  Bcap,
  Bc,
  Bd,
  SOCmin,
  SOCmax,
  SOCini,
  import_capacity,
  export_capacity
) {
  G <- round(G, 2)
  L <- round(L, 2)
  d <- battery_cost_build_constraints(
    G,
    L,
    Bcap,
    Bc,
    Bd,
    SOCmin,
    SOCmax,
    SOCini,
    import_capacity,
    export_capacity
  )
  n <- d$n

  # Extend constraint matrix with mode column block
  A_base <- cbind(d$A, matrix(0, nrow(d$A), n))

  # Import mode: I_t <= import_mode_ub_t * m_t
  A_import_mode <- cbind(
    matrix(0, n, n), # B
    diag(n), # I
    matrix(0, n, n), # E
    -diag(d$import_mode_ub) # -M*m
  )
  # Export mode: E_t <= export_mode_ub_t * (1 - m_t)
  A_export_mode <- cbind(
    matrix(0, n, n), # B
    matrix(0, n, n), # I
    diag(n), # E
    diag(d$export_mode_ub) # M*m
  )

  A_full <- rbind(A_base, A_import_mode, A_export_mode)
  lhs_full <- c(d$lb, rep(-Inf, 2 * n))
  rhs_full <- c(d$ub, rep(0, n), d$export_mode_ub)

  Q_full <- if (!is.null(P_B)) {
    Q <- matrix(0, 4 * n, 4 * n)
    Q[seq_len(n), seq_len(n)] <- P_B
    Q
  } else {
    NULL
  }
  L_full <- c(
    if (!is.null(q_B)) q_B else rep(0, n),
    PI,
    -PE,
    rep(0, n)
  )

  result <- highs::highs_solve(
    Q = Q_full,
    L = L_full,
    lower = c(d$lb_B, d$lb_I, d$lb_E, rep(0, n)),
    upper = c(d$ub_B, d$ub_I, d$ub_E, rep(1, n)),
    A = A_full,
    lhs = lhs_full,
    rhs = rhs_full,
    types = c(rep(1L, 3 * n), rep(2L, n)),
    control = optimization_highs_options(include_mip_gap = TRUE)
  )

  if (
    identical(result$status_message, "Optimal") &&
      !is.null(result$primal_solution)
  ) {
    return(as.numeric(result$primal_solution[seq_len(n)]))
  }

  rep(0, n)
}


battery_solve_cost_osqp_window <- function(
  G,
  L,
  PI,
  PE,
  P_B,
  q_B,
  Bcap,
  Bc,
  Bd,
  SOCmin,
  SOCmax,
  SOCini,
  import_capacity,
  export_capacity
) {
  G <- round(G, 2)
  L <- round(L, 2)
  d <- battery_cost_build_constraints(
    G,
    L,
    Bcap,
    Bc,
    Bd,
    SOCmin,
    SOCmax,
    SOCini,
    import_capacity,
    export_capacity
  )
  n <- d$n

  # Clip PE to PI to keep the QP bounded when export price exceeds import price
  PE_clipped <- pmin(PE, PI)
  if (any(PE_clipped != PE)) {
    message_once(
      "\u26a0\ufe0f Optimization: export price exceeds import price; clipping for bounded QP."
    )
  }

  # Quadratic term for X = [B, I, E] (3n x 3n)
  P_full <- Matrix::sparseMatrix(
    i = integer(),
    j = integer(),
    x = numeric(),
    dims = c(3 * n, 3 * n)
  )
  if (!is.null(P_B)) {
    P_full[seq_len(n), seq_len(n)] <- P_B
  }
  P_full <- (P_full + Matrix::t(P_full)) / 2

  q_full <- c(
    if (!is.null(q_B)) q_B else rep(0, n),
    PI,
    -PE_clipped
  )

  # Variable bounds — cap I and E to prevent unbounded QP when PE_clipped == PI
  ub_I <- pmin(d$ub_I, d$import_mode_ub)
  ub_E <- pmin(d$ub_E, d$export_mode_ub)

  # OSQP constraint matrix: identity rows for variable bounds + problem constraints
  A_osqp <- rbind(
    cbind(diag(n), matrix(0, n, 2 * n)), # B identity
    cbind(matrix(0, n, n), diag(n), matrix(0, n, n)), # I identity
    cbind(matrix(0, n, 2 * n), diag(n)), # E identity
    d$A # balance + SOC + energy
  )
  lower_osqp <- c(d$lb_B, d$lb_I, d$lb_E, d$lb)
  upper_osqp <- c(d$ub_B, ub_I, ub_E, d$ub)

  sol <- solve_osqp(P_full, q_full, A_osqp, lower_osqp, upper_osqp)

  if (!is.null(sol$x)) {
    return(as.numeric(sol$x[seq_len(n)]))
  }

  # Fallback: MILP without quadratic term
  message_once(
    "\u26a0\ufe0f Optimization warning: OSQP failed for cost/combined. Falling back to MILP."
  )
  battery_solve_cost_milp_window(
    G,
    L,
    PI,
    PE,
    NULL,
    NULL,
    Bcap,
    Bc,
    Bd,
    SOCmin,
    SOCmax,
    SOCini,
    import_capacity,
    export_capacity
  )
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
  lambda = 0
) {
  n <- length(G)

  if (lambda == 0) {
    battery_solve_cost_milp_window(
      G,
      L,
      PI,
      PE,
      NULL,
      NULL,
      Bcap,
      Bc,
      Bd,
      SOCmin,
      SOCmax,
      SOCini,
      import_capacity,
      export_capacity
    )
  } else {
    lambdaMat <- get_lambda_matrix(n)
    P_B <- 2 * lambda * (lambdaMat + 1e-6 * diag(n))
    battery_solve_cost_osqp_window(
      G,
      L,
      PI,
      PE,
      P_B,
      rep(0, n),
      Bcap,
      Bc,
      Bd,
      SOCmin,
      SOCmax,
      SOCini,
      import_capacity,
      export_capacity
    )
  }
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
  n <- length(G)
  scale <- mean(PI)^2

  # Quadratic grid term on B
  P_grid <- 2 * w * scale * diag(n)
  q_grid <- 2 * w * scale * (L - G)

  if (lambda > 0) {
    lambdaMat <- get_lambda_matrix(n)
    P_B <- P_grid + 2 * lambda * (lambdaMat + 1e-6 * diag(n))
  } else {
    P_B <- P_grid
  }

  # Check if P_B is effectively zero → MILP path
  if (max(abs(P_B)) < 1e-8) {
    battery_solve_cost_milp_window(
      G,
      L,
      PI = (1 - w) * PI,
      PE = (1 - w) * PE,
      P_B = NULL,
      q_B = NULL,
      Bcap,
      Bc,
      Bd,
      SOCmin,
      SOCmax,
      SOCini,
      import_capacity,
      export_capacity
    )
  } else {
    battery_solve_cost_osqp_window(
      G,
      L,
      PI = (1 - w) * PI,
      PE = (1 - w) * PE,
      P_B = P_B,
      q_B = q_grid,
      Bcap,
      Bc,
      Bd,
      SOCmin,
      SOCmax,
      SOCini,
      import_capacity,
      export_capacity
    )
  }
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
  lambda = 0
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
        lambda = lambda
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
