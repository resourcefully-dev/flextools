# General functions -------------------------------------------------------

# These helpers were moved out of `optimization.R` so demand and battery
# optimization can live in separate files without changing the public API.
# The logic is intentionally kept very close to the original implementation.

check_optimization_data <- function(opt_data, opt_objective) {
  # The optimization code relies on a `datetime` index to build windows.
  if (!("datetime" %in% names(opt_data))) {
    stop("Error: `datetime` variable must exist in `opt_data`")
  }

  # The demand optimization path always expects an explicit flexible profile.
  if (!("flexible" %in% names(opt_data))) {
    stop("Error: variable `flexible` must exist in `opt_data`")
  }

  # Static demand is optional, but the solver always works with a value.
  if (!("static" %in% names(opt_data))) {
    opt_data$static <- 0
  }

  # Local production is optional. When it is not present we keep the
  # optimization logic unchanged by substituting a zero-production profile.
  if (!("production" %in% names(opt_data))) {
    warning(
      "`production` variable not found in `opt_data`. No local energy production will be considered."
    )
    opt_data$production <- 0
  }

  # `grid_capacity` is still accepted for backward compatibility and mapped to
  # import/export capacities exactly as before.
  if ("grid_capacity" %in% names(opt_data)) {
    if (!("import_capacity" %in% names(opt_data))) {
      opt_data$import_capacity <- opt_data$grid_capacity
    }
    if (!("export_capacity" %in% names(opt_data))) {
      opt_data$export_capacity <- opt_data$grid_capacity
    }
  } else {
    if (!("import_capacity" %in% names(opt_data))) {
      opt_data$import_capacity <- Inf
    }
    if (!("export_capacity" %in% names(opt_data))) {
      opt_data$export_capacity <- Inf
    }
  }

  # Flexible demand upper limits are optional at the API level, but the
  # solver uses an explicit bound vector internally.
  if (!("load_capacity" %in% names(opt_data))) {
    opt_data$load_capacity <- Inf
  }

  # The public objective contract is unchanged.
  if (
    !(opt_objective %in% c("grid", "cost", "none", "capacity")) &&
      !is.numeric(opt_objective)
  ) {
    stop("Error: `opt_objective` not valid")
  }

  # Cost-based objectives need price vectors. Missing columns are still filled
  # with the same defaults used by the original implementation.
  if (opt_objective == "cost" || is.numeric(opt_objective)) {
    if (!("price_imported" %in% names(opt_data))) {
      warning("`price_imported` variable not found in `opt_data`.")
      opt_data$price_imported <- 1
    }
    if (!("price_exported" %in% names(opt_data))) {
      message("`price_exported` variable not found in `opt_data`.")
      opt_data$price_exported <- 0
    }
    if (!("price_turn_up" %in% names(opt_data))) {
      opt_data$price_turn_up <- 0
    }
    if (!("price_turn_down" %in% names(opt_data))) {
      opt_data$price_turn_down <- 0
    }
  }

  opt_data
}


triangulate_matrix <- function(mat, direction = c("l", "u"), k = 0) {
  # This helper is reused in multiple optimization formulations to build the
  # triangular cumulative-sum operators that implement shifting constraints.
  if (direction == "l") {
    return(as.matrix(Matrix::tril(mat, k = k)))
  } else if (direction == "u") {
    return(as.matrix(Matrix::triu(mat, k = k)))
  }
}


get_lambda_matrix <- function(time_slots) {
  # The lambda matrix penalizes deviations between adjacent time slots. The
  # original algebra is preserved because several objective formulations were
  # derived directly from it.
  identityMat <- diag(time_slots)
  nextMat <- identityMat
  nextMat[1, 1] <- 0
  nextMat[time_slots, time_slots] <- 0
  lambdaMat <- identityMat +
    nextMat -
    triangulate_matrix(
      triangulate_matrix(matrix(1, time_slots, time_slots), "u", 1),
      "l",
      1
    ) -
    triangulate_matrix(
      triangulate_matrix(matrix(1, time_slots, time_slots), "l", -1),
      "u",
      -1
    )

  lambdaMat
}


get_flex_windows <- function(
  dttm_seq,
  window_days,
  window_start_hour,
  flex_window_hours = NULL
) {
  # Window starts are defined by the requested hour at exact hour boundaries.
  start_hour_idx <- which(
    (lubridate::hour(dttm_seq) == window_start_hour) &
      (lubridate::minute(dttm_seq) == 0)
  )
  n_windows <- trunc(length(start_hour_idx) / window_days)

  # Multi-day windows reuse the original grouping logic: consecutive daily
  # starts are collapsed into one larger optimization window.
  if (window_days > 1) {
    window_days_idx <- rep(seq_len(n_windows), each = window_days)
    start_windows_idx <- split(
      start_hour_idx[seq_len(n_windows * window_days)],
      window_days_idx
    ) %>%
      unname() %>%
      purrr::map_int(~ .x[1])
  } else {
    start_windows_idx <- start_hour_idx
  }

  # Window length is the distance to the next start. The last window keeps the
  # same length as the previous one to preserve the original behavior.
  if (n_windows > 1) {
    windows_length <- dplyr::lead(start_windows_idx) - start_windows_idx
    windows_length[is.na(windows_length)] <- windows_length[1]
  } else {
    windows_length <- length(dttm_seq)
  }

  # Optional flexibility windows simply clip the optimization range inside each
  # larger window.
  resolution <- as.numeric(dttm_seq[2] - dttm_seq[1], units = "mins")
  if (is.null(flex_window_hours)) {
    flex_windows_length <- windows_length
  } else {
    if (flex_window_hours > 24 * window_days) {
      message("`flex_window_hours` must be lower than `window_days` hours.")
      flex_window_hours <- 24 * window_days
    }
    flex_window_length <- flex_window_hours * 60 / resolution
    flex_windows_length <- purrr::map_dbl(
      windows_length,
      ~ ifelse(.x < flex_window_length, .x, flex_window_length)
    )
  }

  dplyr::tibble(
    start = start_windows_idx,
    end = start_windows_idx + windows_length - 1,
    flex_end = start_windows_idx + flex_windows_length - 1,
    flex_idx = purrr::map2(.data$start, .data$flex_end, ~ seq(.x, .y))
  ) %>%
    dplyr::filter(.data$end <= length(dttm_seq))
}


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
  # The demand optimization problems are continuous LP/QP problems, so the
  # default single-thread HiGHS solve is enough and keeps runs deterministic.
  highs::highs_control(
    threads = 1L,
    log_to_console = FALSE
  )
}


demand_highs_is_optimal <- function(result) {
  identical(result$status_message, "Optimal")
}


demand_normalize_quadratic <- function(P, tolerance = 1e-8) {
  # HiGHS expects a proper QP Hessian. When the matrix is numerically zero we
  # pass `NULL` so the problem is solved as a plain LP, which is faster and
  # matches the original OSQP formulation with a zero quadratic term.
  if (is.null(P) || !is.matrix(P)) {
    return(NULL)
  }

  P_symmetric <- (P + t(P)) / 2
  if (max(abs(P_symmetric)) <= tolerance) {
    return(NULL)
  }

  eig <- eigen(P_symmetric, symmetric = TRUE, only.values = TRUE)
  if (any(eig$values < -tolerance)) {
    stop("Error: demand optimization objective must be convex")
  }

  P_symmetric
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

  # Build the same physical bounds as before. Only the numerical solver below
  # changes from OSQP to HiGHS.
  L_bounds <- get_bounds(
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

  if (nrow(P) > time_slots) {
    # Unknown variable: X = [O, I, E]
    # `O` is optimized flexible load, `I` imported energy and `E` exported
    # energy. The block structure is left untouched to keep the original model
    # easy to compare with the previous OSQP version.
    Amat_O <- cbind(identityMat, identityMat * 0, identityMat * 0)
    lb_O <- L_bounds$lb_O
    ub_O <- L_bounds$ub_O

    Amat_I <- cbind(
      identityMat * 0,
      identityMat * 1,
      identityMat * 0
    )
    lb_I <- rep(0, time_slots)
    ub_I <- import_capacity

    Amat_E <- cbind(
      identityMat * 0,
      identityMat * 0,
      identityMat * 1
    )
    lb_E <- rep(0, time_slots)
    ub_E <- export_capacity

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
      L_bounds$Amat_cumsum,
      identityMat * 0,
      identityMat * 0
    )
    lb_cumsum <- L_bounds$lb_cumsum
    ub_cumsum <- L_bounds$ub_cumsum

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
    lb <- round(c(lb_O, lb_I, lb_E, lb_balance, lb_cumsum, lb_energy), 2)
    ub <- round(c(ub_O, ub_I, ub_E, ub_balance, ub_cumsum, ub_energy), 2)
  } else {
    # Unknown variable: X = [O]
    # This smaller model is used when the objective does not require explicit
    # import/export variables.
    Amat_O <- L_bounds$Amat_O
    lb_O <- L_bounds$lb_O
    ub_O <- L_bounds$ub_O

    Amat_cumsum <- L_bounds$Amat_cumsum
    lb_cumsum <- L_bounds$lb_cumsum
    ub_cumsum <- L_bounds$ub_cumsum

    Amat_energy <- matrix(1, ncol = time_slots)
    lb_energy <- sum(LF)
    ub_energy <- sum(LF)

    Amat <- rbind(Amat_O, Amat_cumsum, Amat_energy)
    lb <- round(c(lb_O, lb_cumsum, lb_energy), 2)
    ub <- round(c(ub_O, ub_cumsum, ub_energy), 2)
  }

  # First solve: keep the original grid limits.
  O <- demand_solve_highs_problem(P, q, Amat, lb, ub)
  if (demand_highs_is_optimal(O)) {
    return(round(O$primal_solution[seq_len(time_slots)], 2))
  }

  # Fallback solve: if the original grid limits make the problem infeasible,
  # the legacy implementation removed them and retried. The same fallback is
  # preserved here so public behavior remains stable.
  message_once(
    "\u26A0\uFE0F Optimization warning: optimization not feasible in some windows. Removing grid constraints."
  )
  import_capacity <- rep(Inf, time_slots)
  export_capacity <- rep(Inf, time_slots)
  L_bounds <- get_bounds(
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

  if (nrow(P) > time_slots) {
    ub_I <- import_capacity
    ub_E <- export_capacity
    lb <- round(
      c(L_bounds$lb_O, lb_I, lb_E, lb_balance, lb_cumsum, lb_energy),
      2
    )
    ub <- round(
      c(L_bounds$ub_O, ub_I, ub_E, ub_balance, ub_cumsum, ub_energy),
      2
    )
  } else {
    lb <- round(c(L_bounds$lb_O, lb_cumsum, lb_energy), 2)
    ub <- round(c(L_bounds$ub_O, ub_cumsum, ub_energy), 2)
  }

  O <- demand_solve_highs_problem(P, q, Amat, lb, ub)
  if (demand_highs_is_optimal(O)) {
    return(round(O$primal_solution[seq_len(time_slots)], 2))
  }

  message_once(paste0(
    "\u26A0\uFE0F Optimization warning: ",
    O$status_message,
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
