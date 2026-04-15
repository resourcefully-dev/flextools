get_bounds_qp <- function(
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
  cumsumMat <- triangulate_matrix(matrix(1, time_slots, time_slots), 'l')

  ## General bounds
  LFmax_vct <- round(pmin(pmax(G - LS + import_capacity, 0), LFmax), 2)
  lb_O <- round(pmin(pmax(G - LS - export_capacity, 0), LFmax_vct), 2) # Not negative load, but can be positive to avoid exporting

  ## Shifting bounds
  Amat_cumsum <- cumsumMat
  if (direction == 'forward') {
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

    # Cumulative sum bounds
    lb_cumsum <- horizonMat_cumsum %*% LF
    ub_cumsum <- cumsumMat %*% LF

    # Identity bounds
    ub_shift <- horizonMat_identity %*% LF
    ub_O <- pmin(pmax(ub_shift, lb_O), LFmax_vct) # The maximum average power in every time slot is the maximum power of the load `LFmax`
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

    # Cumulative sum bounds
    lb_cumsum <- cumsumMat %*% LF
    ub_cumsum <- horizonMat_cumsum %*% LF

    # Identity bounds
    ub_shift <- horizonMat_identity %*% LF
    ub_O <- pmin(pmax(ub_shift, lb_O), LFmax)
  }

  return(
    list(
      Amat_O = identityMat,
      lb_O = lb_O,
      ub_O = ub_O,
      Amat_cumsum = Amat_cumsum,
      lb_cumsum = lb_cumsum,
      ub_cumsum = ub_cumsum
    )
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
optimize_demand_qp <- function(
  opt_data,
  opt_objective = "grid",
  direction = 'forward',
  time_horizon = NULL,
  window_days = 1,
  window_start_hour = 0,
  flex_window_hours = NULL,
  lambda = 0
) {
  # Parameters check
  opt_data <- check_optimization_data(opt_data, opt_objective)
  if (is.null(opt_data)) {
    stop("Error: `opt_data` parameter is empty.")
  }

  if (((direction != 'forward') && (direction != 'backward'))) {
    stop("Error: `direction` must be 'forward' or 'backward'")
  }

  # Optimization windows
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
      ~ minimize_net_power_window_qp(
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
      ~ minimize_cost_window_qp(
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
      ~ optimize_demand_window_qp(
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
  } else {
    # Create the complete demand vector with the time slots outside the
    # optimization windows
    O_flex <- left_join(
      tibble(idx = seq_len(length(dttm_seq))),
      tibble(
        idx = flex_windows_idxs_seq,
        O = O
      ),
      by = 'idx'
    ) %>%
      arrange(.data$idx)

    O_flex$O[is.na(O_flex$O)] <- opt_data$flexible[is.na(O_flex$O)]
    return(O_flex$O)
  }
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
solve_optimization_window_qp <- function(
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
  # Round to 2 decimals to avoid problems with lower and upper bounds
  G <- round(G, 2)
  LF <- round(LF, 2)
  LS <- round(LS, 2)

  # Optimization parameters
  time_slots <- length(G)
  if (is.null(time_horizon)) {
    time_horizon <- time_slots
  }
  identityMat <- diag(time_slots)

  # Constraints
  L_bounds <- get_bounds_qp(
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
    ## Optimal demand bounds
    ##    0 <= O <= ub (calculated according to time_horizon)
    Amat_O <- cbind(identityMat, identityMat * 0, identityMat * 0)
    lb_O <- L_bounds$lb_O
    ub_O <- L_bounds$ub_O

    ## Imported energy bounds
    ## 0 <= It <= import_capacity
    Amat_I <- cbind(
      identityMat * 0,
      identityMat * 1,
      identityMat * 0
    )
    lb_I <- rep(0, time_slots)
    ub_I <- import_capacity

    ## Exported energy bounds
    ## 0 <= Et <= export_capacity
    Amat_E <- cbind(
      identityMat * 0,
      identityMat * 0,
      identityMat * 1
    )
    lb_E <- rep(0, time_slots)
    ub_E <- export_capacity

    ## Energy balance
    ## It - Et = OLt + LSt - Gt -> OLt - It + Et = Gt - LSt
    Amat_balance <- cbind(
      identityMat * 1,
      identityMat * -1,
      identityMat * 1
    )
    lb_balance <- G - LS
    ub_balance <- G - LS

    ## Energy can only be shifted forwards or backwards with a specific time horizon
    ## This is done through cumulative sum matrices
    Amat_cumsum <- cbind(L_bounds$Amat_cumsum, identityMat * 0, identityMat * 0)
    lb_cumsum <- L_bounds$lb_cumsum
    ub_cumsum <- L_bounds$ub_cumsum

    ## Total sum of O == E
    Amat_energy <- cbind(
      matrix(1, ncol = time_slots),
      matrix(0, ncol = time_slots),
      matrix(0, ncol = time_slots)
    )
    lb_energy <- sum(LF)
    ub_energy <- sum(LF)

    # Join constraints
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
    # Lower and upper bounds
    ## General bounds
    ##  - Grid capacity: -export_capacity <= LF + LS - G <= +import_capacity
    ##    - LB: LF >= G - LS - export_capacity
    ##    - UB: LF <= G - LS + import_capacity
    ##  - Battery power limits:
    ##    - LB: LF >= 0
    ##    - UB: LF <= LFmax
    Amat_O <- L_bounds$Amat_O
    lb_O <- L_bounds$lb_O
    ub_O <- L_bounds$ub_O

    ## Energy can only be shifted forwards or backwards with a specific time horizon
    ## This is done through cumulative sum matrices
    Amat_cumsum <- L_bounds$Amat_cumsum
    lb_cumsum <- L_bounds$lb_cumsum
    ub_cumsum <- L_bounds$ub_cumsum

    ## Total sum of O == E
    Amat_enery <- matrix(1, ncol = time_slots)
    lb_energy <- sum(LF)
    ub_energy <- sum(LF)

    # Join constraints
    Amat <- rbind(Amat_O, Amat_cumsum, Amat_enery)
    lb <- round(c(lb_O, lb_cumsum, lb_energy), 2)
    ub <- round(c(ub_O, ub_cumsum, ub_energy), 2)
  }

  # Solve
  solver <- osqp::osqp(
    P,
    q,
    Amat,
    lb,
    ub,
    osqp::osqpSettings(
      verbose = FALSE,
      eps_abs = 1e-6,
      eps_rel = 1e-6,
      polishing = TRUE
    )
  )
  O <- solver@Solve()

  # Status values: https://osqp.org/docs/interfaces/status_values.html
  # Admit "solved" (1) and "solved inaccurate" (2)
  if (O$info$status_val %in% c(1, 2)) {
    return(round(O$x[seq_len(time_slots)], 2))
  } else {
    # If it's not feasible, then remove grid constraints
    message_once(
      "\u26A0\uFE0F Optimization warning: optimization not feasible in some windows. Removing grid constraints."
    )
    import_capacity <- rep(Inf, time_slots)
    export_capacity <- rep(Inf, time_slots)
    L_bounds <- get_bounds_qp(
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

    solver <- osqp::osqp(
      P,
      q,
      Amat,
      lb,
      ub,
      osqp::osqpSettings(
        verbose = FALSE,
        eps_abs = 1e-6,
        eps_rel = 1e-6,
        polishing = TRUE
      )
    )
    O <- solver@Solve()

    if (O$info$status_val %in% c(1, 2)) {
      return(round(O$x[seq_len(time_slots)], 2))
    } else {
      message_once(paste0(
        "\u26A0\uFE0F Optimization warning: ",
        O$info$status,
        ". No optimization provided."
      ))
      return(LF)
    }
  }
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
minimize_net_power_window_qp <- function(
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

  # Objective function terms
  P <- 2 * identityMat * (1 + lambda)
  q <- 2 * (LS - G - lambda * LF)

  O <- solve_optimization_window_qp(
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

  return(O)
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
minimize_cost_window_qp <- function(
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

  # Objective function terms
  # unknown variable: X = [O, I, E]
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

  O <- solve_optimization_window_qp(
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

  return(O)
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
optimize_demand_window_qp <- function(
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

  # Objective function terms
  # unknown variable: X = [O, I, E]
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

  O <- solve_optimization_window_qp(
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

  return(O)
}
