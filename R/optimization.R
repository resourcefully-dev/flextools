
# General functions -------------------------------------------------------

check_optimization_data <- function(opt_data, opt_objective) {
  if (!("datetime" %in% names(opt_data))) {
    stop("Error: `datetime` variable must exist in `opt_data`")
  }
  if (!("flexible" %in% names(opt_data))) {
    stop("Error: variable `flexible` must exist in `opt_data`")
  }
  if (!("static" %in% names(opt_data))) {
    opt_data$static <- 0
  }
  if (!("production" %in% names(opt_data))) {
    message("Warning: `production` variable not found in `opt_data`. No local energy production will be considered.")
    opt_data$production <- 0
  }

  if ("grid_capacity" %in% names(opt_data)) {
    # opt_data$grid_capacity <- Inf
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

  if (!("load_capacity" %in% names(opt_data))) {
    opt_data$load_capacity <- Inf
  }

  if (!(opt_objective %in% c("grid", "cost", "none", "curtail")) && !is.numeric(opt_objective)) {
    stop("Error: `opt_objective` not valid")
  }

  if (opt_objective == "cost" || is.numeric(opt_objective)) {
    if (!("price_imported" %in% names(opt_data))) {
      message("Warning: `price_imported` variable not found in `opt_data`.")
      opt_data$price_imported <- 1
    }
    if (!("price_exported" %in% names(opt_data))) {
      message("Warning: `price_exported` variable not found in `opt_data`.")
      opt_data$price_exported <- 0
    }
    if (!("price_turn_up" %in% names(opt_data))) {
      opt_data$price_turn_up <- 0
    }
    if (!("price_turn_down" %in% names(opt_data))) {
      opt_data$price_turn_down <- 0
    }
  }
  return( opt_data )
}


#' Add an extra day at the beginning and the end of datetime sequence
#' using the last and first day of the data
#'
#' @param df data frame, first column named `datetime` of type `datetime`
#'
#' @return tibble
#' @export
#'
#' @importFrom dplyr filter %>% bind_rows arrange
#' @importFrom lubridate date days
#'
add_extra_days <- function(df) {
  first_day <- df %>%
    filter(date(.data$datetime) == min(date(.data$datetime)))
  first_day$datetime <- first_day$datetime - days(1)
  last_day <- df %>%
    filter(date(.data$datetime) == max(date(.data$datetime)))
  last_day$datetime <- last_day$datetime +days(1)

  bind_rows(
    first_day, df, last_day
  ) %>%
    arrange(.data$datetime)
}

triangulate_matrix <- function(mat, direction = c('l', 'u'), k=0) {
  if (direction == 'l') {
    return( as.matrix(Matrix::tril(mat, k = k)) )
  } else if (direction == 'u') {
    return( as.matrix(Matrix::triu(mat, k = k)) )
  }
}

get_lambda_matrix <- function(time_slots) {
  identityMat <- diag(time_slots)
  nextMat <- identityMat
  nextMat[1, 1] <- 0
  nextMat[time_slots, time_slots] <- 0
  lambdaMat <- identityMat + nextMat -
    triangulate_matrix(
      triangulate_matrix(matrix(1, time_slots, time_slots), "u", 1), "l", 1
    ) -
    triangulate_matrix(
      triangulate_matrix(matrix(1, time_slots, time_slots), "l", -1), "u", -1
    )
  return( lambdaMat )
}


get_flex_windows <- function(dttm_seq, window_days, window_start_hour, flex_window_hours = NULL) {

  # Flexibility windows according to `window_start_hour` and `windows_days`
  start_hour_idx <- which(
    (lubridate::hour(dttm_seq) == window_start_hour) &
      (lubridate::minute(dttm_seq) == 0)
  )
  n_windows <- trunc(length(start_hour_idx)/window_days)

  if (window_days > 1) {
    window_days_idx <- rep(seq_len(n_windows), each = window_days)
    start_windows_idx <- split(
      start_hour_idx[seq_len(n_windows*window_days)], window_days_idx
    ) %>%
      unname() %>%
      purrr::map_int(~ .x[1])
  } else {
    start_windows_idx <- start_hour_idx
  }

  if (n_windows > 1) {
    windows_length <- dplyr::lead(start_windows_idx) - start_windows_idx
    windows_length[is.na(windows_length)] <- windows_length[1] # Fill last NA produced by `lead`
  } else {
    windows_length <- length(dttm_seq)
  }

  # Flexibility windows according to `flex_window_hours`
  resolution <- as.numeric(dttm_seq[2] - dttm_seq[1], units = "mins")

  if (is.null(flex_window_hours)) {
    flex_windows_length <- windows_length
  } else {
    if (flex_window_hours > 24 * window_days) {
      message("Warning: `flex_window_hours` must be lower than `window_days` hours.")
      flex_window_hours <- 24 * window_days
    }
    flex_window_length <- flex_window_hours * 60 / resolution
    flex_windows_length <- purrr::map_dbl(
      windows_length,
      ~ ifelse(.x < flex_window_length, .x, flex_window_length)
    )
  }

  flex_windows_idxs <- dplyr::tibble(
    start = start_windows_idx,
    end = start_windows_idx + windows_length - 1,
    flex_end = start_windows_idx + flex_windows_length - 1,
    flex_idx = purrr::map2(.data$start, .data$flex_end, ~ seq(.x, .y))
  ) %>%
    dplyr::filter(.data$end <= length(dttm_seq))

  return(flex_windows_idxs)
}


get_bounds <- function(time_slots, G, LF, LS, direction, time_horizon, LFmax, import_capacity, export_capacity) {

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
      horizonMat_cumsum <- triangulate_matrix(matrix(1, time_slots, time_slots), "l", -time_horizon)
    }
    horizonMat_identity <- triangulate_matrix(triangulate_matrix(matrix(1, time_slots, time_slots), "l"), "u", -time_horizon)

    # Cumulative sum bounds
    lb_cumsum <- horizonMat_cumsum %*% LF
    ub_cumsum <- cumsumMat %*% LF

    # Identity bounds
    ub_shift <- horizonMat_identity %*% LF
    ub_O <- pmin(pmax(ub_shift, lb_O), LFmax_vct) # The maximum average power in every time slot is the maximum power of the load `LFmax`

  } else {
    horizonMat_cumsum <- triangulate_matrix(matrix(1, time_slots, time_slots), "l", time_horizon)
    horizonMat_identity <- triangulate_matrix(triangulate_matrix(matrix(1, time_slots, time_slots), "u"), "l", time_horizon)

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
optimize_demand <- function(opt_data, opt_objective = "grid",
                            direction = 'forward', time_horizon = NULL,
                            window_days = 1, window_start_hour = 0,
                            flex_window_hours = NULL, lambda = 0) {
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

  # Optimization
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
    return( O )
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
    return( O_flex$O )
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
solve_optimization_window <- function (G, LF, LS, direction, time_horizon, LFmax, import_capacity, export_capacity, P, q) {

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
  L_bounds <- get_bounds(
    time_slots, G, LF, LS, direction, time_horizon, LFmax, import_capacity, export_capacity
  )

  if (nrow(P) > time_slots) {

    ## Optimal demand bounds
    ##    0 <= O <= ub (calculated according to time_horizon)
    Amat_O <- cbind(identityMat, identityMat*0, identityMat*0)
    lb_O <- L_bounds$lb_O
    ub_O <- L_bounds$ub_O

    ## Imported energy bounds
    ## 0 <= It <= import_capacity
    Amat_I <- cbind(
      identityMat*0, identityMat*1, identityMat*0
    )
    lb_I <- rep(0, time_slots)
    ub_I <- import_capacity

    ## Exported energy bounds
    ## 0 <= Et <= export_capacity
    Amat_E <- cbind(
      identityMat*0, identityMat*0, identityMat*1
    )
    lb_E <- rep(0, time_slots)
    ub_E <- export_capacity

    ## Energy balance
    ## It - Et = OLt + LSt - Gt -> OLt - It + Et = Gt - LSt
    Amat_balance <- cbind(
      identityMat*1, identityMat*-1, identityMat*1
    )
    lb_balance <- G - LS
    ub_balance <- G - LS

    ## Energy can only be shifted forwards or backwards with a specific time horizon
    ## This is done through cumulative sum matrices
    Amat_cumsum <- cbind(L_bounds$Amat_cumsum, identityMat*0, identityMat*0)
    lb_cumsum <- L_bounds$lb_cumsum
    ub_cumsum <- L_bounds$ub_cumsum

    ## Total sum of O == E
    Amat_energy <- cbind(matrix(1, ncol = time_slots), matrix(0, ncol = time_slots), matrix(0, ncol = time_slots))
    lb_energy <- sum(LF)
    ub_energy <- sum(LF)

    # Join constraints
    Amat <- rbind(Amat_O, Amat_I, Amat_E, Amat_balance, Amat_cumsum, Amat_energy)
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
    P, q, Amat, lb, ub,
    osqp::osqpSettings(
      verbose = FALSE,
      eps_abs = 1e-6,
      eps_rel = 1e-6,
      polish = TRUE
    )
  )
  O <- solver$Solve()

  # Status values: https://osqp.org/docs/interfaces/status_values.html
  # Admit "solved" (1) and "solved inaccurate" (2)
  if (O$info$status_val %in% c(1, 2)) {
    return( round(O$x[seq_len(time_slots)], 2) )
  } else {
    # If it's not feasible, then remove grid constraints
    message_once("\u26A0\uFE0F Optimization warning: optimization not feasible in some windows. Removing grid constraints.")
    import_capacity <- rep(Inf, time_slots)
    export_capacity <- rep(Inf, time_slots)
    L_bounds <- get_bounds(
      time_slots, G, LF, LS, direction, time_horizon, LFmax, import_capacity, export_capacity
    )

    if (nrow(P) > time_slots) {
      ub_I <- import_capacity
      ub_E <- export_capacity
      lb <- round(c(L_bounds$lb_O, lb_I, lb_E, lb_balance, lb_cumsum, lb_energy), 2)
      ub <- round(c(L_bounds$ub_O, ub_I, ub_E, ub_balance, ub_cumsum, ub_energy), 2)
    } else {
      lb <- round(c(L_bounds$lb_O, lb_cumsum, lb_energy), 2)
      ub <- round(c(L_bounds$ub_O, ub_cumsum, ub_energy), 2)
    }

    solver <- osqp::osqp(
      P, q, Amat, lb, ub,
      osqp::osqpSettings(
        verbose = FALSE,
        eps_abs = 1e-6,
        eps_rel = 1e-6,
        polish = TRUE
      )
    )
    O <- solver$Solve()

    if (O$info$status_val %in% c(1, 2)) {
      return( round(O$x[seq_len(time_slots)], 2) )
    } else {
      message_once(paste0("\u26A0\uFE0F Optimization warning: ", O$info$status, ". No optimization provided."))
      return( LF )
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
minimize_net_power_window <- function (G, LF, LS, direction, time_horizon, LFmax, import_capacity, export_capacity, lambda=0) {

  time_slots <- length(LF)
  identityMat <- diag(time_slots)

  # Objective function terms
  P <- 2*identityMat*(1+lambda)
  q <- 2*(LS - G - lambda*LF)

  O <- solve_optimization_window(
    G, LF, LS, direction, time_horizon, LFmax, import_capacity, export_capacity, P, q
  )

  return( O )
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
minimize_cost_window <- function (G, LF, LS, PI, PE, PTD, PTU, direction, time_horizon, LFmax, import_capacity, export_capacity, lambda=0) {

  time_slots <- length(LF)
  identityMat <- diag(time_slots)

  # Objective function terms
  # unknown variable: X = [O, I, E]
  P <- rbind(
    cbind(
      2*lambda*identityMat, identityMat*0, identityMat*0
    ),
    cbind(
      identityMat*0, identityMat*0, identityMat*0
    ),
    cbind(
      identityMat*0, identityMat*0, identityMat*0
    )
  )
  q <- c(
    PTD - PTU - 2*lambda*LF, PI, -PE
  )

  O <- solve_optimization_window(
    G, LF, LS, direction, time_horizon, LFmax, import_capacity, export_capacity, P, q
  )

  return( O )
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
optimize_demand_window <- function (G, LF, LS, PI, PE, PTD, PTU, direction, time_horizon, LFmax, import_capacity, export_capacity, w, lambda) {

  time_slots <- length(LF)
  identityMat <- diag(time_slots)

  # Objective function terms
  # unknown variable: X = [O, I, E]
  P <- rbind(
    cbind(
      2*(w*mean(PI)^2+lambda)*identityMat, identityMat*0, identityMat*0
    ),
    cbind(
      identityMat*0, identityMat*0, identityMat*0
    ),
    cbind(
      identityMat*0, identityMat*0, identityMat*0
    )
  )
  q <- c(
    (1-w)*(PTD-PTU) - 2*lambda*LF - 2*w*mean(PI)^2*(G-LS), (1-w)*PI, -(1-w)*PE
  )

  O <- solve_optimization_window(
    G, LF, LS, direction, time_horizon, LFmax, import_capacity, export_capacity, P, q
  )

  return( O )
}






# Battery optimization ------------------------------------------------------------

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
#' Optimization objective can be `"grid"` (default), `"cost"` or `"curtail"`, or
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
#'
#' @examples
#' library(dplyr)
#' opt_data <- flextools::energy_profiles %>%
#'   filter(lubridate::isoweek(datetime) == 18) %>%
#'   rename(
#'     production = "solar"
#'   ) %>%
#'   select(any_of(c(
#'     "datetime", "production", "building", "price_imported", "price_exported"
#'   ))) %>%
#'   mutate(
#'     static = .data$building
#'   )
#'   opt_battery <- opt_data %>%
#'     add_battery_optimization(
#'       opt_objective = 0.5,
#'       Bcap = 50, Bc = 4, Bd = 4,
#'       window_start_hour = 5
#'     )
#'
add_battery_optimization <- function(opt_data, opt_objective = "grid", Bcap, Bc, Bd,
                                     SOCmin = 0, SOCmax = 100, SOCini = NULL,
                                     window_days = 1, window_start_hour = 0,
                                     flex_window_hours = 24, lambda = 0,
                                     charge_eff = 1, discharge_eff = 1) {

  # Parameters check
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
    message("\u26A0\uFE0F Warning: battery parameters don't allow optimization.")
    return( rep(0, nrow(opt_data)) )
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

  # Optimization windows
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

  # Optimization
  reset_message_once()

  if (opt_objective == "grid") {

    B_windows <- map(
      windows_data,
      ~ minimize_net_power_window_battery(
        G = .x$production, L = .x$static,
        Bcap = Bcap*60/time_resolution, Bc = Bc, Bd = Bd,
        SOCmin = SOCmin, SOCmax = SOCmax, SOCini = SOCini,
        import_capacity = .x$import_capacity,
        export_capacity = .x$export_capacity,
        lambda = lambda, charge_eff = charge_eff,
        discharge_eff = discharge_eff
      )
    )

  } else if (opt_objective == "curtail") {

    B_windows <- map(
      windows_data,
      ~ curtail_capacity_window_battery(
        G = .x$production, L = .x$static,
        Bcap = Bcap*60/time_resolution, Bc = Bc, Bd = Bd,
        SOCmin = SOCmin, SOCmax = SOCmax, SOCini = SOCini,
        import_capacity = .x$import_capacity,
        export_capacity = .x$export_capacity,
        lambda = lambda, charge_eff = charge_eff,
        discharge_eff = discharge_eff
      )
    )

  } else if (opt_objective == "cost") {

    B_windows <- map(
      windows_data,
      ~ minimize_cost_window_battery(
        G = .x$production, L = .x$static,
        PI = .x$price_imported, PE = .x$price_exported,
        PTU = .x$price_turn_up, PTD = .x$price_turn_down,
        Bcap = Bcap*60/time_resolution, Bc = Bc, Bd = Bd,
        SOCmin = SOCmin, SOCmax = SOCmax, SOCini = SOCini,
        import_capacity = .x$import_capacity,
        export_capacity = .x$export_capacity,
        lambda = lambda, charge_eff = charge_eff,
        discharge_eff = discharge_eff
      )
    )

  } else if (is.numeric(opt_objective)) {

    B_windows <- map(
      windows_data,
      ~ optimize_battery_window(
        G = .x$production, L = .x$static,
        PI = .x$price_imported, PE = .x$price_exported,
        PTU = .x$price_turn_up, PTD = .x$price_turn_down,
        Bcap = Bcap*60/time_resolution, Bc = Bc, Bd = Bd,
        SOCmin = SOCmin, SOCmax = SOCmax, SOCini = SOCini,
        import_capacity = .x$import_capacity,
        export_capacity = .x$export_capacity,
        w = opt_objective, lambda = lambda,
        charge_eff = charge_eff, discharge_eff = discharge_eff
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
  } else {
    # Create the complete battery vector with the time slots outside the
    # optimization windows
    B_flex <- left_join(
      tibble(idx = seq_len(length(dttm_seq))),
      tibble(
        idx = flex_windows_idxs_seq,
        battery = battery,
        charge = charge,
        discharge = discharge
      ),
      by = 'idx'
    ) %>%
      arrange(.data$idx)

    B_flex[is.na(B_flex)] <- 0
    battery_full <- B_flex$battery
    attr(battery_full, "charge") <- B_flex$charge
    attr(battery_full, "discharge") <- B_flex$discharge
    return(battery_full)
  }
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
curtail_capacity_window_battery <- function (G, L, Bcap, Bc, Bd, SOCmin, SOCmax, SOCini, import_capacity, export_capacity, lambda, charge_eff, discharge_eff) {
  balance_sum <- tibble(
    consumption = L, production = G
  ) %>%
    get_energy_balance() %>%
    mutate(
      export_capacity = export_capacity,
      import_capacity = import_capacity,
      exported_over = pmax(.data$exported - .data$export_capacity, 0),
      imported_over = pmax(.data$imported - .data$import_capacity, 0)
    ) %>%
    summarise_all(sum)

  Bcap_curtail <- min(max(balance_sum$exported_over, balance_sum$imported_over), Bcap)

  if (Bcap_curtail == 0) {
    return(rep(0, length(G)))
  } else {
    minimize_net_power_window_battery(
      G, L, Bcap_curtail, Bc, Bd, SOCmin, SOCmax, SOCini, import_capacity, export_capacity, lambda, charge_eff, discharge_eff
    )
  }
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
solve_optimization_battery_window <- function (P, q, G, L, Bcap, Bc, Bd, SOCmin, SOCmax, SOCini, import_capacity, export_capacity, charge_eff, discharge_eff) {

  # Optimization parameters
  time_slots <- length(G)
  identityMat <- diag(time_slots)
  cumsumMat <- triangulate_matrix(matrix(1, time_slots, time_slots), 'l')

  if (charge_eff <= 0 || discharge_eff <= 0) {
    stop("Error: charge and discharge efficiencies must be positive")
  }

  attach_profile <- function(battery, charge, discharge) {
    attr(battery, "charge") <- as.numeric(charge)
    attr(battery, "discharge") <- as.numeric(discharge)
    battery
  }

  # Determine variable layout
  n_variables <- length(q)
  has_grid_flows <- n_variables > 2 * time_slots

  # If has grid flows is cost or combined optimization
  if (has_grid_flows) {

    zeroMat <- identityMat * 0

    # Constraints
    ## Charging power bounds: 0 <= C <= Bc
    Amat_C <- cbind(identityMat, zeroMat, zeroMat, zeroMat)
    ub_C <- rep(Bc, time_slots)

    ## Discharging power bounds: 0 <= D <= Bd
    Amat_D <- cbind(zeroMat, identityMat, zeroMat, zeroMat)
    ub_D <- rep(Bd, time_slots)

    ## Imported energy bounds: 0 <= It <= import_capacity
    Amat_I <- cbind(zeroMat, zeroMat, identityMat, zeroMat)

    ## Exported energy bounds: 0 <= Et <= export_capacity
    Amat_E <- cbind(zeroMat, zeroMat, zeroMat, identityMat)

    ## Energy balance: Ct - Dt - It + Et = Gt - Lt
    Amat_balance <- cbind(identityMat, -identityMat, -identityMat, identityMat)
    eq_balance <- G - L

    ## SOC limits including efficiencies
    Amat_cumsum <- cbind(charge_eff * cumsumMat, -(1 / discharge_eff) * cumsumMat, zeroMat, zeroMat)
    lb_cumsum <- rep((SOCmin - SOCini)/100*Bcap, time_slots)
    ub_cumsum <- rep((SOCmax - SOCini)/100*Bcap, time_slots)

    ## Total sum of SOC variation == 0
    Amat_energy <- cbind(
      matrix(charge_eff, nrow = 1, ncol = time_slots),
      matrix(-1 / discharge_eff, nrow = 1, ncol = time_slots),
      matrix(0, nrow = 1, ncol = time_slots),
      matrix(0, nrow = 1, ncol = time_slots)
    )

    bounds_with_capacities <- function(import_cap, export_cap) {
      lb_I <- rep(0, time_slots)
      ub_I <- import_cap
      lb_E <- rep(0, time_slots)
      ub_E <- export_cap
      lb_energy <- 0
      ub_energy <- 0

      lb <- c(
        rep(0, time_slots),           # charge lower
        rep(0, time_slots),           # discharge lower
        lb_I,
        lb_E,
        eq_balance,
        lb_cumsum,
        lb_energy
      )

      ub <- c(
        ub_C,
        ub_D,
        ub_I,
        ub_E,
        eq_balance,
        ub_cumsum,
        ub_energy
      )

      list(
        lb = lb,
        ub = ub
      )
    }

    Amat <- rbind(Amat_C, Amat_D, Amat_I, Amat_E, Amat_balance, Amat_cumsum, Amat_energy)
    bounds <- bounds_with_capacities(import_capacity, export_capacity)
    lb <- bounds$lb
    ub <- bounds$ub

  } else {

    zeroMat <- identityMat * 0

    # Constraints for variables [C, D]
    Amat_C <- cbind(identityMat, zeroMat)
    ub_C <- rep(Bc, time_slots)

    Amat_D <- cbind(zeroMat, identityMat)
    ub_D <- rep(Bd, time_slots)

    # Grid capacity limits applied to net battery exchange
    Amat_grid <- cbind(identityMat, -identityMat)

    # SOC limits with efficiencies
    Amat_cumsum <- cbind(charge_eff * cumsumMat, -(1 / discharge_eff) * cumsumMat)
    lb_cumsum <- rep((SOCmin - SOCini)/100*Bcap, time_slots)
    ub_cumsum <- rep((SOCmax - SOCini)/100*Bcap, time_slots)

    # Neutral SOC at the end of the window
    Amat_energy <- cbind(
      matrix(charge_eff, nrow = 1, ncol = time_slots),
      matrix(-1 / discharge_eff, nrow = 1, ncol = time_slots)
    )

    bounds_with_capacities <- function(import_cap, export_cap) {
      ##  Grid capacity: -export_capacity <= B + L - G <= +import_capacity
      ##    - LB: B >= G - L - export_capacity
      ##    - UB: B <= G - L + import_capacity
      lb_grid <- G - L - export_cap
      ub_grid <- G - L + import_cap
      eq_energy <- 0

      lb <- c(
        rep(0, time_slots),           # charge
        rep(0, time_slots),           # discharge
        lb_grid,
        lb_cumsum,
        eq_energy
      )

      ub <- c(
        ub_C,
        ub_D,
        ub_grid,
        ub_cumsum,
        eq_energy
      )

      list(
        lb = lb,
        ub = ub
      )
    }

    Amat <- rbind(Amat_C, Amat_D, Amat_grid, Amat_cumsum, Amat_energy)
    bounds <- bounds_with_capacities(import_capacity, export_capacity)
    lb <- bounds$lb
    ub <- bounds$ub
  }

  # Solve
  solver <- osqp::osqp(
    P, q, Amat, lb, ub,
    osqp::osqpSettings(
      verbose = FALSE,
      eps_abs = 1e-6,
      eps_rel = 1e-6,
      polish = TRUE
    )
  )
  B <- solver$Solve()

  # Status values: https://osqp.org/docs/interfaces/status_values.html
  # Admit "solved" (1) and "solved inaccurate" (2)
  if (B$info$status_val %in% c(1, 2)) {
    charge_solution <- B$x[seq_len(time_slots)]
    discharge_solution <- B$x[seq_len(time_slots) + time_slots]
    battery_profile <- charge_solution - discharge_solution
    return(attach_profile(battery_profile, charge_solution, discharge_solution))
  } else {
    # If it's not feasible, then remove grid constraints
    message_once("\u26A0\uFE0F Optimization warning: optimization not feasible in some windows. Removing grid constraints.")
    import_capacity <- rep(Inf, time_slots)
    export_capacity <- rep(Inf, time_slots)

    if (has_grid_flows) {
      bounds <- bounds_with_capacities(rep(Inf, time_slots), rep(Inf, time_slots))
      lb <- bounds$lb
      ub <- bounds$ub
    } else {
      bounds <- bounds_with_capacities(rep(Inf, time_slots), rep(Inf, time_slots))
      lb <- bounds$lb
      ub <- bounds$ub
    }

    solver <- osqp::osqp(
      P, q, Amat, lb, ub,
      osqp::osqpSettings(
        verbose = FALSE,
        eps_abs = 1e-6,
        eps_rel = 1e-6,
        polish = TRUE
      )
    )
    B <- solver$Solve()

    if (B$info$status_val %in% c(1, 2)) {
      charge_solution <- B$x[seq_len(time_slots)]
      discharge_solution <- B$x[seq_len(time_slots) + time_slots]
      battery_profile <- charge_solution - discharge_solution
      return(attach_profile(battery_profile, charge_solution, discharge_solution))
    } else {
      message_once(paste0("\u26A0\uFE0F Optimization warning: ", B$info$status, ". Disabling battery for some windows."))
      zero_profile <- rep(0, time_slots)
      return(attach_profile(zero_profile, zero_profile, zero_profile))
    }
  }
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
minimize_net_power_window_battery <- function (G, L, Bcap, Bc, Bd, SOCmin, SOCmax, SOCini, import_capacity, export_capacity, lambda, charge_eff, discharge_eff) {

  # Optimization parameters
  time_slots <- length(G)
  identityMat <- diag(time_slots)
  lambdaMat <- get_lambda_matrix(time_slots)

  # Objective function terms
  penaltyMat <- identityMat + lambda*lambdaMat
  P_block <- 2 * penaltyMat
  P <- rbind(
    cbind(P_block, -P_block),
    cbind(-P_block, P_block)
  )
  q_block <- 2 * (L - G)
  q <- c(q_block, -q_block)

  B <- solve_optimization_battery_window(
    P, q, G, L, Bcap, Bc, Bd, SOCmin, SOCmax, SOCini, import_capacity, export_capacity, charge_eff, discharge_eff
  )

  return( B )
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
minimize_cost_window_battery <- function (G, L, PE, PI, PTD, PTU, Bcap, Bc, Bd, SOCmin, SOCmax, SOCini, import_capacity, export_capacity, lambda, charge_eff, discharge_eff) {

  # Optimization parameters
  time_slots <- length(G)
  lambdaMat <- get_lambda_matrix(time_slots)

  # Objective function terms
  # unknown variable: X = [C, D, I, E]
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

  B <- solve_optimization_battery_window(
    P, q, G, L, Bcap, Bc, Bd, SOCmin, SOCmax, SOCini, import_capacity, export_capacity, charge_eff, discharge_eff
  )

  return( B )
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
optimize_battery_window <- function (G, L, PE, PI, PTD, PTU, Bcap, Bc, Bd, SOCmin, SOCmax, SOCini, import_capacity, export_capacity, w, lambda, charge_eff, discharge_eff) {

  # Optimization parameters
  time_slots <- length(G)
  identityMat <- diag(time_slots)
  lambdaMat <- get_lambda_matrix(time_slots)

  # Objective function terms
  # unknown variable: X = [C, D, I, E]
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
  q_block <- (1-w)*(PTD - PTU) - 2*w*mean(PI)^2*(G-L)
  q <- c(
    q_block,
    -q_block,
    (1-w)*PI,
    -(1-w)*PE
  )

  B <- solve_optimization_battery_window(
    P, q, G, L, Bcap, Bc, Bd, SOCmin, SOCmax, SOCini, import_capacity, export_capacity, charge_eff, discharge_eff
  )

  return( B )
}
