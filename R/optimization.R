
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

  if (!(opt_objective %in% c("grid", "cost", "none")) & !is.numeric(opt_objective)) {
    stop("Error: `opt_objective` not valid")
  }

  if (opt_objective == "cost" | is.numeric(opt_objective)) {
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
  resolution <- as.numeric(dttm_seq[2] - dttm_seq[1], units="mins")

  if (is.null(flex_window_hours)) {
    flex_windows_length <- windows_length
  } else {
    if (flex_window_hours > 24*window_days) {
      message("Warning: `flex_window_hours` must be lower than `window_days` hours.")
      flex_window_hours <- 24*window_days
    }
    flex_window_length <- flex_window_hours*60/resolution
    flex_windows_length <- purrr::map_dbl(
      windows_length,
      ~ ifelse(.x < flex_window_length, .x, flex_window_length)
    )
  }

  flex_windows_idxs <- dplyr::tibble(
    start = start_windows_idx,
    end = start_windows_idx + windows_length - 1,
    flex_end = start_windows_idx + flex_windows_length - 1,
    flex_idx = map2(.data$start, .data$flex_end, ~ seq(.x, .y))
  ) %>%
    dplyr::filter(.data$end <= length(dttm_seq))

  return(flex_windows_idxs)
}


get_bounds <- function(LF, LFmax, time_slots, time_horizon, direction) {

  identityMat <- diag(time_slots)
  cumsumMat <- triangulate_matrix(matrix(1, time_slots, time_slots), 'l')

  ## General bounds
  Amat_general <- identityMat
  lb_general <- rep(0, time_slots)

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
    ub_general <- pmin(ub_shift, LFmax)  # Update general bound with the minimum of both bounds

  } else {
    horizonMat_cumsum <- triangulate_matrix(matrix(1, time_slots, time_slots), "l", time_horizon)
    horizonMat_identity <- triangulate_matrix(triangulate_matrix(matrix(1, time_slots, time_slots), "u"), "l", time_horizon)

    # Cumulative sum bounds
    lb_cumsum <- cumsumMat %*% LF
    ub_cumsum <- horizonMat_cumsum %*% LF

    # Identity bounds
    ub_shift <- horizonMat_identity %*% LF
    ub_general <- pmin(ub_shift, LFmax) # Update general bound with the minimum of both bounds
  }

  return(
    list(
      Amat_general = Amat_general,
      lb_general = lb_general,
      ub_general = ub_general,
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
#' @param opt_objective character, optimization objective being `"grid"` (default),
#'  `"cost"` or a value between 0 (cost) and 1 (grid).
#' @param direction character, being `forward` or `backward`. The direction where energy can be shifted
#' @param time_horizon integer, maximum number of positions to shift energy from.
#'  If `NULL`, the `time_horizon` will be the number of rows of `op_data`.
#' @param window_days integer, number of days to consider as optimization window.
#' @param window_start_hour integer, starting hour of the optimization window.
#' @param flex_window_hours integer, flexibility window length, in hours.
#' This optional feature lets you apply flexibility only during few hours from the `window_start_hour`.
#' It must be lower than `window_days*24` hours.
#' @param lambda numeric, penalty on change for the flexible load.
#' @param mc.cores integer, number of cores to use.
#' Must be at least one, and parallelization requires at least two cores.
#'
#' @return numeric vector
#' @export
#'
#' @importFrom dplyr tibble %>% left_join arrange
#' @importFrom purrr map2
#' @importFrom rlang .data
#' @importFrom parallel mclapply detectCores
#'
optimize_demand <- function(opt_data, opt_objective = "grid",
                            direction = 'forward', time_horizon = NULL,
                            window_days = 1, window_start_hour = 0,
                            flex_window_hours = NULL, lambda = 0,
                            mc.cores = 1) {
  # Parameters check
  opt_data <- check_optimization_data(opt_data, opt_objective)
  if (is.null(opt_data)) {
    stop("Error: `opt_data` parameter is empty.")
  }

  if (((direction != 'forward') & (direction != 'backward'))) {
    stop("Error: `direction` must be 'forward' or 'backward'")
  }

  # Multi-core parameter check
  if (mc.cores > detectCores(logical = FALSE) | mc.cores < 1) {
    mc.cores <- 1
  }
  if (mc.cores > 1) {
    my.mclapply <- switch(
      Sys.info()[['sysname']], # check OS
      Windows = {mclapply.windows}, # case: windows
      Linux   = {mclapply}, # case: linux
      Darwin  = {mclapply} # case: mac
    )
    options(mc.cores = mc.cores) # mclapply functions use `getOption("mc.cores", 2L)`
  } else {
    my.mclapply <- map
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

  # Optimization
  if (opt_objective == "grid") {
    O_windows <- my.mclapply(
      flex_windows_idxs$flex_idx,
      function(.x) {
          minimize_net_power_window(
            G = opt_data$production[.x],
            LF = opt_data$flexible[.x],
            LS = opt_data$static[.x],
            direction = direction,
            time_horizon = time_horizon,
            LFmax = opt_data$load_capacity[.x],
            import_capacity = opt_data$import_capacity[.x],
            export_capacity = opt_data$export_capacity[.x],
            lambda = lambda
        )
      }
    )
  } else if (opt_objective == "cost") {
    O_windows <- my.mclapply(
      flex_windows_idxs$flex_idx,
      function (.x) {
        minimize_cost_window(
          G = opt_data$production[.x],
          LF = opt_data$flexible[.x],
          LS = opt_data$static[.x],
          PI = opt_data$price_imported[.x],
          PE = opt_data$price_exported[.x],
          PTU = opt_data$price_turn_up[.x],
          PTD = opt_data$price_turn_down[.x],
          direction = direction,
          time_horizon = time_horizon,
          LFmax = opt_data$load_capacity[.x],
          import_capacity = opt_data$import_capacity[.x],
          export_capacity = opt_data$export_capacity[.x],
          lambda = lambda
        )
      }
    )
  } else if (is.numeric(opt_objective)) {
    O_windows <- my.mclapply(
      flex_windows_idxs$flex_idx,
      function (.x) {
        optimize_demand_window(
          G = opt_data$production[.x],
          LF = opt_data$flexible[.x],
          LS = opt_data$static[.x],
          PI = opt_data$price_imported[.x],
          PE = opt_data$price_exported[.x],
          PTU = opt_data$price_turn_up[.x],
          PTD = opt_data$price_turn_down[.x],
          direction = direction,
          time_horizon = time_horizon,
          LFmax = opt_data$load_capacity[.x],
          import_capacity = opt_data$import_capacity[.x],
          export_capacity = opt_data$export_capacity[.x],
          lambda = lambda,
          w = opt_objective
        )
      }
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

  # Round to 2 decimals to avoid problems with lower and upper bounds
  G <- round(G, 2)
  LF <- round(LF, 2)
  LS <- round(LS, 2)

  # Optimization parameters
  time_slots <- length(LF)
  E <- sum(LF)
  if (is.null(time_horizon)) {
    time_horizon <- time_slots
  }
  if (time_horizon > time_slots) {
    time_horizon <- time_slots
  }
  LFmax_vct <- round(pmin(import_capacity + G - LS, LFmax), 2)
  if (any(LFmax_vct < 0)) {
    # message("Warning: Grid capacity too low.")
    LFmax_vct <- pmax(LFmax_vct, 0)
  }
  identityMat <- diag(time_slots)

  # Objective function terms
  P <- 2*identityMat*(1+lambda)
  q <- 2*(LS - G - lambda*LF)

  # Constraints
  L_bounds <- get_bounds(LF, LFmax = LFmax_vct, time_slots, time_horizon, direction)

  # Lower and upper bounds
  ## General bounds
  ##  - Grid capacity: -export_capacity <= LF + LS - G <= +import_capacity
  ##    - LB: LF >= G - LS - export_capacity
  ##    - UB: LF <= G - LS + import_capacity
  ##  - Battery power limits:
  ##    - LB: LF >= 0
  ##    - UB: LF <= LFmax
  Amat_general <- identityMat
  lb_general <- pmax(G - LS - export_capacity, 0)
  ub_general <- pmin(G - LS + import_capacity, LFmax)

  ## Energy can only be shifted forwards or backwards with a specific time horizon
  ## This is done through cumulative sum matrices
  Amat_cumsum <- L_bounds$Amat_cumsum
  lb_cumsum <- L_bounds$lb_cumsum
  ub_cumsum <- L_bounds$ub_cumsum

  ## Total sum of O == E
  Amat_enery <- matrix(1, ncol = time_slots)
  lb_energy <- E
  ub_energy <- E

  # Join constraints
  Amat <- rbind(Amat_general, Amat_cumsum, Amat_enery)
  lb <- round(c(lb_general, lb_cumsum, lb_energy), 2)
  ub <- round(c(ub_general, ub_cumsum, ub_energy), 2)

  # Solve
  solver <- osqp::osqp(P, q, Amat, lb, ub, osqp::osqpSettings(verbose = FALSE))
  O <- solver$Solve()

  # Status values: https://osqp.org/docs/interfaces/status_values.html
  # Admit "solved" (1) and "solved inaccurate" (2)
  if (O$info$status_val <= 2) {
    return( pmin(abs(round(O$x, 2)), LFmax) )
  } else {
    message(paste("Optimization warning:", O$info$status))
    return( LF )
  }
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

  # Round to 2 decimals to avoid problems with lower and upper bounds
  G <- round(G, 2)
  LF <- round(LF, 2)
  LS <- round(LS, 2)

  # Optimization parameters
  time_slots <- length(LF)
  E <- sum(LF)
  if (is.null(time_horizon)) {
    time_horizon <- time_slots
  }
  LFmax_vct <- round(pmin(import_capacity + G - LS, LFmax), 2)
  if (any(LFmax_vct < 0)) {
    message("Warning: Grid capacity too low. Skipping optimization.")
    return(LF)
  }
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

  # Constraints
  L_bounds <- get_bounds(LF, LFmax = LFmax_vct, time_slots, time_horizon, direction)

  ## Optimal demand bounds
  ##    0 <= O <= ub (calculated according to time_horizon)
  Amat_O <- cbind(identityMat, identityMat*0, identityMat*0)
  lb_O <- L_bounds$lb_general
  ub_O <- L_bounds$ub_general

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
  lb_energy <- E
  ub_energy <- E

  # Join constraints
  Amat <- rbind(Amat_O, Amat_I, Amat_E, Amat_balance, Amat_cumsum, Amat_energy)
  lb <- round(c(lb_O, lb_I, lb_E, lb_balance, lb_cumsum, lb_energy), 2)
  ub <- round(c(ub_O, ub_I, ub_E, ub_balance, ub_cumsum, ub_energy), 2)

  # Solve
  solver <- osqp::osqp(P, q, Amat, lb, ub, osqp::osqpSettings(verbose = FALSE))
  O <- solver$Solve()

  # Status values: https://osqp.org/docs/interfaces/status_values.html
  # Admit "solved" (1) and "solved inaccurate" (2)
  if (O$info$status_val <= 2) {
    return( pmin(abs(round(O$x, 2))[seq_len(time_slots)], LFmax) )
  } else {
    message(paste("Optimization warning:", O$info$status))
    return( LF )
  }
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

  # Round to 2 decimals to avoid problems with lower and upper bounds
  G <- round(G, 2)
  LF <- round(LF, 2)
  LS <- round(LS, 2)

  # Optimization parameters
  time_slots <- length(LF)
  E <- sum(LF)
  if (is.null(time_horizon)) {
    time_horizon <- time_slots
  }
  LFmax_vct <- round(pmin(import_capacity + G - LS, LFmax), 2)
  if (any(LFmax_vct < 0)) {
    message("Warning: Grid capacity too low. Skipping optimization.")
    return(LF)
  }
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

  # Constraints
  L_bounds <- get_bounds(LF, LFmax = LFmax_vct, time_slots, time_horizon, direction)

  ## Optimal demand bounds
  ##    0 <= O <= ub (calculated according to time_horizon)
  Amat_O <- cbind(identityMat, identityMat*0, identityMat*0)
  lb_O <- L_bounds$lb_general
  ub_O <- L_bounds$ub_general

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
  lb_energy <- E
  ub_energy <- E

  # Join constraints
  Amat <- rbind(Amat_O, Amat_I, Amat_E, Amat_balance, Amat_cumsum, Amat_energy)
  lb <- round(c(lb_O, lb_I, lb_E, lb_balance, lb_cumsum, lb_energy), 2)
  ub <- round(c(ub_O, ub_I, ub_E, ub_balance, ub_cumsum, ub_energy), 2)

  # Solve
  solver <- osqp::osqp(P, q, Amat, lb, ub, osqp::osqpSettings(verbose = FALSE))
  O <- solver$Solve()

  # Status values: https://osqp.org/docs/interfaces/status_values.html
  # Admit "solved" (1) and "solved inaccurate" (2)
  if (O$info$status_val <= 2) {
    return( pmin(abs(round(O$x, 2))[seq_len(time_slots)], LFmax) )
  } else {
    message(paste("Optimization warning:", O$info$status))
    return( LF )
  }
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
#' @param opt_objective character, optimization objective being `"grid"` (default) or `"cost"`
#' @param Bcap numeric, capacity of the battery
#' @param Bc numeric, maximum charging power
#' @param Bd numeric, maximum discharging power
#' @param SOCmin numeric, minimum State-of-Charge of the battery
#' @param SOCmax numeric, maximum State-of-Charge of the battery
#' @param SOCini numeric, required State-of-Charge at the beginning/end of optimization window
#' @param window_days integer, number of days to consider as optimization window.
#' @param window_start_hour integer, starting hour of the optimization window.
#' @param flex_window_hours integer, flexibility window length, in hours.
#' This optional feature lets you apply flexibility only during few hours from the `window_start_hour`.
#' It must be lower than `window_days*24` hours.
#' @param lambda numeric, penalty on change for the flexible load.
#' @param mc.cores integer, number of cores to use.
#' Must be at least one, and parallelization requires at least two cores.
#'
#' @return numeric vector
#' @export
#'
#' @importFrom dplyr tibble %>%
#' @importFrom purrr map
#' @importFrom parallel detectCores mclapply
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
                                     mc.cores = 1) {

  # Parameters check
  opt_data <- opt_data %>% mutate(flexible = 0)
  opt_data <- check_optimization_data(opt_data, opt_objective)
  if (is.null(opt_data)) {
    stop("Error: `opt_data` parameter is empty.")
  }

  if (Bcap == 0 | Bc == 0 | Bd == 0 | SOCmin == SOCmax) {
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

  # Multi-core parameter check
  if (mc.cores > detectCores(logical = FALSE) | mc.cores < 1) {
    mc.cores <- 1
  }
  if (mc.cores > 1) {
    my.mclapply <- switch(
      Sys.info()[['sysname']], # check OS
      Windows = {mclapply.windows}, # case: windows
      Linux   = {mclapply}, # case: linux
      Darwin  = {mclapply} # case: mac
    )
    options(mc.cores = mc.cores) # mclapply functions use `getOption("mc.cores", 2L)`
  } else {
    my.mclapply <- map
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

  # Optimization
  if (opt_objective == "grid") {
    B_windows <- my.mclapply(
      flex_windows_idxs$flex_idx,
      function (.x) {
        minimize_net_power_window_battery(
          G = opt_data$production[.x], L = opt_data$static[.x],
          Bcap = Bcap, Bc = Bc, Bd = Bd,
          SOCmin = SOCmin, SOCmax = SOCmax, SOCini = SOCini,
          import_capacity = opt_data$import_capacity[.x],
          export_capacity = opt_data$export_capacity[.x]
        )
      }
    )
  } else if (opt_objective == "cost") {
    B_windows <- my.mclapply(
      flex_windows_idxs$flex_idx,
      function (.x) {
        minimize_cost_window_battery(
          G = opt_data$production[.x], L = opt_data$static[.x],
          PI = opt_data$price_imported[.x], PE = opt_data$price_exported[.x],
          PTU = opt_data$price_turn_up[.x], PTD = opt_data$price_turn_down[.x],
          Bcap = Bcap, Bc = Bc, Bd = Bd,
          SOCmin = SOCmin, SOCmax = SOCmax, SOCini = SOCini,
          import_capacity = opt_data$import_capacity[.x],
          export_capacity = opt_data$export_capacity[.x],
          lambda = lambda
        )
      }
    )
  } else if (is.numeric(opt_objective)) {
    B_windows <- my.mclapply(
      flex_windows_idxs$flex_idx,
      function (.x) {
        optimize_battery_window(
          G = opt_data$production[.x], L = opt_data$static[.x],
          PI = opt_data$price_imported[.x], PE = opt_data$price_exported[.x],
          PTU = opt_data$price_turn_up[.x], PTD = opt_data$price_turn_down[.x],
          Bcap = Bcap, Bc = Bc, Bd = Bd,
          SOCmin = SOCmin, SOCmax = SOCmax, SOCini = SOCini,
          import_capacity = opt_data$import_capacity[.x],
          export_capacity = opt_data$export_capacity[.x],
          w = opt_objective, lambda = lambda
        )
      }
    )
  } else {
    stop("Error: invalid `opt_objective`")
  }

  B <- as.numeric(unlist(B_windows))

  if (length(flex_windows_idxs_seq) == length(dttm_seq)) {
    return( B )
  } else {
    # Create the complete battery vector with the time slots outside the
    # optimization windows
    B_flex <- left_join(
      tibble(idx = seq_len(length(dttm_seq))),
      tibble(
        idx = flex_windows_idxs_seq,
        B = B
      ),
      by = 'idx'
    ) %>%
      arrange(.data$idx)

    B_flex$B[is.na(B_flex$B)] <- 0
    return( B_flex$B )
  }
}






#' Battery optimal charging/discharging profile to minimize grid interaction (just a window)
#'
#' @param G numeric vector, being the renewable generation profile
#' @param L numeric vector, being the load profile
#' @param Bcap numeric, capacity of the battery
#' @param Bc numeric, maximum charging power
#' @param Bd numeric, maximum discharging power
#' @param SOCmin numeric, minimum State-of-Charge of the battery
#' @param SOCmax numeric, maximum State-of-Charge of the battery
#' @param SOCini numeric, required State-of-Charge at the beginning/end of optimization window
#' @param import_capacity numeric or numeric vector, grid maximum import power capacity that will limit the maximum charging power
#' @param export_capacity numeric or numeric vector, grid maximum export power capacity that will limit the maximum discharging power
#' @param lambda numeric, penalty on change for the flexible load.
#'
#' @return numeric vector
#' @keywords internal
#'
minimize_net_power_window_battery <- function (G, L, Bcap, Bc, Bd, SOCmin, SOCmax, SOCini, import_capacity, export_capacity, lambda=0) {

  # Optimization parameters
  time_slots <- length(G)
  identityMat <- diag(time_slots)
  cumsumMat <- triangulate_matrix(matrix(1, time_slots, time_slots), 'l')
  lambdaMat <- get_lambda_matrix(time_slots)

  # Objective function terms
  P <- 2*(identityMat + lambda*lambdaMat)
  q <- 2*(L - G)

  # Lower and upper bounds
  ## General bounds
  ##  - Grid capacity: -export_capacity <= B + L - G <= +import_capacity
  ##    - LB: B >= G - L - export_capacity
  ##    - UB: B <= G - L + import_capacity
  ##  - Battery power limits:
  ##    - LB: B >= -Bd
  ##    - UB: B <= Bc
  Amat_general <- identityMat
  lb_general <- pmax(G - L - export_capacity, -Bd)
  ub_general <- pmin(G - L + import_capacity, Bc)

  if (any(ub_general < 0)) {
    # message("Warning: Grid import capacity too low.")
    ub_general <- pmax(ub_general, 0)
  }

  if (any(lb_general > 0)) {
    # message("Warning: Grid export capacity too low.")
    lb_general <- pmin(lb_general, 0)
  }

  ## SOC limits
  Amat_cumsum <- cumsumMat
  lb_cumsum <- rep((SOCmin - SOCini)/100*Bcap, time_slots)
  ub_cumsum <- rep((SOCmax - SOCini)/100*Bcap, time_slots)

  ## Total sum of B == 0 (neutral balance)
  Amat_enery <- matrix(1, ncol = time_slots)
  lb_energy <- 0
  ub_energy <- 0

  # Join constraints
  Amat <- rbind(Amat_general, Amat_cumsum, Amat_enery)
  lb <- round(c(lb_general, lb_cumsum, lb_energy), 2)
  ub <- round(c(ub_general, ub_cumsum, ub_energy), 2)

  # Solve
  solver <- osqp::osqp(P, q, Amat, lb, ub, osqp::osqpSettings(verbose = FALSE))
  B <- solver$Solve()

  # Status values: https://osqp.org/docs/interfaces/status_values.html
  # Admit "solved" (1) and "solved inaccurate" (2)
  if (B$info$status_val <= 2) {
    return( round(B$x, 2) )
  } else {
    message(paste("Optimization warning:", B$info$status))
    return( rep(0, time_slots) )
  }
}


#' Battery optimal charging/discharging profile to minimize cost (just a window)
#'
#' @param G numeric vector, being the renewable generation profile
#' @param L numeric vector, being the load profile
#' @param PI numeric vector, electricity prices for imported energy
#' @param PE numeric vector, electricity prices for exported energy
#' @param PTD numeric vector, prices for turn-down energy use
#' @param PTU numeric vector, prices for turn-up energy use
#' @param Bcap numeric, capacity of the battery
#' @param Bc numeric, maximum charging power
#' @param Bd numeric, maximum discharging power
#' @param SOCmin numeric, minimum State-of-Charge of the battery
#' @param SOCmax numeric, maximum State-of-Charge of the battery
#' @param SOCini numeric, required State-of-Charge at the beginning/end of optimization window
#' @param import_capacity numeric or numeric vector, grid maximum import power capacity that will limit the maximum charging power
#' @param export_capacity numeric or numeric vector, grid maximum export power capacity that will limit the maximum discharging power
#' @param lambda numeric, penalty on change for the flexible load.
#'
#' @return numeric vector
#' @keywords internal
#'
minimize_cost_window_battery <- function (G, L, PE, PI, PTD, PTU, Bcap, Bc, Bd, SOCmin, SOCmax, SOCini, import_capacity, export_capacity, lambda=0) {

  # Optimization parameters
  time_slots <- length(G)
  identityMat <- diag(time_slots)
  cumsumMat <- triangulate_matrix(matrix(1, time_slots, time_slots), 'l')
  lambdaMat <- get_lambda_matrix(time_slots)

  # Objective function terms
  # unknown variable: X = [B, I, E]
  P <- rbind(
    cbind(
      2*lambda*lambdaMat, identityMat*0, identityMat*0
    ),
    cbind(
      identityMat*0, identityMat*0, identityMat*0
    ),
    cbind(
      identityMat*0, identityMat*0, identityMat*0
    )
  )
  q <- c(
    PTD - PTU, PI, -PE
  )

  # Constraints
  ## Battery bounds
  ##    -Bd <= B <= Bc
  Amat_B <- cbind(identityMat, identityMat*0, identityMat*0)
  lb_B <- rep(-Bd, time_slots)
  ub_B <- rep(Bc, time_slots)

  ## Imported energy bounds
  ## 0 <= It <= import_capacity
  Amat_I <- cbind(
    identityMat*0, identityMat*1, identityMat*0
  )
  lb_I <- rep(0, time_slots)
  ub_I <- import_capacity

  ## Exported energy bounds
  ## 0 <= Et <= export_capacity --> To test
  Amat_E <- cbind(
    identityMat*0, identityMat*0, identityMat*1
  )
  lb_E <- rep(0, time_slots)
  # ub_E <- G  --> This only allows the battery to discharge during importing hours
  ub_E <- export_capacity

  ## Energy balance
  ## It - Et = Bt + Lt - Gt -> Bt - It + Et = Gt - Lt
  Amat_balance <- cbind(
    identityMat*1, identityMat*-1, identityMat*1
  )
  lb_balance <- G - L
  ub_balance <- G - L

  ## SOC limits
  Amat_cumsum <- cbind(
    cumsumMat, identityMat*0, identityMat*0
  )
  lb_cumsum <- rep((SOCmin - SOCini)/100*Bcap, time_slots)
  ub_cumsum <- rep((SOCmax - SOCini)/100*Bcap, time_slots)

  ## Total sum of B == 0 (neutral balance)
  Amat_energy <- cbind(
    matrix(1, ncol = time_slots), matrix(0, ncol = time_slots), matrix(0, ncol = time_slots)
  )
  lb_energy <- 0
  ub_energy <- 0

  # Join constraints
  Amat <- rbind(Amat_B, Amat_I, Amat_E, Amat_balance, Amat_cumsum, Amat_energy)
  lb <- round(c(lb_B, lb_I, lb_E, lb_balance, lb_cumsum, lb_energy), 2)
  ub <- round(c(ub_B, ub_I, ub_E, ub_balance, ub_cumsum, ub_energy), 2)

  # Solve
  solver <- osqp::osqp(P, q, Amat, lb, ub, osqp::osqpSettings(verbose = FALSE))
  B <- solver$Solve()

  # Status values: https://osqp.org/docs/interfaces/status_values.html
  # Admit "solved" (1) and "solved inaccurate" (2)
  if (B$info$status_val <= 2) {
    return( round(B$x[seq_len(time_slots)], 2) )
  } else {
    message(paste("Optimization warning:", B$info$status))
    return( rep(0, time_slots) )
  }
}




#' Battery optimal charging/discharging profile to minimize net power and cost (just a window)
#'
#' @param G numeric vector, being the renewable generation profile
#' @param L numeric vector, being the load profile
#' @param PI numeric vector, electricity prices for imported energy
#' @param PE numeric vector, electricity prices for exported energy
#' @param PTD numeric vector, prices for turn-down energy use
#' @param PTU numeric vector, prices for turn-up energy use
#' @param Bcap numeric, capacity of the battery
#' @param Bc numeric, maximum charging power
#' @param Bd numeric, maximum discharging power
#' @param SOCmin numeric, minimum State-of-Charge of the battery
#' @param SOCmax numeric, maximum State-of-Charge of the battery
#' @param SOCini numeric, required State-of-Charge at the beginning/end of optimization window
#' @param import_capacity numeric or numeric vector, grid maximum import power capacity that will limit the maximum charging power
#' @param export_capacity numeric or numeric vector, grid maximum export power capacity that will limit the maximum discharging power
#' @param w numeric, optimization objective weight (`w=1` minimizes net power while `w=0` minimizes cost).
#' @param lambda numeric, penalty on change for the flexible load.
#'
#' @return numeric vector
#' @keywords internal
#'
optimize_battery_window <- function (G, L, PE, PI, PTD, PTU, Bcap, Bc, Bd, SOCmin, SOCmax, SOCini, import_capacity, export_capacity, w, lambda) {

  # Optimization parameters
  time_slots <- length(G)
  identityMat <- diag(time_slots)
  cumsumMat <- triangulate_matrix(matrix(1, time_slots, time_slots), 'l')
  lambdaMat <- get_lambda_matrix(time_slots)

  # Objective function terms
  # unknown variable: X = [B, I, E]
  P <- rbind(
    cbind(
      2*(lambda*lambdaMat + w*mean(PI)^2*identityMat), identityMat*0, identityMat*0
    ),
    cbind(
      identityMat*0, identityMat*0, identityMat*0
    ),
    cbind(
      identityMat*0, identityMat*0, identityMat*0
    )
  )
  q <- c(
    (1-w)*(PTD - PTU) - 2*w*mean(PI)^2*(G-L), (1-w)*PI, -(1-w)*PE
  )

  # Constraints
  ## Battery bounds
  ##    -Bd <= B <= Bc
  Amat_B <- cbind(identityMat, identityMat*0, identityMat*0)
  lb_B <- rep(-Bd, time_slots)
  ub_B <- rep(Bc, time_slots)

  ## Imported energy bounds
  ## 0 <= It <= import_capacity
  Amat_I <- cbind(
    identityMat*0, identityMat*1, identityMat*0
  )
  lb_I <- rep(0, time_slots)
  ub_I <- import_capacity

  ## Exported energy bounds
  ## 0 <= Et <= export_capacity --> To test
  Amat_E <- cbind(
    identityMat*0, identityMat*0, identityMat*1
  )
  lb_E <- rep(0, time_slots)
  # ub_E <- G   --> This only allowed the battery to discharge during importing hours
  ub_E <- export_capacity

  ## Energy balance
  ## It - Et = Bt + Lt - Gt -> Bt - It + Et = Gt - Lt
  Amat_balance <- cbind(
    identityMat*1, identityMat*-1, identityMat*1
  )
  lb_balance <- G - L
  ub_balance <- G - L

  ## SOC limits
  Amat_cumsum <- cbind(
    cumsumMat, identityMat*0, identityMat*0
  )
  lb_cumsum <- rep((SOCmin - SOCini)/100*Bcap, time_slots)
  ub_cumsum <- rep((SOCmax - SOCini)/100*Bcap, time_slots)

  ## Total sum of B == 0 (neutral balance)
  Amat_energy <- cbind(
    matrix(1, ncol = time_slots), matrix(0, ncol = time_slots), matrix(0, ncol = time_slots)
  )
  lb_energy <- 0
  ub_energy <- 0

  # Join constraints
  Amat <- rbind(Amat_B, Amat_I, Amat_E, Amat_balance, Amat_cumsum, Amat_energy)
  lb <- round(c(lb_B, lb_I, lb_E, lb_balance, lb_cumsum, lb_energy), 2)
  ub <- round(c(ub_B, ub_I, ub_E, ub_balance, ub_cumsum, ub_energy), 2)

  # Solve
  solver <- osqp::osqp(P, q, Amat, lb, ub, osqp::osqpSettings(verbose = FALSE))
  B <- solver$Solve()

  # Status values: https://osqp.org/docs/interfaces/status_values.html
  # Admit "solved" (1) and "solved inaccurate" (2)
  if (B$info$status_val <= 2) {
    return( round(B$x[seq_len(time_slots)], 2) )
  } else {
    message(paste("Optimization warning:", B$info$status))
    return( rep(0, time_slots) )
  }
}




