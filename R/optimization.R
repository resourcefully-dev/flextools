
# General functions -------------------------------------------------------

check_optimization_data <- function(opt_data, opt_objective) {
  if (!("datetime" %in% names(opt_data))) {
    message("Error: `datetime` variable must exist in `opt_data`")
    return( NULL )
  }
  if (!("static" %in% names(opt_data))) {
    opt_data$static <- 0
  }
  if (!("grid_capacity" %in% names(opt_data))) {
    opt_data$grid_capacity <- Inf
  }
  if (opt_objective == "grid") {
    if (!("production" %in% names(opt_data))) {
      message("Warning: `production` variable not found in `opt_data`.
              No local genaration will be considered.")
      opt_data$production <- 0
    }
  }
  if (opt_objective == "cost") {
    if (!("price_imported" %in% names(opt_data))) {
      message("Warning: `price_imported` variable not found in `opt_data`.")
      opt_data$price_imported <- 1
    }
    if (!("price_exported" %in% names(opt_data))) {
      message("Warning: `price_exported` variable not found in `opt_data`.")
      opt_data$price_exported <- 0
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
#' @importFrom lubridate date years
#'
add_extra_days <- function(df) {
  first_day <- df %>%
    filter(date(.data$datetime) == min(date(.data$datetime)))
  first_day$datetime <- first_day$datetime + years(1)
  last_day <- df %>%
    filter(date(.data$datetime) == max(date(.data$datetime)))
  last_day$datetime <- last_day$datetime - years(1)

  bind_rows(
    last_day, df, first_day
  ) %>%
    arrange(.data$datetime)
}

triangulate_matrix <- function(mat, direction = c('l', 'u'), k=0) {
  if (direction == 'l') {
    return( as.matrix(Matrix::tril(mat, k = k)) )
  } else if (direction == 'u') {
    return( as.matrix(Matrix::triu(mat, k = k)) )
  } else {
    message('Error: not valid direction.')
    return( NULL )
  }
}



# Optimization functions --------------------------------------------------

get_optimization_windows_from_dttm_seq <- function(dttm_seq, window_start_hour) {
  windows_start_idx <- which(
    (lubridate::hour(dttm_seq) == window_start_hour) &
      (lubridate::minute(dttm_seq) == 0)
  )
  windows_length <- dplyr::lead(windows_start_idx) - windows_start_idx
  # Fill last NA produced by `lead`
  windows_length[is.na(windows_length)] <- windows_length[1]
  dplyr::tibble(
    start = windows_start_idx,
    end = windows_start_idx + windows_length - 1,
    length = windows_length
  ) %>%
    dplyr::filter(.data$end <= length(dttm_seq))
}

#' Cut the date time sequence according to the optimization window start hour
#'
#' @param dttm_seq datetime sequence of the original timeseries data
#' @param window_start_hour integer, hour to start the optimization window
#'
#' @return datetime sequence
#' @export
#'
adapt_dttm_seq_to_opt_windows <- function(dttm_seq, window_start_hour) {
  opt_windows <- get_optimization_windows_from_dttm_seq(dttm_seq, window_start_hour)
  first_idx <- opt_windows$start[1]
  last_idx <- opt_windows$end[nrow(opt_windows)]
  dttm_seq_opt <- dttm_seq[first_idx:last_idx]
  return( dttm_seq_opt )
}

#' Filter a data frame according to the optimization window start hour
#'
#' @param df data frame, first column named `datetime` of type `datetime`
#' @param window_start_hour integer, hour to start the optimization window
#'
#' @return tibble
#' @export
#'
#' @importFrom dplyr filter
#'
adapt_df_to_opt_windows <- function(df, window_start_hour) {
  dttm_seq_opt <- adapt_dttm_seq_to_opt_windows(df$datetime, window_start_hour)
  df_op <- filter(df, .data$datetime %in% dttm_seq_opt)
  return( df_op )
}


get_flex_windows <- function(dttm_seq, window_start_hour, window_length, flex_window_length) {

  dttm_seq_opt <- adapt_dttm_seq_to_opt_windows(dttm_seq, window_start_hour)
  seq_length <- length(dttm_seq_opt)
  first_time_slot <- which(dttm_seq %in% dttm_seq_opt[1])

  if (is.null(window_length)) {
    window_length <- seq_length
    flex_window_length <- window_length
  }

  if (is.null(flex_window_length)) {
    flex_window_length <- window_length
  }

  if (length(window_length) == 1) {
    windows_length <- rep(window_length, times = trunc(seq_length/window_length))
  } else {
    if (sum(window_length) > seq_length) {
      message("Error: The length of optimization variables must be lower or equal than
              the sum of vector `window_length`")
      return( NULL )
    } else {
      windows_length <- window_length
      diff_windows_length <- unique(window_length)
    }
  }

  if (length(flex_window_length) == 1) {
    flex_windows_length <- purrr::map_dbl(
      windows_length,
      ~ ifelse(.x < flex_window_length, .x, flex_window_length)
    )
  } else {
    flex_windows_length <- purrr::map2_dbl(
      windows_length, flex_window_length
      ~ ifelse(.x < .y, .x, .y)
    )
  }

  flex_windows_idxs <- tibble(
    flex_start = cumsum(c(first_time_slot, windows_length))[seq_len(length(windows_length))],
    flex_end = .data$flex_start + flex_windows_length - 1,
    flex_idx = map2(.data$flex_start, .data$flex_end, ~ seq(.x, .y))
  )

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
#' @param LF numeric vector, being the flexible load profile (in kW)
#'
#' @param opt_data tibble, optimization contextual data.
#' The first column must be named `datetime` (mandatory) containing the
#' date time sequence where the optimization algorithm is applied.
#' The other columns can be:
#'
#' - `static`: static power demand (in kW) from other sectors like buildings,
#' offices, etc.
#'
#' - `grid_capacity`: maximum imported power from the grid (in kW),
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
#' @param opt_objective character, optimization objective being `"grid"` (default) or `"cost"`
#' @param direction character, being `forward` or `backward`. The direction where energy can be shifted
#' @param time_horizon integer, maximum number of positions to shift energy from.
#'  If `NULL`, the `time_horizon` will be the number of rows of `op_data`.
#' @param window_length integer or integer vector, window length in time slots (not hours).
#' If `NULL`, the `window_length` will be the number of rows of `op_data`.
#' It can also be an integer vector, defining a specific length for every
#' window across the length of `opt_data`. In that case, the sum of all vector values
#' must be equal to the length of `opt_data`.
#' @param window_start_hour integer, hour to start the first optimization window.
#' @param flex_window_length integer, flexibility window length in time slots (not hours).
#' This optional feature lets you apply flexibility only during few hours from the start of the window.
#' It can also be an integer vector, defining a specific length for every
#' window across the length of `opt_data`.
#' It must be lower than `window_length`, if not it is limited to `window_length` value.
#' @param LFmax numeric, value of maximum power (in kW) of the flexible load `LF`
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
optimize_demand <- function(LF, opt_data, opt_objective = "grid",
                            direction = 'forward', time_horizon = NULL,
                            window_length = NULL, window_start_hour = 0,
                            flex_window_length = window_length,
                            LFmax = Inf, mc.cores = 2) {
  # Parameters check
  opt_data <- check_optimization_data(opt_data, opt_objective)
  if (is.null(opt_data)) {
    return( NULL )
  }

  if (!(nrow(opt_data) == length(LF))) {
    message("Error: `opt_data` and `LF` must have same length.")
    return( NULL )
  }

  if (((direction != 'forward') & (direction != 'backward'))) {
    message("Error: `direction` must be 'forward' or 'backward'")
    return( NULL )
  }

  if (mc.cores < 1) {
    message("Parameter mc.cores must be at leas 1. Setting mc.cores = 1.")
    mc.cores <- 1
  }
  if (detectCores() <= (mc.cores)) {
    message("Parameter mc.cores too high. Setting mc.cores = 1 to avoid parallelization.")
    mc.cores <- 1
  }

  # Optimization windows
  dttm_seq <- opt_data$datetime
  flex_windows_idxs <- get_flex_windows(
    dttm_seq = dttm_seq,
    window_start_hour = window_start_hour,
    window_length = window_length,
    flex_window_length = flex_window_length
  )
  if (is.null(flex_windows_idxs)) {
    return( NULL )
  }
  flex_windows_idxs_seq <- as.numeric(unlist(flex_windows_idxs$flex_idx))

  # Optimization
  if (opt_objective == "grid") {
    O_windows <- mclapply(
      flex_windows_idxs$flex_idx,
      function (x)
        minimize_grid_flow_window(
          G = opt_data$production[x], LF = LF[x], LS = opt_data$static[x],
          direction = direction, time_horizon = time_horizon,
          LFmax = LFmax, grid_capacity = opt_data$grid_capacity[x]
        ),
      mc.cores = mc.cores
    )
  } else {
    O_windows <- mclapply(
      flex_windows_idxs$flex_idx,
      function (x)
        minimize_cost_window(
          G = opt_data$production[x], LF = LF[x], LS = opt_data$static[x],
          PI = opt_data$price_imported[x], PE = opt_data$price_exported[x],
          direction = direction, time_horizon = time_horizon,
          LFmax = LFmax, grid_capacity = opt_data$grid_capacity[x]
        ),
      mc.cores = mc.cores
    )
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

    O_flex$O[is.na(O_flex$O)] <- LF[is.na(O_flex$O)]
    return( O_flex$O )
  }
}




#' Minimization of the grid flow (just a window)
#'
#' @param G numeric vector, being the renewable generation profile
#' @param LF numeric vector, being the flexible load profile
#' @param LS numeric vector, being the static load profile
#' @param direction character, being `forward` or `backward`. The direction where energy can be shifted
#' @param time_horizon integer, maximum number of positions to shift energy from
#' @param LFmax numeric, value of maximum power (in kW) of the flexible load `LF`
#' @param grid_capacity numeric or numeric vector, grid maximum power capacity that will limit the maximum optimized demand
#'
#' @return numeric vector
#'
minimize_grid_flow_window <- function (G, LF, LS, direction, time_horizon, LFmax, grid_capacity) {

  # Optimization parameters
  time_slots <- length(LF)
  E <- sum(LF)
  if (is.null(time_horizon)) {
    time_horizon <- time_slots
  }
  if (time_horizon > time_slots) {
    time_horizon <- time_slots
  }
  LFmax_vct <- pmin(grid_capacity + G - LS, LFmax)
  identityMat <- diag(time_slots)

  # Objective function terms
  P <- 2*identityMat
  q <- 2*(LS - G)

  # Constraints
  L_bounds <- get_bounds(LF, LFmax = LFmax_vct, time_slots, time_horizon, direction)

  ## General bounds
  Amat_general <- L_bounds$Amat_general
  lb_general <- L_bounds$lb_general
  ub_general <- L_bounds$ub_general

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
  lb <- round(c(lb_general, lb_cumsum, lb_energy), 1)
  ub <- round(c(ub_general, ub_cumsum, ub_energy), 1)

  # Solve
  solver <- osqp::osqp(P, q, Amat, lb, ub, osqp::osqpSettings(verbose = FALSE))
  O <- solver$Solve()
  LFO <- abs(round(O$x, 2))
  return( LFO )
}




#' Minimization of the cost (just a window)
#'
#' @param G numeric vector, being the renewable generation power profile
#' @param LF numeric vector, being the flexible load power profile
#' @param LS numeric vector, being the static load power profile
#' @param PI numeric vector, electricity prices for imported energy
#' @param PE numeric vector, electricity prices for exported energy
#' @param direction character, being `forward` or `backward`. The direction where energy can be shifted
#' @param time_horizon integer, maximum number of positions to shift energy from
#' @param LFmax numeric, value of maximum power (in kW) of the flexible load `LF`
#' @param grid_capacity numeric or numeric vector, grid maximum power capacity that will limit the maximum optimized demand
#'
#' @import ROI.plugin.lpsolve
#'
#' @return numeric vector
#'
minimize_cost_window <- function (G, LF, LS, PI, PE, direction, time_horizon, LFmax, grid_capacity) {

  # Optimization parameters
  time_slots <- length(LF)
  if (is.null(time_horizon)) {
    time_horizon <- time_slots
  }
  if (time_horizon > time_slots) {
    time_horizon <- time_slots
  }
  identityMat <- diag(time_slots)


  # Optimization problem
  # link: TO-DO
  # I*PI - E*PE

  # Linear Optimization Objective
  # One x vector containing three unknown variables:
  # - OL: Optimal load
  # - I: Imported power from the optimal load
  # - E: Exported power from the optimal load
  OP_names <- c(
    paste0("OL_", seq(1, time_slots)),
    paste0("I_", seq(1, time_slots)),
    paste0("E_", seq(1, time_slots))
  )

  OP_objective <- ROI::L_objective(
    L = cbind(
      identityMat*0, identityMat*PI, -1*identityMat*PE
    ),
    names = OP_names
  )


  # Constraints
  ## It <= OLt + LSt -> It - OLt <= LSt
  OP_const_I_le_OL <- cbind(
    identityMat*-1, identityMat*1, identityMat*0
  )
  OP_const_I_le_OL_dir <- rep("<=", time_slots)
  OP_const_I_le_OL_rhs <- LS

  ## Et <= Gt
  OP_const_E_le_G <- cbind(
    identityMat*0, identityMat*0, identityMat*1
  )
  OP_const_E_le_G_dir <- rep("<=", time_slots)
  OP_const_E_le_G_rhs <- G

  ## It - Et = OLt + LSt - Gt -> OLt - It + Et = Gt - LSt
  OP_const_flows <- cbind(
    identityMat*1, identityMat*-1, identityMat*PE*1
  )
  OP_const_flows_dir <- rep("==", time_slots)
  OP_const_flows_rhs <- G - LS

  ## sum(OL) = sum(L)
  OP_const_equal_energy <- c(rep(1, time_slots), rep(0, time_slots), rep(0, time_slots))
  OP_const_equal_energy_dir <- "=="
  OP_const_equal_energy_rhs <- sum(LF)

  ## Capacity constraints:
  ##    L <= grid_capacity + G - LS
  ## And also, if available:
  ##    L <= LFmax
  LFmax_vct <- pmin(grid_capacity + G - LS, LFmax)

  ## Energy can only be shifted forwards or backwards with a specific time horizon
  ## This is done through cumulative sum matrices
  L_bounds <- get_bounds(LF, LFmax = LFmax_vct, time_slots, time_horizon, direction)
  OP_const_cumsum <- cbind(
    L_bounds$Amat_cumsum, identityMat*0, identityMat*0
  )
  OP_const_cumsum_dir1 <- rep("<=", time_slots)
  OP_const_cumsum_rhs1 <- as.numeric(L_bounds$ub_cumsum)
  OP_const_cumsum_dir2 <- rep(">=", time_slots)
  OP_const_cumsum_rhs2 <- as.numeric(L_bounds$lb_cumsum)


  # Bounds
  OP_lb <- c(as.numeric(L_bounds$lb_general), rep(0, time_slots), rep(0, time_slots))
  OP_ub <- c(as.numeric(L_bounds$ub_general), grid_capacity, grid_capacity)


  # Optimization model
  OP_model <- ROI::OP(
    objective = OP_objective,
    constraints = ROI::L_constraint(
      L = rbind(
        OP_const_I_le_OL, OP_const_E_le_G, OP_const_flows, OP_const_equal_energy, OP_const_cumsum, OP_const_cumsum
      ),
      dir = c(OP_const_I_le_OL_dir, OP_const_E_le_G_dir, OP_const_flows_dir,
              OP_const_equal_energy_dir, OP_const_cumsum_dir1, OP_const_cumsum_dir2),
      rhs = c(OP_const_I_le_OL_rhs, OP_const_E_le_G_rhs, OP_const_flows_rhs,
              OP_const_equal_energy_rhs, OP_const_cumsum_rhs1, OP_const_cumsum_rhs2)
    ),
    # types = ,
    bounds = ROI::V_bound(
      li = seq(1, time_slots*3), ui = seq(1, time_slots*3), lb = OP_lb, ub = OP_ub
    ),
    maximum = FALSE
  )


  # Optimization solver
  OP_sol <- ROI::ROI_solve(OP_model, solver = "lpsolve")

  OP_sol_data <- dplyr::tibble(
    name = names(OP_sol$solution),
    value = as.numeric(OP_sol$solution)
  ) %>%
    tidyr::separate(.data$name, into = c("name", "idx"), sep = "_") %>%
    dplyr::arrange(as.numeric(.data$idx)) %>%
    tidyr::pivot_wider()

  OL <- OP_sol_data$OL %>%
    pmin(LFmax) %>%
    pmax(0)

  return( OL )
}






# Battery optimization ------------------------------------------------------------

#' Battery optimal charging/discharging profile
#'
#' @param opt_data tibble, optimization contextual data.
#' The first column must be named `datetime` (mandatory) containing the
#' date time sequence where the optimization algorithm is applied.
#' The other columns can be:
#'
#' - `static`: static power demand (in kW) from other sectors like buildings,
#' offices, etc.
#'
#' - `grid_capacity`: maximum imported power from the grid (in kW),
#' for example the contracted power with the energy company.
#'
#' - `production`: local power generation (in kW).
#' This is used when `opt_objective = "grid"`.
#'
#'
#' - `price_imported`: price for imported energy (€/kWh).
#' This is used when `opt_objective = "cost"`.
#'
#' - `price_exported`: price for exported energy (€/kWh).
#' This is used when `opt_objective = "cost"`.
#' @param opt_objective character, optimization objective being `"grid"` (default) or `"cost"`
#' @param Bcap numeric, capacity of the battery
#' @param Bc numeric, maximum charging power
#' @param Bd numeric, maximum discharging power
#' @param SOCmin numeric, minimum State-of-Charge of the battery
#' @param SOCmax numeric, maximum State-of-Charge of the battery
#' @param SOCini numeric, required State-of-Charge at the beginning/end of optimization window
#' @param window_length integer or integer vector, window length in time slots (not hours).
#' If `NULL`, the `window_length` will be the number of rows of `op_data`.
#' It can also be an integer vector, defining a specific length for every
#' window across the length of `opt_data`. In that case, the sum of all vector values
#' must be equal to the length of `opt_data`.
#' @param window_start_hour integer, hour to start the first optimization window.
#' @param flex_window_length integer, flexibility window length in time slots (not hours).
#' This optional feature lets you apply flexibility only during few hours from the start of the window.
#' It can also be an integer vector, defining a specific length for every
#' window across the length of `opt_data`.
#' It must be lower than `window_length`, if not it is limited to `window_length` value.
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
add_battery_optimization <- function(opt_data, opt_objective = "grid", Bcap, Bc, Bd,
                                     SOCmin = 0, SOCmax = 100, SOCini = NULL,
                                     window_length = NULL, window_start_hour = 0,
                                     flex_window_length = window_length,
                                     mc.cores = 2) {

  # Parameters check
  opt_data <- check_optimization_data(opt_data, opt_objective)
  if (is.null(opt_data)) {
    return( NULL )
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

  if (mc.cores < 1) {
    message("Parameter mc.cores must be at leas 1. Setting mc.cores = 1.")
    mc.cores <- 1
  }
  if (detectCores() <= (mc.cores)) {
    message("Parameter mc.cores too high. Setting mc.cores = 1 to avoid parallelization.")
    mc.cores <- 1
  }

  # Optimization windows
  dttm_seq <- opt_data$datetime
  flex_windows_idxs <- get_flex_windows(
    dttm_seq = dttm_seq,
    window_start_hour = window_start_hour,
    window_length = window_length,
    flex_window_length = flex_window_length
  )
  if (is.null(flex_windows_idxs)) {
    return( NULL )
  }
  flex_windows_idxs_seq <- as.numeric(unlist(flex_windows_idxs$flex_idx))

  # Optimization
  if (opt_objective == "grid") {
    B_windows <- mclapply(
      flex_windows_idxs$flex_idx,
      function (x)
        minimize_grid_flow_window_battery(
          G = opt_data$production[x], L = opt_data$static[x],
          Bcap = Bcap, Bc = Bc, Bd = Bd,
          SOCmin = SOCmin, SOCmax = SOCmax, SOCini = SOCini,
          grid_capacity = opt_data$grid_capacity[x]
        ),
      mc.cores = mc.cores
    )
  } else {
    B_windows <- mclapply(
      flex_windows_idxs$flex_idx,
      function (x)
        minimize_cost_window_battery(
          G = opt_data$production[x], L = opt_data$static[x],
          PI = opt_data$price_imported[x], PE = opt_data$price_exported[x],
          Bcap = Bcap, Bc = Bc, Bd = Bd,
          SOCmin = SOCmin, SOCmax = SOCmax, SOCini = SOCini,
          grid_capacity = opt_data$grid_capacity[x]
        ),
      mc.cores = mc.cores
    )
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






#' Battery optimal charging/discharging profile (just a window)
#'
#' @param G numeric vector, being the renewable generation profile
#' @param L numeric vector, being the load profile
#' @param Bcap numeric, capacity of the battery
#' @param Bc numeric, maximum charging power
#' @param Bd numeric, maximum discharging power
#' @param SOCmin numeric, minimum State-of-Charge of the battery
#' @param SOCmax numeric, maximum State-of-Charge of the battery
#' @param SOCini numeric, required State-of-Charge at the beginning/end of optimization window
#' @param grid_capacity numeric or numeric vector, grid maximum power capacity that will limit the maximum optimized demand
#'
#' @return numeric vector
#'
minimize_grid_flow_window_battery <- function (G, L, Bcap, Bc, Bd, SOCmin, SOCmax, SOCini, grid_capacity = Inf) {

  # Optimization parameters
  time_slots <- length(G)
  identityMat <- diag(time_slots)
  cumsumMat <- triangulate_matrix(matrix(1, time_slots, time_slots), 'l')

  # Objective function terms
  P <- 2*identityMat
  q <- 2*(L - G)

  # Lower and upper bounds
  ## General bounds
  ##  - Grid capacity: -grid_capacity <= L - G + B <= +grid_capacity
  ##    - LB: B >= G - L - grid_capacity
  ##    - UB: B <= G - L + grid_capacity
  ##  - Battery power limits:
  ##    - LB: B >= -Bd
  ##    - UB: B <= Bc
  Amat_general <- identityMat
  lb_general <- pmax(G - L - grid_capacity, -Bd)
  ub_general <- pmin(G - L + grid_capacity, Bc)

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
  lb <- round(c(lb_general, lb_cumsum, lb_energy), 1)
  ub <- round(c(ub_general, ub_cumsum, ub_energy), 1)

  # Solve
  solver <- osqp::osqp(P, q, Amat, lb, ub, osqp::osqpSettings(verbose = FALSE))
  B <- solver$Solve()
  return( round(B$x, 2) )
}


minimize_cost_window_battery <- function (G, L, PE, PI, Bcap, Bc, Bd, SOCmin, SOCmax, SOCini, grid_capacity = Inf) {

  # Optimization parameters
  time_slots <- length(G)
  identityMat <- diag(time_slots)
  cumsumMat <- triangulate_matrix(matrix(1, time_slots, time_slots), 'l')

    # Optimization problem
  # link: TO-DO
  # I*PI - E*PE

  # Linear Optimization Objective
  # One x vector containing three unknown variables:
  # - B: Optimal battery demand
  # - I: Imported power from the optimal load
  # - E: Exported power from the optimal load
  OP_names <- c(
    paste0("B_", seq(1, time_slots)),
    paste0("I_", seq(1, time_slots)),
    paste0("E_", seq(1, time_slots))
  )

  OP_objective <- ROI::L_objective(
    L = cbind(
      identityMat*0, identityMat*PI, identityMat*PE*-1
    ),
    names = OP_names
  )

  # Constraints
  ## It <= Bt + Lt -> It - Bt <= Lt
  OP_const_I_le_OL <- cbind(
    identityMat*-1, identityMat*1, identityMat*0
  )
  OP_const_I_le_OL_dir <- rep("<=", time_slots)
  OP_const_I_le_OL_rhs <- L

  ## Et <= Gt --> This only allows the battery to discharge during importing hours
  OP_const_E_le_G <- cbind(
    identityMat*0, identityMat*0, identityMat*1
  )
  OP_const_E_le_G_dir <- rep("<=", time_slots)
  OP_const_E_le_G_rhs <- G

  ## It - Et = Bt + Lt - Gt -> Bt - It + Et = Gt - Lt
  OP_const_flows <- cbind(
    identityMat*1, identityMat*-1, identityMat*PE*1
  )
  OP_const_flows_dir <- rep("==", time_slots)
  OP_const_flows_rhs <- G - L

  # Lower and upper bounds
  ## General bounds
  ##  - Grid capacity: -grid_capacity <= L - G + B <= +grid_capacity
  ##    - LB: B >= G - L - grid_capacity
  ##    - UB: B <= G - L + grid_capacity
  ##  - Battery power limits:
  ##    - LB: B >= -Bd
  ##    - UB: B <= Bc
  lb_general <- pmax(G - L - grid_capacity, -Bd)
  ub_general <- pmin(G - L + grid_capacity, Bc)
  OP_lb <- c(lb_general, rep(0, time_slots), rep(0, time_slots))
  OP_ub <- c(ub_general, grid_capacity, grid_capacity)


  # Optimization model
  OP_model <- ROI::OP(
    objective = OP_objective,
    constraints = ROI::L_constraint(
      L = rbind(
        OP_const_I_le_OL, OP_const_E_le_G, OP_const_flows
      ),
      dir = c(OP_const_I_le_OL_dir, OP_const_E_le_G_dir, OP_const_flows_dir),
      rhs = c(OP_const_I_le_OL_rhs, OP_const_E_le_G_rhs, OP_const_flows_rhs)
    ),
    # types = ,
    bounds = ROI::V_bound(
      li = seq(1, time_slots*3), ui = seq(1, time_slots*3), lb = OP_lb, ub = OP_ub
    ),
    maximum = FALSE
  )


  # Optimization solver
  OP_sol <- ROI::ROI_solve(OP_model, solver = "lpsolve")

  OP_sol_data <- dplyr::tibble(
    name = names(OP_sol$solution),
    value = as.numeric(OP_sol$solution)
  ) %>%
    tidyr::separate(.data$name, into = c("name", "idx"), sep = "_") %>%
    dplyr::arrange(as.numeric(.data$idx)) %>%
    tidyr::pivot_wider()

  B <- OP_sol_data$B %>%
    pmin(Bc) %>%
    pmax(-Bd)

  return( round(B, 2) )
}




