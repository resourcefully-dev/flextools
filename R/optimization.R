
# General functions -------------------------------------------------------

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

#' Cut the dateteime sequence according to the optimizion window start hour and length
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
  return(dttm_seq_opt)
}



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

get_flex_windows_idx <- function(G, LF, LS, window_length, flex_window_length) {

  if (!((length(G) == length(LS)) & (length(LS) == length(LF)))) {
    message("Error: `G`, `LS` and `LF` must have same length.")
    message(paste0("Lengths: G=", length(G), ", LS=", length(LS), ", LF=", length(LF)))
    return( NULL )
  }

  if (is.null(window_length)) {
    window_length <- length(G)
    flex_window_length <- window_length
  }

  if (is.null(flex_window_length)) {
    flex_window_length <- window_length
  }

  if (length(window_length) == 1) {
    if (((length(G)/window_length) %% 1) > 0) {
      message("Error: The length of vector `G` must be multiple of `window_length`")
      return( NULL )
    } else {
      windows_length <- rep(window_length, times = length(G)/window_length)
    }
  } else {
    if (sum(window_length) != length(G)) {
      message("Error: The length of vector `G` must be equal to the sum of vector `window_length`")
      return( NULL )
    } else {
      windows_length <- window_length
    }
  }

  flex_windows_idxs <- tibble(
    flex_start = cumsum(c(1, windows_length))[seq_len(length(windows_length))],
    flex_end = .data$flex_start + flex_window_length - 1,
    flex_idx = map2(.data$flex_start, .data$flex_end, ~ seq(.x, .y))
  )

  return( flex_windows_idxs )
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

#' Minimization of the grid flow
#'
#' @param G numeric vector, being the renewable generation profile
#' @param LF numeric vector, being the flexible load profile
#' @param LS numeric vector, being the static load profile
#' @param direction character, being `forward` or `backward`. The direction where energy can be shifted
#' @param time_horizon integer, maximum number of positions to shift energy from.
#'  If `NULL`, the `time_horizon` will be the length of `G`, `LF`, and `LS` (it must be the same)
#' @param window_length integer, window length in time slots (not hours).
#' If `NULL`, the `window_length` will be the length of `G`, `LF`, and `LS` (it must be the same)
#' It can also be an integer vector, specifying a specific length for every
#' window along the length of `G`. In that case, the sum of all vector values
#' must be equal to the length of `G`.
#' @param flex_window_length integer, flexibility window length in time slots (not hours).
#' This optional feature lets you apply flexibility only during few hours from the start of the window.
#' @param LFmax numeric, value of maximum power (in kW) of the flexible load `LF`
#' @param grid_capacity numeric or numeric vector, grid maximum power capacity that will limit the maximum optimized demand
#'
#' @return numeric vector
#' @export
#'
#' @importFrom dplyr tibble %>%
#' @importFrom purrr map2
#' @importFrom rlang .data
#'
minimize_grid_flow <- function(G, LF, LS = NULL, direction = 'forward', time_horizon = NULL,
                               window_length = NULL, flex_window_length = window_length,
                               LFmax = Inf, grid_capacity = Inf) {
  # Parameters check
  if (((direction != 'forward') & (direction != 'backward'))) {
    message("Error: `direction` must be 'forward' or 'backward'")
    return( NULL )
  }

  if (is.null(LS)) {
    LS <- rep(0, length(G))
  }

  flex_windows_idxs <- get_flex_windows_idx(G, LF, LS, window_length, flex_window_length)
  if (is.null(flex_windows_idxs)) {
    return( NULL )
  }

  O <- LF
  for (window_idxs in flex_windows_idxs$flex_idx) {
    O_window <- minimize_grid_flow_window(
      G = G[window_idxs], LF = LF[window_idxs], LS = LS[window_idxs],
      direction = direction, time_horizon = time_horizon,
      LFmax = LFmax, grid_capacity = grid_capacity
    )
    O[window_idxs] <- O_window
  }

  return( O )
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
#' @import ROI.plugin.osqp
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
  LFmax_vct <- pmin(grid_capacity + G - LS, LFmax)
  identityMat <- diag(time_slots)


  # Optimization problem
  # link: TO-DO
  # (G - LS - OL)^2
  # G^2 - 2*G*LS - 2*G*OL + LS^2 + 2*LS*OL + OL^2
  # OL^2 + 2*(LS - G)*OL

  # Quadratic Optimization Objective
  # One x vector containing one unknown variables:
  # - OL: Optimal load
  OP_names <- paste0("OL_", seq(1, time_slots))
  OP_objective <- ROI::Q_objective(
    Q = 2*identityMat, L = 2*(LS - G), names = OP_names
  )


  # Constraints

  ## sum(OL) = sum(L)
  OP_const_equal_energy <- rep(1, time_slots)
  OP_const_equal_energy_dir <- "=="
  OP_const_equal_energy_rhs <- E

  ## Energy can only be shifted forwards or backwards with a specific time horizon
  ## This is done through cumulative sum matrices
  L_bounds <- get_bounds(LF, LFmax = LFmax_vct, time_slots, time_horizon, direction)
  OP_const_cumsum <- L_bounds$Amat_cumsum
  OP_const_cumsum_dir1 <- rep("<=", time_slots)
  OP_const_cumsum_rhs1 <- as.numeric(L_bounds$ub_cumsum)
  OP_const_cumsum_dir2 <- rep(">=", time_slots)
  OP_const_cumsum_rhs2 <- as.numeric(L_bounds$lb_cumsum)


  # Bounds
  OP_lb <- as.numeric(L_bounds$lb_general)
  OP_ub <- as.numeric(L_bounds$ub_general)


  # Optimization model
  OP_model <- ROI::OP(
    objective = OP_objective,
    constraints = ROI::L_constraint(
      L = rbind(
        OP_const_equal_energy, OP_const_cumsum, OP_const_cumsum
      ),
      dir = c(OP_const_equal_energy_dir, OP_const_cumsum_dir1, OP_const_cumsum_dir2),
      rhs = c(OP_const_equal_energy_rhs, OP_const_cumsum_rhs1, OP_const_cumsum_rhs2)
    ),
    bounds = ROI::V_bound(
      li = seq(1, time_slots), ui = seq(1, time_slots), lb = OP_lb, ub = OP_ub
    ),
    maximum = FALSE
  )

  # Optimization solver
  OP_sol <- ROI::ROI_solve(OP_model, solver = "osqp")

  OP_sol_data <- dplyr::tibble(
    name = names(ROI::solution(OP_sol)),
    value = as.numeric(ROI::solution(OP_sol))
  ) %>%
    tidyr::separate(.data$name, into = c("name", "idx"), sep = "_") %>%
    dplyr::arrange(as.numeric(.data$idx)) %>%
    tidyr::pivot_wider()

  OL <- OP_sol_data$OL %>%
    pmin(LFmax) %>%
    pmax(0)

  return( OL )
}


#' Minimization of energy cost
#'
#' @param G numeric vector, being the renewable generation profile
#' @param LF numeric vector, being the flexible load profile
#' @param LS numeric vector, being the static load profile
#' @param PI numeric vector, electricity prices for imported energy
#' @param PE numeric vector, electricity prices for exported energy
#' @param direction character, being `forward` or `backward`. The direction where energy can be shifted
#' @param time_horizon integer, maximum number of positions to shift energy from
#' @param window_length integer, window length in time slots (not hours).
#' If `NULL`, the window length will be the length of `G`, `LF`, and `LS` (it must be the same)
#' It can also be an integer vector, specifying a specific length for every
#' window along the length of `G`. In that case, the sum of all vector values
#' must be equal to the length of `G`.
#' @param flex_window_length integer, flexibility window length in time slots (not hours).
#' This optional feature lets you apply flexibility only during few hours from the start of the window.
#' @param LFmax numeric, value of maximum power (in kW) of the flexible load `LF`
#' @param grid_capacity numeric or numeric vector, grid maximum power capacity that will limit the maximum optimized demand
#'
#' @return numeric vector
#' @export
#'
#' @importFrom dplyr tibble %>%
#' @importFrom purrr map2
#' @importFrom rlang .data
#'
minimize_cost <- function(G, LF, LS = NULL, PI, PE,
                          direction = 'forward', time_horizon = NULL,
                          window_length = NULL, flex_window_length = window_length,
                          LFmax = Inf, grid_capacity = Inf) {

  if (((direction != 'forward') & (direction != 'backward'))) {
    message("Error: `direction` must be 'forward' or 'backward'")
    return( NULL )
  }

  if (is.null(LS)) {
    LS <- rep(0, length(G))
  }

  flex_windows_idxs <- get_flex_windows_idx(G, LF, LS, window_length, flex_window_length)
  if (is.null(flex_windows_idxs)) {
    return( NULL )
  }

  O <- LF
  for (window_idxs in flex_windows_idxs$flex_idx) {
    O_window <- minimize_cost_window(
      G = G[window_idxs], LF = LF[window_idxs], LS = LS[window_idxs],
      PI = PI[window_idxs], PE = PE[window_idxs],
      direction = direction, time_horizon = time_horizon,
      LFmax = LFmax, grid_capacity = grid_capacity
    )
    O[window_idxs] <- O_window
  }

  return( O )
}


#' Minimization of the grid flow (just a window)
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
  E <- sum(LF)
  if (is.null(time_horizon)) {
    time_horizon <- time_slots
  }
  LFmax_vct <- pmin(grid_capacity + G - LS, LFmax)
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
      identityMat*0, identityMat*PI, identityMat*PE*-1
    ),
    names = OP_names
  )


  # Constraints
  ## It <= OLt
  OP_const_I_le_OL <- cbind(
    identityMat*-1, identityMat*0, identityMat*1
  )
  OP_const_I_le_OL_dir <- rep("<=", time_slots)
  OP_const_I_le_OL_rhs <- rep(0, time_slots)

  ## Et <= Gt
  OP_const_E_le_G <- cbind(
    identityMat*0, identityMat*0, identityMat*1
  )
  OP_const_E_le_G_dir <- rep("<=", time_slots)
  OP_const_E_le_G_rhs <- G

  ## It - Et = OLt - Gt -> OLt - It + Et = Gt
  OP_const_flows <- cbind(
    identityMat*1, identityMat*-1, identityMat*PE*1
  )
  OP_const_flows_dir <- rep("==", time_slots)
  OP_const_flows_rhs <- G

  ## sum(OL) = sum(L)
  OP_const_equal_energy <- c(rep(1, time_slots), rep(0, time_slots), rep(0, time_slots))
  OP_const_equal_energy_dir <- "=="
  OP_const_equal_energy_rhs <- E

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
  OP_ub <- c(as.numeric(L_bounds$ub_general), rep(Inf, time_slots), rep(Inf, time_slots))


  # Optimization model
  OP_model <- ROI::OP(
    objective = OP_objective,
    constraints = ROI::L_constraint(
      L = rbind(
        OP_const_I_le_OL, OP_const_E_le_G, OP_const_flows, OP_const_equal_energy, OP_const_cumsum, OP_const_cumsum
      ),
      dir = c(OP_const_I_le_OL_dir, OP_const_E_le_G_dir, OP_const_flows_dir, OP_const_equal_energy_dir, OP_const_cumsum_dir1, OP_const_cumsum_dir2),
      rhs = c(OP_const_I_le_OL_rhs, OP_const_E_le_G_rhs, OP_const_flows_rhs, OP_const_equal_energy_rhs, OP_const_cumsum_rhs1, OP_const_cumsum_rhs2)
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
    name = names(ROI::solution(OP_sol)),
    value = as.numeric(ROI::solution(OP_sol))
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
#' @param G numeric vector, being the renewable generation profile
#' @param L numeric vector, being the load profile
#' @param Bcap numeric, capacity of the battery
#' @param Bc numeric, maximum charging power
#' @param Bd numeric, maximum discharging power
#' @param SOCmin numeric, minimum State-of-Charge of the battery
#' @param SOCmax numeric, maximum State-of-Charge of the battery
#' @param SOCini numeric, required State-of-Charge at the beginning/end of optimization window
#' @param window_length integer, window length in time slots (not hours).
#' If `NULL`, the window length will be the length of `G`.
#' It can also be an integer vector, specifying a specific length for every
#' window along the length of `G`. In that case, the sum of all vector values
#' must be equal to the length of `G`.
#'
#' @return numeric vector
#' @export
#'
#' @importFrom dplyr tibble %>%
#' @importFrom purrr map
#'
add_battery_optimization <- function(G, L, Bcap, Bc, Bd, SOCmin = 0, SOCmax = 100, SOCini = NULL, window_length = NULL) {
  # Parameters check
  if (Bcap == 0 | Bc == 0 | Bd == 0 | SOCmin == SOCmax) {
    return( rep(0, length(G)) )
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

  if (!(length(G) == length(L))) {
    message("Error: `G` and `L` must have same length.")
    message(paste0("Lengths: G=", length(G), ", L=", length(L)))
    return( NULL )
  }

  if (is.null(window_length)) {
    window_length <- length(G)
  } else {
    if (length(window_length) == 1) {
      if (((length(G)/window_length) %% 1) > 0) {
        message("Error: The length of vector `G` must be multiple of `window_length`")
        return( NULL )
      }
    } else {
      if (sum(window_length) != length(G)) {
        message("Error: The length of vector `G` must be equal to the sum of vector `window_length`")
        return( NULL )
      }
    }
  }

  if (length(window_length) == 1) {
    windows_length <- rep(window_length, times = length(G)/window_length)
  } else {
    windows_length <- window_length
  }

  windows_idxs <- unlist(map(
    seq_len(length(windows_length)),
    ~ rep(.x, times = windows_length[.x])
  ))

  B <- tibble(G, L) %>%
    split(windows_idxs) %>%
    map(
      ~ add_battery_window(
        w = w, G = .x$G, L = .x$L, Bcap = Bcap, Bc = Bc, Bd = Bd,
        SOCmin = SOCmin, SOCmax = SOCmax, SOCini = SOCini
      )
    )
  return( as.numeric(unlist(B)) )
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
#'
#' @return numeric vector
#'
add_battery_window <- function (G, L, Bcap, Bc, Bd, SOCmin, SOCmax, SOCini) {

  # Optimization parameters
  time_slots <- length(G)
  identityMat <- diag(time_slots)
  cumsumMat <- triangulate_matrix(matrix(1, time_slots, time_slots), 'l')

  # Optimization problem
  # link: TO-DO
  # (G - L - B)^2
  # G^2 - 2*G*L - 2*G*B + L^2 + 2*L*B + B^2
  # B^2 + 2*(L - G)*B

  # Quadratic Optimization Objective
  # One x vector containing one unknown variables:
  # - B: Optimal battery power profile
  OP_names <- paste0("B_", seq(1, time_slots))
  OP_objective <- ROI::Q_objective(
    Q = 2*identityMat, L = 2*(L - G), names = OP_names
  )


  # Constraints

  ## sum(B) = 0 (neutral balance within the optimization window)
  OP_const_energy <- rep(1, time_slots)
  OP_const_energy_dir <- "=="
  OP_const_energy_rhs <- 0

  ## SOC limits
  OP_const_cumsum <- cumsumMat
  OP_const_cumsum_dir1 <- rep("<=", time_slots)
  OP_const_cumsum_rhs1 <- rep((SOCmax - SOCini)/100*Bcap, time_slots)
  OP_const_cumsum_dir2 <- rep(">=", time_slots)
  OP_const_cumsum_rhs2 <- rep((SOCmin - SOCini)/100*Bcap, time_slots)


  # Bounds
  OP_lb <- rep(-Bd, time_slots)
  OP_ub <- rep(Bc, time_slots)


  # Optimization model
  OP_model <- ROI::OP(
    objective = OP_objective,
    constraints = ROI::L_constraint(
      L = rbind(
        OP_const_energy, OP_const_cumsum, OP_const_cumsum
      ),
      dir = c(OP_const_energy_dir, OP_const_cumsum_dir1, OP_const_cumsum_dir2),
      rhs = c(OP_const_energy_rhs, OP_const_cumsum_rhs1, OP_const_cumsum_rhs2)
    ),
    bounds = ROI::V_bound(
      li = seq(1, time_slots), ui = seq(1, time_slots), lb = OP_lb, ub = OP_ub
    ),
    maximum = FALSE
  )


  # Optimization solver
  OP_sol <- ROI::ROI_solve(OP_model, solver = "osqp")

  OP_sol_data <- dplyr::tibble(
    name = names(ROI::solution(OP_sol)),
    value = as.numeric(ROI::solution(OP_sol))
  ) %>%
    tidyr::separate(.data$name, into = c("name", "idx"), sep = "_") %>%
    dplyr::arrange(as.numeric(.data$idx)) %>%
    tidyr::pivot_wider()

  B <- OP_sol_data$B %>%
    pmin(Bc) %>%
    pmax(-Bd)

  return( B )
}

