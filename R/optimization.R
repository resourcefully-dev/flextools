
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


# Optimization of load ------------------------------------------------------------

#' Minimization of the grid flow
#'
#' @param w numeric between `0` and `1`, being the weight of the flexibility potential
#' @param G numeric vector, being the renewable generation profile
#' @param LF numeric vector, being the flexible load profile
#' @param LS numeric vector, being the static load profile
#' @param direction character, being `forward` or `backward`. The direction where energy can be shifted
#' @param time_horizon integer, maximum number of positions to shift energy from
#' @param up_to_G logical, whether to limit the flexible EV demand up to renewable Generation
#' @param window_length integer, window length. If `NULL`, the window length will be the length of `G`
#'
#' @return numeric vector
#' @export
#'
#' @importFrom dplyr tibble %>%
#' @importFrom purrr map
#'
minimize_grid_flow <- function(w, G, LF, LS = NULL, direction = 'forward', time_horizon = NULL, up_to_G = TRUE, window_length = NULL) {
  # Parameters check
  if (w == 0) {
    return( LF )
  }
  if (w > 1) {
    message("Error: Objective must be lower of equal to 1.")
    return( NULL )
  }

  if (is.null(LS)) {
    LS <- rep(0, length(G))
  }

  if (!((length(G) == length(LS)) & (length(LS) == length(LF)))) {
    message("Error: G, LS and LF must have same length.")
    message(paste0("Lengths: G=", length(G), ", LS=", length(LS), ", LF=", length(LF)))
    return( NULL )
  }

  if (((direction != 'forward') & (direction != 'backward'))) {
    message("Error: Direction must be 'forward' or 'backward'")
    return( NULL )
  }

  if (is.null(window_length)) {
    window_length <- length(G)
  }

  O <- tibble(G, LF, LS) %>%
    split(rep(1:(length(G)/window_length), each = window_length)) %>%
    map(
      ~ minimize_grid_flow_window_osqp(
        w = w, G = .x$G, LF = .x$LF, LS = .x$LS, direction = direction,
        time_horizon = time_horizon, up_to_G = up_to_G
      )
    )
  return( as.numeric(unlist(O)) )
}



#' Minimization of the grid flow (just a window)
#'
#' @param w numeric between `0` and `1`, being the weight of the flexibility potential
#' @param G numeric vector, being the renewable generation profile
#' @param LF numeric vector, being the flexible load profile
#' @param LS numeric vector, being the static load profile
#' @param direction character, being `forward` or `backward`. The direction where energy can be shifted
#' @param time_horizon integer, maximum number of positions to shift energy from
#' @param up_to_G logical, whether to limit the flexible EV demand up to renewable Generation
#'
#' @return numeric vector
#'
#' @importFrom osqp osqp osqpSettings
#'
minimize_grid_flow_window_osqp <- function (w, G, LF, LS = NULL, direction = "forward", time_horizon = NULL, up_to_G = TRUE) {

  if (is.null(time_horizon)) {
    time_horizon <- length(G)
  }

  # LF must remain static when there is G
  LF_static <- rep(0, length(LF)) # LF static vector
  static_idx <- G >= (LS + LF)
  semi_static_idx <- (G < (LS + LF)) & (LF > 0) & (LS < G) # LF static and shiftable for the same timeslot

  # If all demand is lower than solar then skip optimization
  if (all(static_idx)) {
    return( LF )
  }
  if (any(static_idx)) {
    LF_static[static_idx] <- LF[static_idx]
  }
  if (any(semi_static_idx)) {
    LF_rest <- G[semi_static_idx] - LS[semi_static_idx]
    LF_static[semi_static_idx] <- LF_rest
  }

  # Update LF and LS
  LS <- LS + LF_static
  LF <- LF - LF_static

  # Optimization parameters
  time_slots <- length(LF)
  E <- sum(LF)
  # Upper limit for optimal LF
  grid_limit <- rep(max(G+LF+LS)*10, time_slots) # No limit by default
  if (up_to_G) {
    grid_limit[G > (LS + LF)] <- 0
  }
  LFmax <- grid_limit + G - LS

  # Matrices for constraints
  identityMat <- diag(time_slots)
  cumsumMat <- triangulate_matrix(matrix(1, time_slots, time_slots), 'l')
  horizonMat_forward <- triangulate_matrix(matrix(1, time_slots, time_slots), 'l', -time_horizon)
  horizonMat_backward <- triangulate_matrix(matrix(1, time_slots, time_slots), 'l', time_horizon)

  # Objective function terms
  P <- 2*identityMat
  q <- 2*(w*LS - w*G - (1-w)*LF)

  # Lower and upper bounds
  ## General bounds
  Amat_general <- identityMat
  lb_general <- rep(0, time_slots)
  ub_general <- LFmax

  ## Shifting bounds
  cumsumLF <- cumsumMat %*% LF
  Amat_cumsum <- cumsumMat
  if (direction == 'forward') {
    lb_cumsum <- horizonMat_forward %*% LF
    ub_cumsum <- cumsumMat %*% LF
  } else {
    lb_cumsum <- cumsumMat %*% LF
    ub_cumsum <- horizonMat_backward %*% LF
  }

  ## Total sum of O == E
  Amat_enery <- matrix(1, ncol = time_slots)
  lb_energy <- E
  ub_energy <- E

  # Join constraints
  Amat <- rbind(Amat_general, Amat_cumsum, Amat_enery)
  lb <- c(lb_general, lb_cumsum, lb_energy)
  ub <- c(ub_general, ub_cumsum, ub_energy)

  # Solve
  solver <- osqp::osqp(P, q, Amat, lb, ub, osqp::osqpSettings(verbose = FALSE))
  O <- solver$Solve()
  LFO <- LF_static + abs(round(O$x, 2))
  return( LFO )
}


# Battery optimization ------------------------------------------------------------

#' Battery optimal charging/discharging profile
#'
#' @param w numeric between `0` and `1`, being the weight of the flexibility potential
#' @param G numeric vector, being the renewable generation profile
#' @param L numeric vector, being the load profile
#' @param Bcap numeric, capacity of the battery
#' @param Bc numeric, maximum charging power
#' @param Bd numeric, maximum discharging power
#' @param SOCmin numeric, minimum State-of-Charge of the battery
#' @param SOCmax numeric, maximum State-of-Charge of the battery
#' @param SOCini numeric, required State-of-Charge at the beginning/end of optimization window
#' @param window_length integer, window length. If `NULL`, the window length will be the length of `G`
#'
#' @return numeric vector
#' @export
#'
#' @importFrom dplyr tibble %>%
#' @importFrom purrr map
#'
add_battery_optimization <- function(w, G, L, Bcap, Bc, Bd, SOCmin = 0, SOCmax = 100, SOCini = NULL, window_length = NULL) {
  # Parameters check
  if (w == 0 | Bcap == 0 | Bc == 0 | Bd == 0 | SOCmin == SOCmax) {
    return( rep(0, length(G)) )
  }
  if (w > 1) {
    message("Error: Objective must be lower of equal to 1.")
    return( NULL )
  }

  if (!(length(G) == length(L))) {
    message("Error: G and L must have same length.")
    message(paste0("Lengths: G=", length(G), ", L=", length(L)))
    return( NULL )
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

  if (is.null(window_length)) {
    window_length <- length(G)
  }

  B <- tibble(G, L) %>%
    split(rep(1:(length(G)/window_length), each = window_length)) %>%
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
#' @param w numeric between `0` and `1`, being the weight of the flexibility potential
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
#' @importFrom osqp osqp osqpSettings
#'
add_battery_window <- function (w, G, L, Bcap, Bc, Bd, SOCmin, SOCmax, SOCini) {

  # Optimization parameters
  time_slots <- length(G)
  identityMat <- diag(time_slots)
  cumsumMat <- triangulate_matrix(matrix(1, time_slots, time_slots), 'l')

  # Objective function terms
  P <- 2*identityMat
  q <- 2*w*(L - G)

  # Lower and upper bounds
  ## General bounds
  Amat_general <- identityMat
  lb_general <- rep(-Bd, time_slots)
  ub_general <- rep(Bc, time_slots)

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
  lb <- c(lb_general, lb_cumsum, lb_energy)
  ub <- c(ub_general, ub_cumsum, ub_energy)

  # Solve
  solver <- osqp::osqp(P, q, Amat, lb, ub, osqp::osqpSettings(verbose = FALSE))
  B <- solver$Solve()
  return( round(B$x, 2) )
}



