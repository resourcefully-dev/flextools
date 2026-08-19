# Active shared optimization helpers --------------------------------------

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
    warning(
      "`production` variable not found in `opt_data`. No local energy production will be considered."
    )
    opt_data$production <- 0
  }

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

  if (!("load_capacity" %in% names(opt_data))) {
    opt_data$load_capacity <- Inf
  }

  if (
    !(opt_objective %in% c("grid", "cost", "none", "capacity")) &&
      !is.numeric(opt_objective)
  ) {
    stop("Error: `opt_objective` not valid")
  }

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
  if (is.null(k) || length(k) != 1 || !is.finite(k)) {
    k <- 0
  }
  k <- as.integer(round(k))
  k <- max(min(k, ncol(mat)), -nrow(mat))

  if (direction == "l") {
    return(as.matrix(Matrix::tril(mat, k = k)))
  }
  if (direction == "u") {
    return(as.matrix(Matrix::triu(mat, k = k)))
  }
}


get_lambda_matrix <- function(time_slots) {
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
  start_hour_idx <- which(
    (lubridate::hour(dttm_seq) == window_start_hour) &
      (lubridate::minute(dttm_seq) == 0)
  )
  n_windows <- trunc(length(start_hour_idx) / window_days)

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

  if (n_windows > 1) {
    windows_length <- dplyr::lead(start_windows_idx) - start_windows_idx
    windows_length[is.na(windows_length)] <- windows_length[1]
  } else {
    windows_length <- length(dttm_seq)
  }

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


optimization_solution_tolerance <- function() {
  1e-5
}


#' Ceiling for grid-capacity slack
#'
#' How far a grid capacity may be missed when it cannot be met: only as far as
#' the pre-optimization net flow already needed. The result is zero wherever
#' the original profile was within its capacity, which keeps those slots hard
#' capped, so a soft-constrained solution can never create a grid violation
#' worse than the profile it started from.
#'
#' @param capacity numeric vector, grid capacity (kW). May contain `Inf`.
#' @param flow numeric vector, pre-optimization net flow in the same direction
#'   as `capacity` (kW).
#'
#' @return numeric vector, non-negative and finite.
#' @keywords internal
#'
optimization_slack_ceiling <- function(capacity, flow) {
  ceiling <- pmax(0, flow - capacity)
  # `capacity = Inf` yields -Inf before the pmax; guard anything non-finite
  # (including the Inf - Inf = NaN corner) back to "no relaxation allowed".
  ceiling[!is.finite(ceiling)] <- 0
  ceiling
}


#' Linear penalty weight for grid-capacity slack
#'
#' The weight must strictly dominate the marginal gain of a quadratic
#' net-power objective, otherwise the optimizer prefers a flatter profile over
#' respecting the grid capacity. Since `|d/dB sum(net^2)| = 2*|net|` and the
#' achievable net flow is bounded by the relaxed envelope, `2 * max(envelope)`
#' is a tight and valid bound.
#'
#' Deriving the weight from the envelope rather than from the battery power
#' rating matters for performance, not just tidiness: an oversized weight
#' leaves the optimum untouched but multiplies the ADMM iteration count. On a
#' one-year benchmark the power-derived weight needed 8600 iterations against
#' 2625 for the envelope-derived one, for an identical solution.
#'
#' @param envelope numeric vector, per-slot bound on the absolute net flow (kW)
#'
#' @return numeric scalar
#' @keywords internal
#'
optimization_slack_penalty <- function(envelope) {
  envelope <- abs(envelope[is.finite(envelope)])
  if (!length(envelope)) {
    envelope <- 0
  }
  10 * (2 * max(envelope) + 1)
}


#' Concentrate an unavoidable grid-capacity miss
#'
#' When a capacity cannot be met, the slack of
#' [optimization_slack_ceiling()] is penalised linearly in the *volume* missed.
#' Every way of spending the battery's limited energy across the affected slots
#' therefore costs the objective exactly the same, and the quadratic net-power
#' term breaks the tie by flattening: the miss is spread thinly so that every
#' slot ends marginally over its capacity. That is the worst distribution for a
#' capacity contract, whose cost is counted in slots (or hours) in violation —
#' `congestion_time` in [get_energy_kpis()] — not in kWh: it keeps 100% of the
#' window in violation whatever the battery size, so the metric only moves once
#' the battery is large enough to clear the window entirely.
#'
#' This redistributes the *same* energy inside each run of violating slots so
#' the capacity is met exactly for as many slots as it covers, in chronological
#' order, and missed on the remainder.
#'
#' The redistribution is energy-preserving per run, which is what keeps it
#' feasible: every `cumsum` outside a run is untouched, and inside a run both
#' the flat and the front-loaded profile move in one direction only and end at
#' the same value, so the extremum of the storage path is unchanged.
#'
#' Three things make it safe to call unconditionally. A run is skipped when its
#' battery power does not consistently point the way the violation needs, when
#' rearranging the energy cannot bring a single slot inside its capacity (a run
#' the battery misses on *power*, where this would only spike the profile), and
#' the whole result is dropped in favour of `B` if it turns out worse than `B`
#' on any bound.
#'
#' @param B numeric vector, solved battery power (kW), positive when charging.
#' @param net0 numeric vector, pre-battery net flow `L - G` (kW).
#' @param import_capacity,export_capacity numeric vectors of capacities (kW),
#'   constraining the net flow. May contain `Inf`.
#' @param lb_B,ub_B numeric vectors, the box the solution must stay inside.
#' @param lb_cumsum,ub_cumsum numeric vectors, the storage band on `cumsum(B)`.
#' @param tol numeric, tolerance for both the violation test and the
#'   feasibility check.
#'
#' @return numeric vector the same length as `B`: the concentrated profile, or
#'   `B` unchanged when nothing could be improved feasibly.
#' @keywords internal
#'
optimization_concentrate_slack <- function(
  B,
  net0,
  import_capacity,
  export_capacity,
  lb_B,
  ub_B,
  lb_cumsum,
  ub_cumsum,
  tol = optimization_solution_tolerance()
) {
  n <- length(B)
  import_capacity <- rep_len(import_capacity, n)
  export_capacity <- rep_len(export_capacity, n)
  net <- net0 + B

  # +1: over the import capacity, needs more discharge. -1: over the export
  # capacity, needs more charge. A slot cannot be both.
  direction <- rep(0L, n)
  direction[is.finite(import_capacity) & net > import_capacity + tol] <- 1L
  direction[is.finite(export_capacity) & -net > export_capacity + tol] <- -1L
  if (!any(direction != 0L)) {
    return(B)
  }

  # Where the battery would have to sit for the violated capacity to be met
  # exactly, kept inside the box and inside the capacity of the *other*
  # direction so concentrating one miss can never open another.
  target <- ifelse(
    direction == 1L,
    import_capacity - net0,
    -export_capacity - net0
  )
  floor_other <- ifelse(
    is.finite(export_capacity),
    -export_capacity - net0,
    -Inf
  )
  ceil_other <- ifelse(is.finite(import_capacity), import_capacity - net0, Inf)
  target <- pmin(pmax(target, pmax(lb_B, floor_other)), pmin(ub_B, ceil_other))

  runs <- rle(direction)
  ends <- cumsum(runs$lengths)
  starts <- ends - runs$lengths + 1L

  out <- B
  for (k in which(runs$values != 0L)) {
    idx <- starts[k]:ends[k]
    if (length(idx) < 2L) {
      next
    }
    sign_needed <- if (runs$values[k] == 1L) -1 else 1
    # The energy to redistribute, and the target it is redistributed towards,
    # must both point the way the violation needs, or front-loading would not
    # be a monotone rearrangement of the storage path and the argument above
    # would not hold. The target can point the wrong way when the two
    # capacities contradict each other, leaving an empty band.
    if (any(sign_needed * B[idx] < -tol)) {
      next
    }
    if (any(sign_needed * target[idx] < -tol)) {
      next
    }

    budget <- sum(B[idx])
    spent <- 0
    repaired <- numeric(length(idx))
    for (j in seq_along(idx)) {
      remaining <- budget - spent
      if (sign_needed < 0) {
        step <- max(target[idx[j]], min(remaining, 0))
      } else {
        step <- min(target[idx[j]], max(remaining, 0))
      }
      repaired[j] <- step
      spent <- spent + step
    }

    # Only worth it when it actually buys slots back. A run the battery misses
    # on *power* rather than on energy cannot meet its capacity in any slot no
    # matter how the energy is arranged: concentrating there would spike the
    # profile for nothing, so leave the solver's flat answer alone. Every slot
    # in the run is in violation by construction, so the count before is its
    # length.
    net_repaired <- net0[idx] + repaired
    still_over <- if (runs$values[k] == 1L) {
      sum(net_repaired > import_capacity[idx] + tol)
    } else {
      sum(-net_repaired > export_capacity[idx] + tol)
    }
    if (still_over < length(idx)) {
      out[idx] <- repaired
    }
  }

  if (isTRUE(all.equal(out, B))) {
    return(B)
  }

  # Measured against the solved profile, not against the bounds alone: OSQP
  # terminates at a tolerance, so its own answer can sit a whisker outside the
  # storage band (1.2e-05 over the ceiling in the scenario this was written
  # for). Checking absolutely would reject the redistribution over residue that
  # is already in the input and has nothing to do with it. The question that
  # matters is whether this made anything worse, so every bound is widened to
  # whatever the input already used.
  #
  # The storage band is compared on its extremes rather than per slot: the
  # redistribution deliberately moves the storage path *within* a run, and the
  # guarantee is about how far it goes, not slot by slot.
  storage_in <- cumsum(B)
  storage_out <- cumsum(out)
  storage_floor <- min(min(rep_len(lb_cumsum, n)), min(storage_in))
  storage_ceiling <- max(max(rep_len(ub_cumsum, n)), max(storage_in))

  feasible <- abs(sum(out) - sum(B)) <= tol &&
    all(out >= pmin(lb_B, B) - tol) &&
    all(out <= pmax(ub_B, B) + tol) &&
    all(storage_out >= storage_floor - tol) &&
    all(storage_out <= storage_ceiling + tol)
  if (!feasible) {
    return(B)
  }

  out
}


optimization_objective_tolerance <- function() {
  1e-8
}


optimization_relative_gap_tolerance <- function() {
  1e-3
}


optimization_objective_gap <- function(lower_bound, incumbent) {
  (incumbent - lower_bound) / max(1, abs(lower_bound), abs(incumbent))
}


optimization_highs_options <- function(
  include_mip_gap = FALSE,
  time_limit = NULL
) {
  # `threads = 1L` is intentional: callers typically parallelize at the
  # window level (e.g. via mirai + purrr::in_parallel), so letting HiGHS
  # spawn its own threads would oversubscribe the machine.
  args <- list(
    threads = 1L,
    log_to_console = FALSE
  )

  if (include_mip_gap) {
    args$mip_rel_gap <- optimization_relative_gap_tolerance()
  }

  if (!is.null(time_limit) && is.finite(time_limit) && time_limit > 0) {
    args$time_limit <- as.numeric(time_limit)
  }

  do.call(highs::highs_control, args)
}


solve_osqp <- function(P, q, A, lb, ub) {
  osqp_result <- tryCatch(
    {
      solver <- osqp::osqp(
        P = P,
        q = q,
        A = A,
        l = lb,
        u = ub,
        osqp::osqpSettings(
          verbose = FALSE,
          eps_abs = 1e-6,
          eps_rel = 1e-6,
          polishing = TRUE,
          max_iter = 100000L
        )
      )
      solver@Solve()
    },
    error = function(e) {
      list(info = list(status_val = -7L, status = conditionMessage(e)))
    }
  )

  if (osqp_result$info$status_val %in% c(1L, 2L)) {
    list(result = list(status_message = "Optimal"), x = osqp_result$x)
  } else {
    list(result = list(status_message = osqp_result$info$status), x = NULL)
  }
}


optimization_normalize_quadratic <- function(
  P,
  tolerance = 1e-8,
  problem_name = "optimization"
) {
  if (is.null(P) || !is.matrix(P)) {
    return(NULL)
  }

  P_symmetric <- (P + t(P)) / 2
  if (max(abs(P_symmetric)) <= tolerance) {
    return(NULL)
  }

  eig <- eigen(P_symmetric, symmetric = TRUE, only.values = TRUE)
  if (any(eig$values < -tolerance)) {
    stop(sprintf("Error: %s objective must be convex", problem_name))
  }

  P_symmetric
}
