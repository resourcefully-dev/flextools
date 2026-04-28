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
  if (direction == "l") {
    return(as.matrix(Matrix::tril(mat, k = k)))
  } else if (direction == "u") {
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
