# benchmark_helpers.R
# ─────────────────────────────────────────────────────────────────────────────
# Generalised benchmark / parameter-sweep helpers for flextools.
#
# Source this file after calling devtools::load_all() or library(flextools).
# Used by:
#   • tests/benchmark_report.Rmd   (full analysis)
#   • tests/testthat/test-demand-optimization.R  (optionally, for manual runs)
#   • tests/testthat/test-battery-optimization.R (optionally, for manual runs)
# ─────────────────────────────────────────────────────────────────────────────

library(dplyr)
library(purrr)
library(tidyr)
library(tibble)


# ── 1. Data helpers ────────────────────────────────────────────────────────────

#' Return a tibble ready for demand optimization
#'
#' Renames `solar` → `production` and `building` → `flexible` so the result
#' can be passed directly to `optimize_demand()`.
#'
#' @param profiles tibble; defaults to `flextools::energy_profiles`
#' @return tibble with columns: datetime, production, flexible, price_imported,
#'   price_exported, price_turn_up, price_turn_down
demand_opt_data <- function(profiles = flextools::energy_profiles) {
  profiles |>
    rename(production = "solar", flexible = "building")
}

#' Return a tibble ready for battery optimization
#'
#' Renames `solar` → `production` and `building` → `static`, keeping only the
#' columns required by `add_battery_optimization()`.
#'
#' @param profiles tibble; defaults to `flextools::energy_profiles`
#' @return tibble with columns: datetime, production, static, price_imported,
#'   price_exported
battery_opt_data <- function(profiles = flextools::energy_profiles) {
  profiles |>
    rename(production = "solar", static = "building") |>
    select(datetime, production, static, price_imported, price_exported)
}

#' Replicate energy_profiles to simulate datasets of arbitrary length
#'
#' Tiles the 7-day `energy_profiles` dataset, adjusting datetimes so the
#' sequence is continuous.  Useful for computation-time benchmarks.
#'
#' @param n_weeks integer, number of weeks to produce
#' @return tibble with `n_weeks * nrow(energy_profiles)` rows
extend_profiles <- function(n_weeks = 1L) {
  base <- flextools::energy_profiles
  # Length of one tile in seconds (7 days + one 15-min step to avoid overlap)
  period_secs <- as.numeric(difftime(
    max(base$datetime) + lubridate::minutes(15L),
    min(base$datetime),
    units = "secs"
  ))
  purrr::map(seq_len(n_weeks), function(i) {
    base |>
      mutate(datetime = datetime + lubridate::seconds((i - 1L) * period_secs))
  }) |>
    purrr::list_rbind()
}


# ── 2. Single-run benchmarks ───────────────────────────────────────────────────

#' Benchmark a single demand optimization run
#'
#' @param opt_data tibble produced by `demand_opt_data()` (columns: datetime,
#'   flexible, production, price_imported, price_exported).  An optional
#'   `static` column for non-flexible loads is also accepted.
#' @param opt_objective `"grid"`, `"cost"`, or numeric `w` ∈ [0, 1]
#' @param lambda numeric, ramping-penalty weight (default 0)
#' @param direction `"forward"` or `"backward"`
#' @param window_days integer, optimization window in days
#' @param flex_window_hours integer or NULL, flexibility window in hours
#'
#' @return named list with:
#'   \describe{
#'     \item{profile}{numeric vector of optimized flexible demand (kW)}
#'     \item{time_sec}{elapsed seconds}
#'     \item{cost}{total energy cost (currency units)}
#'     \item{peak_from_grid}{maximum imported power (kW)}
#'     \item{peak_to_grid}{maximum exported power (kW)}
#'   }
bench_demand <- function(
  opt_data,
  opt_objective,
  lambda            = 0,
  direction         = "forward",
  window_days       = 1L,
  flex_window_hours = NULL
) {
  # Ensure load_capacity is present; without it the cost/combined objectives
  # can raise demand far above the original profile (unbounded).
  if (!"load_capacity" %in% names(opt_data)) {
    opt_data <- opt_data |>
      mutate(load_capacity = max(.data$flexible, na.rm = TRUE))
  }

  timefully::tic()
  O <- opt_data |>
    optimize_demand(
      opt_objective     = opt_objective,
      direction         = direction,
      window_days       = window_days,
      flex_window_hours = flex_window_hours,
      lambda            = lambda
    )
  time_sec <- timefully::toc()

  # evaluate_cost: O is the total flexible demand → static = 0
  cost_data <- opt_data |> select(-any_of("flexible"))
  cost <- evaluate_cost(cost_data, O)

  production <- if ("production" %in% names(opt_data)) opt_data$production else 0
  static     <- if ("static"     %in% names(opt_data)) opt_data$static     else 0
  net <- O + static - production

  list(
    profile        = O,
    time_sec       = time_sec,
    cost           = cost,
    peak_from_grid = max(pmax(net,  0)),
    peak_to_grid   = max(pmax(-net, 0))
  )
}

#' Compute demand KPIs without any optimization (baseline)
#'
#' @param opt_data tibble produced by `demand_opt_data()`
#' @return named list: cost, peak_from_grid, peak_to_grid
demand_baseline <- function(opt_data) {
  O_base <- opt_data$flexible
  cost_data <- opt_data |> select(-any_of("flexible"))
  cost <- evaluate_cost(cost_data, O_base)

  production <- if ("production" %in% names(opt_data)) opt_data$production else 0
  static     <- if ("static"     %in% names(opt_data)) opt_data$static     else 0
  net <- O_base + static - production

  list(
    cost           = cost,
    peak_from_grid = max(pmax(net,  0)),
    peak_to_grid   = max(pmax(-net, 0))
  )
}

#' Benchmark a single battery optimization run
#'
#' @param opt_data tibble produced by `battery_opt_data()` (columns: datetime,
#'   static, production, price_imported, price_exported)
#' @param opt_objective `"grid"`, `"cost"`, or numeric `w` ∈ [0, 1]
#' @param Bcap numeric, battery capacity (kWh)
#' @param Bc   numeric, max charging power (kW)
#' @param Bd   numeric, max discharging power (kW)
#' @param lambda numeric, ramping-penalty weight (default 0)
#' @param charge_eff numeric, charging efficiency ∈ (0, 1]
#' @param discharge_eff numeric, discharging efficiency ∈ (0, 1]
#' @param window_start_hour integer, start hour of the daily optimization window
#'
#' @return named list with:
#'   \describe{
#'     \item{profile}{numeric vector of battery charge/discharge (kW)}
#'     \item{time_sec}{elapsed seconds}
#'     \item{cost}{total energy cost (currency units)}
#'     \item{n_cycles}{equivalent full cycles (-discharge_sum / Bcap)}
#'     \item{peak_from_grid}{maximum imported power (kW)}
#'     \item{peak_to_grid}{maximum exported power (kW)}
#'   }
bench_battery <- function(
  opt_data,
  opt_objective,
  Bcap              = 50,
  Bc                = 4,
  Bd                = 4,
  lambda            = 0,
  charge_eff        = 1,
  discharge_eff     = 1,
  window_start_hour = 5L
) {
  timefully::tic()
  B <- opt_data |>
    add_battery_optimization(
      opt_objective     = opt_objective,
      Bcap              = Bcap,
      Bc                = Bc,
      Bd                = Bd,
      lambda            = lambda,
      charge_eff        = charge_eff,
      discharge_eff     = discharge_eff,
      window_start_hour = window_start_hour
    )
  time_sec <- timefully::toc()

  cost <- evaluate_cost(opt_data, B)

  # Equivalent full cycles: total discharged energy (kWh) / Bcap
  # B is in kW at 15-min resolution → multiply by 0.25 h to get kWh
  n_cycles <- -sum(pmin(B, 0)) * (15 / 60) / Bcap

  production <- if ("production" %in% names(opt_data)) opt_data$production else 0
  static     <- if ("static"     %in% names(opt_data)) opt_data$static     else 0
  net <- B + static - production

  list(
    profile        = B,
    time_sec       = time_sec,
    cost           = cost,
    n_cycles       = n_cycles,
    peak_from_grid = max(pmax(net,  0)),
    peak_to_grid   = max(pmax(-net, 0))
  )
}

#' Compute battery KPIs without any battery (baseline)
#'
#' @param opt_data tibble produced by `battery_opt_data()`
#' @return named list: cost, peak_from_grid, peak_to_grid
battery_baseline <- function(opt_data) {
  B_base <- rep(0, nrow(opt_data))
  cost <- evaluate_cost(opt_data, B_base)

  production <- if ("production" %in% names(opt_data)) opt_data$production else 0
  static     <- if ("static"     %in% names(opt_data)) opt_data$static     else 0
  net <- static - production

  list(
    cost           = cost,
    peak_from_grid = max(pmax(net,  0)),
    peak_to_grid   = max(pmax(-net, 0))
  )
}


# ── 3. Parameter sweeps ────────────────────────────────────────────────────────

#' Sweep demand optimization over a (w × lambda) grid
#'
#' @param opt_data tibble produced by `demand_opt_data()`
#' @param w_seq numeric vector of combined-objective weights (0 = cost, 1 = grid)
#' @param lambda_seq numeric vector of lambda (ramping penalty) values
#' @param ... additional arguments forwarded to `bench_demand()`
#' @return tidy tibble with columns: w, lambda, time_sec, cost,
#'   peak_from_grid, peak_to_grid
sweep_demand <- function(
  opt_data,
  w_seq      = seq(0, 1, by = 0.1),
  lambda_seq = 0,
  ...
) {
  grid <- tidyr::expand_grid(w = w_seq, lambda = lambda_seq)
  message(sprintf("sweep_demand: %d combinations", nrow(grid)))
  purrr::pmap(grid, function(w, lambda) {
    res <- bench_demand(
      opt_data      = opt_data,
      opt_objective = w,
      lambda        = lambda,
      ...
    )
    tibble::tibble(
      w              = w,
      lambda         = lambda,
      time_sec       = res$time_sec,
      cost           = res$cost,
      peak_from_grid = res$peak_from_grid,
      peak_to_grid   = res$peak_to_grid
    )
  }) |>
    purrr::list_rbind()
}

#' Sweep battery optimization over a (w × lambda × eff) grid
#'
#' When `eff < 1`, `charge_eff = discharge_eff = eff`.
#'
#' @param opt_data tibble produced by `battery_opt_data()`
#' @param w_seq numeric vector of combined-objective weights
#' @param lambda_seq numeric vector of lambda values
#' @param eff_seq numeric vector of round-trip efficiencies (same for charge & discharge)
#' @param ... additional arguments forwarded to `bench_battery()` (e.g. Bcap, Bc, Bd)
#' @return tidy tibble with columns: w, lambda, eff, time_sec, cost,
#'   n_cycles, peak_from_grid, peak_to_grid
sweep_battery <- function(
  opt_data,
  w_seq      = seq(0, 1, by = 0.1),
  lambda_seq = 0,
  eff_seq    = 1,
  ...
) {
  grid <- tidyr::expand_grid(w = w_seq, lambda = lambda_seq, eff = eff_seq)
  message(sprintf("sweep_battery: %d combinations", nrow(grid)))
  purrr::pmap(grid, function(w, lambda, eff) {
    res <- bench_battery(
      opt_data      = opt_data,
      opt_objective = w,
      lambda        = lambda,
      charge_eff    = eff,
      discharge_eff = eff,
      ...
    )
    tibble::tibble(
      w              = w,
      lambda         = lambda,
      eff            = eff,
      time_sec       = res$time_sec,
      cost           = res$cost,
      n_cycles       = res$n_cycles,
      peak_from_grid = res$peak_from_grid,
      peak_to_grid   = res$peak_to_grid
    )
  }) |>
    purrr::list_rbind()
}


# ── 4. Plotting utilities ──────────────────────────────────────────────────────

#' Convert a numeric w value to a readable objective label
#'
#' @param w numeric
#' @return character
w_label <- function(w) {
  dplyr::case_when(
    w == 0 ~ "Cost (w=0)",
    w == 1 ~ "Grid (w=1)",
    TRUE   ~ paste0("Combined (w=", w, ")")
  )
}

#' ggplot2 theme used throughout the benchmark report
theme_bench <- function() {
  ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      plot.title       = ggplot2::element_text(face = "bold", size = 13),
      plot.subtitle    = ggplot2::element_text(colour = "grey40", size = 10),
      strip.text       = ggplot2::element_text(face = "bold"),
      legend.position  = "bottom",
      panel.grid.minor = ggplot2::element_blank()
    )
}
