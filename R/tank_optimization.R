# Tank optimization (OSQP / HiGHS) ---------------------------------------------
#
# A heat pump charging one thermal tank. The compressor's electric power `P`
# is the decision, the heat demand `Q` discharges the tank, and the COP `cop`
# converts one into the other. Per slot of `dt` hours, with the level `S` in
# kWh thermal above the band's bottom:
#
#   S[t] = k * S[t-1] + (cop[t] * P[t] - Q[t]) * dt      k = (1 - loss/100)^dt
#   Smin <= S[t] <= Smax
#   0 <= P[t] <= Pc,  P[t] = 0 where cop[t] = 0
#   S[last slot of a window] >= S[slot before its first]
#
# The level carries from one window to the next, so windows are solved in
# order, and every slot no window optimises runs the static `flexible`
# schedule.


#' Tank level path (unrounded)
#'
#' The level at the start of each slot, and after the last one, applying the
#' clamp at the start of the slot it is given for.
#'
#' @param power numeric vector, compressor electric power (kW)
#' @param heat_demand numeric vector, heat demand (kW thermal)
#' @param cop numeric vector, coefficient of performance
#' @param init numeric, level before the first slot (kWh thermal)
#' @param k numeric, per-slot retention `(1 - loss/100)^dt`
#' @param dt numeric, slot length in hours
#' @param clamp numeric vector, heat taken out at the start of each slot (kWh)
#'
#' @return numeric vector of length `length(power) + 1`
#' @keywords internal
#'
tank_level_path <- function(power, heat_demand, cop, init, k, dt, clamp = 0) {
  n <- length(power)
  clamp <- rep_len(clamp, n)
  heat_in <- (cop * power - heat_demand) * dt
  level <- numeric(n + 1)
  s <- init
  for (t in seq_len(n)) {
    s <- s - clamp[t]
    level[t] <- s
    s <- k * s + heat_in[t]
  }
  level[n + 1] <- s
  level
}


#' Round the compressor power to 2 decimals while tracking the level
#'
#' Rounding each slot on its own drifts the level: a third of a kW rounds to
#' 0.33 in every slot and the tank loses 0.01 kWh of heat per slot and kW of
#' COP. Instead each slot takes the 2-decimal power that brings the level
#' closest to the level the unrounded solution reaches at the end of that slot,
#' so the rounded level never strays further from it than half a rounding step
#' of heat.
#'
#' @inheritParams tank_level_path
#' @param target numeric vector, level at the end of each slot under the
#'   unrounded power (kWh)
#' @param Pc numeric, compressor cap (kW)
#'
#' @return numeric vector, rounded power (kW)
#' @keywords internal
#'
tank_round_power <- function(target, heat_demand, cop, init, k, dt, Pc) {
  n <- length(target)
  p_max <- floor(Pc * 100 + 1e-9) / 100
  out <- numeric(n)
  s <- init
  for (t in seq_len(n)) {
    if (cop[t] > 0) {
      p <- (target[t] - k * s + heat_demand[t] * dt) / (cop[t] * dt)
      out[t] <- min(max(round(p, 2), 0), p_max)
    }
    s <- k * s + (cop[t] * out[t] - heat_demand[t]) * dt
  }
  out
}


#' Solve one tank window
#'
#' @param G numeric vector, production (kW)
#' @param L numeric vector, static load (kW)
#' @param Q numeric vector, heat demand (kW thermal)
#' @param cop numeric vector, coefficient of performance
#' @param PI,PE numeric vectors, import and export prices (EUR/kWh); unused for
#'   `"grid"`
#' @param import_capacity,export_capacity numeric vectors, net grid flow
#'   capacities (kW)
#' @param S0 numeric, level before the first slot, inside the band (kWh)
#' @param Smin,Smax numeric, band (kWh)
#' @param Pc numeric, compressor cap (kW)
#' @param k numeric, per-slot retention
#' @param dt numeric, slot length (hours)
#' @param objective `"grid"`, `"cost"` or a number strictly between 0 and 1
#' @param lambda numeric, ramping penalty weight
#'
#' @return unrounded power (kW), or `NULL` when the window has no solution
#' @keywords internal
#'
tank_solve_window <- function(
  G,
  L,
  Q,
  cop,
  PI,
  PE,
  import_capacity,
  export_capacity,
  S0,
  Smin,
  Smax,
  Pc,
  k,
  dt,
  objective,
  lambda = 0
) {
  n <- length(G)
  import_capacity <- as.numeric(rep_len(import_capacity, n))
  export_capacity <- as.numeric(rep_len(export_capacity, n))

  # The net flow `L + P - G` must stay within the capacities, which for a
  # single non-negative load is a box on `P`. A negative capacity is an
  # obligation to flow the other way and moves the box the same way.
  lb_P <- pmax(0, G - L - export_capacity)
  ub_P <- pmin(ifelse(cop > 0, Pc, 0), G - L + import_capacity)
  if (any(lb_P > ub_P + 1e-9)) {
    return(NULL)
  }
  ub_P <- pmax(ub_P, lb_P)

  lb_S <- rep(Smin, n)
  ub_S <- rep(Smax, n)
  # End condition: the window ends at least as full as it started
  lb_S[n] <- max(Smin, S0)

  with_flows <- !identical(objective, "grid")
  nv <- if (with_flows) 4 * n else 2 * n
  idx_P <- seq_len(n)
  idx_S <- n + seq_len(n)
  idx_I <- 2 * n + seq_len(n)
  idx_E <- 3 * n + seq_len(n)

  # Dynamics: S_t - k * S_{t-1} - cop_t * dt * P_t = -Q_t * dt (+ k * S0 at t = 1)
  A_dyn <- Matrix::sparseMatrix(
    i = c(seq_len(n), seq_len(n), seq_len(n - 1) + 1),
    j = c(idx_S, idx_P, idx_S[seq_len(n - 1)]),
    x = c(rep(1, n), -cop * dt, rep(-k, n - 1)),
    dims = c(n, nv)
  )
  rhs_dyn <- -Q * dt
  rhs_dyn[1] <- rhs_dyn[1] + k * S0

  A <- rbind(Matrix::Diagonal(nv), A_dyn)
  lower <- c(lb_P, lb_S)
  upper <- c(ub_P, ub_S)

  if (with_flows) {
    # Balance: I_t - E_t - P_t = L_t - G_t
    A_bal <- Matrix::sparseMatrix(
      i = rep(seq_len(n), 3),
      j = c(idx_I, idx_E, idx_P),
      x = c(rep(1, n), rep(-1, n), rep(-1, n)),
      dims = c(n, nv)
    )
    A <- rbind(A, A_bal)
    lower <- c(lower, rep(0, n), rep(0, n))
    upper <- c(upper, pmax(L - G + ub_P, 0), pmax(G - L - lb_P, 0))
    lower <- c(lower, rhs_dyn, L - G)
    upper <- c(upper, rhs_dyn, L - G)

    # Importing and exporting in the same slot cancels out on the balance, so
    # it is only free while exporting pays no more than importing costs.
    # Clip the export price as the battery's QP does, keeping the problem
    # convex and bounded.
    PE_clipped <- pmin(PE, PI)
    if (any(PE_clipped != PE)) {
      message_once(
        "\u26a0\ufe0f Optimization: export price exceeds import price; clipping for bounded problem."
      )
    }
  } else {
    lower <- c(lower, rhs_dyn)
    upper <- c(upper, rhs_dyn)
  }

  lambdaMat <- if (lambda > 0) get_lambda_matrix(n) else NULL

  if (identical(objective, "cost") && lambda == 0) {
    # Pure LP on HiGHS
    result <- highs::highs_solve(
      Q = NULL,
      L = c(rep(0, 2 * n), PI, -PE_clipped),
      lower = lower[seq_len(nv)],
      upper = upper[seq_len(nv)],
      A = A[-seq_len(nv), , drop = FALSE],
      lhs = lower[-seq_len(nv)],
      rhs = upper[-seq_len(nv)],
      control = optimization_highs_options()
    )
    if (
      !identical(result$status_message, "Optimal") ||
        is.null(result$primal_solution)
    ) {
      return(NULL)
    }
    x <- as.numeric(result$primal_solution)
  } else {
    P_quad <- Matrix::Matrix(0, nv, nv, sparse = TRUE)
    q <- numeric(nv)
    if (identical(objective, "grid")) {
      # min sum((L - G + P)^2) + lambda * sum((P_t - P_{t-1})^2)
      P_block <- 2 * diag(n)
      if (lambda > 0) {
        P_block <- P_block + 2 * lambda * lambdaMat
      }
      q[idx_P] <- 2 * (L - G)
    } else {
      w <- if (identical(objective, "cost")) 0 else objective
      scale <- mean(PI)^2
      if (!is.finite(scale) || scale <= 0) {
        scale <- 1
      }
      P_block <- 2 * w * scale * diag(n)
      if (lambda > 0) {
        P_block <- P_block + 2 * lambda * (lambdaMat + 1e-6 * diag(n))
      }
      q[idx_P] <- 2 * w * scale * (L - G)
      q[idx_I] <- (1 - w) * PI
      q[idx_E] <- -(1 - w) * PE_clipped
    }
    P_quad[idx_P, idx_P] <- P_block
    obj_scale <- max(max(abs(P_block)), max(abs(q)), 1e-6)
    sol <- solve_osqp(P_quad / obj_scale, q / obj_scale, A, lower, upper)
    if (is.null(sol$x)) {
      return(NULL)
    }
    x <- as.numeric(sol$x)
  }

  # OSQP declares an infeasible problem as such, but its accuracy is absolute,
  # so check the power it returns against the band once more on the exact
  # dynamics before trusting it.
  P <- pmin(pmax(x[idx_P], lb_P), ub_P)
  level <- tank_level_path(P, Q, cop, S0, k, dt)[-1]
  tol <- 1e-3
  if (
    any(level < Smin - tol) ||
      any(level > Smax + tol) ||
      level[n] < S0 - tol
  ) {
    return(NULL)
  }
  P
}


#' Check a single number argument
#' @keywords internal
tank_check_number <- function(x, name, min = -Inf, max = Inf, min_open = FALSE) {
  ok <- is.numeric(x) && length(x) == 1 && !is.na(x) && is.finite(x) &&
    x <= max && (if (min_open) x > min else x >= min)
  if (!ok) {
    range <- paste0(
      if (min_open) "greater than " else "at least ",
      min,
      if (is.finite(max)) paste0(" and at most ", max) else ""
    )
    stop(sprintf("Error: `%s` must be a number %s.", name, range), call. = FALSE)
  }
  invisible(x)
}


#' Check a numeric column of `opt_data`
#' @keywords internal
tank_check_column <- function(opt_data, name, non_negative = TRUE) {
  if (!(name %in% names(opt_data))) {
    stop(
      sprintf("Error: `opt_data` must contain a `%s` column.", name),
      call. = FALSE
    )
  }
  x <- opt_data[[name]]
  if (!is.numeric(x) || anyNA(x) || any(!is.finite(x))) {
    stop(
      sprintf("Error: `%s` in `opt_data` must be numeric without NA.", name),
      call. = FALSE
    )
  }
  if (non_negative && any(x < 0)) {
    stop(
      sprintf("Error: `%s` in `opt_data` must not be negative.", name),
      call. = FALSE
    )
  }
  invisible(x)
}


# Public API -------------------------------------------------------------------

#' Heat pump with a thermal tank: optimal compressor profile
#'
#' Optimises the electric power of a heat pump's compressor that charges one
#' thermal tank, which the heat demand discharges. It mirrors
#' [add_battery_optimization()]: the same `opt_data` conventions, objectives
#' (except `"capacity"`), optimization windows and solvers, with the tank in
#' place of the battery. The compressor's heat is `cop * P`, so there is no
#' conversion efficiency: the COP is the conversion.
#'
#' Per slot of `dt` hours, with `S` the level in kWh thermal above the band's
#' bottom and `k = (1 - loss/100)^dt`:
#'
#' - `S[t] = k * S[t-1] + (cop[t] * P[t] - heat_demand[t]) * dt`
#' - `Tcap * SOCmin/100 <= S[t] <= Tcap * SOCmax/100`
#' - `0 <= P[t] <= Pc`, and `P[t] = 0` where `cop[t] = 0`
#' - every window ends at least as full as it started.
#'
#' The run starts from `Tcap * SOCini/100`, and the level carries from one
#' window to the next. Slots outside every window (before the first
#' `window_start_hour`, after the last full window or past
#' `flex_window_hours`) run the `flexible` schedule.
#'
#' A window that has no solution under the grid capacities is solved again with
#' the capacities relaxed as far as the `flexible` schedule's own net flow
#' needs. If it still has none, the tank cannot hold its band under `Pc` or
#' cannot reach the end condition: the window runs `flexible`, its level follows
#' the balance unclamped and can leave the band, and its first `datetime` is
#' reported in the `infeasible_windows` attribute.
#'
#' At the first slot of every window the level is clamped into the band, so a
#' level that a fallback window or the slots outside every window left outside
#' it does not make the window infeasible from its first slot. The clamp leaves
#' alone a level inside the band, or outside it by less than half a rounding
#' step of the returned power (`0.005 * max(cop) * dt` kWh), and is reported per
#' row in the `tank_clamp_kwh` attribute: the level before the clamp minus the
#' level after it, so positive where it removed heat above `SOCmax` and negative
#' where it topped the tank up below `SOCmin`. Over the run,
#' `sum(cop * P * dt) = sum(heat_demand * dt) + losses + (end level - initial
#' level) + sum(tank_clamp_kwh)`; [get_tank_level()] reproduces the level.
#'
#' Inputs out of range stop with an error naming the argument; nothing is
#' clamped into range, and no zero profile is returned in their place.
#'
#' @param opt_data tibble, optimization contextual data. The first column must
#'   be named `datetime` (mandatory). Required columns:
#'
#' - `heat_demand`: heat the tank must deliver (kW thermal, >= 0)
#' - `cop`: heat per electric kWh (>= 0); 0 means the compressor cannot serve
#'   the tank in that slot
#' - `flexible`: the static electric schedule of the heat pump (kW, >= 0),
#'   returned wherever the function does not optimise
#'
#' Optional columns, as in [add_battery_optimization()]: `static`, `production`,
#' `import_capacity`, `export_capacity`, `price_imported`, `price_exported`.
#' @param opt_objective character or numeric. `"grid"` (default), `"cost"`, or a
#'   numeric weight `w` in `[0, 1]` where `w = 1` is pure grid and `w = 0` is
#'   pure cost. `"grid"` minimises the squared net grid flow
#'   `static + P - production`; `"cost"` minimises the energy cost of it.
#'   `"capacity"` has no tank form and stops with an error.
#' @param Tcap numeric, tank capacity (kWh thermal, > 0): the heat between the
#'   band's bottom and top.
#' @param Pc numeric, compressor cap (kW electric, > 0).
#' @param SOCmin numeric, minimum State-of-Charge (%), the comfort floor.
#' @param SOCmax numeric, maximum State-of-Charge (%).
#' @param SOCini numeric, State-of-Charge (%) at the start of the run, within
#'   `[SOCmin, SOCmax]`. `NULL` (default) means `SOCmax`, unlike the battery: a
#'   tank starting at its floor has no reserve for its first window.
#' @param window_days integer, optimization window length in days.
#' @param window_start_hour integer, start hour of each optimization window.
#' @param flex_window_hours numeric, flexibility window length (hours).
#' @param lambda numeric, ramping penalty weight on changes of `P` between
#'   consecutive slots.
#' @param loss numeric, standing loss of the tank's level (% per hour, >= 0), as
#'   in [get_storage_losses()].
#'
#' @return numeric vector, the compressor's electric power (kW), one value per
#'   `opt_data` row, rounded to 2 decimals: the optimised power in optimised
#'   windows and `flexible` everywhere else. Attributes:
#'
#' - `infeasible_windows`: the `datetime` of the first slot of every window that
#'   fell back to `flexible` (empty when none did).
#' - `tank_clamp_kwh`: numeric vector (kWh thermal), one value per row, 0 except
#'   at the first slot of a window whose starting level was clamped.
#'
#' @export
#'
#' @importFrom dplyr %>% mutate
#' @importFrom timefully get_time_resolution
#'
#' @examples
#' library(dplyr)
#' opt_data <- tibble(
#'   datetime = as.POSIXct("2024-01-01", tz = "UTC") + 3600 * 0:23,
#'   production = 0,
#'   heat_demand = 1,
#'   cop = 3,
#'   flexible = 1 / 3,
#'   price_imported = ifelse(0:23 < 6, 0.1, 0.3),
#'   price_exported = 0
#' )
#' P <- add_tank_optimization(
#'   opt_data, opt_objective = "cost", Tcap = 3, Pc = 3
#' )
#' get_tank_level(
#'   P, opt_data$heat_demand, opt_data$cop, init = 3,
#'   clamp = attr(P, "tank_clamp_kwh")
#' )
#'
add_tank_optimization <- function(
  opt_data,
  opt_objective = "grid",
  Tcap,
  Pc,
  SOCmin = 0,
  SOCmax = 100,
  SOCini = NULL,
  window_days = 1,
  window_start_hour = 0,
  flex_window_hours = 24,
  lambda = 0,
  loss = 0
) {
  if (is.null(opt_data)) {
    stop("Error: `opt_data` parameter is empty.", call. = FALSE)
  }
  if (!("datetime" %in% names(opt_data))) {
    stop("Error: `datetime` variable must exist in `opt_data`", call. = FALSE)
  }
  for (col in c("heat_demand", "cop", "flexible")) {
    tank_check_column(opt_data, col)
  }
  for (col in intersect(c("static", "production"), names(opt_data))) {
    tank_check_column(opt_data, col, non_negative = FALSE)
  }

  if (identical(opt_objective, "capacity")) {
    stop(
      "Error: `opt_objective = \"capacity\"` has no tank form; use \"grid\", \"cost\" or a number in [0, 1].",
      call. = FALSE
    )
  }
  if (is.numeric(opt_objective)) {
    tank_check_number(opt_objective, "opt_objective", 0, 1)
    if (opt_objective <= 0) {
      opt_objective <- "cost"
    } else if (opt_objective >= 1) {
      opt_objective <- "grid"
    }
  } else if (
    !(is.character(opt_objective) &&
      length(opt_objective) == 1 &&
      opt_objective %in% c("grid", "cost"))
  ) {
    stop(
      "Error: `opt_objective` must be \"grid\", \"cost\" or a number in [0, 1].",
      call. = FALSE
    )
  }

  tank_check_number(Tcap, "Tcap", 0, min_open = TRUE)
  tank_check_number(Pc, "Pc", 0, min_open = TRUE)
  tank_check_number(SOCmin, "SOCmin", 0, 100)
  tank_check_number(SOCmax, "SOCmax", 0, 100)
  if (SOCmin >= SOCmax) {
    stop("Error: `SOCmin` must be lower than `SOCmax`.", call. = FALSE)
  }
  if (is.null(SOCini)) {
    SOCini <- SOCmax
  }
  tank_check_number(SOCini, "SOCini", SOCmin, SOCmax)
  tank_check_number(loss, "loss", 0, 100)
  if (loss >= 100) {
    stop("Error: `loss` must be lower than 100 (%/hour).", call. = FALSE)
  }
  tank_check_number(lambda, "lambda", 0)

  opt_data <- check_optimization_data(opt_data, opt_objective)

  dttm_seq <- opt_data$datetime
  n_rows <- nrow(opt_data)
  time_resolution <- get_time_resolution(dttm_seq)
  dt <- time_resolution / 60
  k <- (1 - loss / 100)^dt
  Smin <- Tcap * SOCmin / 100
  Smax <- Tcap * SOCmax / 100

  Q <- opt_data$heat_demand
  cop <- opt_data$cop
  flexible <- opt_data$flexible
  G <- opt_data$production
  L <- opt_data$static
  if (is.null(opt_data$price_imported)) {
    opt_data$price_imported <- 0
  }
  if (is.null(opt_data$price_exported)) {
    opt_data$price_exported <- 0
  }
  import_capacity <- as.numeric(rep_len(opt_data$import_capacity, n_rows))
  export_capacity <- as.numeric(rep_len(opt_data$export_capacity, n_rows))

  # Levels within half a rounding step of the returned power of the band are
  # rounding, not a level to restore.
  clamp_tol <- 0.005 * max(cop) * dt + 1e-9

  flex_windows <- get_flex_windows(
    dttm_seq = dttm_seq,
    window_days = window_days,
    window_start_hour = window_start_hour,
    flex_window_hours = flex_window_hours
  )

  reset_message_once()

  P_out <- round(flexible, 2)
  clamp <- numeric(n_rows)
  infeasible <- integer(0)
  level <- Tcap * SOCini / 100 # level at the start of slot `pos`
  pos <- 1L

  advance <- function(level, idx) {
    if (!length(idx)) {
      return(level)
    }
    path <- tank_level_path(P_out[idx], Q[idx], cop[idx], level, k, dt)
    path[length(path)]
  }

  for (w in seq_len(nrow(flex_windows))) {
    start <- flex_windows$start[w]
    idx <- flex_windows$flex_idx[[w]]

    # Slots before the window run `flexible`, unclamped
    if (start > pos) {
      level <- advance(level, seq(pos, start - 1))
    }

    # Clamp the starting level into the band
    S0 <- min(max(level, Smin), Smax)
    if (abs(level - S0) > clamp_tol) {
      clamp[start] <- level - S0
      level <- S0
    }

    window_args <- list(
      G = G[idx],
      L = L[idx],
      Q = Q[idx],
      cop = cop[idx],
      PI = opt_data$price_imported[idx],
      PE = opt_data$price_exported[idx],
      import_capacity = import_capacity[idx],
      export_capacity = export_capacity[idx],
      S0 = S0,
      Smin = Smin,
      Smax = Smax,
      Pc = Pc,
      k = k,
      dt = dt,
      objective = opt_objective,
      lambda = lambda
    )
    P_window <- do.call(tank_solve_window, window_args)

    if (is.null(P_window)) {
      # Relax the capacities only as far as the static schedule's own net
      # flow needs them, as `optimization_slack_ceiling()` bounds the battery.
      net_static <- L[idx] + flexible[idx] - G[idx]
      import_relaxed <- pmax(import_capacity[idx], net_static)
      export_relaxed <- pmax(export_capacity[idx], -net_static)
      if (
        any(import_relaxed != import_capacity[idx]) ||
          any(export_relaxed != export_capacity[idx])
      ) {
        window_args$import_capacity <- import_relaxed
        window_args$export_capacity <- export_relaxed
        P_window <- do.call(tank_solve_window, window_args)
        if (!is.null(P_window)) {
          message_once(
            "\u26a0\ufe0f Optimization warning: grid capacity not reachable in some windows. Relaxing it to the static schedule's net flow there."
          )
        }
      }
    }

    if (is.null(P_window)) {
      message_once(
        "\u26a0\ufe0f Optimization warning: the tank cannot hold its band under `Pc` in some windows. Using the `flexible` schedule there."
      )
      infeasible <- c(infeasible, start)
    } else {
      target <- tank_level_path(P_window, Q[idx], cop[idx], S0, k, dt)[-1]
      P_out[idx] <- tank_round_power(
        target,
        Q[idx],
        cop[idx],
        level,
        k,
        dt,
        Pc
      )
    }

    level <- advance(level, idx)
    pos <- max(idx) + 1L
  }

  attr(P_out, "infeasible_windows") <- dttm_seq[infeasible]
  attr(P_out, "tank_clamp_kwh") <- clamp
  P_out
}


#' Tank level (thermal energy)
#'
#' The tank's counterpart of [get_storage_level()]: the level in kWh thermal at
#' the start of each slot, starting from `init`, with
#' `S[t+1] = k * S[t] + (cop[t] * power[t] - heat_demand[t]) * dt - clamp[t+1]`
#' and `k = (1 - loss/100)^dt`, `dt` being the time resolution in hours.
#'
#' The function applies only the clamps it is given and never clamps on its
#' own, so where the static schedule runs the level can leave the band.
#'
#' @param power numeric vector, compressor electric power (kW), such as the
#'   result of [add_tank_optimization()]
#' @param heat_demand numeric vector, heat demand (kW thermal)
#' @param cop numeric vector, coefficient of performance (heat per electric
#'   kWh)
#' @param init numeric, level before the first slot (in kWh, not %)
#' @param loss numeric, standing loss of the level (% per hour)
#' @param time_resolution numeric, time resolution of the time-series (in
#'   minutes)
#' @param clamp numeric, heat taken out of the tank at the start of each slot
#'   (kWh): `attr(result, "tank_clamp_kwh")` of [add_tank_optimization()], or 0
#'   for a schedule that was never clamped
#'
#' @return numeric vector of the level (kWh), rounded to 2 decimals
#' @export
#'
get_tank_level <- function(
  power,
  heat_demand,
  cop,
  init,
  loss = 0,
  time_resolution = 60,
  clamp = 0
) {
  n <- length(power)
  for (arg in c("heat_demand", "cop", "clamp")) {
    value <- get(arg)
    if (!is.numeric(value) || !(length(value) %in% c(1, n)) || anyNA(value)) {
      stop(
        sprintf(
          "Error: `%s` must be numeric without NA, of length 1 or `length(power)`.",
          arg
        ),
        call. = FALSE
      )
    }
  }
  if (!is.numeric(power) || anyNA(power)) {
    stop("Error: `power` must be numeric without NA.", call. = FALSE)
  }
  tank_check_number(init, "init")
  tank_check_number(loss, "loss", 0, 100)
  if (loss >= 100) {
    stop("Error: `loss` must be lower than 100 (%/hour).", call. = FALSE)
  }
  tank_check_number(time_resolution, "time_resolution", 0, min_open = TRUE)

  dt <- time_resolution / 60
  k <- (1 - loss / 100)^dt
  level <- tank_level_path(
    power,
    rep_len(heat_demand, n),
    rep_len(cop, n),
    init,
    k,
    dt,
    rep_len(clamp, n)
  )
  round(level[seq_len(n)], 2)
}
