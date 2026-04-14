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
#'
#' @param opt_objective character or numeric.
#' Optimization objective can be `"grid"` (default) or `"capacity"`
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
#'     production = "solar", static = "building"
#'   ) %>%
#'   select(any_of(c(
#'     "datetime", "production", "static", "price_imported", "price_exported"
#'   )))
#'   opt_battery <- opt_data %>%
#'     add_battery_optimization_qp(
#'       opt_objective = "grid",
#'       Bcap = 50, Bc = 4, Bd = 4,
#'       window_start_hour = 5
#'     )
#'
add_battery_optimization_qp <- function(
    opt_data,
    opt_objective = "grid",
    Bcap,
    Bc,
    Bd,
    SOCmin = 0,
    SOCmax = 100,
    SOCini = NULL,
    window_days = 1,
    window_start_hour = 0,
    flex_window_hours = 24,
    lambda = 0
) {
    # Parameters check
    if (is.null(opt_data)) {
        stop("Error: `opt_data` parameter is empty.")
    }
    opt_data <- opt_data %>% mutate(flexible = 0)
    opt_data <- check_optimization_data(opt_data, opt_objective)

    if (Bcap == 0 | Bc == 0 | Bd == 0 | SOCmin == SOCmax) {
        message(
            "\u26A0\uFE0F Warning: battery parameters don't allow optimization."
        )
        return(rep(0, nrow(opt_data)))
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

    reset_message_once()

    # Optimization
    if (opt_objective == "grid") {
        B_windows <- map(
            windows_data,
            ~ minimize_net_power_window_battery_qp(
                G = .x$production,
                L = .x$static,
                Bcap = Bcap * 60 / time_resolution,
                Bc = Bc,
                Bd = Bd,
                SOCmin = SOCmin,
                SOCmax = SOCmax,
                SOCini = SOCini,
                import_capacity = .x$import_capacity,
                export_capacity = .x$export_capacity,
                lambda = lambda
            )
        )
    } else if (opt_objective == "capacity") {
        B_windows <- map(
            windows_data,
            ~ curtail_capacity_window_battery_qp(
                G = .x$production,
                L = .x$static,
                Bcap = Bcap * 60 / time_resolution,
                Bc = Bc,
                Bd = Bd,
                SOCmin = SOCmin,
                SOCmax = SOCmax,
                SOCini = SOCini,
                import_capacity = .x$import_capacity,
                export_capacity = .x$export_capacity,
                lambda = lambda
            )
        )
    } else {
        stop("Error: invalid `opt_objective`")
    }

    B <- as.numeric(unlist(B_windows))

    if (length(flex_windows_idxs_seq) == length(dttm_seq)) {
        return(B)
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
        return(B_flex$B)
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
#' @param lambda numeric, penalty on change for the flexible load.
#'
#' @importFrom dplyr  %>% tibble mutate summarise_all
#'
#' @return numeric vector
#' @keywords internal
#'
curtail_capacity_window_battery_qp <- function(
    G,
    L,
    Bcap,
    Bc,
    Bd,
    SOCmin,
    SOCmax,
    SOCini,
    import_capacity,
    export_capacity,
    lambda = 0
) {
    balance_sum <- tibble(
        consumption = L,
        production = G
    ) %>%
        get_energy_balance() %>%
        mutate(
            export_capacity = export_capacity,
            import_capacity = import_capacity,
            exported_over = pmax(.data$exported - .data$export_capacity, 0),
            imported_over = pmax(.data$imported - .data$import_capacity, 0)
        ) %>%
        summarise_all(sum)

    Bcap_curtail <- min(
        max(balance_sum$exported_over, balance_sum$imported_over),
        Bcap
    )

    if (Bcap_curtail == 0) {
        return(rep(0, length(G)))
    } else {
        minimize_net_power_window_battery_qp(
            G,
            L,
            Bcap_curtail,
            Bc,
            Bd,
            SOCmin,
            SOCmax,
            SOCini,
            import_capacity,
            export_capacity,
            lambda
        )
    }
}


battery_qp_zero_profile <- function(time_slots) {
    rep(0, time_slots)
}


battery_qp_infeasible_bounds <- function(lower, upper, tolerance = 1e-8) {
    any(lower > upper + tolerance)
}


battery_qp_clip <- function(x, lower, upper) {
    pmin(pmax(x, lower), upper)
}


battery_qp_try_heuristic <- function(
    target,
    lower,
    upper,
    Bcap,
    Bc,
    Bd,
    SOCmin,
    SOCmax,
    SOCini
) {
    time_slots <- length(target)
    storage <- 0
    profile <- numeric(time_slots)
    lower_storage <- rep((SOCmin - SOCini) / 100 * Bcap, time_slots)
    upper_storage <- rep((SOCmax - SOCini) / 100 * Bcap, time_slots)

    for (i in seq_len(time_slots)) {
        remaining_slots <- time_slots - i
        future_storage_lower <- -remaining_slots * Bc
        future_storage_upper <- remaining_slots * Bd
        storage_lower <- max(lower_storage[i], future_storage_lower)
        storage_upper <- min(upper_storage[i], future_storage_upper)
        step_lower <- max(lower[i], storage_lower - storage)
        step_upper <- min(upper[i], storage_upper - storage)

        if (step_lower > step_upper + 1e-8) {
            return(NULL)
        }

        profile[i] <- battery_qp_clip(target[i], step_lower, step_upper)
        storage <- storage + profile[i]
    }

    profile
}


battery_qp_solve_osqp <- function(P, q, A, lower, upper, time_slots) {
    solver <- osqp::osqp(
        P = P,
        q = q,
        A = A,
        l = lower,
        u = upper,
        pars = osqp::osqpSettings(
            verbose = FALSE,
            eps_abs = 1e-6,
            eps_rel = 1e-6,
            polishing = TRUE,
            max_iter = 50000
        )
    )
    result <- solver@Solve()

    list(
        result = result,
        profile = if (result$info$status_val %in% c(1, 2)) {
            as.numeric(result$x[seq_len(time_slots)])
        } else {
            NULL
        }
    )
}


#' Perform battery optimization (just a window)
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
#' @param P numeric matrix, optimization objective parameter
#' @param q numeric vector, optimization objective parameter
#'
#' @return numeric vector
#' @keywords internal
#'
solve_optimization_battery_window_qp <- function(
    G,
    L,
    Bcap,
    Bc,
    Bd,
    SOCmin,
    SOCmax,
    SOCini,
    import_capacity,
    export_capacity,
    P,
    q
) {
    # Round to 2 decimals to avoid problems with lower and upper bounds
    G <- round(G, 2)
    L <- round(L, 2)

    time_slots <- length(G)
    import_capacity <- as.numeric(rep_len(import_capacity, time_slots))
    export_capacity <- as.numeric(rep_len(export_capacity, time_slots))
    identityMat <- diag(time_slots)
    cumsumMat <- triangulate_matrix(matrix(1, time_slots, time_slots), "l")
    target <- G - L

    # Lower and upper bounds
    ## General bounds
    ##  - Grid capacity: -export_capacity <= B + L - G <= +import_capacity
    ##    - LB: B >= G - L - export_capacity
    ##    - UB: B <= G - L + import_capacity
    ##  - Battery power limits:
    ##    - LB: B >= -Bd
    ##    - UB: B <= Bc
    lb_B <- pmax(-Bd, G - L - export_capacity)
    ub_B <- pmin(Bc, G - L + import_capacity)
    relaxed_bounds <- FALSE

    if (battery_qp_infeasible_bounds(lb_B, ub_B)) {
        message_once(
            "\u26A0\uFE0F Optimization warning: infeasible battery QP bounds. Removing grid constraints."
        )
        lb_B <- rep(-Bd, time_slots)
        ub_B <- rep(Bc, time_slots)
        relaxed_bounds <- TRUE
    }

    ## SOC limits
    Amat_cumsum <- cumsumMat
    lb_cumsum <- rep((SOCmin - SOCini) / 100 * Bcap, time_slots)
    ub_cumsum <- rep((SOCmax - SOCini) / 100 * Bcap, time_slots)

    ## Total sum of B == 0 (neutral balance)
    Amat_energy <- matrix(1, nrow = 1, ncol = time_slots)
    lb_energy <- 0
    ub_energy <- 0

    Amat <- rbind(
        identityMat,
        cumsumMat,
        Amat_energy
    )

    solve_once <- function(lower_B, upper_B) {
        lower <- round(c(lower_B, lb_cumsum, lb_energy), 2)
        upper <- round(c(upper_B, ub_cumsum, ub_energy), 2)
        battery_qp_solve_osqp(P, q, Amat, lower, upper, time_slots)
    }

    solution <- solve_once(lb_B, ub_B)
    if (!is.null(solution$profile)) {
        return(solution$profile)
    }

    heuristic <- battery_qp_try_heuristic(
        target = target,
        lower = lb_B,
        upper = ub_B,
        Bcap = Bcap,
        Bc = Bc,
        Bd = Bd,
        SOCmin = SOCmin,
        SOCmax = SOCmax,
        SOCini = SOCini
    )
    if (!is.null(heuristic)) {
        message_once(paste0(
            "\u26A0\uFE0F Optimization warning: ",
            solution$result$info$status,
            ". Using heuristic battery profile for this window."
        ))
        return(heuristic)
    }

    if (!relaxed_bounds) {
        message_once(
            "\u26A0\uFE0F Optimization warning: optimization not feasible in some windows. Removing grid constraints."
        )
        lb_B <- rep(-Bd, time_slots)
        ub_B <- rep(Bc, time_slots)

        solution <- solve_once(lb_B, ub_B)
        if (!is.null(solution$profile)) {
            return(solution$profile)
        }

        heuristic <- battery_qp_try_heuristic(
            target = target,
            lower = lb_B,
            upper = ub_B,
            Bcap = Bcap,
            Bc = Bc,
            Bd = Bd,
            SOCmin = SOCmin,
            SOCmax = SOCmax,
            SOCini = SOCini
        )
        if (!is.null(heuristic)) {
            message_once(paste0(
                "\u26A0\uFE0F Optimization warning: ",
                solution$result$info$status,
                ". Using heuristic battery profile for this window."
            ))
            return(heuristic)
        }
    }

    message_once(paste0(
        "\u26A0\uFE0F Optimization warning: ",
        solution$result$info$status,
        ". Disabling battery for this window."
    ))
    battery_qp_zero_profile(time_slots)
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
#' @param lambda numeric, penalty on change for the flexible load.
#'
#' @return numeric vector
#' @keywords internal
#'
minimize_net_power_window_battery_qp <- function(
    G,
    L,
    Bcap,
    Bc,
    Bd,
    SOCmin,
    SOCmax,
    SOCini,
    import_capacity,
    export_capacity,
    lambda = 0
) {
    time_slots <- length(G)
    identityMat <- diag(time_slots)
    lambdaMat <- get_lambda_matrix(time_slots)

    # Objective function terms
    P <- 2 * (identityMat + lambda * lambdaMat)
    q <- 2 * (L - G)

    B <- solve_optimization_battery_window_qp(
        G,
        L,
        Bcap,
        Bc,
        Bd,
        SOCmin,
        SOCmax,
        SOCini,
        import_capacity,
        export_capacity,
        P,
        q
    )

    return(B)
}
