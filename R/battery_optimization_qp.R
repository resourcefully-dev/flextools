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


battery_qp_cumsum_matrix <- function(time_slots) {
    Matrix::sparseMatrix(
        i = sequence(time_slots:1, from = seq_len(time_slots)),
        j = rep(seq_len(time_slots), times = time_slots:1),
        x = 1,
        dims = c(time_slots, time_slots)
    )
}


battery_qp_lambda_matrix <- function(time_slots) {
    if (time_slots == 1) {
        return(Matrix::Matrix(1, nrow = 1, ncol = 1, sparse = TRUE))
    }

    Matrix::bandSparse(
        time_slots,
        k = c(-1, 0, 1),
        diagonals = list(
            rep(-1, time_slots - 1),
            c(1, rep(2, time_slots - 2), 1),
            rep(-1, time_slots - 1)
        )
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
    time_slots <- length(G)
    import_capacity <- as.numeric(rep_len(import_capacity, time_slots))
    export_capacity <- as.numeric(rep_len(export_capacity, time_slots))
    cumsumMat <- battery_qp_cumsum_matrix(time_slots)

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

    ## SOC limits
    Amat_cumsum <- cumsumMat
    lb_cumsum <- rep((SOCmin - SOCini) / 100 * Bcap, time_slots)
    ub_cumsum <- rep((SOCmax - SOCini) / 100 * Bcap, time_slots)

    ## Total sum of B == 0 (neutral balance)
    Amat_energy <- Matrix::Matrix(
        1,
        nrow = 1,
        ncol = time_slots,
        sparse = TRUE
    )
    lb_energy <- 0
    ub_energy <- 0

    if (battery_qp_infeasible_bounds(lb_B, ub_B)) {
        message_once(
            "\u26A0\uFE0F Optimization warning: infeasible battery QP bounds. Disabling battery for some windows."
        )
        return(battery_qp_zero_profile(time_slots))
    }

    Amat <- rbind(
        Matrix::Diagonal(time_slots),
        cumsumMat,
        Amat_energy
    )
    lb <- c(lb_B, lb_cumsum, lb_energy)
    ub <- c(ub_B, ub_cumsum, ub_energy)

    solver <- osqp::osqp(
        P = P,
        q = q,
        A = Amat,
        l = lb,
        u = ub,
        pars = osqp::osqpSettings(
            verbose = FALSE,
            eps_abs = 1e-6,
            eps_rel = 1e-6,
            polishing = TRUE
        )
    )
    result <- solver@Solve()

    if (result$info$status_val %in% c(1, 2)) {
        return(as.numeric(result$x[seq_len(time_slots)]))
    }

    # If it's not feasible, then remove grid constraints
    message_once(
        "\u26A0\uFE0F Optimization warning: optimization not feasible in some windows. Removing grid constraints."
    )
    lb_B <- rep(-Bd, time_slots)
    ub_B <- rep(Bc, time_slots)

    if (battery_qp_infeasible_bounds(lb_B, ub_B)) {
        message_once(
            "\u26A0\uFE0F Optimization warning: infeasible battery QP bounds. Disabling battery for some windows."
        )
        return(battery_qp_zero_profile(time_slots))
    }

    lb <- c(lb_B, lb_cumsum, lb_energy)
    ub <- c(ub_B, ub_cumsum, ub_energy)

    solver <- osqp::osqp(
        P = P,
        q = q,
        A = Amat,
        l = lb,
        u = ub,
        pars = osqp::osqpSettings(
            verbose = FALSE,
            eps_abs = 1e-6,
            eps_rel = 1e-6,
            polishing = TRUE
        )
    )
    result <- solver@Solve()

    if (result$info$status_val %in% c(1, 2)) {
        return(as.numeric(result$x[seq_len(time_slots)]))
    }

    message_once(paste0(
        "\u26A0\uFE0F Optimization warning: ",
        result$info$status,
        ". Disabling battery for some windows."
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
    lambdaMat <- battery_qp_lambda_matrix(time_slots)

    # Objective function terms
    P <- as(
        Matrix::Diagonal(time_slots) + lambda * lambdaMat,
        "dgCMatrix"
    ) *
        2
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
