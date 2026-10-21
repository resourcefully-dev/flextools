library(dplyr)

# Use first 50 sessions
sessions <- evsim::california_ev_sessions_profiles %>%
    slice_head(n = 100) %>%
    mutate(Power = Power * 2) |>
    evsim::adapt_charging_features(time_resolution = 15) |>
    filter(
        Profile == "Worktime",
        date(ConnectionStartDateTime) == as.Date("2018-10-08")
    )
sessions_demand <- evsim::get_demand(sessions, resolution = 15) |>
    select(datetime, Worktime)

# Don't require any other variable than datetime, since we don't
# care about local generation (just peak shaving objective)
opt_data <- tibble(
    datetime = sessions_demand$datetime,
    production = 0,
    import_capacity = c(
        rep(
            c(
                rep(100, 60 / 15 * 7), # 00:00-07:00
                rep(-20, 60 / 15 * (11 - 7)), # 07:00-11:00
                rep(100, 60 / 15 * (24 - 11)) # 11:00-24:00
            ),
            times = 1
        ),
        100
    ) # 3 days + 1 extra time step
)
# timefully::plot_ts(opt_data)

# The most the connected sessions can discharge in each slot: their nominal
# power over their connection. The -20 kW import cap starts at 07:00, when only
# a few vehicles are plugged in, so the setpoint can only approach it there.
connected_power <- sessions %>%
    mutate(
        ChargingStartDateTime = ConnectionStartDateTime,
        ChargingEndDateTime = ConnectionEndDateTime,
        ChargingHours = ConnectionHours,
        Energy = Power * ConnectionHours
    ) %>%
    evsim::get_demand(dttm_seq = opt_data$datetime) %>%
    pull(Worktime)

test_that("optimization for v2g works and negative import capacity is achieved", {
    v2g_results <- smart_v2g(
        sessions = sessions,
        opt_data,
        opt_objective = "grid",
        window_days = 1,
        window_start_hour = 0,
        show_progress = TRUE,
        include_log = TRUE
    )
    # plot_smart_charging(v2g_results, sessions, legend_width = 150)
    # view_smart_charging_logs(v2g_results)

    # check that setpoints are always lower than the import capacity, as far
    # as the connected vehicles can reach it
    expect_true(
        all(
            v2g_results$setpoints$Worktime <=
                pmax(opt_data$import_capacity, -connected_power) + 1e-2
        )
    )
    # and never discharge more than the connected vehicles can
    expect_true(all(v2g_results$setpoints$Worktime >= -connected_power - 1e-2))

    # check that negative import capacity is achieved
    neg_idx <- which(opt_data$import_capacity < 0)
    expect_true(any(v2g_results$demand$Worktime[neg_idx] < 0))
})

test_that("v2g grid optimization also works with explicit export capacity", {
    opt_data_explicit_export <- opt_data %>%
        mutate(export_capacity = 100)

    v2g_results <- smart_v2g(
        sessions = sessions,
        opt_data_explicit_export,
        opt_objective = "grid",
        window_days = 1,
        window_start_hour = 0
    )

    expect_true(
        all(
            v2g_results$setpoints$Worktime <=
                pmax(
                    opt_data_explicit_export$import_capacity,
                    -connected_power
                ) +
                    1e-2
        )
    )

    neg_idx <- which(opt_data_explicit_export$import_capacity < 0)
    expect_true(any(v2g_results$demand$Worktime[neg_idx] < 0))
})

test_that("profile setpoints in opt_data are used directly in v2g", {
    opt_data_profile <- tibble(
        datetime = sessions_demand$datetime,
        production = 0,
        Worktime = round(sessions_demand$Worktime * 0.5, 2)
    )

    v2g_results <- smart_v2g(
        sessions = sessions,
        opt_data_profile,
        opt_objective = "none",
        window_days = 1,
        window_start_hour = 0,
        energy_min = 0
    )

    expect_equal(
        v2g_results$setpoints$Worktime,
        opt_data_profile$Worktime,
        tolerance = 1e-6
    )
})

test_that("energy_min controls capacity compliance vs energy charged in v2g", {
    opt_data_capacity <- tibble(
        datetime = sessions_demand$datetime,
        production = 0,
        import_capacity = round(sessions_demand$Worktime * 0.2, 2),
        export_capacity = 0
    )

    v2g_full_energy <- smart_v2g(
        sessions = sessions,
        opt_data_capacity,
        opt_objective = "none",
        window_days = 1,
        window_start_hour = 0,
        energy_min = 1
    )

    demand_full <- rowSums(
        select(v2g_full_energy$demand, -datetime)
    )
    expect_true(
        any(demand_full > opt_data_capacity$import_capacity + 1e-6)
    )

    energy_full <- summarise_energy_charged(v2g_full_energy, sessions)
    expect_false(any(is.na(energy_full$PctEnergyCharged)))
    expect_equal(
        nrow(filter(energy_full, PctEnergyCharged < 99)),
        0
    )

    v2g_min_energy <- smart_v2g(
        sessions = sessions,
        opt_data_capacity,
        opt_objective = "none",
        window_days = 1,
        window_start_hour = 0,
        energy_min = 0
    )
    # plot_smart_charging(v2g_min_energy, sessions, legend_width = 150)

    demand_min <- rowSums(
        select(v2g_min_energy$demand, -datetime)
    )
    expect_true(
        all(demand_min <= opt_data_capacity$import_capacity + 1e-6)
    )

    energy_min <- summarise_energy_charged(v2g_min_energy, sessions)
    avg_pct <- mean(
        energy_min$EnergyCharged / energy_min$EnergyRequired * 100
    )
    expect_lt(avg_pct, 100)
})

test_that("power_th allows limited capacity exceedance in v2g", {
    power_th <- 0.1
    opt_data_threshold <- tibble(
        datetime = sessions_demand$datetime,
        production = 0,
        import_capacity = 25,
        export_capacity = 0
    )

    v2g_threshold <- smart_v2g(
        sessions = sessions,
        opt_data_threshold,
        opt_objective = "none",
        window_days = 1,
        window_start_hour = 0,
        power_th = power_th,
        energy_min = 0
    )
    # plot_smart_charging(v2g_threshold, sessions, legend_width = 150)

    demand_threshold <- rowSums(
        select(v2g_threshold$demand, -datetime)
    )
    expect_true(
        any(demand_threshold > opt_data_threshold$import_capacity + 1e-6)
    )
    expect_true(
        all(
            demand_threshold <=
                opt_data_threshold$import_capacity * (1 + power_th + 1e-3) # Some tolerance
        )
    )
})

test_that("charging_power_min enforces minimum absolute power in v2g", {
    charging_power_min <- 2
    sessions_min_power <- sessions %>%
        filter(.data$Power >= charging_power_min)

    demand_min_power <- evsim::get_demand(
        sessions_min_power,
        resolution = 15
    )
    opt_data_min_power <- tibble(
        datetime = demand_min_power$datetime,
        production = 0,
        import_capacity = 5,
        export_capacity = 0
    )

    v2g_min_power <- smart_v2g(
        sessions = sessions_min_power,
        opt_data_min_power,
        opt_objective = "none",
        window_days = 1,
        window_start_hour = 0,
        charging_power_min = charging_power_min,
        energy_min = 0
    )
    # plot_smart_charging(v2g_min_power, sessions_min_power, legend_width = 150)

    exploited_non_zero <- v2g_min_power$sessions %>%
        filter(.data$Exploited) %>%
        filter(.data$Power != 0)
    # Exclude the last time step of each session if it has low power,
    # since it might be lower (average timeslot power) than real charging power
    exploited_filtered <- exploited_non_zero %>%
        group_by(.data$Session) %>%
        mutate(MaxEnd = max(.data$ChargingEndDateTime)) %>%
        ungroup() %>%
        filter(
            !(.data$ChargingEndDateTime == .data$MaxEnd &
                abs(.data$Power) < charging_power_min)
        )
    expect_gt(nrow(exploited_filtered), 0)
    expect_true(
        all(abs(exploited_filtered$Power) >= charging_power_min - 1e-6)
    )
})

test_that("capacity objective works in v2g", {
    opt_data_cap <- opt_data %>%
        mutate(import_capacity = 25, export_capacity = 0)

    capacity_results <- smart_v2g(
        sessions = sessions,
        opt_data_cap,
        opt_objective = "capacity",
        window_days = 1,
        window_start_hour = 0,
        responsive = list(Workday = list(Worktime = 1))
    )

    expect_type(capacity_results, "list")
    expect_equal(
        trunc(sum(sessions$Energy) - sum(capacity_results$sessions$Energy)),
        0
    )
})

test_that("capacity_v2g_window relaxes minimally when a window is infeasible", {
    # Two 10 kW spikes above the 2 kW import cap with no room to shift
    # (time_horizon = 1) make the capacity slice LP infeasible, forcing the
    # minimal-relaxation fallback. LFmax below the LF peak also exercises the
    # ub_O clamp in the v2g solver retry.
    n <- 6
    G <- rep(0, n)
    LS <- rep(0, n)
    LF <- c(10, 0, 0, 10, 0, 0)
    import_capacity <- rep(2, n)
    export_capacity <- rep(0, n)

    O <- suppressMessages(flextools:::capacity_v2g_window(
        G = G, LF = LF, LS = LS, direction = "forward",
        time_horizon = 1L, LFmax = 5,
        import_capacity = import_capacity, export_capacity = export_capacity
    ))

    relax_tol <- 0.02
    net_import <- pmax(O + LS - G, 0)
    orig_import <- pmax(LF + LS - G, 0)

    # Never worse than the input profile had.
    expect_true(all(net_import <= pmax(import_capacity, orig_import) + relax_tol))
    # Slots within their cap stay hard-capped at the true capacity.
    within <- orig_import <= import_capacity
    expect_true(all(net_import[within] <= import_capacity[within] + relax_tol))
    # Flexible energy is preserved.
    expect_equal(sum(O), sum(LF), tolerance = 1e-6)
    # The relaxation actually optimizes; it does not hit the crash guard (LF).
    expect_false(isTRUE(all.equal(as.numeric(O), LF)))
})

test_that("cost objective currently reuses grid objective in v2g", {
    opt_data_prices <- opt_data %>%
        mutate(
            price_imported = 1,
            price_exported = 0
        )

    grid_results <- smart_v2g(
        sessions = sessions,
        opt_data_prices,
        opt_objective = "grid",
        window_days = 1,
        window_start_hour = 0
    )

    cost_results <- NULL
    expect_message(
        cost_results <- smart_v2g(
            sessions = sessions,
            opt_data_prices,
            opt_objective = "cost",
            window_days = 1,
            window_start_hour = 0
        ),
        "reusing grid objective"
    )

    expect_equal(
        cost_results$setpoints$Worktime,
        grid_results$setpoints$Worktime,
        tolerance = 1e-6
    )
    expect_equal(
        cost_results$demand$Worktime,
        grid_results$demand$Worktime,
        tolerance = 1e-6
    )
})

test_that("combined objective currently reuses grid objective in v2g", {
    opt_data_prices <- opt_data %>%
        mutate(
            price_imported = 1,
            price_exported = 0
        )

    grid_results <- smart_v2g(
        sessions = sessions,
        opt_data_prices,
        opt_objective = "grid",
        window_days = 1,
        window_start_hour = 0
    )

    combined_results <- NULL
    expect_message(
        combined_results <- smart_v2g(
            sessions = sessions,
            opt_data_prices,
            opt_objective = 0.5,
            window_days = 1,
            window_start_hour = 0
        ),
        "reusing grid objective"
    )

    expect_equal(
        combined_results$setpoints$Worktime,
        grid_results$setpoints$Worktime,
        tolerance = 1e-6
    )
    expect_equal(
        combined_results$demand$Worktime,
        grid_results$demand$Worktime,
        tolerance = 1e-6
    )
})

test_that("energy_max caps the energy charged in v2g, fleet and per session", {
    required <- sum(sessions$Energy)

    for (objective in c("grid", "capacity")) {
        v2g_results <- smart_v2g(
            sessions = sessions,
            opt_data,
            opt_objective = objective,
            window_days = 1,
            window_start_hour = 0,
            energy_min = 0.8,
            energy_max = 0.8
        )

        # The setpoint targets 80% of the responsive sessions' energy
        expect_equal(
            sum(v2g_results$setpoints$Worktime) * 15 / 60,
            0.8 * required,
            tolerance = 0.01
        )

        # The fleet charges 80% of its energy, net of discharge
        expect_equal(
            sum(v2g_results$sessions$Energy),
            0.8 * required,
            tolerance = 0.01
        )
        expect_equal(
            sum(v2g_results$demand$Worktime) * 15 / 60,
            0.8 * required,
            tolerance = 0.01
        )

        # And so does every session
        energy <- summarise_energy_charged(v2g_results, sessions)
        expect_false(any(is.na(energy$EnergyCharged)))
        pct <- energy$EnergyCharged / energy$EnergyRequired * 100
        expect_true(all(pct > 79 & pct < 81))
    }
})

test_that("energy_max stops a session at its share even after discharging", {
    v2g_results <- smart_v2g(
        sessions = sessions,
        opt_data,
        opt_objective = "grid",
        window_days = 1,
        window_start_hour = 0,
        energy_min = 0.8,
        energy_max = 0.8,
        discharge_eff = 0.9
    )
    # Discharging happened, so the losses were charged back on top of the 80%
    neg_idx <- which(opt_data$import_capacity < 0)
    expect_true(any(v2g_results$demand$Worktime[neg_idx] < 0))

    energy <- summarise_energy_charged(v2g_results, sessions)
    pct <- energy$EnergyCharged / energy$EnergyRequired * 100
    expect_true(all(pct > 79))
    expect_true(all(pct < 0.8 / 0.9 * 100 + 1))
})

test_that("energy_max = 1 keeps the v2g results unchanged", {
    v2g_default <- smart_v2g(
        sessions = sessions,
        opt_data,
        opt_objective = "grid",
        window_days = 1,
        window_start_hour = 0
    )
    v2g_max_1 <- smart_v2g(
        sessions = sessions,
        opt_data,
        opt_objective = "grid",
        window_days = 1,
        window_start_hour = 0,
        energy_max = 1
    )
    expect_equal(v2g_max_1, v2g_default)

    # Totals as produced by flextools 1.9.0, before `energy_max` existed, and
    # re-captured on 1.13.0: the setpoint no longer discharges before the
    # vehicles arrive, so the cap is only approached where it is unreachable.
    expect_equal(sum(v2g_max_1$demand$Worktime), 1020.98, tolerance = 1e-6)
    expect_equal(sum(abs(v2g_max_1$demand$Worktime)), 1633.94, tolerance = 1e-6)
    expect_equal(sum(v2g_max_1$setpoints$Worktime), 1020.89, tolerance = 1e-6)

    v2g_capacity <- smart_v2g(
        sessions = sessions,
        opt_data %>% mutate(import_capacity = round(sessions_demand$Worktime * 0.5, 2)),
        opt_objective = "capacity",
        window_days = 1,
        window_start_hour = 0,
        energy_max = 1
    )
    expect_equal(sum(v2g_capacity$demand$Worktime), 1020.86, tolerance = 1e-6)
    expect_equal(sum(v2g_capacity$setpoints$Worktime), 1020.88, tolerance = 1e-6)
})

test_that("the v2g setpoint drops energy down to energy_min before relaxing the grid capacity", {
    # 10 kWh in the first slot, a 2 kW import cap and three slots: at most 6 kWh
    # fit under the cap.
    LF <- c(10, 0, 0)
    args <- list(
        G = rep(0, 3), LF = LF, LS = rep(0, 3), direction = "forward",
        time_horizon = NULL, LFmax = Inf,
        import_capacity = rep(2, 3), export_capacity = rep(0, 3)
    )

    # A range down to 50% fits 6 kWh under the cap instead of relaxing it
    O <- do.call(
        flextools:::demand_grid_v2g_window,
        c(args, list(energy_ratio = c(0.5, 1)))
    )
    expect_equal(sum(O), 6, tolerance = 1e-3)
    expect_true(all(O <= 2 + 0.02))

    # The capacity objective takes the same route
    O <- do.call(
        flextools:::capacity_v2g_window,
        c(args, list(energy_ratio = c(0.5, 1)))
    )
    expect_equal(sum(O), 6, tolerance = 1e-3)
    expect_true(all(O <= 2 + 0.02))

    # When even the minimum does not fit, the cap is relaxed only as far as the
    # minimum-energy profile needs, and exactly the minimum is delivered
    O <- suppressMessages(do.call(
        flextools:::demand_grid_v2g_window,
        c(args, list(energy_ratio = c(0.8, 1)))
    ))
    expect_equal(sum(O), 8, tolerance = 1e-3)
    expect_true(all(O <= 8 + 0.02))

    # A range that fits keeps the maximum
    O <- do.call(
        flextools:::demand_grid_v2g_window,
        c(args, list(energy_ratio = c(0.2, 0.5)))
    )
    # Each slot is rounded to 2 decimals
    expect_equal(sum(O), 5, tolerance = 0.01)
})

test_that("smart_v2g validates energy_min and energy_max", {
    run_v2g <- function(...) {
        smart_v2g(
            sessions = sessions,
            opt_data,
            opt_objective = "grid",
            window_days = 1,
            window_start_hour = 0,
            ...
        )
    }
    expect_error(run_v2g(energy_min = 0, energy_max = 0), "energy_max")
    expect_error(run_v2g(energy_min = 0, energy_max = 1.2), "energy_max")
    expect_error(run_v2g(energy_min = 0, energy_max = -0.1), "energy_max")
    expect_error(run_v2g(energy_min = 0, energy_max = "0.8"), "energy_max")
    expect_error(run_v2g(energy_min = 0, energy_max = c(0.5, 0.8)), "energy_max")
    expect_error(run_v2g(energy_min = 0, energy_max = NA_real_), "energy_max")
    expect_error(run_v2g(energy_min = 1.5), "energy_min")
    expect_error(
        run_v2g(energy_min = 0.9, energy_max = 0.8),
        "cannot be higher than `energy_max`"
    )
    # The default `energy_min = 1` is above any `energy_max` below 1
    expect_error(run_v2g(energy_max = 0.8), "cannot be higher than `energy_max`")
})
