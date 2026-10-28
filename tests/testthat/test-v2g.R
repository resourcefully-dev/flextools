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

# A night-and-evening case for the price objectives: three vehicles plug in
# from 17:00 with a high arrival SoC (so they can discharge more than the
# site's evening load), the import price is cheap at night (0.10, 00:00-07:00)
# and expensive in the evening (0.40, 17:00-21:00), 0.25 otherwise.
v2g_price_case <- function(price_exported = NULL) {
    dttm <- seq(
        as.POSIXct("2025-03-03 12:00", tz = "UTC"),
        as.POSIXct("2025-03-04 11:45", tz = "UTC"),
        by = "15 min"
    )
    h <- as.numeric(format(dttm, "%H")) + as.numeric(format(dttm, "%M")) / 60
    evening <- h >= 17 & h < 21
    night <- h < 7
    price_imported <- ifelse(night, 0.10, ifelse(evening, 0.40, 0.25))
    opt_data <- tibble(
        datetime = dttm,
        static = ifelse(evening, 4, 2),
        production = 0,
        import_capacity = 30,
        export_capacity = 20,
        price_imported = price_imported,
        price_exported = if (is.null(price_exported)) {
            price_imported
        } else {
            price_exported
        }
    )
    session <- function(id, start, hours, energy, soc) {
        tibble(
            Session = paste0("S", id),
            Timecycle = "Workday",
            Profile = "Fleet",
            ConnectionStartDateTime = as.POSIXct(
                "2025-03-03 00:00",
                tz = "UTC"
            ) +
                start * 3600,
            ConnectionHours = hours,
            Power = 11,
            Energy = energy,
            StateOfCharge = soc
        )
    }
    sessions <- bind_rows(
        session(1, 17, 14, 15, 70),
        session(2, 17.5, 14, 15, 70),
        session(3, 18, 13.5, 20, 60)
    )
    uncontrolled <- evsim::get_demand(
        evsim::adapt_charging_features(sessions, time_resolution = 15),
        dttm_seq = dttm
    )$Fleet
    list(
        opt_data = opt_data,
        sessions = sessions,
        evening = evening,
        night = night,
        uncontrolled = uncontrolled
    )
}

run_v2g_price_case <- function(case, objective, ...) {
    suppressMessages(suppressWarnings(smart_v2g(
        case$sessions,
        case$opt_data,
        opt_objective = objective,
        window_days = 1,
        window_start_hour = 12,
        soc_min = 20,
        ...
    )))
}

# Energy cost of the site, sum(import * PI - export * PE), for a fleet load
v2g_energy_cost <- function(case, fleet) {
    net <- case$opt_data$static - case$opt_data$production + fleet
    sum(
        pmax(net, 0) * case$opt_data$price_imported -
            pmax(-net, 0) * case$opt_data$price_exported
    ) *
        0.25
}

expect_v2g_sessions_energy <- function(case, results) {
    delivered <- results$sessions %>%
        group_by(Session) %>%
        summarise(Energy = sum(Energy))
    expect_equal(
        delivered$Energy[match(case$sessions$Session, delivered$Session)],
        case$sessions$Energy,
        tolerance = 0.05
    )
}

expect_v2g_breaches_not_worse <- function(case, fleet) {
    net_without_fleet <- case$opt_data$static - case$opt_data$production
    breaches <- function(load) {
        net <- net_without_fleet + load
        sum(
            net > case$opt_data$import_capacity + 0.05 |
                net < -case$opt_data$export_capacity - 0.05
        )
    }
    expect_lte(breaches(fleet), breaches(case$uncontrolled))
}

test_that("cost v2g charges in the cheap slots, discharges in the expensive ones and costs less than grid", {
    case <- v2g_price_case()
    grid_results <- run_v2g_price_case(case, "grid")
    cost_results <- NULL
    # No "not yet implemented" message any more.
    expect_no_message(
        cost_results <- smart_v2g(
            case$sessions,
            case$opt_data,
            opt_objective = "cost",
            window_days = 1,
            window_start_hour = 12,
            soc_min = 20
        )
    )
    fleet <- cost_results$demand$Fleet

    # Charges in the cheap night, discharges in the expensive evening.
    expect_true(all(fleet[case$evening] <= 0.05))
    expect_lt(sum(fleet[case$evening]), -10)
    charged <- sum(pmax(fleet, 0))
    expect_gt(sum(pmax(fleet[case$night], 0)), 0.95 * charged)

    # Cheaper than the grid objective (and than uncontrolled charging).
    cost_cost <- v2g_energy_cost(case, fleet)
    cost_grid <- v2g_energy_cost(case, grid_results$demand$Fleet)
    expect_lt(cost_cost, cost_grid)
    expect_lt(cost_grid, v2g_energy_cost(case, case$uncontrolled))
    # The setpoint is the cheapest of the two plans as well.
    expect_lt(
        v2g_energy_cost(case, cost_results$setpoints$Fleet),
        v2g_energy_cost(case, grid_results$setpoints$Fleet)
    )

    # Every session gets its energy, within the grid capacities.
    expect_v2g_sessions_energy(case, cost_results)
    expect_v2g_breaches_not_worse(case, fleet)
    net <- case$opt_data$static + fleet
    expect_true(all(net <= case$opt_data$import_capacity + 0.05))
    expect_true(all(net >= -case$opt_data$export_capacity - 0.05))
})

test_that("cost v2g discharges into export only when the export price pays", {
    # Export price equal to the import price (net metering): exporting at
    # 0.40 in the evening and charging back at 0.10 at night pays, so the
    # fleet discharges beyond the site's own load. The discharge floor of the
    # grid objective would forbid it, and would cost more.
    netting <- v2g_price_case()
    results <- run_v2g_price_case(netting, "cost")
    net <- netting$opt_data$static + results$demand$Fleet
    expect_lt(min(net[netting$evening]), -1)
    expect_true(all(net >= -netting$opt_data$export_capacity - 0.05))
    grid_results <- run_v2g_price_case(netting, "grid")
    expect_true(all(
        (netting$opt_data$static + grid_results$demand$Fleet) >= -0.05
    ))

    # A flat export price of 0.07, below the cheapest import price: exporting
    # never pays for the energy charged back, so the fleet discharges only to
    # cut import and the site never exports.
    flat <- v2g_price_case(price_exported = 0.07)
    results <- run_v2g_price_case(flat, "cost")
    net <- flat$opt_data$static + results$demand$Fleet
    expect_true(all(net >= -0.05))
    expect_lt(sum(results$demand$Fleet[flat$evening]), -10)
    expect_v2g_sessions_energy(flat, results)

    # An export price above the import price in every slot: the binary grid
    # mode keeps the site from importing and exporting at once, and the
    # export capacity still binds.
    high <- v2g_price_case(price_exported = 1.5 * v2g_price_case()$opt_data$price_imported)
    results <- run_v2g_price_case(high, "cost")
    net <- high$opt_data$static + results$demand$Fleet
    expect_true(all(net >= -high$opt_data$export_capacity - 0.05))
    expect_true(all(net <= high$opt_data$import_capacity + 0.05))
    expect_v2g_sessions_energy(high, results)
})

test_that("combined v2g blends grid and cost, with the pure objectives at its ends", {
    # As in smart_charging(): a weight of 1 minimises the net power ("grid"),
    # a weight of 0 the energy cost ("cost").
    case <- v2g_price_case()
    grid_results <- run_v2g_price_case(case, "grid")
    cost_results <- run_v2g_price_case(case, "cost")
    expect_equal(run_v2g_price_case(case, 1), grid_results)
    expect_equal(run_v2g_price_case(case, 0), cost_results)

    combined_results <- NULL
    expect_no_message(
        combined_results <- smart_v2g(
            case$sessions,
            case$opt_data,
            opt_objective = 0.5,
            window_days = 1,
            window_start_hour = 12,
            soc_min = 20
        )
    )
    expect_v2g_sessions_energy(case, combined_results)
    expect_v2g_breaches_not_worse(case, combined_results$demand$Fleet)

    # The blend lies between its ends: it costs no less than "cost" and no
    # more than "grid", and its net power is no flatter than "grid"'s.
    setpoint_cost <- function(results) {
        v2g_energy_cost(case, results$setpoints$Fleet)
    }
    squared_net <- function(results) {
        sum((case$opt_data$static + results$setpoints$Fleet)^2)
    }
    expect_lte(setpoint_cost(cost_results), setpoint_cost(combined_results) + 1e-3)
    expect_lte(setpoint_cost(combined_results), setpoint_cost(grid_results) + 1e-3)
    expect_lt(setpoint_cost(combined_results), setpoint_cost(grid_results))
    expect_gte(squared_net(combined_results), squared_net(grid_results) - 1e-3)

    # Its window at w = 1 is the grid window.
    LF <- case$uncontrolled
    LS <- case$opt_data$static
    G <- case$opt_data$production
    PI <- case$opt_data$price_imported
    floor <- v2g_discharge_floor(G, LS, 30)
    expect_equal(
        demand_combined_v2g_window(
            G, LF, LS, PI, PI, 0, 0, "forward", NULL, 33, 30, 20,
            w = 1, LFmin = floor
        ),
        demand_grid_v2g_window(
            G, LF, LS, "forward", NULL, 33, 30, 20,
            LFmin = floor
        ),
        tolerance = 0.02
    )
})

test_that("cost v2g with a ramping penalty and an energy range keeps its constraints", {
    case <- v2g_price_case()
    for (args in list(list(lambda = 0.1), list(energy_min = 0.8))) {
        results <- do.call(run_v2g_price_case, c(list(case, "cost"), args))
        fleet <- results$demand$Fleet
        net <- case$opt_data$static + fleet
        expect_true(all(net <= case$opt_data$import_capacity + 0.05))
        expect_true(all(net >= -case$opt_data$export_capacity - 0.05))
        expect_lt(
            v2g_energy_cost(case, fleet),
            v2g_energy_cost(case, run_v2g_price_case(case, "grid")$demand$Fleet)
        )
        delivered <- results$sessions %>%
            group_by(Session) %>%
            summarise(Energy = sum(Energy))
        expect_true(all(
            delivered$Energy[match(case$sessions$Session, delivered$Session)] >=
                (args$energy_min %||% 1) * case$sessions$Energy - 0.05
        ))
    }
})

test_that("cost v2g discharges into export only when the spread covers the discharge losses", {
    # A flat export price of 0.15 against a night import price of 0.10: the
    # round trip pays with no losses or with discharge_eff = 0.8 (0.12 > 0.10),
    # but not with discharge_eff = 0.6 (0.09 < 0.10), where the setpoint no
    # longer plans any export.
    case <- v2g_price_case(price_exported = 0.15)
    setpoint_export <- function(results) {
        sum(pmax(-(case$opt_data$static + results$setpoints$Fleet), 0))
    }
    battery_gain <- function(results, eff) {
        results$sessions %>%
            group_by(Session) %>%
            summarise(
                Gain = sum(pmax(Energy, 0)) + sum(pmin(Energy, 0)) / eff
            ) %>%
            arrange(match(Session, case$sessions$Session)) %>%
            pull(Gain)
    }
    exports <- c()
    for (eff in c(1, 0.8, 0.6)) {
        for (objective in list("cost", 0.5)) {
            results <- run_v2g_price_case(case, objective, discharge_eff = eff)
            if (identical(objective, "cost")) {
                exports[as.character(eff)] <- setpoint_export(results)
            }
            # Every session charges its energy back, losses included.
            expect_equal(
                battery_gain(results, eff),
                case$sessions$Energy,
                tolerance = 0.05,
                info = paste(objective, eff)
            )
            expect_v2g_breaches_not_worse(case, results$demand$Fleet)
        }
    }
    expect_gt(exports[["1"]], 10)
    expect_gt(exports[["0.8"]], 10)
    expect_lt(exports[["0.8"]], exports[["1"]])
    expect_equal(exports[["0.6"]], 0, tolerance = 0.05)
})

test_that("the v2g scheduler keeps the deadline and the SoC floor with discharge losses", {
    # One 11 kW session that needs 22 kWh in 4 hours, and a setpoint that
    # discharges as much as it can for 1.5 hours. Discharging d kWh to the grid
    # costs d / discharge_eff kWh of battery and of energy to charge, so the
    # scheduler must size the discharge it allows by discharge_eff, or the
    # session leaves short (19.25 kWh at discharge_eff = 0.5).
    dttm <- seq(
        as.POSIXct("2025-03-03 00:00", tz = "UTC"),
        by = "15 min",
        length.out = 16
    )
    session <- tibble(
        Session = "S1",
        Timecycle = "Workday",
        Profile = "Fleet",
        ConnectionStartDateTime = dttm[1],
        ConnectionHours = 4,
        Power = 11,
        Energy = 22,
        StateOfCharge = 0.5
    ) %>%
        evsim::adapt_charging_features(time_resolution = 15) %>%
        mutate(Responsive = TRUE)
    setpoint <- tibble(
        datetime = dttm,
        setpoint = c(rep(-11, 6), rep(11, 10))
    )
    battery_capacity <- 22 / (1 - 0.5)

    for (eff in c(1, 0.8, 0.5)) {
        for (soc_min in c(0, 0.45)) {
            results <- schedule_sessions_v2g(
                session,
                setpoint,
                discharge_eff = eff,
                soc_min = soc_min
            )
            energy <- results$sessions$Energy
            charged <- pmax(energy, 0)
            discharged <- pmin(energy, 0)
            # The battery gains its energy, net of the discharge losses.
            expect_equal(
                sum(charged) + sum(discharged) / eff,
                22,
                tolerance = 0.02,
                info = paste(eff, soc_min)
            )
            # It discharged, and never below its SoC floor.
            expect_lt(sum(discharged), 0)
            battery <- 0.5 * battery_capacity +
                cumsum(charged + discharged / eff)
            expect_true(
                all(battery >= soc_min * battery_capacity - 0.02),
                info = paste(eff, soc_min)
            )
        }
    }
})

test_that("cost and combined v2g approach a negative import capacity", {
    opt_data_prices <- opt_data %>%
        mutate(price_imported = 1, price_exported = 0)
    for (objective in list("cost", 0.5)) {
        v2g_results <- suppressMessages(smart_v2g(
            sessions = sessions,
            opt_data_prices,
            opt_objective = objective,
            window_days = 1,
            window_start_hour = 0
        ))
        expect_true(all(
            v2g_results$setpoints$Worktime <=
                pmax(opt_data$import_capacity, -connected_power) + 1e-2
        ))
        expect_true(all(
            v2g_results$setpoints$Worktime >= -connected_power - 1e-2
        ))
        neg_idx <- which(opt_data$import_capacity < 0)
        expect_true(any(v2g_results$demand$Worktime[neg_idx] < 0))
    }
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

test_that("v2g never discharges while the site exports, whatever the objective", {
    # PV above the 50 kW export capacity around noon, an import capacity that
    # never binds. Discharging at a slot that already exports only adds export,
    # yet the LP placing the energy (minimum squared net power, also behind the
    # capacity and combined objectives) used to discharge in the shallow
    # export of the morning to charge in the deeper export at noon. The cost
    # objective has no discharge floor, but with an export price of 0 a
    # discharge into export never pays.
    dttm <- seq(
        as.POSIXct("2025-03-03 06:00", tz = "UTC"),
        as.POSIXct("2025-03-04 05:45", tz = "UTC"),
        by = "15 min"
    )
    h <- as.numeric(difftime(
        dttm,
        as.POSIXct("2025-03-03 00:00", tz = "UTC"),
        units = "hours"
    ))
    pv <- pmax(0, 160 * sin(pi * (h - 7) / 11))
    pv[h > 18] <- 0
    opt_data_pv <- tibble(
        datetime = dttm,
        static = 30,
        production = round(pv, 2),
        import_capacity = 200,
        export_capacity = 50,
        price_imported = 1,
        price_exported = 0
    )
    session <- function(id, start, hours, energy) {
        tibble(
            Session = paste0("S", id),
            Timecycle = "Workday",
            Profile = "Fleet",
            ConnectionStartDateTime = as.POSIXct(
                "2025-03-03 00:00",
                tz = "UTC"
            ) +
                start * 3600,
            ConnectionHours = hours,
            Power = 11,
            Energy = energy
        )
    }
    sessions_pv <- bind_rows(
        session(1, 8, 8, 30),
        session(2, 9, 7, 20),
        session(3, 12, 5, 15),
        session(4, 15, 12, 40)
    )

    for (objective in list("capacity", "grid", "cost", 0.5)) {
        results <- suppressMessages(suppressWarnings(smart_v2g(
            sessions_pv,
            opt_data_pv,
            opt_objective = objective,
            window_days = 1,
            window_start_hour = 6,
            soc_min = 20
        )))

        uncontrolled <- evsim::get_demand(
            evsim::adapt_charging_features(sessions_pv, time_resolution = 15),
            dttm_seq = dttm
        )$Fleet
        fleet <- results$demand$Fleet
        net_without_fleet <- opt_data_pv$static - opt_data_pv$production
        exporting <- net_without_fleet < 0

        # Neither the setpoint nor the fleet discharges while exporting.
        expect_true(
            all(results$setpoints$Fleet[exporting] >= -0.05),
            info = format(objective)
        )
        expect_true(all(fleet[exporting] >= -0.05), info = format(objective))
        # The export breaches are never worse than with uncontrolled charging.
        export_cap <- opt_data_pv$export_capacity
        expect_lte(
            sum(net_without_fleet + fleet < -export_cap - 0.05),
            sum(net_without_fleet + uncontrolled < -export_cap - 0.05)
        )
        # Every session still gets its energy.
        delivered <- results$sessions %>%
            group_by(Session) %>%
            summarise(Energy = sum(Energy))
        expect_equal(
            delivered$Energy[match(sessions_pv$Session, delivered$Session)],
            sessions_pv$Energy,
            tolerance = 0.05
        )
    }
})


# Capacity objective: change only what a breach needs ---------------------

test_that("v2g capacity leaves the evening alone and absorbs the midday export", {
    opt_data_cap <- capacity_scenario_opt_data()
    sessions_cap <- capacity_scenario_sessions()
    uncontrolled <- capacity_scenario_uncontrolled(sessions_cap)

    results <- suppressMessages(smart_v2g(
        sessions_cap, opt_data_cap,
        opt_objective = "capacity",
        window_days = 1, window_start_hour = 6
    ))
    fleet <- results$demand$Fleet

    # The evening import peaks within the import capacity: no discharge to
    # shave it, as the grid objective does on the same input.
    evening <- opt_data_cap$datetime >= as.POSIXct("2025-03-12 17:00", tz = "UTC") &
        opt_data_cap$datetime < as.POSIXct("2025-03-13 06:00", tz = "UTC")
    expect_equal(fleet[evening], uncontrolled[evening])
    expect_equal(results$setpoints$Fleet[evening], uncontrolled[evening])
    grid <- suppressMessages(smart_v2g(
        sessions_cap, opt_data_cap,
        opt_objective = "grid",
        window_days = 1, window_start_hour = 6
    ))
    expect_false(isTRUE(all.equal(grid$demand$Fleet[evening], uncontrolled[evening])))

    # Discharge in the morning, while the site imports, and charge it back in
    # the export breach: more export absorbed than the one-way capacity
    # objective of smart_charging() manages on the same input.
    net_without_fleet <- opt_data_cap$static - opt_data_cap$production
    morning <- opt_data_cap$datetime < as.POSIXct("2025-03-12 12:00", tz = "UTC")
    expect_true(any(fleet[morning & net_without_fleet > 0] < -1))
    breach <- -net_without_fleet > opt_data_cap$export_capacity
    expect_gt(sum(fleet[breach]), sum(uncontrolled[breach]))

    one_way <- suppressMessages(smart_charging(
        sessions_cap, opt_data_cap,
        opt_objective = "capacity", method = "curtail",
        window_days = 1, window_start_hour = 6
    ))
    excess_before <- capacity_scenario_excess(opt_data_cap, uncontrolled)
    excess_one_way <- capacity_scenario_excess(opt_data_cap, one_way$demand$Fleet)
    excess_v2g <- capacity_scenario_excess(opt_data_cap, fleet)
    expect_lt(excess_v2g[["export"]], excess_one_way[["export"]] - 20)
    expect_lt(excess_one_way[["export"]], excess_before[["export"]])

    # No import breach, no discharge into export, and every session gets its
    # energy.
    expect_equal(excess_v2g[["import"]], 0)
    expect_true(all(fleet[net_without_fleet < 0] >= -0.05))
    delivered <- results$sessions %>%
        group_by(Session) %>%
        summarise(Energy = sum(Energy))
    expect_equal(
        delivered$Energy[match(sessions_cap$Session, delivered$Session)],
        sessions_cap$Energy,
        tolerance = 0.01
    )
})

test_that("v2g capacity returns a window within its caps unchanged", {
    opt_data_cap <- capacity_scenario_opt_data(
        import_capacity = 200, export_capacity = 200
    )
    sessions_cap <- capacity_scenario_sessions()
    uncontrolled <- capacity_scenario_uncontrolled(sessions_cap)

    expect_no_message(results <- smart_v2g(
        sessions_cap, opt_data_cap,
        opt_objective = "capacity",
        window_days = 1, window_start_hour = 6
    ))
    expect_equal(results$setpoints$Fleet, uncontrolled)
    expect_equal(results$demand$Fleet, uncontrolled)
})

test_that("capacity_v2g_window only discharges where it cuts import", {
    # Import of 5 kW in slots 1-2, an export breach of 6 kW in slots 3-4; the
    # flexible load charges 4 kW in slot 1 only. Discharging 5 kW in slots 1-2
    # and charging back in slots 3-4 absorbs the breach; slot 5 exports
    # within its cap and is left alone.
    O <- flextools:::capacity_v2g_window(
        G = c(0, 0, 20, 20, 12), LF = c(4, 0, 0, 0, 0), LS = c(5, 5, 4, 4, 4),
        direction = "forward", time_horizon = NULL, LFmax = 10,
        import_capacity = 100, export_capacity = 10
    )
    expect_equal(sum(O), 4, tolerance = 1e-6)
    expect_true(all(O[1:2] >= -5 - 1e-6))
    expect_equal(O[3:4], c(6, 6), tolerance = 1e-6)
    expect_equal(O[5], 0)

    # Without a breach, the input comes back as it is.
    expect_identical(
        flextools:::capacity_v2g_window(
            G = c(0, 0, 20, 20, 12), LF = c(4, 0, 0, 0, 0), LS = c(5, 5, 4, 4, 4),
            direction = "forward", time_horizon = NULL, LFmax = 10,
            import_capacity = 100, export_capacity = 20
        ),
        c(4, 0, 0, 0, 0)
    )
})

test_that("v2g capacity reduces an export breach it cannot remove, moving only the slice", {
    opt_data_cap <- capacity_scenario_opt_data()
    sessions_cap <- capacity_scenario_sessions()
    uncontrolled <- capacity_scenario_uncontrolled(sessions_cap)

    expect_message(
        results <- smart_v2g(
            sessions_cap, opt_data_cap,
            opt_objective = "capacity",
            window_days = 1, window_start_hour = 6
        ),
        "never beyond the original profile"
    )
    fleet <- results$setpoints$Fleet

    # The morning charge and the discharge down to the SoC floor (12 kWh per
    # vehicle) move into the breach.
    excess_before <- capacity_scenario_excess(opt_data_cap, uncontrolled)
    excess_after <- capacity_scenario_excess(opt_data_cap, fleet)
    expect_equal(
        excess_after[["export"]], excess_before[["export"]] - 60 - 36,
        tolerance = 0.05
    )

    # Only the slice moves: the fleet charges less or discharges in the
    # morning while the site imports, and charges more in the breach.
    net_without_fleet <- opt_data_cap$static - opt_data_cap$production
    breach <- -net_without_fleet > opt_data_cap$export_capacity
    morning_import <- net_without_fleet > 0 &
        opt_data_cap$datetime < as.POSIXct("2025-03-12 12:00", tz = "UTC")
    changed <- abs(fleet - uncontrolled) > 0.005
    expect_true(all(changed <= (morning_import | breach)))
    expect_true(all(fleet[changed & !breach] < uncontrolled[changed & !breach]))
    expect_true(all(fleet[changed & breach] > uncontrolled[changed & breach]))
    expect_true(all(fleet >= pmin(-net_without_fleet, 0) - 0.005 | !morning_import))
})
