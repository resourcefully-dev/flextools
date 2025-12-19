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

    # check that setpoints are always lower than the import capacity
    expect_true(
        all(
            v2g_results$setpoints$Worktime <= opt_data$import_capacity + 1e-6
        )
    )

    # check that negative import capacity is achieved
    neg_idx <- which(opt_data$import_capacity < 0)
    expect_true(any(v2g_results$demand$Worktime[neg_idx] < 0))
})
