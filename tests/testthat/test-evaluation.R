library(dplyr)


# Energy evaluation -------------------------------------------------------------

df <- select(
  energy_profiles,
  "datetime",
  production = "solar",
  consumption = "building"
)

test_that("energy kpis are calculated", {
  expect_type(get_energy_kpis(df), "list")
})

test_that("net power is plotted", {
  plot <- plot_net_power(df, import_capacity = 10, export_capacity = 10)
  expect_equal(class(plot), c("dygraphs", "htmlwidget"))
})

test_that("net power is plotted with original df", {
  building_variation <- rnorm(nrow(df), mean = 0, sd = 1)
  df2 <- dplyr::mutate(df, consumption = consumption + building_variation)
  plot <- plot_net_power(df2, original_df = df, import_capacity = 10, export_capacity = 10)
  expect_equal(class(plot), c("dygraphs", "htmlwidget"))
})

test_that("load duration curve is plotted", {
  plot <- plot_load_duration_curve(df)
  expect_true("ggplot" %in% class(plot))
})

test_that("load duration curve is plotted with original df", {
  building_variation <- rnorm(nrow(df), mean = 0, sd = 1)
  df2 <- dplyr::mutate(df, consumption = consumption + building_variation)
  plot <- plot_load_duration_curve(df2, original_df = df)
  expect_true("ggplot" %in% class(plot))
})

# Smart charging evaluation -----------------------------------------------

sessions <- evsim::california_ev_sessions_profiles %>%
  slice_head(n = 50) %>%
  evsim::adapt_charging_features(time_resolution = 15)
sessions_demand <- evsim::get_demand(sessions, resolution = 15)

opt_data <- tibble(
  datetime = sessions_demand$datetime,
  production = 0
)
sc_results <- smart_charging(
  sessions, opt_data, opt_objective = "grid", method = "curtail",
  window_days = 1, window_start_hour = 6,
  responsive = list(Workday = list(Worktime = 0.9)),
  energy_min = 0.5
)

test_that("energy charged is summarised", {
  expect_true(
    tibble::is_tibble(summarise_energy_charged(sc_results, sessions))
  )
})



# Energy cost evaluation --------------------------------------------------

df <- dplyr::select(
  energy_profiles,
  "datetime",
  production = "solar",
  consumption = "building",
  "price_imported",
  "price_exported"
)
df <- dplyr::slice_head(df, n = 300)

test_that("energy cost is calculated", {
  expect_type(
    get_energy_total_cost(df),
    "double"
  )
})

test_that("energy cost is plotted", {
  plot <- plot_energy_cost(df)
  expect_equal(class(plot), c("dygraphs", "htmlwidget"))
})

test_that("energy cost is plotted with original df", {
  building_variation <- rnorm(nrow(df), mean = 0, sd = 1)
  df2 <- dplyr::mutate(df, consumption = consumption + building_variation)
  plot <- plot_energy_cost(df2, original_df = df)
  expect_equal(class(plot), c("dygraphs", "htmlwidget"))
})



# Test imbalance income ---------------------------------------------------

df <- dplyr::select(
  energy_profiles,
  "datetime",
  demand_baseline = "building",
  "price_turn_up",
  "price_turn_down"
)

# Build another random consumption profile
building_variation <- rnorm(nrow(df), mean = 0, sd = 1)
df <- dplyr::mutate(
  df, demand_final = demand_baseline + building_variation
)

test_that("imbalance income is calculated", {
  expect_type(
    get_imbalance_total_income(df),
    "double"
  )
})
