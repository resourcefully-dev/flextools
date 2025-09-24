library(lubridate)
library(dplyr)

df <- select(
  flextools::energy_profiles,
  "datetime",
  production = "solar",
  consumption = "building"
)

df_batt <- df %>%
  mutate(
    battery = add_battery_simple(
      df = df,
      Bcap = 100, Bc = 10, Bd = 10
    )
  )


# ------------------------------------------------------------------------------
# Tests
# ------------------------------------------------------------------------------

test_that("simple battery is added", {
  expect_true("battery" %in% names(df_batt))
})

test_that("storage losses are added", {
  batt_loss <- get_storage_losses(df_batt$battery, loss = 0.5)
  expect_true(sum(batt_loss) > 0)
})

test_that("storage level is inside bounds", {
  batt_level <- get_soc_level(df_batt$battery, time_resolution = 15)
  expect_true(min(batt_level) >= 0 & max(batt_level) <= 100)
})

test_that("storage level is inside bounds with SOCmin and SOCmax", {
  df_batt_limited <- df %>%
    mutate(
      battery = add_battery_simple(
        df = df,
        Bcap = 150, Bc = 10, Bd = 10,
        SOCmin = 20, SOCmax = 80, SOCini = 50
      )
    )
  batt_level <- get_soc_level(
    df_batt_limited$battery,
    time_resolution = 15,
    init = 150 * 50/100
  )
  expect_true(
    round(min(batt_level)) >= 150 * 20/100 &
      round(max(batt_level)) <= 150 * 80/100
  )
})

test_that("conversion losses are calculated", {
  batt_loss <- get_conversion_losses(df_batt$battery, loss_charge = 5, loss_discharge = 5)
  expect_true(sum(batt_loss) > 0)
})

test_that("storage level accounts for efficiencies", {
  profile <- c(rep(5, 4), rep(-5, 4))
  lvl_ideal <- get_soc_level(profile, time_resolution = 60)
  lvl_lossy <- get_soc_level(profile, time_resolution = 60, charge_eff = 0.9, discharge_eff = 0.9)
  expect_lt(max(lvl_lossy), max(lvl_ideal))
})
