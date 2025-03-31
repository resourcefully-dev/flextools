library(dplyr)

test_that("time resultion is returned", {
  expect_equal(get_time_resolution(energy_profiles$datetime), 15)
})

test_that("number is converted to period", {
  expect_equal(
    flextools:::convert_time_num_to_period(15),
    lubridate::period(15, "hours")
  )
})

test_that("time-series year is changed", {
  df2 <- change_timeseries_year(energy_profiles, 2024)
  expect_equal(lubridate::year(df2$datetime[1]), 2024)
})

test_that("time-series resolution is decreased with average", {
  df2 <- change_timeseries_resolution(energy_profiles, 60, "average")
  expect_equal(get_time_resolution(df2$datetime), 60)
})
test_that("numeric vector resolution is decreased with average", {
  p2 <- decrease_numeric_resolution(energy_profiles$solar, 4, "average")
  expect_equal(length(p2), nrow(energy_profiles)/4)
})

test_that("time-series resolution is decreased with first", {
  df2 <- change_timeseries_resolution(energy_profiles, 60, "first")
  expect_equal(get_time_resolution(df2$datetime), 60)
})
test_that("numeric vector resolution is decreased with first", {
  p2 <- decrease_numeric_resolution(energy_profiles$solar, 4, "first")
  expect_equal(length(p2), nrow(energy_profiles)/4)
})

test_that("time-series resolution is decreased with sum", {
  df2 <- change_timeseries_resolution(energy_profiles, 60, "sum")
  expect_equal(get_time_resolution(df2$datetime), 60)
})
test_that("numeric vector resolution is decreased with sum", {
  p2 <- decrease_numeric_resolution(energy_profiles$solar, 4, "sum")
  expect_equal(length(p2), nrow(energy_profiles)/4)
})

test_that("Error when method to change is incorrect", {
  expect_error(change_timeseries_resolution(energy_profiles, 60, "divide"))
})

test_that("Error when method to decrease is incorrect", {
  expect_error(
    decrease_timeseries_resolution(energy_profiles, 60, "averages")
  )
})


test_that("time-series resolution is increased with repeat", {
  df2 <- change_timeseries_resolution(energy_profiles, 5, "repeat")
  expect_equal(get_time_resolution(df2$datetime), 5)
})
test_that("time-series resolution is increased with interpolate", {
  df2 <- change_timeseries_resolution(energy_profiles[seq_len(100), ], 5, "interpolate")
  expect_equal(get_time_resolution(df2$datetime), 5)
})
test_that("time-series resolution is increased with divide", {
  df2 <- change_timeseries_resolution(energy_profiles, 5, "divide")
  expect_equal(get_time_resolution(df2$datetime), 5)
})
test_that("Error when method to increase is incorrect", {
  expect_error(change_timeseries_resolution(energy_profiles, 5, "average"))
})

test_that("time-series are aggregated", {
  df <- select(
    energy_profiles, "datetime", building1 = "building", building2 = "building"
  )

  expect_equal(
    names(aggregate_timeseries(df, varname = "total")),
    c("datetime", "total")
  )
})

test_that("time-series are aggregated, omitting some variables", {
  df <- select(
    energy_profiles, "datetime", building1 = "building", building2 = "building"
  )

  expect_equal(
    names(aggregate_timeseries(df, varname = "total", omit = "building1")),
    c("datetime", "total", "building1")
  )
})
