library(dplyr)

# Sessions over the daylight-saving nights of Europe/Amsterdam. Every slot is
# `resolution` minutes of elapsed time: on the spring-forward night the 01:45
# slot ends at 03:00 CEST (02:00 does not exist), and on the fall-back night
# the 02:45 CEST slot ends at 02:00 CET (not at 03:00 CET, an hour later).

dst_tz <- "Europe/Amsterdam"

# Two days of 15-minute slots from noon, with a busy site in the evening and
# in the first 02:00 hour of the fall-back night, so the schedule cuts the
# session into several segments, one of them ending at 02:00 CET
dst_opt_data <- function(day) {
  dttm <- seq(
    as.POSIXct(paste(day, "12:00"), tz = dst_tz),
    by = "15 min",
    length.out = 4 * 48
  )
  tibble(
    datetime = dttm,
    production = 0,
    static = ifelse(
      lubridate::hour(dttm) %in% 17:22 |
        (lubridate::hour(dttm) == 2 & lubridate::dst(dttm) & lubridate::month(dttm) == 10),
      40, 5
    )
  )
}

# Connected from 19:30 the day before the change until 11:30 the day of it
dst_sessions <- function(day, ...) {
  start <- as.POSIXct(paste(day, "19:30"), tz = dst_tz)
  # 16 hours of clock time, 15 or 17 of elapsed time
  end <- as.POSIXct(paste(as.Date(day) + 1, "11:30"), tz = dst_tz)
  hours <- as.numeric(end - start, units = "hours")
  tibble(
    Session = "S1",
    Timecycle = "Day",
    Profile = "Fleet",
    ConnectionStartDateTime = start,
    ConnectionEndDateTime = end,
    ConnectionHours = hours,
    Power = 3.7,
    Energy = 20,
    ChargingStartDateTime = start,
    ChargingHours = 20 / 3.7,
    ChargingEndDateTime = start + 20 / 3.7 * 3600,
    FlexibilityHours = hours - 20 / 3.7,
    ...
  )
}

# Every segment starts and ends, ends after it starts, and starts where the
# previous one of its session ended
expect_contiguous_segments <- function(sessions) {
  for (col in c(
    "ConnectionStartDateTime", "ConnectionEndDateTime",
    "ChargingStartDateTime", "ChargingEndDateTime"
  )) {
    expect_false(anyNA(sessions[[col]]), label = col)
  }
  expect_true(all(sessions$ConnectionEndDateTime > sessions$ConnectionStartDateTime))
  expect_true(all(sessions$ChargingEndDateTime > sessions$ChargingStartDateTime))
  sessions %>%
    arrange(.data$Session, .data$ConnectionStartDateTime) %>%
    group_by(.data$Session) %>%
    group_walk(function(segments, key) {
      n <- nrow(segments)
      expect_gt(n, 1)
      expect_equal(
        as.numeric(segments$ConnectionStartDateTime[-1]),
        as.numeric(segments$ConnectionEndDateTime[-n])
      )
    })
  # Together the segments span the whole connection, in elapsed time
  expect_equal(
    sum(sessions$ConnectionHours),
    as.numeric(
      max(sessions$ConnectionEndDateTime) - min(sessions$ConnectionStartDateTime),
      units = "hours"
    )
  )
}

dst_days <- c(spring_forward = "2050-03-26", fall_back = "2050-10-29")

for (change in names(dst_days)) {
  day <- dst_days[[change]]

  test_that(paste("smart_v2g segments are contiguous over the", change, "night"), {
    results <- suppressMessages(smart_v2g(
      dst_sessions(day, StateOfCharge = 50, BatteryCapacity = 60),
      dst_opt_data(day),
      opt_objective = "grid",
      window_days = 1,
      window_start_hour = 12
    ))
    expect_contiguous_segments(results$sessions)
  })

  test_that(paste("smart_charging segments are contiguous over the", change, "night"), {
    results <- suppressMessages(smart_charging(
      dst_sessions(day),
      dst_opt_data(day),
      opt_objective = "grid",
      method = "curtail",
      window_days = 1,
      window_start_hour = 12
    ))
    expect_contiguous_segments(results$sessions)
  })
}

test_that("smart_charging schedules a window that ends in the spring-forward gap", {
  # The last slot starts at 01:45, so the run ends at 03:00 CEST
  opt_data <- dst_opt_data("2050-03-26") %>%
    filter(.data$datetime <= as.POSIXct("2050-03-27 01:45", tz = dst_tz))
  sessions <- dst_sessions("2050-03-26") %>%
    mutate(
      ConnectionEndDateTime = as.POSIXct("2050-03-27 03:00", tz = dst_tz),
      ConnectionHours = 6.5,
      FlexibilityHours = 6.5 - ChargingHours
    )
  results <- suppressMessages(smart_charging(
    sessions,
    opt_data,
    opt_objective = "grid",
    method = "curtail",
    window_days = 1,
    window_start_hour = 12
  ))
  # The window ends inside the gap, yet the session in it is responsive
  expect_true(all(results$sessions$Responsive %in% TRUE))
  expect_contiguous_segments(results$sessions)
  expect_equal(sum(results$sessions$Energy), 20, tolerance = 0.01)
})

test_that("a tank window starting in the spring-forward gap starts at its end", {
  # Twelve hours from midnight, too short for a full window: the leading
  # window ends before 02:00, which on this day is 03:00 CEST (slot 9)
  dttm <- seq(
    as.POSIXct("2050-03-27 00:00", tz = dst_tz),
    by = "15 min",
    length.out = 4 * 12
  )
  windows <- flextools:::tank_flex_windows(
    dttm,
    window_days = 1,
    window_start_hour = 2,
    flex_window_hours = 24
  )
  expect_equal(windows$start, c(1L, 9L))
  expect_equal(windows$flex_idx[[1]], 1:8)
})
