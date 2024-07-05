
#' Round a numeric value to interval
#'
#' @param dbl numeric value
#' @param interval decimal interval (from 0 to 1)
#'
#' @keywords internal
#'
round_to_interval <- function (dbl, interval) {
  round(dbl/interval) * interval
}

#' Convert numeric time value to a datetime period (hour-based)
#'
#' @param time_num Numeric time value (hour-based)
#'
#' @importFrom lubridate hours minutes
#' @keywords internal
#'
convert_time_num_to_period <- function(time_num) {
  h <- time_num %/% 1
  m <- (time_num - h)*60 %/% 1
  hours(as.integer(h)) + minutes(as.integer(m))
}

