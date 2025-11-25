
# General -----------------------------------------------------------------

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

#' Normalize between 0 and 1
#'
#' @param x numeric value or vector
#'
#' @keywords internal
#'
normalize <- function(x) {
  (x-min(x))/(max(x)-min(x))
}


# Time-series -------------------------------------------------------------


#' Return the time resolution of a datetime sequence
#'
#' @param dttm_seq datetime sequence
#' @param units character being one of "auto", "secs", "mins", "hours", "days" and "weeks"
#'
#' @return numeric
#' @export
#'
#' @examples
#' resolution <- get_time_resolution(energy_profiles$datetime)
#' print(resolution)
#'
get_time_resolution <- function(dttm_seq, units = 'mins') {
  as.numeric(dttm_seq[2] - dttm_seq[1], units)
}


#' Change the year of a time series data frame keeping the original weekdays
#'
#' The input `df` must contain full-week time-series profiles in order to
#' arrange the data according to the day of the week. For example, if the first
#' day in the `df` is a Monday the last one must be a Sunday.
#'
#' @param df tibble with first column being `datetime`
#' @param year_out integer, year of the desired `datetime`
#'
#' @return tibble
#' @export
#'
#' @importFrom lubridate year wday tz year<-
#' @importFrom dplyr %>% mutate select bind_rows everything
#'
#' @examples
#' # Example time-series data set: year 2023
#' head(energy_profiles)
#'
#' # Change year to 2025
#' # Note that the data from columns has changed according to the weekday
#' head(change_timeseries_year(
#'   df = energy_profiles,
#'   year_out = 2025
#' ))
#'
#'
change_timeseries_year <- function(df, year_out) {
  df_year <- unique(year(df$datetime))
  df_tz <- tz(df$datetime)
  df_resolution <- get_time_resolution(df$datetime, units = "mins")

  # Checks
  if (length(df_year) > 1) {
    stop("Error: more than one year in date time sequence of data")
  }
  if (year_out == df_year) {
    return( df )
  }

  # Which day of the week is the first one in the year_out?
  datetime_seq_out <- df$datetime
  year(datetime_seq_out) <- year_out
  year_out_first_wday <- wday(datetime_seq_out[1], week_start = 1)

  year_in_start_wday_idx <-
    which(wday(df$datetime, week_start = 1) == year_out_first_wday)[1]

  # If it is not possible to re-order the data to match days of the week
  # just return the same data with the new datetime sequence
  if (is.na(year_in_start_wday_idx)) {
    df_out <- df %>%
      mutate(datetime = datetime_seq_out)
    return( df_out )
  } else {
    # Reorder data to match days of the week
    if (year_in_start_wday_idx > 1) {
      df_wday <- bind_rows(
        df[seq(year_in_start_wday_idx, nrow(df)), ],
        df[seq(1, year_in_start_wday_idx-1), ]
      )
    } else {
      df_wday <- df
    }
    df_wday <- df_wday %>%
      mutate(
        datetime = datetime_seq_out
      ) %>%
      select("datetime", everything())

    return( df_wday )
  }
}


#' Change time resolution of a time-series data frame
#'
#' @param df tibble, first column of name `datetime` being of class datetime and rest of columns being numeric
#' @param resolution_out integer, desired interval of minutes between two consecutive datetime values
#' @param method character, being `interpolate`, `repeat` or `divide` if the resolution has to be increased,
#' or `average`, `first` or `sum` if the resolution has to be decreased. See Examples for more information.
#'
#' @return tibble
#' @export
#'
#' @importFrom dplyr tibble select_if %>%
#'
#' @examples
#' # Example time-series data set: time resolution 15 minutes
#' head(energy_profiles)
#'
#' # Change time resolution to 60 minutes
#' # It is decreasing time resolution, so we use the `average` method
#' head(change_timeseries_resolution(
#'   df = energy_profiles,
#'   resolution_out = 60,
#'   method = "average"
#' ))
#'
#'
#'
change_timeseries_resolution <- function(df, resolution_out, method) {
  current_resolution <- get_time_resolution(df$datetime, units = "mins")
  if (current_resolution == resolution_out) {
    return(df)
  } else if (resolution_out > current_resolution) {
    if (method %in% c("average", "first", "sum")) {
      return(decrease_timeseries_resolution(df, resolution_out, method))
    } else {
      stop("Error: method not valid for decreasing resolution")
    }
  } else {
    if (method %in%  c("interpolate", "repeat", "divide")) {
      return(increase_timeseries_resolution(df, resolution_out, method))
    } else {
      stop("Error: method not valid for increasing resolution")
    }
  }
}



#' Interpolate `n` values between two numeric values
#'
#' @param y1 first value
#' @param y2 second value
#' @param n integer, number of intra-values (counting the original value as the first one)
#'
#' @importFrom dplyr tibble
#' @importFrom stats lm predict
#'
#' @keywords internal
#' @return numeric vector
#'
interpolation <- function(y1, y2, n) {
  if (is.na(y1) | is.na(y2)) {
    return( rep(y1, n) )
  }
  as.numeric(
    predict(
      lm(
        y ~ x,
        tibble(x = c(1, (n+1)), y = c(y1, y2))
      ),
      tibble(x=c(1:n))
    )
  )
}

#' Increase numeric vector resolution
#'
#' @param y original numeric vector
#' @param n integer, number of intra-values (counting the original value as the first one)
#' @param method character, being `interpolate`, `repeat` or `divide` as valid options
#'
#' @return numeric vector
#' @keywords internal
#'
#' @importFrom dplyr tibble lead %>%
#' @importFrom purrr pmap simplify
#'
#' @details
#' if we have a vector v = c(1, 2), and we choose the `interpolate` method,
#' then:
#'
#' `increase_numeric_resolution(v, 4, 'interpolate')`
#'
#' returns `c(1, 1.25, 1.5, 1.75, 2)`
#'
#' if we choose the `repeat` method, then:
#'
#' `increase_numeric_resolution(v, 4, 'repeat')`
#'
#' returns c(1, 1, 1, 1, 2)
#'
increase_numeric_resolution <- function(y, n, method = c('interpolate', 'repeat', 'divide')) {
  if (method == 'interpolate') {
    tibble(y1 = y, y2 = lead(y, default = 0)) %>%
      pmap(~ interpolation(..1, ..2, n)) %>%
      simplify() %>%
      as.double()
  } else if (method == 'repeat') {
    rep(y, each = n)
  } else if (method == 'divide') {
    rep(y/n, each = n)
  } else {
    stop("Error: method not valid")
  }
}

#' Increase datetime vector resolution
#'
#' @param y vector of datetime values
#' @param resolution_mins integer, interval of minutes between two consecutive datetime values
#'
#' @return datetime vector
#' @keywords internal
#'
#' @importFrom lubridate minutes as_datetime tz
#'
increase_datetime_resolution <- function(y, resolution_mins) {
  seq.POSIXt(y[1], y[length(y)]+(y[2]-y[1])-minutes(resolution_mins), by = paste(resolution_mins, 'min')) %>% as_datetime(tz = tz(y))
}

#' Increase time resolution of a timeseries data frame
#'
#' @param df data.frame or tibble, first column of name `datetime` being of class datetime and rest of columns being numeric
#' @param resolution_mins integer, interval of minutes between two consecutive datetime values
#' @param method character, being `interpolate`, `repeat` or `divide` as valid options.
#' See `increase_numeric_resolution` function for more information.
#'
#' @return tibble
#' @keywords internal
#'
#' @importFrom dplyr tibble select_if %>%
#'
increase_timeseries_resolution <- function(df, resolution_mins, method = c('interpolate', 'repeat', 'divide')) {
  new_df <- tibble(datetime = increase_datetime_resolution(df$datetime, resolution_mins))
  current_resolution <- get_time_resolution(df$datetime, units = "mins")
  numeric_df <- df %>% select_if(is.numeric)
  for (col in colnames(numeric_df)) {
    new_df[[col]] <- increase_numeric_resolution(numeric_df[[col]], n = current_resolution/resolution_mins, method)
  }
  return( new_df )
}


#' Decrease resolution of a numeric vector
#'
#' @param y original numeric vector
#' @param n integer, number of intra-values (counting the original value as the first one)
#' @param method character, being `average`, `first` or `sum` as valid options
#'
#' @return numeric vector
#' @keywords internal
#'
#' @importFrom dplyr %>% group_by summarise pull tibble
#' @importFrom rlang .data
#'
decrease_numeric_resolution <- function(y, n, method = c('average', 'first', 'sum')) {
  if ((length(y)%%n) > 0) {
    stop("Error decreasing resolution: the original vector should have a length multiple of `n`.")
  }

  if (method == 'average') {
    return(
      tibble(
        idx = rep(seq(1, length(y)/n), each = n),
        y = y
      ) %>%
        group_by(.data$idx) %>%
        summarise(y = mean(y)) %>%
        pull(y) %>%
        as.numeric()
    )
  } else if (method == 'first') {
    return(
      y[seq(1, length(y), n)]
    )
  } else if (method == 'sum') {
    return(
      tibble(
        idx = rep(seq(1, length(y)/n), each = n),
        y = y
      ) %>%
        group_by(.data$idx) %>%
        summarise(y = sum(y)) %>%
        pull(y) %>%
        as.numeric()
    )
  } else {
    stop("Error: method not valid")
  }
}

#' Decrease time resolution of timeseries data frame
#'
#' @param df data.frame or tibble, first column of name `datetime` being of class datetime and rest of columns being numeric
#' @param resolution_mins integer, interval of minutes between two consecutive datetime values
#' @param method character, being `average`, `first` or `sum` as valid options
#'
#' @return tibble
#' @keywords internal
#'
#' @importFrom dplyr %>% mutate group_by summarise_all distinct
#' @importFrom lubridate floor_date
#' @importFrom rlang .data
#'
decrease_timeseries_resolution <- function(df, resolution_mins, method = c('average', 'first', 'sum')) {
  df2 <- df %>%
    mutate(datetime = floor_date(.data$datetime, paste(resolution_mins, 'minute')))
  if (method == 'average') {
    return(
      df2 %>%
        group_by(.data$datetime) %>%
        summarise_all(mean)
    )
  } else if (method == 'first') {
    return(
      df2 %>%
        distinct(.data$datetime, .keep_all = T)
    )
  } else if (method == 'sum') {
    return(
      df2 %>%
        group_by(.data$datetime) %>%
        summarise_all(sum)
    )
  } else {
    stop("Error: method not valid")
  }
}


#' Aggregate multiple timeseries columns to a single one
#'
#' The first column `datetime` will be kept.
#'
#' @param df data.frame or tibble, first column of name `datetime` being of class datetime and rest of columns being numeric
#' @param varname character, name of the aggregation column
#' @param omit character, name of columns to not aggregate
#'
#' @return tibble
#' @export
#'
#' @examples
#' # Example data set with 2 identical building profiles
#' df <- dplyr::select(
#'   energy_profiles, datetime, building1 = building, building2 = building
#' )
#' head(df)
#'
#' # Aggregate the total building demand
#' head(aggregate_timeseries(df, varname = "total_buildings"))
#'
aggregate_timeseries <- function(df, varname, omit = NULL) {
  tbl <- df['datetime']
  omit_col_n <- which(colnames(df) %in% c('datetime', omit))
  tbl[[varname]] <- rowSums(df[-omit_col_n])
  if (!is.null(omit)) {
    for (omit_var in omit) {
      tbl[[omit_var]] <- df[[omit_var]]
    }
  }
  return( tbl )
}


#' Add an extra day at the beginning and the end of datetime sequence
#' using the last and first day of the data
#'
#' @param df data frame, first column named `datetime` of type `datetime`
#'
#' @return tibble
#' @export
#'
#' @importFrom dplyr filter %>% bind_rows arrange
#' @importFrom lubridate date days
#'
add_extra_days <- function(df) {
  first_day <- df %>%
    filter(date(.data$datetime) == min(date(.data$datetime)))
  first_day$datetime <- first_day$datetime - days(1)
  last_day <- df %>%
    filter(date(.data$datetime) == max(date(.data$datetime)))
  last_day$datetime <- last_day$datetime +days(1)

  bind_rows(
    first_day, df, last_day
  ) %>%
    arrange(.data$datetime)
}