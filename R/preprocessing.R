
# Sessions approximation --------------------------------------------------

#' Round a numeric value to interval
#'
#' @param dbl numeric value
#' @param interval decimal interval (from 0 to 1)
#'
#' @return dbl
#' @export
#'
round_to_interval <- function (dbl, interval) {
  round(dbl/interval) * interval
}

#' Approximate sessions to perfect power steps
#'
#' @param sessions sessions data set
#' @param time_interval interval of time approximation, in minutes
#' @param power_interval interval of power approximation, in kW
#'
#' @return tibble
#' @export
#'
#' @importFrom dplyr mutate %>%
#' @importFrom rlang .data
#' @importFrom lubridate round_date
#'
approximate_sessions <- function(sessions, time_interval = 15, power_interval = 0.1) {
  sessions %>%
    mutate(
      ConnectionStartDateTime = round_date(.data$ConnectionStartDateTime, paste(time_interval, "minutes")),
      ConnectionEndDateTime = round_date(.data$ConnectionEndDateTime, paste(time_interval, "minutes")),
      ChargingStartDateTime = round_date(.data$ChargingStartDateTime, paste(time_interval, "minutes")),
      ChargingEndDateTime = round_date(.data$ChargingEndDateTime, paste(time_interval, "minutes")),
      Power = round_to_interval(.data$Power, power_interval),
      ConnectionHours = as.numeric(.data$ConnectionEndDateTime - .data$ConnectionStartDateTime, units='hours'),
      ChargingHours = as.numeric(.data$ChargingEndDateTime - .data$ChargingStartDateTime, units='hours'),
      Energy = .data$Power*.data$ChargingHours,
      FlexibilityHours = .data$ConnectionHours - .data$ChargingHours
    )
}

# Sessions normalization --------------------------------------------------

#' Convert datetime to time slot index
#'
#' @param dttm datetime, value to convert to time slot index
#' @param start datetime, start datetime value
#' @param time_interval integer, interval of time between time slots (minutes)
#'
#' @importFrom lubridate day hour minute with_tz
#'
convert_datetime_to_timeslot <- function(dttm, start, time_interval) {
  start_utc <- with_tz(start, 'UTC')
  dttm_utc <- with_tz(dttm, 'UTC')
  as.integer(as.numeric(dttm_utc - start_utc, unit = 'hours')*60/time_interval) + 1
}


#' Convert time slot index to datetime
#'
#' @param timeslot integer, time slot
#' @param start datetime, start datetime value
#' @param time_interval integer, interval of time between time slots (minutes)
#'
#' @importFrom lubridate minutes with_tz tz
#'
convert_timeslot_to_datetime <- function(timeslot, start, time_interval) {
  start_utc <- with_tz(start, 'UTC')
  dttm_utc <- start_utc + minutes((timeslot - 1) * time_interval)
  with_tz(dttm_utc, tz(start))
}


#' Normalize sessions (from datetime to timeslots)
#'
#' @param sessions sessions data set
#' @param start datetime, start datetime value
#' @param time_interval integer, interval of time between time slots (minutes)
#'
#' @return tibble
#' @export
#'
#' @importFrom dplyr tibble mutate
#' @importFrom rlang .data
#'
normalize_sessions <- function (sessions, start, time_interval) {
  sessions_norm <- tibble(
    Session = as.character(sessions[["Session"]]),
    Profile = as.character(sessions[["Profile"]]),
    cos = convert_datetime_to_timeslot(sessions[["ConnectionStartDateTime"]], start, time_interval),
    chs = convert_datetime_to_timeslot(sessions[["ChargingStartDateTime"]], start, time_interval),
    che = convert_datetime_to_timeslot(sessions[["ChargingEndDateTime"]], start, time_interval),
    coe = convert_datetime_to_timeslot(sessions[["ConnectionEndDateTime"]], start, time_interval),
    p = sessions[["Power"]]
  ) %>%
    mutate(
      e = .data$p*(.data$che - .data$chs),
      f = (.data$coe - .data$chs) - (.data$che - .data$chs)
    )
  return( sessions_norm )
}


#' Denormalize sessions (from timeslot to datetime)
#'
#' @param sessions_norm tibble, normalized sessions data set
#' @param start datetime, start datetime value
#' @param time_interval integer, interval of time between time slots (minutes)
#'
#' @return tibble
#' @export
#'
#' @importFrom rlang .data
#' @importFrom dplyr %>% mutate rename select
#'
denormalize_sessions <- function (sessions_norm, start, time_interval) {
  sessions_norm %>%
    mutate(
      ConnectionStartDateTime = convert_timeslot_to_datetime(.data$cos, start, time_interval),
      ConnectionEndDateTime = convert_timeslot_to_datetime(.data$coe, start, time_interval),
      ChargingStartDateTime = convert_timeslot_to_datetime(.data$chs, start, time_interval),
      ChargingEndDateTime = convert_timeslot_to_datetime(.data$che, start, time_interval),
      ConnectionHours = as.numeric(.data$ConnectionEndDateTime - .data$ConnectionStartDateTime, units = "hours"),
      ChargingHours = as.numeric(.data$ChargingEndDateTime - .data$ChargingStartDateTime, units = "hours"),
      Energy = .data$e/(60/time_interval),
      FlexibilityHours = .data$ConnectionHours - .data$ChargingHours
    ) %>%
    rename(Power = .data$p) %>%
    select(-c("cos", "coe", "chs", "che", "e", "f"))
}


#' Denormalize time-series (from timeslot to demand)
#'
#' @param df tibble or data.frame with first column being `timeslot`
#' @param start datetime, start datetime value
#' @param time_interval integer, interval of time between time slots (minutes)
#'
#' @return tibble with first column being `datetime`
#' @export
#'
denormalize_timeseries <- function(df, start, time_interval) {
  df %>%
    mutate(datetime = convert_timeslot_to_datetime(.data$timeslot, start, time_interval)) %>%
    select(.data$datetime, everything(), -.data$timeslot)
}


# Sessions demand ---------------------------------------------------------

#' Obtain demand from a starting dttm value and certain duration interval
#'
#' @param sessions tibble, sessions data set in standard format marked by `{evprof}` package
#' @param timeslot datetime, time slot of the requested demand
#' @param by character, being 'Profile' or 'Session'. When `by='Profile'` each column corresponds to an EV user profile.
#' @param normalized logical, whether the `sessions` datetime columns are time-slot values
#'
#' @return tibble
#'
#' @importFrom dplyr %>% filter group_by summarise mutate sym as_tibble
#' @importFrom rlang .data
#'
get_sessions_interval_demand <- function(sessions, timeslot, by, normalized) {
  if (normalized) {
    return(
      sessions %>%
        filter(.data$chs <= timeslot, timeslot < .data$che) %>%
        group_by(!!sym(by)) %>%
        summarise(Power = sum(.data$p)) %>%
        mutate(datetime = timeslot) %>%
        as_tibble()
    )
  } else {
    return(
      sessions %>%
        filter(.data$ChargingStartDateTime <= timeslot, timeslot < .data$ChargingEndDateTime) %>%
        group_by(!!sym(by)) %>%
        summarise(Power = sum(.data$Power)) %>%
        mutate(datetime = timeslot) %>%
        as_tibble()
    )
  }
}


#' Obtain timeseries demand from sessions dataset
#'
#' @param sessions tibble, sessions data set in standard format marked by `{evprof}` package
#' @param dttm_seq sequence of datetime values that will be the datetime variable of the returned time-series data frame.
#' This can be an integer sequence as well if `normalized = TRUE`.
#' @param by character, being 'Profile' or 'Session'. When `by='Profile'` each column corresponds to an EV user profile.
#' @param normalized logical, whether the `sessions` datetime columns are time-slot values
#'
#' @return tibble
#' @export
#'
#' @importFrom dplyr left_join tibble sym as_tibble
#' @importFrom rlang .data
#' @importFrom tidyr pivot_wider
#' @importFrom purrr map_dfr
#' @importFrom dtplyr lazy_dt
#' @importFrom data.table as.data.table
#'
get_sessions_demand <- function(sessions, dttm_seq, by = "Profile", normalized = F) {

  if (length(dttm_seq) == 0 | is.null(dttm_seq)) {
    message("Datetime sequence must be valid")
    return( NULL )
  }

  if (is.null(sessions)) {
    message("Sessions parameter must be a tibble")
    return( NULL )
  }

  if (nrow(sessions) == 0) {
    return( tibble(datetime = dttm_seq) )
  }

  sessions_dt <- lazy_dt(sessions)

  demand <- as_tibble(left_join(
    lazy_dt(tibble(datetime = dttm_seq)),
    lazy_dt(map_dfr(dttm_seq, ~ get_sessions_interval_demand(sessions_dt, .x, by, normalized)) %>%
      pivot_wider(names_from = !!sym(by), values_from = .data$Power, values_fill = 0)),
    by = 'datetime'
  ))
  return( replace(demand, is.na(demand), 0) )
}



get_all_sessions_interval_demand_fast <- function(sessions, slot) {
  dplyr::as_tibble(
    dplyr::summarise(
      dplyr::filter(sessions, .data$chs <= slot, slot < .data$che),
      demand = sum(.data$p)
    )
  )
}

get_all_sessions_demand_fast <- function(sessions, timeslot_seq) {
  demand <- dplyr::as_tibble(dplyr::left_join(
    dtplyr::lazy_dt(dplyr::tibble(timeslot = timeslot_seq)),
    dplyr::mutate(dtplyr::lazy_dt(purrr::map_dfr(
      purrr::set_names(timeslot_seq, timeslot_seq),
      ~ get_all_sessions_interval_demand_fast(dtplyr::lazy_dt(sessions), .x),
      .id = 'timeslot'
    )), timeslot = as.numeric(.data$timeslot)),
    by = 'timeslot'
  ))
  return( replace(demand, is.na(demand), 0) )
}

