
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
#' @importFrom lubridate day hour minute
#'
convert_datetime_to_timeslot <- function(dttm, start, time_interval) {
  as.integer(as.numeric(dttm - start, unit = 'hours')*60/time_interval) + 1
}


#' Convert time slot index to datetime
#'
#' @param timeslot integer, time slot
#' @param start datetime, start datetime value
#' @param time_interval integer, interval of time between time slots (minutes)
#'
#' @importFrom lubridate minutes
#'
convert_timeslot_to_datetime <- function(timeslot, start, time_interval) {
  start + minutes((timeslot - 1) * time_interval)
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
#' @importFrom dplyr tibble mutate_if
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
  )
  sessions_norm[["e"]] <- sessions_norm[["p"]]* (sessions_norm[["che"]] - sessions_norm[["chs"]])
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
    select(-c("cos", "coe", "chs", "che", "e"))
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
#' @importFrom dplyr %>% filter group_by summarise mutate sym
#' @importFrom rlang .data
#'
get_sessions_interval_demand <- function(sessions, timeslot, by, normalized) {
  if (normalized) {
    return(
      sessions %>%
        filter(.data$chs <= timeslot, timeslot < .data$che) %>%
        group_by(!!sym(by)) %>%
        summarise(Power = sum(.data$p)) %>%
        mutate(datetime = timeslot)
    )
  } else {
    return(
      sessions %>%
        filter(.data$ChargingStartDateTime <= timeslot, timeslot < .data$ChargingEndDateTime) %>%
        group_by(!!sym(by)) %>%
        summarise(Power = sum(.data$Power)) %>%
        mutate(datetime = timeslot)
    )
  }
}


#' Obtain timeseries demand from sessions dataset
#'
#' @param sessions tibble, sessions data set in standard format marked by `{evprof}` package
#' @param dttm_seq sequence of datetime values that will be the datetime variable of the returned time-series data frame
#' @param by character, being 'Profile' or 'Session'. When `by='Profile'` each column corresponds to an EV user profile.
#' @param resolution integer, time resolution (in minutes) of the output demand time-series. If `dttm_seq` is defined this parameter is ignored.
#' @param normalized logical, whether the `sessions` datetime columns are time-slot values
#'
#' @return tibble
#' @export
#'
#' @importFrom dplyr left_join tibble sym mutate_if
#' @importFrom lubridate floor_date days is.timepoint
#' @importFrom rlang .data
#' @importFrom tidyr pivot_wider
#' @importFrom purrr map_dfr
#'
get_sessions_demand <- function(sessions, dttm_seq = NULL, by = "Profile", resolution = 15, normalized = F) {

  if (nrow(sessions) == 0) {
    if (is.null(dttm_seq)) {
      message("Must provide sessions or dttm_seq parameter")
      return( NULL )
    } else {
      return( tibble(datetime = dttm_seq, demand = 0) )
    }
  } else {
    if (is.null(dttm_seq)) {
      dttm_seq <- seq.POSIXt(
        from = floor_date(min(sessions$ConnectionStartDateTime), 'day'),
        to = floor_date(max(sessions$ConnectionEndDateTime), 'day') + days(1),
        by = paste(resolution, 'min')
      )
    } else {
      resolution <- as.numeric(dttm_seq[2] - dttm_seq[1], units = 'mins')
    }
  }

  sessions_aligned <- sessions %>%
    mutate_if(is.timepoint, floor_date, paste(resolution, 'min'))

  demand <- left_join(
    tibble(datetime = dttm_seq),
    map_dfr(dttm_seq, ~ get_sessions_interval_demand(sessions, .x, by, normalized)) %>%
      pivot_wider(names_from = !!sym(by), values_from = .data$Power, values_fill = 0),
    by = 'datetime'
  )
  return( replace(demand, is.na(demand), 0) )
}



get_all_sessions_interval_demand_fast <- function(sessions, slot) {
  dplyr::tibble(
    timeslot = slot,
    dplyr::summarise(
      dplyr::filter(sessions, .data$chs <= slot, slot < .data$che),
      demand = sum(.data$p)
    )
  )
}

get_all_sessions_demand_fast <- function(sessions, timeslot_seq) {
  demand <- dplyr::left_join(
    dplyr::tibble(timeslot = timeslot_seq),
    purrr::map_dfr(
      timeslot_seq,
      ~get_all_sessions_interval_demand_fast(sessions, .x)
    ), by = 'timeslot'
  )
  return( replace(demand, is.na(demand), 0) )
}

