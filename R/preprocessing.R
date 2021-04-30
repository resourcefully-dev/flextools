
# Dygraphs support -------------------------------------------------------------------------

#' Convert a data.frame or tibble to timeseries data.frame
#'
#' @param df data.frame or tibble
#'
#' @return timeseries data.frame
#' @export
#'
#' @importFrom xts xts
#'
df_to_ts <- function(df) {
  xts::xts(df[-1], order.by = df[[1]])
}


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
#' @importFrom xts align.time
#'
approximate_sessions <- function(sessions, time_interval = 15, power_interval = 1) {
  sessions %>%
    mutate(
      ConnectionStartDateTime = xts::align.time(.data$ConnectionStartDateTime, n=60*time_interval),
      ConnectionEndDateTime = xts::align.time(.data$ConnectionEndDateTime, n=60*time_interval),
      ChargingStartDateTime = xts::align.time(.data$ChargingStartDateTime, n=60*time_interval),
      ChargingEndDateTime = xts::align.time(.data$ChargingEndDateTime, n=60*time_interval),
      Power = round_to_interval(.data$Power, power_interval),
      ConnectionHours = as.numeric(.data$ConnectionEndDateTime - .data$ConnectionStartDateTime, units='hours'),
      ChargingHours = as.numeric(.data$ChargingEndDateTime - .data$ChargingStartDateTime, units='hours'),
      Energy = .data$Power*.data$ChargingHours,
      FlexibilityHours = .data$ConnectionHours - .data$ChargingHours
    )
}

# Sessions normalization --------------------------------------------------

#' Get initial index
#'
#' @param dt datetime
#' @param time_interval_mins interval of time (minutes)
#'
#' @importFrom lubridate day hour minute
#'
get_dhm_idx <- function(dt, time_interval_mins) {
  day(dt)*24*60/time_interval_mins + hour(dt)*60/time_interval_mins + round(minute(dt)/time_interval_mins)
}


#' Get energy from charging hours and charging power
#'
#' @param power_kW charging power (kW)
#' @param charging_start connection start hour
#' @param charging_end charging end hour
#' @param time_interval_mins interval of time (minutes)
#'
get_energy <- function(power_kW, charging_start, charging_end, time_interval_mins) {
  power_kW*(charging_end-charging_start)*time_interval_mins
}


#' Normalize sessions (from datetime to timeslots)
#'
#' @param sessions sessions data set
#' @param start first value of the normalization datetime sequence
#' @param time_interval interval of time (minutes)
#'
# #' @export
#'
#' @importFrom dplyr tibble mutate_if
#'
normalize_sessions <- function(sessions, start, time_interval) {
  # Normalization: The time slot index is an integer (e.g. from 0 to 96 with 15 minutes interval) instead of a datetime vector.
  # Thus, we have to make the translation considering that the `start` datetime value is now index 0.
  i0 <- get_dhm_idx(start, time_interval)
  sessions_data <- tibble(
    "Session" = as.character(sessions[["Session"]]),
    "prof" = as.character(sessions[["Profile"]]),
    "cos" = get_dhm_idx(sessions[['ConnectionStartDateTime']], time_interval)-i0,
    "chs" = get_dhm_idx(sessions[['ChargingStartDateTime']], time_interval)-i0,
    "che" = get_dhm_idx(sessions[['ChargingEndDateTime']], time_interval)-i0,
    "coe" = get_dhm_idx(sessions[['ConnectionEndDateTime']], time_interval)-i0,
    "p" = sessions[["Power"]]
  )
  # Only session after the starting datetime value
  # sessions_data <- sessions_data[sessions_data$cos >= 0, ]
  # Check that all sessions are feasible: start before charging end and end charging before session end
  sessions_data <- sessions_data.loc[
    (sessions_data['cos'] >= 0) & (sessions_data['chs'] >= sessions_data['cos']) &
      (sessions_data['che'] >= sessions_data['chs']) & (sessions_data['coe'] >= sessions_data['che'])
  ]
  # sessions_data <- sessions_data[sessions_data[['che']] >= sessions_data[['chs']], ]
  # sessions_data <- sessions_data[sessions_data[['coe']] >= sessions_data[['che']], ]
  # Build energy vector
  sessions_data[['e']] <- get_energy(sessions_data[['p']], sessions_data[['chs']], sessions_data[['che']], time_interval)

  # # Power levels
  # sessions_data[['pl']] <- 2

  # Check if energy charged is feasible (sum == 0)
  check <- round(sum((sessions_data[['che']] - sessions_data[['chs']])*time_interval*sessions_data[['p']] - sessions_data[['e']]))
  if (check == 0) {
    return(sessions_data)
  }
  else {
    message(paste("Error: sum of energy values doesn't match sum of power*time product. The difference is", check))
    return(NULL)
  }
}


#' Denormalize sessions (from timeslot to datetime)
#'
#' @param sessions_norm normalized sessions data set
#' @param start first value of the normalization datetime sequence
#' @param time_interval interval of time (minutes)
#'
#' @return tibble
# #' @export
#'
#' @importFrom rlang .data
#' @importFrom tibble as_tibble
#' @importFrom lubridate minutes
#' @importFrom dplyr %>% mutate rename select arrange
#'
denormalize_sessions <- function(sessions_norm, start, time_interval) {
  # Normalization: The index must be from 0 to 96 (if 15 minutes interval) instead of a datetime vector.
  sessions_norm %>%
    as_tibble() %>%
    mutate(
      ConnectionStartDateTime = start + minutes(.data$cos*time_interval),
      ConnectionEndDateTime = start + minutes(.data$coe*time_interval),
      ChargingStartDateTime = start + minutes(.data$chs*time_interval),
      ChargingEndDateTime = start + minutes(.data$che*time_interval),
      ConnectionHours = as.numeric(.data$ConnectionEndDateTime - .data$ConnectionStartDateTime, units='hours'),
      ChargingHours = as.numeric(.data$ChargingEndDateTime - .data$ChargingStartDateTime, units='hours'),
      ShiftHours = .data$shifted*time_interval/60,
      Energy = .data$e/60, # From kW·min to kW·h
      FlexibilityHours = .data$ConnectionHours - .data$ChargingHours
    ) %>%
    rename(
      Profile = .data$prof,
      Power = .data$p
    ) %>%
    select('Profile', 'Session', 'ConnectionStartDateTime', 'ConnectionEndDateTime',
           'ChargingStartDateTime', 'ChargingEndDateTime', 'Power', 'Energy',
           'ConnectionHours', 'ChargingHours', 'FlexibilityHours', 'ShiftHours') %>%
    arrange(.data$ConnectionStartDateTime)
}


# Sessions demand ---------------------------------------------------------


#' Get schedule
#'
#' @param sessions_norm dataframe of normalized sessions
#' @param window vector with time window's indices
#'
#' @importFrom reticulate import_from_path
#'
#' @return data.frame
#'
get_schedule <- function(sessions_norm, window) {

  if (!pyenv.exists()) load.pyenv()
  pyenv$get_schedule(sessions_norm, window)

}


#' Get demand
#'
#' @param sessions sessions data set
#' @param dttm_seq vector with a sequence of datetime values
#' @param aggregated single "Demand" column or one column for each profile
#' @param stacked single "Profile" column or one column for each profile
#' @param normalized True if sessions datatset is normalized (timeslots instead of datetimes)
#'
#' @importFrom dplyr %>% filter mutate_if mutate select everything
#' @importFrom tibble column_to_rownames as_tibble
#' @importFrom rlang .data
#' @importFrom reticulate import_from_path
#'
#' @return data.frame
#' @export
#'
get_demand <- function(sessions, dttm_seq, aggregated = FALSE, stacked = FALSE, normalized = FALSE) {

  if (!pyenv.exists()) load.pyenv()

  time_interval <- as.integer(as.numeric(dttm_seq[2] - dttm_seq[1], unit = 'hours')*60)
  start <- dttm_seq[1]
  end <- dttm_seq[length(dttm_seq)]
  window <- c(0, length(dttm_seq)) %>% as.integer

  if (normalized) {
    sessions_norm <- sessions
  } else {
    sessions_norm <- normalize_sessions(sessions, start, time_interval)
  }

  demand_df <- pyenv$get_demand(sessions_norm, window, aggregated = reticulate::r_to_py(aggregated), stacked = reticulate::r_to_py(stacked))

  demand_df %>%
    as_tibble() %>%
    mutate(datetime = dttm_seq) %>%
    select(.data$datetime, everything())
}








