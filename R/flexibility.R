
# Optimization ------------------------------------------------------------

#' Minimization of the grid flow
#'
#' @param w numeric between `0` and `1`, being the weight of the flexibility potential
#' @param G numeric vector, being the renewable generation profile
#' @param LF numeric vector, being the flexible load profile
#' @param LS numeric vector, being the static load profile
#' @param direction character, being `forward` or `backward`. The direction where energy can be shifted
#' @param time_horizon integer, maximum number of positions to shift energy from
#' @param up_to_G logical, whether to limit the flexible EV demand up to renewable Generation
#' @param grid_cap numeric, grid capacity limit. Configuring this value make sense mainly when `w = 0` since `w > 0` will apply peak shaving
#' @param window_length integer, window length. If `NULL`, the window length will be the length of `G`
#'
#' @return numeric vector
#' @export
#'
#' @importFrom reticulate r_to_py
#'
minimize_grid_flow <- function(w, G, LF, LS = NULL, direction = 'forward', time_horizon = NULL, up_to_G = TRUE, grid_cap = NULL, window_length = NULL) {
  if (!pyenv.exists()) load.pyenv()
  pyenv$minimize_grid_flow_time_series(
    w,
    G,
    LF,
    LS = reticulate::r_to_py(LS),
    direction = direction,
    time_horizon = reticulate::r_to_py(time_horizon),
    up_to_G = reticulate::r_to_py(up_to_G),
    grid_cap = reticulate::r_to_py(grid_cap),
    window_length = reticulate::r_to_py(window_length)
  )
}




# EV flexibility management -----------------------------------------------

#' Smart charging algorithm
#'
#' @param sessions sessions data set
#' @param fitting_data optimization fitting data, first column being `datetime`. The other columns could be `solar` (solar generation) and `fixed` (static demand from other sectors like buildings, offices, ...).
#' @param dttm_seq datetime sequence to apply smart charging
#' @param window_length optimization window length
#' @param opt_weights Named list with the optimization weight `w` of the `minimize_grid_flow` function. The names of the list must exactly match the user profiles names.
#' @param responsive Named list with the ratio of sessions responsive to smart charging program for each profile. The names of the list must exactly match the user profiles names.
#' @param power_th power threshold from to consider flexibility required
#' @param up_to_G logical, whether to limit the flexible EV demand up to renewable Generation
#' @param grid_cap numeric, grid capacity limit. Configuring this value make sense mainly when `w = 0` since `w > 0` will apply peak shaving
#' @param sort_by_flex logical, whether to sort the sessions to shift from higher to lower flexibility
#' @param include_msg logical, whether to output the algorithm messages for every user profile and time-slot
#'
#' @importFrom dplyr %>% filter mutate_if mutate select everything row_number left_join bind_rows
#' @importFrom tibble column_to_rownames as_tibble
#' @importFrom rlang .data
#' @importFrom reticulate dict r_to_py import_from_path
#'
#' @return a list with two elements: optimization setpoints and coordinated sessions schedule
#' @export
#'
smart_charging <- function(sessions, fitting_data, dttm_seq, window_length, opt_weights, responsive, power_th = 0, up_to_G = TRUE, grid_cap = NULL, sort_by_flex = TRUE, include_msg = FALSE) {

  if (!pyenv.exists()) load.pyenv()
  # pyenv <- import_utils()

  time_interval <- as.integer(as.numeric(dttm_seq[2] - dttm_seq[1], unit = 'hours')*60)
  start <- dttm_seq[1]
  end <- dttm_seq[length(dttm_seq)]
  window <- c(0, length(dttm_seq)) %>% as.integer
  window_length <- as.integer(window_length)

  sessions_norm <- sessions %>%
    filter(.data$ConnectionStartDateTime >= start) %>%
    normalize_sessions(start, time_interval)

  fitting_data_norm <- fitting_data %>%
    filter(.data$datetime >= start, .data$datetime <= end) %>%
    mutate(timeslot = row_number()) %>%
    select(.data$timeslot, -.data$datetime, everything()) %>%
    tibble::column_to_rownames("timeslot")

  profiles_demand <- pyenv$get_demand(sessions_norm, window)

  # Smart charging results
  results <- pyenv$smart_charging(sessions_norm, profiles_demand, fitting_data_norm, window_length, dict(opt_weights), dict(responsive), power_th, r_to_py(up_to_G), r_to_py(grid_cap), r_to_py(sort_by_flex), r_to_py(include_msg))
  setpoints <- as_tibble(results[[1]]) %>% mutate(datetime = dttm_seq) %>% select('datetime', everything())
  sessions_opt <- denormalize_sessions(results[[2]], start, time_interval)

  # Add sessions discarded by optimization windows
  sessions_out <- sessions[!(sessions[['Session']] %in% sessions_norm[['Session']]), ]
  sessions_opt_final <- bind_rows(sessions_opt, sessions_out)
  sessions_opt_final_sorted <- left_join(sessions['Session'], sessions_opt_final, by = 'Session') %>%
    select('Profile', everything())

  if (include_msg) {
    list(setpoints = setpoints, sessions =  sessions_opt_final_sorted, msg = results[[3]])
  } else {
    list(setpoints = setpoints, sessions =  sessions_opt_final_sorted)
  }
}



# # DR potential ---------------------------------------------------------------
#
# get_interval_flexible_power <- function(interval, sessions_flex, interval_mins) {
#   sessions_flex %>%
#     group_by(Profile) %>%
#     filter(
#       StartTime <= interval,
#       (StartTime + convert_time_num_to_period(ChargingTime)) >= (interval + minutes(interval_mins)),
#       Flexibility > interval_mins/60
#     ) %>%
#     summarise(Power = sum(ChargingPower)) %>%
#     mutate(datetime = interval) %>%
#     spread(Profile, Power)
# }
#
# get_flexible_power <- function(sessions_flex, seq_dt, interval_mins) {
#   tibble(datetime = seq_dt) %>%
#     left_join(
#       map_dfr(seq_dt, ~get_interval_flexible_power(.x, sessions_flex, interval_mins)),
#       by = "datetime"
#     ) %>%
#     replace(is.na(.), 0)
# }
