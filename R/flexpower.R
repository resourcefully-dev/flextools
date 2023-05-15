

#' Divide a session into multiple sessions according to time resolution
#'
#' @param session tibble, charging sessions data set
#' @param resolution integer, time resolution in minutes
#'
#' @return tibble
#' @export
#'
#' @importFrom dplyr tibble mutate select all_of
#' @importFrom lubridate floor_date minutes
#'
expand_session <- function(session, resolution) {
  tibble(
    Timeslot = seq.POSIXt(
      session$ConnectionStartDateTime,
      floor_date(session$ConnectionEndDateTime, paste(resolution, "minutes")) + minutes(resolution),
      by = paste(resolution, "mins")
    )
  ) %>%
    mutate(
      Session = session$Session,
      ChargingPoint = session$ChargingPoint,
      EnergyLeft = session$Energy,
      Phases = session$Phases,
      Power = 0
    ) %>%
    select(all_of(c("Session", "ChargingPoint", "Phases", "Timeslot", "EnergyLeft", "Power")))
}



#' Simulate charging power limitation based on grid capacity signals
#'
#' @param sessions tibble, charging sessions data set.
#' If sessions have been expanded using `expand_session` function, set parameter `expand = FALSE`.
#' @param grid_capacity tibble, grid capacity time-series with columns `datetime` and `max_amps`
#' @param amps_car_min integer, minimum current to charge the vehicles
#' @param expand logical, whether to expand session using `expand_session` function
#' @param include_log logical, whether to print and return log messages
#'
#' @return tibble
#' @export
#'
#' @importFrom dplyr %>% tibble mutate select all_of row_number filter group_by summarise
#' @importFrom lubridate as_datetime tz
#' @importFrom rlang .data
#'
limit_charging_power <- function(sessions, grid_capacity, amps_car_min = 8, expand = TRUE, include_log = FALSE) {
  log_messages <- c()

  if (expand) {
    resolution <- as.integer(as.numeric(grid_capacity$datetime[2] - grid_capacity$datetime[1], unit = 'hours')*60)
    sessions_expanded <- sessions %>%
      split(1:nrow(sessions)) %>%
      map_dfr(
        ~ expand_session(.x, resolution = resolution)
      ) %>%
      mutate(ID = row_number())
  } else {
    sessions_expand <- sessions
  }


  for (timeslot in grid_capacity$datetime) {

    # Sessions that are charging (EnergyLeft > 0) during this time slot
    sessions_timeslot <- sessions_expanded %>%
      filter(.data$Timeslot == timeslot, .data$EnergyLeft > 0) %>%
      mutate(ChargingPointLimit = 16)

    if (nrow(sessions_timeslot) == 0) {
      next
    }

    # Find the maximum current allowed by the MSR per charging socket
    phases_used_by_chpoint <- sessions_timeslot %>%
      group_by(.data$ChargingPoint) %>%
      summarise(n_phases = sum(.data$Phases)) %>%
      mutate(n_sessions = ifelse(.data$n_phases <= 3, 1, 2))
    n_times_phases_used <- sum(phases_used_by_chpoint$n_sessions)

    amps_msr_max <- grid_capacity$max_amps[
      grid_capacity$datetime == timeslot
    ]/n_times_phases_used

    # Find the maximum current allowed by every charging point per charging socket
    for (chpoint in unique(sessions_timeslot$ChargingPoint)) {
      sessions_chpoint <- sessions_timeslot %>%
        filter(.data$ChargingPoint == chpoint)

      if (nrow(sessions_chpoint) > 2) {
        message(paste(
          "Warning: more than 2 sessions in charging point",
          chpoint, "at", sessions_timeslot$Timeslot[1]
        ))
        print(sessions_chpoint)
        sessions_chpoint <- sessions_chpoint[c(1, 2), ]
      }

      amps_charging_point_max <- ifelse(
        sum(sessions_chpoint$Phases) <= 3, 16, 12.5
      )

      sessions_timeslot$ChargingPointLimit[
        sessions_timeslot$ChargingPoint == chpoint
      ] <- amps_charging_point_max
    }

    # Charging current of session, comparing:
    #   1. Charging point limit
    #   2. MSR limit
    #   4. Minimum charging current per vehicle (typically 8A). The equivalence
    #      of the rotation system is to divide minimum current by the number
    #      of charging cars during that time slot.
    sessions_timeslot <- sessions_timeslot %>%
      mutate(
        Amps = max(
          min(.data$ChargingPointLimit, amps_msr_max),
          amps_car_min/nrow(sessions_timeslot)
        )
      )

    # For every session update in `sessions` table
    #   1. The charging power during THIS time slot (session id == `ID`)
    #   2. The energy left for the ALL time slots of the session (session id == `Session`)
    for (s in 1:nrow(sessions_timeslot)) {
      session_power <- sessions_timeslot$Amps[s]*230*sessions_timeslot$Phases[s]/1000
      session_energy <- session_power*resolution/60

      # If energy left is less than the energy that can be charged in this
      # time slot, then only charge the energy left.
      if (sessions_timeslot$EnergyLeft[s] < session_energy) {
        session_power <- sessions_timeslot$EnergyLeft[s]/(resolution/60)
        session_energy <- sessions_timeslot$EnergyLeft[s]
      }

      if (include_log) {
        log_message <- paste(
          as_datetime(timeslot, tz = tz(grid_capacity$datetime)),
          "- Session", sessions_timeslot$Session[s],
          "at", session_power, "kW. Charged",
          session_energy, "kWh of",
          sessions_timeslot$EnergyLeft[s], "kWh required"
        )

        message(log_message)
        log_messages <- c(
          log_messages,
          log_message
        )
      }

      # Update power
      sessions_expanded$Power[
        sessions_expanded$ID == sessions_timeslot$ID[s]
      ] <- session_power

      # Update energy
      session_energy_idx <- sessions_expanded$Session == sessions_timeslot$Session[s]
      sessions_expanded$EnergyLeft[session_energy_idx] <-
        round(sessions_timeslot$EnergyLeft[s] - session_energy, 2)
    }
  }

  return(
    list(
      sessions = sessions_expanded,
      log = log_messages
    )
  )
}
