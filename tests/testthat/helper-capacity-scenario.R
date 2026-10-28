# A site for the "capacity" objective of smart_charging() and smart_v2g(), one
# day of 15 minutes (windows start at 06:00): an office of 60 kW from 07:00 to
# 19:00 (10 kW otherwise) and a 150 kW PV plant from 09:00 to 18:00. The site
# imports in the morning, exports above the 50 kW export capacity around noon
# (some 110 kWh above it) and never imports above the 100 kW import capacity.
# Three vehicles plug in at 07:00 and leave at 17:00 (20 kWh each, at 0.5
# state of charge), two at 18:00 and leave at 05:00 (20 kWh each): their
# uncontrolled charge peaks the evening at 32 kW, well within the caps.
capacity_scenario_dttm <- seq(
  as.POSIXct("2025-03-12 00:00", tz = "UTC"),
  as.POSIXct("2025-03-13 23:45", tz = "UTC"),
  by = "15 min"
)

capacity_scenario_opt_data <- function(import_capacity = 100,
                                       export_capacity = 50) {
  h <- as.numeric(difftime(
    capacity_scenario_dttm,
    as.POSIXct("2025-03-12 00:00", tz = "UTC"),
    units = "hours"
  )) %% 24
  pv <- ifelse(h >= 9 & h <= 18, 150 * sin(pi * (h - 9) / 9), 0)
  tibble::tibble(
    datetime = capacity_scenario_dttm,
    production = round(pv, 2),
    static = ifelse(h >= 7 & h < 19, 60, 10),
    import_capacity = import_capacity,
    export_capacity = export_capacity
  )
}

capacity_scenario_sessions <- function() {
  session <- function(id, start, hours) {
    tibble::tibble(
      Session = paste0("S", id),
      Timecycle = "Workday",
      Profile = "Fleet",
      ConnectionStartDateTime = as.POSIXct("2025-03-12 00:00", tz = "UTC") +
        start * 3600,
      ConnectionHours = hours,
      Power = 11,
      Energy = 20,
      StateOfCharge = 50
    )
  }
  dplyr::bind_rows(
    session(1, 7, 10),
    session(2, 7, 10),
    session(3, 7, 10),
    session(4, 18, 11),
    session(5, 18, 11)
  )
}

capacity_scenario_uncontrolled <- function(sessions) {
  evsim::get_demand(
    evsim::adapt_charging_features(sessions, time_resolution = 15),
    dttm_seq = capacity_scenario_dttm
  )$Fleet
}

# Energy (kWh) imported above `import_capacity` and exported above
# `export_capacity` with the fleet load `fleet`.
capacity_scenario_excess <- function(opt_data, fleet) {
  net <- opt_data$static + fleet - opt_data$production
  c(
    import = sum(pmax(net - opt_data$import_capacity, 0)) / 4,
    export = sum(pmax(-net - opt_data$export_capacity, 0)) / 4
  )
}
