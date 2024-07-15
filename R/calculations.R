


# Energy calculations -----------------------------------------------------

#' Get energy balance time-series
#'
#' Input data frame must have columns `consumption`, `production`.
#' Output data frame has extra columns `balance`, `local`, `imported`, `exported`.
#' Column `balance` is positive when there is more production than consumption.
#'
#' @param df tibble, with columns `datetime`, `consumption`, `production`
#'
#' @return tibble
#' @export
#'
#' @importFrom dplyr %>% mutate
#' @importFrom rlang .data
#'
get_energy_balance <- function(df) {
  df %>%
    mutate(
      balance = .data$production - .data$consumption,
      local = ifelse(.data$balance > 0, .data$consumption, .data$production),
      imported = .data$consumption - .data$local,
      exported = .data$production - .data$local,
    )
}


#' Obtain a list of energy indicators given the energy profiles
#'
#' @param df data.frame or tibble, with columns `datetime`, `consumption`, `production` and `kg_co2_kwh`
#' @param kg_co2_kwh factor of CO2 kg emissions per kWh of energy consumed from the distribution grid
#'
#' @return named list
#' @export
#'
#' @importFrom dplyr %>% mutate summarise
#' @importFrom rlang .data
#'
get_energy_kpis <- function(df, kg_co2_kwh = 0.5) {
  if (!('kg_co2_kwh' %in% names(df))) {
    df[['kg_co2_kwh']] <- kg_co2_kwh
  }
  # Energy = Power * resolution(h)
  resolution <- as.numeric(df$datetime[2] - df$datetime[1], unit='hours')

  df2 <- df %>%
    get_energy_balance() %>%
    mutate(
      kg_co2 = .data$kg_co2_kwh*.data$imported*resolution,
    )

  peak_to_grid_dttm <- df2$datetime[which(df2$exported == max(df2$exported, na.rm = T))][1]
  peak_from_grid_dttm <- df2$datetime[which(df2$imported == max(df2$imported, na.rm = T))][1]

  df2 %>%
    summarise(
      total_consumption = sum(.data$consumption*resolution, na.rm = T),
      total_production = sum(.data$production*resolution, na.rm = T),
      total_local = sum(.data$local*resolution, na.rm = T),
      total_exported = sum(.data$exported*resolution, na.rm = T),
      total_imported = sum(.data$imported*resolution, na.rm = T),
      selfconsumption = .data$total_local/.data$total_production,
      selfsufficiency = .data$total_local/.data$total_consumption,
      peak_to_grid = max(.data$exported, na.rm = T),
      peak_from_grid = max(.data$imported, na.rm = T),
      peak_to_grid_dttm = peak_to_grid_dttm,
      peak_from_grid_dttm = peak_from_grid_dttm,
      kg_co2 = sum(.data$kg_co2, na.rm = T)
    ) %>%
    as.list()
}



# Costs calculations ------------------------------------------------------

#' Get energy cost
#'
#' The energy cost (in Euros) based on imported and exported energy
#' and the corresponding prices.
#'
#' @param df tibble, with columns `datetime`, `consumption`, `production`, `price_imported` and `price_exported`
#'
#' @return tibble
#' @export
#'
#' @importFrom dplyr %>% mutate pull
#' @importFrom rlang .data
#'
get_energy_cost <- function(df) {
  df %>%
    get_energy_balance() %>%
    mutate(
      cost = .data$imported*.data$price_imported -
        .data$exported*.data$price_exported
    ) %>%
    pull(.data$cost) %>%
    sum(na.rm = T)
}



#' Get imbalance income
#'
#' The energy income (in Euros) based on the difference between the baseline and
#' final power profile for a certain demand. The difference represents the
#' optimal changes due to flexibility, so this function calculates the income
#' from applying this demand-side flexibility.
#'
#' @param df tibble, with columns `datetime`, `demand_baseline`, `demand_final`, `price_turn_up` and `price_turn_down`
#'
#' @return tibble
#' @export
#'
#' @importFrom dplyr %>% mutate pull
#' @importFrom rlang .data
#'
get_imbalance_income <- function(df) {
  df %>%
    mutate(
      demand_diff = .data$demand_final - .data$demand_baseline,
      demand_turn_up = ifelse(
        .data$demand_diff > 0, .data$demand_diff, 0
      ),
      demand_turn_down = ifelse(
        .data$demand_diff < 0, -.data$demand_diff, 0
      ),
      income = .data$demand_turn_up*.data$price_turn_up +
        .data$demand_turn_down*.data$price_turn_down
    ) %>%
    pull(.data$income) %>%
    sum(na.rm = T)
}



