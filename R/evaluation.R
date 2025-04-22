

# Smart charging impact ---------------------------------------------------

#' Get a summary of the energy charged per session
#'
#' A table is provided containing the originally required energy for every
#' session and the actual energy charged with the smart charging program
#' (also in percentage).
#'
#' @param smart_charging SmartCharging object, returned by function `smart_charging()`
#' @param sessions tibble, sessions data set containig the following variables:
#' `"Session"`, `"Timecycle"`, `"Profile"`, `"ConnectionStartDateTime"`, `"ConnectionHours"`, `"Power"` and `"Energy"`.
#'
#' @return tibble
#' @export
#'
#' @importFrom dplyr %>% mutate group_by summarise left_join select
#' @importFrom rlang .data
#'
#' @examples
#' library(dplyr)
#'
#' sessions <- evsim::california_ev_sessions_profiles %>%
#'   slice_head(n = 50) %>%
#'   evsim::adapt_charging_features(time_resolution = 15)
#' sessions_demand <- evsim::get_demand(sessions, resolution = 15)
#'
#' # Don't require any other variable than datetime, since we don't
#' # care about local generation (just peak shaving objective)
#' opt_data <- tibble(
#'   datetime = sessions_demand$datetime,
#'   production = 0
#' )
#' sc_results <- smart_charging(
#'   sessions, opt_data, opt_objective = "grid", method = "curtail",
#'   window_days = 1, window_start_hour = 6,
#'   responsive = list(Workday = list(Worktime = 0.9)),
#'   energy_min = 0.5
#' )
#'
#' summarise_energy_charged(sc_results, sessions)
#'
summarise_energy_charged <- function(smart_charging, sessions) {
  smart_charging$sessions %>%
    group_by(.data$Session) %>%
    summarise(EnergyCharged = round(sum(.data$Energy), 2)) %>%
    left_join(
      sessions %>% select("Session", EnergyRequired = "Energy"),
      by = "Session"
    ) %>%
    mutate(
      PctEnergyCharged = round(.data$EnergyCharged/.data$EnergyRequired*100)
    )
}


# Energy calculations -----------------------------------------------------

#' Get energy balance time-series
#'
#' Input data frame must have columns `consumption`, `production`.
#' Output data frame has extra columns `net`, `local`, `imported`, `exported`.
#' Column `net` is positive when there is more consumption than production
#'
#' @param df tibble, with columns `datetime`, `consumption`, `production`
#'
#' @return tibble
#' @export
#'
#' @importFrom dplyr %>% mutate
#' @importFrom rlang .data
#'
#' @examples
#' df <- dplyr::select(
#'   energy_profiles,
#'   datetime,
#'   production = solar,
#'   consumption = building
#' )
#' head(df)
#'
#' head(get_energy_balance(df))
#'
#'
get_energy_balance <- function(df) {
  df %>%
    mutate(
      net = .data$consumption - .data$production,
      local = ifelse(.data$net > 0, .data$production, .data$consumption),
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
#' @examples
#' df <- dplyr::select(
#'   energy_profiles,
#'   datetime,
#'   production = solar,
#'   consumption = building
#' )
#' head(df)
#'
#' get_energy_kpis(df)
#'
get_energy_kpis <- function(df, kg_co2_kwh = 0.5) {
  if (!('kg_co2_kwh' %in% names(df))) {
    df[['kg_co2_kwh']] <- kg_co2_kwh
  }
  # Energy = Power * resolution(h)
  resolution <- get_time_resolution(df$datetime, units = "hours")

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

#' Get energy cost time-series
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
#' @examples
#' df <- dplyr::select(
#'   energy_profiles,
#'   datetime,
#'   production = solar,
#'   consumption = building,
#'   price_imported,
#'   price_exported
#' )
#' df <- dplyr::slice_head(df, n = 300)
#' head(df)
#'
#' head(get_energy_cost(df))
#'
get_energy_cost <- function(df) {
  df %>%
    get_energy_balance() %>%
    mutate(
      cost = .data$imported*.data$price_imported -
        .data$exported*.data$price_exported
    )
}

#' Get energy total cost (one number)
#'
#' The total energy cost (in Euros) based on imported and exported energy
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
#' @examples
#' df <- dplyr::select(
#'   energy_profiles,
#'   datetime,
#'   production = solar,
#'   consumption = building,
#'   price_imported,
#'   price_exported
#' )
#' head(df)
#'
#' get_energy_total_cost(df)
#'
get_energy_total_cost <- function(df) {
  df %>%
    get_energy_cost() %>%
    pull("cost") %>%
    sum(na.rm = T)
}



#' Get imbalance income time-series
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
#' @examples
#' df <- dplyr::select(
#'   energy_profiles,
#'   datetime,
#'   demand_baseline = building,
#'   price_turn_up,
#'   price_turn_down
#' )
#'
#' # Build another random consumption profile
#' building_variation <- rnorm(nrow(df), mean = 0, sd = 1)
#' df <- dplyr::mutate(
#'   df, demand_final = demand_baseline + building_variation
#' )
#' head(df)
#'
#' head(get_imbalance_income(df))
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
    )
}

#' Get imbalance total income (one number)
#'
#' The total energy income (in Euros) based on the difference between the baseline and
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
#' @examples
#' df <- dplyr::select(
#'   energy_profiles,
#'   datetime,
#'   demand_baseline = building,
#'   price_turn_up,
#'   price_turn_down
#' )
#'
#' # Build another random consumption profile
#' building_variation <- rnorm(nrow(df), mean = 0, sd = 1)
#' df <- dplyr::mutate(
#'   df, demand_final = demand_baseline + building_variation
#' )
#' head(df)
#'
#' get_imbalance_total_income(df)
#'
get_imbalance_total_income <- function(df) {
  df %>%
    get_imbalance_income() %>%
    pull(.data$income) %>%
    sum(na.rm = T)
}


# Evaluation plots -------------------------------------------------------------------

#' Plot net power interaction with the grid
#'
#' HTML interactive plot showing the hourly energy cost. Also,
#' a comparison between the original scenario is done when
#' `original_df` is not `NULL`.
#'
#' @param df tibble, with columns `datetime`, `consumption`, `production`
#' @param original_df tibble with same columns than `df` corresponding to the
#' original scenario (e.g. without flexibility)
#' @param import_capacity numeric, maximum power to import from the grid (in kW)
#' @param export_capacity numeric, maximum power to export to the grid (in kW, positive).
#' @param ... extra arguments to pass to dygraphs::dyOptions function
#'
#' @return dygraphs plot
#' @export
#'
#' @importFrom dplyr %>% mutate left_join select any_of all_of
#' @importFrom dygraphs dySeries dyLimit
#' @importFrom rlang .data
#'
#' @examples
#' df <- dplyr::select(
#'   energy_profiles,
#'   datetime,
#'   production = solar,
#'   consumption = building
#' )
#' df <- dplyr::slice_head(df, n = 300)
#' head(df)
#'
#' plot_net_power(df)
#'
#' # Build another random building profile
#' building_variation <- rnorm(nrow(df), mean = 0, sd = 1)
#' df2 <- dplyr::mutate(df, consumption = consumption + building_variation)
#' plot_net_power(df2, original_df = df)
#'
plot_net_power <- function(df, original_df = NULL, import_capacity = NULL, export_capacity = NULL, ...) {

  plot_df <- df %>%
    get_energy_balance() %>%
    rename(net_flex = "net")

  if (is.null(original_df)) {
    plot_dy <- plot_df %>%
      select(any_of(c("datetime", "consumption", "production", "net_flex"))) %>%
      plot_ts(ylab = "Power demand (kW)", fillGraph = TRUE, strokeWidth = 2, ...) %>%
      dySeries("production", "Production", color = "orange") %>%
      dySeries("consumption", "Consumption",  color = "navy") %>%
      dySeries("net_flex", "Net power", color = "brown", fillGraph = FALSE)
  } else {
    original_df <- original_df %>%
      get_energy_balance() %>%
      rename(net_static = "net") %>%
      select(all_of(c("datetime", "net_static")))
    plot_dy <- plot_df %>%
      select(all_of(c("datetime", "net_flex"))) %>%
      left_join(original_df,by = "datetime") %>%
      mutate(
        lwr = ifelse(
          .data$net_flex < .data$net_static,
          .data$net_flex, .data$net_static
        ),
        upr = ifelse(
          .data$net_flex > .data$net_static,
          .data$net_flex, .data$net_static
        )
      ) %>%
      select(any_of(c("datetime", "net_static", "net_flex", "lwr", "upr"))) %>%
      plot_ts(ylab = "Net power (kW)", ...) %>%
      dySeries(c("lwr", "net_static", "upr"), label = "Original case", color = "red", strokePattern = "dashed") %>%
      dySeries("net_flex", label = "Flexible case", color = "darkgreen", strokeWidth = 2)
  }

  if (!is.null(import_capacity)) {
    plot_dy <- plot_dy %>%
      dyLimit(import_capacity, color = "brown")
  }

  if (!is.null(export_capacity)) {
    plot_dy <- plot_dy %>%
      dyLimit(-export_capacity, color = "brown")
  }

  plot_dy
}


#' Plot hourly energy cost
#'
#' HTML interactive plot showing the hourly energy cost. Also,
#' a comparison between the original scenario is done when
#' `original_df` is not `NULL`.
#'
#' @param df tibble, with columns `datetime`, `consumption`, `production`,
#' `price_imported` and `price_exported`. If `imbalance = TRUE`, then columns
#'  `demand_baseline`, `demand_final`, `price_turn_up` and `price_turn_down` are
#'  also required.
#' @param original_df tibble with same columns than `df` corresponding to the
#' original scenario (e.g. without flexibility).
#' @param ... extra arguments to pass to dygraphs::dyOptions function.
#'
#' @return dygraphs plot
#' @export
#'
#' @importFrom dplyr %>% mutate left_join select any_of all_of
#' @importFrom dygraphs dySeries dyLimit
#' @importFrom rlang .data
#'
#' @examples
#' df <- dplyr::select(
#'   energy_profiles,
#'   datetime,
#'   production = solar,
#'   consumption = building,
#'   price_imported,
#'   price_exported
#' )
#' df <- dplyr::slice_head(df, n = 300)
#' head(df)
#'
#' plot_energy_cost(df)
#'
#' # Build another random building profile
#' building_variation <- rnorm(nrow(df), mean = 0, sd = 1)
#' df2 <- dplyr::mutate(df, consumption = consumption + building_variation)
#' plot_energy_cost(df2, original_df = df)
#'
plot_energy_cost <- function(df, original_df = NULL, ...) {

  plot_df <- df %>%
    get_energy_cost() %>%
    rename(cost_flex = "cost")

  if (is.null(original_df)) {
    plot_dy <- plot_df %>%
      select(all_of(c("datetime", "cost_flex"))) %>%
      plot_ts(ylab = "Cost (Euros)", fillGraph = TRUE, strokeWidth = 2, ...) %>%
      dySeries("cost_flex", "Cost", color = "navy")
  } else {
    original_df <- original_df %>%
      get_energy_cost() %>%
      rename(cost_static = "cost") %>%
      select(all_of(c("datetime", "cost_static")))
    plot_dy <- plot_df %>%
      select(all_of(c("datetime", "cost_flex"))) %>%
      left_join(original_df,by = "datetime") %>%
      mutate(
        lwr = ifelse(
          .data$cost_flex < .data$cost_static,
          .data$cost_flex, .data$cost_static
        ),
        upr = ifelse(
          .data$cost_flex > .data$cost_static,
          .data$cost_flex, .data$cost_static
        )
      ) %>%
      select(any_of(c("datetime", "cost_static", "cost_flex", "lwr", "upr"))) %>%
      plot_ts(ylab = "Cost (Euros)", ...) %>%
      dySeries(c("lwr", "cost_static", "upr"), label = "Original case", color = "red", strokePattern = "dashed") %>%
      dySeries("cost_flex", label = "Flexible case", color = "darkgreen", strokeWidth = 2)
  }

  plot_dy
}


#' Get percentage of hours for load duration curve
#'
#' The percentage of hours that a certain threshold of power is surpassed
#'
#' @param vct numeric vector of power
#' @param threshold numeric, threshold of power
#'
#' @return numeric
#' @keywords internal
#'
get_percentage_hours <- function(vct, threshold) {
  sum(vct >= threshold)/length(vct)*100
}


#' Load duration curve table
#'
#' The Load Duration Curve (LDC) represents the percentage of time that a specific
#' value of power has been used in the electrical grid for a specific time period.
#' It's widely used for power system planning and grid reliability assessments.
#'
#' @param df tibble, with columns `datetime`, `consumption`, `production`
#'
#' @return dygraphs plot
#' @export
#'
#' @importFrom dplyr %>% tibble group_by summarise
#' @importFrom purrr map_dbl
#' @importFrom rlang .data
#'
#' @examples
#' df <- dplyr::select(
#'   energy_profiles,
#'   datetime,
#'   consumption = building
#' )
#'
#' head(df)
#'
#' get_load_duration_curve(df)
#'
get_load_duration_curve <- function(df) {
  if (!("consumption" %in% names(df))) {
    df$consumption <- 0
  }
  if (!("production" %in% names(df))) {
    df$production <- 0
  }
  net <- df$consumption - df$production
  load_data <- tibble(
    power = seq(min(net), max(net), by = 1),
    pct = round(map_dbl(.data$power, ~ get_percentage_hours(net, .x)), 2)
  ) %>%
    group_by(.data$pct) %>%
    summarise(power = min(.data$power)) # Just one value of power per percentage
  return( load_data )
}


#' Plot of the load duration curve
#'
#' HTML interactive plot showing the graphical representation of a load duration curve.
#' The Load Duration Curve (LDC) represents the percentage of time that a specific
#' value of power has been used in the electrical grid for a specific time period.
#' It's widely used for power system planning and grid reliability assessments.
#' Also, a comparison between the original scenario is done when
#' `original_df` is not `NULL`.
#'
#' @param df tibble, with columns `datetime`, `consumption`, `production`
#' @param original_df tibble with same columns than `df` corresponding to the
#' original scenario (e.g. without flexibility).
#'
#' @return dygraphs plot
#' @export
#'
#' @importFrom dplyr %>% rename full_join arrange
#' @importFrom tidyr fill pivot_longer
#' @importFrom ggplot2 ggplot aes geom_line labs theme_light theme scale_color_manual
#' @importFrom rlang .data
#' @importFrom stats setNames
#'
#' @examples
#' df <- dplyr::select(
#'   energy_profiles,
#'   datetime,
#'   consumption = building
#' )
#' head(df)
#'
#' plot_load_duration_curve(df)
#'
#' # Build another random building profile
#' building_variation <- rnorm(nrow(df), mean = 0, sd = 1)
#' df2 <- dplyr::mutate(df, consumption = consumption + building_variation)
#' plot_load_duration_curve(df2, original_df = df)
#'
plot_load_duration_curve <- function(df, original_df = NULL) {
  plot_df <- df %>%
    get_load_duration_curve()

  if (is.null(original_df)) {
    plot_gg <- plot_df %>%
      ggplot(aes(.data$pct, .data$power)) +
      geom_line(linewidth = 1.2) +
      labs(x = "Percentage of hours (%)", y = "Power (kW)") +
      theme_light() +
      theme(legend.position = "top")
  } else {
    plot_df <- plot_df %>%
      rename(`Flexible case` = "power")
    plot_df_original <- original_df %>%
      get_load_duration_curve() %>%
      rename(`Original case` = "power")

    plot_gg <- full_join(
      plot_df_original,
      plot_df,
      by = "pct"
    ) %>%
      arrange("pct") %>%
      fill("Original case", "Flexible case", .direction = "down") %>%
      pivot_longer(-"pct", names_to = "case", values_to = "power") %>%
      ggplot(aes(.data$pct, .data$power, color = .data$case)) +
      scale_color_manual(values = c("red", "darkgreen") %>% setNames(c("Original case", "Flexible case"))) +
      geom_line(linewidth = 1.2) +
      labs(x = "Percentage of hours (%)", y = "Power (kW)", color = "") +
      theme_light() +
      theme(legend.position = "top")
  }

  plot_gg
}
