

#' Storage losses over time
#'
#' @param power numeric vector, being positive when charging and negative when discharging
#' @param loss numeric, the hourly storage loss in percentage (%/hour)
#' @param time_resolution numeric, time resolution of the time-series (in minutes)
#'
#' @return numeric vector
#' @export
#'
get_storage_losses <- function(power, loss, time_resolution = 60) {
  storage <- cumsum(power*time_resolution/60)
  losses <- storage/(1-loss/100) - storage
  return( losses )
}


#' Accumulated storage level
#'
#' @param power numeric vector, being positive when charging and negative when discharging
#' @param init numeric, initial storage level (in energy units, not %)
#' @param loss numeric, the hourly storage loss in percentage (%/hour), passed to `get_storage_losses` function
#' @param time_resolution numeric, time resolution of the time-series (in minutes)
#'
#' @return numeric vector
#' @export
#'
#' @importFrom dplyr lag
#'
get_storage_level <- function(power, init = 0, loss = 0, time_resolution = 60) {
  storage_losses <- get_storage_losses(power, loss, time_resolution = time_resolution)
  storage_vct <- pmax(c(init, init + cumsum(power*time_resolution/60 + storage_losses) - cumsum(lag(storage_losses, default = 0))), 0)
  return( storage_vct[seq_len(length(power))] ) # The `storage_vct` would be T+1 length for the cumsum starting at `init`
}



#' Losses due to charging/discharging process
#'
#' @param power numeric vector, being positive when charging and negative when discharging
#' @param loss_charge numeric, the charging loss in percentage (%)
#' @param loss_discharge numeric, the discharging loss in percentage (%)
#'
#' @return numeric vector
#' @export
#'
get_conversion_losses <- function(power, loss_charge, loss_discharge) {
  losses <- rep(0, length(power))
  losses[power>0] <- power[power>0]/(1-loss_charge/100) - power[power>0]
  losses[power<0] <- abs(power[power<0])/(1-loss_discharge/100) - abs(power[power<0])
  return( losses )
}


#' Simple battery profile
#'
#' Charging when there is surplus, discharging when there is deficit
#'
#' @param df tibble, time-series tibble with columns `datetime`, `production`
#' and `consumption`
#' @param Bcap numeric, capacity of the battery
#' @param Bc numeric, maximum charging power
#' @param Bd numeric, maximum discharging power
#' @param SOCmin numeric, minimum State-of-Charge of the battery
#' @param SOCmax numeric, maximum State-of-Charge of the battery
#' @param SOCini numeric, required State-of-Charge at the beginning of time-series
#'
#' @return numeric vector
#' @export
#'
#' @importFrom dplyr lag
#'
#' @examples
#' library(dplyr)
#' df <- select(
#'   energy_profiles,
#'   "datetime",
#'   production = "solar",
#'   consumption = "building"
#' )
#'
#' df_batt <- df %>%
#'   mutate(
#'     battery = add_battery_simple(df, Bcap = 100, Bc = 10, Bd = 10)
#'   )
#'
#' df_batt %>% dygraphs::dygraph()
#'
add_battery_simple <- function(df, Bcap, Bc, Bd, SOCmin = 0, SOCmax = 100, SOCini = 0) {
  time_resolution <- get_time_resolution(df$datetime)
  storage <- rep(0, (nrow(df)+1))
  storage[1] <- Bcap*SOCini/100
  battery_potential <- pmax(pmin(df$production - df$consumption, Bc), - Bd)
  # battery_potential[!(hour(datetime) %in% discharging_hours) & (battery_potential < 0)] <- 0

  for (i in 2:length(storage)) {
    storage_i <- storage[i-1] + battery_potential[i-1]*time_resolution/60
    storage_i <- max(min(storage_i, Bcap*SOCmax/100), Bcap*SOCmin/100)
    storage[i] <- storage_i
  }

  battery <- storage - lag(storage, default = NA)

  battery <- battery[seq(2, length(battery))]/(time_resolution/60)

  return( battery )
}



