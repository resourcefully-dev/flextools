

#' Storage losses over time
#'
#' @param power numeric vector, being positive when charging and negative when discharging
#' @param loss numeric, the hourly storage loss in percentage (%/hour)
#'
#' @return numeric vector
#' @export
#'
get_storage_losses <- function(power, loss) {
  storage <- cumsum(power)
  losses <- storage/(1-loss/100) - storage
  return( losses )
}


#' Accumulated storage level
#'
#' @param power numeric vector, being positive when charging and negative when discharging
#' @param init numeric, initial storage level
#' @param loss numeric, the hourly storage loss in percentage (%/hour), passed to `get_storage_losses` function
#'
#' @return numeric vector
#' @export
#'
#' @importFrom dplyr lag
#'
get_storage_level <- function(power, init = 0, loss = 0) {
  storage_losses <- get_storage_losses(power, loss)
  pmax(c(init, cumsum(power + storage_losses) - cumsum(lag(storage_losses, default = 0))), 0)
}


#' Add `storage` column to time-series tibble
#'
#' @param df tibble, being the first column `datetime` variable
#' @param power numeric vector, being positive when charging and negative when discharging
#' @param init numeric, initial storage level
#' @param loss numeric, the hourly storage loss in percentage (%/hour), passed to `get_storage_losses` function
#'
#' @return tibble, with extra column `storage`
#' @export
#'
#' @importFrom lubridate minutes
#' @importFrom dplyr left_join tibble %>% select everything
#'
add_storage_level <- function(df, power, init = 0, loss = 0) {
  storage <- get_storage_level(power, init, loss)
  resolution_minutes <- as.integer(df$datetime[2] - df$datetime[1], units = 'minutes')
  new_datetime <- c(df$datetime, df$datetime[nrow(df)] + minutes(resolution_minutes))

  left_join(tibble(datetime = new_datetime, storage), df, by = 'datetime') %>%
    select(everything(), 'storage')
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




#' Battery optimal charging/discharging profile
#'
#' @param w numeric between `0` and `1`, being the weight of the flexibility potential
#' @param G numeric vector, being the renewable generation profile
#' @param L numeric vector, being the load profile
#' @param Bcap numeric, capacity of the battery
#' @param Bc numeric, maximum charging power
#' @param Bd numeric, maximum discharging power
#' @param SOCmin numeric, minimum State-of-Charge of the battery
#' @param SOCmax numeric, maximum State-of-Charge of the battery
#' @param SOCini numeric, required State-of-Charge at the beginning/end of optimization window
#' @param window_length integer, window length. If `NULL`, the window length will be the length of `G`
#'
#' @return numeric vector
#' @export
#'
#' @importFrom reticulate r_to_py
#'
add_battery_optimization <- function(w, G, L, Bcap, Bc, Bd, SOCmin = 0, SOCmax = 100, SOCini = NULL, window_length = NULL) {
  if (!pyenv.exists()) load.pyenv()
  pyenv$add_battery_time_series(
    w,
    G,
    L,
    Bcap,
    Bc,
    Bd,
    SOCmin,
    SOCmax,
    SOCini = r_to_py(SOCini),
    window_length = r_to_py(window_length)
  )
}


#' Simple battery profile
#'
#' Charging when there is surplus, discharging when there is deficit
#'
#' @param G numeric vector, being the renewable generation profile
#' @param L numeric vector, being the load profile
#' @param Bcap numeric, capacity of the battery
#' @param Bc numeric, maximum charging power
#' @param Bd numeric, maximum discharging power
#' @param SOCini numeric, required State-of-Charge at the window beginning
#'
#' @return numeric vector
#' @export
#'
#' @importFrom dplyr lag
#'
add_battery_simple <- function(G, L, Bcap, Bc, Bd, SOCini = 0) {
  storage <- rep(0, (length(G)+1))
  storage[1] <- Bcap*SOCini/100
  battery_potential <- pmax(pmin(G - L, Bc), - Bd)
  # battery_potential[!(hour(datetime) %in% discharging_hours) & (battery_potential < 0)] <- 0

  for (i in 2:length(storage)) {
    storage_i <- storage[i-1] + battery_potential[i-1]
    storage_i <- pmax(pmin(storage_i, Bcap), 0)
    storage[i] <- storage_i
  }

  battery <- c(storage, NA) - lag(storage)
  return(battery[!is.na(battery)])
}



