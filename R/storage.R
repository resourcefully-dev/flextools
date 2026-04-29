#' Storage losses over time
#'
#' Standing loss proportional to stored energy each time step.
#'
#' @param power numeric vector, being positive when charging and negative when discharging
#' @param loss numeric, the hourly storage loss in percentage (%/hour)
#' @param time_resolution numeric, time resolution of the time-series (in minutes)
#'
#' @return numeric vector
#' @export
#'
get_storage_losses <- function(power, loss, time_resolution = 60) {
  if (loss < 0 || loss >= 100) {
    stop("Error: loss must be between 0 and 100 (%/hour)")
  }
  if (time_resolution <= 0) {
    stop("Error: time_resolution must be greater than 0")
  }

  storage <- cumsum(power * time_resolution / 60)
  storage <- pmax(storage, 0)

  # standing loss proportional to stored energy each time step,
  # per-step loss factor with 1 - (1 - loss/100)^(time_resolution/60)
  loss_step <- 1 - (1 - loss / 100)^(time_resolution / 60)
  losses <- storage / (1 - loss_step) - storage

  return(losses)
}


#' Losses due to charging/discharging process
#'
#' @param power numeric vector, being positive when charging and negative when discharging
#' @param charge_eff numeric, charging efficiency (from 0 to 1, default to 1)
#' @param discharge_eff numeric, discharging efficiency (from 0 to 1, default to 1)
#'
#' @return numeric vector
#' @export
#'
get_conversion_losses <- function(power, charge_eff = 1, discharge_eff = 1) {
  losses <- rep(0, length(power))

  # charging: final grid power = power / eta_c
  losses[power > 0] <- power[power > 0] * (1 / charge_eff - 1)

  # discharging: final grid power = power * eta_d
  losses[power < 0] <- abs(power[power < 0]) * (1 - discharge_eff)

  losses
  return(losses)
}


#' Accumulated storage level (energy)
#'
#' Each value represents the energy level at the beginning of the time slot,
#' starting from the provided initial State-of-Charge.
#'
#' `power` is interpreted as the **grid-side** battery power: positive when the
#' grid is charging the battery, negative when the battery is supplying the grid.
#' Pass `charge_eff` / `discharge_eff` to convert grid-side power to actual
#' stored energy (grid draws `power / charge_eff` when charging; storage releases
#' `|power| / discharge_eff` when discharging).
#'
#' If `power` is already **storage-side** (e.g. from `add_battery_optimization()`),
#' call with the default efficiencies of 1.
#'
#' @param power numeric vector, being positive when charging and negative when discharging
#' @param init numeric, initial storage level (in kWh, not %)
#' @param charge_eff numeric, charging efficiency (from 0 to 1, default to 1)
#' @param discharge_eff numeric, discharging efficiency (from 0 to 1, default to 1)
#' @param time_resolution numeric, time resolution of the time-series (in minutes)
#'
#' @return numeric vector of energy stored
#' @export
#'
#' @importFrom dplyr lag
#'
get_storage_level <- function(
  power,
  init = 0,
  charge_eff = 1,
  discharge_eff = 1,
  time_resolution = 60
) {
  if (charge_eff <= 0 || discharge_eff <= 0) {
    stop("Error: efficiencies must be greater than 0")
  }
  if (charge_eff > 1 || discharge_eff > 1) {
    stop("Error: efficiencies must be lower or equal to 1")
  }

  energy_delta <- power * time_resolution / 60
  energy_delta[energy_delta >= 0] <- energy_delta[energy_delta >= 0] * charge_eff
  energy_delta[energy_delta < 0] <- energy_delta[energy_delta < 0] / discharge_eff

  storage <- c(init, init + cumsum(energy_delta))
  storage <- round(storage[seq_len(length(power))], 2)
  return(storage)
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
#' @importFrom timefully get_time_resolution
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
add_battery_simple <- function(
  df,
  Bcap,
  Bc,
  Bd,
  SOCmin = 0,
  SOCmax = 100,
  SOCini = 0
) {
  time_resolution <- get_time_resolution(df$datetime)
  storage <- rep(0, (nrow(df) + 1))
  storage[1] <- Bcap * SOCini / 100
  battery_potential <- pmax(pmin(df$production - df$consumption, Bc), -Bd)
  # battery_potential[!(hour(datetime) %in% discharging_hours) & (battery_potential < 0)] <- 0

  for (i in 2:length(storage)) {
    storage_i <- storage[i - 1] +
      battery_potential[i - 1] * time_resolution / 60
    storage_i <- max(min(storage_i, Bcap * SOCmax / 100), Bcap * SOCmin / 100)
    storage[i] <- storage_i
  }

  battery <- storage - lag(storage, default = NA)

  battery <- battery[seq(2, length(battery))] / (time_resolution / 60)

  return(battery)
}
