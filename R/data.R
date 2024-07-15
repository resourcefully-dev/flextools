

#' Time-series profiles for consumption, production and energy prices
#'
#' A tibble with time-series profiles for solar production, building consumption
#' and energy prices
#'
#' @format A tibble
#' \describe{
#'   \item{solar}{Solar PV production (in kW)}
#'   \item{building}{Consumption profile of a building (in kW)}
#'   \item{price_imported}{Imported energy price (in €/kWh)}
#'   \item{price_exported}{Exported energy price (in €/kWh)}
#'   \item{price_turn_up}{Balancing price for increasing demand (in €/kWh)}
#'   \item{price_turn_down}{Balancing price for decreasing demand (in €/kWh)}
#' }
#' @keywords internal
#'
"energy_profiles"



