% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/storage.R
\name{add_battery_simple}
\alias{add_battery_simple}
\title{Simple battery profile}
\usage{
add_battery_simple(df, Bcap, Bc, Bd, SOCmin = 0, SOCmax = 100, SOCini = 0)
}
\arguments{
\item{df}{tibble, time-series tibble with columns \code{datetime}, \code{production}
and \code{consumption}}

\item{Bcap}{numeric, capacity of the battery}

\item{Bc}{numeric, maximum charging power}

\item{Bd}{numeric, maximum discharging power}

\item{SOCmin}{numeric, minimum State-of-Charge of the battery}

\item{SOCmax}{numeric, maximum State-of-Charge of the battery}

\item{SOCini}{numeric, required State-of-Charge at the beginning of time-series}
}
\value{
numeric vector
}
\description{
Charging when there is surplus, discharging when there is deficit
}
\examples{
library(dplyr)
df <- select(
  energy_profiles,
  "datetime",
  production = "solar",
  consumption = "building"
)

df_batt <- df \%>\%
  mutate(
    battery = add_battery_simple(df, Bcap = 100, Bc = 10, Bd = 10)
  )

df_batt \%>\% dygraphs::dygraph()

}
