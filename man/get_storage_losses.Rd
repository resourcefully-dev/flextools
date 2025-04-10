% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/storage.R
\name{get_storage_losses}
\alias{get_storage_losses}
\title{Storage losses over time}
\usage{
get_storage_losses(power, loss, time_resolution = 60)
}
\arguments{
\item{power}{numeric vector, being positive when charging and negative when discharging}

\item{loss}{numeric, the hourly storage loss in percentage (\%/hour)}

\item{time_resolution}{numeric, time resolution of the time-series (in minutes)}
}
\value{
numeric vector
}
\description{
Storage losses over time
}
