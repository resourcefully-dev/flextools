# Get a summary of the energy charged per session

A table is provided containing the originally required energy for every
session and the actual energy charged with the smart charging program
(also in percentage).

## Usage

``` r
summarise_energy_charged(smart_charging, sessions)
```

## Arguments

- smart_charging:

  SmartCharging object, returned by function
  [`smart_charging()`](https://resourcefully-dev.github.io/flextools/reference/smart_charging.md)

- sessions:

  tibble, sessions data set containig the following variables:
  `"Session"`, `"Timecycle"`, `"Profile"`, `"ConnectionStartDateTime"`,
  `"ConnectionHours"`, `"Power"` and `"Energy"`.

## Value

tibble

## Examples

``` r
library(dplyr)

sessions <- evsim::california_ev_sessions_profiles %>%
  slice_head(n = 50) %>%
  evsim::adapt_charging_features(time_resolution = 15)
sessions_demand <- evsim::get_demand(sessions, resolution = 15)

# Don't require any other variable than datetime, since we don't
# care about local generation (just peak shaving objective)
opt_data <- tibble(
  datetime = sessions_demand$datetime,
  production = 0
)
sc_results <- smart_charging(
  sessions, opt_data, opt_objective = "grid", method = "curtail",
  window_days = 1, window_start_hour = 6,
  responsive = list(Workday = list(Worktime = 0.9)),
  energy_min = 0.5
)

summarise_energy_charged(sc_results, sessions)
#> # A tibble: 50 × 4
#>    Session EnergyRequired EnergyCharged PctEnergyCharged
#>    <chr>            <dbl>         <dbl>            <dbl>
#>  1 S1                6.31          6.31              100
#>  2 S2               22.8          18.6                81
#>  3 S3               25.4          19.5                77
#>  4 S4                7.53          5.68               75
#>  5 S5               20.7          15.6                76
#>  6 S6                5.01          3.85               77
#>  7 S7                8.42          6.93               82
#>  8 S8               17.1          13.6                80
#>  9 S9               15.7          12.0                76
#> 10 S10              58.9          43.0                73
#> # ℹ 40 more rows
```
