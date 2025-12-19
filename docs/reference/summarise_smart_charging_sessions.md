# Get a summary of the new schedule of charging sessions

A table is provided containing the number of `Considered`, `Responsive`,
`Flexbile` and `Exploited` sessions, by user profile.

## Usage

``` r
summarise_smart_charging_sessions(smart_charging)
```

## Arguments

- smart_charging:

  SmartCharging object, returned by function
  [`smart_charging()`](https://resourcefully-dev.github.io/flextools/reference/smart_charging.md)

## Value

tibble with columns: `timecycle` (time-cycle name), `profile` (user
profile name), `group` (name of sessions' group), `subgroup` (nome of
sessions' subgroup), `n_sessions` (number of sessions) and `pct`
(percentage of subgroup sessions from the group)

## Examples

``` r
library(dplyr)

# Use first 50 sessions
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
  sessions, opt_data,
  opt_objective = "grid", method = "curtail",
  window_days = 1, window_start_hour = 6,
  responsive = list(Workday = list(Worktime = 0.9)),
  energy_min = 0.5
)

summarise_smart_charging_sessions(sc_results)
#> # A tibble: 7 × 6
#>   timecycle profile  group      subgroup       n_sessions   pct
#>   <chr>     <chr>    <chr>      <chr>               <int> <dbl>
#> 1 Workday   Visit    Total      Not considered          9   100
#> 2 Workday   Worktime Total      Considered             15    37
#> 3 Workday   Worktime Total      Not considered         26    63
#> 4 Workday   Worktime Considered Responsive             14    93
#> 5 Workday   Worktime Considered Non responsive          1     7
#> 6 Workday   Worktime Responsive Flexible               14   100
#> 7 Workday   Worktime Flexible   Exploited              14   100
```
