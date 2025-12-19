# Interactive Log Viewer

Launches an interactive Shiny app to explore smart charging logs by
window and profile. This function requires the `shiny` package to be
installed.

## Usage

``` r
view_smart_charging_logs(smart_charging)
```

## Arguments

- smart_charging:

  `SmartCharging` object returned by `smart_charging` function

## Value

Opens Viewer with the log viewer mini app

## Examples

``` r
if (FALSE) { # \dontrun{
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
  sessions, opt_data,
  opt_objective = "grid",
  method = "curtail",
  window_days = 1, window_start_hour = 6,
  include_log = TRUE
)
view_smart_charging_logs(sc_results)
} # }
```
