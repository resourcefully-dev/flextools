# Battery optimal charging/discharging profile

See the formulation of the optimization problems in the [documentation
website](https://resourcefully-dev.github.io/flextools/).

## Usage

``` r
add_battery_optimization(
  opt_data,
  opt_objective = "grid",
  Bcap,
  Bc,
  Bd,
  SOCmin = 0,
  SOCmax = 100,
  SOCini = NULL,
  window_days = 1,
  window_start_hour = 0,
  flex_window_hours = 24,
  lambda = 0,
  charge_eff = 1,
  discharge_eff = 1,
  cycle_cost = 0
)
```

## Arguments

- opt_data:

  tibble, optimization contextual data. The first column must be named
  `datetime` (mandatory). Optional columns:

  - `static`: static power demand (kW)

  - `production`: local generation (kW)

  - `import_capacity`: max grid import (kW)

  - `export_capacity`: max grid export (kW)

  - `price_imported`: energy import price (required for cost/combined)

  - `price_exported`: energy export price (required for cost/combined)

- opt_objective:

  character or numeric. `"grid"` (default), `"capacity"`, `"cost"`, or a
  numeric weight `w` where `w=1` is pure grid and `w=0` is pure cost.

- Bcap:

  numeric, battery capacity (kWh)

- Bc:

  numeric, maximum charging power (kW)

- Bd:

  numeric, maximum discharging power (kW)

- SOCmin:

  numeric, minimum State-of-Charge (%)

- SOCmax:

  numeric, maximum State-of-Charge (%)

- SOCini:

  numeric, initial State-of-Charge (%). Defaults to `SOCmin`.

- window_days:

  integer, optimization window length in days.

- window_start_hour:

  integer, start hour of each optimization window.

- flex_window_hours:

  numeric, flexibility window length (hours).

- lambda:

  numeric, ramping penalty weight. Penalises rapid changes in battery
  power between consecutive time slots.

- charge_eff:

  numeric, charging efficiency in (0, 1\]. Default 1 (lossless). Embeds
  round-trip losses in the SOC constraints for accurate energy
  accounting.

- discharge_eff:

  numeric, discharging efficiency in (0, 1\]. Default 1 (lossless). See
  `charge_eff`.

- cycle_cost:

  numeric, degradation cost per kWh cycled (Euro/kWh). Default 0. Adds a
  linear penalty on battery discharge so the optimizer trades off energy
  cost savings against battery wear. When positive, the problem is
  solved as a pure LP (no binary variables) which is substantially
  faster than the default MILP.

## Value

numeric vector

## Examples

``` r
library(dplyr)
#> 
#> Attaching package: ‘dplyr’
#> The following objects are masked from ‘package:stats’:
#> 
#>     filter, lag
#> The following objects are masked from ‘package:base’:
#> 
#>     intersect, setdiff, setequal, union
opt_data <- flextools::energy_profiles %>%
  filter(lubridate::isoweek(datetime) == 18) %>%
  rename(production = "solar", static = "building") %>%
  select(any_of(c(
    "datetime", "production", "static", "price_imported", "price_exported"
  )))
opt_battery <- opt_data %>%
  add_battery_optimization(
    opt_objective = "grid",
    Bcap = 50, Bc = 4, Bd = 4,
    window_start_hour = 5
  )
```
