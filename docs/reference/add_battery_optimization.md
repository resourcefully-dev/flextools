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
  discharge_eff = 1
)
```

## Arguments

- opt_data:

  tibble, optimization contextual data. The first column must be named
  `datetime` (mandatory) containing the date time sequence where the
  optimization algorithm is applied. The other columns can be
  (optional):

  - `static`: static power demand (in kW) from other sectors like
    buildings, offices, etc.

  - `import_capacity`: maximum imported power from the grid (in kW), for
    example the contracted power with the energy company.

  - `export_capacity`: maximum exported power from the grid (in kW), for
    example the contracted power with the energy company.

  - `production`: local power generation (in kW). This is used when
    `opt_objective = "grid"`.

  - `price_imported`: price for imported energy (€/kWh). This is used
    when `opt_objective = "cost"`.

  - `price_exported`: price for exported energy (€/kWh). This is used
    when `opt_objective = "cost"`.

  - `price_turn_down`: price for turn-down energy use (€/kWh). This is
    used when `opt_objective = "cost"`.

  - `price_turn_up`: price for turn-up energy use (€/kWh). This is used
    when `opt_objective = "cost"`.

- opt_objective:

  character or numeric. Optimization objective can be `"grid"`
  (default), `"cost"` or `"curtail"`, or a number between `0` and `1` to
  perform combined optimization where `0 == "cost"` and `1 == "grid"`.

- Bcap:

  numeric, capacity of the battery (in kWh)

- Bc:

  numeric, maximum charging power (in kW)

- Bd:

  numeric, maximum discharging power (in kW)

- SOCmin:

  numeric, minimum State-of-Charge of the battery

- SOCmax:

  numeric, maximum State-of-Charge of the battery

- SOCini:

  numeric, required State-of-Charge at the beginning/end of optimization
  window

- window_days:

  integer, number of days to consider as optimization window.

- window_start_hour:

  integer, starting hour of the optimization window.

- flex_window_hours:

  integer, flexibility window length, in hours. This optional feature
  lets you apply flexibility only during few hours from the
  `window_start_hour`. It must be lower than `window_days*24` hours.

- lambda:

  numeric, penalty on change for the battery compared to the previous
  time slot.

- charge_eff:

  numeric, battery charging efficiency (from 0 to 1, default 1).

- discharge_eff:

  numeric, battery discharging efficiency (from 0 to 1, default 1).

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
  rename(
    production = "solar"
  ) %>%
  select(any_of(c(
    "datetime", "production", "building", "price_imported", "price_exported"
  ))) %>%
  mutate(
    static = .data$building
  )
  opt_battery <- opt_data %>%
    add_battery_optimization(
      opt_objective = 0.5,
      Bcap = 50, Bc = 4, Bd = 4,
      window_start_hour = 5
    )
#> ⚠️ Optimization warning: optimization not feasible in some windows. Removing grid constraints.
#> ⚠️ Optimization warning: dual infeasible. Disabling battery for some windows.
```
