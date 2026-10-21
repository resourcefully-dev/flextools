# Tank level (thermal energy)

The tank's counterpart of
[`get_storage_level()`](https://resourcefully-dev.github.io/flextools/reference/get_storage_level.md):
the level in kWh thermal at the start of each slot, starting from
`init`, with
`S[t+1] = max(k * S[t] + (cop[t] * power[t] + backup[t] - heat_demand[t]) * dt, 0)`
and `k = (1 - loss/100)^dt`, `dt` being the time resolution in hours.
Level 0 is the inlet temperature: an empty tank gives no more heat, and
the demand it cannot serve is unmet heat.

## Usage

``` r
get_tank_level(
  power,
  heat_demand,
  cop,
  init,
  loss = 0,
  time_resolution = 60,
  backup = 0
)
```

## Arguments

- power:

  numeric vector, compressor electric power (kW), such as the result of
  [`add_tank_optimization()`](https://resourcefully-dev.github.io/flextools/reference/add_tank_optimization.md)

- heat_demand:

  numeric vector, heat demand (kW thermal)

- cop:

  numeric vector, coefficient of performance (heat per electric kWh)

- init:

  numeric, level before the first slot (in kWh, not %)

- loss:

  numeric, standing loss of the level (% per hour)

- time_resolution:

  numeric, time resolution of the time-series (in minutes)

- backup:

  numeric, backup heater power (kW electric = kW thermal):
  `attr(result, "backup_kw")` of
  [`add_tank_optimization()`](https://resourcefully-dev.github.io/flextools/reference/add_tank_optimization.md),
  or 0 without a backup heater

## Value

numeric vector of the level (kWh), rounded to 2 decimals
