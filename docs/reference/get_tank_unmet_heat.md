# Unmet heat of a tank schedule

The heat that would have been required in each slot to keep the tank at
least at its comfort floor, for any compressor schedule: the static
schedule's counterpart of `attr(result, "unmet_heat_kw")` of
[`add_tank_optimization()`](https://resourcefully-dev.github.io/flextools/reference/add_tank_optimization.md),
which uses this same function. Per slot of `dt` hours, with the level
`S` following
[`get_tank_level()`](https://resourcefully-dev.github.io/flextools/reference/get_tank_level.md)
(it stops at 0, the inlet temperature), `Smin = Tcap * SOCmin / 100`,
`k = (1 - loss/100)^dt` and
`S~[t] = k * S[t-1] + (cop[t] * power[t] + backup[t] - heat_demand[t]) * dt`
the level the balance asks for:

## Usage

``` r
get_tank_unmet_heat(
  power,
  heat_demand,
  cop,
  init,
  Tcap,
  SOCmin,
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

- Tcap:

  numeric, tank capacity (kWh thermal, \> 0), as in
  [`add_tank_optimization()`](https://resourcefully-dev.github.io/flextools/reference/add_tank_optimization.md):
  the heat between the inlet water temperature and the top of the tank

- SOCmin:

  numeric, comfort floor (%, between 0 and 100)

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

numeric vector of the unmet heat (kW thermal), one value per slot,
rounded to 2 decimals

## Details

`unmet[t] * dt = max(0, min(Smin, k * S[t-1]) - S~[t])`

that is the heat the tank gives from below its comfort floor, plus the
demand it cannot serve at all at the inlet temperature. It is positive
while the level falls below the floor, and 0 while the level stays at or
above it or rises back towards it; standing losses below the floor are
not counted, and neither is a slot whose gap is within half a rounding
step of the power (`0.005 * max(cop, 1) * dt` kWh).

## Examples

``` r
# A 3 kWh tank with its floor at 1 kWh, emptied by 2 kW for 2 hours
get_tank_unmet_heat(
  power = c(0, 0, 0), heat_demand = c(2, 2, 0), cop = 3,
  init = 3, Tcap = 3, SOCmin = 100 / 3
)
#> [1] 0 2 0
```
