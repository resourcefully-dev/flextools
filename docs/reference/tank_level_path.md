# Tank level path (unrounded)

The level at the start of each slot, and after the last one. The level
never goes below 0, the inlet temperature (a tank there gives no more
heat), except by less than half a rounding step of the power.

## Usage

``` r
tank_level_path(power, heat_demand, cop, init, k, dt, backup = 0)
```

## Arguments

- power:

  numeric vector, compressor electric power (kW)

- heat_demand:

  numeric vector, heat demand (kW thermal)

- cop:

  numeric vector, coefficient of performance

- init:

  numeric, level before the first slot (kWh thermal)

- k:

  numeric, per-slot retention `(1 - loss/100)^dt`

- dt:

  numeric, slot length in hours

- backup:

  numeric vector, backup heater power (kW, electric = thermal)

## Value

numeric vector of length `length(power) + 1`
