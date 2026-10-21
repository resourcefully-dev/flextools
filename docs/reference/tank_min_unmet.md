# Least heat a window cannot give at the inlet temperature

The heat demand the tank cannot serve at the inlet temperature (level 0)
with the compressor and the backup heater at full power whenever the
tank has room, the compressor first: no schedule leaves less.

## Usage

``` r
tank_min_unmet(Q, cop, S0, k, dt, Smax, ub_P, ub_B, ub_total)
```

## Arguments

- Q:

  numeric vector, heat demand (kW thermal)

- cop:

  numeric vector, coefficient of performance

- S0:

  numeric, level before the first slot, inside the band (kWh)

- k:

  numeric, per-slot retention

- dt:

  numeric, slot length (hours)

- ub_P, ub_B, ub_total:

  numeric vectors, caps of the compressor, the backup heater and their
  sum (kW)

## Value

numeric vector, heat not served per slot (kW thermal)
