# Accumulated storage level

Accumulated storage level

## Usage

``` r
get_soc_level(
  power,
  init = 0,
  charge_eff = 1,
  discharge_eff = 1,
  time_resolution = 60
)
```

## Arguments

- power:

  numeric vector, being positive when charging and negative when
  discharging

- init:

  numeric, initial storage level (in energy units, not %)

- charge_eff:

  numeric, charging efficiency (from 0 to 1, default to 1)

- discharge_eff:

  numeric, discharging efficiency (from 0 to 1, default to 1)

- time_resolution:

  numeric, time resolution of the time-series (in minutes)

## Value

numeric vector
