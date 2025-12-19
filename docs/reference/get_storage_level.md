# Accumulated storage level (energy)

Each value represents the energy level at the beginning of the time
slot, starting from the provided initial State-of-Charge. When the
`power` vector carries `charge` and `discharge` attributes (as provided
by
[`add_battery_optimization()`](https://resourcefully-dev.github.io/flextools/reference/add_battery_optimization.md)),
those are used internally to compute the storage evolution when
efficiencies are below 1.

## Usage

``` r
get_storage_level(
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

  numeric, initial storage level (in kWh, not %)

- charge_eff:

  numeric, charging efficiency (from 0 to 1, default to 1)

- discharge_eff:

  numeric, discharging efficiency (from 0 to 1, default to 1)

- time_resolution:

  numeric, time resolution of the time-series (in minutes)

## Value

numeric vector of energy stored
