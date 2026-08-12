# Accumulated storage level (energy)

Each value represents the energy level at the beginning of the time
slot, starting from the provided initial State-of-Charge.

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

## Details

`power` is interpreted as the **grid-side** battery power: positive when
the grid is charging the battery, negative when the battery is supplying
the grid. Pass `charge_eff` / `discharge_eff` to convert grid-side power
to actual stored energy (grid draws `power / charge_eff` when charging;
storage releases `|power| / discharge_eff` when discharging).

If `power` is already **storage-side** (e.g. from
[`add_battery_optimization()`](https://resourcefully-dev.github.io/flextools/reference/add_battery_optimization.md)),
call with the default efficiencies of 1.
