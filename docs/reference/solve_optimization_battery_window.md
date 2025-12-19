# Perform battery optimization (just a window)

Perform battery optimization (just a window)

## Usage

``` r
solve_optimization_battery_window(
  P,
  q,
  G,
  L,
  Bcap,
  Bc,
  Bd,
  SOCmin,
  SOCmax,
  SOCini,
  import_capacity,
  export_capacity,
  charge_eff,
  discharge_eff
)
```

## Arguments

- P:

  numeric matrix, optimization objective parameter

- q:

  numeric vector, optimization objective parameter

- G:

  numeric vector, being the renewable generation profile

- L:

  numeric vector, being the load profile

- Bcap:

  numeric, capacity of the battery (NOT in kWh but in energy units
  according to time resolution)

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

- import_capacity:

  numeric vector, grid maximum import power capacity that will limit the
  maximum charging power

- export_capacity:

  numeric vector, grid maximum export power capacity that will limit the
  maximum discharging power

- charge_eff:

  numeric, battery charging efficiency (from 0 to 1)

- discharge_eff:

  numeric, battery discharging efficiency (from 0 to 1)

## Value

numeric vector
