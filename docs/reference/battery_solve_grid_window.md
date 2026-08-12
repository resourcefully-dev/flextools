# Perform battery optimization for a single window (grid/capacity objective)

Perform battery optimization for a single window (grid/capacity
objective)

## Usage

``` r
battery_solve_grid_window(
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
  P,
  q
)
```

## Arguments

- G:

  numeric vector, renewable generation profile

- L:

  numeric vector, static load profile

- Bcap:

  numeric, battery capacity in energy units (kWh \* slots/h)

- Bc:

  numeric, maximum charging power (kW)

- Bd:

  numeric, maximum discharging power (kW)

- SOCmin:

  numeric, minimum State-of-Charge (%)

- SOCmax:

  numeric, maximum State-of-Charge (%)

- SOCini:

  numeric, initial State-of-Charge (%)

- import_capacity:

  numeric vector, maximum grid import (kW)

- export_capacity:

  numeric vector, maximum grid export (kW)

- P:

  numeric matrix, quadratic objective term

- q:

  numeric vector, linear objective term

## Value

numeric vector
