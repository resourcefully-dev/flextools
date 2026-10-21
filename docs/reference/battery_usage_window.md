# Minimize battery usage (just a window)

The capacity is enforced by
[`battery_solve_grid_window()`](https://resourcefully-dev.github.io/flextools/reference/battery_solve_grid_window.md)'s
constraints, so the objective is free to ask for the *least battery*
that satisfies them: `min sum(B^2)`, with no linear term pulling the net
flow anywhere. The quadratic penalises power, so the discharge settles
exactly on the binding capacity rather than below it, and the energy
neutrality it has to give back is recharged at the lowest power the
window allows.

## Usage

``` r
battery_usage_window(
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
  lambda = 0
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

- lambda:

  numeric, ramping penalty weight.

## Value

numeric vector

## Details

This is the opposite reading of the same constraints from
`battery_grid_window()`, whose `q = 2 * (L - G)` makes the objective
`sum((L - G + B)^2)` and therefore flattens the whole profile.
