# Solve one tank window

Solve one tank window

## Usage

``` r
tank_solve_window(
  G,
  L,
  Q,
  cop,
  PI,
  PE,
  import_capacity,
  export_capacity,
  S0,
  Smin,
  Smax,
  Pc,
  k,
  dt,
  objective,
  lambda = 0,
  Pb = 0,
  soft = FALSE,
  slack_import_max = 0,
  slack_export_max = 0
)
```

## Arguments

- G:

  numeric vector, production (kW)

- L:

  numeric vector, static load (kW)

- Q:

  numeric vector, heat demand (kW thermal)

- cop:

  numeric vector, coefficient of performance

- PI, PE:

  numeric vectors, import and export prices (EUR/kWh); unused for
  `"grid"`

- import_capacity, export_capacity:

  numeric vectors, net grid flow capacities (kW)

- S0:

  numeric, level before the first slot, inside the band (kWh)

- Smin, Smax:

  numeric, band (kWh)

- Pc:

  numeric, compressor cap (kW)

- k:

  numeric, per-slot retention

- dt:

  numeric, slot length (hours)

- objective:

  `"grid"`, `"cost"` or a number strictly between 0 and 1

- lambda:

  numeric, ramping penalty weight

- Pb:

  numeric, backup heater cap (kW); 0 means no backup heater

- soft:

  logical, whether the comfort floor is soft: the level may go down to
  the inlet temperature, below the floor at a price

- slack_import_max, slack_export_max:

  numeric vectors, how far each slot may miss its import / export
  capacity (kW), from
  [`optimization_slack_ceiling()`](https://resourcefully-dev.github.io/flextools/reference/optimization_slack_ceiling.md);
  the capacities are hard where they are 0

## Value

list of the unrounded `power` and `backup` (kW) and the `status`; when
the window has no solution, `power` is `NULL` and `status` says why (see
[`tank_failure()`](https://resourcefully-dev.github.io/flextools/reference/tank_failure.md))
