# Minimisation of net power with bidirectional flexibility (single window)

Minimisation of net power with bidirectional flexibility (single window)

## Usage

``` r
demand_grid_v2g_window(
  G,
  LF,
  LS,
  direction,
  time_horizon,
  LFmax,
  import_capacity,
  export_capacity,
  lambda = 0,
  energy_ratio = c(1, 1),
  cumsum_min = NULL,
  LFmin = NULL
)
```

## Arguments

- energy_ratio:

  numeric vector `c(min, max)`, share of the flexible load's energy the
  optimized load must carry (net of discharge). `c(1, 1)` (default)
  preserves the energy exactly; a range lets the optimizer drop energy -
  down to `min` - when the grid capacity cannot fit `max`.

- LFmin:

  numeric vector (or `NULL`, default), per-slot floor of the optimized
  load on top of `-LFmax`.
  [`get_setpoints_v2g()`](https://resourcefully-dev.github.io/flextools/reference/get_setpoints_v2g.md)
  (for the grid and combined objectives) and
  [`capacity_v2g_window()`](https://resourcefully-dev.github.io/flextools/reference/capacity_v2g_window.md)
  pass
  [`v2g_discharge_floor()`](https://resourcefully-dev.github.io/flextools/reference/v2g_discharge_floor.md),
  so the fleet never discharges where that only adds export.
