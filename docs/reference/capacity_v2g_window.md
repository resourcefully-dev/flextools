# Capacity optimisation with bidirectional flexibility (single window)

Mirrors `demand_capacity_window()` but falls back to
[`demand_grid_v2g_window()`](https://resourcefully-dev.github.io/flextools/reference/demand_grid_v2g_window.md)
so that discharge is considered when resolving capacity violations.

## Usage

``` r
capacity_v2g_window(
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
  cumsum_min = NULL
)
```

## Arguments

- energy_ratio:

  numeric vector `c(min, max)`, share of the flexible load's energy the
  optimized load must carry. See `demand_capacity_window()`.
