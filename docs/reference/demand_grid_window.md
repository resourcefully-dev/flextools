# Minimization of net power (just a window)

Minimization of net power (just a window)

## Usage

``` r
demand_grid_window(
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

- G:

  numeric vector, being the renewable generation profile

- LF:

  numeric vector, being the flexible load profile

- LS:

  numeric vector, being the static load profile

- direction:

  character, being `forward` or `backward`. The direction where energy
  can be shifted

- time_horizon:

  integer, maximum number of positions to shift energy from

- LFmax:

  numeric, value of maximum power (in kW) of the flexible load `LF`

- import_capacity:

  numeric or numeric vector, grid maximum import capacity that will
  limit the maximum optimized demand

- export_capacity:

  numeric or numeric vector, grid maximum export capacity that will
  limit the maximum optimized demand

- lambda:

  numeric, penalty on change for the flexible load.

- energy_ratio:

  numeric vector `c(min, max)`, share of the flexible load's energy the
  optimized load must carry. `c(1, 1)` (default) preserves the energy
  exactly; a range lets the optimizer drop energy — down to `min` — when
  the grid capacity cannot fit `max`.

- cumsum_min:

  numeric vector or `NULL`, minimum cumulative optimized load at each
  slot (in the units of `LF`): the energy owed to the sessions that have
  departed by then (see
  [`get_flex_envelope()`](https://resourcefully-dev.github.io/flextools/reference/get_flex_envelope.md)).
  Only applies to `direction = "forward"`, and is capped at `cumsum(LF)`
  so the original profile stays feasible.

## Value

numeric vector
