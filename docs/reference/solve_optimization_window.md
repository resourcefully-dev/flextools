# Perform demand optimization (just a window)

Perform demand optimization (just a window)

## Usage

``` r
solve_optimization_window(
  G,
  LF,
  LS,
  direction,
  time_horizon,
  LFmax,
  import_capacity,
  export_capacity,
  P,
  q
)
```

## Arguments

- G:

  numeric vector, being the renewable generation power profile

- LF:

  numeric vector, being the flexible load power profile

- LS:

  numeric vector, being the static load power profile

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

- P:

  numeric matrix, optimization objective parameter

- q:

  numeric vector, optimization objective parameter

## Value

numeric vector
