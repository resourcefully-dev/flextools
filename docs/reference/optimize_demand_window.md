# Combined optimization (just a window)

Combined optimization (just a window)

## Usage

``` r
optimize_demand_window(
  G,
  LF,
  LS,
  PI,
  PE,
  PTD,
  PTU,
  direction,
  time_horizon,
  LFmax,
  import_capacity,
  export_capacity,
  w,
  lambda
)
```

## Arguments

- G:

  numeric vector, being the renewable generation power profile

- LF:

  numeric vector, being the flexible load power profile

- LS:

  numeric vector, being the static load power profile

- PI:

  numeric vector, electricity prices for imported energy

- PE:

  numeric vector, electricity prices for exported energy

- PTD:

  numeric vector, prices for turn-down energy use

- PTU:

  numeric vector, prices for turn-up energy use

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

- w:

  numeric, optimization objective weight (`w=1` minimizes net power
  while `w=0` minimizes cost).

- lambda:

  numeric, penalty on change for the flexible load.

## Value

numeric vector
