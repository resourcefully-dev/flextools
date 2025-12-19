# Minimization of net power (just a window)

Minimization of net power (just a window)

## Usage

``` r
minimize_net_power_window(
  G,
  LF,
  LS,
  direction,
  time_horizon,
  LFmax,
  import_capacity,
  export_capacity,
  lambda = 0
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

## Value

numeric vector
