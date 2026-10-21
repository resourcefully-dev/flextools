# Set setpoints for smart V2G

Set setpoints for smart V2G

## Usage

``` r
get_setpoints_v2g(
  sessions_window,
  opt_data,
  profiles_demand,
  opt_objective,
  lambda,
  energy_min = 1,
  energy_max = 1
)
```

## Arguments

- energy_min, energy_max:

  numeric, minimum and maximum share (between 0 and 1) of each profile's
  energy the setpoint must carry, net of discharge. See
  [`smart_v2g()`](https://resourcefully-dev.github.io/flextools/reference/smart_v2g.md).
