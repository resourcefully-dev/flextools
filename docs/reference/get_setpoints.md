# Set setpoints for smart charging

Set setpoints for smart charging

## Usage

``` r
get_setpoints(
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

- sessions_window:

  tibble, sessions corresponding to a single windows

- opt_data:

  tibble, optimization data

- profiles_demand:

  tibble, user profiles power demand

- opt_objective:

  character, optimization objective

- lambda:

  numeric, penalty on change for the flexible load.

- energy_min, energy_max:

  numeric, minimum and maximum share (between 0 and 1) of each profile's
  energy the setpoint must carry. See
  [`smart_charging()`](https://resourcefully-dev.github.io/flextools/reference/smart_charging.md).
