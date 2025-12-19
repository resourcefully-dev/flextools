# Set setpoints for smart charging

Set setpoints for smart charging

## Usage

``` r
get_setpoints(
  sessions_window,
  opt_data,
  profiles_demand,
  opt_objective,
  lambda
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
