# Set `Responsive` column in `sessions`

Set `Responsive` column in `sessions`

## Usage

``` r
set_responsive(
  sessions_window,
  dttm_seq,
  responsive,
  opt_objective,
  time_resolution
)
```

## Arguments

- sessions_window:

  tibble, sessions corresponding to a single windows

- dttm_seq:

  datetime vector

- responsive:

  named list with responsive ratios

- opt_objective:

  character, optimization objective being `"none"`, `"grid"`, `"cost"`
  or a value between 0 (cost) and 1 (grid).

- time_resolution:

  numeric, time resolution in minutes
