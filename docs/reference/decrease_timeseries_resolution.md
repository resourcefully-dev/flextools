# Decrease time resolution of timeseries data frame

Decrease time resolution of timeseries data frame

## Usage

``` r
decrease_timeseries_resolution(
  df,
  resolution_mins,
  method = c("average", "first", "sum")
)
```

## Arguments

- df:

  data.frame or tibble, first column of name `datetime` being of class
  datetime and rest of columns being numeric

- resolution_mins:

  integer, interval of minutes between two consecutive datetime values

- method:

  character, being `average`, `first` or `sum` as valid options

## Value

tibble
