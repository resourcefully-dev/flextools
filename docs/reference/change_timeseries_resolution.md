# Change time resolution of a time-series data frame

Change time resolution of a time-series data frame

## Usage

``` r
change_timeseries_resolution(df, resolution_out, method)
```

## Arguments

- df:

  tibble, first column of name `datetime` being of class datetime and
  rest of columns being numeric

- resolution_out:

  integer, desired interval of minutes between two consecutive datetime
  values

- method:

  character, being `interpolate`, `repeat` or `divide` if the resolution
  has to be increased, or `average`, `first` or `sum` if the resolution
  has to be decreased. See Examples for more information.

## Value

tibble

## Examples

``` r
# Example time-series data set: time resolution 15 minutes
head(energy_profiles)
#> # A tibble: 6 × 7
#>   datetime            solar building price_imported price_exported price_turn_up
#>   <dttm>              <dbl>    <dbl>          <dbl>          <dbl>         <dbl>
#> 1 2023-01-01 00:00:00     0     2.61       -0.00146          0.025        0     
#> 2 2023-01-01 00:15:00     0     2.42       -0.00146          0.025        0     
#> 3 2023-01-01 00:30:00     0     2.23       -0.00146          0.025        0     
#> 4 2023-01-01 00:45:00     0     2.04       -0.00146          0.025        0     
#> 5 2023-01-01 01:00:00     0     1.85       -0.00152          0.025        0.0328
#> 6 2023-01-01 01:15:00     0     1.78       -0.00152          0.025        0.0351
#> # ℹ 1 more variable: price_turn_down <dbl>

# Change time resolution to 60 minutes
# It is decreasing time resolution, so we use the `average` method
head(change_timeseries_resolution(
  df = energy_profiles,
  resolution_out = 60,
  method = "average"
))
#> # A tibble: 6 × 7
#>   datetime            solar building price_imported price_exported price_turn_up
#>   <dttm>              <dbl>    <dbl>          <dbl>          <dbl>         <dbl>
#> 1 2023-01-01 00:00:00     0     2.33       -0.00146          0.025        0     
#> 2 2023-01-01 01:00:00     0     1.74       -0.00152          0.025        0.0320
#> 3 2023-01-01 02:00:00     0     1.55       -0.005            0.025        0.0225
#> 4 2023-01-01 03:00:00     0     1.48       -0.0046           0.025        0     
#> 5 2023-01-01 04:00:00     0     1.39       -0.00405          0.025        0     
#> 6 2023-01-01 05:00:00     0     1.43       -0.0036           0.025        0     
#> # ℹ 1 more variable: price_turn_down <dbl>


```
