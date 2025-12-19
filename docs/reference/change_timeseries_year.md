# Change the year of a time series data frame keeping the original weekdays

The input `df` must contain full-week time-series profiles in order to
arrange the data according to the day of the week. For example, if the
first day in the `df` is a Monday the last one must be a Sunday.

## Usage

``` r
change_timeseries_year(df, year_out)
```

## Arguments

- df:

  tibble with first column being `datetime`

- year_out:

  integer, year of the desired `datetime`

## Value

tibble

## Examples

``` r
# Example time-series data set: year 2023
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

# Change year to 2025
# Note that the data from columns has changed according to the weekday
head(change_timeseries_year(
  df = energy_profiles,
  year_out = 2025
))
#> # A tibble: 6 × 7
#>   datetime            solar building price_imported price_exported price_turn_up
#>   <dttm>              <dbl>    <dbl>          <dbl>          <dbl>         <dbl>
#> 1 2025-01-01 00:00:00     0     1.60         0.0684          0.025             0
#> 2 2025-01-01 00:15:00     0     1.56         0.0684          0.025             0
#> 3 2025-01-01 00:30:00     0     1.51         0.0684          0.025             0
#> 4 2025-01-01 00:45:00     0     1.47         0.0684          0.025             0
#> 5 2025-01-01 01:00:00     0     1.43         0.0379          0.025             0
#> 6 2025-01-01 01:15:00     0     1.41         0.0379          0.025             0
#> # ℹ 1 more variable: price_turn_down <dbl>

```
