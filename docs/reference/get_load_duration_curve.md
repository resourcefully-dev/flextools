# Load duration curve table

The Load Duration Curve (LDC) represents the percentage of time that a
specific value of power has been used in the electrical grid for a
specific time period. It's widely used for power system planning and
grid reliability assessments.

## Usage

``` r
get_load_duration_curve(df)
```

## Arguments

- df:

  tibble, with columns `datetime`, `consumption`, `production`

## Value

tibble

## Examples

``` r
df <- dplyr::select(
  energy_profiles,
  datetime,
  consumption = building
)

head(df)
#> # A tibble: 6 × 2
#>   datetime            consumption
#>   <dttm>                    <dbl>
#> 1 2023-01-01 00:00:00        2.61
#> 2 2023-01-01 00:15:00        2.42
#> 3 2023-01-01 00:30:00        2.23
#> 4 2023-01-01 00:45:00        2.04
#> 5 2023-01-01 01:00:00        1.85
#> 6 2023-01-01 01:15:00        1.78

get_load_duration_curve(df)
#> # A tibble: 7 × 2
#>      pct power
#>    <dbl> <dbl>
#> 1   0.01 6.78 
#> 2   0.37 5.78 
#> 3   4.77 4.78 
#> 4  14.2  3.78 
#> 5  41.1  2.78 
#> 6  73.8  1.78 
#> 7 100    0.775
```
