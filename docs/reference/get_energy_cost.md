# Get energy cost time-series

The energy cost (in Euros) based on imported and exported energy and the
corresponding prices.

## Usage

``` r
get_energy_cost(df)
```

## Arguments

- df:

  tibble, with columns `datetime`, `consumption`, `production`,
  `price_imported` and `price_exported`

## Value

tibble

## Examples

``` r
df <- dplyr::select(
  energy_profiles,
  datetime,
  production = solar,
  consumption = building,
  price_imported,
  price_exported
)
df <- dplyr::slice_head(df, n = 300)
head(df)
#> # A tibble: 6 × 5
#>   datetime            production consumption price_imported price_exported
#>   <dttm>                   <dbl>       <dbl>          <dbl>          <dbl>
#> 1 2023-01-01 00:00:00          0        2.61       -0.00146          0.025
#> 2 2023-01-01 00:15:00          0        2.42       -0.00146          0.025
#> 3 2023-01-01 00:30:00          0        2.23       -0.00146          0.025
#> 4 2023-01-01 00:45:00          0        2.04       -0.00146          0.025
#> 5 2023-01-01 01:00:00          0        1.85       -0.00152          0.025
#> 6 2023-01-01 01:15:00          0        1.78       -0.00152          0.025

head(get_energy_cost(df))
#> # A tibble: 6 × 10
#>   datetime            production consumption price_imported price_exported   net
#>   <dttm>                   <dbl>       <dbl>          <dbl>          <dbl> <dbl>
#> 1 2023-01-01 00:00:00          0        2.61       -0.00146          0.025  2.61
#> 2 2023-01-01 00:15:00          0        2.42       -0.00146          0.025  2.42
#> 3 2023-01-01 00:30:00          0        2.23       -0.00146          0.025  2.23
#> 4 2023-01-01 00:45:00          0        2.04       -0.00146          0.025  2.04
#> 5 2023-01-01 01:00:00          0        1.85       -0.00152          0.025  1.85
#> 6 2023-01-01 01:15:00          0        1.78       -0.00152          0.025  1.78
#> # ℹ 4 more variables: local <dbl>, imported <dbl>, exported <dbl>, cost <dbl>
```
