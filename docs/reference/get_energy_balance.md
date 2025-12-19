# Get energy balance time-series

Input data frame must have columns `consumption`, `production`. Output
data frame has extra columns `net`, `local`, `imported`, `exported`.
Column `net` is positive when there is more consumption than production

## Usage

``` r
get_energy_balance(df)
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
  production = solar,
  consumption = building
)
head(df)
#> # A tibble: 6 × 3
#>   datetime            production consumption
#>   <dttm>                   <dbl>       <dbl>
#> 1 2023-01-01 00:00:00          0        2.61
#> 2 2023-01-01 00:15:00          0        2.42
#> 3 2023-01-01 00:30:00          0        2.23
#> 4 2023-01-01 00:45:00          0        2.04
#> 5 2023-01-01 01:00:00          0        1.85
#> 6 2023-01-01 01:15:00          0        1.78

head(get_energy_balance(df))
#> # A tibble: 6 × 7
#>   datetime            production consumption   net local imported exported
#>   <dttm>                   <dbl>       <dbl> <dbl> <dbl>    <dbl>    <dbl>
#> 1 2023-01-01 00:00:00          0        2.61  2.61     0     2.61        0
#> 2 2023-01-01 00:15:00          0        2.42  2.42     0     2.42        0
#> 3 2023-01-01 00:30:00          0        2.23  2.23     0     2.23        0
#> 4 2023-01-01 00:45:00          0        2.04  2.04     0     2.04        0
#> 5 2023-01-01 01:00:00          0        1.85  1.85     0     1.85        0
#> 6 2023-01-01 01:15:00          0        1.78  1.78     0     1.78        0

```
