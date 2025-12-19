# Get imbalance total income (one number)

The total energy income (in Euros) based on the difference between the
baseline and final power profile for a certain demand. The difference
represents the optimal changes due to flexibility, so this function
calculates the income from applying this demand-side flexibility.

## Usage

``` r
get_imbalance_total_income(df)
```

## Arguments

- df:

  tibble, with columns `datetime`, `demand_baseline`, `demand_final`,
  `price_turn_up` and `price_turn_down`

## Value

tibble

## Examples

``` r
df <- dplyr::select(
  energy_profiles,
  datetime,
  demand_baseline = building,
  price_turn_up,
  price_turn_down
)

# Build another random consumption profile
building_variation <- rnorm(nrow(df), mean = 0, sd = 1)
df <- dplyr::mutate(
  df, demand_final = demand_baseline + building_variation
)
head(df)
#> # A tibble: 6 × 5
#>   datetime            demand_baseline price_turn_up price_turn_down demand_final
#>   <dttm>                        <dbl>         <dbl>           <dbl>        <dbl>
#> 1 2023-01-01 00:00:00            2.61        0               0.209          3.55
#> 2 2023-01-01 00:15:00            2.42        0               0.0233         2.30
#> 3 2023-01-01 00:30:00            2.23        0               0.0256         3.44
#> 4 2023-01-01 00:45:00            2.04        0               0.0406         3.47
#> 5 2023-01-01 01:00:00            1.85        0.0328          0.0406         2.70
#> 6 2023-01-01 01:15:00            1.78        0.0351          0              2.29

get_imbalance_total_income(df)
#> [1] 1396.151
```
