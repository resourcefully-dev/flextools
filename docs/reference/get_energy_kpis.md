# Obtain a list of energy indicators given the energy profiles

Obtain a list of energy indicators given the energy profiles

## Usage

``` r
get_energy_kpis(df, kg_co2_kwh = 0.5)
```

## Arguments

- df:

  data.frame or tibble, with columns `datetime`, `consumption`,
  `production` and `kg_co2_kwh`

- kg_co2_kwh:

  factor of CO2 kg emissions per kWh of energy consumed from the
  distribution grid

## Value

named list

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

get_energy_kpis(df)
#> $total_consumption
#> [1] 23114
#> 
#> $total_production
#> [1] 7000
#> 
#> $total_local
#> [1] 5296.591
#> 
#> $total_exported
#> [1] 1703.409
#> 
#> $total_imported
#> [1] 17817.41
#> 
#> $selfconsumption
#> [1] 0.7566558
#> 
#> $selfsufficiency
#> [1] 0.2291508
#> 
#> $peak_to_grid
#> [1] 4.487642
#> 
#> $peak_from_grid
#> [1] 6.863723
#> 
#> $peak_to_grid_dttm
#> [1] "2023-06-06 11:00:00 CEST"
#> 
#> $peak_from_grid_dttm
#> [1] "2023-01-02 17:00:00 CET"
#> 
#> $kg_co2
#> [1] 8908.705
#> 
```
