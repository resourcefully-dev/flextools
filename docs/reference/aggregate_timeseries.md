# Aggregate multiple timeseries columns to a single one

The first column `datetime` will be kept.

## Usage

``` r
aggregate_timeseries(df, varname, omit = NULL)
```

## Arguments

- df:

  data.frame or tibble, first column of name `datetime` being of class
  datetime and rest of columns being numeric

- varname:

  character, name of the aggregation column

- omit:

  character, name of columns to not aggregate

## Value

tibble

## Examples

``` r
# Example data set with 2 identical building profiles
df <- dplyr::select(
  energy_profiles, datetime, building1 = building, building2 = building
)
head(df)
#> # A tibble: 6 × 3
#>   datetime            building1 building2
#>   <dttm>                  <dbl>     <dbl>
#> 1 2023-01-01 00:00:00      2.61      2.61
#> 2 2023-01-01 00:15:00      2.42      2.42
#> 3 2023-01-01 00:30:00      2.23      2.23
#> 4 2023-01-01 00:45:00      2.04      2.04
#> 5 2023-01-01 01:00:00      1.85      1.85
#> 6 2023-01-01 01:15:00      1.78      1.78

# Aggregate the total building demand
head(aggregate_timeseries(df, varname = "total_buildings"))
#> # A tibble: 6 × 2
#>   datetime            total_buildings
#>   <dttm>                        <dbl>
#> 1 2023-01-01 00:00:00            5.23
#> 2 2023-01-01 00:15:00            4.85
#> 3 2023-01-01 00:30:00            4.47
#> 4 2023-01-01 00:45:00            4.09
#> 5 2023-01-01 01:00:00            3.71
#> 6 2023-01-01 01:15:00            3.56
```
