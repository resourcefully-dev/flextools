# Return the time resolution of a datetime sequence

Return the time resolution of a datetime sequence

## Usage

``` r
get_time_resolution(dttm_seq, units = "mins")
```

## Arguments

- dttm_seq:

  datetime sequence

- units:

  character being one of "auto", "secs", "mins", "hours", "days" and
  "weeks"

## Value

numeric

## Examples

``` r
resolution <- get_time_resolution(energy_profiles$datetime)
print(resolution)
#> [1] 15
```
