# Validate the minimum charging power

Shared by
[`smart_charging()`](https://resourcefully-dev.github.io/flextools/reference/smart_charging.md),
[`smart_v2g()`](https://resourcefully-dev.github.io/flextools/reference/smart_v2g.md)
and the schedulers. `charging_power_min` is a power in kW (up to 1.12.0
a value below 1 was a share of the nominal power).

## Usage

``` r
check_charging_power_min(charging_power_min)
```

## Arguments

- charging_power_min:

  numeric, a single finite value of at least 0 (kW).

## Value

`TRUE`, invisibly. Stops with a clear message otherwise.
