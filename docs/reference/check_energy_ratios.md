# Validate the pair of energy ratios

Shared by
[`smart_charging()`](https://resourcefully-dev.github.io/flextools/reference/smart_charging.md)
and
[`schedule_sessions()`](https://resourcefully-dev.github.io/flextools/reference/schedule_sessions.md).
Both ratios are shares of each session's energy requirement:
`energy_min` is what must be delivered even at the expense of the grid
capacity, `energy_max` is what may be delivered at most.

## Usage

``` r
check_energy_ratios(energy_min, energy_max)
```

## Arguments

- energy_min:

  numeric, between 0 and 1.

- energy_max:

  numeric, higher than 0 and at most 1; at least `energy_min`.

## Value

`TRUE`, invisibly. Stops with a clear message otherwise.
