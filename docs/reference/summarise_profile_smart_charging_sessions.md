# Get a summary of the new schedule of charging sessions

A table is provided containing the number of `Considered`, `Responsive`,
`Flexible` and `Exploited` sessions.

## Usage

``` r
summarise_profile_smart_charging_sessions(profile_sessions)
```

## Arguments

- profile_sessions:

  tibble, charging `sessions` object from
  [`smart_charging()`](https://resourcefully-dev.github.io/flextools/reference/smart_charging.md)

## Value

tibble with columns `group` (name of sessions' group), `subgroup` (nome
of sessions' subgroup), `n_sessions` (number of sessions) and `pct`
(percentage of subgroup sessions from the group)
