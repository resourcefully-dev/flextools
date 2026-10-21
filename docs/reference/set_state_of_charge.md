# Set and validate the arrival State-of-Charge of the V2G sessions

Adds `StateOfCharge = 0.5` when the column is absent, and checks that
every value is a fraction with `0 <= StateOfCharge < 1` (a full battery
has no energy to charge, so it gives no battery size).

## Usage

``` r
set_state_of_charge(sessions)
```

## Arguments

- sessions:

  tibble of sessions

## Value

`sessions` with a `StateOfCharge` column
