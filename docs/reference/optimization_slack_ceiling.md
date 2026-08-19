# Ceiling for grid-capacity slack

How far a grid capacity may be missed when it cannot be met: only as far
as the pre-optimization net flow already needed. The result is zero
wherever the original profile was within its capacity, which keeps those
slots hard capped, so a soft-constrained solution can never create a
grid violation worse than the profile it started from.

## Usage

``` r
optimization_slack_ceiling(capacity, flow)
```

## Arguments

- capacity:

  numeric vector, grid capacity (kW). May contain `Inf`.

- flow:

  numeric vector, pre-optimization net flow in the same direction as
  `capacity` (kW).

## Value

numeric vector, non-negative and finite.
