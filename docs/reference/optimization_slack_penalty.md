# Linear penalty weight for grid-capacity slack

The weight must strictly dominate the marginal gain of a quadratic
net-power objective, otherwise the optimizer prefers a flatter profile
over respecting the grid capacity. Since `|d/dB sum(net^2)| = 2*|net|`
and the achievable net flow is bounded by the relaxed envelope,
`2 * max(envelope)` is a tight and valid bound.

## Usage

``` r
optimization_slack_penalty(envelope)
```

## Arguments

- envelope:

  numeric vector, per-slot bound on the absolute net flow (kW)

## Value

numeric scalar

## Details

Deriving the weight from the envelope rather than from the battery power
rating matters for performance, not just tidiness: an oversized weight
leaves the optimum untouched but multiplies the ADMM iteration count. On
a one-year benchmark the power-derived weight needed 8600 iterations
against 2625 for the envelope-derived one, for an identical solution.
