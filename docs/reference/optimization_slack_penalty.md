# Linear penalty weight for grid-capacity slack

The weight must strictly dominate the marginal gain of the quadratic
objective it is added to, otherwise the optimizer prefers a better
objective value over respecting the grid capacity. The caller therefore
passes an envelope bounding whatever that objective differentiates to:
the achievable net flow for `sum(net^2)`, where
`|d/dB sum(net^2)| = 2*|net|`, or the battery power rating for
`sum(B^2)`. `2 * max(envelope)` is then a tight and valid bound.

## Usage

``` r
optimization_slack_penalty(envelope)
```

## Arguments

- envelope:

  numeric, one or more bounds on the absolute value of the quantity the
  objective is quadratic in (kW). Only the largest is used, so callers
  may pass either a per-slot vector or a single global bound.

## Value

numeric scalar

## Details

Keeping the envelope no larger than the objective needs matters for
performance, not just tidiness: an oversized weight leaves the optimum
untouched but multiplies the ADMM iteration count. On a one-year
benchmark a weight derived from a battery power rating far above the
site's own flows needed 8600 iterations against 2625 for the
flow-derived one, for an identical solution.
