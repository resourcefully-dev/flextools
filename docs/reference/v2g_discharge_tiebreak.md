# Cost of a unit of V2G discharge that breaks ties

A linear cost objective is indifferent between many profiles: with a
flat export price, discharging in one exporting slot to charge in
another earns exactly nothing.
[`demand_cost_v2g_window()`](https://resourcefully-dev.github.io/flextools/reference/demand_cost_v2g_window.md)
charges the fleet's net discharge this tiny price, 1e-4 of the largest
absolute price (1e-4 when every price is 0), so such round trips are
never chosen, while a discharge that earns more than that still is.

## Usage

``` r
v2g_discharge_tiebreak(prices)
```

## Arguments

- prices:

  numeric vector, the prices of the window.

## Value

numeric scalar
