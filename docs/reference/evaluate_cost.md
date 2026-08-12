# Evaluate total grid cost for an optimized demand profile

Thin benchmarking helper that takes the optimization context `opt_data`
(the same tibble passed to
[`optimize_demand()`](https://resourcefully-dev.github.io/flextools/reference/optimize_demand.md))
and an optimized flexible-demand vector `O`, and returns the total grid
energy cost. Makes cost benchmarking a single call:

## Usage

``` r
evaluate_cost(opt_data, O)
```

## Arguments

- opt_data:

  tibble, same shape expected by
  [`optimize_demand()`](https://resourcefully-dev.github.io/flextools/reference/optimize_demand.md).
  Must contain `datetime`. Optional columns `static`, `production`,
  `price_imported` and `price_exported` default to `0`, `0`, `1` and `0`
  respectively when missing – matching the defaults used by
  [`optimize_demand()`](https://resourcefully-dev.github.io/flextools/reference/optimize_demand.md).

- O:

  numeric vector of flexible demand (kW), same length as
  `opt_data$datetime`. Pass `opt_data$flexible` for the baseline cost,
  or the return value of
  [`optimize_demand()`](https://resourcefully-dev.github.io/flextools/reference/optimize_demand.md)
  for the optimized cost.

## Value

numeric scalar, the total grid cost in the currency units of the price
columns.

## Details

    # Baseline cost (no flexibility)
    evaluate_cost(opt_data, opt_data$flexible)

    # Cost after optimization
    O <- optimize_demand(opt_data, opt_objective = "cost")
    evaluate_cost(opt_data, O)

Internally this builds the equivalent `consumption = O + static` column
and delegates to
[`get_energy_total_cost()`](https://resourcefully-dev.github.io/flextools/reference/get_energy_total_cost.md),
so the pricing convention matches the rest of the package.
