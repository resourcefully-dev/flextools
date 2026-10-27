# Solve a V2G window priced on its grid flows

Builds the problem of
[`demand_cost_v2g_window()`](https://resourcefully-dev.github.io/flextools/reference/demand_cost_v2g_window.md)
and
[`demand_combined_v2g_window()`](https://resourcefully-dev.github.io/flextools/reference/demand_combined_v2g_window.md)
on the bounds of `get_bounds_v2g()`. The unknown is `X = [O, I, E]`, or
`[O, I, E, D]` with the net discharge `D`: the optimized load keeps the
box, departure and energy rows of the grid objective, and the site
balance `O - I + E = G - LS` ties it to the import and the export. Their
upper bounds follow from the box of `O` (which already carries the grid
capacities, also negative ones), so the original-profile fallbacks of
`solve_optimization_window_v2g()` stay feasible by construction: `I` and
`E` are the profile's own net flows and `D = max(-O, 0)`. Solved as in
[`demand_solve_window()`](https://resourcefully-dev.github.io/flextools/reference/demand_solve_window.md):
a MILP with a binary grid mode (no import and export in the same slot)
when the objective is linear, otherwise a QP with the export price
clipped to the import price.

## Usage

``` r
solve_grid_flows_v2g(bounds, G, LS, P, q, energy_floor, energy_ceiling)
```

## Arguments

- bounds:

  list, output of `get_bounds_v2g()`.

- G, LS:

  numeric vectors, production and static load.

- P:

  numeric matrix or `NULL`, normalised quadratic term.

- q:

  numeric vector, linear term.

- energy_floor, energy_ceiling:

  numeric, energy range of the window.

## Value

list with `result` (its `status_message`) and `x`
