# Reward per unit of optimized load kept

When the energy row is a range, the optimizer must be told to keep
energy rather than drop it: every objective in this file improves when
load is removed (less net flow, less cost). A uniform linear reward on
`O` that strictly dominates the objective's marginal gain from removing
one unit — `|d/dO| <= max row sum of |P| * max |X| + max |q|` anywhere
in the feasible box — makes the solver keep as much energy as the
constraints admit and shape it afterwards. Being the same in every slot
it does not bias where the energy goes, only how much of it stays.

## Usage

``` r
demand_energy_reward(P, q, envelope)
```

## Arguments

- P:

  numeric matrix or `NULL`, normalised quadratic term.

- q:

  numeric vector, linear term.

- envelope:

  numeric, bound on the absolute value of any decision variable in the
  feasible box (kW).

## Value

numeric scalar
