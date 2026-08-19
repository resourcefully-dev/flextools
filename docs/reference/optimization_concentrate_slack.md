# Concentrate an unavoidable grid-capacity miss

When a capacity cannot be met, the slack of
[`optimization_slack_ceiling()`](https://resourcefully-dev.github.io/flextools/reference/optimization_slack_ceiling.md)
is penalised linearly in the *volume* missed. Every way of spending the
battery's limited energy across the affected slots therefore costs the
objective exactly the same, and the quadratic net-power term breaks the
tie by flattening: the miss is spread thinly so that every slot ends
marginally over its capacity. That is the worst distribution for a
capacity contract, whose cost is counted in slots (or hours) in
violation — `congestion_time` in
[`get_energy_kpis()`](https://resourcefully-dev.github.io/flextools/reference/get_energy_kpis.md)
— not in kWh: it keeps 100% of the window in violation whatever the
battery size, so the metric only moves once the battery is large enough
to clear the window entirely.

## Usage

``` r
optimization_concentrate_slack(
  B,
  net0,
  import_capacity,
  export_capacity,
  lb_B,
  ub_B,
  lb_cumsum,
  ub_cumsum,
  tol = optimization_solution_tolerance()
)
```

## Arguments

- B:

  numeric vector, solved battery power (kW), positive when charging.

- net0:

  numeric vector, pre-battery net flow `L - G` (kW).

- import_capacity, export_capacity:

  numeric vectors of capacities (kW), constraining the net flow. May
  contain `Inf`.

- lb_B, ub_B:

  numeric vectors, the box the solution must stay inside.

- lb_cumsum, ub_cumsum:

  numeric vectors, the storage band on `cumsum(B)`.

- tol:

  numeric, tolerance for both the violation test and the feasibility
  check.

## Value

numeric vector the same length as `B`: the concentrated profile, or `B`
unchanged when nothing could be improved feasibly.

## Details

This redistributes the *same* energy inside each run of violating slots
so the capacity is met exactly for as many slots as it covers, in
chronological order, and missed on the remainder.

The redistribution is energy-preserving per run, which is what keeps it
feasible: every `cumsum` outside a run is untouched, and inside a run
both the flat and the front-loaded profile move in one direction only
and end at the same value, so the extremum of the storage path is
unchanged.

Three things make it safe to call unconditionally. A run is skipped when
its battery power does not consistently point the way the violation
needs, when rearranging the energy cannot bring a single slot inside its
capacity (a run the battery misses on *power*, where this would only
spike the profile), and the whole result is dropped in favour of `B` if
it turns out worse than `B` on any bound.
