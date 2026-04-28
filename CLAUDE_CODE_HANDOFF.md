# Handoff: fix demand cost / combined optimization in `flextools`

## Context

You are working on the `flextools` R package (energy optimization for demand,
battery, and EV smart charging). The package is at the current working
directory.

The previous agent worked from a Cowork sandbox without R installed and
without CRAN access, so it edited the code by reasoning about it and never
actually ran anything. The user is (rightly) tired of untested edits.
**Before claiming a fix, run R, exercise the failing case, and show that the
benchmark below completes without warnings.** Use `devtools::load_all()` so
edits take effect without reinstall.

## What's broken

`optimize_demand()` works for `opt_objective = "cost"` (and combined, where
`opt_objective` is numeric in `(0, 1)`) when `lambda = 0`, but fails when
`lambda > 0`. The user's reproducer:

```r
devtools::load_all(".")
opt_data_cost <- flextools::energy_profiles |>
  dplyr::rename(production = "solar", flexible = "building") |>
  dplyr::mutate(load_capacity = max(flexible, na.rm = TRUE)) |>
  dplyr::filter(lubridate::isoweek(datetime) == 18)

# Works:
O0 <- optimize_demand(opt_data_cost, opt_objective = "cost",
                     direction = "forward", lambda = 0)

# Fails with:
#   ⚠️ Optimization warning: optimization not feasible in some windows.
#   ⚠️ Optimization warning: Solve error. No optimization provided.
O1 <- optimize_demand(opt_data_cost, opt_objective = "cost",
                     direction = "forward",
                     lambda = 0.01 * mean(opt_data_cost$price_imported)^2)
```

There was also a "Row 96 inconsistent bounds [208.28, 208.28]" warning at
`lambda = 0` that may or may not still be present — verify.

## Likely root cause (unverified — confirm before fixing)

`R/demand_optimization.R` has three solve paths for cost / combined windows:

1. `solve_demand_cost_window_epigraph()` (LP/QP fast path; only used when
   `all(PI >= PE)`).
2. `demand_solve_milp_window()` (HiGHS native MIP, used when `lambda == 0`).
3. `demand_solve_miqp_window()` (R-level branch-and-bound that solves
   continuous QP at each node; used when `lambda > 0`).

The third path was rewritten by the previous agent to delegate to
`demand_solve_milp_window()`, which calls `highs::highs_solve(Q = ...,
types = c(continuous, integer))` — i.e. asks HiGHS to solve a MIQP. **HiGHS
does not support MIQP** (LP/MIP/QP only). The previous agent then restored
an R-level B&B but didn't run it. The B&B may or may not actually work
now; verify with the reproducer above.

The battery code in `R/battery_optimization.R` has a working R-level B&B
for the same reason (`battery_solve_miqp_window`, ~line 670). Use it as a
reference — the demand B&B was modeled on it.

## What the user wants

The simplest and fastest formulation that handles `lambda > 0` reliably.
The user is open to switching solvers if HiGHS is genuinely the wrong tool.
Performance target: better than the original OSQP-based path
(`R/demand_optimization_qp.R`), which solved the same problem in ~58 s on
their benchmark.

## Constraints to respect

- The package's mirai-based parallelization is not yours to change. Don't
  set `threads > 1` in solver options — windows are parallelized at the
  caller level.
- Public API: `optimize_demand()` parameters and return value must stay
  stable. The `warm_start` parameter has been removed (user confirmed not
  using it).
- Don't break `opt_objective = "grid"` or the battery / V2G modules.
- The `lambda` parameter must keep the same semantics as in the QP version
  (`R/demand_optimization_qp.R` and `R/battery_optimization.R`):
  `lambda * sum((O_t - O_{t-1})^2)`.

## Suggested plan (you may revise after diagnosis)

1. **Reproduce the failure with full traces.** Add `trace()` calls on
   `solve_demand_cost_window_epigraph`, `demand_solve_miqp_window`,
   `demand_solve_qp_relaxation`. Find out (a) which path is being taken,
   (b) what HiGHS status is actually returned, (c) whether the failure is
   in the QP relaxation itself or in branching.

2. **Decide the fix.** Two routes worth considering:

   - **Route A (simpler, my recommendation):** make the epigraph
     formulation always work, even when `all(PI >= PE)` is false. In the
     rare slots where `PE > PI`, the LP relaxation has an arbitrage
     incentive and would put both `I > 0` and `E > 0`. Decide on a
     well-defined behavior: either warn and clip post-hoc, or add a tiny
     penalty `epsilon * (I + E)` to break the arbitrage. Document the
     compromise. This drops the big-M MIP / MIQP path entirely — cleanest
     and fastest.

   - **Route B (correct, more work):** keep big-M and use a real MIQP
     solver. The R options are limited:
     - `gurobi` (commercial, requires license — check if the user has one).
     - `Rcplex` (commercial).
     - `ompr` + `ompr.roi` + `ROI.plugin.gurobi` / `ROI.plugin.cplex`
       (modeling layer; same commercial backends).
     - SCIP via `scipoptr` (open source, MIQP-capable, painful to install).
     - Roll your own B&B over QP relaxations (current approach; just verify
       it works).

3. **Benchmark.** Compare against `optimize_demand_qp()` (the OSQP path,
   ~58 s on the reproducer). Report total time and per-window time.

4. **Run the test suite.** `devtools::test()`. The failure cases should
   pass; nothing else should regress.

## Don't repeat the previous agent's mistakes

- Don't claim a fix without running R end-to-end on the reproducer above.
- Don't trust assumptions about solver capabilities — call
  `?highs::highs_solve` and `?highs::highs_control` and read what's actually
  supported in the installed version.
- If a fix is "should work in theory," say so and verify, don't promise.
