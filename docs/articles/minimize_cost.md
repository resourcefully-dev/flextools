# Energy cost optimization

Functions
[`optimize_demand()`](https://resourcefully-dev.github.io/flextools/reference/optimize_demand.md),
[`add_battery_optimization()`](https://resourcefully-dev.github.io/flextools/reference/add_battery_optimization.md)
and
[`smart_charging()`](https://resourcefully-dev.github.io/flextools/reference/smart_charging.md)
solve one independent optimization problem per **optimization window**
(typically one day). Depending on the objective and the variables
involved, the window problem is solved either as a quadratic program
(QP) with the [OSQP](https://osqp.org/) backend, or as a linear or
mixed-integer linear program (LP/MILP) with the
[HiGHS](https://highs.dev/) backend. Currently, the `flextools` package
allows to optimize a time-series power load considering the following
objectives:

- [Minimize the power exchanged with the grid (net
  power)](https://resourcefully-dev.github.io/flextools/articles/minimize_net_power.html)
- Minimize the energy cost
- [A weighted combination of
  both](https://resourcefully-dev.github.io/flextools/articles/combined_optimization.html)

In this article, we’ll cover the optimization problem for the **energy
cost minimization** objective, for both the **flexible demand**
(e.g. heatpumps, electric vehicles, etc.) and the **battery**.

Our energy cost optimization takes into account multiple profiles of
energy prices with the objective to minimize the total cost,
considering:

- Imported energy cost
- Exported energy income
- Income from balancing markets (turn up/down demand), **for the
  flexible demand only**

## Demand optimization

To minimize the energy cost using the **flexibility from a power demand
profile**, the objective function of the optimization problem has been
raised in the following way:

``` math
\min_{O_t,\, I_t,\, E_t} \; \sum_{t=1}^{T} \Big( I_t \cdot PI_t - E_t \cdot PE_t - PTU_t\,(O_t-LF_t) - PTD_t\,(LF_t-O_t) \Big) + \lambda \sum_{t=1}^{T-1}\left(O_{t+1} - O_t\right)^{2}
```

The objective function and constraints of this optimization problem are
represented below, where:

- $`T`$ : Number of time intervals within the optimization window
- $`G_t`$ : Local power generation time-series vector
- $`LS_t`$ : Non-flexible (static) load time-series vector
- $`LF_t`$ : Flexible load time-series vector (if not optimized)
- $`O_t`$ : Optimal flexible load time-series vector (decision variable)
- $`I_t`$ : Imported power (decision variable, non-negative)
- $`E_t`$ : Exported power (decision variable, non-negative)
- $`y_t`$ : Binary grid mode (1 = importing, 0 = exporting)
- $`LFmax_t`$ : Maximum power that the flexible load can consume
  (`load_capacity`)
- $`IC_t`$, $`EC_t`$ : Grid import and export capacity
- $`PI_t`$ : Imported energy price
- $`PE_t`$ : Exported energy price
- $`PTU_t`$ : Balancing price for turn-up power
- $`PTD_t`$ : Balancing price for turn-down power
- $`h`$ : Time horizon, in number of time slots
- $`\lambda`$ : Ramping penalty weight

Since $`LF_t`$ is a known input, the two balancing terms collapse into a
single linear coefficient on the decision variable, and the
implementation minimizes the equivalent expression (both differ only by
a constant):

``` math
\min_{O_t,\, I_t,\, E_t} \; \sum_{t=1}^{T} \Big( I_t \cdot PI_t - E_t \cdot PE_t + (PTD_t - PTU_t)\,O_t \Big) + \lambda \sum_{t=1}^{T-1}\left(O_{t+1} - O_t\right)^{2}
```

Note that $`\lambda`$ penalizes slot-to-slot changes in the optimal load
(**ramping**), not the deviation from the original flexible load
profile.

The decision variables are $`X = [O_t, I_t, E_t]`$, i.e. $`3T`$
continuous variables, and the solver depends on $`\lambda`$:

| `lambda` | Solver | Extra variables | Import/export exclusivity |
|----|----|----|----|
| `0` (default) | HiGHS MILP | $`y_t`$ ($`T`$ binary) | Enforced exactly by the binary grid mode |
| `> 0` | OSQP QP | — | No binary variable; see the note below |

Moreover, this optimization problem has the following constraints:

- The energy consumed by the flexible load must remain the same than the
  expected behavior:

``` math
\sum_{t=1}^T O_t \Delta t = \sum_{t=1}^T LF_t \Delta t
```

- Energy balance behind-the-meter:

``` math
I_t - E_t = O_t + LS_t - G_t \quad t \in T
```

- The optimal flexible load must respect both its own maximum power and
  the grid connection capacity, combined into a single box bound exactly
  as described in the [net power
  article](https://resourcefully-dev.github.io/flextools/articles/minimize_net_power.html#demand-optimization):

``` math
\underline{O}_t \;\le\; O_t \;\le\; \overline{LF}_t \quad t \in T
```

``` math
\overline{LF}_t = \min\!\left(\max\!\left(G_t - LS_t + IC_t,\; 0\right),\; LFmax_t\right), \qquad
\underline{O}_t = \min\!\left(\max\!\left(G_t - LS_t - EC_t,\; 0\right),\; \overline{LF}_t\right)
```

- The imported and exported power must remain between 0 and the grid
  import and export capacity:

``` math
0 \le I_t \le IC_t \quad 0 \le E_t \le EC_t \quad t \in T
```

- Imported and exported energy can not be positive at the same time.
  When $`\lambda = 0`$ this is enforced exactly with one binary grid
  mode variable per time slot:

``` math
0 \le I_t \le M^I_t\, y_t \qquad 0 \le E_t \le M^E_t\,(1-y_t) \qquad y_t \in \{0,1\} \quad t \in T
```

Where $`M^I_t`$ and $`M^E_t`$ are the tightest per-slot bounds implied
by the optimal load bounds and the site balance:

``` math
M^I_t = \min\!\left(IC_t,\; \max\!\left(\overline{LF}_t + LS_t - G_t,\; 0\right)\right), \qquad
M^E_t = \min\!\left(EC_t,\; \max\!\left(G_t - LS_t - \underline{O}_t,\; 0\right)\right)
```

- The forward and backward **time horizon** constraints are identical to
  those described in the [net power
  article](https://resourcefully-dev.github.io/flextools/articles/minimize_net_power.html#demand-optimization),
  and are not repeated here.

**When `lambda > 0`.** The quadratic ramping term makes the problem a
QP, which is solved with OSQP over the same $`3T`$ continuous variables
but **without** the binary grid mode. Two adjustments keep that QP well
posed:

- Export prices are clipped to $`PE_t \leftarrow \min(PE_t, PI_t)`$.
  Without this, a slot where exporting pays more than importing costs
  would make the objective unbounded below. A warning is emitted once
  when clipping occurs.
- The upper bounds on $`I_t`$ and $`E_t`$ are tightened to $`M^I_t`$ and
  $`M^E_t`$ above, which is what keeps the problem bounded when import
  prices are negative.

Because there is no binary variable on this path, the solver may in
principle return a small simultaneous import and export in the same
slot. Since only the net flow $`I_t - E_t`$ is physically meaningful and
it is preserved, the reported profiles are collapsed to a single
direction per slot afterwards. If a strict per-slot exclusivity
guarantee is required, use the default `lambda = 0`.

**Infeasible windows.** The same minimal grid capacity relaxation
described in the [net power
article](https://resourcefully-dev.github.io/flextools/articles/minimize_net_power.html#demand-optimization)
applies here, falling back to the original profile $`LF`$ if the solver
still fails.

## Battery optimization

To minimize the energy cost using the **flexibility from a battery**,
the objective function of the optimization problem has been raised in
the following way:

``` math
\min_{C_t,\, D_t,\, I_t,\, E_t} \; \sum_{t=1}^{T} \left( I_t \cdot PI_t - E_t \cdot PE_t + \frac{c_{cyc}}{B_{cap}} \cdot D_t \right) + \lambda \sum_{t=1}^{T-1}\Big((C_{t+1} - D_{t+1}) - (C_t - D_t)\Big)^{2}
```

Where:

- $`T`$ : Number of time intervals within the optimization window
- $`G_t`$ : Local power generation time-series vector
- $`L_t`$ : Power load time-series vector
- $`C_t`$ : Battery charging power (decision variable, non-negative)
- $`D_t`$ : Battery discharging power (decision variable, non-negative)
- $`B_t = C_t - D_t`$ : Net battery exchange (positive = charging)
- $`I_t`$ : Imported power (decision variable, non-negative)
- $`E_t`$ : Exported power (decision variable, non-negative)
- $`y_t`$ : Binary grid mode (1 = importing, 0 = exporting)
- $`PI_t`$ : Imported energy price
- $`PE_t`$ : Exported energy price
- $`\lambda`$ : Ramping penalty weight

Additionally, this optimization problem also counts with the following
parameters used in the constraints below:

- $`B_{cap}`$ : Battery capacity
- $`B_c`$ : Maximum charging power
- $`B_d`$ : Maximum discharging power
- $`SOC_{min}`$ : Minimum state of charge of the battery (%)
- $`SOC_{max}`$ : Maximum state of charge of the battery (%)
- $`SOC_{ini}`$ : State of charge at the beginning/end of the
  optimization window (%)
- $`\eta_c`$ : Battery charging efficiency (`charge_eff`)
- $`\eta_d`$ : Battery discharging efficiency (`discharge_eff`)
- $`c_{cyc}`$ : Cycle degradation cost per kWh cycled (`cycle_cost`)

Unlike the [net power
objective](https://resourcefully-dev.github.io/flextools/articles/minimize_net_power.html#battery-optimization),
the cost objective uses **separate charging and discharging variables**
$`C_t`$ and $`D_t`$, which is what allows the charging and discharging
efficiencies to be applied correctly in the state-of-charge constraints.

Note also that the balancing market prices ($`PTU_t`$, $`PTD_t`$) only
apply to the flexible demand optimization; they are not part of the
battery objective.

Optimization constraints:

- Charging and discharging power limits:

``` math
0 \le C_t \le B_c \qquad 0 \le D_t \le B_d \quad t \in T
```

There is **no binary battery mode** variable forcing
$`C_t \cdot D_t = 0`$. Simultaneous charging and discharging is avoided
implicitly: it either has no benefit (at equal prices there is nothing
to gain from cycling energy through the battery) or is directly
penalized, since a positive `cycle_cost` makes it economically
self-defeating.

- State of charge limits, considering charging and discharging
  efficiencies:

``` math
SOC_{min} \le SOC_{ini} + \frac{100}{B_{cap}} \sum_{k=1}^t \left(\eta_c C_k - \frac{D_k}{\eta_d}\right) \Delta t \le SOC_{max} \quad t = 1, \dots, T
```

- The balance of stored energy must be 0 at the end of the optimization
  window to have the same initial state of charge at the beginning of
  every optimization window:

``` math
\sum_{t=1}^T \left(\eta_c C_t - \frac{D_t}{\eta_d}\right) \Delta t = 0
```

- Energy balance behind-the-meter:

``` math
I_t - E_t = C_t - D_t + L_t - G_t \quad t \in T
```

- The imported and exported power must remain between 0 and the grid
  import and export capacity. $`IC_t`$ and $`EC_t`$ limit the **net**
  flow while $`I_t`$ and $`E_t`$ are one sided, so only the non-negative
  part of the capacity can act as a box:

``` math
0 \le I_t \le \max\!\left(IC_t,\; 0\right) \quad 0 \le E_t \le \max\!\left(EC_t,\; 0\right) \quad t \in T
```

- The capacity itself is imposed on the net flow, with the same
  penalised slack as in the [net power
  article](https://resourcefully-dev.github.io/flextools/articles/minimize_net_power.html#battery-optimization).
  This is what expresses a *negative* capacity — an obligation to flow
  the other way, e.g. $`IC_t = -100`$ requiring at least 100 kW of
  export — which a one-sided box cannot represent:

``` math
I_t - E_t - s^I_t \;\le\; IC_t, \qquad E_t - I_t - s^E_t \;\le\; EC_t \quad t \in T
```

``` math
0 \le s^I_t \le \max\!\left(0,\; (L_t - G_t) - IC_t\right), \qquad
0 \le s^E_t \le \max\!\left(0,\; (G_t - L_t) - EC_t\right)
```

These rows and variables are added only when a capacity can be missed or
is negative. With no capacity limits the problem is exactly the one
described above.

Unlike the quadratic paths, this objective needs no post-processing to
concentrate an unreachable capacity: being an LP (or MILP), its optimum
sits on a vertex of the feasible region, so the miss already comes out
concentrated on as few slots as the battery’s energy allows rather than
spread thinly across the window. The redistribution described in the
[net power
article](https://resourcefully-dev.github.io/flextools/articles/minimize_net_power.html#battery-optimization)
is therefore not applied here.

### Solver paths

The combination of `lambda` and `cycle_cost` selects one of three
formulations:

| `lambda` | `cycle_cost` | Solver | Variables | Import/export exclusivity |
|----|----|----|----|----|
| `0` (default) | `0` (default) | HiGHS MILP | $`C_t, D_t, I_t, E_t`$ + $`y_t`$ binary | Enforced exactly by the binary grid mode |
| `0` | `> 0` | HiGHS LP | $`C_t, D_t, I_t, E_t`$ | Not enforced; see below |
| `> 0` | any | OSQP QP | $`C_t, D_t, I_t, E_t`$ + SOC state | Not enforced; see below |

**MILP path (default).** Imported and exported energy can not be
positive at the same time, enforced with one binary grid mode variable
per time slot:

``` math
0 \le I_t \le M^I_t\, y_t \qquad 0 \le E_t \le M^E_t\,(1-y_t) \qquad y_t \in \{0,1\} \quad t \in T
```

``` math
M^I_t = \min\!\left(IC_t,\; \max\!\left(L_t - G_t + B_c,\; 0\right)\right), \qquad
M^E_t = \min\!\left(EC_t,\; \max\!\left(G_t - L_t + B_d,\; 0\right)\right)
```

This is the most accurate but also the most expensive path, since a
branch-and-bound problem is solved for every optimization window.

**LP path (`cycle_cost > 0`).** A positive cycle cost adds the linear
term $`\frac{c_{cyc}}{B_{cap}} D_t`$ to the objective, which converts
the per-kWh degradation cost into a penalty on discharged power, so the
optimizer trades energy cost savings against battery wear. This penalty
alone makes simultaneous charging and discharging uneconomical, so the
binary grid mode is dropped and the problem becomes a pure LP —
substantially faster than the MILP. On this path the import and export
bounds are tightened to their physically achievable values, which is
what keeps the LP bounded when import prices are negative:

``` math
0 \le I_t \le \min\!\left(IC_t,\; \max\!\left(L_t - G_t + B_c,\; 0\right)\right), \qquad
0 \le E_t \le \min\!\left(EC_t,\; \max\!\left(G_t - L_t + B_d,\; 0\right)\right)
```

**QP path (`lambda > 0`).** The quadratic ramping term is applied to the
net battery power $`B_t = C_t - D_t`$ and the problem is solved with
OSQP. Both `cycle_cost` and `lambda` are active simultaneously when both
are positive. As for the demand QP, export prices are clipped to
$`PE_t \leftarrow \min(PE_t, PI_t)`$ to keep the objective bounded
below, with a warning emitted once. If OSQP fails to converge, the
solver falls back to the LP path, which retains the cycle cost but loses
the ramping term.

Neither the LP nor the QP path includes a binary variable enforcing
$`I_t \cdot E_t = 0`$. In practice the state-of-charge constraint
couples all slots of the window strongly enough that within-slot
simultaneous flows are not profitable. When a strict exclusivity
guarantee is required, use the default MILP path (`lambda = 0` and
`cycle_cost = 0`).
