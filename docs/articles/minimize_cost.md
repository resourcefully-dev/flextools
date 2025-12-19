# Energy cost optimization

Function
[`optimize_demand()`](https://resourcefully-dev.github.io/flextools/reference/optimize_demand.md)
and
[`add_battery_optimization()`](https://resourcefully-dev.github.io/flextools/reference/add_battery_optimization.md)
makes use of Quadratic programming in order to obtain the optimal power
load given certain conditions. The Quadratic programming problem can be
formulated according to multiple objectives. Currently, `flextools`
package allows to optimize a time-series power load considering two
different goals:

- Minimize the power exchanged with the grid (net power)
- Minimize the energy cost

In this article, we’ll cover the optimization problem for the first
objective, **energy_cost minimization**, for both the **flexible
demand** (e.g. heatpumps, electric vehicles, etc.) and the **battery**.

Our energy cost optimization takes into account multiple profiles of
energy prices with the objective to minimize the total cost,
considering:

- Imported energy cost
- Exported energy income
- Income from balancing markets (turn up/down demand)

## Demand optimization

To minimize the energy cost using the **flexibility from a power demand
profile**, the objective function of the optimization problem has been
raised in the following way:

``` math
min \sum_{t=1}^{T} I_t·PI_t - E_t·PE_t - PTU_t(O_t-LF_t)  - PTD_t(LF_t-O_t) + \lambda (O_t-LF_t)^{2}
```

The objective function and constraints of this optimization problem are
represented below, where:

- $`T`$ : Number of time intervals within the optimization window
- $`G_t`$ : Local power generation time-series vector
- $`LS_t`$ : Non-flexible load time-series vector
- $`LF_t`$ : Flexible load time-series vector (if not optimized)
- $`O_t`$ : Optimal flexible load time-series vector
- $`I_t`$ : imported energy
- $`E_t`$ : exported energy
- $`PI_t`$ : imported energy price
- $`PE_t`$ : exported energy price
- $`PTU_t`$ : balancing price for turn-up power
- $`PTD_t`$ : balancing price for turn-down power
- $`\lambda`$ : penalty on change for the flexible load

Moreover, this optimization problem has the following constraints:

- The energy consumed by the flexible load must remain the same than the
  expected behavior:

``` math
\sum_{t=1}^T O_t \Delta t = \sum_{t=1}^T LF_t \Delta t
```

- Optimal flexible load must be lower than a certain maximum power
  $`LF_{max}`$:

``` math
0 \le O_t \le LFmax_t \quad t \in T
```

- The imported energy must remain between 0 and the corresponding energy
  consumed with the maximum grid import capacity $`IC_T`$:

``` math
0 \le I_t \le IC_t \Delta t \quad t \in T
```

- The exported energy must remain between 0 and maximum grid export
  capacity $`EC_t`$:

``` math
0 \le E_t \le EC_t \Delta t \quad t \in T
```

- Energy balance behind-the-meter:

``` math
I_t - E_t = O_t + LS_t - G_t
```

At the same time, we can optimize the flexible energy demand with two
opposite approaches:

1.  Postpone the consumption to later time-slots (**shift the energy
    forward**)
2.  Consume now (store) the energy that will be consumed later (**shift
    the energy backward**)

We consider both approaches for different objectives or applications.
The energy shift is always done within a maximum **time horizon**
($`h`$) to consider realistic scenarios. For example, the time horizon
for the energy demand of a water boiler could have a time horizon of 6
hours, because it wouldn’t make sense to heat the water more than 6
hours before the final consumption. Each one of these approaches brings
extra constraints to the optimization problem:

If the energy can only be shifted **forward**:

- The cumulative sum of the optimal load $`O`$ must be higher than the
  cumulative sum of the original flexible load $`LF`$ except the last
  $`h`$ time slots, and lower than the total cumulative sum of the
  original flexible load $`LF`$ (energy can only be shifted forwards):

``` math
\sum_{t = 1}^{u-h} LF_t \le \sum_{t=1}^u O_t \le \sum_{t=1}^u LF_t \quad u = 1 \dots T
```

- The maximum values for the optimal demand $`O_t`$ will depend on the
  time horizon $`h`$:

``` math
O_u \le \sum_{t = u-h}^u LF_t \quad u = 1 \dots T
```

If the energy can only be shifted **backward**:

- The cumulative sum of the optimal demand $`O`$ must be higher than the
  total cumulative sum of the original demand $`LF`$ (energy can only be
  shifted backwards), and lower than the cumulative sum of the original
  demand $`LF`$ except the following $`h`$ time slots:

``` math
\sum_{t=1}^u LF_t \le \sum_{t=1}^u O_t \le \sum_{t=1}^{u+h} LF_t \quad u = 1 \dots T
```

- The maximum values for the optimal demand $`O_t`$ will depend on the
  time horizon $`h`$:

``` math
O_u \le \sum_{t = u}^{u+h} LF_t \quad u = 1 \dots T
```

## Battery optimization

To minimize the energy cost using the **flexibility from a battery**,
the objective function of the optimization problem has been raised in
the following way:

``` math
\min_{C_t, D_t, I_t, E_t} \sum_{t=1}^{T} \left( I_t \cdot PI_t - E_t \cdot PE_t + (PTD_t - PTU_t)(C_t - D_t) \right) + \lambda \sum_{t=1}^{T-1}\Big((C_{t+1} - D_{t+1}) - (C_t - D_t)\Big)^{2}
```

Where:

- $`T`$ : Number of time intervals within the optimization window
- $`G_t`$ : Local power generation time-series vector
- $`L_t`$ : Power load time-series vector
- $`C_t`$ : Battery charging power (decision variable, non-negative)
- $`D_t`$ : Battery discharging power (decision variable, non-negative)
- $`B_t = C_t - D_t`$ : Net battery exchange with the grid (positive =
  charging)
- $`I_t`$ : imported energy
- $`E_t`$ : exported energy
- $`PI_t`$ : imported energy price
- $`PE_t`$ : exported energy price
- $`PTU_t`$ : balancing price for turn-up power
- $`PTD_t`$ : balancing price for turn-down power
- $`\lambda`$ : penalty on change for the flexible load

Additionally, this optimization problem also counts with the following
parameters used in the constraints below:

- $`B_{cap}`$ : Battery capacity
- $`B_c`$ : Maximum charging power
- $`B_d`$ : Maximum discharging power
- $`SOC_{min}`$ : Minimum state of charge of the battery
- $`SOC_{max}`$ : Maximum state of charge of the battery
- $`SOC_{ini}`$ : state of charge at the beginning/end of the
  optimization window
- $`\eta_c`$ : Battery charging efficiency
- $`\eta_d`$ : Battery discharging efficiency

Optimization constraints:

- Battery charging and discharging limits:

``` math
0 \le C_t \le B_c \quad t \in T \qquad 0 \le D_t \le B_d \quad t \in T
```

- State of charge limits, considering charging and discharging
  efficiencies:

``` math
SOC_{min} \le SOC_{ini} + \frac{1}{B_{cap}} \sum_{k=1}^t \left(\eta_c C_k - \frac{D_k}{\eta_d}\right) \Delta t \le SOC_{max} \quad t = 1, \dots, T
```

- The balance of stored energy must be 0 at the end of the optimization
  window to have the same initial state of charge at the beginning of
  every optimization window:

``` math
\sum_{t=1}^T \left(\eta_c C_t - \frac{D_t}{\eta_d}\right) \Delta t = 0
```

- The imported energy must remain between 0 and the maximum grid import
  capacity $`IC_t`$:

``` math
0 \le I_t \le IC_t \Delta t \quad t \in T
```

- The exported energy must remain between 0 and the maximum grid export
  capacity $`EC_t`$:

``` math
0 \le E_t \le EC_t \Delta t \quad t \in T
```

- Energy balance behind-the-meter:

``` math
I_t - E_t = C_t - D_t + L_t - G_t
```
