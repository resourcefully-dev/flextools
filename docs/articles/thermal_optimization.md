# Thermal optimization

Function
[`add_tank_optimization()`](https://resourcefully-dev.github.io/flextools/reference/add_tank_optimization.md)
optimizes a **heat pump that charges one thermal tank** (e.g. a domestic
hot water or a buffer tank), which the heat demand of the building
discharges. It solves one independent optimization problem per
**optimization window** (typically one day), with the same objectives,
optimization windows and solvers as
[`add_battery_optimization()`](https://resourcefully-dev.github.io/flextools/reference/add_battery_optimization.md):

- Minimize the power exchanged with the grid (net power), with
  `opt_objective = "grid"` (QP, [OSQP](https://osqp.org/))
- Minimize the energy cost, with `opt_objective = "cost"` (LP,
  [HiGHS](https://highs.dev/), or QP when `lambda > 0`)
- Minimize a weighted combination of net power and cost, with a numeric
  `opt_objective` between `0` and `1` (QP)

The tank plays the role of the battery: it stores heat, so the
compressor can run when electricity is cheap or locally produced, and
the tank delivers the heat later. On top of the compressor, the heat
pump can have a **resistive backup heater**, and the comfort floor of
the tank is **soft**: when neither the compressor nor the backup heater
can keep the tank above it, the tank keeps giving heat below its comfort
temperature, down to the temperature of the inlet water, and the
optimization reports that heat as **unmet heat** instead of failing. In
this article, we’ll cover the tank model, the backup heater and the soft
comfort floor, followed by a worked example.

## Tank model

The decision variables of every optimization window, with $`T`$ time
slots of $`\Delta t`$ hours, are:

- $`P_t`$ : Electric power of the compressor (kW)
- $`B_t`$ : Power of the backup heater (kW). It is resistive, so its
  electric power is also its heat ($`COP = 1`$). Only a variable when
  the heat pump has a backup heater (`Pb > 0`)
- $`S_t`$ : Level of the tank at the end of slot $`t`$ (kWh thermal):
  the heat it holds above the **inlet temperature**, the temperature of
  the cold water that refills it. At $`S_t = 0`$ the tank is at the
  inlet temperature and gives no more heat

And the parameters:

- $`Q_t`$ : Heat demand (kW thermal), the column `heat_demand` of
  `opt_data`
- $`COP_t`$ : Coefficient of performance of the compressor, the column
  `cop` of `opt_data`. $`COP_t = 0`$ means the compressor cannot serve
  the tank in that slot (e.g. while it defrosts or serves another
  circuit)
- $`T_{cap}`$ : Tank capacity (kWh thermal), `Tcap`: the heat between
  the inlet temperature and the top of the tank
- $`P_c`$ : Maximum electric power of the compressor, `Pc`
- $`P_b`$ : Rating of the backup heater, `Pb` (`NULL` or `0` without
  one)
- $`SOC_{min}`$ : Comfort floor of the tank (%), the lowest level that
  still delivers heat at the comfort temperature
- $`SOC_{max}`$ : Top of the tank (%)
- $`l`$ : Standing loss of the tank (% per hour), `loss`
- $`S_0`$ : Level before the first slot of the window

The level of the tank follows the heat balance:

``` math
S_t = S_{t-1} \left(1 - \frac{l}{100}\right)^{\Delta t} + \left(COP_t \cdot P_t + B_t - Q_t\right) \Delta t \quad t \in T
```

with the following constraints:

- The level stays between the comfort floor and the top of the tank. The
  floor is soft (see [Soft comfort floor](#soft-comfort-floor)): the
  level can go below it, down to the inlet temperature ($`S_t = 0`$) at
  most:

``` math
T_{cap} \frac{SOC_{min}}{100} \le S_t \le T_{cap} \frac{SOC_{max}}{100} \quad t \in T
```

- The compressor runs between 0 and its maximum power, and not at all
  where the COP is 0:

``` math
0 \le P_t \le P_c, \qquad P_t = 0 \;\; \text{where} \;\; COP_t = 0 \quad t \in T
```

- The backup heater runs between 0 and its rating, **also where the COP
  is 0**:

``` math
0 \le B_t \le P_b \quad t \in T
```

- Every window ends at least as full as it started, so that a window
  does not empty the tank at the expense of the next one:

``` math
S_T \ge S_0
```

- The heat pump’s electric load $`P_t + B_t`$ is part of the net power
  of the site, $`L_t + P_t + B_t - G_t`$, which must stay within the
  grid import and export capacities, as in the [net power
  article](https://resourcefully-dev.github.io/flextools/articles/minimize_net_power.html),
  unless they cannot be met (see [Soft grid
  capacities](#soft-grid-capacities)).

### Litres and temperatures

The model counts the heat in the tank in kWh, but a tank is usually
described by its **volume** and its **temperatures**. Assuming a mixed
tank (one temperature for all its water), the heat it holds above the
inlet temperature $`T_{in}`$ is:

``` math
S = V \cdot c_p \cdot \left(T - T_{in}\right), \qquad c_p = \frac{4.186}{3600} \approx 0.001163 \; \text{kWh / (L K)}
```

So a tank of volume $`V`$ (L) that can be heated up to $`T_{max}`$, with
a comfort floor at $`T_{floor}`$, translates into the arguments of
[`add_tank_optimization()`](https://resourcefully-dev.github.io/flextools/reference/add_tank_optimization.md)
as:

``` math
T_{cap} = V \cdot c_p \cdot \left(T_{max} - T_{in}\right), \qquad
SOC_{min} = 100 \cdot \frac{T_{floor} - T_{in}}{T_{max} - T_{in}}, \qquad
SOC_{max} = 100
```

and the level $`S_t`$ back into the temperature
$`T_t = T_{in} + S_t / (V c_p)`$. For example, 300 L between an inlet
temperature of 20 °C and a maximum of 55 °C hold
$`300 \cdot 0.001163 \cdot 35 \approx 12.2`$ kWh, and a comfort floor at
30 °C is $`SOC_{min} \approx 28.6\%`$.

### Optimization windows

The level carries from one window to the next, so the windows are solved
in order. They are the windows of the other optimization functions, one
every `window_days` days from `window_start_hour`, plus the **partial
windows** that the run cuts at both ends:

- When the first time slot of the run is not a window start (e.g. a run
  from 00:00 with `window_start_hour = 6`), the slots up to the first
  window start form a **leading window**. It starts from `SOCini`, and
  it must also end at least as full as it started.
- The slots after the last full window form a **trailing window**.

Both are optimized like any other window. Only the first
`flex_window_hours` of every window are optimized, and the slots after
them run the static schedule of the heat pump, the column `flexible` of
`opt_data`. So with `flex_window_hours = 24` (and `window_days = 1`)
**every time slot is optimized**.

The slots that run `flexible` behave like the tank too: at the top, the
compressor stops as a thermostat would (its power is the static schedule
reduced to what keeps the tank at $`SOC_{max}`$), and below the comfort
floor the tank cools down to the inlet temperature at most, with unmet
heat as in the optimized slots.

## Objective

The objective is the one of the battery, with the heat pump’s electric
load $`P_t + B_t`$ in place of the battery’s power, plus a linear term
on the backup heater. For the net power objective:

``` math
\min_{P_t,\, B_t} \; \sum_{t=1}^{T} \left(L_t + P_t + B_t - G_t\right)^2 + \lambda \sum_{t=1}^{T-1}\left(P_{t+1} - P_t\right)^{2} + \sum_{t=1}^{T} 2h \cdot B_t
```

and for the energy cost objective, with the imported power $`I_t`$ and
exported power $`E_t`$ of the [cost
article](https://resourcefully-dev.github.io/flextools/articles/minimize_cost.html)
and $`I_t - E_t = L_t + P_t + B_t - G_t`$:

``` math
\min_{P_t,\, B_t,\, I_t,\, E_t} \; \sum_{t=1}^{T} \Big( I_t \cdot PI_t - E_t \cdot PE_t \Big) + \lambda \sum_{t=1}^{T-1}\left(P_{t+1} - P_t\right)^{2} + \sum_{t=1}^{T} 2h \cdot B_t
```

The weighted objective combines both as described in the [combined
optimization
article](https://resourcefully-dev.github.io/flextools/articles/combined_optimization.html),
with the same linear term. When the comfort floor or the grid capacities
cannot be held, the window gets the further terms described below.

### Compressor first, backup heater second, comfort last

The linear terms order the ways to cover the heat demand. They are built
from the **heat price** $`h`$ of the window: the highest value that one
kW of electric load can take in the window’s objective, divided by the
lowest positive COP when it is below 1, so that no kWh of heat from the
compressor is worth more than $`h`$:

| Objective | Heat price $`h`$ |
|----|----|
| `"cost"` | $`\max_t \left(\lvert PI_t \rvert, \lvert PE_t \rvert\right)`$, the highest electricity price of the window |
| `"grid"` | $`2 \left(\max_t \lvert L_t - G_t \rvert + P_c + P_b\right)`$, the largest derivative of the squared net power |
| weight `w` | `w` times the grid value (scaled as the net power term) plus `1 - w` times the cost value |

and with `lambda > 0` the largest derivative of the ramping penalty,
$`4 \lambda P_c`$, is added to it. Then:

- **The backup heater** pays $`2h`$ per kW of heat on top of its
  electricity, which the objective already sees like the compressor’s.
  Its heat is therefore always more expensive than the compressor’s, so
  it only runs for the heat the compressor cannot deliver, either
  because the compressor is at $`P_c`$, or because its COP is 0. It
  never runs to absorb a PV surplus or a cheap hour that the compressor
  could absorb.
- **Missing a grid capacity** costs $`10h`$ per kW (see [Soft grid
  capacities](#soft-grid-capacities)).
- **Going below the comfort floor** costs $`20h / \Delta t`$ per kWh
  below it and time slot, more than any kWh of heat the compressor, the
  backup heater or a missed grid capacity can deliver. So the tank only
  cools below its floor when they cannot hold it, and is brought back
  above it as soon as they can.

### Soft comfort floor

Without a soft floor, a window where the compressor and the backup
heater cannot keep the tank above $`SOC_{min}`$ (a cold spell, a
compressor too small for the peak, a COP of 0 during a long defrost)
would have no solution at all. A real tank, in that case, keeps giving
heat: its water gets colder than the comfort temperature, down to the
temperature of the inlet water, and from there it gives no more. The
optimization does the same. When a window has no solution that holds the
floor, it is solved again with:

- the level between the inlet temperature and the top,
  $`0 \le S_t \le T_{cap} \frac{SOC_{max}}{100}`$;
- the depth below the floor,
  $`D_t \ge T_{cap} \frac{SOC_{min}}{100} - S_t`$, priced at
  $`20h / \Delta t`$ per kWh and slot;
- the end condition $`S_T \ge S_0`$ missed by $`R \ge 0`$ at the same
  price, so that a tank that has been drained does not have to be
  refilled by the end of the window at any cost.

**Every window has a solution** then. The **unmet heat** $`U_t`$ is the
**heat that would have been required to keep the tank at least at its
comfort floor**: the extra heat that, added in slot $`t`$, would have
stopped the temperature from falling below the comfort temperature (or
further below it). It is a missing energy, not a measure of discomfort:
it is positive while the temperature is falling below the floor, and 0
again as soon as it stops falling, even if the tank is still below the
floor while it warms back up. Its total is the extra heat a larger heat
pump or backup heater would have had to deliver. With
$`\tilde S_t = S_{t-1} (1 - l/100)^{\Delta t} + (COP_t P_t + B_t - Q_t) \Delta t`$
the level the balance asks for (and $`S_t = \max(\tilde S_t, 0)`$ the
level the tank reaches):

``` math
U_t \, \Delta t = \max\left(0,\; \min\left(T_{cap} \frac{SOC_{min}}{100},\; S_{t-1} (1 - l/100)^{\Delta t}\right) - \tilde S_t\right)
```

The first term covers the heat the tank gives from below its floor, and
the part of $`\tilde S_t`$ below 0 the demand it cannot serve at all
once it is at the inlet temperature. Standing losses below the floor are
not counted.

So in terms of temperature: the backup heater runs when, with the
compressor at full power, the temperature of the tank would drop below
the comfort floor; and **heat is unmet when, even with the backup
heater, the temperature drops below the comfort floor**, by as much heat
as it would have taken to keep it there. The temperature keeps falling
with the demand, to the inlet temperature at most. The unmet heat is a
report: it is not part of the heat balance of the tank.

The soft floor only enters the problem of a window when the window has
no solution that holds the floor. So, without a backup heater and with a
band that the compressor can hold, the problem is exactly the one
without it.

### Soft grid capacities

As for the
[battery](https://resourcefully-dev.github.io/flextools/articles/minimize_net_power.html#battery-optimization),
the grid import and export capacities are **soft**. When a window has no
solution that holds the band within them (e.g. a PV surplus above the
export capacity that a full tank cannot absorb, or an import capacity
too low to cover the heat demand), it is solved with the soft comfort
floor and with a penalised slack on each capacity, $`s^I_t`$ and
$`s^E_t`$:

``` math
L_t + P_t + B_t - G_t - s^I_t \le IC_t, \qquad G_t - L_t - P_t - B_t - s^E_t \le EC_t
```

The slack is bounded by how far the site already misses the capacity
without the optimization: for import, with the heat pump running its
static schedule $`LF_t`$; for export, with the heat pump off, since a
full tank cannot take any more heat:

``` math
0 \le s^I_t \le \max\left(0,\; L_t + LF_t - G_t - IC_t\right), \qquad
0 \le s^E_t \le \max\left(0,\; G_t - L_t - EC_t\right)
```

It costs $`10h`$ per kW in the objective, between the backup heater and
going below the comfort floor. So the solver uses the compressor and the
backup heater to their limits before missing a capacity, and misses an
import capacity before letting the tank cool below its floor. The heat
pump switched off, with the tank cooling, always fits these bounds, so
**every window has a solution**. When a capacity is still missed, a
message says so, and the net power $`L_t + P_t + B_t - G_t`$ shows
where.

## Results

[`add_tank_optimization()`](https://resourcefully-dev.github.io/flextools/reference/add_tank_optimization.md)
returns the compressor’s electric power $`P_t`$ (kW), one value per row
of `opt_data`, rounded to 2 decimals. Its attributes hold the rest of
the solution, one value per row as well:

| Attribute | Content |
|----|----|
| `backup_kw` | Power of the backup heater $`B_t`$ (kW electric, which is also kW thermal). 0 without a backup heater |
| `unmet_heat_kw` | Unmet heat $`U_t`$ (kW thermal): the heat that would have been required in each slot to keep the tank at least at its comfort floor. Positive while the temperature falls below the floor, 0 while it warms back up |

The electric load of the heat pump is the returned power plus
`attr(P, "backup_kw")`. The level of the tank is not returned, but
[`get_tank_level()`](https://resourcefully-dev.github.io/flextools/reference/get_tank_level.md)
computes it from the power and the backup heater, stopping at the inlet
temperature.
[`get_tank_unmet_heat()`](https://resourcefully-dev.github.io/flextools/reference/get_tank_unmet_heat.md)
computes the unmet heat of any schedule, with the same definition: for
example, of the static schedule `flexible`, to compare it with the
optimized one.

A window the solver fails on (a numerical failure, since every window
has a solution) runs the static schedule, with a message. With
`on_failure = "error"`, it stops with an error naming the window and the
solver status instead.

## Example

Package `flextools` provides an example data set of time-series energy
and prices profiles, `energy_profiles`. We will use three days of
January 2023 with 15-minute resolution, the building consumption as
static load and the solar PV production. The import prices of this data
set are sometimes negative, so we cap the export price at the import
price (a feed-in tariff that never pays more than importing costs).

The data set has no heat demand, so we build one with a morning and an
evening peak, around a base of 2 kW. The **second day is a cold
morning**: its morning peak reaches 12 kW, far above what the compressor
can deliver. The COP of the compressor is 3.2, and drops to 2.7 on the
cold day:

``` r

opt_data <- energy_profiles %>%
  filter(
    datetime >= dmy("09/01/2023", tz = "CET"),
    datetime < dmy("12/01/2023", tz = "CET")
  ) %>%
  mutate(
    day = as.integer(date(datetime) - dmy("09/01/2023")) + 1,
    hour = hour(datetime) + minute(datetime) / 60
  ) %>%
  transmute(
    datetime,
    production = solar,
    static = building,
    price_imported,
    price_exported = pmin(price_exported, price_imported),
    heat_demand = 2 +
      ifelse(
        day == 2,
        10 * exp(-((hour - 8) / 2)^2), # cold morning
        2.5 * exp(-((hour - 7.5) / 1.2)^2)
      ) +
      3 * exp(-((hour - 19) / 1.5)^2),
    cop = ifelse(day == 2, 2.7, 3.2),
    flexible = heat_demand / cop
  )

head(opt_data)
```

    ## # A tibble: 6 × 8
    ##   datetime            production static price_imported price_exported
    ##   <dttm>                   <dbl>  <dbl>          <dbl>          <dbl>
    ## 1 2023-01-09 00:00:00          0   1.65         0.0644          0.025
    ## 2 2023-01-09 00:15:00          0   1.64         0.0644          0.025
    ## 3 2023-01-09 00:30:00          0   1.63         0.0644          0.025
    ## 4 2023-01-09 00:45:00          0   1.62         0.0644          0.025
    ## 5 2023-01-09 01:00:00          0   1.61         0.0603          0.025
    ## 6 2023-01-09 01:15:00          0   1.57         0.0603          0.025
    ## # ℹ 3 more variables: heat_demand <dbl>, cop <dbl>, flexible <dbl>

The static schedule `flexible` is the heat pump following the heat
demand without any storage:

The heat pump has a 2 kW compressor and a **300 L tank** that can be
heated up to **55 °C**, with a **comfort floor at 30 °C**, refilled with
inlet water at **20 °C**. We convert them into the tank’s capacity and
comfort floor as described [above](#litres-and-temperatures):

``` r

volume <- 300 # L
T_in <- 20 # °C, inlet water
T_floor <- 30 # °C, comfort floor
T_max <- 55 # °C
kwh_per_degree <- volume * 4.186 / 3600 # kWh per °C

Tcap <- kwh_per_degree * (T_max - T_in)
SOCmin <- 100 * (T_floor - T_in) / (T_max - T_in)
c(Tcap = Tcap, SOCmin = SOCmin)
```

    ##     Tcap   SOCmin 
    ## 12.20917 28.57143

The tank always holds its 300 L, since the inlet water refills whatever
is drawn: it is its temperature that tells how much heat it holds. From
55 °C down to the 30 °C floor it can give 8.7 kWh at a comfortable
temperature, and 3.5 kWh more, colder, down to the 20 °C of the inlet
water. On the cold morning the compressor delivers at most
$`2.7 \cdot 2 = 5.4`$ kW of heat: not enough for the peak.

To show the results, a small helper gathers the compressor power, the
backup heater, the unmet heat and the temperature of the tank (from the
level that
[`get_tank_level()`](https://resourcefully-dev.github.io/flextools/reference/get_tank_level.md)
computes) for each scenario:

``` r

tank_results <- function(opt_data, P) {
  opt_data %>%
    mutate(
      P = P,
      B = attr(P, "backup_kw"),
      U = attr(P, "unmet_heat_kw"),
      level = get_tank_level(
        P, heat_demand, cop,
        init = Tcap, time_resolution = 15,
        backup = B
      ),
      temperature = T_in + level / kwh_per_degree
    )
}

daily_totals <- function(results) {
  results %>%
    group_by(date = date(datetime)) %>%
    summarise(
      demand_kWh = sum(heat_demand) / 4,
      compressor_kWh = sum(P) / 4,
      backup_kWh = sum(B) / 4,
      unmet_kWh = sum(U) / 4,
      min_temp_C = min(temperature)
    )
}
```

### Without backup heater

First, the heat pump has no backup heater and minimizes the energy cost:

``` r

P <- add_tank_optimization(
  opt_data,
  opt_objective = "cost",
  Tcap = Tcap,
  Pc = 2,
  SOCmin = SOCmin
)
```

    ## ⚠️ Optimization warning: the tank goes below its comfort floor in some slots. That heat is unmet (attribute `unmet_heat_kw`).

``` r

results <- tank_results(opt_data, P)
daily_totals(results)
```

    ## # A tibble: 3 × 6
    ##   date       demand_kWh compressor_kWh backup_kWh unmet_kWh min_temp_C
    ##   <date>          <dbl>          <dbl>      <dbl>     <dbl>      <dbl>
    ## 1 2023-01-09       61.3           19.2          0      0          30.0
    ## 2 2023-01-10       91.4           32.3          0      7.59       20  
    ## 3 2023-01-11       61.3           19.2          0      0          30.0

On the first and the last day, the compressor heats the tank up to 55 °C
during the cheapest hours before each peak, and the tank covers the
peaks without dropping below 30 °C. On the cold morning the tank starts
full, and the compressor runs at full power through the peak, but the
temperature falls below the comfort floor at around 08:00 and down to
the inlet temperature (20 °C) at around 09:00, where the tank gives no
more heat. The **unmet heat** is the heat that would have been needed to
keep the tank at 30 °C: the heat the tank gives below 30 °C, and the
demand it cannot serve at all at 20 °C. It follows how fast the
temperature falls below the floor, not how far below it is: it ends when
the temperature stops falling, at the end of the peak, while the
compressor is still bringing the tank back above its floor.

### With a backup heater

Now the heat pump also has a **1.5 kW backup heater** (`Pb = 1.5`):

``` r

P_backup <- add_tank_optimization(
  opt_data,
  opt_objective = "cost",
  Tcap = Tcap,
  Pc = 2,
  SOCmin = SOCmin,
  Pb = 1.5
)
```

    ## ⚠️ Optimization warning: the tank goes below its comfort floor in some slots. That heat is unmet (attribute `unmet_heat_kw`).

``` r

results_backup <- tank_results(opt_data, P_backup)
daily_totals(results_backup)
```

    ## # A tibble: 3 × 6
    ##   date       demand_kWh compressor_kWh backup_kWh unmet_kWh min_temp_C
    ##   <date>          <dbl>          <dbl>      <dbl>     <dbl>      <dbl>
    ## 1 2023-01-09       61.3           19.2       0         0          30.0
    ## 2 2023-01-10       91.4           31.4       6.76      1.94       24.4
    ## 3 2023-01-11       61.3           19.2       0         0          30.0

The compressor runs as before, and the **backup heater switches on**
when the compressor at full power can no longer keep the tank above the
comfort floor during the cold morning. The optimization looks ahead: it
starts the backup heater as soon as the peak starts, rather than when
the tank reaches 30 °C, because the heat it adds early is kept in the
tank for later. The tank now reaches its comfort floor later, at around
08:45, and **only then is heat unmet**: the tank cools to about 24 °C,
and the unmet heat is the heat that, on top of the compressor and the
backup heater, would have kept it at 30 °C. It stops when the
temperature reaches its lowest point. Right after the peak, the backup
heater keeps running with the compressor until the tank is back above
its floor. On the first and the last day the compressor suffices, and
the backup heater stays off even though it could run in cheaper hours.

With a larger backup heater (e.g. `Pb = 3`), the tank stays above its
floor and the unmet heat disappears altogether.
