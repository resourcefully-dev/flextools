# Heat pump with a thermal tank: optimal compressor profile

Optimises the electric power of a heat pump's compressor that charges
one thermal tank, which the heat demand discharges, with an optional
resistive backup heater next to it. It mirrors
[`add_battery_optimization()`](https://resourcefully-dev.github.io/flextools/reference/add_battery_optimization.md):
the same `opt_data` conventions, objectives (except `"capacity"`),
optimization windows and solvers, with the tank in place of the battery.
The compressor's heat is `cop * P`, so there is no conversion
efficiency: the COP is the conversion. The backup heater turns `B` kW of
electricity into `B` kW of heat.

## Usage

``` r
add_tank_optimization(
  opt_data,
  opt_objective = "grid",
  Tcap,
  Pc,
  SOCmin = 0,
  SOCmax = 100,
  SOCini = NULL,
  window_days = 1,
  window_start_hour = 0,
  flex_window_hours = 24,
  lambda = 0,
  loss = 0,
  Pb = NULL,
  on_failure = c("flexible", "error")
)
```

## Arguments

- opt_data:

  tibble, optimization contextual data. The first column must be named
  `datetime` (mandatory). Required columns:

  - `heat_demand`: heat the tank must deliver (kW thermal, \>= 0)

  - `cop`: heat per electric kWh (\>= 0); 0 means the compressor cannot
    serve the tank in that slot

  - `flexible`: the static electric schedule of the heat pump (kW, \>=
    0), returned wherever the function does not optimise

  Optional columns, as in
  [`add_battery_optimization()`](https://resourcefully-dev.github.io/flextools/reference/add_battery_optimization.md):
  `static`, `production`, `import_capacity`, `export_capacity`,
  `price_imported`, `price_exported`.

- opt_objective:

  character or numeric. `"grid"` (default), `"cost"`, or a numeric
  weight `w` in `[0, 1]` where `w = 1` is pure grid and `w = 0` is pure
  cost. `"grid"` minimises the squared net grid flow
  `static + P + B - production`; `"cost"` minimises the energy cost of
  it. `"capacity"` has no tank form and stops with an error.

- Tcap:

  numeric, tank capacity (kWh thermal, \> 0): the heat between the inlet
  water temperature (`SOC = 0`, where the tank gives no more heat) and
  the top. For a mixed tank of `V` litres between the inlet temperature
  `T_in` and its top temperature `T_max` (degrees C), it is
  `V * 4.186 / 3600 * (T_max - T_in)`, and a comfort floor at `T_floor`
  is `SOCmin = 100 * (T_floor - T_in) / (T_max - T_in)`.

- Pc:

  numeric, compressor cap (kW electric, \> 0).

- SOCmin:

  numeric, minimum State-of-Charge (%), the comfort floor.

- SOCmax:

  numeric, maximum State-of-Charge (%).

- SOCini:

  numeric, State-of-Charge (%) at the start of the run, within
  `[SOCmin, SOCmax]`. `NULL` (default) means `SOCmax`, unlike the
  battery: a tank starting at its floor has no reserve for its first
  window.

- window_days:

  integer, optimization window length in days.

- window_start_hour:

  integer, start hour of each optimization window.

- flex_window_hours:

  numeric, flexibility window length (hours).

- lambda:

  numeric, ramping penalty weight on changes of `P` between consecutive
  slots.

- loss:

  numeric, standing loss of the tank's level (% per hour, \>= 0), as in
  [`get_storage_losses()`](https://resourcefully-dev.github.io/flextools/reference/get_storage_losses.md).

- Pb:

  numeric, rating of the resistive backup heater (kW electric, which is
  also its kW thermal, \>= 0). `NULL` (default) or 0 means no backup
  heater.

- on_failure:

  character, what a window the solver fails on does: `"flexible"`
  (default) runs the `flexible` schedule there, as described in *Soft
  grid capacities*, with a message; `"error"` stops with an error naming
  the window's first `datetime` and the solver status.

## Value

numeric vector, the compressor's electric power (kW), one value per
`opt_data` row, rounded to 2 decimals: the optimised power in optimised
windows and `flexible` everywhere else, reduced where it would heat the
tank above `SOCmax`. Attributes:

- `backup_kw`: numeric vector, one value per row, rounded to 2 decimals:
  the backup heater's power (kW electric = kW thermal); 0 outside
  optimised windows and everywhere without a backup heater.

- `unmet_heat_kw`: numeric vector, one value per row, rounded to 2
  decimals: the unmet heat (kW thermal, not electricity): the heat that
  would have been required in that slot to keep the tank at least at its
  comfort floor (the heat it gave from below the floor, plus the demand
  it could not serve at the inlet temperature). 0 wherever the level
  stays at or above the floor, or rises back towards it. It is a report,
  not part of the balance.

The heat pump's electric load is the power plus `attr(, "backup_kw")`.

## Details

Per slot of `dt` hours, with `S` the level in kWh thermal above the
inlet water temperature (level 0, `SOC = 0`) and
`k = (1 - loss/100)^dt`:

- `S[t] = k * S[t-1] + (cop[t] * P[t] + B[t] - heat_demand[t]) * dt`

- `Tcap * SOCmin/100 <= S[t] <= Tcap * SOCmax/100`

- `0 <= P[t] <= Pc`, and `P[t] = 0` where `cop[t] = 0`

- `0 <= B[t] <= Pb`, also where `cop[t] = 0`

- every window ends at least as full as it started.

`B` (the backup heater) is a variable only with `Pb > 0`. The comfort
floor `SOCmin` is **soft**: when the compressor and the backup heater
cannot hold it, the tank keeps giving heat below it, cooling down to the
inlet temperature (level 0) at most, as a real tank would. The **unmet
heat** is the heat that would have been required to keep the tank at
least at its comfort floor: the heat it gives from below the floor, plus
the demand it cannot serve at all once it is at the inlet temperature.
It is positive while the level falls below the floor and 0 while it
recovers, so it is a missing energy, not a measure of how long or how
far the tank stays below its floor. Every window therefore has a
schedule.

The run starts from `Tcap * SOCini/100`, and the level carries from one
window to the next. The windows are those of the other optimization
functions (one every `window_days` from `window_start_hour`), plus the
partial windows the run cuts at both ends: when the first slot is not a
window start, the slots up to the first window start form a **leading
window**, and the slots after the last full window a **trailing
window**, both optimised like any other window (the leading one from
`SOCini`, both with the end-of-window rule at their last slot). Only the
first `flex_window_hours` of every window are optimised, so with
`flex_window_hours = 24 * window_days` every slot is optimised. The
slots past `flex_window_hours` run the `flexible` schedule, without
backup heater.

## Compressor first, backup heater second, unmet heat last

The backup heater's electricity is part of the load the objective sees,
like the compressor's. To use it only after the compressor, and to cool
the tank below its floor only after both, the objective prices them from
the window's **heat price** `h`: the highest marginal value a kW of
electric load can take in the window's objective, divided by the lowest
positive COP when it is below 1, so that no kWh of compressor heat is
worth more than `h`:

- `"cost"`: the highest absolute import or export price of the window
  (EUR/kWh);

- `"grid"`: the largest derivative of the squared net flow,
  `2 * (max|static - production| + Pc + Pb)` (kW);

- a weight `w`: `w` times the grid value (scaled as the grid term of the
  objective is) plus `1 - w` times the cost value;

- with `lambda > 0`, plus the largest derivative of the ramping penalty,
  `4 * lambda * Pc`.

The backup heater pays `2 * h` per kW on top of its electricity, so it
only runs for heat the compressor cannot deliver (it never absorbs a PV
surplus the compressor could).

When a window has no schedule that holds the band within the grid
capacities, it is solved again with a soft floor: the level may go down
to 0, and every kWh below `SOCmin` costs `20 * h / dt` per slot, more
than any kWh of heat the compressor, the backup heater or a missed grid
capacity (below) can deliver. So the tank only cools below its floor
when they cannot hold it, and is brought back above it as soon as they
can. The end condition becomes soft at the same price, so a drained tank
does not have to be refilled by the end of the window. Without a backup
heater and with a band the compressor can hold, the problem, and so the
result, is the one without any of this.

## Soft grid capacities

As for
[`add_battery_optimization()`](https://resourcefully-dev.github.io/flextools/reference/add_battery_optimization.md),
the grid capacities are soft: a window that has no schedule holding the
band within them is solved with the soft comfort floor and with a
penalised slack on each capacity. The slack is bounded by how far the
site already misses the capacity without optimisation (see
[`optimization_slack_ceiling()`](https://resourcefully-dev.github.io/flextools/reference/optimization_slack_ceiling.md)):
for import, with the heat pump running `flexible`; for export, with the
heat pump off, since a full tank cannot absorb a surplus. It costs
`10 * h` per kW (electric), between the backup heater and the comfort
floor: the solver uses the compressor and the backup heater to their
limits before it misses a capacity, and misses an import capacity before
it lets the tank cool below its floor. The heat pump off with the tank
cooling always fits these bounds, so **every window has a schedule**. A
capacity left missed is reported with a message; compare the net flow
`static + P + attr(, "backup_kw") - production` with the capacities to
find where.

The slots that run `flexible` (past `flex_window_hours`, or in a window
the solver failed on, reported with a message, unless
`on_failure = "error"` stops the run there instead) keep the tank below
its top: the compressor stops as a thermostat would, its power there
being `flexible` reduced to what keeps the level at `SOCmax`. Below the
floor they cool the tank down to the inlet temperature at most, with
unmet heat as above. So the level stays between 0 and `SOCmax`
everywhere, and
[`get_tank_level()`](https://resourcefully-dev.github.io/flextools/reference/get_tank_level.md)
reproduces it.

Inputs out of range stop with an error naming the argument; nothing is
clamped into range, and no zero profile is returned in their place.

## Examples

``` r
library(dplyr)
opt_data <- tibble(
  datetime = as.POSIXct("2024-01-01", tz = "UTC") + 3600 * 0:23,
  production = 0,
  heat_demand = c(rep(1, 17), rep(6, 3), rep(1, 4)),
  cop = 3,
  flexible = 1 / 3,
  price_imported = ifelse(0:23 < 6, 0.1, 0.3),
  price_exported = 0
)
P <- add_tank_optimization(
  opt_data, opt_objective = "cost", Tcap = 3, Pc = 1, Pb = 2
)
attr(P, "backup_kw")
#>  [1] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 2 2 2 0 0 0 0
attr(P, "unmet_heat_kw")
#>  [1] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
get_tank_level(
  P, opt_data$heat_demand, opt_data$cop, init = 3,
  backup = attr(P, "backup_kw")
)
#>  [1]  3.00  2.00  1.00  3.00  2.99  3.01  3.00  2.00  1.00  0.00 -0.01  0.01
#> [13]  0.00 -0.01  0.01  0.00  1.01  3.01  2.01  1.01  0.01  0.00 -0.01  1.00
```
