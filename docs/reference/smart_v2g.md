# Smart V2G algorithm

Prototype vehicle-to-grid simulation built on top of
[`smart_charging()`](https://resourcefully-dev.github.io/flextools/reference/smart_charging.md)
helpers. The current implementation mirrors the classic workflow
(responsiveness filtering, setpoint definition and scheduling
placeholder) but it isolates the new optimisation pieces so the existing
API remains untouched.

## Usage

``` r
smart_v2g(
  sessions,
  opt_data,
  opt_objective = "grid",
  window_days,
  window_start_hour,
  responsive = NULL,
  power_th = 0,
  charging_power_min = 0,
  energy_min = 1,
  energy_max = 1,
  discharge_eff = 1,
  include_log = FALSE,
  show_progress = FALSE,
  lambda = 0,
  soc_min = 0.2
)
```

## Arguments

- sessions:

  tibble, sessions data set containig the following variables:
  `"Session"`, `"Timecycle"`, `"Profile"`, `"ConnectionStartDateTime"`,
  `"ConnectionHours"`, `"Power"` and `"Energy"`.

- opt_data:

  tibble, optimization contextual data. The first column must be named
  `datetime` (mandatory) containing the date time sequence where the
  smart charging algorithm is applied, so only sessions starting within
  the time sequence of column `datetime` will be optimized. The other
  columns can be:

  - `static`: static power demand (in kW) from other sectors like
    buildings, offices, etc.

  - `import_capacity`: maximum imported power from the grid (in kW), for
    example the contracted power with the energy company.

  - `export_capacity`: maximum exported power from the grid (in kW), for
    example the contracted power with the energy company.

  - `load_capacity`: maximum charging power (in kW) allowed for the
    flexible EV demand at every time slot. By default the algorithm
    already limits the optimized setpoint to the nominal power of the
    connected EVs (the physical charging envelope). This variable can be
    used to tighten that limit further (e.g. a charging point or
    transformer power limit).

  - `production`: local power generation (in kW). This is used when
    `opt_objective = "grid"`.

  - `price_imported`: price for imported energy (Euro/kWh). This is used
    when `opt_objective = "cost"`.

  - `price_exported`: price for exported energy (Euro/kWh). This is used
    when `opt_objective = "cost"`.

  - `price_turn_down`: price for turn-down energy use (Euro/kWh). This
    is used when `opt_objective = "cost"`.

  - `price_turn_up`: price for turn-up energy use (Euro/kWh). This is
    used when `opt_objective = "cost"`.

  If columns of `opt_data` are user profiles names, these are used as
  setpoints and no optimization is performed for the corresponding user
  profiles.

- opt_objective:

  character, optimization objective being `"none"`, `"grid"`, `"cost"`,
  `"capacity"` or a value between 0 (cost) and 1 (grid). See details
  section for more information about the different objectives.

- window_days:

  integer, number of days to consider as optimization window.

- window_start_hour:

  integer, starting hour of the optimization window.

- responsive:

  Named two-level list with the ratio (between 0 and 1) of sessions
  responsive to smart charging program. The names of the list must
  exactly match the Time-cycle and User profiles names. For example:
  `list(Monday = list(Worktime = 1, Shortstay = 0.1))`

- power_th:

  numeric, power threshold (between 0 and 1) accepted from setpoint. For
  example, with `power_th = 0.1` and `setpoint = 100` for a certain time
  slot, then sessions' demand can reach a value of `110` without needing
  to schedule sessions.

- charging_power_min:

  numeric, minimum power in kW (default 0), for charging and for
  discharging alike: a responsive session charges at
  `charging_power_min` or more, discharges at `charging_power_min` or
  more (e.g. "discharge only from 4 kW upwards"), or does nothing. When
  the SoC floor or the energy deadline would only allow a smaller
  discharge (say -2 kW against a 4 kW minimum), the session does not
  discharge in that slot. The one exception is a session's last charging
  slot, where it may charge less than the minimum because it only needs
  that much to finish (the tail of its charge). A session whose nominal
  `Power` is below `charging_power_min` is not flexible: it is never
  responsive, charges as uncontrolled, never discharges, and the
  setpoint carries its load as fixed. Up to version 1.12.0 a value below
  1 was a share of the nominal power; it is now always kW.

- energy_min:

  numeric, minimum allowed ratio (between 0 and 1) of required energy.
  Every session charges at least this share of the energy it requires,
  net of discharge. The scheduler never discharges or curtails a session
  below it, and the setpoint optimization may drop energy down to it -
  and no further - when the grid capacity cannot fit `energy_max`. When
  even this minimum does not fit, the grid capacity is relaxed only as
  far as the minimum-energy profile needs.

- energy_max:

  numeric, maximum allowed ratio (between 0 and 1) of required energy.
  Every session charges at most this share of the energy it requires,
  net of discharge (energy discharged, grossed up by `discharge_eff`,
  has to be charged back on top), and the setpoint optimization targets
  this share for the responsive sessions. Must be higher than 0 and at
  least `energy_min`.

- discharge_eff:

  numeric, efficiency (0, 1\] of a discharge, counted as the round trip
  grid -\> battery -\> grid. For every kWh a session discharges to the
  grid, its battery loses `1 / discharge_eff` kWh, which it charges back
  from the grid before it leaves (0.9: 1.11 kWh). The sessions' `Energy`
  and `Power` are grid-side, as metered at the charge point, so they
  already include the charging losses of a normal charge, and there is
  no `charge_eff`. The losses of charging back the discharged energy are
  not separate either: pass the round-trip efficiency (about 0.9 for 95%
  each way) to count them, which also makes the SoC floor slightly
  conservative, because the battery bookkeeping counts grid-side kWh.
  The scheduler sizes each discharge so the session still meets its
  energy deadline and its SoC floor after the losses, and the `"cost"`
  and combined setpoints plan a discharge only when the price spread
  covers them.

- include_log:

  logical, whether to output the algorithm messages for every user
  profile and time-slot

- show_progress:

  logical, whether to output the progress bar in the console

- lambda:

  numeric, penalty on change for the flexible load.

- soc_min:

  numeric, State-of-Charge floor of every V2G session, as a fraction of
  its battery capacity (default 0.2, `0 <= soc_min < 1`). A session
  never discharges below it. A session that arrives below it
  (`StateOfCharge < soc_min`) charges up to the floor first, as fast as
  it needs to (at nominal power when necessary), and can neither be
  curtailed nor discharge before it reaches the floor.

## Value

list with setpoints, sessions and demand similar to
[`smart_charging()`](https://resourcefully-dev.github.io/flextools/reference/smart_charging.md).
Scheduling is still charging-only while the V2G-specific session logic
is under development.

## Details

The `sessions` tibble can carry a `StateOfCharge` column, the
State-of-Charge of each vehicle at arrival as a fraction
(`0 <= StateOfCharge < 1`); when it is absent every session arrives at
0.5. Each session's `Energy` takes its battery from `StateOfCharge` to
full, so its battery capacity is `Energy / (1 - StateOfCharge)` (twice
the energy at 0.5). With `energy_max` below 1 the session charges only
that share of its energy and leaves below full, at
`StateOfCharge + energy_max * (1 - StateOfCharge)`.

The setpoint plans charge and discharge only while the responsive
sessions are connected, within their nominal power in both directions
(and `load_capacity`), and keeps the energy owed to every session by its
departure: the cumulative flexible load never drops below the
`energy_min` share of the energy of the sessions that have left, minus
what the connected sessions can still discharge down to `soc_min`.

The `opt_objective` shapes that plan as in
[`smart_charging()`](https://resourcefully-dev.github.io/flextools/reference/smart_charging.md):

- `"grid"` minimises the squared net power of the site. The fleet
  discharges only to cut import (or to meet a negative
  `import_capacity`), never into a slot where the site already exports.

- `"cost"` minimises the energy cost of the site,
  `sum(import * price_imported - export * price_exported)` (plus the
  turn-up and turn-down prices, as in
  [`smart_charging()`](https://resourcefully-dev.github.io/flextools/reference/smart_charging.md)).
  The fleet charges in the cheap slots and discharges in the expensive
  ones, also into export when the export price pays for the energy
  charged back later (an evening export price above the night's import
  price); it never discharges into export that earns nothing. The import
  and export capacities still bind.

- A number `w` between 0 and 1 blends the two: `w` weighs the squared
  net power and `1 - w` the cost, as in
  [`smart_charging()`](https://resourcefully-dev.github.io/flextools/reference/smart_charging.md).
  `w = 1` is `"grid"` and `w = 0` is `"cost"`; in between the fleet
  discharges only to cut import, as under `"grid"`.

- `"capacity"` keeps the site within its grid capacities, discharging
  only to cut import.

Under `"cost"` and combined objectives the setpoint counts the discharge
losses (`discharge_eff`): every discharged kWh is charged back as
`1 / discharge_eff` kWh, so a discharge is planned only when the price
spread covers the losses. The `"grid"` and `"capacity"` setpoints, which
discharge only to cut import, do not count them; the scheduler charges
the losses back on top under every objective.
