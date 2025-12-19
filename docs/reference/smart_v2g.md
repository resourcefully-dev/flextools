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
  include_log = FALSE,
  show_progress = FALSE,
  lambda = 0
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

  character, optimisation objective being `"none"`, `"grid"`, `"cost"`
  or a value between 0 (cost) and 1 (grid). Only the `"grid"` mode is
  currently supported by the V2G prototype.

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

  numeric. It can be configured in two ways: (1) minimum allowed ratio
  (between 0 and 1) of nominal power (i.e. `Power` column in
  `sessions`), or (2) specific value of minimum power (in kW) higher
  than 1 kW.

  For example, if `charging_power_min = 0.5` and `method = 'curtail'`,
  sessions' charging power can only be curtailed until the 50% of the
  nominal charging power. And if `charging_power_min = 2`, sessions'
  charging power can be curtailed until 2 kW.

- energy_min:

  numeric, minimum allowed ratio (between 0 and 1) of required energy.

- include_log:

  logical, whether to output the algorithm messages for every user
  profile and time-slot

- show_progress:

  logical, whether to output the progress bar in the console

- lambda:

  numeric, penalty on change for the flexible load.

## Value

list with setpoints, sessions and demand similar to
[`smart_charging()`](https://resourcefully-dev.github.io/flextools/reference/smart_charging.md).
Scheduling is still charging-only while the V2G-specific session logic
is under development.
