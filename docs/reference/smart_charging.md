# Smart charging algorithm

This function provides a framework to simulate different smart charging
methods (i.e. postpone, interrupt and curtail) to reach multiple goals
(e.g. grid congestion, net power minimization, cost minimization). See
implementation examples and the formulation of the optimization problems
in the [documentation
website](https://resourcefully-dev.github.io/flextools/).

## Usage

``` r
smart_charging(
  sessions,
  opt_data,
  opt_objective,
  method,
  window_days,
  window_start_hour,
  responsive = NULL,
  power_th = 0,
  charging_power_min = 0,
  energy_min = 1,
  energy_max = 1,
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

- method:

  character, scheduling method being `"none"`, `"postpone"`, `"curtail"`
  or `"interrupt"`. If `none`, the scheduling part is skipped and the
  sessions returned in the results will be identical to the original
  parameter.

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

  numeric, minimum charging power in kW (default 0). With
  `method = "curtail"`, a session is never curtailed below it: with
  `charging_power_min = 4`, an 11 kW session can be curtailed down to 4
  kW and no further. The one exception is a session's last charging
  slot, where it may charge less than the minimum because it only needs
  that much to finish (the tail of its charge). A session whose nominal
  `Power` is below `charging_power_min` (e.g. 3.7 kW against 4 kW) is
  not flexible, whatever the `method`: it is never responsive, charges
  as uncontrolled and the setpoint carries its load as fixed.

  Up to version 1.12.0 a value below 1 was a share of the nominal power;
  it is now always kW, so `charging_power_min = 0.5` means 0.5 kW.

- energy_min:

  numeric, minimum allowed ratio (between 0 and 1) of required energy.
  Every session charges at least this share of the energy it requires.
  The scheduler never curtails, postpones or interrupts a session below
  it, and the setpoint optimization may drop energy down to it - and no
  further - when the grid capacity cannot fit `energy_max`. When even
  this minimum does not fit, the grid capacity is relaxed only as far as
  the minimum-energy profile needs, so the minimum is delivered at the
  expense of the capacity.

- energy_max:

  numeric, maximum allowed ratio (between 0 and 1) of required energy.
  Every session charges at most this share of the energy it requires,
  and the setpoint optimization targets exactly this share for the
  responsive sessions. Lower it to save energy or cost. Must be higher
  than 0 and at least `energy_min`.

- include_log:

  logical, whether to output the algorithm messages for every user
  profile and time-slot

- show_progress:

  logical, whether to output the progress bar in the console

- lambda:

  numeric, penalty on change for the flexible load.

## Value

a list with three elements: optimal setpoints (tibble), sessions
schedule (tibble) and log messages (list of character vectors, one per
window). The date-time values in the log list are in the time zone of
the `opt_data`.

A session whose connection crosses an optimization window boundary is
split into one part per window before scheduling (energy shared by
connection time), so the sessions schedule carries a `Part` column and
such a session returns several rows per part; its delivered energy is
the sum of its rows' `Energy` grouped by `Session`.

## Details

An important parameter of this function is `opt_data`, which defines the
time sequence of the smart charging algorithm and the optimization
variables. The `opt_data` parameter is directly related with the
`opt_objective` parameter. There are four different optimization
objectives implemented by this function:

- Minimize grid interaction (`opt_objective = "grid"`): minimizes the
  peak of the flexible load and the amount of imported power from the
  grid. If `production` is not found in `opt_data`, only a peak shaving
  objective will be considered.

- Minimize capacity violations (`opt_objective = "capacity"`): minimizes
  only the load slices that exceed the grid capacity limits
  (`import_capacity` or `export_capacity` in `opt_data`), leaving the
  rest of the load profile unchanged. Falls back to grid objective when
  the capacity slice is infeasible.

- Minimize the energy cost (`opt_objective = "cost"`): minimizes the
  energy cost. In this case, the columns `grid_capacity`,
  `price_imported`, `price_exported`, `price_turn_up` and
  `price_turn_down` of tibble `opt_data` are important. If these
  variables are not configured, default values of `grid_capacity = Inf`,
  `price_imported = 1`, `price_exported = 0`, `price_turn_up = 0` and
  `price_turn_down = 0` are considered to just minimize the imported
  energy.

- Combined optimization (`opt_objective` between `0` and `1`): minimizes
  both the net power peaks and energy cost.

- No optimization (`opt_objective = "none"`): this will skip
  optimization and at least one user profile name must be in an
  `opt_data` column to be considered as a setpoint for the scheduling
  algorithm, or a grid capacity variable such as `grid_capacity`,
  `import_capacity`or `export_capacity`. The user profiles that don't
  appear in `opt_data` will not be optimized.

The optimized setpoint of a profile stays within the physical envelope
of its responsive sessions: at most their nominal power while they are
connected (and `load_capacity`), and, at every time slot, at least the
`energy_min` share of the energy of the sessions that have already
departed. Energy owed to a car leaving early is therefore never
postponed into the slots of the cars that stay longer.

## Examples

``` r
# Example: we will use the example data set of charging sessions
# from the `evsim` package.

# The user profiles of this data set are `Visit` and `Worktime`,
# identified in two different time cycles `Workday` and `Weekend`.
# These two variables in the `sessions` tibble, `Profile` and `Timecycle`,
# are required for the `smart_charging` function and give more versatility
# to the smart charging context. For example, we may want to only coordinate
# `Worktime` sessions instead of all sessions.

# For this example we want the following:
# - Curtail only `Worktime` sessions, which have a responsiveness rate of
# 0.9 (i.e. 90% of Worktime users accept to postpone the session).
# - Minimize the power peak of the sessions (peak shaving)
# - Time series resolution of 15 minutes
# - Optimization window of 24 hours from 6:00AM to 6:00 AM
# - The energy charged can be reduced up to 50% of the original requirement

library(dplyr)

# Use first 50 sessions
sessions <- evsim::california_ev_sessions_profiles %>%
  slice_head(n = 50) %>%
  evsim::adapt_charging_features(time_resolution = 15)
sessions_demand <- evsim::get_demand(sessions, resolution = 15)

# Don't require any other variable than datetime, since we don't
# care about local generation (just peak shaving objective)
opt_data <- tibble(
  datetime = sessions_demand$datetime,
  production = 0
)
sc_results <- smart_charging(
  sessions, opt_data,
  opt_objective = "grid", method = "curtail",
  window_days = 1, window_start_hour = 6,
  responsive = list(Workday = list(Worktime = 0.9)),
  energy_min = 0.5
)
```
