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

  character, optimization objective being `"none"`, `"grid"`, `"cost"`
  or a value between 0 (cost) and 1 (grid). See details section for more
  information about the different objectives.

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

a list with three elements: optimal setpoints (tibble), sessions
schedule (tibble) and log messages (list with character strings). The
date-time values in the log list are in the time zone of the `opt_data`.

## Details

An important parameter of this function is `opt_data`, which defines the
time sequence of the smart charging algorithm and the optimization
variables. The `opt_data` parameter is directly related with the
`opt_objective` parameter. There are three different optimization
objectives implemented by this function:

- Minimize grid interaction (`opt_objective = "grid"`): minimizes the
  peak of the flexible load and the amount of imported power from the
  grid. If `production` is not found in `opt_data`, only a peak shaving
  objective will be considered.

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
  algorithm. The user profiles that don't appear in `opt_data` will not
  be optimized.

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
