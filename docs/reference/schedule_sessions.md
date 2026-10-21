# Schedule sessions according to optimal setpoint

Schedule sessions according to optimal setpoint

## Usage

``` r
schedule_sessions(
  sessions,
  setpoint,
  method,
  power_th = 0,
  charging_power_min = 0.5,
  energy_min = 1,
  energy_max = 1,
  include_log = FALSE,
  show_progress = TRUE
)
```

## Arguments

- sessions:

  tibble, sessions data set containing the following variables:
  `"Session"`, `"ConnectionStartDateTime"`, `"ConnectionHours"`,
  `"Power"` and `"Energy"`.

  IMPORTANT: Make sure that the `sessions` `ConnectionStartDateTime` and
  `ChargingStartDateTime` are in the same time resolution than
  `setpoint$datetime`.

- setpoint:

  tibble with columns `datetime` and `setpoint`.

- method:

  character, being `"postpone"`, `"curtail"` or `"interrupt"`.

- power_th:

  numeric, power threshold (between 0 and 1) accepted from setpoint. For
  example, with `power_th = 0.1` and `setpoint = 100` for a certain time
  slot, then sessions' demand can reach a value of `110` without needing
  to schedule sessions.

- charging_power_min:

  numeric, minimum allowed ratio (between 0 and 1) of nominal power. For
  example, if `charging_power_min = 0.5` and `method = 'curtail'`,
  sessions' charging power can only be curtailed until the 50% of the
  nominal charging power (i.e. `Power` variable in `sessions` tibble).

- energy_min:

  numeric, minimum allowed ratio (between 0 and 1) of required energy. A
  session is never curtailed, postponed or interrupted below this share
  of the energy it requires, whatever the setpoint says.

- energy_max:

  numeric, maximum allowed ratio (between 0 and 1) of required energy.
  Every session stops charging once it has received this share of the
  energy it requires. Must be higher than 0 and at least `energy_min`.

- include_log:

  logical, whether to output the algorithm messages for every user
  profile and time-slot

- show_progress:

  logical, whether to output the progress bar in the console

## Value

list of three elements: `sessions` (the schedule, one row per
constant-power segment), `demand` (the scheduled power per time slot,
one column per `Profile` when the sessions carry one, else `Demand`,
summed from the exact per-slot powers) and `log`
