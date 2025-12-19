# Set setpoints for smart charging

Set setpoints for smart charging

## Usage

``` r
smart_charging_window(
  sessions_window,
  profiles_demand,
  setpoints,
  method,
  power_th = 0,
  charging_power_min = 0,
  energy_min = 1,
  include_log = FALSE
)
```

## Arguments

- sessions_window:

  tibble, sessions corresponding to a single windows

- profiles_demand:

  tibble, user profiles power demand

- setpoints:

  tibble, user profiles power setpoints

- method:

  character, scheduling method being `"none"`, `"postpone"`, `"curtail"`
  or `"interrupt"`. If `none`, the scheduling part is skipped and the
  sessions returned in the results will be identical to the original
  parameter.

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
