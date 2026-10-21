# Set `Responsive` column in `sessions`

Set `Responsive` column in `sessions`

## Usage

``` r
set_responsive(
  sessions_window,
  dttm_seq,
  responsive,
  time_resolution,
  charging_power_min = 0
)
```

## Arguments

- sessions_window:

  tibble, sessions corresponding to a single windows

- dttm_seq:

  datetime vector

- responsive:

  named list with responsive ratios

- time_resolution:

  numeric, time resolution in minutes

- charging_power_min:

  numeric, minimum charging (and discharging) power in kW. A session
  whose nominal `Power` is below it cannot be curtailed or discharged
  within that minimum, so it is never responsive (`Responsive = FALSE`):
  it charges as uncontrolled and the setpoint carries its load as fixed.
