# Set setpoints for smart V2G

The setpoint plans charge and discharge within the physical envelope of
the responsive sessions (see
[`get_flex_envelope()`](https://resourcefully-dev.github.io/flextools/reference/get_flex_envelope.md)):
at most their nominal power while connected, in both directions, and a
cumulative load that never drops below the energy owed to the departed
sessions minus what the connected ones can still discharge down to
`soc_min`.

## Usage

``` r
get_setpoints_v2g(
  sessions_window,
  opt_data,
  profiles_demand,
  opt_objective,
  lambda,
  energy_min = 1,
  energy_max = 1,
  soc_min = 0.2,
  discharge_eff = 1
)
```

## Arguments

- energy_min, energy_max:

  numeric, minimum and maximum share (between 0 and 1) of each profile's
  energy the setpoint must carry, net of discharge. See
  [`smart_v2g()`](https://resourcefully-dev.github.io/flextools/reference/smart_v2g.md).

- soc_min:

  numeric, SoC floor of the sessions. See
  [`smart_v2g()`](https://resourcefully-dev.github.io/flextools/reference/smart_v2g.md).

- discharge_eff:

  numeric, discharge efficiency of the vehicles, counted by the cost and
  combined objectives. See
  [`smart_v2g()`](https://resourcefully-dev.github.io/flextools/reference/smart_v2g.md).
