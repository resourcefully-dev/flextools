# Physical flexibility envelope of a profile's responsive sessions

Shared by
[`get_setpoints()`](https://resourcefully-dev.github.io/flextools/reference/get_setpoints.md)
and
[`get_setpoints_v2g()`](https://resourcefully-dev.github.io/flextools/reference/get_setpoints_v2g.md),
so the one-way and the V2G setpoints plan within the same physical
limits:

## Usage

``` r
get_flex_envelope(
  sessions,
  dttm_seq,
  profile,
  load_capacity = Inf,
  energy_min = 1,
  soc_min = NULL
)
```

## Arguments

- sessions:

  tibble, the responsive sessions of one profile in a window

- dttm_seq:

  datetime vector of the window

- profile:

  character, the profile name

- load_capacity:

  numeric (vector), maximum flexible load in kW

- energy_min:

  numeric, share of each session's energy owed at departure

- soc_min:

  numeric or `NULL`, SoC floor of the V2G sessions (fraction)

## Value

list with the numeric vectors `LFmax`, `owed` and `reserve`, each of
`length(dttm_seq)`

## Details

- `LFmax`: the power the responsive sessions can draw at each slot, i.e.
  their nominal `Power` over their connection (the demand they would
  produce charging at nominal power for the whole connection), capped by
  `load_capacity`. `Inf` when no session is given.

- `owed`: the cumulative energy owed to the sessions that have departed
  by the end of each slot, `energy_min * Energy` per session, in the
  units of the setpoint (kW per slot, i.e. kWh divided by the slot
  length in hours). A session is due in the slot containing its
  departure, or its uncontrolled charging end when that is later (a
  session that cannot charge its energy within its connection), so its
  uncontrolled profile always meets the deadline and the original
  profile stays feasible.

- `reserve` (only with `soc_min`, else zeros): the energy the connected
  V2G sessions can still discharge at each slot,
  `(StateOfCharge - soc_min) * BatteryCapacity` per session, from its
  arrival slot up to the slot before the one it is due in, in the same
  units. It is negative for a session that arrives below the SoC floor,
  which has to charge up to the floor first. See
  [`smart_v2g()`](https://resourcefully-dev.github.io/flextools/reference/smart_v2g.md)
  for `BatteryCapacity`.
