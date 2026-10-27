# Round the compressor power to 2 decimals while tracking the level

Rounding each slot on its own drifts the level: a third of a kW rounds
to 0.33 in every slot and the tank loses 0.01 kWh of heat per slot and
kW of COP. Instead each slot takes the 2-decimal power that brings the
level closest to the level the unrounded solution reaches at the end of
that slot, so the rounded level never strays further from it than half a
rounding step of heat, and never goes above the top of the band. The
backup heater is rounded on its own and the compressor makes up its
rounding; where the COP is 0 the backup heater tracks the level in the
compressor's place.

## Usage

``` r
tank_round_power(
  target,
  heat_demand,
  cop,
  init,
  k,
  dt,
  Pc,
  backup = 0,
  Pb = 0,
  power = NULL,
  Smax = Inf
)
```

## Arguments

- target:

  numeric vector, level at the end of each slot under the unrounded
  power (kWh)

- heat_demand:

  numeric vector, heat demand (kW thermal)

- cop:

  numeric vector, coefficient of performance

- init:

  numeric, level before the first slot (kWh thermal)

- k:

  numeric, per-slot retention `(1 - loss/100)^dt`

- dt:

  numeric, slot length in hours

- Pc:

  numeric, compressor cap (kW)

- backup:

  numeric vector, backup heater power (kW, electric = thermal)

- Pb:

  numeric, backup heater cap (kW), 0 without one

- power:

  numeric vector, the unrounded power (kW). Where the tank ends a slot
  empty, the demand it leaves unserved is not for the rounding to make
  up: the power there stays within a rounding step of it.

- Smax:

  numeric, top of the band (kWh): where the nearest step would take the
  level above it, the power rounds down instead

## Value

list of the rounded `power` and `backup` (kW)
