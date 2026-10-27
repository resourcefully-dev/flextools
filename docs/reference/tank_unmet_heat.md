# Unmet heat of a schedule

Per slot, the heat that would have been required to keep the tank at
least at its comfort floor: the heat it gives from below the floor, to
the demand or to its standing loss, plus the heat demand it cannot serve
at all once it is at the inlet temperature (level 0). A gap within half
a rounding step of the power is carried to the next slots, where the
level climbing back cancels it, and counted once the gaps add up beyond
that step.

## Usage

``` r
tank_unmet_heat(power, heat_demand, cop, init, k, dt, backup, Smin, tol = 0)
```

## Arguments

- power:

  numeric vector, compressor electric power (kW)

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

- backup:

  numeric vector, backup heater power (kW, electric = thermal)

- Smin:

  numeric, comfort floor (kWh)

- tol:

  numeric, gap ignored (kWh)

## Value

numeric vector, unmet heat per slot (kW thermal), rounded to 2 decimals
