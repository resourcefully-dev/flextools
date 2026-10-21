# Battery capacity of a V2G session

The session's energy takes the battery from its arrival State-of-Charge
to full: `Energy / (1 - StateOfCharge)` (twice the energy at 50%).

## Usage

``` r
v2g_battery_capacity(energy, state_of_charge)
```

## Arguments

- energy:

  numeric, energy the session requires (kWh)

- state_of_charge:

  numeric, arrival State-of-Charge (fraction, \< 1)

## Value

numeric, battery capacity (kWh)
