# Battery optimal charging/discharging profile to minimize net power and cost (just a window)

Battery optimal charging/discharging profile to minimize net power and
cost (just a window)

## Usage

``` r
optimize_battery_window(
  G,
  L,
  PE,
  PI,
  PTD,
  PTU,
  Bcap,
  Bc,
  Bd,
  SOCmin,
  SOCmax,
  SOCini,
  import_capacity,
  export_capacity,
  w,
  lambda,
  charge_eff,
  discharge_eff
)
```

## Arguments

- G:

  numeric vector, being the renewable generation profile

- L:

  numeric vector, being the load profile

- PE:

  numeric vector, electricity prices for exported energy

- PI:

  numeric vector, electricity prices for imported energy

- PTD:

  numeric vector, prices for turn-down energy use

- PTU:

  numeric vector, prices for turn-up energy use

- Bcap:

  numeric, capacity of the battery (NOT in kWh but in energy units
  according to time resolution)

- Bc:

  numeric, maximum charging power (in kW)

- Bd:

  numeric, maximum discharging power (in kW)

- SOCmin:

  numeric, minimum State-of-Charge of the battery

- SOCmax:

  numeric, maximum State-of-Charge of the battery

- SOCini:

  numeric, required State-of-Charge at the beginning/end of optimization
  window

- import_capacity:

  numeric vector, grid maximum import power capacity that will limit the
  maximum charging power

- export_capacity:

  numeric vector, grid maximum export power capacity that will limit the
  maximum discharging power

- w:

  numeric, optimization objective weight (`w=1` minimizes net power
  while `w=0` minimizes cost)

- lambda:

  numeric, penalty on change for the flexible load

- charge_eff:

  numeric, battery charging efficiency (from 0 to 1)

- discharge_eff:

  numeric, battery discharging efficiency (from 0 to 1)

## Value

numeric vector
