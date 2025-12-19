# Time-series profiles for consumption, production and energy prices

A tibble with time-series profiles for solar production, building
consumption and energy prices

## Usage

``` r
energy_profiles
```

## Format

A tibble

- solar:

  Solar PV production (in kW)

- building:

  Consumption profile of a building (in kW)

- price_imported:

  Imported energy price (in €/kWh)

- price_exported:

  Exported energy price (in €/kWh)

- price_turn_up:

  Balancing price for increasing demand (in €/kWh)

- price_turn_down:

  Balancing price for decreasing demand (in €/kWh)
