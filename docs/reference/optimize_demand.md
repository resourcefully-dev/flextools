# Optimize a vector of flexible demand

See the formulation of the optimization problems in the [documentation
website](https://resourcefully-dev.github.io/flextools/).

## Usage

``` r
optimize_demand(
  opt_data,
  opt_objective = "grid",
  direction = "forward",
  time_horizon = NULL,
  window_days = 1,
  window_start_hour = 0,
  flex_window_hours = NULL,
  lambda = 0
)
```

## Arguments

- opt_data:

  tibble, optimization contextual data. The first column must be named
  `datetime` (mandatory) containing the date time sequence where the
  optimization algorithm is applied.

  The second column must be named `flexible` (mandatory), being the
  power demand (in kW) vector that will be optimized.

  The other columns can be (optional):

  - `static`: static power demand (in kW) from other sectors like
    buildings, offices, etc.

  - `import_capacity`: maximum imported power from the grid (in kW), for
    example the contracted power with the energy company.

  - `export_capacity`: maximum exported power from the grid (in kW), for
    example the contracted power with the energy company.

  - `load_capacity`: maximum power that the `flexible` load can consume
    (in kW).

  - `production`: local power generation (in kW). This is used when
    `opt_objective = "grid"`.

  - `price_imported`: price for imported energy (€/kWh). This is used
    when `opt_objective = "cost"`.

  - `price_exported`: price for exported energy (€/kWh). This is used
    when `opt_objective = "cost"`.

  - `price_turn_down`: price for turn-down energy use (€/kWh). This is
    used when `opt_objective = "cost"`.

  - `price_turn_up`: price for turn-up energy use (€/kWh). This is used
    when `opt_objective = "cost"`.

- opt_objective:

  character or numeric. Optimization objective can be `"grid"` (default)
  or `"cost"`, or a number between `0` and `1` to perform combined
  optimization where `0 == "cost"` and `1 == "grid"`.

- direction:

  character, being `forward` or `backward`. The direction where energy
  can be shifted

- time_horizon:

  integer, maximum number of time slots to shift energy from. If `NULL`,
  the `time_horizon` will be the total optimization window length.

- window_days:

  integer, number of days to consider as optimization window.

- window_start_hour:

  integer, starting hour of the optimization window.

- flex_window_hours:

  integer, flexibility window length, in hours. This optional feature
  lets you apply flexibility only during few hours from the
  `window_start_hour`. It must be lower than `window_days*24` hours.

- lambda:

  numeric, penalty on change for the flexible load.

## Value

numeric vector
