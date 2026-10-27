# Minimisation of the energy cost with bidirectional flexibility (single window)

The V2G counterpart of
[`demand_cost_window()`](https://resourcefully-dev.github.io/flextools/reference/demand_cost_window.md):
the optimized load `O` may be negative (discharge), within the same
bounds as
[`demand_grid_v2g_window()`](https://resourcefully-dev.github.io/flextools/reference/demand_grid_v2g_window.md)
(`get_bounds_v2g()`), and the site's import `I` and export `E` are
priced at `PI` and `PE`. A binary grid mode keeps the site from
importing and exporting in the same slot (when `lambda = 0`; otherwise
the problem is a QP with the export price clipped to the import price,
as in
[`demand_cost_window()`](https://resourcefully-dev.github.io/flextools/reference/demand_cost_window.md)).

## Usage

``` r
demand_cost_v2g_window(
  G,
  LF,
  LS,
  PI,
  PE,
  PTD,
  PTU,
  direction,
  time_horizon,
  LFmax,
  import_capacity,
  export_capacity,
  lambda = 0,
  energy_ratio = c(1, 1),
  cumsum_min = NULL,
  LFmin = NULL,
  discharge_eff = 1
)
```

## Arguments

- G:

  numeric vector, being the renewable generation power profile

- LF:

  numeric vector, being the flexible load power profile

- LS:

  numeric vector, being the static load power profile

- PI:

  numeric vector, electricity prices for imported energy

- PE:

  numeric vector, electricity prices for exported energy

- PTD:

  numeric vector, prices for turn-down energy use

- PTU:

  numeric vector, prices for turn-up energy use

- direction:

  character, being `forward` or `backward`. The direction where energy
  can be shifted

- time_horizon:

  integer, maximum number of positions to shift energy from

- LFmax:

  numeric, value of maximum power (in kW) of the flexible load `LF`

- import_capacity:

  numeric or numeric vector, grid maximum import capacity that will
  limit the maximum optimized demand

- export_capacity:

  numeric or numeric vector, grid maximum export capacity that will
  limit the maximum optimized demand

- lambda:

  numeric, penalty on the change of the optimized load between slots,
  `lambda * sum((O_t - O_{t-1})^2)`, as in
  [`demand_cost_window()`](https://resourcefully-dev.github.io/flextools/reference/demand_cost_window.md).

- energy_ratio:

  numeric vector `c(min, max)`, share of the flexible load's energy the
  optimized load must carry. `c(1, 1)` (default) preserves the energy
  exactly; a range lets the optimizer drop energy — down to `min` — when
  the grid capacity cannot fit `max`.

- cumsum_min:

  numeric vector or `NULL`, minimum cumulative optimized load at each
  slot (in the units of `LF`): the energy owed to the sessions that have
  departed by then (see
  [`get_flex_envelope()`](https://resourcefully-dev.github.io/flextools/reference/get_flex_envelope.md)).
  Only applies to `direction = "forward"`, and is capped at `cumsum(LF)`
  so the original profile stays feasible.

- LFmin:

  numeric vector or `NULL` (default), per-slot floor of the optimized
  load. See
  [`demand_grid_v2g_window()`](https://resourcefully-dev.github.io/flextools/reference/demand_grid_v2g_window.md).

- discharge_eff:

  numeric, discharge efficiency (0, 1\] of the vehicles. Below 1, every
  kWh discharged must be charged back as `1 / discharge_eff` kWh, as the
  scheduler counts it, so a discharge pays only when the price spread
  covers the losses (see `solve_optimization_window_v2g()`).

## Value

numeric vector, the optimized load

## Details

No discharge floor
([`v2g_discharge_floor()`](https://resourcefully-dev.github.io/flextools/reference/v2g_discharge_floor.md))
applies by default: the fleet discharges into export when the export
price pays for the energy charged back later, which is the economic case
of V2G. A fourth block of variables, the fleet's net discharge
`D >= max(-O, 0)`, carries a tiny cost so that a discharge that earns
nothing (a tie, e.g. discharging into a flat export price to charge in
another exporting slot) is never chosen.
