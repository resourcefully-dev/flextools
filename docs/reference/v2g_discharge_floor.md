# Discharge floor of the V2G optimized load

Under the objectives with a squared net-power term (`"grid"`, combined
and `"capacity"`), discharge only serves to cut import, or to meet a
negative import capacity: in a slot where the site already exports, a
discharge only adds export. The floor lets the fleet discharge at most
down to a net power of `min(import_capacity, 0)`, so not at all in a
slot where the site exports without it (under a non-negative import
capacity). Without it the minimum-squared-net-power LP trades a shallow
export for a deeper one (discharging into the first to charge more in
the second), which raises the export and can push it over
`export_capacity`.

## Usage

``` r
v2g_discharge_floor(G, LS, import_capacity)
```

## Arguments

- G, LS, import_capacity:

  numeric vectors, production, static load and import capacity of the
  slots.

## Value

numeric vector, the per-slot floor (`<= 0`) of the optimized load.

## Details

The floor is always `<= 0 <= LF`, so the original profile stays a
feasible point for the fallbacks of `solve_optimization_window_v2g()`.

The `"cost"` objective has no floor: there, discharging into export
earns the export price, and the fleet does it when that pays for the
energy charged back later (see
[`demand_cost_v2g_window()`](https://resourcefully-dev.github.io/flextools/reference/demand_cost_v2g_window.md)).
