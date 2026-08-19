# Changelog

## flextools 1.5.0

- A grid capacity the battery cannot fully meet is now **concentrated**
  rather than spread: the capacity is met exactly for as many slots as
  the battery’s energy covers, and missed on the remainder. Both spend
  the same energy and leave the same volume unserved, but the previous
  behaviour - a consequence of the slack penalty being linear in the
  volume missed, so the quadratic term chose the flattest of the
  equally-priced answers - left *every* affected slot marginally over
  its capacity. `congestion_time` counts slots, not kWh, so it stayed at
  100% of the window for every battery size and only dropped once a
  battery covered the window outright. **This changes results** for the
  `grid`, `capacity` and combined objectives on windows where a capacity
  is out of reach. Runs the battery misses on *power* rather than on
  energy are left alone, since no arrangement of the energy meets the
  capacity there. The `cost` objective is unaffected: being linear, it
  already concentrated.
- Fixed: the `capacity` objective reserved exactly the overshoot volume
  as the battery’s capacity, but the SOC band is a *percentage* of that
  reserve - so only a fraction of the volume was usable around `SOCini`,
  and the capacity went unmet however much battery the caller actually
  had. At the common `SOCini = 50` a 4-hour forced export reached -63 kW
  against its -100 kW capacity, and doubling the battery changed
  nothing. The reserve is now sized against the usable SOC band.

## flextools 1.4.0

- Grid capacities are now **soft constraints** for the battery: a
  capacity the battery cannot physically reach is approached at full
  power instead of being dropped, which previously made the optimizer
  fall back to plain net-power flattening. The result is still
  guaranteed never to be worse than the profile without a battery, and
  slots that can meet their capacity stay strictly capped. Windows with
  no capacity to miss are solved exactly as before.
- Fixed: a **negative** `import_capacity` or `export_capacity` — an
  obligation to flow the other way, e.g. a congestion contract requiring
  export — produced an inconsistent variable box
  (`Col N has inconsistent bounds [0, -100]`) on the `cost` and combined
  objectives. The solve failed and the battery was disabled for the
  whole window. Capacities are now imposed on the net flow.
- Fixed: the `capacity` objective sized its curtailment from the
  one-sided imported/exported series rather than the net flow, which
  mis-sized the usable battery capacity whenever a capacity was
  negative.

## flextools 1.3.0

- Changed optimization backend from OSQP to HiGHS solver
  <https://highs.dev/>

## flextools 1.2.0

- Added V2G functions
- Introduced `timefully` package
- Function `plot_net_power` allows grid capacities in `df`

## flextools 1.1.0

- Added parallel processing
- Multiple bug fix in
  [`smart_charging()`](https://resourcefully-dev.github.io/flextools/reference/smart_charging.md)
  function and introduction of “flex types”
- Optimization functions accepting `import_capacity` and
  `export_capacity` instead of `grid_capacity`
- Improvements in
  [`plot_smart_charging()`](https://resourcefully-dev.github.io/flextools/reference/plot_smart_charging.md)
  function (e.g. `by = "FlexType"`)
- Added
  [`view_smart_charging_logs()`](https://resourcefully-dev.github.io/flextools/reference/view_smart_charging_logs.md)
  function

## flextools 1.0.0

- First release with documentation.
