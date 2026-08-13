# flextools 1.4.0

* Grid capacities are now **soft constraints** for the battery: a capacity the
  battery cannot physically reach is approached at full power instead of being
  dropped, which previously made the optimizer fall back to plain net-power
  flattening. The result is still guaranteed never to be worse than the profile
  without a battery, and slots that can meet their capacity stay strictly
  capped. Windows with no capacity to miss are solved exactly as before.
* Fixed: a **negative** `import_capacity` or `export_capacity` — an obligation
  to flow the other way, e.g. a congestion contract requiring export — produced
  an inconsistent variable box (`Col N has inconsistent bounds [0, -100]`) on
  the `cost` and combined objectives. The solve failed and the battery was
  disabled for the whole window. Capacities are now imposed on the net flow.
* Fixed: the `capacity` objective sized its curtailment from the one-sided
  imported/exported series rather than the net flow, which mis-sized the usable
  battery capacity whenever a capacity was negative.

# flextools 1.3.0

* Changed optimization backend from OSQP to HiGHS solver [https://highs.dev/](https://highs.dev/)


# flextools 1.2.0

* Added V2G functions
* Introduced `timefully` package
* Function `plot_net_power` allows grid capacities in `df`


# flextools 1.1.0

* Added parallel processing
* Multiple bug fix in `smart_charging()` function and introduction of "flex types" 
* Optimization functions accepting `import_capacity` and `export_capacity` instead of `grid_capacity`
* Improvements in `plot_smart_charging()` function (e.g. `by = "FlexType"`)
* Added `view_smart_charging_logs()` function


# flextools 1.0.0

* First release with documentation.
