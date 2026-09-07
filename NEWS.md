# flextools 1.8.0

* `smart_charging()` now **splits every session whose connection crosses an
  optimization window boundary** into one part per window before scheduling,
  with the energy shared by connection time. Previously such a session belonged
  to the window it started in, was never marked responsive there because its
  nominal charging ended past the window, and charged unmanaged at full power
  — with its raw demand added back into the setpoint, so the setpoint itself
  exceeded the grid capacity. Its flexibility, all of it on the far side of the
  boundary, was never used. Now each part is scheduled in the window it is
  connected in; the sessions schedule gains a `Part` column and a split session
  returns rows for each part under the same `Session` id (delivered energy is
  still `sum(Energy)` by `Session`). A part that could not fill one time slot
  at nominal power is folded into its longest neighbour instead of being
  emitted — `evsim::get_demand()` renders sub-slot charging as a whole slot at
  nominal power, which would inflate the optimizer's view of it — so a session
  arriving shortly before a boundary moves whole into the next window.
  **This changes results** for any fleet with sessions spanning a window
  boundary; fleets without such sessions are unaffected (the package's own
  golden test was re-captured for the two scenarios where the California
  fixture crosses the 06:00 boundary; the rest is byte-identical to 1.6.0).
* The responsiveness test "charging ends inside the window" compared against
  the window's last slot instead of its end, so a session whose charging ended
  exactly on the next boundary was excluded. Fixed.
* **The 95% arrival-time band is gone.** Sessions whose connection times fell
  outside the profile's `mean +- 2 sd` band in a window were "not considered":
  never scheduled, charged unmanaged at full power, and not counted in the
  responsive share. The band existed to stop one late arrival from stretching
  the setpoint span and smearing the profile's energy into slots where only
  that car was plugged in; the setpoint LP now bounds every slot by the nominal
  power of the sessions actually connected, which removes that failure mode at
  the source. Every session that charges inside a window is now considered.
  `set_responsive()` loses its `opt_objective` argument. **This changes
  results** for fleets with unusual arrivals.
* Responsiveness is now looked up per session by its own `Timecycle` and
  `Profile`, instead of by the window's most common time cycle. A Friday
  session carried over into Saturday's window keeps Friday's responsiveness;
  before, it was looked up under Saturday and, when that profile was not
  configured for Saturday, left unscheduled.
* Fixed: with a single eligible session in a window, the responsive draw used
  `sample(idx, 1)`, which samples from `1:idx` rather than returning `idx`, so
  the wrong row could be marked responsive.
* **A met capacity no longer comes back one cent over it.** Three roundings
  to 2 decimals stacked up so that a setpoint of 3.00 kW met exactly was
  reported as 3.01 (33 of 39 over-cap slots on the reference fleet): the
  flexibility requirement was rounded before the decision to act, the window
  demand was rebuilt from the session rows through `evsim::get_demand()`,
  which rounds each session's slot power, and each profile column of the
  returned demand was rounded separately while callers sum the columns.
  Decisions are now taken on exact arithmetic against named tolerances
  (`SCHEDULE_FLEX_TOL_KW = 0.001`, the unchanged 0.1 kW / 0.025 kWh
  flexibility thresholds); `schedule_sessions()` returns the scheduled demand
  per slot (`demand`, summed from the exact per-slot powers) and
  `smart_charging_window()` uses it instead of rebuilding it; the session rows
  and the demand profile carry four decimals. The demand of non-responsive
  sessions still comes from `evsim::get_demand()`.
* Fixed a latent energy loss for straddling sessions: the window they spilled
  into overwrote the profile's demand with the demand of its own sessions only,
  so the tail of a straddler vanished whenever another session of the same
  profile was scheduled that day. No session spills any more.

# flextools 1.7.1

* Fixed: with `energy_min = 0`, two fallback paths of the 1.7.0 energy range
  built a `c(0, 0)` ratio for a follow-up LP, which then stopped with "the
  maximum energy ratio must be higher than 0" — once when the capacity slice
  LP removed EV energy and could re-add none of it (the caps were already full
  of static load), and once when a window was infeasible for a reason energy
  cannot fix (production above the export capacity, load capacity). The first
  now returns the profile with the energy simply dropped; the second falls back
  to the original-profile capacity relaxation, as 1.6.0 did, after the
  minimum-energy relaxation has been tried where it exists. `smart_charging()`
  with `energy_min = 0` no longer errors on such windows.

# flextools 1.7.0

* New `energy_max` argument in `smart_charging()`, `smart_charging_window()`
  and `schedule_sessions()`: the maximum share (between 0 and 1) of the energy
  each session requires that may be charged. Every session stops at
  `energy_max * Energy`, and the setpoint optimization targets exactly that
  share for the responsive sessions, so the setpoint and the scheduled demand
  agree — also with `method = "none"`, where the setpoint is the result. Default
  `1`. Must be at least `energy_min`.
* `energy_min` now also bounds the **setpoint optimization**, not only the
  scheduler. The energy-conservation row of every demand LP (`grid`, `cost`,
  `capacity`, combined) becomes a range `[energy_min, energy_max] * sum(LF)`
  with a uniform reward on the optimized load that dominates the objective's
  marginal gain from dropping energy, so the optimizer keeps as much energy as
  the grid capacity admits and drops the least that restores feasibility — never
  below `energy_min`. **This changes results for callers passing
  `energy_min < 1`**: against a capacity the fleet cannot fit under, the
  setpoint now holds the capacity and sheds energy, where before it conserved
  the energy and relaxed the capacity; and where the capacity does not bind,
  the energy is still delivered in full but a linear objective with a flat
  price (the `cost` objective is a tie among many equal-cost profiles) may land
  on a different profile than before. At the defaults (`energy_min = 1`,
  `energy_max = 1`) the setpoints are identical to 1.6.0 and the scheduled
  demand is unchanged up to the scheduler's own 2-decimal rounding.
* The infeasible-window fallback follows the same order: energy first,
  capacity second. When even `energy_min * sum(LF)` does not fit, the grid
  capacity is relaxed only as far as the *minimum-energy* profile needs
  (previously: as far as the full original profile needs), with its own
  warning message. The `opt_objective = "none"` branch of `get_setpoints()`
  likewise inflates the available capacity only as far as `energy_min`
  requires (previously: always enough for 100% of the energy).
* `smart_charging()` and `schedule_sessions()` now validate the ratios and stop
  on `energy_min > energy_max`, `energy_max = 0` or values outside `[0, 1]`.
* Not touched: `smart_v2g()` keeps its own setpoint path and has no
  `energy_max`; V2G is still under development.

# flextools 1.6.0

* The battery `capacity` objective now **minimizes battery usage** subject to
  the grid capacity, instead of minimizing the net grid flow with a
  capacity-sized slice of the battery. It keeps every constraint of the net
  power formulation and changes only the objective, from
  `sum((L + B - G)^2)` to `sum(B^2)`. **This changes results** for
  `opt_objective = "capacity"`: the battery now discharges onto the capacity
  line rather than below it, cycles only the overshoot volume, and recharges at
  the lowest power the window allows.

  The previous formulation reserved a slice of the battery sized to the
  overshoot and ran the net power objective on it, so the reserve was the only
  thing holding the optimizer back from flattening. Sizing that reserve against
  the usable SOC band in 1.5.0 - necessary, since a reserve too small could not
  clear the window at all - roughly doubled it, and the flattening it had been
  restraining became plainly visible: on a 4000 kW peak against a 3400 kW
  capacity the battery pulled the profile down to 2788 kW and cycled twice the
  energy the overshoot needed. Minimizing the battery directly removes the need
  for a reserve, so the nameplate no longer decides how hard the battery works -
  the capacity does, and two batteries that can both clear a window now return
  the same profile. Use `"grid"` to minimize the peak itself.
* Fixed: the slack penalty was derived from the grid capacities rather than from
  the flows a solution can actually reach, so a capacity standing in for
  "unlimited" as a large finite number (rather than `Inf`, which is filtered
  out) produced a penalty large enough to wreck the solver's conditioning -
  OSQP returned no solution and the battery was disabled for the whole window.
  It is now derived from `max|L - G| + max(Bc, Bd)`, which bounds both
  objectives' gradients and is tighter than the capacities in the common case.
* Fixed: a battery window that fell back to the heuristic or to a disabled
  battery reported an empty solver status, because the message read
  `result$info$status` where `solve_osqp()` stores `result$status_message`.

# flextools 1.5.0

* A grid capacity the battery cannot fully meet is now **concentrated** rather
  than spread: the capacity is met exactly for as many slots as the battery's
  energy covers, and missed on the remainder. Both spend the same energy and
  leave the same volume unserved, but the previous behaviour - a consequence of
  the slack penalty being linear in the volume missed, so the quadratic term
  chose the flattest of the equally-priced answers - left *every* affected slot
  marginally over its capacity. `congestion_time` counts slots, not kWh, so it
  stayed at 100% of the window for every battery size and only dropped once a
  battery covered the window outright. **This changes results** for the `grid`,
  `capacity` and combined objectives on windows where a capacity is out of
  reach. Runs the battery misses on *power* rather than on energy are left
  alone, since no arrangement of the energy meets the capacity there. The `cost`
  objective is unaffected: being linear, it already concentrated.
* Fixed: the `capacity` objective reserved exactly the overshoot volume as the
  battery's capacity, but the SOC band is a *percentage* of that reserve - so
  only a fraction of the volume was usable around `SOCini`, and the capacity
  went unmet however much battery the caller actually had. At the common
  `SOCini = 50` a 4-hour forced export reached -63 kW against its -100 kW
  capacity, and doubling the battery changed nothing. The reserve is now sized
  against the usable SOC band.

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
