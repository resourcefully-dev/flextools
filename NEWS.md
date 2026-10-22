# flextools 1.14.0

* `smart_v2g(opt_objective = "cost")` now minimises the energy cost of the
  site, `sum(import * price_imported - export * price_exported)` (plus the
  turn-up and turn-down prices, as in `smart_charging()`), with charge and
  discharge. Until now it reused the `"grid"` objective with the message
  "Cost-based optimisation is not yet implemented for V2G". The fleet charges
  in the cheap slots and discharges in the expensive ones, within the same
  constraints as `"grid"`: the power of the connected vehicles, their
  departure deadlines and SoC floor, `energy_min`/`energy_max`, and the import
  and export capacities, with the same fallbacks when they cannot all be met.
  As in `smart_charging()`, the site never imports and exports in the same
  slot.
* Under `"cost"` the fleet may discharge into export, which 1.13.1 forbade
  for every objective: it does when the export price pays for the energy
  charged back later (an evening export price above the night's import
  price), and never when the export earns nothing (a tiny tie-break price on
  the discharge and the export makes it cut import first). With a fleet that
  can discharge more than the site's evening load and an export price equal
  to the import price, this lowers the energy cost of the test case from
  13.97 to -0.90; with an export price below every import price the fleet
  still only cuts import. `"grid"`, `"capacity"` and combined objectives keep
  the discharge floor of 1.13.1.
* Under `"cost"` and combined objectives the setpoint counts the discharge
  losses: every kWh discharged is charged back as `1 / discharge_eff` kWh, as
  the scheduler counts it, so the fleet discharges only when the price spread
  covers the losses. With a flat export price of 0.15 against a night import
  price of 0.10, the cost setpoint exports at `discharge_eff = 0.8`
  (0.15 x 0.8 > 0.10) and not at 0.6. The setpoint plans without the losses
  first, then re-solves with them in the slots that discharge, which keeps
  the problem convex without letting the solver invent losses to charge more
  energy. The `"grid"` and `"capacity"` setpoints are unchanged.
* Fix: with `discharge_eff` below 1, the V2G scheduler let a session
  discharge as much as its energy deadline and SoC floor allowed in grid-side
  kWh, but every kWh discharged costs `1 / discharge_eff` kWh of battery and
  of energy to charge back. A session that discharged up to that limit left
  short of its energy (19.25 of 22 kWh at `discharge_eff = 0.5`) or went
  below its SoC floor. The allowed discharge is now scaled by
  `discharge_eff`.
* `discharge_eff` is documented as the round-trip efficiency (grid -> battery
  -> grid): the sessions' `Energy` is grid-side, as metered at the charge
  point, so it already includes the charging losses, which is why there is no
  `charge_eff`.
* A numeric `opt_objective` (`w` between 0 and 1) now blends the two
  objectives in `smart_v2g()` as in `smart_charging()`: `w` weighs the squared
  net power (normalised by the squared mean import price) and `1 - w` the
  cost. `w = 1` gives the `"grid"` results and `w = 0` the `"cost"` results.
  It used to reuse the `"grid"` objective with the message "Combined
  optimisation is not yet implemented for V2G".
* Under `"cost"` and combined objectives, `lambda` penalises the change of
  the setpoint between slots, as in `smart_charging()`; under `"grid"` it
  still penalises the deviation from the original profile.

# flextools 1.13.1

* `smart_v2g()` no longer discharges a vehicle in a slot where the site
  already exports, whatever the `opt_objective`. The setpoint places the
  energy by minimising the squared net power (directly for `"grid"`, and for
  `"cost"` and combined objectives, which reuse it; for the shifted energy
  under `"capacity"`), which traded a shallow export for a deeper one: it
  discharged into the morning's PV surplus to charge more at noon, adding
  export and, next to an export limit, export above `export_capacity`. The
  fleet now discharges only to cut import, down to a net power of 0 (or to a
  negative `import_capacity`, which asks the site to export), so its export
  breaches are never worse than those of uncontrolled charging.

# flextools 1.13.0

* **Breaking:** `charging_power_min` is now always a power in kW, in
  `smart_charging()`, `smart_v2g()` and `schedule_sessions()`. Up to 1.12.0 a
  value below 1 was a share of the nominal power, so `charging_power_min = 0.5`
  meant 50% and now means 0.5 kW: callers passing a share must pass the kW
  value instead. The default of `schedule_sessions()` goes from 0.5 (50%) to 0.
  A value that is not a single number of at least 0 is an error.
* One minimum applies to charging and to discharging: with
  `charging_power_min = 4`, `method = "curtail"` curtails down to 4 kW and no
  further, and `smart_v2g()` charges or discharges at 4 kW or more, or not at
  all. A session may still charge less than the minimum in its last charging
  slot, where it only needs that much to finish (the tail of its charge).
  `smart_v2g()` used to push a discharge inside `(-min, 0)` to the SoC floor's
  limit, so a session the floor allowed only -2 kW discharged at -2 kW against
  a 4 kW minimum; it now does not discharge in that slot.
* A session whose nominal `Power` is below `charging_power_min` (e.g. 3.7 kW
  against 4 kW) is **not flexible**, in both functions and whatever the
  `method`: it is never responsive, charges as uncontrolled (never curtailed,
  never discharged), and the setpoint carries its load as fixed instead of
  planning flexibility it cannot give.
* **Departure deadlines in the setpoint.** `smart_charging()` and
  `smart_v2g()` merge the sessions of a profile into one flexible load, and the
  setpoint could postpone energy owed to a car that leaves early into the slots
  of the cars that stay longer, forcing the scheduler to catch up before each
  departure. The setpoint now keeps, at every slot, at least the `energy_min`
  share of the energy of the sessions that have already departed. A new
  internal helper, `get_flex_envelope()`, computes this and the existing
  nominal-power ceiling (`LFmax`) once for both functions.
* `smart_v2g()` plans charge and discharge only while vehicles are connected:
  its setpoint is bounded by the nominal power of the connected responsive
  sessions (and `load_capacity`) in both directions, where it used to have no
  bound and could discharge with no vehicle plugged in. Its cumulative load
  never drops below the energy owed to the departed sessions minus what the
  connected ones can still discharge down to their SoC floor. When a grid
  capacity asks for more than the connected vehicles can give (an import cap of
  -20 kW with 15 kW plugged in, or a cap before the first arrival), the
  setpoint approaches it as far as their power allows, with a message, before
  falling back to relaxing the capacity to the original profile.
* `smart_v2g()` reads the arrival State-of-Charge of each session from a new
  optional `StateOfCharge` column of `sessions` (a fraction, `0 <=
  StateOfCharge < 1`, default 0.5 when the column is absent), and gains
  `soc_min` (last, default 0.2, `0 <= soc_min < 1`), the SoC floor of every
  session. Both used to be fixed at 0.5 and 0.2 inside the scheduler. The
  battery capacity is `Energy / (1 - StateOfCharge)`: the session's energy
  takes it from its arrival SoC to full (twice the energy at 0.5, as before).
  With `energy_max` below 1 a session leaves below full, at `StateOfCharge +
  energy_max * (1 - StateOfCharge)`. A session that arrives below its floor
  charges up to the floor first, as fast as it needs to, and can neither be
  curtailed nor discharge before it reaches it.

# flextools 1.12.0

* New `get_tank_unmet_heat()`: the unmet heat of any tank schedule (the heat
  that would have been required in each slot to keep the tank at least at its
  comfort floor), such as the static schedule's. `add_tank_optimization()`
  computes its `unmet_heat_kw` attribute with it, so both share one
  definition.
* `add_tank_optimization()` gains `on_failure` (last, default `"flexible"`):
  with `"error"`, a window the solver fails on stops with an error naming its
  first `datetime` and the solver status, instead of running `flexible` with a
  message.

# flextools 1.11.0

* `add_tank_optimization()` gains a resistive **backup heater** and a **soft
  comfort floor**, so every window has a schedule. The new argument `Pb` (last,
  default `NULL`) is the backup heater's rating in kW electric (= kW thermal,
  COP 1); `NULL` or 0 means none. The balance becomes
  `S[t] = S[t-1] * (1 - loss/100)^dt + (cop * P + B - heat_demand) * dt`
  with `0 <= B <= Pb` (also where `cop = 0`). The level is measured above the
  **inlet water temperature** (`SOC = 0`, where the tank gives no more heat).
  When the compressor and the backup heater cannot hold `SOCmin`, the tank
  cools below it, down to the inlet temperature at most, and the **unmet
  heat** is the heat that would have been required to keep it at least at its
  floor (the heat it gives below the floor, plus the demand it cannot serve
  at the inlet temperature). The backup heater's load enters the objective like
  the compressor's, plus `2 * h` per kW; going below the floor costs
  `20 * h / dt` per kWh and slot (the end-of-window rule becomes soft at the
  same price), `h` being the window's highest value of a kW of load in the
  objective (the highest absolute price for `"cost"`, the largest derivative
  of the squared net flow for `"grid"`, both weighted for a numeric
  objective). So the solver uses the compressor first, then the backup
  heater, and only then lets the tank cool below its floor. The soft floor
  only enters a window that has no schedule without it. The result gains the
  attributes `backup_kw` and `unmet_heat_kw` (one value per row, rounded to 2
  decimals). Without a backup heater and in windows whose band holds, the
  results are unchanged.
* `add_tank_optimization()` makes the grid capacities **soft**, as
  `add_battery_optimization()` does: a window with no schedule that holds the
  band within them is solved with the soft comfort floor and a penalised slack
  on each capacity (`10 * h` per kW, between the backup heater and the comfort
  floor), bounded by how far the site misses the capacity with the heat pump
  running `flexible` (import) or off (export). This replaces the relaxation of
  the capacities to the static schedule's net flow and the fallback to
  `flexible`: every window now has a schedule, and **the `infeasible_windows`
  attribute is gone**. A missed capacity is reported with a message.
* `add_tank_optimization()` also optimises the **partial windows** the run cuts
  at both ends: when the first slot is not a window start, the slots up to the
  first `window_start_hour` form a leading window (from `SOCini`, with the
  end-of-window rule at its last slot), and the slots after the last full
  window a trailing one, both limited to their first `flex_window_hours` like
  any window. Previously they ran `flexible`. With `flex_window_hours = 24 *
  window_days` every slot is now optimised.
* The slots that still run `flexible` (past `flex_window_hours`, or in a
  window the solver failed on) behave like the tank too: the compressor stops
  as a thermostat would at `SOCmax` (the returned power there is `flexible`
  reduced to what keeps the level at the top), and below the floor the tank
  cools to the inlet temperature at most, with unmet heat. The level therefore
  never leaves `[0, SOCmax]`, and the clamp into the band at each window start
  is gone: **`add_tank_optimization()` no longer returns `tank_clamp_kwh`, and
  `get_tank_level()` loses its `clamp` argument.**
* `get_tank_level()` gains `backup` (default 0) and stops the level at the
  inlet temperature (0).
* New article "Thermal optimization" on the tank model, the backup heater and
  the soft comfort floor, with a worked example in litres and degrees.

# flextools 1.10.0

* `smart_v2g()` gains `energy_max` (default `1`, right after `energy_min`), the
  same cap `smart_charging(energy_max = )` already has: every session charges
  at most this share of the energy it requires, net of discharge. Energy
  discharged still has to be charged back grossed up by `discharge_eff`, so
  with losses a session takes more than `energy_max` from the grid. The V2G
  setpoint optimisation now carries between `energy_min` and `energy_max` of
  the responsive sessions' energy (previously exactly all of it): it targets
  `energy_max`, drops energy down to `energy_min` before relaxing the grid
  capacity, and when even the minimum does not fit relaxes the capacity only
  as far as the minimum-energy profile needs. Both ratios are validated like
  in `smart_charging()`. With the defaults (`energy_min = energy_max = 1`) the
  results are unchanged.

# flextools 1.9.0

* New `add_tank_optimization()`: a heat pump charging one thermal tank,
  optimised on the tank's heat. It mirrors `add_battery_optimization()` (same
  `opt_data` conventions, windows from `get_flex_windows()`, `"grid"` and
  numeric weights on OSQP, `"cost"` on HiGHS) with the tank in place of the
  battery: the compressor's electric power `P` heats the tank at the COP, the
  `heat_demand` column discharges it, and the level follows
  `S[t] = S[t-1] * (1 - loss/100)^dt + (cop * P - heat_demand) * dt` inside
  `[SOCmin, SOCmax]` of `Tcap`, with `0 <= P <= Pc`, `P = 0` where `cop = 0`,
  and every window ending at least as full as it started. The level carries
  across windows. `opt_data` gains `heat_demand`, `cop` and `flexible` (the
  static schedule, run outside every window and in a window with no feasible
  solution). It returns the compressor's power, with the attributes
  `infeasible_windows` (the first `datetime` of each window that fell back)
  and `tank_clamp_kwh` (the heat the clamp into the band at each window start
  took out of the tank, signed, per row). `SOCini = NULL` means `SOCmax`;
  `"capacity"` and inputs out of range stop with an error naming the argument.
* New `get_tank_level()`, the tank's counterpart of `get_storage_level()`: the
  level in kWh at the start of each slot, applying the clamps it is given.

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
