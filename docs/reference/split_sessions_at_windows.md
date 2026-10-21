# Split sessions at the optimization window boundaries

A session whose connection crosses a window boundary is cut into one
part per window: each part keeps the session's `Session` id, `Power`,
`Profile` and `Timecycle`, gets the real connection interval of its
window, a `Part` number, and a share of the energy proportional to its
connection time. Sessions that cross no boundary are returned unchanged
with `Part = 1`.

## Usage

``` r
split_sessions_at_windows(sessions, boundaries, time_resolution)
```

## Arguments

- sessions:

  tibble, sessions as returned by
  [`evsim::adapt_charging_features()`](https://resourcefully-dev.github.io/evsim/reference/adapt_charging_features.html)

- boundaries:

  POSIXct vector, window start instants (and the end of the last
  window); only boundaries strictly inside a connection split it

- time_resolution:

  numeric, minutes

## Value

the sessions tibble with a `Part` column, possibly more rows

## Details

The share is by connection time, not by what nominal charging would
deliver first: a car that arrives 45 minutes before a boundary and stays
nine hours would otherwise carry most of its energy into those 45
minutes, where no capacity could hold it and the `energy_min` floor
would force it through. Proportional puts the energy where the
flexibility is. The parts sum to the session's energy up to the
2-decimal rounding of each part.

A part that could not fill one time slot at nominal power (energy below
`Power * time_resolution / 60`) is folded into its longest neighbouring
part instead of being emitted:
[`evsim::get_demand()`](https://resourcefully-dev.github.io/evsim/reference/get_demand.html)
renders sub-slot charging as a whole slot at nominal power, so such a
part would enter the optimizer as several times its real energy. Folding
keeps the energy and drops the sliver of connection time, which is what
the scheduler could do with it anyway. A session that arrives shortly
before a boundary and stays long therefore moves whole into the next
window. The charging features of every part are recomputed with
[`evsim::adapt_charging_features()`](https://resourcefully-dev.github.io/evsim/reference/adapt_charging_features.html).
