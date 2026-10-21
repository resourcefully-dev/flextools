# Optimization windows of the tank, partial ones at both ends included

The full windows of `get_flex_windows()`, plus a leading window over the
slots before the first window start and a trailing window over the slots
after the last full window, which the run cuts short. Each partial
window keeps the flexible part (the first `flex_window_hours` of its
window) that falls inside the run, so with
`flex_window_hours = 24 * window_days` every slot lies in a window.

## Usage

``` r
tank_flex_windows(dttm_seq, window_days, window_start_hour, flex_window_hours)
```

## Arguments

- dttm_seq:

  datetime vector of the run

- window_days, window_start_hour, flex_window_hours:

  numeric, as in
  [`add_tank_optimization()`](https://resourcefully-dev.github.io/flextools/reference/add_tank_optimization.md)

## Value

tibble with the columns `start` (first optimised slot) and `flex_idx`
(list of the optimised slots), ordered by `start`
