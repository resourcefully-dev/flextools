# Validate the energy range of a demand window

The optimized load carries, by default, exactly the energy of the
flexible profile it starts from. `energy_ratio = c(min, max)` widens
that to a range `[min, max] * sum(LF)`. A range below 1 only makes sense
when energy may be left undelivered, which the cumulative-shift
constraints admit only for forward shifting over the whole window: any
finite horizon, and the backward direction, pin `cumsum(O)` from below
to the original profile.

## Usage

``` r
demand_check_energy_ratio(energy_ratio, direction, time_horizon, time_slots)
```

## Arguments

- energy_ratio:

  numeric vector `c(min, max)`, or `NULL` for `c(1, 1)`.

- direction:

  character, `forward` or `backward`.

- time_horizon:

  integer or `NULL` (whole window).

- time_slots:

  integer, number of slots in the window.

## Value

the validated `c(min, max)`
