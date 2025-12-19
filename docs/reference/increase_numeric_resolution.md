# Increase numeric vector resolution

Increase numeric vector resolution

## Usage

``` r
increase_numeric_resolution(
  y,
  n,
  method = c("interpolate", "repeat", "divide")
)
```

## Arguments

- y:

  original numeric vector

- n:

  integer, number of intra-values (counting the original value as the
  first one)

- method:

  character, being `interpolate`, `repeat` or `divide` as valid options

## Value

numeric vector

## Details

if we have a vector v = c(1, 2), and we choose the `interpolate` method,
then:

`increase_numeric_resolution(v, 4, 'interpolate')`

returns `c(1, 1.25, 1.5, 1.75, 2)`

if we choose the `repeat` method, then:

`increase_numeric_resolution(v, 4, 'repeat')`

returns c(1, 1, 1, 1, 2)
