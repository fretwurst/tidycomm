# Rescale numeric vector to have specified maximum

Rescale numeric vector to have specified maximum

## Usage

``` r
rescale_max(x, to = c(0, 1), from = range(x, na.rm = TRUE))
```

## Arguments

- x:

  numeric vector of values to manipulate.

- to:

  output range (numeric vector of length two)

- from:

  input range (numeric vector of length two). If not given, is
  calculated from the range of `x`
