# Rescale continuous vector to have specified minimum and maximum

Rescale continuous vector to have specified minimum and maximum

## Usage

``` r
rescale(x, to, from, ...)

# S3 method for class 'numeric'
rescale(x, to = c(0, 1), from = range(x, na.rm = TRUE, finite = TRUE), ...)

# S3 method for class 'dist'
rescale(x, to = c(0, 1), from = range(x, na.rm = TRUE, finite = TRUE), ...)

# S3 method for class 'logical'
rescale(x, to = c(0, 1), from = range(x, na.rm = TRUE, finite = TRUE), ...)

# S3 method for class 'POSIXt'
rescale(x, to = c(0, 1), from = range(x, na.rm = TRUE, finite = TRUE), ...)

# S3 method for class 'Date'
rescale(x, to = c(0, 1), from = range(x, na.rm = TRUE, finite = TRUE), ...)

# S3 method for class 'integer64'
rescale(x, to = c(0, 1), from = range(x, na.rm = TRUE), ...)
```

## Arguments

- x:

  continuous vector of values to manipulate.

- to:

  output range (numeric vector of length two)

- from:

  input range (vector of length two). If not given, is calculated from
  the range of `x`

- ...:

  other arguments passed on to methods
