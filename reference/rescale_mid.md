# Rescale vector to have specified minimum, midpoint, and maximum

Rescale vector to have specified minimum, midpoint, and maximum

## Usage

``` r
rescale_mid(x, to, from, mid, ...)

# S3 method for class 'numeric'
rescale_mid(x, to = c(0, 1), from = range(x, na.rm = TRUE), mid = 0, ...)

# S3 method for class 'logical'
rescale_mid(x, to = c(0, 1), from = range(x, na.rm = TRUE), mid = 0, ...)

# S3 method for class 'dist'
rescale_mid(x, to = c(0, 1), from = range(x, na.rm = TRUE), mid = 0, ...)

# S3 method for class 'POSIXt'
rescale_mid(x, to = c(0, 1), from = range(x, na.rm = TRUE), mid, ...)

# S3 method for class 'Date'
rescale_mid(x, to = c(0, 1), from = range(x, na.rm = TRUE), mid, ...)

# S3 method for class 'integer64'
rescale_mid(x, to = c(0, 1), from = range(x, na.rm = TRUE), mid = 0, ...)
```

## Arguments

- x:

  vector of values to manipulate.

- to:

  output range (numeric vector of length two)

- from:

  input range (vector of length two). If not given, is calculated from
  the range of `x`

- mid:

  mid-point of input range

- ...:

  other arguments passed on to methods
