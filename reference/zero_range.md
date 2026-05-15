# Determine if range of vector is close to zero, with a specified tolerance

The machine epsilon is the difference between 1.0 and the next number
that can be represented by the machine. By default, this function uses
epsilon \* 1000 as the tolerance. First it scales the values so that
they have a mean of 1, and then it checks if the difference between them
is larger than the tolerance.

## Usage

``` r
zero_range(x, tol = 1000 * .Machine$double.eps)
```

## Arguments

- x:

  numeric range: vector of length 2

- tol:

  A value specifying the tolerance.

## Value

logical `TRUE` if the relative difference of the endpoints of the range
are not distinguishable from 0.
