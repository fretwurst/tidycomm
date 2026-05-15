# `tdcmm` output constructor

Creates a new `tdcmm` class output object.

The `tdcmm` class is a subclass of a
[`tbl_df`](https://tibble.tidyverse.org/reference/tbl_df-class.html),
also know as a "tibble", used for augmenting output tibbles of
`tidycomm` functions with additional information. For output based on
statistical tests, the model object(s) estimated and any performed
assumption checks.

`tdcmm` objects in `tidycomm` are further subclassed with individual
classes handling visualization and printing per "output" type.

## Usage

``` r
new_tdcmm(x, func, data, model = NULL, checks = NULL, params = list())

is_tdcmm(x)
```

## Arguments

- x:

  A [tibble](https://tibble.tidyverse.org/reference/tibble.html).

- func:

  Function name called that produced this model.

- data:

  The [tibble](https://tibble.tidyverse.org/reference/tibble.html) that
  served as input to the function.

- model:

  A list of model object(s) used in preparation of the output. Defaults
  to `NULL`. A single model should be wrapped in a list of length `1`.

- checks:

  A list of assumption check object(s) used in preparation of the
  output. Defaults to `NULL`.

- params:

  A named list of parameters originally passed to the call. Defaults to
  an empty list.

## Functions

- `is_tdcmm()`: Test for class `tdcmm`
