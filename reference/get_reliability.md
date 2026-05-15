# Get reliability estimates of index variables

Get reliability estimates of index variables created with
[`add_index`](https://github.com/tidycomm/tidycomm/reference/add_index.md).

## Usage

``` r
get_reliability(
  data,
  ...,
  type = "alpha",
  interval.type = NULL,
  bootstrap.samples = NULL,
  conf.level = NULL,
  progress = FALSE
)
```

## Arguments

- data:

  a [tibble](https://tibble.tidyverse.org/reference/tibble-package.html)
  or a
  [tdcmm](https://github.com/tidycomm/tidycomm/reference/tdcmm-class.md)
  model

- ...:

  Index variables created with
  [`add_index`](https://github.com/tidycomm/tidycomm/reference/add_index.md).
  Leave empty to get reliability estimates for all index variables.

- type:

  Type of reliability estimate. See
  [`ci.reliability`](https://rdrr.io/pkg/MBESS/man/ci.reliability.html)

- interval.type:

  Type of reliability estimate confidence interval. See
  [`ci.reliability`](https://rdrr.io/pkg/MBESS/man/ci.reliability.html)

- bootstrap.samples:

  Number of bootstrap samples for CI calculation. See
  [`ci.reliability`](https://rdrr.io/pkg/MBESS/man/ci.reliability.html)

- conf.level:

  Confidence level for estimate CI. See
  [`ci.reliability`](https://rdrr.io/pkg/MBESS/man/ci.reliability.html)

- progress:

  Show progress for reliability estimate computation. Useful if using
  computationally intense computations (e. g., many bootstrapping
  samples) and many index variables.

## Value

a [tdcmm](https://github.com/tidycomm/tidycomm/reference/tdcmm-class.md)
model

## See also

[`add_index()`](https://github.com/tidycomm/tidycomm/reference/add_index.md)
to create index variables

## Examples

``` r
WoJ %>%
  add_index(ethical_flexibility, ethics_1, ethics_2, ethics_3, ethics_4) %>%
  get_reliability()
#> # A tibble: 1 × 5
#>   Index               Index_of                           M    SD Cronbachs_Alpha
#> * <chr>               <chr>                          <dbl> <dbl>           <dbl>
#> 1 ethical_flexibility ethics_1, ethics_2, ethics_3,…  2.45 0.777           0.612
```
