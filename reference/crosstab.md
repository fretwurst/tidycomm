# Crosstab variables

Computes contingency table for one independent (column) variable and one
or more dependent (row) variables.

## Usage

``` r
crosstab(
  data,
  col_var,
  ...,
  add_total = FALSE,
  percentages = FALSE,
  chi_square = FALSE
)
```

## Arguments

- data:

  a [tibble](https://tibble.tidyverse.org/reference/tibble-package.html)
  or a
  [tdcmm](https://github.com/tidycomm/tidycomm/reference/tdcmm-class.md)
  model

- col_var:

  Independent (column) variable.

- ...:

  Dependent (row) variables.

- add_total:

  Logical indicating whether a 'Total' column should be computed.
  Defaults to `FALSE`.

- percentages:

  Logical indicating whether to output column-wise percentages instead
  of absolute values. Defaults to `FALSE`.

- chi_square:

  Logical indicating whether a Chi-square test should be computed. Test
  results will be reported via message(). Defaults to `FALSE`.

## Value

a [tdcmm](https://github.com/tidycomm/tidycomm/reference/tdcmm-class.md)
model

## See also

Other categorical:
[`knit_crosstab()`](https://github.com/tidycomm/tidycomm/reference/knit_crosstab.md),
[`knit_frequencies()`](https://github.com/tidycomm/tidycomm/reference/knit_frequencies.md),
[`tab_frequencies()`](https://github.com/tidycomm/tidycomm/reference/tab_frequencies.md)

## Examples

``` r
WoJ %>% crosstab(reach, employment)
#> # A tibble: 3 × 5
#>   employment Local Regional National Transnational
#> * <chr>      <dbl>    <dbl>    <dbl>         <dbl>
#> 1 Freelancer    23       36      104             9
#> 2 Full-time    111      287      438            66
#> 3 Part-time     15       32       75             4
WoJ %>% crosstab(reach, employment, add_total = TRUE, percentages = TRUE, chi_square = TRUE)
#> # A tibble: 3 × 6
#>   employment Local Regional National Transnational Total
#> * <chr>      <dbl>    <dbl>    <dbl>         <dbl> <dbl>
#> 1 Freelancer 0.154   0.101     0.169        0.114  0.143
#> 2 Full-time  0.745   0.808     0.710        0.835  0.752
#> 3 Part-time  0.101   0.0901    0.122        0.0506 0.105
#> # Chi-square = 16.005, df = 6, p = 0.014, V = 0.082
```
