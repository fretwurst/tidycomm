# Tabulate percentiles for numeric variables

This function tabulates specified percentiles for given numeric
variables. If no variables are provided, the function will attempt to
describe all numeric (either integer or double) variables found within
the input. The percentiles are calculated based on the levels parameter,
which defaults to every 10% from 10% to 90%. NA values are always
removed because the concept of a percentile is based on ranking. As NA
is not a value, it cannot be ordered in relation to actual numbers.

## Usage

``` r
tab_percentiles(
  data,
  ...,
  levels = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1)
)
```

## Arguments

- data:

  a [tibble](https://tibble.tidyverse.org/reference/tibble-package.html)
  or a
  [tdcmm](https://github.com/tidycomm/tidycomm/reference/tdcmm-class.md)
  model that contains the numeric data to be tabulated.

- ...:

  Variables within the data for which to tabulate the percentiles. If no
  variables are provided, all numeric variables are used.

- levels:

  a numeric vector specifying the percentiles to compute. Defaults to
  c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0).

## Value

a [tdcmm](https://github.com/tidycomm/tidycomm/reference/tdcmm-class.md)
model

## See also

Other descriptives:
[`describe()`](https://github.com/tidycomm/tidycomm/reference/describe.md),
[`describe_cat()`](https://github.com/tidycomm/tidycomm/reference/describe_cat.md)

## Examples

``` r
WoJ %>% tab_percentiles(work_experience)
#> # A tibble: 1 × 11
#>   Variable          p10   p20   p30   p40   p50   p60   p70   p80   p90  p100
#> * <chr>           <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1 work_experience     4     7    10    14    17    20    25    28    33    53
WoJ %>% tab_percentiles(work_experience, autonomy_emphasis)
#> # A tibble: 2 × 11
#>   Variable            p10   p20   p30   p40   p50   p60   p70   p80   p90  p100
#> * <chr>             <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1 work_experience       4     7    10    14    17    20    25    28    33    53
#> 2 autonomy_emphasis     3     4     4     4     4     4     4     5     5     5
```
