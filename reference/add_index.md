# Add index

Add a rowwise mean or sum index of specific variables to the dataset.

## Usage

``` r
add_index(data, name, ..., type = "mean", na.rm = TRUE, cast.numeric = FALSE)
```

## Arguments

- data:

  a [tibble](https://tibble.tidyverse.org/reference/tibble-package.html)
  or a
  [tdcmm](https://github.com/tidycomm/tidycomm/reference/tdcmm-class.md)
  model

- name:

  Name of the index column to compute.

- ...:

  Variables used for the index.

- type:

  Type of index to compute. Either "mean" (default) or "sum".

- na.rm:

  a logical value indicating whether `NA` values should be stripped
  before the computation proceeds. Defaults to `TRUE`.

- cast.numeric:

  a logical value indicating whether all variables selected for index
  computation should be converted to numeric. Useful if computing
  indices from factor variables. Defaults to `FALSE`.

## Value

a [tdcmm](https://github.com/tidycomm/tidycomm/reference/tdcmm-class.md)
model

## See also

[`get_reliability()`](https://github.com/tidycomm/tidycomm/reference/get_reliability.md)
to compute reliability estimates of added index variables.

## Examples

``` r
WoJ %>% add_index(ethical_flexibility, ethics_1, ethics_2, ethics_3, ethics_4)
#> # A tibble: 1,200 × 16
#>    country   reach employment temp_contract autonomy_selection autonomy_emphasis
#>  * <fct>     <fct> <chr>      <fct>                      <dbl>             <dbl>
#>  1 Germany   Nati… Full-time  Permanent                      5                 4
#>  2 Germany   Nati… Full-time  Permanent                      3                 4
#>  3 Switzerl… Regi… Full-time  Permanent                      4                 4
#>  4 Switzerl… Local Part-time  Permanent                      4                 5
#>  5 Austria   Nati… Part-time  Permanent                      4                 4
#>  6 Switzerl… Local Freelancer NA                             4                 4
#>  7 Germany   Local Full-time  Permanent                      4                 4
#>  8 Denmark   Nati… Full-time  Permanent                      3                 3
#>  9 Switzerl… Local Full-time  Permanent                      5                 5
#> 10 Denmark   Nati… Full-time  Permanent                      2                 4
#> # ℹ 1,190 more rows
#> # ℹ 10 more variables: ethics_1 <dbl>, ethics_2 <dbl>, ethics_3 <dbl>,
#> #   ethics_4 <dbl>, work_experience <dbl>, trust_parliament <dbl>,
#> #   trust_government <dbl>, trust_parties <dbl>, trust_politicians <dbl>,
#> #   ethical_flexibility <dbl>
WoJ %>% add_index(ethical_flexibility, ethics_1, ethics_2, ethics_3, ethics_4, type = "sum")
#> # A tibble: 1,200 × 16
#>    country   reach employment temp_contract autonomy_selection autonomy_emphasis
#>  * <fct>     <fct> <chr>      <fct>                      <dbl>             <dbl>
#>  1 Germany   Nati… Full-time  Permanent                      5                 4
#>  2 Germany   Nati… Full-time  Permanent                      3                 4
#>  3 Switzerl… Regi… Full-time  Permanent                      4                 4
#>  4 Switzerl… Local Part-time  Permanent                      4                 5
#>  5 Austria   Nati… Part-time  Permanent                      4                 4
#>  6 Switzerl… Local Freelancer NA                             4                 4
#>  7 Germany   Local Full-time  Permanent                      4                 4
#>  8 Denmark   Nati… Full-time  Permanent                      3                 3
#>  9 Switzerl… Local Full-time  Permanent                      5                 5
#> 10 Denmark   Nati… Full-time  Permanent                      2                 4
#> # ℹ 1,190 more rows
#> # ℹ 10 more variables: ethics_1 <dbl>, ethics_2 <dbl>, ethics_3 <dbl>,
#> #   ethics_4 <dbl>, work_experience <dbl>, trust_parliament <dbl>,
#> #   trust_government <dbl>, trust_parties <dbl>, trust_politicians <dbl>,
#> #   ethical_flexibility <dbl>
```
