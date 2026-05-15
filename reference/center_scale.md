# Center numeric, continuous variables

This function centers the specified numeric columns or all numeric
columns if none are specified. A centered scale has a mean of 0.0.

## Usage

``` r
center_scale(data, ..., name = NULL, overwrite = FALSE)
```

## Arguments

- data:

  A [tibble](https://tibble.tidyverse.org/reference/tibble-package.html)
  or a
  [tdcmm](https://github.com/tidycomm/tidycomm/reference/tdcmm-class.md)
  model.

- ...:

  Numeric variables to be centered. If none are provided, all numeric
  columns will be centered.

- name:

  Optional name for the new centered variable when a single variable is
  provided. By default, the name will be the original variable name
  suffixed with `_centered`.

- overwrite:

  Logical. If `TRUE`, it overwrites the original variable(s) with the
  centered values. If `FALSE` (default), a new variable(s) is created.

## Value

A [tdcmm](https://github.com/tidycomm/tidycomm/reference/tdcmm-class.md)
model with the centered variable(s).

## See also

Other scaling:
[`categorize_scale()`](https://github.com/tidycomm/tidycomm/reference/categorize_scale.md),
[`dummify_scale()`](https://github.com/tidycomm/tidycomm/reference/dummify_scale.md),
[`minmax_scale()`](https://github.com/tidycomm/tidycomm/reference/minmax_scale.md),
[`recode_cat_scale()`](https://github.com/tidycomm/tidycomm/reference/recode_cat_scale.md),
[`reverse_scale()`](https://github.com/tidycomm/tidycomm/reference/reverse_scale.md),
[`setna_scale()`](https://github.com/tidycomm/tidycomm/reference/setna_scale.md),
[`z_scale()`](https://github.com/tidycomm/tidycomm/reference/z_scale.md)

## Examples

``` r
WoJ %>% dplyr::select(autonomy_emphasis) %>% center_scale(autonomy_emphasis)
#> # A tibble: 1,200 × 2
#>    autonomy_emphasis autonomy_emphasis_centered
#>  *             <dbl>                      <dbl>
#>  1                 4                    -0.0762
#>  2                 4                    -0.0762
#>  3                 4                    -0.0762
#>  4                 5                     0.924 
#>  5                 4                    -0.0762
#>  6                 4                    -0.0762
#>  7                 4                    -0.0762
#>  8                 3                    -1.08  
#>  9                 5                     0.924 
#> 10                 4                    -0.0762
#> # ℹ 1,190 more rows
WoJ %>% center_scale(autonomy_emphasis, name = "my_centered_variable")
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
#> #   my_centered_variable <dbl>
WoJ %>% center_scale(overwrite = TRUE)
#> NOTE: No variables provided. All numeric columns will be centered.
#> # A tibble: 1,200 × 15
#>    country   reach employment temp_contract autonomy_selection autonomy_emphasis
#>  * <fct>     <fct> <chr>      <fct>                      <dbl>             <dbl>
#>  1 Germany   Nati… Full-time  Permanent                  1.12            -0.0762
#>  2 Germany   Nati… Full-time  Permanent                 -0.876           -0.0762
#>  3 Switzerl… Regi… Full-time  Permanent                  0.124           -0.0762
#>  4 Switzerl… Local Part-time  Permanent                  0.124            0.924 
#>  5 Austria   Nati… Part-time  Permanent                  0.124           -0.0762
#>  6 Switzerl… Local Freelancer NA                         0.124           -0.0762
#>  7 Germany   Local Full-time  Permanent                  0.124           -0.0762
#>  8 Denmark   Nati… Full-time  Permanent                 -0.876           -1.08  
#>  9 Switzerl… Local Full-time  Permanent                  1.12             0.924 
#> 10 Denmark   Nati… Full-time  Permanent                 -1.88            -0.0762
#> # ℹ 1,190 more rows
#> # ℹ 9 more variables: ethics_1 <dbl>, ethics_2 <dbl>, ethics_3 <dbl>,
#> #   ethics_4 <dbl>, work_experience <dbl>, trust_parliament <dbl>,
#> #   trust_government <dbl>, trust_parties <dbl>, trust_politicians <dbl>
WoJ %>%
  center_scale(autonomy_emphasis) %>%
  tab_frequencies(autonomy_emphasis, autonomy_emphasis_centered)
#> # A tibble: 6 × 6
#>   autonomy_emphasis autonomy_emphasis_centered     n percent cum_n cum_percent
#> *             <dbl>                      <dbl> <int>   <dbl> <int>       <dbl>
#> 1                 1                    -3.08      10 0.00833    10     0.00833
#> 2                 2                    -2.08      36 0.03       46     0.0383 
#> 3                 3                    -1.08     165 0.138     211     0.176  
#> 4                 4                    -0.0762   626 0.522     837     0.698  
#> 5                 5                     0.924    358 0.298    1195     0.996  
#> 6                NA                    NA          5 0.00417  1200     1      
```
