# Rescale numeric continuous variables to new minimum/maximum boundaries

Given a specified minimum and maximum, this function translates each
value into a new value within this specified range. The transformation
maintains the relative distances between values, resulting in changes to
the mean and standard deviations. However, if both the original scale
and the transformed scale are z-standardized, they will be equal again,
indicating that the relative positions and distributions of the values
remain consistent.

## Usage

``` r
minmax_scale(
  data,
  ...,
  change_to_min = 0,
  change_to_max = 1,
  name = NULL,
  overwrite = FALSE
)
```

## Arguments

- data:

  A [tibble](https://tibble.tidyverse.org/reference/tibble-package.html)
  or a
  [tdcmm](https://github.com/tidycomm/tidycomm/reference/tdcmm-class.md)
  model.

- ...:

  Numeric variables to be min-max scaled. If none are provided, all
  numeric columns will be scaled.

- change_to_min:

  The desired minimum value after scaling.

- change_to_max:

  The desired maximum value after scaling.

- name:

  Optional name for the new scaled variable when a single variable is
  provided. By default, the name will be the original variable name
  suffixed with the range. For example, "variable" becomes
  "variable_3to5". Negative values are prefixed with "neg" to avoid
  invalid columns names (e.g., -3 to 3 becomes "variable_neg3to5").

- overwrite:

  Logical. If `TRUE`, it overwrites the original variable(s) with the
  scaled values. If `FALSE` (default), a new variable(s) is created.

## Value

A [tdcmm](https://github.com/tidycomm/tidycomm/reference/tdcmm-class.md)
model with the min-max scaled variable(s).

## See also

Other scaling:
[`categorize_scale()`](https://github.com/tidycomm/tidycomm/reference/categorize_scale.md),
[`center_scale()`](https://github.com/tidycomm/tidycomm/reference/center_scale.md),
[`dummify_scale()`](https://github.com/tidycomm/tidycomm/reference/dummify_scale.md),
[`recode_cat_scale()`](https://github.com/tidycomm/tidycomm/reference/recode_cat_scale.md),
[`reverse_scale()`](https://github.com/tidycomm/tidycomm/reference/reverse_scale.md),
[`setna_scale()`](https://github.com/tidycomm/tidycomm/reference/setna_scale.md),
[`z_scale()`](https://github.com/tidycomm/tidycomm/reference/z_scale.md)

## Examples

``` r
WoJ %>% minmax_scale(autonomy_emphasis, change_to_min = 0,
change_to_max = 1)
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
#> #   autonomy_emphasis_0to1 <dbl>
WoJ %>% minmax_scale(autonomy_emphasis, name = "my_scaled_variable",
change_to_min = 0, change_to_max = 1)
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
#> #   my_scaled_variable <dbl>
WoJ %>%
  minmax_scale(autonomy_emphasis, change_to_min = 0, change_to_max = 1) %>%
  tab_frequencies(autonomy_emphasis, autonomy_emphasis_0to1)
#> # A tibble: 6 × 6
#>   autonomy_emphasis autonomy_emphasis_0to1     n percent cum_n cum_percent
#> *             <dbl>                  <dbl> <int>   <dbl> <int>       <dbl>
#> 1                 1                   0       10 0.00833    10     0.00833
#> 2                 2                   0.25    36 0.03       46     0.0383 
#> 3                 3                   0.5    165 0.138     211     0.176  
#> 4                 4                   0.75   626 0.522     837     0.698  
#> 5                 5                   1      358 0.298    1195     0.996  
#> 6                NA                  NA        5 0.00417  1200     1      
```
