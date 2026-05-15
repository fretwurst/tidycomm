# Reverse numeric, logical, or date/time continuous variables

Reverses a continuous scale into a new variable. A 5-1 scale thus turns
into a 1-5 scale. Missing values are retained. For a given continuous
variable the lower and upper end of the scale should be provided. If
they are not provided, the function assumes the scale's minimum and
maximum value to represent these lower/upper ends (and issues a warning
about this fact). This default behavior is prone to errors, however,
because a scale may not include its actual lower and upper ends which
might in turn affect correct reversing. Hence, it is strongly suggested
to manually set the lower and upper bounds of the original continuous
scale.

## Usage

``` r
reverse_scale(
  data,
  ...,
  lower_end = NULL,
  upper_end = NULL,
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

  Numeric variables to be reverse scaled. If none are provided, all
  numeric columns will be scaled.

- lower_end:

  Lower end of provided continuous scale (default is to use minimum
  value of current values, which might not be the actual lower end of
  the scale).

- upper_end:

  Upper end of provided continuous scale (default is to use maximum
  value of current values, which might not be the actual upper end of
  the scale).

- name:

  Optional name for the new reversed variable when a single variable is
  provided. By default, the name will be the original variable name
  suffixed with `_rev`.

- overwrite:

  Logical. If `TRUE`, it overwrites the original variable(s) with the
  reversed values. If `FALSE` (default), a new variable(s) is created.

## Value

A [tdcmm](https://github.com/tidycomm/tidycomm/reference/tdcmm-class.md)
model with the reversed variable(s).

## See also

Other scaling:
[`categorize_scale()`](https://github.com/tidycomm/tidycomm/reference/categorize_scale.md),
[`center_scale()`](https://github.com/tidycomm/tidycomm/reference/center_scale.md),
[`dummify_scale()`](https://github.com/tidycomm/tidycomm/reference/dummify_scale.md),
[`minmax_scale()`](https://github.com/tidycomm/tidycomm/reference/minmax_scale.md),
[`recode_cat_scale()`](https://github.com/tidycomm/tidycomm/reference/recode_cat_scale.md),
[`setna_scale()`](https://github.com/tidycomm/tidycomm/reference/setna_scale.md),
[`z_scale()`](https://github.com/tidycomm/tidycomm/reference/z_scale.md)

## Examples

``` r
WoJ %>% reverse_scale(autonomy_emphasis, lower_end = 0, upper_end = 1)
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
#> #   autonomy_emphasis_rev <dbl>
WoJ %>% reverse_scale(autonomy_emphasis, name = "my_reversed_variable",
lower_end = 0, upper_end = 1)
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
#> #   my_reversed_variable <dbl>
WoJ %>% reverse_scale(overwrite = TRUE)
#> NOTE: No variables provided. All numeric columns will be reversed.
#> Warning: Lower and/or upper end missing. Based on the minimum and maximum values observed in the data, the original scale (autonomy_selection) is assumed to range from 1 to 5. To prevent this warning, please provide the lower_end and upper_end values as arguments when calling the function.
#> # A tibble: 1,200 × 15
#>    country   reach employment temp_contract autonomy_selection autonomy_emphasis
#>  * <fct>     <fct> <chr>      <fct>                      <dbl>             <dbl>
#>  1 Germany   Nati… Full-time  Permanent                      1                 2
#>  2 Germany   Nati… Full-time  Permanent                      3                 2
#>  3 Switzerl… Regi… Full-time  Permanent                      2                 2
#>  4 Switzerl… Local Part-time  Permanent                      2                 1
#>  5 Austria   Nati… Part-time  Permanent                      2                 2
#>  6 Switzerl… Local Freelancer NA                             2                 2
#>  7 Germany   Local Full-time  Permanent                      2                 2
#>  8 Denmark   Nati… Full-time  Permanent                      3                 3
#>  9 Switzerl… Local Full-time  Permanent                      1                 1
#> 10 Denmark   Nati… Full-time  Permanent                      4                 2
#> # ℹ 1,190 more rows
#> # ℹ 9 more variables: ethics_1 <dbl>, ethics_2 <dbl>, ethics_3 <dbl>,
#> #   ethics_4 <dbl>, work_experience <dbl>, trust_parliament <dbl>,
#> #   trust_government <dbl>, trust_parties <dbl>, trust_politicians <dbl>
WoJ %>%
  reverse_scale(autonomy_emphasis, lower_end = 0, upper_end = 1) %>%
  tab_frequencies(autonomy_emphasis, autonomy_emphasis_rev)
#> # A tibble: 6 × 6
#>   autonomy_emphasis autonomy_emphasis_rev     n percent cum_n cum_percent
#> *             <dbl>                 <dbl> <int>   <dbl> <int>       <dbl>
#> 1                 1                     0    10 0.00833    10     0.00833
#> 2                 2                    NA    36 0.03       46     0.0383 
#> 3                 3                    NA   165 0.138     211     0.176  
#> 4                 4                    NA   626 0.522     837     0.698  
#> 5                 5                    NA   358 0.298    1195     0.996  
#> 6                NA                    NA     5 0.00417  1200     1      
```
