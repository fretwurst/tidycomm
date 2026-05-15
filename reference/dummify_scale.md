# Convert categorical variables to dummy variables

This function transforms specified categorical variables into dummy
variables. Each level of the categorical variable is represented by a
new dummy variable. Missing values are retained. These new dummy
variables are appended to the original data frame. This function does
not allow specifying new column names for the dummy variables. Instead,
it follows a consistent naming pattern: the new dummy variables are
named using the original variable name with the category value appended.
For example, if a categorical variable named "autonomy" with levels
"low", "medium", "high" is dummified, the new dummy variables will be
named "autonomy_low", "autonomy_medium", "autonomy_high".

## Usage

``` r
dummify_scale(data, ..., overwrite = FALSE)
```

## Arguments

- data:

  A [tibble](https://tibble.tidyverse.org/reference/tibble-package.html)
  or a
  [tdcmm](https://github.com/tidycomm/tidycomm/reference/tdcmm-class.md)
  model.

- ...:

  Categorical variables to be transformed into dummy variables. Category
  names will be automatically appended to the newly created dummy
  variables.

- overwrite:

  Logical. If `TRUE`, it overwrites the original variable(s) with the
  dummy variables. If `FALSE` (default), new variables are created.

## Value

A [tdcmm](https://github.com/tidycomm/tidycomm/reference/tdcmm-class.md)
model with the dummy variables appended.

## See also

Other scaling:
[`categorize_scale()`](https://github.com/tidycomm/tidycomm/reference/categorize_scale.md),
[`center_scale()`](https://github.com/tidycomm/tidycomm/reference/center_scale.md),
[`minmax_scale()`](https://github.com/tidycomm/tidycomm/reference/minmax_scale.md),
[`recode_cat_scale()`](https://github.com/tidycomm/tidycomm/reference/recode_cat_scale.md),
[`reverse_scale()`](https://github.com/tidycomm/tidycomm/reference/reverse_scale.md),
[`setna_scale()`](https://github.com/tidycomm/tidycomm/reference/setna_scale.md),
[`z_scale()`](https://github.com/tidycomm/tidycomm/reference/z_scale.md)

## Examples

``` r
WoJ %>% dplyr::select(temp_contract) %>% dummify_scale(temp_contract)
#> # A tibble: 1,200 × 3
#>    temp_contract temp_contract_permanent temp_contract_temporary
#>  * <fct>                           <int>                   <int>
#>  1 Permanent                           1                       0
#>  2 Permanent                           1                       0
#>  3 Permanent                           1                       0
#>  4 Permanent                           1                       0
#>  5 Permanent                           1                       0
#>  6 NA                                 NA                      NA
#>  7 Permanent                           1                       0
#>  8 Permanent                           1                       0
#>  9 Permanent                           1                       0
#> 10 Permanent                           1                       0
#> # ℹ 1,190 more rows
WoJ %>% categorize_scale(autonomy_emphasis, breaks = c(2, 3),
labels = c('low', 'medium', 'high')) %>%
dummify_scale(autonomy_emphasis_cat) %>% dplyr::select(starts_with('autonomy_emphasis'))
#> Warning: Lower and/or upper end missing. Based on the minimum and maximum values observed in the data, the original scale (autonomy_emphasis) is assumed to range from 1 to 5. To prevent this warning, please provide the lower_end and upper_end values as arguments when calling the function.
#> # A tibble: 1,200 × 5
#>    autonomy_emphasis autonomy_emphasis_cat autonomy_emphasis_cat_low
#>                <dbl> <fct>                                     <int>
#>  1                 4 high                                          0
#>  2                 4 high                                          0
#>  3                 4 high                                          0
#>  4                 5 high                                          0
#>  5                 4 high                                          0
#>  6                 4 high                                          0
#>  7                 4 high                                          0
#>  8                 3 medium                                        0
#>  9                 5 high                                          0
#> 10                 4 high                                          0
#> # ℹ 1,190 more rows
#> # ℹ 2 more variables: autonomy_emphasis_cat_medium <int>,
#> #   autonomy_emphasis_cat_high <int>
```
