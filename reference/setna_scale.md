# Set specified values to NA in selected variables or entire data frame

This function allows users to set specific values to `NA` in chosen
variables within a data frame. It can handle numeric, character, and
factor variables.

## Usage

``` r
setna_scale(data, ..., value, name = NULL, overwrite = FALSE)
```

## Arguments

- data:

  A [tibble](https://tibble.tidyverse.org/reference/tibble-package.html)
  or a
  [tdcmm](https://github.com/tidycomm/tidycomm/reference/tdcmm-class.md)
  model.

- ...:

  One or more variables where specified values will be set to NA. If no
  variables are provided, the function is applied to the entire data
  frame.

- value:

  A value (or vector of values) that needs to be set to NA.

- name:

  The name of the new variable(s). By default, this is the same name as
  the provided variable(s) but suffixed with `_na`.

- overwrite:

  Logical. If `TRUE`, it overwrites the original variable(s). You cannot
  specify both 'name' and 'overwrite' parameters simultaneously.

## Value

A [tdcmm](https://github.com/tidycomm/tidycomm/reference/tdcmm-class.md)
model or a tibble.

## See also

Other scaling:
[`categorize_scale()`](https://github.com/tidycomm/tidycomm/reference/categorize_scale.md),
[`center_scale()`](https://github.com/tidycomm/tidycomm/reference/center_scale.md),
[`dummify_scale()`](https://github.com/tidycomm/tidycomm/reference/dummify_scale.md),
[`minmax_scale()`](https://github.com/tidycomm/tidycomm/reference/minmax_scale.md),
[`recode_cat_scale()`](https://github.com/tidycomm/tidycomm/reference/recode_cat_scale.md),
[`reverse_scale()`](https://github.com/tidycomm/tidycomm/reference/reverse_scale.md),
[`z_scale()`](https://github.com/tidycomm/tidycomm/reference/z_scale.md)

## Examples

``` r
WoJ %>%
dplyr::select(autonomy_emphasis) %>%
setna_scale(autonomy_emphasis, value = 5)
#> # A tibble: 1,200 × 2
#>    autonomy_emphasis autonomy_emphasis_na
#>  *             <dbl>                <dbl>
#>  1                 4                    4
#>  2                 4                    4
#>  3                 4                    4
#>  4                 5                   NA
#>  5                 4                    4
#>  6                 4                    4
#>  7                 4                    4
#>  8                 3                    3
#>  9                 5                   NA
#> 10                 4                    4
#> # ℹ 1,190 more rows
WoJ %>%
dplyr::select(autonomy_emphasis) %>%
setna_scale(autonomy_emphasis, value = 5, name = "new_na_autonomy")
#> # A tibble: 1,200 × 2
#>    autonomy_emphasis new_na_autonomy
#>  *             <dbl>           <dbl>
#>  1                 4               4
#>  2                 4               4
#>  3                 4               4
#>  4                 5              NA
#>  5                 4               4
#>  6                 4               4
#>  7                 4               4
#>  8                 3               3
#>  9                 5              NA
#> 10                 4               4
#> # ℹ 1,190 more rows
WoJ %>%
setna_scale(value = c(2, 3, 4), overwrite = TRUE)
#> NOTE: No variables provided. All columns that fit the value type will receive updates to their missing values (NA).FALSE
#> # A tibble: 1,200 × 15
#>    country   reach employment temp_contract autonomy_selection autonomy_emphasis
#>  * <fct>     <fct> <chr>      <fct>                      <dbl>             <dbl>
#>  1 Germany   Nati… Full-time  Permanent                      5                NA
#>  2 Germany   Nati… Full-time  Permanent                     NA                NA
#>  3 Switzerl… Regi… Full-time  Permanent                     NA                NA
#>  4 Switzerl… Local Part-time  Permanent                     NA                 5
#>  5 Austria   Nati… Part-time  Permanent                     NA                NA
#>  6 Switzerl… Local Freelancer NA                            NA                NA
#>  7 Germany   Local Full-time  Permanent                     NA                NA
#>  8 Denmark   Nati… Full-time  Permanent                     NA                NA
#>  9 Switzerl… Local Full-time  Permanent                      5                 5
#> 10 Denmark   Nati… Full-time  Permanent                     NA                NA
#> # ℹ 1,190 more rows
#> # ℹ 9 more variables: ethics_1 <dbl>, ethics_2 <dbl>, ethics_3 <dbl>,
#> #   ethics_4 <dbl>, work_experience <dbl>, trust_parliament <dbl>,
#> #   trust_government <dbl>, trust_parties <dbl>, trust_politicians <dbl>
WoJ %>%
dplyr::select(country) %>% setna_scale(country, value = "Germany")
#> # A tibble: 1,200 × 2
#>    country     country_na 
#>  * <fct>       <fct>      
#>  1 Germany     NA         
#>  2 Germany     NA         
#>  3 Switzerland Switzerland
#>  4 Switzerland Switzerland
#>  5 Austria     Austria    
#>  6 Switzerland Switzerland
#>  7 Germany     NA         
#>  8 Denmark     Denmark    
#>  9 Switzerland Switzerland
#> 10 Denmark     Denmark    
#> # ℹ 1,190 more rows
WoJ %>%
dplyr::select(country) %>% setna_scale(country, value = c("Germany", "Switzerland"))
#> # A tibble: 1,200 × 2
#>    country     country_na
#>  * <fct>       <fct>     
#>  1 Germany     NA        
#>  2 Germany     NA        
#>  3 Switzerland NA        
#>  4 Switzerland NA        
#>  5 Austria     Austria   
#>  6 Switzerland NA        
#>  7 Germany     NA        
#>  8 Denmark     Denmark   
#>  9 Switzerland NA        
#> 10 Denmark     Denmark   
#> # ℹ 1,190 more rows
```
