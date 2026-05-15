# Recode one or more categorical variables into new categories

This function transforms one or more categorical variables into new
categories based on specified mapping. For unmatched cases not specified
in the mapping, a default value can be assigned. Missing values are
retained.

## Usage

``` r
recode_cat_scale(
  data,
  ...,
  assign = NULL,
  other = NA,
  overwrite = FALSE,
  name = NULL
)
```

## Arguments

- data:

  A tibble or a tdcmm model.

- ...:

  Variables to recode.

- assign:

  A named vector where names are the old values and values are the new
  values to be assigned.

- other:

  The value for unmatched cases. By default, it is `NA`. This parameter
  is used to assign a value to cases that do not match any of the keys
  in the `assign` vector.

- overwrite:

  Logical. If `TRUE`, it overwrites the original variable(s). You cannot
  specify both 'name' and 'overwrite' parameters simultaneously.

- name:

  The name of the new variable(s). If not specified, this is the same
  name as the provided variable(s) but suffixed with `_rec`.

## Value

A [tdcmm](https://github.com/tidycomm/tidycomm/reference/tdcmm-class.md)
model or a tibble.

## See also

Other scaling:
[`categorize_scale()`](https://github.com/tidycomm/tidycomm/reference/categorize_scale.md),
[`center_scale()`](https://github.com/tidycomm/tidycomm/reference/center_scale.md),
[`dummify_scale()`](https://github.com/tidycomm/tidycomm/reference/dummify_scale.md),
[`minmax_scale()`](https://github.com/tidycomm/tidycomm/reference/minmax_scale.md),
[`reverse_scale()`](https://github.com/tidycomm/tidycomm/reference/reverse_scale.md),
[`setna_scale()`](https://github.com/tidycomm/tidycomm/reference/setna_scale.md),
[`z_scale()`](https://github.com/tidycomm/tidycomm/reference/z_scale.md)

## Examples

``` r
WoJ %>%
recode_cat_scale(country,
assign = c("Germany" = 1, "Switzerland" = 2), overwrite = TRUE)
#> The following unassigned values were found in country : Austria, Denmark, UK . They were recoded to the 'other' value ( NA ).
#> # A tibble: 1,200 × 15
#>    country reach   employment temp_contract autonomy_selection autonomy_emphasis
#>  * <fct>   <fct>   <chr>      <fct>                      <dbl>             <dbl>
#>  1 1       Nation… Full-time  Permanent                      5                 4
#>  2 1       Nation… Full-time  Permanent                      3                 4
#>  3 2       Region… Full-time  Permanent                      4                 4
#>  4 2       Local   Part-time  Permanent                      4                 5
#>  5 NA      Nation… Part-time  Permanent                      4                 4
#>  6 2       Local   Freelancer NA                             4                 4
#>  7 1       Local   Full-time  Permanent                      4                 4
#>  8 NA      Nation… Full-time  Permanent                      3                 3
#>  9 2       Local   Full-time  Permanent                      5                 5
#> 10 NA      Nation… Full-time  Permanent                      2                 4
#> # ℹ 1,190 more rows
#> # ℹ 9 more variables: ethics_1 <dbl>, ethics_2 <dbl>, ethics_3 <dbl>,
#> #   ethics_4 <dbl>, work_experience <dbl>, trust_parliament <dbl>,
#> #   trust_government <dbl>, trust_parties <dbl>, trust_politicians <dbl>
WoJ %>%
recode_cat_scale(country,
assign = c("Germany" = "german", "Switzerland" = "swiss"), other = "other",
overwrite = TRUE)
#> The following unassigned values were found in country : Austria, Denmark, UK . They were recoded to the 'other' value ( other ).
#> # A tibble: 1,200 × 15
#>    country reach   employment temp_contract autonomy_selection autonomy_emphasis
#>  * <fct>   <fct>   <chr>      <fct>                      <dbl>             <dbl>
#>  1 german  Nation… Full-time  Permanent                      5                 4
#>  2 german  Nation… Full-time  Permanent                      3                 4
#>  3 swiss   Region… Full-time  Permanent                      4                 4
#>  4 swiss   Local   Part-time  Permanent                      4                 5
#>  5 other   Nation… Part-time  Permanent                      4                 4
#>  6 swiss   Local   Freelancer NA                             4                 4
#>  7 german  Local   Full-time  Permanent                      4                 4
#>  8 other   Nation… Full-time  Permanent                      3                 3
#>  9 swiss   Local   Full-time  Permanent                      5                 5
#> 10 other   Nation… Full-time  Permanent                      2                 4
#> # ℹ 1,190 more rows
#> # ℹ 9 more variables: ethics_1 <dbl>, ethics_2 <dbl>, ethics_3 <dbl>,
#> #   ethics_4 <dbl>, work_experience <dbl>, trust_parliament <dbl>,
#> #   trust_government <dbl>, trust_parties <dbl>, trust_politicians <dbl>
WoJ %>%
recode_cat_scale(ethics_1, ethics_2,
assign = c(`1` = 5, `2` = 4, `3` = 3, `4` = 2, `5` = 1), other = 6, overwrite = TRUE)
#> # A tibble: 1,200 × 15
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
#> # ℹ 9 more variables: ethics_1 <fct>, ethics_2 <fct>, ethics_3 <dbl>,
#> #   ethics_4 <dbl>, work_experience <dbl>, trust_parliament <dbl>,
#> #   trust_government <dbl>, trust_parties <dbl>, trust_politicians <dbl>
WoJ %>%
recode_cat_scale(ethics_1, ethics_2,
assign = c(`1` = "very low", `2` = "low", `3` = "medium", `4` = "high", `5` = "very high"),
overwrite = TRUE)
#> # A tibble: 1,200 × 15
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
#> # ℹ 9 more variables: ethics_1 <fct>, ethics_2 <fct>, ethics_3 <dbl>,
#> #   ethics_4 <dbl>, work_experience <dbl>, trust_parliament <dbl>,
#> #   trust_government <dbl>, trust_parties <dbl>, trust_politicians <dbl>
WoJ %>%
dplyr::select(temp_contract) %>% recode_cat_scale(temp_contract,
assign = c(`Permanent` = "P", `Temporary` = "T"), other = "O")
#> # A tibble: 1,200 × 2
#>    temp_contract temp_contract_rec
#>  * <fct>         <fct>            
#>  1 Permanent     P                
#>  2 Permanent     P                
#>  3 Permanent     P                
#>  4 Permanent     P                
#>  5 Permanent     P                
#>  6 NA            NA               
#>  7 Permanent     P                
#>  8 Permanent     P                
#>  9 Permanent     P                
#> 10 Permanent     P                
#> # ℹ 1,190 more rows
```
