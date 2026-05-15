# Categorize numeric variables into categories

This function recodes one or more numeric variables into categorical
variables based on a specified lower end, upper end, and intermediate
breaks. The intervals created include the right endpoint of the
interval. For example, breaks = c(2, 3) with lower_end = 1 and upper_end
= 5 creates intervals from 1 to \<= 2, \>2 to \<= 3, and \>3 to \<= 5.
If the lower or upper ends are not provided, the function defaults to
the minimum and maximum values of the data and issues a warning. This
default behavior is prone to errors, however, because a scale may not
include its actual lower and upper ends which might in turn affect the
recoding process. Hence, it is strongly suggested to manually set the
lower and upper bounds of the original continuous scale.

## Usage

``` r
categorize_scale(
  data,
  ...,
  breaks,
  labels,
  lower_end = NULL,
  upper_end = NULL,
  name = NULL,
  overwrite = FALSE
)
```

## Arguments

- data:

  A tibble or a tdcmm model.

- ...:

  Variables to recode as factor variables in categories. If no variables
  are specified, all numeric columns will be recoded.

- breaks:

  A vector of numeric values specifying the breaks for categorizing the
  data between the lower and upper ends. The breaks define the
  boundaries of the intervals. Setting this parameter is required.

- labels:

  A vector of string labels for each interval. The number of labels must
  match the number of intervals defined by the breaks and lower/upper
  ends.Setting this parameter is required.

- lower_end:

  Optional numeric value specifying the lower end of the scale. If not
  provided, defaults to the minimum value of the data.

- upper_end:

  Optional numeric value specifying the upper end of the scale. If not
  provided, defaults to the maximum value of the data.

- name:

  Optional string specifying the name of the new variable(s). By
  default, the new variable names are the original variable names
  suffixed with `_cat`.

- overwrite:

  Logical indicating whether to overwrite the original variable(s) with
  the new categorical variables. If `TRUE`, the original variable(s) are
  overwritten.

## Value

A modified tibble or tdcmm model with the recoded variables.

## See also

Other scaling:
[`center_scale()`](https://github.com/tidycomm/tidycomm/reference/center_scale.md),
[`dummify_scale()`](https://github.com/tidycomm/tidycomm/reference/dummify_scale.md),
[`minmax_scale()`](https://github.com/tidycomm/tidycomm/reference/minmax_scale.md),
[`recode_cat_scale()`](https://github.com/tidycomm/tidycomm/reference/recode_cat_scale.md),
[`reverse_scale()`](https://github.com/tidycomm/tidycomm/reference/reverse_scale.md),
[`setna_scale()`](https://github.com/tidycomm/tidycomm/reference/setna_scale.md),
[`z_scale()`](https://github.com/tidycomm/tidycomm/reference/z_scale.md)

## Examples

``` r
WoJ %>%
dplyr::select(trust_parliament, trust_politicians) %>%
categorize_scale(trust_parliament, trust_politicians,
lower_end = 1, upper_end = 5, breaks = c(2, 3),
labels = c("Low", "Medium", "High"), overwrite = FALSE)
#> # A tibble: 1,200 × 4
#>    trust_parliament trust_politicians trust_parliament_cat trust_politicians_cat
#>  *            <dbl>             <dbl> <fct>                <fct>                
#>  1                3                 3 Medium               Medium               
#>  2                4                 3 High                 Medium               
#>  3                4                 3 High                 Medium               
#>  4                4                 3 High                 Medium               
#>  5                3                 2 Medium               Low                  
#>  6                4                 2 High                 Low                  
#>  7                2                 2 Low                  Low                  
#>  8                4                 3 High                 Medium               
#>  9                1                 1 Low                  Low                  
#> 10                3                 3 Medium               Medium               
#> # ℹ 1,190 more rows
WoJ %>%
dplyr::select(autonomy_selection) %>%
categorize_scale(autonomy_selection, breaks = c(2, 3, 4),
lower_end = 1, upper_end = 5,
labels = c("Low", "Medium", "High", "Very High"),
name = "autonomy_in_categories")
#> # A tibble: 1,200 × 2
#>    autonomy_selection autonomy_in_categories_1
#>  *              <dbl> <fct>                   
#>  1                  5 Very High               
#>  2                  3 Medium                  
#>  3                  4 High                    
#>  4                  4 High                    
#>  5                  4 High                    
#>  6                  4 High                    
#>  7                  4 High                    
#>  8                  3 Medium                  
#>  9                  5 Very High               
#> 10                  2 Low                     
#> # ℹ 1,190 more rows
```
