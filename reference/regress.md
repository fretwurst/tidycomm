# Compute linear regression

Computes linear regression for all independent variables on the
specified dependent variable. Linear modeling of multiple independent
variables uses stepwise regression modeling. If specified, preconditions
for (multi-)collinearity and for homoscedasticity are checked.

## Usage

``` r
regress(
  data,
  dependent_var,
  ...,
  check_independenterrors = FALSE,
  check_multicollinearity = FALSE,
  check_homoscedasticity = FALSE
)
```

## Arguments

- data:

  a [tibble](https://tibble.tidyverse.org/reference/tibble-package.html)
  or a
  [tdcmm](https://github.com/tidycomm/tidycomm/reference/tdcmm-class.md)
  model

- dependent_var:

  The dependent variable on which the linear model is fitted. Specify as
  column name.

- ...:

  Independent variables to take into account as (one or many) predictors
  for the dependent variable. Specify as column names. At least one has
  to be specified.

- check_independenterrors:

  if set, the independence of errors among any two cases is being
  checked using a Durbin-Watson test

- check_multicollinearity:

  if set, multicollinearity among all specified independent variables is
  being checked using the variance inflation factor (VIF) and the
  tolerance (1/VIF); this check can only be performed if at least two
  independent variables are provided, and all provided variables need to
  be numeric

- check_homoscedasticity:

  if set, homoscedasticity is being checked using a Breusch-Pagan test

## Value

a [tdcmm](https://github.com/tidycomm/tidycomm/reference/tdcmm-class.md)
model

## Examples

``` r
WoJ %>% regress(autonomy_selection, ethics_1)
#> # A tibble: 2 × 6
#>   Variable          B StdErr    beta     t       p
#> * <chr>         <dbl>  <dbl>   <dbl> <dbl>   <dbl>
#> 1 (Intercept)  3.99   0.0481 NA      82.9  0      
#> 2 ethics_1    -0.0689 0.0259 -0.0766 -2.66 0.00798
#> # F(1, 1195) = 7.061023, p = 0.007983, R-square = 0.005874
WoJ %>% regress(autonomy_selection, work_experience, trust_government)
#> # A tibble: 3 × 6
#>   Variable              B  StdErr    beta     t         p
#> * <chr>             <dbl>   <dbl>   <dbl> <dbl>     <dbl>
#> 1 (Intercept)      3.52   0.0906  NA      38.8  3.02e-213
#> 2 work_experience  0.0121 0.00211  0.164   5.72 1.35e-  8
#> 3 trust_government 0.0501 0.0271   0.0531  1.85 6.49e-  2
#> # F(2, 1181) = 17.400584, p = 0.000000, R-square = 0.028624
```
