# Compute linear regression

Computes linear regression for all independent variables on the
specified dependent variable. Linear modeling of multiple independent
variables uses stepwise regression modeling. If specified, preconditions
for (multi-)collinearity and for homoscedasticity are checked.

## Usage

``` r
knit_regress_table(
  x,
  digits = 2,
  R2adj = FALSE,
  CIs = TRUE,
  cap = NULL,
  B = B,
  LL = LL,
  UL = UL,
  beta_LL_compare = NULL,
  beta_UL_compare = NULL,
  beta_LL = NULL,
  beta_UL = NULL
)
```

## Arguments

- x:

  a
  [tdcmm](https://github.com/tidycomm/tidycomm/reference/tdcmm-class.md)
  model

- digits:

  the decimal digits

- R2adj:

  Show R^2adj in the table footnote. Default is FALSE.

- CIs:

  show the Confidence intervals or not. Default to TRUE.

- cap:

  Set the caption. Default is NULL, because in Quarto its better to set
  the tbl-cap in the chunk options

- B:

  Name of the coefficient column.

- LL:

  Name of the lower confidence interval column.

- UL:

  Name of the upper confidence interval column.

- beta_LL_compare:

  Name of the lower comparison column for beta.

- beta_UL_compare:

  Name of the upper comparison column for beta.

- beta_LL:

  Name of the lower beta confidence interval column.

- beta_UL:

  Name of the upper beta confidence interval column.

## Value

a gt-table

## Examples

``` r
WoJ |> regress(autonomy_selection, ethics_1) |>
  knit_regress_table()
#> # A tibble: 2 × 6
#>   Variable          B StdErr    beta     t       p
#> * <chr>         <dbl>  <dbl>   <dbl> <dbl>   <dbl>
#> 1 (Intercept)  3.99   0.0481 NA      82.9  0      
#> 2 ethics_1    -0.0689 0.0259 -0.0766 -2.66 0.00798
#> # F(1, 1195) = 7.061023, p = 0.007983, R-square = 0.005874
WoJ %>% regress(autonomy_selection, work_experience, trust_government) |>
  knit_regress_table(digits = 3, CIs = FALSE, cap = "Regression on Autonomy Selection")
#> # A tibble: 3 × 6
#>   Variable              B  StdErr    beta     t         p
#> * <chr>             <dbl>   <dbl>   <dbl> <dbl>     <dbl>
#> 1 (Intercept)      3.52   0.0906  NA      38.8  3.02e-213
#> 2 work_experience  0.0121 0.00211  0.164   5.72 1.35e-  8
#> 3 trust_government 0.0501 0.0271   0.0531  1.85 6.49e-  2
#> # F(2, 1181) = 17.400584, p = 0.000000, R-square = 0.028624
```
