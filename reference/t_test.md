# Compute t-tests

Computes t-tests for one group variable and specified test variables. If
no variables are specified, all numeric (integer or double) variables
are used. A Levene's test will automatically determine whether the
pooled variance is used to estimate the variance. Otherwise the Welch
(or Satterthwaite) approximation to the degrees of freedom is used.

## Usage

``` r
t_test(
  data,
  group_var,
  ...,
  var.equal = TRUE,
  paired = FALSE,
  pooled_sd = TRUE,
  levels = NULL,
  case_var = NULL,
  mu = NULL
)
```

## Arguments

- data:

  a [tibble](https://tibble.tidyverse.org/reference/tibble-package.html)
  or a
  [tdcmm](https://github.com/tidycomm/tidycomm/reference/tdcmm-class.md)
  model

- group_var:

  group variable (column name) to specify where to split two samples
  (two-sample t-test) or which variable to compare a one-sample t-test
  on

- ...:

  test variables (column names). Leave empty to compute t-tests for all
  numeric variables in data. Also leave empty for one-sample t-tests.

- var.equal:

  this parameter is deprecated (previously: a logical variable
  indicating whether to treat the two variances as being equal. If
  `TRUE` then the pooled variance is used to estimate the variance
  otherwise the Welch (or Satterthwaite) approximation to the degrees of
  freedom is used. Defaults to `TRUE`).

- paired:

  a logical indicating whether you want a paired t-test. Defaults to
  `FALSE`.

- pooled_sd:

  a logical indicating whether to use the pooled standard deviation in
  the calculation of Cohen's d. Defaults to `TRUE`.

- levels:

  optional: a vector of length two specifying the two levels of the
  group variable.

- case_var:

  optional: case-identifying variable (column name). If you set
  `paired = TRUE`, specifying a case variable will ensure that data are
  properly sorted for a dependent t-test.

- mu:

  optional: a number indicating the *true* value of the mean in the
  general population (\\\mu\\). If set, a one-sample t-test (i.e., a
  location test) is being calculated. Leave to `NULL` to calculate
  two-sample t-test(s).

## Value

a [tdcmm](https://github.com/tidycomm/tidycomm/reference/tdcmm-class.md)
model

## Examples

``` r
WoJ %>% t_test(temp_contract, autonomy_selection, autonomy_emphasis)
#> # A tibble: 2 × 12
#>   Variable M_Permanent SD_Permanent M_Temporary SD_Temporary Delta_M     t    df
#> * <chr>      <num:.3!>    <num:.3!>   <num:.3!>    <num:.3!> <num:.> <num> <dbl>
#> 1 autonom…       3.910        0.755       3.698        0.932   0.212 1.627    56
#> 2 autonom…       4.124        0.768       3.887        0.870   0.237 2.171   995
#> # ℹ 4 more variables: p <num:.3!>, d <num:.3!>, Levene_p <dbl>, var_equal <chr>
WoJ %>% t_test(temp_contract)
#> # A tibble: 11 × 12
#>    Variable     M_Permanent SD_Permanent M_Temporary SD_Temporary Delta_M      t
#>  * <chr>          <num:.3!>    <num:.3!>   <num:.3!>    <num:.3!> <num:.> <num:>
#>  1 autonomy_se…       3.910        0.755       3.698        0.932   0.212  1.627
#>  2 autonomy_em…       4.124        0.768       3.887        0.870   0.237  2.171
#>  3 ethics_1           1.568        0.850       1.981        0.990  -0.414 -3.415
#>  4 ethics_2           3.241        1.263       3.509        1.234  -0.269 -1.510
#>  5 ethics_3           2.369        1.121       2.283        0.928   0.086  0.549
#>  6 ethics_4           2.534        1.239       2.566        1.217  -0.032 -0.185
#>  7 work_experi…      17.707       10.540      11.283       11.821   6.424  4.288
#>  8 trust_parli…       3.073        0.797       3.019        0.772   0.054  0.480
#>  9 trust_gover…       2.870        0.847       2.642        0.811   0.229  1.918
#> 10 trust_parti…       2.430        0.724       2.358        0.736   0.072  0.703
#> 11 trust_polit…       2.533        0.707       2.396        0.689   0.136  1.369
#> # ℹ 5 more variables: df <dbl>, p <num:.3!>, d <num:.3!>, Levene_p <dbl>,
#> #   var_equal <chr>
WoJ %>% t_test(employment, autonomy_selection, autonomy_emphasis,
  levels = c("Full-time", "Freelancer"))
#> # A tibble: 2 × 12
#>   Variable `M_Full-time` `SD_Full-time` M_Freelancer SD_Freelancer Delta_M     t
#> * <chr>        <num:.3!>      <num:.3!>    <num:.3!>     <num:.3!> <num:.> <num>
#> 1 autonom…         3.903          0.782        3.765         0.993   0.139 1.724
#> 2 autonom…         4.118          0.781        3.901         0.852   0.217 3.287
#> # ℹ 5 more variables: df <dbl>, p <num:.3!>, d <num:.3!>, Levene_p <dbl>,
#> #   var_equal <chr>
WoJ %>% t_test(autonomy_selection, mu = 3.62)
#> # A tibble: 1 × 9
#>   Variable               M    SD CI_95_LL CI_95_UL    Mu     t    df        p
#> * <chr>              <dbl> <dbl>    <dbl>    <dbl> <dbl> <dbl> <dbl>    <dbl>
#> 1 autonomy_selection  3.88 0.803     3.83     3.92  3.62  11.0  1196 6.10e-27
```
