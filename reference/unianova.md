# Compute one-way ANOVAs

Computes one-way ANOVAs for one group variable and specified test
variables. If no variables are specified, all numeric (integer or
double) variables are used. A Levene's test will automatically determine
whether a classic ANOVA is used. Otherwise Welch's ANOVA with a
(Satterthwaite's) approximation to the degrees of freedom is used.

## Usage

``` r
unianova(data, group_var, ..., descriptives = FALSE, post_hoc = FALSE)
```

## Arguments

- data:

  a [tibble](https://tibble.tidyverse.org/reference/tibble-package.html)
  or a
  [tdcmm](https://github.com/tidycomm/tidycomm/reference/tdcmm-class.md)
  model

- group_var:

  group variable (column name)

- ...:

  test variables (column names). Leave empty to compute ANOVAs for all
  numeric variables in data.

- descriptives:

  a logical indicating whether descriptive statistics (mean & standard
  deviation) for all group levels should be added to the returned
  tibble. Defaults to `FALSE`.

- post_hoc:

  a logical value indicating whether post-hoc tests should be performed.
  Tukey's HSD is employed when the assumption of equal variances is met,
  whereas the Games-Howell test is automatically applied when this
  assumption is violated. The results of the post-hoc test will be added
  to a list column in the resulting tibble.

## Value

a [tdcmm](https://github.com/tidycomm/tidycomm/reference/tdcmm-class.md)
model

## Examples

``` r
WoJ %>% unianova(employment, autonomy_selection, autonomy_emphasis)
#> # A tibble: 2 × 9
#>   Variable            F df_num df_denom     p omega_squared eta_squared Levene_p
#> * <chr>           <num>  <dbl>    <dbl> <num>     <num:.3!>   <num:.3!>    <dbl>
#> 1 autonomy_selec… 2.012      2      251 0.136         0.002      NA        0    
#> 2 autonomy_empha… 5.861      2     1192 0.003        NA           0.010    0.175
#> # ℹ 1 more variable: var_equal <chr>
WoJ %>% unianova(employment, descriptives = TRUE, post_hoc = TRUE)
#> # A tibble: 11 × 16
#>    Variable                  F df_num df_denom     p omega_squared `M_Full-time`
#>  * <chr>              <num:.3>  <dbl>    <dbl> <num>     <num:.3!>         <dbl>
#>  1 autonomy_selection    2.012      2      251 0.136         0.002          3.90
#>  2 autonomy_emphasis     5.861      2     1192 0.003        NA              4.12
#>  3 ethics_1              2.171      2     1197 0.115        NA              1.62
#>  4 ethics_2              2.204      2     1197 0.111        NA              3.24
#>  5 ethics_3              5.823      2      253 0.003         0.007          2.39
#>  6 ethics_4              3.453      2     1197 0.032        NA              2.58
#>  7 work_experience       3.739      2      240 0.025         0.006         17.5 
#>  8 trust_parliament      1.527      2     1197 0.218        NA              3.06
#>  9 trust_government     12.864      2     1197 0.000        NA              2.82
#> 10 trust_parties         0.842      2     1197 0.431        NA              2.42
#> 11 trust_politicians     0.328      2     1197 0.721        NA              2.52
#> # ℹ 9 more variables: `SD_Full-time` <dbl>, `M_Part-time` <dbl>,
#> #   `SD_Part-time` <dbl>, M_Freelancer <dbl>, SD_Freelancer <dbl>,
#> #   post_hoc <list>, eta_squared <num:.3!>, Levene_p <dbl>, var_equal <chr>
if (FALSE) { # \dontrun{
WoJ %>% unianova(employment)
} # }
```
