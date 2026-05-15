# Compute correlation coefficients

Computes correlation coefficients for all combinations of the specified
variables. If no variables are specified, all numeric (integer or
double) variables are used.

## Usage

``` r
correlate(data, ..., method = "pearson", partial = NULL, with = NULL)
```

## Arguments

- data:

  a [tibble](https://tibble.tidyverse.org/reference/tibble-package.html)
  or a
  [tdcmm](https://github.com/tidycomm/tidycomm/reference/tdcmm-class.md)
  model

- ...:

  Variables to compute correlations for (column names). Leave empty to
  compute for all numeric variables in data.

- method:

  a character string indicating which correlation coefficient is to be
  computed. One of "pearson" (default), "kendall", or "spearman"

- partial:

  Specifies a variable to be used as a control in a partial correlation.
  By default, this parameter is set to `NULL`, indicating that no
  control variable is used in the correlation. If used, `with` must be
  set to `NULL` (default).

- with:

  Specifies a focus variable to correlate all other variables with. By
  default, this parameter is set to `NULL`, indicating that no focus
  variable is used in the correlation. If used, `partial` must be set to
  `NULL` (default).

## Value

a [tdcmm](https://github.com/tidycomm/tidycomm/reference/tdcmm-class.md)
model

## Examples

``` r
WoJ %>% correlate(ethics_1, ethics_2, ethics_3)
#> # A tibble: 3 × 6
#>   x        y            r    df        p     n
#> * <chr>    <chr>    <dbl> <int>    <dbl> <int>
#> 1 ethics_1 ethics_2 0.172  1198 2.04e- 9  1200
#> 2 ethics_1 ethics_3 0.165  1198 8.44e- 9  1200
#> 3 ethics_2 ethics_3 0.409  1198 1.05e-49  1200
WoJ %>% correlate()
#> # A tibble: 55 × 6
#>    x                  y                        r    df         p     n
#>  * <chr>              <chr>                <dbl> <int>     <dbl> <int>
#>  1 autonomy_selection autonomy_emphasis  0.644    1192 4.83e-141  1194
#>  2 autonomy_selection ethics_1          -0.0766   1195 7.98e-  3  1197
#>  3 autonomy_selection ethics_2          -0.0274   1195 3.43e-  1  1197
#>  4 autonomy_selection ethics_3          -0.0257   1195 3.73e-  1  1197
#>  5 autonomy_selection ethics_4          -0.0781   1195 6.89e-  3  1197
#>  6 autonomy_selection work_experience    0.161    1182 2.71e-  8  1184
#>  7 autonomy_selection trust_parliament  -0.00840  1195 7.72e-  1  1197
#>  8 autonomy_selection trust_government   0.0414   1195 1.53e-  1  1197
#>  9 autonomy_selection trust_parties      0.0269   1195 3.52e-  1  1197
#> 10 autonomy_selection trust_politicians  0.0109   1195 7.07e-  1  1197
#> # ℹ 45 more rows
WoJ %>% correlate(ethics_1, ethics_2, ethics_3, with = work_experience)
#> # A tibble: 3 × 6
#>   x               y              r    df             p     n
#> * <chr>           <chr>      <dbl> <int>         <dbl> <int>
#> 1 work_experience ethics_1 -0.103   1185 0.000387       1187
#> 2 work_experience ethics_2 -0.168   1185 0.00000000619  1187
#> 3 work_experience ethics_3 -0.0442  1185 0.128          1187
WoJ %>% correlate(autonomy_selection, autonomy_emphasis, partial = work_experience)
#> # A tibble: 1 × 7
#>   x                  y                 z                 r    df         p     n
#> * <chr>              <chr>             <chr>         <dbl> <dbl>     <dbl> <int>
#> 1 autonomy_selection autonomy_emphasis work_experie… 0.637  1178 3.07e-135  1181
WoJ %>% correlate(with = work_experience)
#> Warning: At least one of work_experience and country is not numeric, skipping computation.
#> Warning: At least one of work_experience and reach is not numeric, skipping computation.
#> Warning: At least one of work_experience and employment is not numeric, skipping computation.
#> Warning: At least one of work_experience and temp_contract is not numeric, skipping computation.
#> # A tibble: 10 × 6
#>    x               y                         r    df             p     n
#>  * <chr>           <chr>                 <dbl> <int>         <dbl> <int>
#>  1 work_experience autonomy_selection  0.161    1182 0.0000000271   1184
#>  2 work_experience autonomy_emphasis   0.155    1180 0.0000000887   1182
#>  3 work_experience ethics_1           -0.103    1185 0.000387       1187
#>  4 work_experience ethics_2           -0.168    1185 0.00000000619  1187
#>  5 work_experience ethics_3           -0.0442   1185 0.128          1187
#>  6 work_experience ethics_4           -0.116    1185 0.0000602      1187
#>  7 work_experience trust_parliament   -0.00941  1185 0.746          1187
#>  8 work_experience trust_government   -0.0708   1185 0.0146         1187
#>  9 work_experience trust_parties      -0.0454   1185 0.118          1187
#> 10 work_experience trust_politicians  -0.00976  1185 0.737          1187
```
