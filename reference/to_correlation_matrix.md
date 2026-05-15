# Create correlation matrix

Turns the tibble exported from
[`correlate`](https://github.com/tidycomm/tidycomm/reference/correlate.md)
into a correlation matrix.

## Usage

``` r
to_correlation_matrix(data, verbose = FALSE)
```

## Arguments

- data:

  a
  [tdcmm](https://github.com/tidycomm/tidycomm/reference/tdcmm-class.md)
  model returned from
  [`correlate`](https://github.com/tidycomm/tidycomm/reference/correlate.md)

- verbose:

  A logical, defaulted to `FALSE`. Only applicable when correlating two
  variables. If set to `TRUE`, the function outputs information
  regarding the sample size.

## Value

a [tdcmm](https://github.com/tidycomm/tidycomm/reference/tdcmm-class.md)
model

## Examples

``` r
WoJ %>% correlate() %>% to_correlation_matrix()
#> # A tibble: 11 × 12
#>    r             autonomy_selection autonomy_emphasis ethics_1 ethics_2 ethics_3
#>  * <chr>                      <dbl>             <dbl>    <dbl>    <dbl>    <dbl>
#>  1 autonomy_sel…            1                 0.644   -0.0766  -0.0274   -0.0257
#>  2 autonomy_emp…            0.644             1       -0.114   -0.0337   -0.0297
#>  3 ethics_1                -0.0766           -0.114    1        0.172     0.165 
#>  4 ethics_2                -0.0274           -0.0337   0.172    1         0.409 
#>  5 ethics_3                -0.0257           -0.0297   0.165    0.409     1     
#>  6 ethics_4                -0.0781           -0.127    0.343    0.321     0.273 
#>  7 work_experie…            0.161             0.155   -0.103   -0.168    -0.0442
#>  8 trust_parlia…           -0.00840          -0.00465 -0.0378   0.00161  -0.0486
#>  9 trust_govern…            0.0414            0.0268  -0.102    0.0374   -0.0743
#> 10 trust_parties            0.0269            0.0102  -0.0472   0.0238   -0.0115
#> 11 trust_politi…            0.0109            0.00242 -0.00725  0.0250   -0.0212
#> # ℹ 6 more variables: ethics_4 <dbl>, work_experience <dbl>,
#> #   trust_parliament <dbl>, trust_government <dbl>, trust_parties <dbl>,
#> #   trust_politicians <dbl>
```
