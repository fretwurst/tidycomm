# Tabulate frequencies

Tabulates frequencies for one or more categorical variable, including
relative, and cumulative frequencies.

## Usage

``` r
tab_frequencies(data, ...)
```

## Arguments

- data:

  a [tibble](https://tibble.tidyverse.org/reference/tibble-package.html)
  or a
  [tdcmm](https://github.com/tidycomm/tidycomm/reference/tdcmm-class.md)
  model

- ...:

  Variables to tabulate

## Value

a [tdcmm](https://github.com/tidycomm/tidycomm/reference/tdcmm-class.md)
model

## See also

Other categorical:
[`crosstab()`](https://github.com/tidycomm/tidycomm/reference/crosstab.md),
[`knit_crosstab()`](https://github.com/tidycomm/tidycomm/reference/knit_crosstab.md),
[`knit_frequencies()`](https://github.com/tidycomm/tidycomm/reference/knit_frequencies.md)

## Examples

``` r
WoJ %>% tab_frequencies(employment)
#> # A tibble: 3 × 5
#>   employment     n percent cum_n cum_percent
#> * <chr>      <int>   <dbl> <int>       <dbl>
#> 1 Freelancer   172   0.143   172       0.143
#> 2 Full-time    902   0.752  1074       0.895
#> 3 Part-time    126   0.105  1200       1    
WoJ %>% tab_frequencies(employment, country)
#> # A tibble: 15 × 6
#>    employment country         n percent cum_n cum_percent
#>  * <chr>      <fct>       <int>   <dbl> <int>       <dbl>
#>  1 Freelancer Austria        16 0.0133     16      0.0133
#>  2 Freelancer Denmark        85 0.0708    101      0.0842
#>  3 Freelancer Germany        29 0.0242    130      0.108 
#>  4 Freelancer Switzerland    10 0.00833   140      0.117 
#>  5 Freelancer UK             32 0.0267    172      0.143 
#>  6 Full-time  Austria       165 0.138     337      0.281 
#>  7 Full-time  Denmark       275 0.229     612      0.51  
#>  8 Full-time  Germany       139 0.116     751      0.626 
#>  9 Full-time  Switzerland   154 0.128     905      0.754 
#> 10 Full-time  UK            169 0.141    1074      0.895 
#> 11 Part-time  Austria        26 0.0217   1100      0.917 
#> 12 Part-time  Denmark        16 0.0133   1116      0.93  
#> 13 Part-time  Germany         5 0.00417  1121      0.934 
#> 14 Part-time  Switzerland    69 0.0575   1190      0.992 
#> 15 Part-time  UK             10 0.00833  1200      1     
```
