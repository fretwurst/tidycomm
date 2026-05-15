# Describe numeric variables

Describe numeric variables by several measures of central tendency and
variability. If no variables are specified, all numeric (integer or
double) variables are described.

## Usage

``` r
describe(data, ..., na.rm = TRUE)
```

## Arguments

- data:

  a [tibble](https://tibble.tidyverse.org/reference/tibble-package.html)
  or a
  [tdcmm](https://github.com/tidycomm/tidycomm/reference/tdcmm-class.md)
  model

- ...:

  Variables to describe (column names). Leave empty to describe all
  numeric variables in data.

- na.rm:

  a logical value indicating whether `NA` values should be stripped
  before the computation proceeds. Defaults to `TRUE`.

## Value

a [tdcmm](https://github.com/tidycomm/tidycomm/reference/tdcmm-class.md)
model

## Details

- N: number of valid cases (i.e., all but missing)

- Missing: number of NA cases

- M: [mean](https://rdrr.io/r/base/mean.html) average

- SD: standard deviation, [sd](https://rdrr.io/r/stats/sd.html)

- Min: minimum value, [min](https://rdrr.io/r/base/Extremes.html)

- Q25: 25% quantile, [quantile](https://rdrr.io/r/stats/quantile.html)

- Mdn: [median](https://rdrr.io/r/stats/median.html) average, same as
  50% quantile

- Q75: 75% quantile, [quantile](https://rdrr.io/r/stats/quantile.html)

- Max: maximum value, [max](https://rdrr.io/r/base/Extremes.html)

- Range: difference between Min and Max

- CI_95_LL: \\M - Q(0.975) \times \frac{SD}{\sqrt{N}}\\ where
  \\Q(0.975)\\ denotes Student t's
  [stats::quantile](https://rdrr.io/r/stats/quantile.html) function with
  a probability of \\0.975\\ and \\N-1\\ degrees of freedom

- CI_95_UL: \\M + Q(0.975) \times \frac{SD}{\sqrt{N}}\\ where
  \\Q(0.975)\\ denotes Student t's
  [stats::quantile](https://rdrr.io/r/stats/quantile.html) function with
  a probability of \\0.975\\ and \\N-1\\ degrees of freedom

- Skewness: traditional Fisher-Pearson coefficient of skewness of valid
  cases as per \\\frac{\frac{1}{N} \sum\limits\_{i=1}^N
  (x\_{i}-\overline{x})^3}{\[\frac{1}{N}\sum\limits\_{i=1}^N
  (x\_{i}-\overline{x})^2\]^{3/2}}\\ where \\\overline{x}\\ denotes
  \\M\\, following Doane & Seward (2011, p. 6, 1a). See DOI
  [doi:10.1080/10691898.2011.11889611](https://doi.org/10.1080/10691898.2011.11889611)
  .

- Kurtosis: empirical sample kurtosis (i.e., standardized fourth
  population moment about the mean) as per \\\frac{\sum
  (x-\overline{x})^4 / N}{(\sum (x-\overline{x})^2 / N)^2}\\, following
  DeCarlo (1997, p. 292, b2). See DOI
  [doi:10.1037/1082-989X.2.3.292](https://doi.org/10.1037/1082-989X.2.3.292)
  .

## See also

Other descriptives:
[`describe_cat()`](https://github.com/tidycomm/tidycomm/reference/describe_cat.md),
[`tab_percentiles()`](https://github.com/tidycomm/tidycomm/reference/tab_percentiles.md)

## Examples

``` r
WoJ %>% describe(autonomy_selection, autonomy_emphasis, work_experience)
#> # A tibble: 3 × 15
#>   Variable            N Missing     M     SD   Min   Q25   Mdn   Q75   Max Range
#> * <chr>           <int>   <int> <dbl>  <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1 autonomy_selec…  1197       3  3.88  0.803     1     4     4     4     5     4
#> 2 autonomy_empha…  1195       5  4.08  0.793     1     4     4     5     5     4
#> 3 work_experience  1187      13 17.8  10.9       1     8    17    25    53    52
#> # ℹ 4 more variables: CI_95_LL <dbl>, CI_95_UL <dbl>, Skewness <dbl>,
#> #   Kurtosis <dbl>
fbposts %>% describe(n_pictures)
#> # A tibble: 1 × 15
#>   Variable       N Missing     M    SD   Min   Q25   Mdn   Q75   Max Range
#> * <chr>      <int>   <int> <dbl> <dbl> <int> <dbl> <dbl> <dbl> <int> <int>
#> 1 n_pictures   270       0 0.952  1.47     0     0     1     1    12    12
#> # ℹ 4 more variables: CI_95_LL <dbl>, CI_95_UL <dbl>, Skewness <dbl>,
#> #   Kurtosis <dbl>
```
