# Perform an intercoder reliability test

Performs an intercoder reliability test by computing various intercoder
reliability estimates for the included variables

## Usage

``` r
test_icr(
  data,
  unit_var,
  coder_var,
  ...,
  levels = NULL,
  na.omit = FALSE,
  agreement = TRUE,
  holsti = TRUE,
  kripp_alpha = TRUE,
  cohens_kappa = FALSE,
  fleiss_kappa = FALSE,
  brennan_prediger = FALSE,
  lotus = FALSE,
  s_lotus = FALSE,
  check_disagreements = FALSE,
  text_var = NULL,
  check_pairs = FALSE
)
```

## Arguments

- data:

  a [tibble](https://tibble.tidyverse.org/reference/tibble-package.html)
  or a
  [tdcmm](https://github.com/tidycomm/tidycomm/reference/tdcmm-class.md)
  model

- unit_var:

  Variable with unit identifiers

- coder_var:

  Variable with coder identifiers

- ...:

  Variables to compute intercoder reliability estimates for. Leave empty
  to compute for all variables (excluding `unit_var` and \`coder_var“)
  in data.

- levels:

  Optional named vector with levels of test variables

- na.omit:

  Logical indicating whether `NA` values should be stripped before
  computation. Defaults to `FALSE`.

- agreement:

  Logical indicating whether simple percent agreement should be
  computed. Defaults to `TRUE`.

- holsti:

  Logical indicating whether Holsti's reliability estimate (mean
  pairwise agreement) should be computed. Defaults to `TRUE`.

- kripp_alpha:

  Logical indicating whether Krippendorff's Alpha should be computed.
  Defaults to `TRUE`.

- cohens_kappa:

  Logical indicating whether Cohen's Kappa should be computed. Defaults
  to `FALSE`.

- fleiss_kappa:

  Logical indicating whether Fleiss' Kappa should be computed. Defaults
  to `FALSE`.

- brennan_prediger:

  Logical indicating whether Brennan & Prediger's Kappa should be
  computed (extension to 3+ coders as proposed by von Eye (2006)).
  Defaults to `FALSE`.

- lotus:

  Logical indicating whether Fretwurst's Lotus should be computed.
  Defaults to `FALSE`

- s_lotus:

  Logical indicating whether Fretwurst's standardized Lotus (S-Lotus)
  should be computed. Defaults to `FALSE`.

- check_disagreements:

  Logical, indicates whether to check for intercoder disagreements. This
  must be set to TRUE to use the `text_var` parameter, as `text_var` is
  used specifically for evaluating reasons behind intercoder
  disagreements. Defaults to `FALSE`. This is mutually exclusive with
  `check_pairs`.

- text_var:

  Optional, specifies the name of the text variable that contains
  annotated data. This variable is used to facilitate the analysis of
  reasons for intercoder disagreements. This parameter should only be
  provided if `check_disagreements` is `TRUE`. Defaults to `NULL`.

- check_pairs:

  Logical, indicates whether to compute and compare intercoder
  reliability for each pair of coders. Defaults to `FALSE`. This is
  mutually exclusive with `check_disagreements`.

## Value

a [tdcmm](https://github.com/tidycomm/tidycomm/reference/tdcmm-class.md)
model

## References

Brennan, R. L., & Prediger, D. J. (1981). Coefficient Kappa: Some uses,
misuses, and alternatives. Educational and Psychological Measurement,
41(3), 687-699. https://doi.org/10.1177/001316448104100307

Cohen, J. (1960). A coefficient of agreement for nominal scales.
Educational and Psychological Measurement, 20(1), 37-46.
https://doi.org/10.1177/001316446002000104

Fleiss, J. L. (1971). Measuring nominal scale agreement among many
raters. Psychological Bulletin, 76(5), 378-382.
https://doi.org/10.1037/h0031619

Fretwurst, B. (2015). Reliabilität und Validität von Inhaltsanalysen.
Mit Erläuterungen zur Berechnung des Reliabilitätskoeffizienten „Lotus“
mit SPSS. In W. Wirth, K. Sommer, M. Wettstein, & J. Matthes (Ed.),
Qualitätskriterien in der Inhaltsanalyse (S. 176–203). Herbert von
Halem.

Krippendorff, K. (2011). Computing Krippendorff's Alpha-Reliability.
Retrieved from http://repository.upenn.edu/asc_papers/43

von Eye, A. (2006). An Alternative to Cohen's Kappa. European
Psychologist, 11(1), 12-24. https://doi.org/10.1027/1016-9040.11.1.12

## Examples

``` r
fbposts %>% test_icr(post_id, coder_id, pop_elite, pop_othering)
#> # A tibble: 2 × 8
#>   Variable     n_Units n_Coders n_Categories Level   Agreement Holstis_CR
#> * <chr>          <int>    <int>        <int> <chr>       <dbl>      <dbl>
#> 1 pop_elite         45        6            6 nominal     0.733      0.861
#> 2 pop_othering      45        6            4 nominal     0.867      0.945
#> # ℹ 1 more variable: Krippendorffs_Alpha <dbl>
fbposts %>% test_icr(post_id, coder_id, levels = c(n_pictures = "ordinal"), fleiss_kappa = TRUE)
#> # A tibble: 5 × 9
#>   Variable     n_Units n_Coders n_Categories Level   Agreement Holstis_CR
#> * <chr>          <int>    <int>        <int> <chr>       <dbl>      <dbl>
#> 1 type              45        6            4 nominal     1          1    
#> 2 n_pictures        45        6            7 ordinal     0.822      0.930
#> 3 pop_elite         45        6            6 nominal     0.733      0.861
#> 4 pop_people        45        6            2 nominal     0.778      0.916
#> 5 pop_othering      45        6            4 nominal     0.867      0.945
#> # ℹ 2 more variables: Krippendorffs_Alpha <dbl>, Fleiss_Kappa <dbl>
```
