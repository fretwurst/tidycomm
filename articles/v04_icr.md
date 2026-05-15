# Intercoder reliability tests

Tidycomm provides the
[`test_icr()`](https://github.com/tidycomm/tidycomm/reference/test_icr.md)
function to conveniently compute intercoder reliability tests for
several variables and reliability estimates at the same time.

Test data has to be structured in a long format, with one column
indicating the unit (e.g., article 1, article 2, etc.), one column
indicating the coder (either by a string or a numeric ID), and one
column each per coded variable to test.

For demonstration purposes, we will use the `fbposts` data included in
Tidycomm that consists of 45 political Facebook posts (identified by
`post_id`) coded by six coders (identified by `coder_id`) for various
formal (post type, number of pictures used in post) and populism-related
(attacks on elites, references to ‘the people’, othering) features:

``` r

fbposts
#> # A tibble: 270 × 7
#>    post_id coder_id type  n_pictures pop_elite pop_people pop_othering
#>      <int>    <int> <chr>      <int>     <int>      <int>        <int>
#>  1       1        1 photo          1         0          0            0
#>  2       1        2 photo          1         0          0            0
#>  3       1        3 photo          1         0          0            0
#>  4       1        4 photo          1         0          0            0
#>  5       1        5 photo          1         0          0            0
#>  6       1        6 photo          1         0          0            0
#>  7       2        1 photo          1         0          0            0
#>  8       2        2 photo          1         0          0            0
#>  9       2        3 photo          1         0          0            0
#> 10       2        4 photo          1         0          0            0
#> # ℹ 260 more rows
```

## Basic use

[`test_icr()`](https://github.com/tidycomm/tidycomm/reference/test_icr.md)
computes various intercoder reliability estimates for all specified
variables. The first two arguments (in a pipe) are the unit-identifying
variable and the coder-identifying variable, followed by the test
variables:

``` r

fbposts %>% 
  test_icr(post_id, coder_id, pop_elite, pop_people, pop_othering)
#> # A tibble: 3 × 8
#>   Variable     n_Units n_Coders n_Categories Level   Agreement Holstis_CR
#> * <chr>          <int>    <int>        <int> <chr>       <dbl>      <dbl>
#> 1 pop_elite         45        6            6 nominal     0.733      0.861
#> 2 pop_people        45        6            2 nominal     0.778      0.916
#> 3 pop_othering      45        6            4 nominal     0.867      0.945
#> # ℹ 1 more variable: Krippendorffs_Alpha <dbl>
```

If no test variables are specified, all variables in the dataset
(excluding the unit and coder variables) will be tested:

``` r

fbposts %>% 
  test_icr(post_id, coder_id)
#> # A tibble: 5 × 8
#>   Variable     n_Units n_Coders n_Categories Level   Agreement Holstis_CR
#> * <chr>          <int>    <int>        <int> <chr>       <dbl>      <dbl>
#> 1 type              45        6            4 nominal     1          1    
#> 2 n_pictures        45        6            7 nominal     0.822      0.930
#> 3 pop_elite         45        6            6 nominal     0.733      0.861
#> 4 pop_people        45        6            2 nominal     0.778      0.916
#> 5 pop_othering      45        6            4 nominal     0.867      0.945
#> # ℹ 1 more variable: Krippendorffs_Alpha <dbl>
```

## Reliability estimates

Currently,
[`test_icr()`](https://github.com/tidycomm/tidycomm/reference/test_icr.md)
supports the following reliability estimates:

- `agreement`: Simple percent agreement.
- `holsti`: Holsti’s $`CR`$ (mean pairwise percent agreement).
- `kripp_alpha`: Krippendorff’s $`\alpha`$.
- `cohens_kappa`: Cohen’s $`\kappa`$ (only available for two coders).
- `fleiss_kappa`: Fleiss’ $`\kappa`$.
- `brennan_prediger`: Brennan & Prediger’s $`\kappa`$ (for more than two
  coders, [von Eye’s (2006)](https://doi.org/10.1027/1016-9040.11.1.12)
  proposed extension to multiple coders is computed).

By default,
[`test_icr()`](https://github.com/tidycomm/tidycomm/reference/test_icr.md)
will output simple percent agreement, Holsti’s $`CR`$, and
Krippendorff’s $`\alpha`$ as reliability estimates. You can add other
estimates by setting their name to `TRUE` in the function call (and
remove the default ones by setting them to `FALSE`):

``` r

fbposts %>% 
  test_icr(post_id, coder_id, fleiss_kappa = TRUE, agreement = FALSE)
#> # A tibble: 5 × 8
#>   Variable    n_Units n_Coders n_Categories Level Holstis_CR Krippendorffs_Alpha
#> * <chr>         <int>    <int>        <int> <chr>      <dbl>               <dbl>
#> 1 type             45        6            4 nomi…      1                   1    
#> 2 n_pictures       45        6            7 nomi…      0.930               0.880
#> 3 pop_elite        45        6            6 nomi…      0.861               0.339
#> 4 pop_people       45        6            2 nomi…      0.916               0.287
#> 5 pop_otheri…      45        6            4 nomi…      0.945               0.566
#> # ℹ 1 more variable: Fleiss_Kappa <dbl>
```

## Variable levels

By default,
[`test_icr()`](https://github.com/tidycomm/tidycomm/reference/test_icr.md)
assumes all test variables to be nominal. You can set other variable
levels by passing a named vector of the form
`c(variable_name = "variable_level")` to the `levels` argument.

``` r

fbposts %>% 
  test_icr(post_id, coder_id, levels = c(n_pictures = "ordinal"))
#> # A tibble: 5 × 8
#>   Variable     n_Units n_Coders n_Categories Level   Agreement Holstis_CR
#> * <chr>          <int>    <int>        <int> <chr>       <dbl>      <dbl>
#> 1 type              45        6            4 nominal     1          1    
#> 2 n_pictures        45        6            7 ordinal     0.822      0.930
#> 3 pop_elite         45        6            6 nominal     0.733      0.861
#> 4 pop_people        45        6            2 nominal     0.778      0.916
#> 5 pop_othering      45        6            4 nominal     0.867      0.945
#> # ℹ 1 more variable: Krippendorffs_Alpha <dbl>
```

Nominal test variables can be represented by either integer codes or
string labels, whereas ordinal variables must be represented by integer
codes, and interval/ratio variables must be numeric (integer or float).

Please note that currently only the computation of Krippendorff’s
$`\alpha`$ is influenced by the variable level.

## Missing values

Missing values in intercoder reliability tests can be ambiguous (did the
coder forget to code this variable for this unit, or does the missing
value indicate that none of the categories was deemed fitting?) and
present an obstacle to several reliability estimates (of the currently
implemented estimates, only Krippendorff’s $`\alpha`$ can deal with
missing values).

Thus,
[`test_icr()`](https://github.com/tidycomm/tidycomm/reference/test_icr.md)
will by default respond with a warning when `NA` values are present in
the test variables and output `NA` for all reliability estimates but
Krippendorff’s $`\alpha`$:

``` r

# Introduce some missing values
fbposts$type[1] <- NA
fbposts$type[2] <- NA
fbposts$pop_elite[5] <- NA

fbposts %>% 
  test_icr(post_id, coder_id)
#> Warning: Variable 'type' contains missing values. Consider setting na.omit =
#> TRUE or recoding missing values
#> Warning: Variable 'pop_elite' contains missing values. Consider setting na.omit
#> = TRUE or recoding missing values
#> # A tibble: 5 × 8
#>   Variable     n_Units n_Coders n_Categories Level   Agreement Holstis_CR
#> * <chr>          <int>    <int>        <int> <chr>       <dbl>      <dbl>
#> 1 type              45        6            4 nominal    NA         NA    
#> 2 n_pictures        45        6            7 nominal     0.822      0.930
#> 3 pop_elite         45        6            6 nominal    NA         NA    
#> 4 pop_people        45        6            2 nominal     0.778      0.916
#> 5 pop_othering      45        6            4 nominal     0.867      0.945
#> # ℹ 1 more variable: Krippendorffs_Alpha <dbl>
```

You can set `na.omit = TRUE` to exclude all units with `NA` values for a
specific test variable from the computation for this variable:

``` r

fbposts %>% 
  test_icr(post_id, coder_id, na.omit = TRUE)
#> # A tibble: 5 × 8
#>   Variable     n_Units n_Coders n_Categories Level   Agreement Holstis_CR
#> * <chr>          <int>    <int>        <int> <chr>       <dbl>      <dbl>
#> 1 type              44        6            4 nominal     1          1    
#> 2 n_pictures        45        6            7 nominal     0.822      0.930
#> 3 pop_elite         44        6            6 nominal     0.727      0.858
#> 4 pop_people        45        6            2 nominal     0.778      0.916
#> 5 pop_othering      45        6            4 nominal     0.867      0.945
#> # ℹ 1 more variable: Krippendorffs_Alpha <dbl>
```

## ‘Hidden’ missing values

There are situations in which all units are coded multiple times, but
not by same coders (e.g., in research seminars). Consider the following
sample data:

``` r

data <- tibble::tibble(
  unit = c(1, 1, 2, 2, 3, 3),
  coder = c('a', 'b', 'a', 'c', 'b', 'c'),
  code = c(1, 0, 1, 1, 0, 0)
)

data
#> # A tibble: 6 × 3
#>    unit coder  code
#>   <dbl> <chr> <dbl>
#> 1     1 a         1
#> 2     1 b         0
#> 3     2 a         1
#> 4     2 c         1
#> 5     3 b         0
#> 6     3 c         0
```

Each unit was coded two times, but no coder coded all units. Thus, the
units-coders matrix will contain one `NA` value per (unit) row,
indicating that one coder did not code the respective unit. Setting
`na.omit = TRUE` in
[`test_icr()`](https://github.com/tidycomm/tidycomm/reference/test_icr.md)
will thus result in an empty units-coders matrix:

``` r

data %>%
  test_icr(unit, coder, code, na.omit = TRUE)
#> Error in `map()`:
#> ℹ In index: 1.
#> Caused by error:
#> ! Empty units-coders matrix detected. This is most likely due to none of the units having been coded by all coders. See vignette('v04_icr') for details.
```

If it is not relevant that all units were coded by same coders, consider
setting a variable indicating each *coding* per unit as the `coder_id`:

``` r

data %>% 
  dplyr::group_by(unit) %>% 
  dplyr::mutate(coding = 1:dplyr::n()) %>% 
  dplyr::ungroup() %>% 
  test_icr(unit, coding, code)
#> # A tibble: 1 × 8
#>   Variable n_Units n_Coders n_Categories Level   Agreement Holstis_CR
#> * <chr>      <int>    <int>        <int> <chr>       <dbl>      <dbl>
#> 1 code           3        2            2 nominal     0.667      0.667
#> # ℹ 1 more variable: Krippendorffs_Alpha <dbl>
```
