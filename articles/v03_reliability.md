# Adding indices and computing reliability estimates

Tidycomm provides a workflow to quickly add mean/sum indices of several
variables to the dataset and compute reliability estimates for those
added indices:

- [`add_index()`](https://github.com/tidycomm/tidycomm/reference/add_index.md)
  adds a mean or sum index of the specified variables
- [`get_reliability()`](https://github.com/tidycomm/tidycomm/reference/get_reliability.md)
  computes reliability estimates for all added indices

Once again, we will again sample data from the [Worlds of
Journalism](https://worldsofjournalism.org/) 2012-16 study for
demonstration purposes.

``` r

WoJ
#> # A tibble: 1,200 × 15
#>    country   reach employment temp_contract autonomy_selection autonomy_emphasis
#>    <fct>     <fct> <chr>      <fct>                      <dbl>             <dbl>
#>  1 Germany   Nati… Full-time  Permanent                      5                 4
#>  2 Germany   Nati… Full-time  Permanent                      3                 4
#>  3 Switzerl… Regi… Full-time  Permanent                      4                 4
#>  4 Switzerl… Local Part-time  Permanent                      4                 5
#>  5 Austria   Nati… Part-time  Permanent                      4                 4
#>  6 Switzerl… Local Freelancer NA                             4                 4
#>  7 Germany   Local Full-time  Permanent                      4                 4
#>  8 Denmark   Nati… Full-time  Permanent                      3                 3
#>  9 Switzerl… Local Full-time  Permanent                      5                 5
#> 10 Denmark   Nati… Full-time  Permanent                      2                 4
#> # ℹ 1,190 more rows
#> # ℹ 9 more variables: ethics_1 <dbl>, ethics_2 <dbl>, ethics_3 <dbl>,
#> #   ethics_4 <dbl>, work_experience <dbl>, trust_parliament <dbl>,
#> #   trust_government <dbl>, trust_parties <dbl>, trust_politicians <dbl>
```

`ethics_1` to `ethics_4` measure agreement with statements concerning
ethics in journalism and may be combined into an index of ‘ethical
flexbility’, while the items starting with `trust_` measure trust in
various political institutions and thus may be combined into an index of
trust in politics.

## Add mean and sum indices

[`add_index()`](https://github.com/tidycomm/tidycomm/reference/add_index.md)
adds a mean index of specified variables to the data. The second (or
first, if used in a pipe) argument is the name of index variable to be
created:

``` r

WoJ %>% 
  add_index(ethical_flexibility, ethics_1, ethics_2, ethics_3, ethics_4) %>%
  # Select variables of relevance for output
  dplyr::select(ethical_flexibility, ethics_1, ethics_2, ethics_3, ethics_4)
#> # A tibble: 1,200 × 5
#>    ethical_flexibility ethics_1 ethics_2 ethics_3 ethics_4
#>                  <dbl>    <dbl>    <dbl>    <dbl>    <dbl>
#>  1                2           2        3        2        1
#>  2                1.5         1        2        2        1
#>  3                2.25        2        4        2        1
#>  4                1.75        1        3        1        2
#>  5                2           2        3        2        1
#>  6                3.25        2        4        4        3
#>  7                2           1        3        2        2
#>  8                3.5         2        4        4        4
#>  9                1.75        1        2        1        3
#> 10                3.25        1        4        4        4
#> # ℹ 1,190 more rows
```

To create a sum index instead, set `type = "sum"`:

``` r

WoJ %>% 
  add_index(ethical_flexibility, ethics_1, ethics_2, ethics_3, ethics_4, type = "sum") %>%
  # Select variables of relevance for output
  dplyr::select(ethical_flexibility, ethics_1, ethics_2, ethics_3, ethics_4)
#> # A tibble: 1,200 × 5
#>    ethical_flexibility ethics_1 ethics_2 ethics_3 ethics_4
#>                  <dbl>    <dbl>    <dbl>    <dbl>    <dbl>
#>  1                   8        2        3        2        1
#>  2                   6        1        2        2        1
#>  3                   9        2        4        2        1
#>  4                   7        1        3        1        2
#>  5                   8        2        3        2        1
#>  6                  13        2        4        4        3
#>  7                   8        1        3        2        2
#>  8                  14        2        4        4        4
#>  9                   7        1        2        1        3
#> 10                  13        1        4        4        4
#> # ℹ 1,190 more rows
```

## Compute reliability estimates of created indices

Use
[`get_reliability()`](https://github.com/tidycomm/tidycomm/reference/get_reliability.md)
to compute reliability/internal consistency estimates for indices
created with
[`add_index()`](https://github.com/tidycomm/tidycomm/reference/add_index.md).
Passing no further arguments to the function will automatically compute
reliability estimates for all indices created with
[`add_index()`](https://github.com/tidycomm/tidycomm/reference/add_index.md)
found in the data and output Cronbach’s $`\alpha`$ along with
descriptives and index information.

``` r

# Add two indices to data
WoJ <- WoJ %>% 
  add_index(ethical_flexibility, ethics_1, ethics_2, ethics_3, ethics_4) %>%
  add_index(trust_in_politics, trust_parliament, trust_government, trust_parties, trust_politicians)

WoJ %>% 
  get_reliability()
#> # A tibble: 2 × 5
#>   Index               Index_of                           M    SD Cronbachs_Alpha
#> * <chr>               <chr>                          <dbl> <dbl>           <dbl>
#> 1 ethical_flexibility ethics_1, ethics_2, ethics_3,…  2.45 0.777           0.612
#> 2 trust_in_politics   trust_parliament, trust_gover…  2.70 0.652           0.856
```

If you only want reliability estimates for specific indices, pass their
names as function arguments.

``` r

WoJ %>% 
  get_reliability(trust_in_politics)
#> # A tibble: 1 × 5
#>   Index             Index_of                             M    SD Cronbachs_Alpha
#> * <chr>             <chr>                            <dbl> <dbl>           <dbl>
#> 1 trust_in_politics trust_parliament, trust_governm…  2.70 0.652           0.856
```

Essentially,
[`get_reliability()`](https://github.com/tidycomm/tidycomm/reference/get_reliability.md)
provides a wrapper for [the ci.reliability function from the MBESS
package](https://cran.r-project.org/package=MBESS). Thus, all arguments
of
[`MBESS::ci.reliability()`](https://rdrr.io/pkg/MBESS/man/ci.reliability.html)
can be passed to
[`get_reliability()`](https://github.com/tidycomm/tidycomm/reference/get_reliability.md).
For example, to output $`\omega`$ instead of Cronbach’s $`\alpha`$
including robust maximum likelihood confidence intervals, you can type:

``` r

WoJ %>% 
  get_reliability(type = 'omega', interval.type = 'mlr')
#> # A tibble: 2 × 8
#>   Index               Index_of                 M    SD Omega CI_LL CI_UL CI_Type
#> * <chr>               <chr>                <dbl> <dbl> <dbl> <dbl> <dbl> <chr>  
#> 1 ethical_flexibility ethics_1, ethics_2,…  2.45 0.777 0.626 0.590 0.663 robust…
#> 2 trust_in_politics   trust_parliament, t…  2.70 0.652 0.856 0.840 0.871 robust…
```

See the [function
documentation](https://cran.r-project.org/package=MBESS) for more info
(and don’t forget to cite the `MBESS` package if using
[`get_reliability()`](https://github.com/tidycomm/tidycomm/reference/get_reliability.md)).
