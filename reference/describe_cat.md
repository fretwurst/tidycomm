# Describe categorical variables

Describe categorical variables by N, number of unique values, and mode.
Note that in case of multiple modes, the first mode by order of values
is chosen.

## Usage

``` r
describe_cat(data, ...)
```

## Arguments

- data:

  a [tibble](https://tibble.tidyverse.org/reference/tibble-package.html)
  or a
  [tdcmm](https://github.com/tidycomm/tidycomm/reference/tdcmm-class.md)
  model

- ...:

  Variables to describe (column names). Leave empty to describe all
  categorical variables in data.

## Value

a [tdcmm](https://github.com/tidycomm/tidycomm/reference/tdcmm-class.md)
model

## Details

If no variables are specified, all categorical (character or factor)
variables are described.

- N: number of valid cases (i.e., all but missing)

- Missing: number of NA cases

- Unique: number of unique categories in a given variable, without
  Missing

- Mode: mode average (if multiple modes exist, first mode by order of
  values is returned)

- Mode_N: number of cases reflecting the Mode

## See also

Other descriptives:
[`describe()`](https://github.com/tidycomm/tidycomm/reference/describe.md),
[`tab_percentiles()`](https://github.com/tidycomm/tidycomm/reference/tab_percentiles.md)

## Examples

``` r
WoJ %>% describe_cat(reach, employment, temp_contract)
#> # A tibble: 3 × 6
#>   Variable          N Missing Unique Mode      Mode_N
#> * <chr>         <int>   <int>  <dbl> <chr>      <int>
#> 1 reach          1200       0      4 National     617
#> 2 employment     1200       0      3 Full-time    902
#> 3 temp_contract  1001     199      2 Permanent    948
fbposts %>% describe_cat(type)
#> # A tibble: 1 × 6
#>   Variable     N Missing Unique Mode  Mode_N
#> * <chr>    <int>   <int>  <dbl> <chr>  <int>
#> 1 type       270       0      4 photo    162
```
