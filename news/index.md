# Changelog

## tidycomm 0.4.2

CRAN release: 2025-08-27

### Installation / Compatibility

- Minimum required R version increased from 2.10 to 3.6.0.

### Bugfixes

- Updated GGally usage to rely on `ggmatrix` class instead of `gg`,
  ensuring compatibility with the latest ggplot2 changes.
- Migrated tests to testthat v3 to maintain correct test behavior.

### Documentation

- Merged README.Rmd with index for pkgdown site.
- Updated links to reflect the new GitHub organization
  (<https://github.com/tidycomm/tidycomm>).
- Enabled Bootstrap 5 and light/dark switch for the pkgdown
  documentation site.

## tidycomm 0.4.1

CRAN release: 2024-02-22

## tidycomm 0.3.0

### New features

- Converted `tibble` returns into `tdcmm/tibble` return objects (they
  behave just like tibbles but are in essence our own objects now)
- Added partial correlation in `correlate(..., partial = z_var)`
- Added correlation with a focus variable
  `correlate(..., with = focus_var)`
- Added linear regression
  [`regress()`](https://github.com/tidycomm/tidycomm/reference/regress.md)
- Added one-sample t-test `t_test(..., mu = ...)`
- Added
  [`reverse_scale()`](https://github.com/tidycomm/tidycomm/reference/reverse_scale.md),
  [`minmax_scale()`](https://github.com/tidycomm/tidycomm/reference/minmax_scale.md),
  [`z_scale()`](https://github.com/tidycomm/tidycomm/reference/z_scale.md),
  [`center_scale()`](https://github.com/tidycomm/tidycomm/reference/center_scale.md),
  [`setna_scale()`](https://github.com/tidycomm/tidycomm/reference/setna_scale.md),
  [`recode_cat_scale()`](https://github.com/tidycomm/tidycomm/reference/recode_cat_scale.md),
  `recode_scale()`, and `dummify_cale()` to shift and modify continuous
  and categorical scales
- Added
  [`tab_percentiles()`](https://github.com/tidycomm/tidycomm/reference/tab_percentiles.md)
- Added
  [`visualize()`](https://github.com/tidycomm/tidycomm/reference/visualize.md)
  to visualize almost everything
- Added `snscomments` and `incvlcomments` as additional data sets

### Minor changes

- Changed
  [`unianova()`](https://github.com/tidycomm/tidycomm/reference/unianova.md)
  and
  [`t_test()`](https://github.com/tidycomm/tidycomm/reference/t_test.md)
  to build on `leveneTest()`
- Allowed
  [`test_icr()`](https://github.com/tidycomm/tidycomm/reference/test_icr.md)
  to work with grouped data
- Converted all code examples in documentation to be built on
  `tidycomm`-provided data sets
- Added `omega_squared`, `Levene_p`, and `var_equal` columns to default
  return from
  [`unianova()`](https://github.com/tidycomm/tidycomm/reference/unianova.md)
- Added `d`, `se`, `t`, and `df`
- Removed `null.value` from list of return values in
  [`unianova()`](https://github.com/tidycomm/tidycomm/reference/unianova.md)
  post-hoc test
- Renamed
  [`unianova()`](https://github.com/tidycomm/tidycomm/reference/unianova.md)
  return column names to `Variable` (previously: `Var`), `Group_Var`
  (prev. `term`), `Delta_M` (prev. `estimate`), `p` (prev.
  `adj.p.value`), `conf.lower` and `conf.upper` (prev. `conf.low` and
  `conf.high`)

### Bugfixes

- Unified output to coherent number of after-comma digits

## tidycomm 0.2.0

### New features

- Added Fretwurst’s Lotus and S-Lotus intercoder reliability
  coeffecients to
  [`test_icr()`](https://github.com/tidycomm/tidycomm/reference/test_icr.md)
  function
- [`describe_cat()`](https://github.com/tidycomm/tidycomm/reference/describe_cat.md)
  function added to describe categorical variables

### Minor changes

- More descriptive error messages for common errors
- [`describe()`](https://github.com/tidycomm/tidycomm/reference/describe.md)
  now also reports 95% confidence intervals
- [`describe()`](https://github.com/tidycomm/tidycomm/reference/describe.md)
  now reports valid N instead of full N
- [`test_icr()`](https://github.com/tidycomm/tidycomm/reference/test_icr.md)
  now works with `tidyselect` selection

### Bugfixes

- Empty groups are dropped if describing with more than one grouping
  variable
- Krippendorff’s Alpha returns 1 if variable has only one category
- [`unianova()`](https://github.com/tidycomm/tidycomm/reference/unianova.md)
  now works with variable names containing whitespace
- Groups are dropped for
  [`test_icr()`](https://github.com/tidycomm/tidycomm/reference/test_icr.md)
  to avoid computational issues
