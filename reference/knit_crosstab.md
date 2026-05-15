# knit crosstab table variables

Knits table for one ore more variables

## Usage

``` r
knit_crosstab(
  data,
  col_var,
  row_var,
  width_pct = 80,
  weight = NULL,
  na = FALSE,
  percent = TRUE,
  num_decimal = 0,
  percent_decimal = 0,
  chi_square = TRUE,
  name_row_total = "Gesamt",
  name_percent = "Prozent"
)
```

## Arguments

- data:

  a [tibble](https://tibble.tidyverse.org/reference/tibble-package.html)

- col_var:

  the column variable.

- row_var:

  the row variable.

- width_pct:

  The table width in percent. Default = 80%.

- weight:

  Weighting variable. Default = NULL, means all weights are 1.

- na:

  = FALSE include NA. Default is FALSE.

- percent:

  = TRUE. Show column percent instead of counts. Default is TRUE.

- num_decimal:

  Decimal places of the numeric columns. Default is 0.

- percent_decimal:

  Decimal places of the percent columns. Default is 0.

- chi_square:

  = TRUE include chi_square. Default is TRUE.

- name_row_total:

  the name of the "Total" column.

- name_percent:

  = "Prozent" Decimal places of the percent columns. Default is 0.

## Value

a gt-table

## See also

Other categorical:
[`crosstab()`](https://github.com/tidycomm/tidycomm/reference/crosstab.md),
[`knit_frequencies()`](https://github.com/tidycomm/tidycomm/reference/knit_frequencies.md),
[`tab_frequencies()`](https://github.com/tidycomm/tidycomm/reference/tab_frequencies.md)

## Examples

``` r
WoJ |> knit_crosstab(row_var = employment, col_var = reach)


  


employment
```
