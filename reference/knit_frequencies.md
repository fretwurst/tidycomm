# knit frequencies table variables

Knits table for one ore more variables

## Usage

``` r
knit_frequencies(
  data,
  ...,
  width_pct = 80,
  weight = NULL,
  num_decimal = 0,
  percent_decimal = 0,
  name_n = "N",
  name_total_percent = "Prozent",
  name_valid_percent = "Valide %",
  name_cum_n = "Kum n",
  name_cum_percent = "Kum %",
  name_row_total = "Gesamt",
  cums = TRUE
)
```

## Arguments

- data:

  a [tibble](https://tibble.tidyverse.org/reference/tibble-package.html)

- ...:

  List of variables.

- width_pct:

  The table width in percent. Default = 80.

- weight:

  Weighting variable. Default = NULL, means all weights are 1.

- num_decimal:

  Decimal places of the numeric columns. Default is 0.

- percent_decimal:

  Decimal places of the percent columns. Default is 0.

- name_n:

  Rename Column n. Default is "N".

- name_total_percent:

  Rename total percent. Default is "Prozent".

- name_valid_percent:

  Rename valid percent. Default is "N".

- name_cum_n:

  Rename cum n. Default is "Kum n".

- name_cum_percent:

  Rename cum n. Default is "Kum %".

- name_row_total:

  Rename cum n. Default is "Kum %".

- cums:

  Produce columns for cumulative counts and percents.

## Value

a gt-table

## See also

Other categorical:
[`crosstab()`](https://github.com/tidycomm/tidycomm/reference/crosstab.md),
[`knit_crosstab()`](https://github.com/tidycomm/tidycomm/reference/knit_crosstab.md),
[`tab_frequencies()`](https://github.com/tidycomm/tidycomm/reference/tab_frequencies.md)

## Examples

``` r
WoJ |> knit_frequencies(reach, employment)
#> <div id="ulbfdpbcyv" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
#>   <style>#ulbfdpbcyv table {
#>   font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
#>   -webkit-font-smoothing: antialiased;
#>   -moz-osx-font-smoothing: grayscale;
#> }
#> 
#> #ulbfdpbcyv thead, #ulbfdpbcyv tbody, #ulbfdpbcyv tfoot, #ulbfdpbcyv tr, #ulbfdpbcyv td, #ulbfdpbcyv th {
#>   border-style: none;
#> }
#> 
#> #ulbfdpbcyv p {
#>   margin: 0;
#>   padding: 0;
#> }
#> 
#> #ulbfdpbcyv .gt_table {
#>   display: table;
#>   border-collapse: collapse;
#>   line-height: normal;
#>   margin-left: auto;
#>   margin-right: auto;
#>   color: #333333;
#>   font-size: 16px;
#>   font-weight: normal;
#>   font-style: normal;
#>   background-color: #FFFFFF;
#>   width: 80%;
#>   border-top-style: solid;
#>   border-top-width: 2px;
#>   border-top-color: #A8A8A8;
#>   border-right-style: none;
#>   border-right-width: 2px;
#>   border-right-color: #D3D3D3;
#>   border-bottom-style: solid;
#>   border-bottom-width: 2px;
#>   border-bottom-color: #A8A8A8;
#>   border-left-style: none;
#>   border-left-width: 2px;
#>   border-left-color: #D3D3D3;
#> }
#> 
#> #ulbfdpbcyv .gt_caption {
#>   padding-top: 4px;
#>   padding-bottom: 4px;
#> }
#> 
#> #ulbfdpbcyv .gt_title {
#>   color: #333333;
#>   font-size: 125%;
#>   font-weight: initial;
#>   padding-top: 4px;
#>   padding-bottom: 4px;
#>   padding-left: 5px;
#>   padding-right: 5px;
#>   border-bottom-color: #FFFFFF;
#>   border-bottom-width: 0;
#> }
#> 
#> #ulbfdpbcyv .gt_subtitle {
#>   color: #333333;
#>   font-size: 85%;
#>   font-weight: initial;
#>   padding-top: 3px;
#>   padding-bottom: 5px;
#>   padding-left: 5px;
#>   padding-right: 5px;
#>   border-top-color: #FFFFFF;
#>   border-top-width: 0;
#> }
#> 
#> #ulbfdpbcyv .gt_heading {
#>   background-color: #FFFFFF;
#>   text-align: center;
#>   border-bottom-color: #FFFFFF;
#>   border-left-style: none;
#>   border-left-width: 1px;
#>   border-left-color: #D3D3D3;
#>   border-right-style: none;
#>   border-right-width: 1px;
#>   border-right-color: #D3D3D3;
#> }
#> 
#> #ulbfdpbcyv .gt_bottom_border {
#>   border-bottom-style: solid;
#>   border-bottom-width: 2px;
#>   border-bottom-color: #D3D3D3;
#> }
#> 
#> #ulbfdpbcyv .gt_col_headings {
#>   border-top-style: solid;
#>   border-top-width: 2px;
#>   border-top-color: #D3D3D3;
#>   border-bottom-style: solid;
#>   border-bottom-width: 2px;
#>   border-bottom-color: #D3D3D3;
#>   border-left-style: none;
#>   border-left-width: 1px;
#>   border-left-color: #D3D3D3;
#>   border-right-style: none;
#>   border-right-width: 1px;
#>   border-right-color: #D3D3D3;
#> }
#> 
#> #ulbfdpbcyv .gt_col_heading {
#>   color: #333333;
#>   background-color: #FFFFFF;
#>   font-size: 100%;
#>   font-weight: normal;
#>   text-transform: inherit;
#>   border-left-style: none;
#>   border-left-width: 1px;
#>   border-left-color: #D3D3D3;
#>   border-right-style: none;
#>   border-right-width: 1px;
#>   border-right-color: #D3D3D3;
#>   vertical-align: bottom;
#>   padding-top: 5px;
#>   padding-bottom: 6px;
#>   padding-left: 5px;
#>   padding-right: 5px;
#>   overflow-x: hidden;
#> }
#> 
#> #ulbfdpbcyv .gt_column_spanner_outer {
#>   color: #333333;
#>   background-color: #FFFFFF;
#>   font-size: 100%;
#>   font-weight: normal;
#>   text-transform: inherit;
#>   padding-top: 0;
#>   padding-bottom: 0;
#>   padding-left: 4px;
#>   padding-right: 4px;
#> }
#> 
#> #ulbfdpbcyv .gt_column_spanner_outer:first-child {
#>   padding-left: 0;
#> }
#> 
#> #ulbfdpbcyv .gt_column_spanner_outer:last-child {
#>   padding-right: 0;
#> }
#> 
#> #ulbfdpbcyv .gt_column_spanner {
#>   border-bottom-style: solid;
#>   border-bottom-width: 2px;
#>   border-bottom-color: #D3D3D3;
#>   vertical-align: bottom;
#>   padding-top: 5px;
#>   padding-bottom: 5px;
#>   overflow-x: hidden;
#>   display: inline-block;
#>   width: 100%;
#> }
#> 
#> #ulbfdpbcyv .gt_spanner_row {
#>   border-bottom-style: hidden;
#> }
#> 
#> #ulbfdpbcyv .gt_group_heading {
#>   padding-top: 8px;
#>   padding-bottom: 8px;
#>   padding-left: 5px;
#>   padding-right: 5px;
#>   color: #333333;
#>   background-color: #FFFFFF;
#>   font-size: 100%;
#>   font-weight: initial;
#>   text-transform: inherit;
#>   border-top-style: solid;
#>   border-top-width: 2px;
#>   border-top-color: #D3D3D3;
#>   border-bottom-style: solid;
#>   border-bottom-width: 2px;
#>   border-bottom-color: #D3D3D3;
#>   border-left-style: none;
#>   border-left-width: 1px;
#>   border-left-color: #D3D3D3;
#>   border-right-style: none;
#>   border-right-width: 1px;
#>   border-right-color: #D3D3D3;
#>   vertical-align: middle;
#>   text-align: left;
#> }
#> 
#> #ulbfdpbcyv .gt_empty_group_heading {
#>   padding: 0.5px;
#>   color: #333333;
#>   background-color: #FFFFFF;
#>   font-size: 100%;
#>   font-weight: initial;
#>   border-top-style: solid;
#>   border-top-width: 2px;
#>   border-top-color: #D3D3D3;
#>   border-bottom-style: solid;
#>   border-bottom-width: 2px;
#>   border-bottom-color: #D3D3D3;
#>   vertical-align: middle;
#> }
#> 
#> #ulbfdpbcyv .gt_from_md > :first-child {
#>   margin-top: 0;
#> }
#> 
#> #ulbfdpbcyv .gt_from_md > :last-child {
#>   margin-bottom: 0;
#> }
#> 
#> #ulbfdpbcyv .gt_row {
#>   padding-top: 8px;
#>   padding-bottom: 8px;
#>   padding-left: 5px;
#>   padding-right: 5px;
#>   margin: 10px;
#>   border-top-style: solid;
#>   border-top-width: 1px;
#>   border-top-color: #D3D3D3;
#>   border-left-style: none;
#>   border-left-width: 1px;
#>   border-left-color: #D3D3D3;
#>   border-right-style: none;
#>   border-right-width: 1px;
#>   border-right-color: #D3D3D3;
#>   vertical-align: middle;
#>   overflow-x: hidden;
#> }
#> 
#> #ulbfdpbcyv .gt_stub {
#>   color: #333333;
#>   background-color: #FFFFFF;
#>   font-size: 100%;
#>   font-weight: initial;
#>   text-transform: inherit;
#>   border-right-style: solid;
#>   border-right-width: 2px;
#>   border-right-color: #D3D3D3;
#>   padding-left: 5px;
#>   padding-right: 5px;
#> }
#> 
#> #ulbfdpbcyv .gt_stub_row_group {
#>   color: #333333;
#>   background-color: #FFFFFF;
#>   font-size: 100%;
#>   font-weight: initial;
#>   text-transform: inherit;
#>   border-right-style: solid;
#>   border-right-width: 2px;
#>   border-right-color: #D3D3D3;
#>   padding-left: 5px;
#>   padding-right: 5px;
#>   vertical-align: top;
#> }
#> 
#> #ulbfdpbcyv .gt_row_group_first td {
#>   border-top-width: 2px;
#> }
#> 
#> #ulbfdpbcyv .gt_row_group_first th {
#>   border-top-width: 2px;
#> }
#> 
#> #ulbfdpbcyv .gt_summary_row {
#>   color: #333333;
#>   background-color: #FFFFFF;
#>   text-transform: inherit;
#>   padding-top: 8px;
#>   padding-bottom: 8px;
#>   padding-left: 5px;
#>   padding-right: 5px;
#> }
#> 
#> #ulbfdpbcyv .gt_first_summary_row {
#>   border-top-style: solid;
#>   border-top-color: #D3D3D3;
#> }
#> 
#> #ulbfdpbcyv .gt_first_summary_row.thick {
#>   border-top-width: 2px;
#> }
#> 
#> #ulbfdpbcyv .gt_last_summary_row {
#>   padding-top: 8px;
#>   padding-bottom: 8px;
#>   padding-left: 5px;
#>   padding-right: 5px;
#>   border-bottom-style: solid;
#>   border-bottom-width: 2px;
#>   border-bottom-color: #D3D3D3;
#> }
#> 
#> #ulbfdpbcyv .gt_grand_summary_row {
#>   color: #333333;
#>   background-color: #FFFFFF;
#>   text-transform: inherit;
#>   padding-top: 8px;
#>   padding-bottom: 8px;
#>   padding-left: 5px;
#>   padding-right: 5px;
#> }
#> 
#> #ulbfdpbcyv .gt_first_grand_summary_row {
#>   padding-top: 8px;
#>   padding-bottom: 8px;
#>   padding-left: 5px;
#>   padding-right: 5px;
#>   border-top-style: double;
#>   border-top-width: 6px;
#>   border-top-color: #D3D3D3;
#> }
#> 
#> #ulbfdpbcyv .gt_last_grand_summary_row_top {
#>   padding-top: 8px;
#>   padding-bottom: 8px;
#>   padding-left: 5px;
#>   padding-right: 5px;
#>   border-bottom-style: double;
#>   border-bottom-width: 6px;
#>   border-bottom-color: #D3D3D3;
#> }
#> 
#> #ulbfdpbcyv .gt_striped {
#>   background-color: rgba(128, 128, 128, 0.05);
#> }
#> 
#> #ulbfdpbcyv .gt_table_body {
#>   border-top-style: solid;
#>   border-top-width: 2px;
#>   border-top-color: #D3D3D3;
#>   border-bottom-style: solid;
#>   border-bottom-width: 2px;
#>   border-bottom-color: #D3D3D3;
#> }
#> 
#> #ulbfdpbcyv .gt_footnotes {
#>   color: #333333;
#>   background-color: #FFFFFF;
#>   border-bottom-style: none;
#>   border-bottom-width: 2px;
#>   border-bottom-color: #D3D3D3;
#>   border-left-style: none;
#>   border-left-width: 2px;
#>   border-left-color: #D3D3D3;
#>   border-right-style: none;
#>   border-right-width: 2px;
#>   border-right-color: #D3D3D3;
#> }
#> 
#> #ulbfdpbcyv .gt_footnote {
#>   margin: 0px;
#>   font-size: 90%;
#>   padding-top: 4px;
#>   padding-bottom: 4px;
#>   padding-left: 5px;
#>   padding-right: 5px;
#> }
#> 
#> #ulbfdpbcyv .gt_sourcenotes {
#>   color: #333333;
#>   background-color: #FFFFFF;
#>   border-bottom-style: none;
#>   border-bottom-width: 2px;
#>   border-bottom-color: #D3D3D3;
#>   border-left-style: none;
#>   border-left-width: 2px;
#>   border-left-color: #D3D3D3;
#>   border-right-style: none;
#>   border-right-width: 2px;
#>   border-right-color: #D3D3D3;
#> }
#> 
#> #ulbfdpbcyv .gt_sourcenote {
#>   font-size: 90%;
#>   padding-top: 4px;
#>   padding-bottom: 4px;
#>   padding-left: 5px;
#>   padding-right: 5px;
#> }
#> 
#> #ulbfdpbcyv .gt_left {
#>   text-align: left;
#> }
#> 
#> #ulbfdpbcyv .gt_center {
#>   text-align: center;
#> }
#> 
#> #ulbfdpbcyv .gt_right {
#>   text-align: right;
#>   font-variant-numeric: tabular-nums;
#> }
#> 
#> #ulbfdpbcyv .gt_font_normal {
#>   font-weight: normal;
#> }
#> 
#> #ulbfdpbcyv .gt_font_bold {
#>   font-weight: bold;
#> }
#> 
#> #ulbfdpbcyv .gt_font_italic {
#>   font-style: italic;
#> }
#> 
#> #ulbfdpbcyv .gt_super {
#>   font-size: 65%;
#> }
#> 
#> #ulbfdpbcyv .gt_footnote_marks {
#>   font-size: 75%;
#>   vertical-align: 0.4em;
#>   position: initial;
#> }
#> 
#> #ulbfdpbcyv .gt_asterisk {
#>   font-size: 100%;
#>   vertical-align: 0;
#> }
#> 
#> #ulbfdpbcyv .gt_indent_1 {
#>   text-indent: 5px;
#> }
#> 
#> #ulbfdpbcyv .gt_indent_2 {
#>   text-indent: 10px;
#> }
#> 
#> #ulbfdpbcyv .gt_indent_3 {
#>   text-indent: 15px;
#> }
#> 
#> #ulbfdpbcyv .gt_indent_4 {
#>   text-indent: 20px;
#> }
#> 
#> #ulbfdpbcyv .gt_indent_5 {
#>   text-indent: 25px;
#> }
#> 
#> #ulbfdpbcyv .katex-display {
#>   display: inline-flex !important;
#>   margin-bottom: 0.75em !important;
#> }
#> 
#> #ulbfdpbcyv div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
#>   height: 0px !important;
#> }
#> </style>
#>   <table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
#>   <thead>
#>     <tr class="gt_col_headings">
#>       <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="reach">reach</th>
#>       <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="N">N</th>
#>       <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="Prozent">Prozent</th>
#>       <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="Valide-%">Valide %</th>
#>       <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="Kum-n">Kum n</th>
#>       <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="Kum-%">Kum %</th>
#>     </tr>
#>   </thead>
#>   <tbody class="gt_table_body">
#>     <tr><td headers="reach" class="gt_row gt_left">Local</td>
#> <td headers="N" class="gt_row gt_right">149</td>
#> <td headers="Prozent" class="gt_row gt_right">12%</td>
#> <td headers="Valide %" class="gt_row gt_right">12%</td>
#> <td headers="Kum n" class="gt_row gt_right">149</td>
#> <td headers="Kum %" class="gt_row gt_right">12%</td></tr>
#>     <tr><td headers="reach" class="gt_row gt_left">Regional</td>
#> <td headers="N" class="gt_row gt_right">355</td>
#> <td headers="Prozent" class="gt_row gt_right">30%</td>
#> <td headers="Valide %" class="gt_row gt_right">30%</td>
#> <td headers="Kum n" class="gt_row gt_right">504</td>
#> <td headers="Kum %" class="gt_row gt_right">42%</td></tr>
#>     <tr><td headers="reach" class="gt_row gt_left">National</td>
#> <td headers="N" class="gt_row gt_right">617</td>
#> <td headers="Prozent" class="gt_row gt_right">51%</td>
#> <td headers="Valide %" class="gt_row gt_right">51%</td>
#> <td headers="Kum n" class="gt_row gt_right">1121</td>
#> <td headers="Kum %" class="gt_row gt_right">93%</td></tr>
#>     <tr><td headers="reach" class="gt_row gt_left">Transnational</td>
#> <td headers="N" class="gt_row gt_right">79</td>
#> <td headers="Prozent" class="gt_row gt_right">7%</td>
#> <td headers="Valide %" class="gt_row gt_right">7%</td>
#> <td headers="Kum n" class="gt_row gt_right">1200</td>
#> <td headers="Kum %" class="gt_row gt_right">100%</td></tr>
#>     <tr><td headers="reach" class="gt_row gt_left">Gesamt</td>
#> <td headers="N" class="gt_row gt_right">1200</td>
#> <td headers="Prozent" class="gt_row gt_right">100%</td>
#> <td headers="Valide %" class="gt_row gt_right">100%</td>
#> <td headers="Kum n" class="gt_row gt_right">—</td>
#> <td headers="Kum %" class="gt_row gt_right">—</td></tr>
#>   </tbody>
#>   
#> </table>
#> </div>
#> <div id="kgufivjubq" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
#>   <style>#kgufivjubq table {
#>   font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
#>   -webkit-font-smoothing: antialiased;
#>   -moz-osx-font-smoothing: grayscale;
#> }
#> 
#> #kgufivjubq thead, #kgufivjubq tbody, #kgufivjubq tfoot, #kgufivjubq tr, #kgufivjubq td, #kgufivjubq th {
#>   border-style: none;
#> }
#> 
#> #kgufivjubq p {
#>   margin: 0;
#>   padding: 0;
#> }
#> 
#> #kgufivjubq .gt_table {
#>   display: table;
#>   border-collapse: collapse;
#>   line-height: normal;
#>   margin-left: auto;
#>   margin-right: auto;
#>   color: #333333;
#>   font-size: 16px;
#>   font-weight: normal;
#>   font-style: normal;
#>   background-color: #FFFFFF;
#>   width: 80%;
#>   border-top-style: solid;
#>   border-top-width: 2px;
#>   border-top-color: #A8A8A8;
#>   border-right-style: none;
#>   border-right-width: 2px;
#>   border-right-color: #D3D3D3;
#>   border-bottom-style: solid;
#>   border-bottom-width: 2px;
#>   border-bottom-color: #A8A8A8;
#>   border-left-style: none;
#>   border-left-width: 2px;
#>   border-left-color: #D3D3D3;
#> }
#> 
#> #kgufivjubq .gt_caption {
#>   padding-top: 4px;
#>   padding-bottom: 4px;
#> }
#> 
#> #kgufivjubq .gt_title {
#>   color: #333333;
#>   font-size: 125%;
#>   font-weight: initial;
#>   padding-top: 4px;
#>   padding-bottom: 4px;
#>   padding-left: 5px;
#>   padding-right: 5px;
#>   border-bottom-color: #FFFFFF;
#>   border-bottom-width: 0;
#> }
#> 
#> #kgufivjubq .gt_subtitle {
#>   color: #333333;
#>   font-size: 85%;
#>   font-weight: initial;
#>   padding-top: 3px;
#>   padding-bottom: 5px;
#>   padding-left: 5px;
#>   padding-right: 5px;
#>   border-top-color: #FFFFFF;
#>   border-top-width: 0;
#> }
#> 
#> #kgufivjubq .gt_heading {
#>   background-color: #FFFFFF;
#>   text-align: center;
#>   border-bottom-color: #FFFFFF;
#>   border-left-style: none;
#>   border-left-width: 1px;
#>   border-left-color: #D3D3D3;
#>   border-right-style: none;
#>   border-right-width: 1px;
#>   border-right-color: #D3D3D3;
#> }
#> 
#> #kgufivjubq .gt_bottom_border {
#>   border-bottom-style: solid;
#>   border-bottom-width: 2px;
#>   border-bottom-color: #D3D3D3;
#> }
#> 
#> #kgufivjubq .gt_col_headings {
#>   border-top-style: solid;
#>   border-top-width: 2px;
#>   border-top-color: #D3D3D3;
#>   border-bottom-style: solid;
#>   border-bottom-width: 2px;
#>   border-bottom-color: #D3D3D3;
#>   border-left-style: none;
#>   border-left-width: 1px;
#>   border-left-color: #D3D3D3;
#>   border-right-style: none;
#>   border-right-width: 1px;
#>   border-right-color: #D3D3D3;
#> }
#> 
#> #kgufivjubq .gt_col_heading {
#>   color: #333333;
#>   background-color: #FFFFFF;
#>   font-size: 100%;
#>   font-weight: normal;
#>   text-transform: inherit;
#>   border-left-style: none;
#>   border-left-width: 1px;
#>   border-left-color: #D3D3D3;
#>   border-right-style: none;
#>   border-right-width: 1px;
#>   border-right-color: #D3D3D3;
#>   vertical-align: bottom;
#>   padding-top: 5px;
#>   padding-bottom: 6px;
#>   padding-left: 5px;
#>   padding-right: 5px;
#>   overflow-x: hidden;
#> }
#> 
#> #kgufivjubq .gt_column_spanner_outer {
#>   color: #333333;
#>   background-color: #FFFFFF;
#>   font-size: 100%;
#>   font-weight: normal;
#>   text-transform: inherit;
#>   padding-top: 0;
#>   padding-bottom: 0;
#>   padding-left: 4px;
#>   padding-right: 4px;
#> }
#> 
#> #kgufivjubq .gt_column_spanner_outer:first-child {
#>   padding-left: 0;
#> }
#> 
#> #kgufivjubq .gt_column_spanner_outer:last-child {
#>   padding-right: 0;
#> }
#> 
#> #kgufivjubq .gt_column_spanner {
#>   border-bottom-style: solid;
#>   border-bottom-width: 2px;
#>   border-bottom-color: #D3D3D3;
#>   vertical-align: bottom;
#>   padding-top: 5px;
#>   padding-bottom: 5px;
#>   overflow-x: hidden;
#>   display: inline-block;
#>   width: 100%;
#> }
#> 
#> #kgufivjubq .gt_spanner_row {
#>   border-bottom-style: hidden;
#> }
#> 
#> #kgufivjubq .gt_group_heading {
#>   padding-top: 8px;
#>   padding-bottom: 8px;
#>   padding-left: 5px;
#>   padding-right: 5px;
#>   color: #333333;
#>   background-color: #FFFFFF;
#>   font-size: 100%;
#>   font-weight: initial;
#>   text-transform: inherit;
#>   border-top-style: solid;
#>   border-top-width: 2px;
#>   border-top-color: #D3D3D3;
#>   border-bottom-style: solid;
#>   border-bottom-width: 2px;
#>   border-bottom-color: #D3D3D3;
#>   border-left-style: none;
#>   border-left-width: 1px;
#>   border-left-color: #D3D3D3;
#>   border-right-style: none;
#>   border-right-width: 1px;
#>   border-right-color: #D3D3D3;
#>   vertical-align: middle;
#>   text-align: left;
#> }
#> 
#> #kgufivjubq .gt_empty_group_heading {
#>   padding: 0.5px;
#>   color: #333333;
#>   background-color: #FFFFFF;
#>   font-size: 100%;
#>   font-weight: initial;
#>   border-top-style: solid;
#>   border-top-width: 2px;
#>   border-top-color: #D3D3D3;
#>   border-bottom-style: solid;
#>   border-bottom-width: 2px;
#>   border-bottom-color: #D3D3D3;
#>   vertical-align: middle;
#> }
#> 
#> #kgufivjubq .gt_from_md > :first-child {
#>   margin-top: 0;
#> }
#> 
#> #kgufivjubq .gt_from_md > :last-child {
#>   margin-bottom: 0;
#> }
#> 
#> #kgufivjubq .gt_row {
#>   padding-top: 8px;
#>   padding-bottom: 8px;
#>   padding-left: 5px;
#>   padding-right: 5px;
#>   margin: 10px;
#>   border-top-style: solid;
#>   border-top-width: 1px;
#>   border-top-color: #D3D3D3;
#>   border-left-style: none;
#>   border-left-width: 1px;
#>   border-left-color: #D3D3D3;
#>   border-right-style: none;
#>   border-right-width: 1px;
#>   border-right-color: #D3D3D3;
#>   vertical-align: middle;
#>   overflow-x: hidden;
#> }
#> 
#> #kgufivjubq .gt_stub {
#>   color: #333333;
#>   background-color: #FFFFFF;
#>   font-size: 100%;
#>   font-weight: initial;
#>   text-transform: inherit;
#>   border-right-style: solid;
#>   border-right-width: 2px;
#>   border-right-color: #D3D3D3;
#>   padding-left: 5px;
#>   padding-right: 5px;
#> }
#> 
#> #kgufivjubq .gt_stub_row_group {
#>   color: #333333;
#>   background-color: #FFFFFF;
#>   font-size: 100%;
#>   font-weight: initial;
#>   text-transform: inherit;
#>   border-right-style: solid;
#>   border-right-width: 2px;
#>   border-right-color: #D3D3D3;
#>   padding-left: 5px;
#>   padding-right: 5px;
#>   vertical-align: top;
#> }
#> 
#> #kgufivjubq .gt_row_group_first td {
#>   border-top-width: 2px;
#> }
#> 
#> #kgufivjubq .gt_row_group_first th {
#>   border-top-width: 2px;
#> }
#> 
#> #kgufivjubq .gt_summary_row {
#>   color: #333333;
#>   background-color: #FFFFFF;
#>   text-transform: inherit;
#>   padding-top: 8px;
#>   padding-bottom: 8px;
#>   padding-left: 5px;
#>   padding-right: 5px;
#> }
#> 
#> #kgufivjubq .gt_first_summary_row {
#>   border-top-style: solid;
#>   border-top-color: #D3D3D3;
#> }
#> 
#> #kgufivjubq .gt_first_summary_row.thick {
#>   border-top-width: 2px;
#> }
#> 
#> #kgufivjubq .gt_last_summary_row {
#>   padding-top: 8px;
#>   padding-bottom: 8px;
#>   padding-left: 5px;
#>   padding-right: 5px;
#>   border-bottom-style: solid;
#>   border-bottom-width: 2px;
#>   border-bottom-color: #D3D3D3;
#> }
#> 
#> #kgufivjubq .gt_grand_summary_row {
#>   color: #333333;
#>   background-color: #FFFFFF;
#>   text-transform: inherit;
#>   padding-top: 8px;
#>   padding-bottom: 8px;
#>   padding-left: 5px;
#>   padding-right: 5px;
#> }
#> 
#> #kgufivjubq .gt_first_grand_summary_row {
#>   padding-top: 8px;
#>   padding-bottom: 8px;
#>   padding-left: 5px;
#>   padding-right: 5px;
#>   border-top-style: double;
#>   border-top-width: 6px;
#>   border-top-color: #D3D3D3;
#> }
#> 
#> #kgufivjubq .gt_last_grand_summary_row_top {
#>   padding-top: 8px;
#>   padding-bottom: 8px;
#>   padding-left: 5px;
#>   padding-right: 5px;
#>   border-bottom-style: double;
#>   border-bottom-width: 6px;
#>   border-bottom-color: #D3D3D3;
#> }
#> 
#> #kgufivjubq .gt_striped {
#>   background-color: rgba(128, 128, 128, 0.05);
#> }
#> 
#> #kgufivjubq .gt_table_body {
#>   border-top-style: solid;
#>   border-top-width: 2px;
#>   border-top-color: #D3D3D3;
#>   border-bottom-style: solid;
#>   border-bottom-width: 2px;
#>   border-bottom-color: #D3D3D3;
#> }
#> 
#> #kgufivjubq .gt_footnotes {
#>   color: #333333;
#>   background-color: #FFFFFF;
#>   border-bottom-style: none;
#>   border-bottom-width: 2px;
#>   border-bottom-color: #D3D3D3;
#>   border-left-style: none;
#>   border-left-width: 2px;
#>   border-left-color: #D3D3D3;
#>   border-right-style: none;
#>   border-right-width: 2px;
#>   border-right-color: #D3D3D3;
#> }
#> 
#> #kgufivjubq .gt_footnote {
#>   margin: 0px;
#>   font-size: 90%;
#>   padding-top: 4px;
#>   padding-bottom: 4px;
#>   padding-left: 5px;
#>   padding-right: 5px;
#> }
#> 
#> #kgufivjubq .gt_sourcenotes {
#>   color: #333333;
#>   background-color: #FFFFFF;
#>   border-bottom-style: none;
#>   border-bottom-width: 2px;
#>   border-bottom-color: #D3D3D3;
#>   border-left-style: none;
#>   border-left-width: 2px;
#>   border-left-color: #D3D3D3;
#>   border-right-style: none;
#>   border-right-width: 2px;
#>   border-right-color: #D3D3D3;
#> }
#> 
#> #kgufivjubq .gt_sourcenote {
#>   font-size: 90%;
#>   padding-top: 4px;
#>   padding-bottom: 4px;
#>   padding-left: 5px;
#>   padding-right: 5px;
#> }
#> 
#> #kgufivjubq .gt_left {
#>   text-align: left;
#> }
#> 
#> #kgufivjubq .gt_center {
#>   text-align: center;
#> }
#> 
#> #kgufivjubq .gt_right {
#>   text-align: right;
#>   font-variant-numeric: tabular-nums;
#> }
#> 
#> #kgufivjubq .gt_font_normal {
#>   font-weight: normal;
#> }
#> 
#> #kgufivjubq .gt_font_bold {
#>   font-weight: bold;
#> }
#> 
#> #kgufivjubq .gt_font_italic {
#>   font-style: italic;
#> }
#> 
#> #kgufivjubq .gt_super {
#>   font-size: 65%;
#> }
#> 
#> #kgufivjubq .gt_footnote_marks {
#>   font-size: 75%;
#>   vertical-align: 0.4em;
#>   position: initial;
#> }
#> 
#> #kgufivjubq .gt_asterisk {
#>   font-size: 100%;
#>   vertical-align: 0;
#> }
#> 
#> #kgufivjubq .gt_indent_1 {
#>   text-indent: 5px;
#> }
#> 
#> #kgufivjubq .gt_indent_2 {
#>   text-indent: 10px;
#> }
#> 
#> #kgufivjubq .gt_indent_3 {
#>   text-indent: 15px;
#> }
#> 
#> #kgufivjubq .gt_indent_4 {
#>   text-indent: 20px;
#> }
#> 
#> #kgufivjubq .gt_indent_5 {
#>   text-indent: 25px;
#> }
#> 
#> #kgufivjubq .katex-display {
#>   display: inline-flex !important;
#>   margin-bottom: 0.75em !important;
#> }
#> 
#> #kgufivjubq div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
#>   height: 0px !important;
#> }
#> </style>
#>   <table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
#>   <thead>
#>     <tr class="gt_col_headings">
#>       <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="employment">employment</th>
#>       <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="N">N</th>
#>       <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="Prozent">Prozent</th>
#>       <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="Valide-%">Valide %</th>
#>       <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="Kum-n">Kum n</th>
#>       <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="Kum-%">Kum %</th>
#>     </tr>
#>   </thead>
#>   <tbody class="gt_table_body">
#>     <tr><td headers="employment" class="gt_row gt_left">Freelancer</td>
#> <td headers="N" class="gt_row gt_right">172</td>
#> <td headers="Prozent" class="gt_row gt_right">14%</td>
#> <td headers="Valide %" class="gt_row gt_right">14%</td>
#> <td headers="Kum n" class="gt_row gt_right">172</td>
#> <td headers="Kum %" class="gt_row gt_right">14%</td></tr>
#>     <tr><td headers="employment" class="gt_row gt_left">Full-time</td>
#> <td headers="N" class="gt_row gt_right">902</td>
#> <td headers="Prozent" class="gt_row gt_right">75%</td>
#> <td headers="Valide %" class="gt_row gt_right">75%</td>
#> <td headers="Kum n" class="gt_row gt_right">1074</td>
#> <td headers="Kum %" class="gt_row gt_right">90%</td></tr>
#>     <tr><td headers="employment" class="gt_row gt_left">Part-time</td>
#> <td headers="N" class="gt_row gt_right">126</td>
#> <td headers="Prozent" class="gt_row gt_right">10%</td>
#> <td headers="Valide %" class="gt_row gt_right">10%</td>
#> <td headers="Kum n" class="gt_row gt_right">1200</td>
#> <td headers="Kum %" class="gt_row gt_right">100%</td></tr>
#>     <tr><td headers="employment" class="gt_row gt_left">Gesamt</td>
#> <td headers="N" class="gt_row gt_right">1200</td>
#> <td headers="Prozent" class="gt_row gt_right">100%</td>
#> <td headers="Valide %" class="gt_row gt_right">100%</td>
#> <td headers="Kum n" class="gt_row gt_right">—</td>
#> <td headers="Kum %" class="gt_row gt_right">—</td></tr>
#>   </tbody>
#>   
#> </table>
#> </div>
mtcars |> knit_frequencies(mpg, num_decimal = 1, 
  percent_decimal = 1, cums = FALSE, 
  name_total_percent = "Percent")


  

mpg
```
