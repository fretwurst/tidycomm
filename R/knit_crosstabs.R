#' knit crosstab table variables
#'
#' Knits table for one ore more variables
#'
#' @param data a [tibble][tibble::tibble-package]
#' @param col_var the column variable.
#' @param row_var the row variable.
#' @param weight Weighting variable. Default = NULL, means all weights are 1.
#' @param num_decimal Decimal places of the numeric columns. Default is 0.
#' @param percent_decimal Decimal places of the percent columns. Default is 0.
#' @param name_percent = "Prozent" Decimal places of the percent columns. Default is 0.
#' @param name_row_total the name of the "Total" column.
#' @param cums = FALSE Decimal places of the percent columns. Default is 0.
#' @param na = FALSE include NA. Default is FALSE.
#' @param chi_square = FALSE include NA. Default is FALSE.
#'
#' @return a gt-table
#'
#' @examples
#' WoJ |> knit_crosstab(row_var = employment, col_var = reach)
#' WoJ |> knit_crosstab(row_var = employment, col_var = reach, num_decimal = 1, percent_decimal = 1, cums = FALSE, name_percent = "Percent")
#'
#' @family categorical
#'
#' @export
knit_crosstab <- function(
  data,
  row_var,
  col_var,
  width_pct = 80,
  weight = NULL,
  na = FALSE,
  percent = TRUE,
  num_decimal = 0,
  percent_decimal = 0,
  chi_square = TRUE,
  name_row_total = "Gesamt",
  name_percent = "Prozent",
  cums = TRUE
) {
  dt <- data |>
    dplyr::select({{ col_var }}, {{ row_var }}, tidyselect::any_of(weight)) |>
    dplyr::mutate(
      .temp_weight = if (!is.null({{ weight }})) {{ weight }} else {
        1
      }
    )

  if (na == FALSE) {
    dt <- dt |>
      dplyr::filter_out(is.na({{ col_var }}) | is.na({{ row_var }}))
  }

  n_total <- sum(dt$.temp_weight, na.rm = TRUE) # Gesamtanzahl für Prozente berechnen

  n_valid <- dt |>
    dplyr::summarise(n_valid = sum(.temp_weight, na.rm = TRUE)) |>
    dplyr::pull(n_valid)

  t <- dt |>
    dplyr::count({{ col_var }}, {{ row_var }}, wt = .temp_weight) |>
    sjlabelled::label_to_colnames() |>
    tidyr::pivot_wider(names_from = 1, values_from = n)

  if (chi_square) {
    chi2 <- t %>%
      dplyr::select(where(is.numeric)) |>
      as.matrix() |>
      chisq.test()

    V <- sqrt(
      unname(chi2$statistic) /
        (sum(chi2$observed) *
          min(nrow(chi2$observed) - 1, ncol(chi2$observed) - 1))
    )

    p_fmt <- formatC(
      chi2$p.value,
      format = "f",
      digits = 3
    ) |>
      sub("^0", "", x = _)
  }

  t <- t |>
    dplyr::mutate(
      Total = rowSums(dplyr::pick(tidyselect::where(is.numeric)), na.rm = TRUE)
    ) |>
    dplyr::select(tidyselect::everything(), Total) |>
    dplyr::rename(!!name_row_total := Total)

  if (percent == TRUE) {
    t <- t |>
      dplyr::mutate(dplyr::across(
        tidyselect::where(is.numeric),
        ~ .x / sum(.x, na.rm = TRUE)
      ))
  }

  t_sum <- t |>
    dplyr::summarise(dplyr::across(
      tidyselect::where(is.numeric),
      ~ sum(.x, na.rm = TRUE)
    )) |>
    dplyr::mutate(Total = !!name_row_total) |>
    dplyr::rename({{ row_var }} := Total)

  t <- t |>
    dplyr::bind_rows(t_sum)

  gt <- t |> gt::gt()

  if (percent) {
    gt <- gt |>
      gt::fmt_percent(
        columns = -1,
        decimals = percent_decimal,
        use_seps = FALSE
      )
  }

  gt <- gt |>
    gt::sub_missing(
      columns = tidyselect::everything(),
      missing_text = "---"
    ) |>
    gt::tab_options(
      table.width = gt::pct(width_pct)
    )

  if (chi_square) {
    gt <- gt |>
      gt::tab_footnote(
        footnote = glue::glue(
          "𝜒²(df={chi2$parameter}, N={n_total})={chi2$statistic  |> round(1)}, p={p_fmt}; V={V |> round(2)}"
        )
      )
  }
  return(gt)
}
