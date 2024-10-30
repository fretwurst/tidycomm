#' knit frequencies table variables
#'
#' Knits table for one ore more variables
#'
#' @param data a [tibble][tibble::tibble-package]
#' @param weight Weighting variable. Default = NULL, means all weights are 1.
#' @param ... List of variables.
#' @param num_decimals Decimal places of the numeric columns. Default is 0.
#' @param percent_decimals Decimal places of the percent columns. Default is 0.
#'
#' @return a gt-table
#'
#' @examples
#' WoJ |> knit_frequencies(reach, employment)
#' mtcars |> knit_frequencies(mpg, num_decimal = 1, percent_decimal = 1, cums = FALSE)
#'
#' @family categorical
#'
#' @export
knit_frequencies <- function(data,
                             ...,
                             weight = NULL,
                             num_decimal = 0,
                             percent_decimal = 0,
                             cums = TRUE) {

  selected_vars <- tidyselect::eval_select(expr(c(...)), data = data) |>
    names()

  selected_var_labels <- data |>
    dplyr::select(selected_vars) |>
    sjlabelled::label_to_colnames() |>
    names()

  dt <- data |>
    dplyr::mutate(.temp_weight = if (!is.null({{weight}})) {{weight}} else 1) |>
    sjlabelled::label_to_colnames()

  tables <- purrr::map(selected_var_labels, ~ {
    var_sym <- sym(.x)
    n_total <- sum(dt$.temp_weight, na.rm = TRUE)  # Gesamtanzahl für Prozente berechnen

    n_valid <- dt %>%
      dplyr::filter(!is.na(!!var_sym)) %>%
      dplyr::summarise(n_valid = sum(.temp_weight, na.rm = TRUE)) %>%
      dplyr::pull(n_valid)

  t <- dt %>%
    dplyr::group_by(!!var_sym) %>%
    dplyr::summarise(n = sum(.temp_weight, na.rm = TRUE), .groups = "drop") %>%
    dplyr::mutate(
    percent = n / n_total,
    valid_percent = dplyr::if_else(is.na(!!var_sym), 0, n / n_valid))

  if(cums == TRUE){
    t <- t |>
      dplyr::mutate(n_cum = cumsum(n),
             cum_valid_percent = cumsum(valid_percent))
  }

  gt <- t |>
    sjlabelled::copy_labels(dt) |>
    dplyr::mutate(!!var_sym := dplyr::coalesce(sjlabelled::as_character(!!var_sym), as.character(!!var_sym))) |>
    gt::gt() |>
    gt::fmt_number(columns = n, decimals = num_decimal, use_seps = FALSE)|>
    gt::fmt_percent(columns = c(percent, valid_percent), decimals = percent_decimal, use_seps = FALSE) |>
    gt::cols_label(
      percent = "total %",
      valid_percent = "valid %"
      ) |>
    gt::text_transform(
      locations = gt::cells_body(
        columns = c(valid_percent),
        rows = is.na(!!var_sym)
      ),
      fn = function(x) "---"
    )

 if(cums == TRUE){
   gt <- gt |>
     gt::fmt_number(columns = c(n_cum), decimals = num_decimal, use_seps = FALSE)|>
     gt::fmt_percent(columns = c(cum_valid_percent), decimals = percent_decimal, use_seps = FALSE) |>
     gt::cols_label(
       n_cum = "cum n",
       cum_valid_percent = "cum %"
     )|>
     gt::text_transform(
       locations = gt::cells_body(
         columns = c(cum_valid_percent),
         rows = is.na(!!var_sym)
       ),
       fn = function(x) "---"
     )
 }

 gt <- gt |>
  gt::tab_style(
    style = gt::cell_text(color = "grey60"),
    locations = gt::cells_body(
      rows = is.na(!!var_sym)
    )
  )

  })

 if (length(tables) == 1) {
   gt_table <- tables[[1]]
   return(gt_table)
 } else {
   purrr::walk(tables, print)
 }

}
