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
    select(selected_vars) |>
    sjlabelled::label_to_colnames() |>
    names()

  dt <- data |>
    mutate(.temp_weight = if (!is.null({{weight}})) {{weight}} else 1) |>
    sjlabelled::label_to_colnames()

  tables <- map(selected_var_labels, ~ {
    var_sym <- sym(.x)
    n_total <- sum(dt$.temp_weight, na.rm = TRUE)  # Gesamtanzahl für Prozente berechnen

    n_valid <- dt %>%
      filter(!is.na(!!var_sym)) %>%
      summarise(n_valid = sum(.temp_weight, na.rm = TRUE)) %>%
      pull(n_valid)

  t <- dt %>%
    group_by(!!var_sym) %>%
    summarise(n = sum(.temp_weight, na.rm = TRUE), .groups = "drop") %>%
    mutate(
    percent = n / n_total,
    valid_percent = if_else(is.na(!!var_sym), 0, n / n_valid))

  if(cums == TRUE){
    t <- t |>
      mutate(n_cum = cumsum(n),
             cum_valid_percent = cumsum(valid_percent))
  }

  gt <- t |>
    sjlabelled::copy_labels(dt) |>
    mutate(!!var_sym := coalesce(sjlabelled::as_character(!!var_sym), as.character(!!var_sym))) |>
    gt::gt() |>
    gt::fmt_number(columns = n, decimals = num_decimal, use_seps = FALSE)|>
    gt::fmt_percent(columns = c(percent, valid_percent), decimals = percent_decimal, use_seps = FALSE) |>
    gt::cols_label(
      percent = "total %",
      valid_percent = "valid %"
      ) |>
    gt::text_transform(
      locations = gt::cells_body(
        columns = c(cum_valid_percent, valid_percent),
        rows = is.na(!!var_sym)
      ),
      fn = function(x) "—"
    )

 if(cums == TRUE){
   gt <- gt |>
     gt::fmt_number(columns = c(n_cum), decimals = num_decimal, use_seps = FALSE)|>
     gt::fmt_percent(columns = c(cum_valid_percent), decimals = percent_decimal, use_seps = FALSE) |>
     gt::cols_label(
       n_cum = "cum n",
       cum_valid_percent = "cum %"
     )
 }

gt |>
  # gt::tab_style(
  #   style = gt::cell_borders(
  #     sides = "t",
  #     color = "grey20",  # dunkleres Grau für eine stärkere Linie
  #     weight = gt::px(2)     # gleiche Dicke wie die Standardabschlusslinien
  #   ),
  #   locations = gt::cells_body(
  #     rows = nrow(gt[["_data"]])
  #   )
  # ) |>
  gt::tab_style(
    style = gt::cell_text(color = "grey60"),
    locations = gt::cells_body(
      rows = nrow(gt[["_data"]])
    )
  )

  })
  purrr::walk(tables, print)
}
