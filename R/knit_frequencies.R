library(dplyr)
library(purrr)
library(rlang)
library(tidyselect)

knit_frequencies <- function(data,
                             ...,
                             weight = NULL,
                             num_decimal = 0,
                             percent_decimal = 0,
                             cums = TRUE) {

  selected_vars <- tidyselect::eval_select(expr(c(...)), data = data)
  selected_vars <- names(selected_vars) # Namen der ausgewählten Variablen

  dt <- data %>%
    mutate(.temp_weight = if (!is.null({{weight}})) {{weight}} else 1)

  map(selected_vars, ~ {
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
    gt::gt() |>
    gt::fmt_number(columns = n, decimals = num_decimal)|>
    gt::fmt_percent(columns = c(percent, valid_percent), decimals = percent_decimal) |>
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
     gt::fmt_number(columns = c(n_cum), decimals = num_decimal)|>
     gt::fmt_percent(columns = c(cum_valid_percent), decimals = percent_decimal) |>
     gt::cols_label(
       n_cum = "cum n",
       cum_valid_percent = "cum %"
     )
 }

return(gt)
  })
}

# Funktion aufrufen
DATEN |>
  knit_frequencies(var1, var2, var3, var4:var12, c(var13, var14, var15:var30), cums = TRUE)
