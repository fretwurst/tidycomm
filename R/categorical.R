#' Tabulate frequencies
#'
#' Tabulates frequencies for one or more categorical variable, including relative,
#' and cumulative frequencies.
#'
#' @param data a [tibble][tibble::tibble-package] or a [tdcmm] model
#' @param ... Variables to tabulate
#'
#' @return a [tdcmm] model
#'
#' @examples
#' WoJ %>% tab_frequencies(employment)
#' WoJ %>% tab_frequencies(employment, country)
#'
#' @family categorical
#'
#' @export
tab_frequencies <- function(data, ...) {
  vars <- grab_vars(data, enquos(...))
  vars_str <- purrr::map_chr(vars, as_label)

  grouping <- dplyr::groups(data)

  d <- data %>%
    dplyr::group_by(..., !!!grouping) %>%
    dplyr::summarise(n = dplyr::n()) %>%
    dplyr::group_by(!!!grouping) %>%
    dplyr::mutate(percent = n / sum(n)) %>%
    dplyr::arrange(!!!grouping)

  out <- d %>%
    dplyr::bind_cols(
      d %>%
        dplyr::select(!!!grouping, cum_n = "n", cum_percent = "percent") %>%
        dplyr::mutate_at(dplyr::vars(-dplyr::group_cols()), cumsum) %>%
        dplyr::ungroup() %>%
        dplyr::select("cum_n", "cum_percent")
    )

  return(new_tdcmm_ctgrcl(new_tdcmm(
    out,
    func = "tab_frequencies",
    data = data,
    params = list(vars = vars_str)
  )))
}

#' Crosstab variables
#'
#' Computes contingency table for one independent (column) variable and one or
#' more dependent (row) variables.
#'
#' @param data a [tibble][tibble::tibble-package] or a [tdcmm] model
#' @param col_var Independent (column) variable.
#' @param ... Dependent (row) variables.
#' @param add_total Logical indicating whether a 'Total' column should be
#'   computed. Defaults to `FALSE`.
#' @param percentages Logical indicating whether to output column-wise
#'   percentages instead of absolute values. Defaults to `FALSE`.
#' @param chi_square Logical indicating whether a Chi-square test should be computed.
#'   Test results will be reported via message(). Defaults to `FALSE`.
#'
#' @return a [tdcmm] model
#'
#' @examples
#' WoJ %>% crosstab(reach, employment)
#' WoJ %>% crosstab(reach, employment, add_total = TRUE, percentages = TRUE, chi_square = TRUE)
#'
#' @family categorical
#'
#' @export
crosstab <- function(
  data,
  col_var,
  ...,
  add_total = FALSE,
  percentages = FALSE,
  chi_square = FALSE
) {
  # Checks
  if (dplyr::is_grouped_df(data)) {
    warning(
      "Grouping variable(s) present in data will be ignored.",
      call. = FALSE
    )
  }

  vars <- grab_vars(data, enquos(...))
  vars_str <- purrr::map_chr(vars, as_label)

  cross_vars <- length(quos(...))

  if (cross_vars < 1) {
    stop("Must provide at least one variable to crosstabulate.")
  }

  # Prepare crosstab
  xt <- data %>%
    dplyr::group_by({{ col_var }}, ...) %>%
    dplyr::count() %>%
    tidyr::spread({{ col_var }}, n, fill = 0) %>%
    dplyr::ungroup()

  xt_cross_vars <- xt %>%
    dplyr::select(1:tidyselect::all_of(cross_vars))

  xt_col_vars <- xt %>%
    dplyr::select(-(1:tidyselect::all_of(cross_vars)))

  # Estimate Chi-square test
  if (chi_square) {
    chi2 <- xt_col_vars %>%
      as.matrix() %>%
      chisq.test()
  }

  # Augment
  if (add_total) {
    xt_col_vars <- xt_col_vars %>%
      dplyr::mutate(Total = rowSums(xt_col_vars))
  }

  if (percentages) {
    xt_col_vars <- xt_col_vars %>%
      dplyr::mutate_all(col_percs)
  }

  # Output
  out <- xt_cross_vars %>%
    dplyr::bind_cols(xt_col_vars)

  if (chi_square) {
    return(new_tdcmm_ctgrcl(
      new_tdcmm(
        out,
        func = "crosstab",
        data = data,
        params = list(
          vars = vars_str,
          col_var = as_name(enquo(col_var)),
          add_total = add_total,
          percentages = percentages,
          chi_square = chi_square
        ),
        model = list(chi2)
      )
    ))
  } else {
    return(new_tdcmm_ctgrcl(
      new_tdcmm(
        out,
        func = "crosstab",
        data = data,
        params = list(
          vars = vars_str,
          col_var = as_name(enquo(col_var)),
          add_total = add_total,
          percentages = percentages,
          chi_square = chi_square
        )
      )
    ))
  }
}

#' @rdname visualize
#' @export
visualize.tdcmm_ctgrcl <- function(x, ..., .design = design_lmu()) {
  if (attr(x, "func") == "tab_frequencies") {
    return(visualize_tab_frequencies(x, .design))
  }

  if (attr(x, "func") == "crosstab") {
    return(visualize_crosstab(x, .design))
  }

  return(warn_about_missing_visualization(x))
}

# Internal functions ----

## Compute Cramer's V
##
## Computes Cramer's V
##
## @param chi2 Output from a `chisq.test()`.
##
## @return a `dbl`
##
## @family categorical
##
## @keywords internal
cramer_V <- function(chi2) {
  X2 <- chi2$statistic
  N <- sum(chi2$observed)
  k = min(dim(chi2$observed))

  unname(sqrt(X2 / (N * (k - 1))))
}

## Compute column percentages
##
## Computes column percentages
##
## @param x Numeric vector
##
## @return a `dbl`
col_percs <- function(x) {
  x / sum(x, na.rm = TRUE)
}

## Visualize `tab_frequencies()` as one or many histogram(s)
##
## @param x a [tdcmm] model
##
## @return a [ggplot2] object
##
## @family tdcmm visualize
##
## @keywords internal
visualize_tab_frequencies <- function(x, design = design_lmu()) {
  var_names <- attr(x, "params")$vars
  num_histograms <- length(var_names)

  # collect data
  data <- NULL
  for (variable in var_names) {
    data <- data %>%
      rbind(
        attr(x, "data") %>%
          tab_frequencies(!!sym(variable)) %>%
          dplyr::mutate(
            var = variable,
            level = forcats::as_factor(.data[[variable]])
          ) %>%
          dplyr::select(var, level, percent)
      )
  }

  # visualize
  g <- data %>%
    ggplot2::ggplot(ggplot2::aes(x = level, y = percent)) +
    ggplot2::geom_bar(stat = "identity", fill = design$main_color_1) +
    ggplot2::facet_wrap(dplyr::vars(var), scales = "free_x") +
    ggplot2::scale_x_discrete(NULL) +
    ggplot2::scale_y_continuous(
      NULL,
      labels = percentage_labeller,
      limits = c(0, 1),
      breaks = seq(0, 1, .1)
    ) +
    design$theme()

  # wrap depending on number of variables
  if (num_histograms >= 5) {
    warning(
      glue(
        "Visualizing too many histograms at once might strongly ",
        "inhibit readability. Consider reducing the number of ",
        "variables in tab_frequencies() before calling visualize()."
      ),
      call. = FALSE
    )
  }

  return(g)
}

## Visualize `crosstab()` as horizontal stacked bar plot, either absolute or
## relative (depending on `percentages`).
##
## @param x a [tdcmm] model
##
## @return a [ggplot2] object
##
## @family tdcmm visualize
#
## @keywords internal
visualize_crosstab <- function(x, design = design_lmu()) {
  independent_var_string <- attr(x, "params")$col_var
  dependent_var_strings <- attr(x, "params")$vars
  dependent_var_string <- dependent_var_strings[1]

  if (length(dependent_var_strings) > 1) {
    stop(
      glue(
        "Visualizing multiple crosstabs at once looks overwhelming. ",
        "Consider reducing the number of variables in crosstab() to ",
        "two before calling visualize()."
      ),
      call. = FALSE
    )
  }

  data <- x %>%
    tidyr::pivot_longer(
      !c(!!sym(dependent_var_string)),
      names_to = "label_independent"
    ) %>%
    dplyr::mutate(
      label_independent = forcats::as_factor(label_independent),
      label_independent_desc = forcats::fct_rev(label_independent)
    ) %>%
    dplyr::rename(level = !!sym(dependent_var_string))

  if (length(dplyr::n_distinct(data$label_independent)) > 12) {
    stop(
      glue(
        "Cannot visualize crosstabs with more than 12 levels of the ",
        "independent variable ({independent_var_string})."
      ),
      call. = FALSE
    )
  }

  # visualize
  g <- data %>%
    ggplot2::ggplot(ggplot2::aes(
      x = value,
      y = label_independent_desc,
      fill = level
    )) +
    ggplot2::geom_bar(stat = "identity", position = "stack")

  if (attr(x, "params")$percentages) {
    g <- g +
      ggplot2::geom_text(
        ggplot2::aes(label = percentage_labeller(value), color = level),
        position = ggplot2::position_stack(vjust = .5)
      ) +
      ggplot2::scale_x_continuous(
        NULL,
        labels = percentage_labeller,
        limits = c(0, 1),
        breaks = seq(0, 1, .1)
      )
  } else {
    g <- g +
      ggplot2::geom_text(
        ggplot2::aes(label = value, color = level),
        position = ggplot2::position_stack(vjust = .5)
      ) +
      ggplot2::scale_x_continuous('N', limits = c(0, NA), n.breaks = 10)
  }

  g <- g +
    ggplot2::scale_y_discrete(NULL) +
    ggplot2::scale_fill_manual(
      NULL,
      values = design$main_colors,
      guide = ggplot2::guide_legend(reverse = TRUE)
    ) +
    ggplot2::scale_color_manual(
      NULL,
      values = design$main_contrasts,
      guide = NULL
    ) +
    design$theme() +
    ggplot2::theme(legend.position = "bottom")

  return(g)
}

# Table Formatting S3 Method ----

#' Format categorical results as a table
#'
#' @param x a `tdcmm_ctgrcl` model
#' @param width_pct table width in percent. Default is 80.
#' @param num_decimal decimal places for raw numbers. Default is 0.
#' @param percent_decimal decimal places for percentages. Default is 0.
#' @param name_row_total label for the row totals. Default is "Total".
#' @param add_col_totals Logical indicating whether to add a bottom row with column totals. Default is TRUE.
#' @param dv_name Optional character string to manually label the dependent (row) variable in a crosstab.
#' @param iv_name Optional character string to manually label the independent (column) variable in a crosstab.
#' @param var_names Optional named character vector to rename target variables in frequencies (e.g., `c("old_name" = "New Name")`).
#' @param name_n Rename the "n" column for frequencies. Default is "N".
#' @param name_percent Rename the "percent" column for frequencies. Default is "Percent".
#' @param name_cum_n Rename the "cum_n" column for frequencies. Default is "Cum. N".
#' @param name_cum_percent Rename the "cum_percent" column for frequencies. Default is "Cum. %".
#' @param ... further arguments passed to or from other methods.
#'
#' @rdname format_table
#'
#' @examples
#' \dontrun{
#' WoJ %>%
#'   crosstab(reach, employment, add_total = TRUE, percentages = TRUE, chi_square = TRUE) %>%
#'   format_table(
#'     dv_name = "Anstellungsverhältnis",
#'     iv_name = "Reichweite des Mediums",
#'     name_row_total = "Gesamt"
#'   )
#'
#'  WoJ %>%
#'   tab_frequencies(work_experience) %>%
#'   format_table(var_names = c("work_experience" = "Work Experience"))
#'
#'  WoJ %>%
#'   tab_frequencies(work_experience) %>%
#'   format_table(
#'     var_names = c("work_experience" = "Berufserfahrung (Jahre)"),
#'     name_n = "Anzahl",
#'     name_percent = "Prozent",
#'     name_cum_n = "Kum. Anzahl",
#'     name_cum_percent = "Kum. Prozent",
#'     name_row_total = "Gesamt"
#'   )
#' }
#'
#' @export
format_table.tdcmm_ctgrcl <- function(
  x,
  width_pct = 80,
  num_decimal = 0,
  percent_decimal = 0,
  add_col_totals = TRUE,
  name_row_total = "Total",
  dv_name = NULL,
  iv_name = NULL,
  var_names = NULL,
  name_n = "N",
  name_percent = "Percent",
  name_cum_n = "Cum. N",
  name_cum_percent = "Cum. %",
  ...
) {
  # 1. Formatierung für tab_frequencies()
  if (attr(x, "func") == "tab_frequencies") {
    t <- x

    # Custom Labels für die Variablen anwenden
    if (!is.null(var_names)) {
      valid_old_names <- names(var_names)[names(var_names) %in% colnames(t)]
      if (length(valid_old_names) > 0) {
        rename_mapping <- stats::setNames(
          valid_old_names,
          var_names[valid_old_names]
        )
        t <- t %>% dplyr::rename(!!!rename_mapping)
      }
    }

    # Statistische Spalten umbenennen
    t <- t %>%
      dplyr::rename(
        !!name_n := n,
        !!name_percent := percent,
        !!name_cum_n := cum_n,
        !!name_cum_percent := cum_percent
      )

    # Zeilensumme für den unteren Abschluss berechnen
    if (add_col_totals) {
      t_sum <- t %>%
        dplyr::summarise(dplyr::across(
          tidyselect::any_of(c(name_n, name_percent)),
          ~ sum(.x, na.rm = TRUE)
        ))

      # Die allererste Spalte ist die Gruppierungsvariable
      first_col <- colnames(t)[1]

      # FIX: Originalspalte in Character umwandeln, um Type-Mismatch (<double> vs <character>) zu vermeiden
      t <- t %>%
        dplyr::mutate(
          !!rlang::sym(first_col) := as.character(!!rlang::sym(first_col))
        )

      # Total-Label in die Summen-Zeile schreiben und anhängen
      t_sum <- t_sum %>%
        dplyr::mutate(!!rlang::sym(first_col) := name_row_total)
      t <- t %>% dplyr::bind_rows(t_sum)
    }

    # gt-Tabelle aufbauen
    tab_knit <- t %>%
      gt::gt() %>%
      gt::fmt_number(
        columns = tidyselect::any_of(c(name_n, name_cum_n)),
        decimals = num_decimal
      ) %>%
      gt::fmt_percent(
        columns = tidyselect::any_of(c(name_percent, name_cum_percent)),
        decimals = percent_decimal
      ) %>%
      gt::sub_missing(
        columns = tidyselect::everything(),
        missing_text = "---"
      ) %>%
      gt::tab_options(table.width = gt::pct(width_pct))

    if (knitr::is_html_output() || knitr::is_latex_output() || interactive()) {
      return(tab_knit)
    }
    if (knitr::pandoc_to("docx")) {
      return(gt::as_raw_html(tab_knit))
    }
    return(x)
  }

  # 2. Formatierung für crosstab()
  if (attr(x, "func") == "crosstab") {
    t <- x
    row_var_original <- colnames(t)[1]
    col_var_original <- attr(x, "params")$col_var

    # Custom Labels für Row (DV) und Column (IV) anwenden
    dv_label <- if (!is.null(dv_name)) {
      dv_name
    } else {
      stringr::str_to_title(gsub("_", " ", row_var_original))
    }
    iv_label <- if (!is.null(iv_name)) {
      iv_name
    } else {
      stringr::str_to_title(gsub("_", " ", col_var_original))
    }

    # Parameter auslesen, die bereits in crosstab() berechnet wurden
    is_percentages <- attr(x, "params")$percentages
    has_chi <- attr(x, "params")$chi_square
    chi2 <- if (has_chi) attr(x, "model")[[1]] else NULL

    # Wurde in crosstab() add_total = TRUE gesetzt, benennen wir die "Total"-Spalte passend um
    if ("Total" %in% colnames(t)) {
      t <- t %>% dplyr::rename(!!name_row_total := Total)
    }

    # Zeilensumme für den unteren Abschluss berechnen (Spaltensummen), nur wenn gewollt
    if (add_col_totals) {
      t_sum <- t %>%
        dplyr::summarise(dplyr::across(
          tidyselect::where(is.numeric),
          ~ sum(.x, na.rm = TRUE)
        )) %>%
        dplyr::mutate(!!rlang::sym(row_var_original) := name_row_total)

      # Originalspalte in Character umwandeln, um Type-Mismatch zu vermeiden
      t <- t %>%
        dplyr::mutate(
          !!rlang::sym(row_var_original) := as.character(
            !!rlang::sym(row_var_original)
          )
        )

      t <- t %>% dplyr::bind_rows(t_sum)
    }

    # Erste Spalte (DV) sauber labeln
    t <- t %>% dplyr::rename(!!dv_label := !!rlang::sym(row_var_original))

    # gt-Tabelle aufbauen (IV wird als Spanner über die Spalten gelegt)
    gt_tab <- t %>%
      gt::gt() %>%
      gt::tab_spanner(
        label = iv_label,
        columns = -1 # Spanner über alle numerischen Spalten legen
      )

    # Entweder als Prozent oder als absolute Nummern formatieren
    if (is_percentages) {
      gt_tab <- gt_tab %>%
        gt::fmt_percent(
          columns = -1,
          decimals = percent_decimal,
          use_seps = FALSE
        )
    } else {
      gt_tab <- gt_tab %>%
        gt::fmt_number(columns = -1, decimals = num_decimal, use_seps = FALSE)
    }

    gt_tab <- gt_tab %>%
      gt::sub_missing(
        columns = tidyselect::everything(),
        missing_text = "---"
      ) %>%
      gt::tab_options(table.width = gt::pct(width_pct))

    # Fußnote mit Chi-Quadrat und Cramer's V anhängen
    if (has_chi && !is.null(chi2)) {
      V <- cramer_V(chi2)
      n_total <- sum(chi2$observed)
      p_fmt <- formatC(chi2$p.value, format = "f", digits = 3) %>%
        sub("^(-?)0\\.", "\\1.", x = .)

      gt_tab <- gt_tab %>%
        gt::tab_footnote(
          footnote = glue::glue(
            "𝜒²(df={chi2$parameter}, N={round(n_total, 0)}) = {round(chi2$statistic, 1)}, p = {p_fmt}; V = {round(V, 2)}"
          )
        )
    }

    # Ausgabe steuern (für Konsole, HTML oder Word)
    if (knitr::is_html_output() || knitr::is_latex_output() || interactive()) {
      return(gt_tab)
    }
    if (knitr::pandoc_to("docx")) {
      return(gt::as_raw_html(gt_tab))
    }

    return(x)
  }

  return(x)
}


# Constructors ----

new_tdcmm_ctgrcl <- function(x) {
  stopifnot(is_tdcmm(x))

  structure(
    x,
    class = c("tdcmm_ctgrcl", class(x))
  )
}

# Formatting ----

#' @export
tbl_format_footer.tdcmm_ctgrcl <- function(x, ...) {
  default_footer <- NextMethod()

  if (
    attr(x, "func") != "crosstab" |
      length(attr(x, "params")) == 0 |
      is.null(attr(x, "model"))
  ) {
    return(default_footer)
  }

  # Get values
  chi2 <- model(x)

  # Format test string
  test_string <- glue(
    "Chi-square = {format_value(chi2$statistic, 3)}, ",
    "df = {format(chi2$parameter, digits = 4)}, ",
    "{format_pvalue(chi2$p.value)}, ",
    "V = {format_value(cramer_V(chi2), 3)}"
  )

  # Add to footer and display
  test_footer <- style_subtle(glue("# {test_string}"))

  c(default_footer, test_footer)
}
