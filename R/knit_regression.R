#' Compute linear regression
#'
#' Computes linear regression for all independent variables on the specified
#' dependent variable. Linear modeling of multiple independent variables uses
#' stepwise regression modeling. If specified, preconditions for
#' (multi-)collinearity and for homoscedasticity are checked.
#'
#' @param x a [tdcmm] model
#' @param digits the decimal digits
#' @param CIs show the Confidence intervals or not. Default to TRUE.
#' @param cap Set the caption. Default is NULL, because in Quarto its better to set the tbl-cap in the chunk options
#' @param R2adj Show R^2adj in the table footnote. Default is FALSE.
#'
#' @return a gt-table
#'
#' @examples
#' WoJ |> regress(autonomy_selection, ethics_1) |>
#'   knit_regress_table()
#' WoJ %>% regress(autonomy_selection, work_experience, trust_government) |>
#'   knit_regress_table(digits = 3, CIs = FALSE, cap = "Regression on Autonomy Selection")
#'
#' @export
knit_regress_table <- function(x,
                               digits = 2,
                               R2adj = FALSE,
                               CIs = TRUE,
                               cap = NULL,
                               B = B,
                               LL = LL,
                               UL = UL,
                               beta_LL_compare = NULL,
                               beta_UL_compare = NULL,
                               beta_LL = NULL,
                               beta_UL = NULL
                               ) {
  model <- model(x)

  SDs <- model$model |>
    dplyr::summarise(dplyr::across(dplyr::everything(), ~sd(.x, na.rm = TRUE))) |>
    tidyr::pivot_longer(cols = dplyr::everything(), names_to = "Variable", values_to = "SD")

  sd_Y <- SDs[1,2] |>
    dplyr::pull()

  model_tibble <- x

  if (CIs == TRUE){
  model_tibble <- model_tibble |>
    dplyr::left_join(SDs, by = "Variable") |>
    dplyr::mutate(beta = B * SD/sd_Y,
                  LL = stats::confint(model)[,1],
                  UL = stats::confint(model)[,2],
                  beta_LL = LL * SD/sd_Y,
                  beta_UL = UL * SD/sd_Y
                  ) |>
    dplyr::select(dplyr::any_of(c('Variable',
                           'B', 'StdErr', 'LL', 'UL',
                           'beta', 'beta_LL', 'beta_UL',
                           't', 'p',
                           'TOL', 'VIF')))}

  model_summary <- summary(model)

  pf <- pf(model_summary$fstatistic[["value"]],
        model_summary$fstatistic[["numdf"]],
        model_summary$fstatistic[["dendf"]],
        lower.tail = FALSE) |>
    format.pval(eps = .001, nsmall = 3) %>%
    gsub("0\\.","\\.", .)

  R_squared <- model_summary$r.squared |>
    round(3)  %>%
    gsub("0\\.","\\.", .)

if(model$df.residual < 100 | R2adj == TRUE) {

  R_squared_adj <- model_summary$adj.r.squared |>
    round(3) %>%
    gsub("0\\.","\\.", .) 
  
  R_squared <- glue::glue("{R_squared}, R²adj = {R_squared_adj}") 
}

  F <- model_summary$fstatistic[['value']] |>
    round(0)

  dependent_var <- model$terms[[2]] %>%
   gsub("_", " ", .) %>%
    stringr::str_to_title(.)

  if(!is.null(cap)) {
    cap <- glue::glue("Regression Model on {dependent_var}")
  }

  quality_notes_R2 <- glue::glue("{dependent_var}, R² = {R_squared}")

  quality_notes_F <- glue::glue("
        F({model_summary$fstatistic[['numdf']]},{model_summary$fstatistic[['dendf']]}) = {F}, p = {pf}, CI-Level = 95%")
  
  

  tab <- model_tibble
  tab_format <- tab |>
    dplyr::select(dplyr::any_of(c('Variable',
                    'B', 'StdErr', 'LL', 'UL',
                    'beta',
       #            'beta_LL', 'beta_UL',
       #             'beta_LL_compare', 'beta_UL_compare',
                    't', 'p',
                    'TOL', 'VIF'))) |>
    dplyr::mutate(dplyr::across(-1, ~round(.x, digits)),
                  dplyr::across(dplyr::any_of("p"), ~format.pval(.x, eps = .001, nsmall = 3, na.form = "—")),
                  dplyr::across(dplyr::any_of("p"), ~gsub("0\\.","\\.", .x))) |>
    dplyr::mutate(
      dplyr::across(dplyr::any_of(c("beta", "beta_LL", "beta_UL", "TOL")),
             ~ dplyr::if_else(is.na(.x), "—", sub("^(-?)0.", "\\1.", sprintf("%.3f", .x))))
    ) |>
    dplyr::mutate(dplyr::across(dplyr::any_of("VIF"), as.character))

  tab_knit <- tab_format |>
    gt::gt() |>
    gt::cols_align(align = ("right"),
                   columns = -1) |>
    gt::tab_footnote(footnote = quality_notes_R2, placement = "left") |>
    gt::tab_footnote(footnote = quality_notes_F, placement = "left") |>
    gt::tab_options(
      table.width = gt::pct(80)) |>
    gt::tab_spanner(label = "unstd.",
                    columns =  c("B",
                                 "StdErr",
                                 dplyr::starts_with("LL"),
                                 dplyr::starts_with("UL")
                    )) |>
    gt::tab_spanner(label = "std.",
                    columns = c("beta",
                                dplyr::starts_with("beta_LL"),
                                dplyr::starts_with("beta_UL"))) |>
    gt::tab_spanner(label = "sig.",
                    columns = c("t", "p")) |>
    gt::tab_spanner(label = "multicoll.",
                    columns = c(dplyr::starts_with("TOL"),
                                dplyr::starts_with("VIF"))) |>
    gt::sub_missing() |>
    gt::cols_label(StdErr = "SE B",
                   beta = "B*")

  if(CIs == TRUE){
  tab_knit <-  tab_knit |>
    gt::cols_label(LL = "LL",
                   UL = "UL",
      #            beta_LL = "LL",
      #            beta_UL = "UL"
                   )

  }

  if (knitr::is_html_output() |
      knitr::is_latex_output() |
      interactive() # if interactive in R-Studio
      ){

  return(tab_knit)

  } else if (knitr::pandoc_to("docx")){

  tab_knit <- tab_knit |>
    gt::as_raw_html()

  return(tab_knit)

  } else {
 return(x)
  }
}

## Visualize swimming BETA confidence intervals
## Usefull for comparing BETAs and BETAs vs points like 0
##
## @param x a [tdcmm] model
##
## @return a [ggplot2] object
##
## @family tdcmm visualize
#
## @keywords internal
visualize_regress_sbci <- function(x,
                                   .design = NULL,
                                   title = NULL,
                                   B = B,
                                   LL = LL,
                                   UL = UL,
                                   beta_LL_compare = NULL,
                                   beta_UL_compare = NULL,
                                   beta_LL = NULL,
                                   beta_UL = NULL
                                   ){

  if(!is.null(.design)){
    design <- .design
  } else if(!is.null(getOption("design"))){
    design <- getOption("design")
  } else {
    design <- tidycomm::design_viridis()
  }

  if(is.null(NULL)){
    title = "Beta Coefficients with Confidence Intervals"
  }

  model <- model(x)

  # Berechne die Standardabweichungen
  SDs <- model$model |>
    dplyr::summarise(dplyr::across(dplyr::everything(), ~sd(.x, na.rm = TRUE))) |>
    tidyr::pivot_longer(cols = dplyr::everything(), names_to = "Variable", values_to = "SD")

  sd_Y <- SDs[1, 2] |>
    dplyr::pull()

  model_tibble <- x

  # Füge SD und andere benötigte Variablen hinzu und berechne betas und Konfidenzintervalle
  model_tibble <- model_tibble |>
    dplyr::left_join(SDs, by = "Variable") |>
    dplyr::mutate(beta = B * SD / sd_Y,
                  LL = stats::confint(model)[, 1],
                  UL = stats::confint(model)[, 2],
                  beta_LL = LL * SD / sd_Y,
                  beta_UL = UL * SD / sd_Y,
                  beta_LL_compare = stats::confint(model, level = .9)[, 1] * SD / sd_Y,
                  beta_UL_compare = stats::confint(model, level = .9)[, 2] * SD / sd_Y) |>
    dplyr::select(dplyr::any_of(c('Variable', 'B', 'StdErr', 'LL', 'UL', 'beta', 'beta_LL', 'beta_UL',
                                  'beta_LL_compare', 'beta_UL_compare', 't', 'p', 'TOL', 'VIF')))

  # Setze die Variable-Spalte als Faktor, basierend auf der Reihenfolge in model_tibble und drehe die Reihenfolge um
  model_tibble <- model_tibble |>
    dplyr::mutate(Variable = factor(Variable, levels = rev(Variable)))  # Sortiere umgekehrt für die korrekte Reihenfolge

  # Erzeuge das Diagramm
  sbci <- model_tibble |>
    dplyr::filter(!is.na(beta)) |>
    ggplot2::ggplot(aes(y = Variable, x = beta)) +
    ggplot2::geom_segment(aes(x = beta_LL_compare,
                     xend = beta_UL_compare,
                     y = Variable,
                     yend = Variable,
                     color = "CIs for BETA comparison"),
                 size = 4,
                 alpha = 1) +
    # Dünne Linie für die normalen CIs
    ggplot2::geom_segment(aes(x = beta_LL,
                     xend = beta_UL,
                     y = Variable,
                     yend = Variable,
                     color = "CIs for point comparison"),
                 size = 0.8,
                 alpha = 1) +
    # Verwende geom_point, um das "I" als Punkt anzuzeigen
    ggplot2::geom_point(aes(x = beta, y = Variable, color = "Beta-Value"),
               shape = "|", size = 5) +  # Hier wird das "I" als Punkt verwendet
    ggplot2::geom_vline(aes(xintercept = 0),
               color = "grey",
               linetype = "dotted",
               size = .8) +
    # Achsenbeschriftungen
    ggplot2::labs(title = title,
         x = "Beta",
         y = "",
         color = "Legend") +
    ggplot2::scale_color_manual(values = c(
      "Beta-Value" = design$main_color_1,
      "CIs for BETA comparison" = design$main_colors[2],
      "CIs for point comparison" = design$main_colors[3]
    )) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_blank(),   # Entferne alle großen Gitternetzlinien
      panel.grid.minor = ggplot2::element_blank(),   # Entferne alle kleinen Gitternetzlinien
      legend.position = "bottom"
    )

  return(sbci)
}
