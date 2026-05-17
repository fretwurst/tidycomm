#' Compute linear regression
#'
#' Computes linear regression for all independent variables on the specified
#' dependent variable. Linear modeling of multiple independent variables uses
#' stepwise regression modeling. If specified, preconditions for
#' (multi-)collinearity and for homoscedasticity are checked.
#'
#' @param data a [tibble][tibble::tibble-package], or a [tdcmm] model or an lm model
#' @param dependent_var The dependent variable on which the linear model is
#'   fitted. Specify as column name.
#' @param ... Independent variables to take into account as (one or many)
#'   predictors for the dependent variable. Specify as column names. At least
#'   one has to be specified.
#' @param check_independenterrors if set, the independence of errors among any
#'   two cases is being checked using a Durbin-Watson test
#' @param check_multicollinearity if set, multicollinearity among all specified
#'   independent variables is being checked using the variance inflation factor
#'   (VIF) and the tolerance (1/VIF); this check can only be performed if at
#'   least two independent variables are provided, and all provided variables
#'   need to be numeric
#' @param check_homoscedasticity if set, homoscedasticity is being checked
#'   using a Breusch-Pagan test
#' @param formula if exist, the lm-formula is used
#'
#' @return a [tdcmm] model
#'
#' @examples
#' WoJ %>% regress(autonomy_selection, ethics_1)
#' WoJ %>% regress(autonomy_selection, work_experience, trust_government)
#'
#' @export

regress <- function(
  data,
  dependent_var,
  ...,
  check_independenterrors = FALSE,
  check_multicollinearity = FALSE,
  check_homoscedasticity = FALSE
) {
  # Check if 'data' is an lm model
  if (inherits(data, "lm")) {
    # Extract the formula and the data from the lm model
    model <- data
    model_formula <- stats::formula(model) # Extract the formula from the model
    data <- dplyr::as_tibble(model$model) # Extract the data from the model
    yvar_string <- all.vars(model_formula)[1] # Dependent variable
    xvars_string <- all.vars(model_formula)[-1] # Independent variables

    # Create dummy placeholders for yvar and xvars to ensure compatibility
    yvar <- rlang::sym(yvar_string) # Convert yvar_string back to symbol
    xvars <- rlang::syms(xvars_string) # Convert xvars_string back to symbols
  } else {
    # Original processing for when data and variables are provided separately
    yvar <- expr({{ dependent_var }})
    xvars <- grab_vars(data, enquos(...), alternative = "none")

    yvar_string <- as_label(yvar) # Convert dependent variable to string
    xvars_string <- purrr::map_chr(xvars, as_label) # Convert independent variables to string
  }

  # basic checks
  if (length(xvars) == 0) {
    stop("No independent variable(s) given.")
  }

  yvar_numeric <- data %>%
    dplyr::select(!!yvar) %>%
    dplyr::select_if(is.numeric) %>%
    names() %>%
    length()

  if (yvar_numeric < 1) {
    stop("Dependent variable must be numeric.")
  }

  xvars_count <- data %>%
    dplyr::select(!!!xvars) %>%
    names() %>%
    length()

  xvars_count_numeric <- data %>%
    dplyr::select(!!!xvars) %>%
    dplyr::select_if(is.numeric) %>%
    names() %>%
    length()

  xvars_count_factor <- data %>%
    dplyr::select(!!!xvars) %>%
    dplyr::select_if(is.factor) %>%
    names() %>%
    length()

  if ((xvars_count_factor + xvars_count_numeric) < xvars_count) {
    stop("At least one independent variable is neither numeric nor a factor.")
  }

  if (dplyr::is.grouped_df(data)) {
    warning(
      "regress does not support grouped data. Groups will be dropped.",
      call. = FALSE
    )
    data <- dplyr::ungroup(data)
  }

  # main lm
  model_formula <- as.formula(sprintf(
    "%s ~ %s",
    yvar_string,
    paste(xvars_string, collapse = " + ")
  ))

  model <- stats::lm(model_formula, data)

  model_summary <- summary(model)

  model_tibble <-
    tibble::tibble(
      Variable = dimnames(model_summary$coefficients)[[1]],
      B = model_summary$coefficients[, 1],
      StdErr = model_summary$coefficients[, 2],
      beta = lm.beta::lm.beta(model)$standardized.coefficients,
      t = model_summary$coefficients[, 3],
      p = model_summary$coefficients[, 4]
    )

  # checks
  model_checks <- list()

  if (check_independenterrors) {
    check_durbin_watson <- car::durbinWatsonTest(model)
    model_checks[['independenterrors']] <- check_durbin_watson
  }

  if (check_homoscedasticity) {
    check_breusch_pagan <- car::ncvTest(model)
    model_checks[['homoscedasticity']] <- check_breusch_pagan
  }

  if (check_multicollinearity) {
    if (xvars_count < 2) {
      warning(paste0(
        "multicollinearity (VIF) checks are only applicable with ",
        "2+ independent variables. No VIF will be computed."
      ))
    } else {
      if (xvars_count_factor > 0) {
        warning(paste0(
          "multicollinearity (VIF) checks are only applicable ",
          "to numeric (rather than factorial) independent ",
          "variables. No VIF will be computed."
        ))
      } else {
        check_vif <- car::vif(model)
        model_tibble <- model_tibble %>%
          dplyr::bind_cols(tibble::tibble(
            VIF = c(NA, check_vif),
            TOL = c(NA, 1 / check_vif)
          ))
        model_checks[['multicollinearity']] <- check_vif
      }
    }
  }

  checks_factors <- list()
  if (xvars_count_factor > 0) {
    for (xvar in xvars_string) {
      if (is.factor(data[[xvar]])) {
        checks_factors[[length(checks_factors) + 1]] <-
          c(xvar, levels(data[[xvar]])[[1]])
      }
    }
  }
  model_checks[['factors']] <- checks_factors

  # return ----
  return(new_tdcmm_rgrssn(
    new_tdcmm(
      model_tibble,
      func = "regress",
      data = data,
      params = list(
        dependent_var = yvar_string,
        vars = xvars_string,
        check_independenterrors = check_independenterrors,
        check_multicollinearity = check_multicollinearity,
        check_homoscedasticity = check_homoscedasticity
      ),
      model = list(model),
      checks = model_checks
    )
  ))
}

#' @param which string to specify type of regression visualization. One of
#' "jitter" (default), "alpha", "correlogram", "residualsfitted" (or "resfit"),
#' "pp", "qq", "scalelocation" (or "scaloc"), "residualsleverage" (or "reslev"), "sbci".
#' See below for details.
#'
#' @rdname visualize
#' @export
visualize.tdcmm_rgrssn <- function(
  x,
  which = "jitter",
  ...,
  .design = NULL,
  title = NULL
) {
  if (!is.null(.design)) {
    .design <- .design
  } else if (!is.null(getOption("design"))) {
    .design <- getOption("design")
  } else {
    .design <- tidycomm::design_lmu()
  }

  if (attr(x, "func") == "regress") {
    which <- tolower(which)
    if (which == "correlogram") {
      return(visualize_regress_correlogram(x, .design))
    }
    if (which == "residualsfitted" | which == "resfit") {
      return(visualize_regress_resfit(x, .design))
    }
    if (which == "pp") {
      return(visualize_regress_pp(x, .design))
    }
    if (which == "sbci") {
      return(visualize_regress_sbci(x, .design, title = title))
    }
    if (which == "qq") {
      return(visualize_regress_qq(x, .design))
    }
    if (which == "scalelocation" | which == "scaloc") {
      return(visualize_regress_scaloc(x, .design))
    }
    if (which == "residualsleverage" | which == "reslev") {
      return(visualize_regress_reslev(x, .design))
    }
    if (!which %in% c("jitter", "alpha")) {
      warning(
        glue(
          'which must be one of "jitter", "alpha", "correlogram", ',
          '"sbci",',
          '"residualsfitted" (or "resfit"), "pp", "qq", ',
          '"scalelocation" (or "scaloc"), ',
          'or "residualsleverage" (or "reslev"). Since none was ',
          'provided, "jitter" is considered by default.'
        ),
        call. = FALSE
      )
      which <- "jitter"
    }
    return(visualize_regress_lm(x, which, .design))
  }

  return(warn_about_missing_visualization(x))
}


# Formatting ----

#' @export
tbl_format_footer.tdcmm_rgrssn <- function(x, ...) {
  default_footer <- NextMethod()
  footers <- c(default_footer)

  # Get values
  model <- model(x)
  model_summary <- summary(model)
  model_checks <- attr(x, "checks")
  model_check_names <- names(model_checks)

  # overall quality
  quality_footer <- sprintf(
    "F(%d, %d) = %f, p = %f, R-square = %f",
    model_summary$fstatistic[["numdf"]],
    model_summary$fstatistic[["dendf"]],
    model_summary$fstatistic[["value"]],
    pf(
      model_summary$fstatistic[["value"]],
      model_summary$fstatistic[["numdf"]],
      model_summary$fstatistic[["dendf"]],
      lower.tail = FALSE
    ),
    model_summary$r.squared
  )
  footers <- c(footers, glue("# {quality_footer}"))

  # checks
  if ("independenterrors" %in% model_check_names) {
    durbin_watson_footer <- sprintf(
      paste0("Check for independent errors: ", "Durbin-Watson = %f (p = %f)"),
      model_checks$independenterrors$dw,
      model_checks$independenterrors$p
    )
    footers <- c(footers, glue("- {durbin_watson_footer}"))
  }

  if ("homoscedasticity" %in% model_check_names) {
    breusch_pagan_footer <- sprintf(
      paste0("Check for homoscedasticity: ", "Breusch-Pagan = %f (p = %f)"),
      model_checks$homoscedasticity$ChiSquare,
      model_checks$homoscedasticity$p
    )
    footers <- c(footers, glue("- {breusch_pagan_footer}"))
  }

  if ("multicollinearity" %in% model_check_names) {
    footers <- c(
      footers,
      "- Check for multicollinearity: VIF/tolerance added to output"
    )
  }

  if (length(model_checks$factors) > 0) {
    for (i in 1:length(model_checks$factors)) {
      factor_footer <- sprintf(
        paste0(
          "%s is a factor and was split into one ",
          "variable per level, each compared to ",
          "'%s' (reference level)"
        ),
        model_checks$factors[[i]][[1]],
        model_checks$factors[[i]][[2]]
      )
      footers <- c(footers, glue("- {factor_footer}"))
    }
  }

  style_subtle(footers)
}

### Internal functions ###

## Visualize as scatter plot with linear model.
##
## @param x a [tdcmm] model
##
## @return a [ggplot2] object
##
## @family tdcmm visualize
#
## @keywords internal
visualize_regress_lm <- function(x, which = "jitter", design = design_lmu()) {
  g <- attr(x, "data") %>%
    dplyr::select(
      !!sym(attr(x, "params")$dependent_var),
      !!!syms(attr(x, "params")$vars)
    ) %>%
    dplyr::rename(dependent_var = !!sym(attr(x, "params")$dependent_var)) %>%
    dplyr::mutate(dplyr::across(dplyr::where(is.factor), as.numeric)) %>%
    tidyr::pivot_longer(c(!!!syms(attr(x, "params")$vars)), names_to = "iv") %>%
    dplyr::mutate(
      iv = forcats::fct(.data$iv, levels = attr(x, "params")$vars)
    ) %>%
    ggplot2::ggplot(ggplot2::aes(x = .data$value, y = .data$dependent_var))
  if (which == "jitter") {
    g <- g +
      ggplot2::geom_jitter(width = .3, height = .3, na.rm = TRUE)
  } else {
    g <- g +
      ggplot2::geom_point(alpha = .25, na.rm = TRUE)
  }

  g +
    ggplot2::geom_smooth(
      method = "lm",
      se = TRUE,
      level = .95,
      formula = "y ~ x",
      na.rm = TRUE,
      color = design$main_color_1,
      linewidth = design$main_size
    ) +
    ggplot2::facet_wrap(dplyr::vars(.data$iv), scales = "free_x") +
    ggplot2::scale_x_continuous(NULL) +
    ggplot2::scale_y_continuous(attr(x, "params")$dependent_var) +
    design$theme()
}

## Visualize as correlogram between independent variables.
## Helps to determine indpendent errors and multicollinearity.
##
## @param x a [tdcmm] model
##
## @return a [ggplot2] object
##
## @family tdcmm visualize
##
## @see visualize.tdcmm_crrltn
#
## @keywords internal
visualize_regress_correlogram <- function(x, design = design_lmu()) {
  if (length(attr(x, "checks")$factors) > 0) {
    factor_variables <- c()
    for (i in 1:length(attr(x, "checks")$factors)) {
      factor_variables <- c(
        factor_variables,
        attr(x, "checks")$factors[[i]][[1]]
      )
    }
    warning(
      glue(
        "only numeric variables will be included in this plot, ",
        "all factor variables (",
        paste(factor_variables, collapse = ", "),
        ") have been dropped for this visualization"
      ),
      call. = FALSE
    )
  }
  attr(x, "data") %>%
    dplyr::select(!!!syms(attr(x, "params")$vars)) %>%
    dplyr::select_if(is.numeric) %>%
    correlate() %>%
    to_correlation_matrix() %>%
    visualize(.design = design)
}

## Visualize as residuals v. fitted plot.
## Useful for determining distributions.
##
## @param x a [tdcmm] model
##
## @return a [ggplot2] object
##
## @family tdcmm visualize
##
## @keywords internal
visualize_regress_resfit <- function(x, design = design_lmu()) {
  m <- model(x)
  mf <- ggplot2::fortify(m)
  lowess_fit <- dplyr::as_tibble(stats::lowess(x = mf$.fitted, y = mf$.resid))
  mf %>%
    ggplot2::ggplot(ggplot2::aes(x = .data$.fitted, y = .data$.resid)) +
    ggplot2::geom_hline(
      yintercept = 0,
      linetype = design$comparison_linetype,
      color = design$comparison_color,
      linewidth = design$main_size
    ) +
    ggplot2::geom_point() +
    ggplot2::geom_path(
      data = lowess_fit,
      ggplot2::aes(x = .data$x, y = .data$y),
      color = design$main_color_1,
      linewidth = design$main_size
    ) +
    ggplot2::scale_x_continuous("Fitted", n.breaks = 8) +
    ggplot2::scale_y_continuous("Residuals", n.breaks = 8) +
    design$theme(panel.grid = ggplot2::element_blank())
}

## Visualize as probability-probability plot.
## Useful for checking multicollinearity (focus on the IQR).
##
## @param x a [tdcmm] model
##
## @return a [ggplot2] object
##
## @family tdcmm visualize
#
## @keywords internal
visualize_regress_pp <- function(x, design = design_lmu()) {
  x %>%
    model() %>%
    ggplot2::fortify() %>%
    tibble::as_tibble() %>%
    dplyr::mutate(
      .p_theoretical = (1:dplyr::n()) / dplyr::n() - .5 / dplyr::n(),
      .p_sample = sort(stats::pnorm(
        .data$.stdresid,
        mean(.data$.stdresid),
        sd(.data$.stdresid)
      ))
    ) %>%
    ggplot2::ggplot(ggplot2::aes(
      x = .data$.p_theoretical,
      y = .data$.p_sample
    )) +
    ggplot2::geom_abline(
      intercept = 0,
      slope = 1,
      color = design$comparison_color,
      linewidth = design$comparison_size,
      linetype = design$comparison_linetype
    ) +
    ggplot2::geom_point(alpha = .25) +
    ggplot2::scale_x_continuous("Theoretical Probability", n.breaks = 8) +
    ggplot2::scale_y_continuous("Sample Residual Probability", n.breaks = 8) +
    design$theme(panel.grid = ggplot2::element_blank())
}

## Visualize as quantile-quantile plot.
## Useful for checking multicollinearity (focus on outliers).
##
## @param x a [tdcmm] model
##
## @return a [ggplot2] object
##
## @family tdcmm visualize
#
## @keywords internal
visualize_regress_qq <- function(x, design = design_lmu()) {
  x %>%
    model() %>%
    ggplot2::ggplot(ggplot2::aes(sample = .data$.stdresid)) +
    ggplot2::stat_qq(alpha = .25) +
    ggplot2::geom_qq_line(
      color = design$main_color_1,
      linewidth = design$main_size
    ) +
    ggplot2::scale_x_continuous("Theoretical Quantiles", n.breaks = 8) +
    ggplot2::scale_y_continuous("Sample Quantiles", n.breaks = 8) +
    design$theme(panel.grid = ggplot2::element_blank())
}

## Visualize as scale-location plot.
## Useful for checking homoscedasticity.
##
## @param x a [tdcmm] model
##
## @return a [ggplot2] object
##
## @family tdcmm visualize
##
## @keywords internal
visualize_regress_scaloc <- function(x, design = design_lmu()) {
  x %>%
    model() %>%
    ggplot2::ggplot(ggplot2::aes(
      x = .data$.fitted,
      y = sqrt(abs(.data$.stdresid))
    )) +
    ggplot2::geom_point(na.rm = T, alpha = .25) +
    ggplot2::stat_smooth(
      method = "loess",
      se = FALSE,
      color = design$main_color_1,
      linewidth = design$main_size,
      formula = "y ~ x"
    ) +
    ggplot2::scale_x_continuous("Fitted Values", n.breaks = 8) +
    ggplot2::scale_y_continuous(
      expression(sqrt("|Standardized Residuals|")),
      n.breaks = 8
    ) +
    design$theme(panel.grid = ggplot2::element_blank())
}

## Visualize as residuals v. leverage plot.
## Useful for checking for influential single cases ("outliers").
##
## @param x a [tdcmm] model
##
## @return a [ggplot2] object
##
## @family tdcmm visualize
##
## @keywords internal
visualize_regress_reslev <- function(x, design = design_lmu()) {
  data_labels <- x %>%
    model() %>%
    ggplot2::fortify() %>%
    dplyr::as_tibble() %>%
    tibble::rownames_to_column(var = "case_number") %>%
    dplyr::slice_max(.data$.cooksd, n = 5)
  x %>%
    model() %>%
    ggplot2::ggplot(ggplot2::aes(x = .data$.hat, y = .data$.stdresid)) +
    ggplot2::geom_point(alpha = .25) +
    ggplot2::geom_smooth(
      method = "loess",
      se = FALSE,
      color = design$main_color_1,
      linewidth = design$main_size,
      formula = "y ~ x"
    ) +
    ggplot2::geom_text(
      data = data_labels,
      ggplot2::aes(label = .data$case_number),
      check_overlap = TRUE,
      nudge_x = .00075,
      color = design$main_color_1
    ) +
    ggplot2::scale_x_continuous("Leverage", n.breaks = 8) +
    ggplot2::scale_y_continuous("Standardized Residuals", n.breaks = 8) +
    design$theme(panel.grid = ggplot2::element_blank())
}

## Visualize swimming BETA confidence intervals
## Usefull for comparing BETAs and BETAs vs points like 0
##
## @param x a [tdcmm] model
## @param .design design settings
## @param title character string for the plot title
##
## @return a [ggplot2] object
##
## @family tdcmm visualize
#
## @keywords internal
visualize_regress_sbci <- function(x, .design = NULL, title = NULL) {
  if (!is.null(.design)) {
    design <- .design
  } else if (!is.null(getOption("design"))) {
    design <- getOption("design")
  } else {
    design <- tidycomm::design_lmu()
  }

  if (is.null(title)) {
    title <- "Beta Coefficients with Confidence Intervals"
  }

  m <- model(x)
  dependent_var <- attr(x, "params")$dependent_var

  sd_Y <- sd(m$model[[dependent_var]], na.rm = TRUE)

  # FIX: model.matrix() nutzen, um Faktoren als numerische Dummies zu behandeln
  SDs <- as.data.frame(stats::model.matrix(m)) %>%
    dplyr::summarise(dplyr::across(
      dplyr::everything(),
      ~ sd(.x, na.rm = TRUE)
    )) %>%
    tidyr::pivot_longer(
      cols = dplyr::everything(),
      names_to = "Variable",
      values_to = "SD"
    )

  ci_95 <- stats::confint(m, level = 0.95)
  ci_90 <- stats::confint(m, level = 0.90)

  ci_tibble <- tibble::tibble(
    Variable = rownames(ci_95),
    LL = ci_95[, 1],
    UL = ci_95[, 2],
    LL_compare = ci_90[, 1],
    UL_compare = ci_90[, 2]
  )

  model_tibble <- x %>%
    dplyr::left_join(SDs, by = "Variable") %>%
    dplyr::left_join(ci_tibble, by = "Variable") %>%
    dplyr::mutate(
      beta = B * (SD / sd_Y),
      beta_LL = LL * (SD / sd_Y),
      beta_UL = UL * (SD / sd_Y),
      beta_LL_compare = LL_compare * (SD / sd_Y),
      beta_UL_compare = UL_compare * (SD / sd_Y)
    )

  model_tibble <- model_tibble %>%
    dplyr::filter(!is.na(beta)) %>%
    dplyr::mutate(Variable = factor(Variable, levels = rev(Variable)))

  sbci <- model_tibble %>%
    ggplot2::ggplot(ggplot2::aes(y = .data$Variable, x = .data$beta)) +
    ggplot2::geom_segment(
      ggplot2::aes(
        x = .data$beta_LL_compare,
        xend = .data$beta_UL_compare,
        y = .data$Variable,
        yend = .data$Variable,
        color = "CIs for BETA comparison"
      ),
      linewidth = 4,
      alpha = 0.3
    ) +
    ggplot2::geom_segment(
      ggplot2::aes(
        x = .data$beta_LL,
        xend = .data$beta_UL,
        y = .data$Variable,
        yend = .data$Variable,
        color = "CIs for point comparison"
      ),
      linewidth = 0.9,
      alpha = 1.0
    ) +
    ggplot2::geom_vline(
      xintercept = 0,
      color = "grey",
      linetype = "dotted",
      linewidth = 0.8
    ) +
    ggplot2::geom_point(
      ggplot2::aes(color = "Beta-Value"),
      shape = "|",
      size = 7,
      alpha = 1.0
    ) +
    ggplot2::labs(
      title = title,
      x = "Beta",
      y = NULL,
      color = "Legend"
    ) +
    ggplot2::scale_color_manual(
      values = c(
        "Beta-Value" = design$main_colors[8],
        "CIs for BETA comparison" = design$main_color_1,
        "CIs for point comparison" = design$main_colors[6]
      )
    ) +
    design$theme() +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      legend.position = "bottom"
    )

  return(sbci)
}

#' Format linear regression results as a table
#'
#' @param x a `tdcmm_rgrssn` model
#' @param digits the decimal digits
#' @param CIs show the Confidence intervals or not. Default to TRUE.
#' @param cap Set the caption. Default is NULL.
#' @param R2adj Show R^2adj in the table footnote. Default is FALSE.
#' @param dv_name Optional character string to manually label the dependent variable.
#' @param iv_names Optional named character vector to rename independent variables in the table (e.g., `c("old_name" = "New Name")`).
#' @param ... further arguments passed to or from other methods.
#'
#' @rdname format_table
#'
#' @examples
#' \dontrun{
#' WoJ %>%
#' regress(autonomy_selection, work_experience, trust_government) %>%
#' format_table(
#' dv_name = "Autonomieauswahl der Journalisten",
#' iv_names = c(
#' "work_experience" = "Berufserfahrung (Jahre)",
#' "trust_government" = "Vertrauen in die Regierung",
#' "(Intercept)" = "Konstante"
#' )
#' )
#' }
#' @export
format_table.tdcmm_rgrssn <- function(
  x,
  digits = 2,
  R2adj = FALSE,
  CIs = TRUE,
  cap = NULL,
  dv_name = NULL,
  iv_names = NULL,
  ...
) {
  m <- model(x)
  dependent_var_original <- attr(x, "params")$dependent_var
  sd_Y <- sd(m$model[[dependent_var_original]], na.rm = TRUE)

  # FIX: model.matrix() nutzen, um Faktoren als numerische Dummies zu behandeln
  SDs <- as.data.frame(stats::model.matrix(m)) %>%
    dplyr::summarise(dplyr::across(
      dplyr::everything(),
      ~ sd(.x, na.rm = TRUE)
    )) %>%
    tidyr::pivot_longer(
      cols = dplyr::everything(),
      names_to = "Variable",
      values_to = "SD"
    )

  model_tibble <- x

  if (CIs) {
    ci_vals <- stats::confint(m)
    ci_tibble <- tibble::tibble(
      Variable = rownames(ci_vals),
      LL = ci_vals[, 1],
      UL = ci_vals[, 2]
    )

    model_tibble <- model_tibble %>%
      dplyr::left_join(SDs, by = "Variable") %>%
      dplyr::left_join(ci_tibble, by = "Variable") %>%
      dplyr::mutate(
        beta = B * (SD / sd_Y),
        beta_LL = LL * (SD / sd_Y),
        beta_UL = UL * (SD / sd_Y)
      ) %>%
      dplyr::select(dplyr::any_of(c(
        'Variable',
        'B',
        'StdErr',
        'LL',
        'UL',
        'beta',
        'beta_LL',
        'beta_UL',
        't',
        'p',
        'TOL',
        'VIF'
      )))
  }

  if (!is.null(iv_names)) {
    model_tibble <- model_tibble %>%
      dplyr::mutate(
        Variable = dplyr::coalesce(unname(iv_names[Variable]), Variable)
      )
  }

  model_summary <- summary(m)
  f_stat <- model_summary$fstatistic

  pf_str <- format.pval(
    pf(
      f_stat[["value"]],
      f_stat[["numdf"]],
      f_stat[["dendf"]],
      lower.tail = FALSE
    ),
    eps = .001,
    nsmall = 3
  ) %>%
    sub("^(-?)0\\.", "\\1.", x = .)

  r_sq_str <- sprintf("%.3f", round(model_summary$r.squared, 3)) %>%
    sub("^(-?)0\\.", "\\1.", x = .)

  if (m$df.residual < 100 || R2adj) {
    r_sq_adj_str <- sprintf("%.3f", round(model_summary$adj.r.squared, 3)) %>%
      sub("^(-?)0\\.", "\\1.", x = .)
    r_sq_str <- glue::glue("{r_sq_str}, R²adj = {r_sq_adj_str}")
  }

  F_val <- round(f_stat[['value']], 2)

  if (!is.null(dv_name)) {
    dependent_var_clean <- dv_name
  } else {
    dependent_var_clean <- stringr::str_to_title(gsub(
      "_",
      " ",
      dependent_var_original
    ))
  }

  if (is.null(cap)) {
    cap <- glue::glue("Regression Model on {dependent_var_clean}")
  }

  quality_notes_R2 <- glue::glue("{dependent_var_clean}, R² = {r_sq_str}")
  quality_notes_F <- glue::glue(
    "F({f_stat[['numdf']]}, {f_stat[['dendf']]}) = {F_val}, p = {pf_str}, CI-Level = 95%"
  )

  tab_format <- model_tibble %>%
    dplyr::mutate(
      dplyr::across(dplyr::where(is.numeric), ~ round(.x, digits)),
      p = format.pval(p, eps = .001, nsmall = 3, na.form = "—"),
      p = sub("^(-?)0\\.", "\\1.", p)
    ) %>%
    dplyr::mutate(dplyr::across(
      dplyr::any_of(c("beta", "beta_LL", "beta_UL", "TOL")),
      ~ dplyr::if_else(
        is.na(.x),
        "—",
        sub("^(-?)0\\.", "\\1.", sprintf(paste0("%.", digits, "f"), .x))
      )
    )) %>%
    dplyr::mutate(dplyr::across(dplyr::any_of("VIF"), as.character))

  tab_knit <- tab_format %>%
    gt::gt() %>%
    gt::cols_align(align = "right", columns = -1) %>%
    gt::tab_footnote(footnote = quality_notes_R2, placement = "left") %>%
    gt::tab_footnote(footnote = quality_notes_F, placement = "left") %>%
    gt::tab_options(table.width = gt::pct(85)) %>%
    gt::tab_spanner(
      label = "unstd.",
      columns = c(
        "B",
        "StdErr",
        dplyr::starts_with("LL"),
        dplyr::starts_with("UL")
      )
    ) %>%
    gt::tab_spanner(
      label = "std.",
      columns = c(
        "beta",
        dplyr::starts_with("beta_LL"),
        dplyr::starts_with("beta_UL")
      )
    ) %>%
    gt::tab_spanner(label = "sig.", columns = c("t", "p")) %>%
    gt::tab_spanner(
      label = "multicoll.",
      columns = c(dplyr::starts_with("TOL"), dplyr::starts_with("VIF"))
    ) %>%
    gt::sub_missing() %>%
    gt::cols_label(StdErr = "SE B", beta = "B*")

  if (CIs) {
    tab_knit <- tab_knit %>% gt::cols_label(LL = "LL", UL = "UL")
  }

  # Output Handling
  if (knitr::is_html_output() || knitr::is_latex_output() || interactive()) {
    return(tab_knit)
  }
  if (knitr::pandoc_to("docx")) {
    return(gt::as_raw_html(tab_knit))
  }
  return(x)
}

# Constructors ----

new_tdcmm_rgrssn <- function(x) {
  stopifnot(is_tdcmm(x))

  structure(
    x,
    class = c("tdcmm_rgrssn", class(x))
  )
}
