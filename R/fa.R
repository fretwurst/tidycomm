#' Principal component analysis for teaching
#' (based on base R without psych or factoextra etc.)
#'
#' Performs a principal component analysis based on a correlation matrix
#' and applies an orthogonal Varimax rotation.
#'
#' @param .data A data frame.
#' @param ... Variables to include, using tidyselect syntax.
#' @param n_components Optional fixed number of components.
#' @param determine Method used to determine the number of components.
#'   Either `"kaiser"` or `"parallel"`.
#' @param correlations Should the correlation matrix be included in the
#'   returned object?
#' @param missing Treatment of missing values. Currently only `"listwise"`.
#' @param loading_cutoff Loading threshold used for the print method.
#' @param parallel_repetitions Number of random data sets for parallel analysis.
#' @param parallel_quantile Quantile of random eigenvalues used for parallel
#'   analysis.
#' @param seed Optional random seed for parallel analysis.
#'
#' @return An object of class `tidycomm_pca`.
#'
#' @export
principal_components <- function(
  .data,
  ...,
  n_components = NULL,
  determine = c("kaiser", "parallel"),
  correlations = FALSE,
  missing = "listwise",
  loading_cutoff = 0.30,
  parallel_repetitions = 500,
  parallel_quantile = 0.95,
  seed = NULL
) {
  determine <- match.arg(determine)

  if (!identical(missing, "listwise")) {
    stop(
      "`missing` currently has to be \"listwise\".",
      call. = FALSE
    )
  }

  if (
    !is.numeric(loading_cutoff) ||
      length(loading_cutoff) != 1L ||
      is.na(loading_cutoff) ||
      loading_cutoff < 0 ||
      loading_cutoff > 1
  ) {
    stop(
      "`loading_cutoff` must be a single number between 0 and 1.",
      call. = FALSE
    )
  }

  selected_data <- .data |>
    select(...)

  if (ncol(selected_data) < 3L) {
    stop(
      "Select at least three variables for the analysis.",
      call. = FALSE
    )
  }

  non_numeric <- names(selected_data)[
    !vapply(selected_data, is.numeric, logical(1))
  ]

  if (length(non_numeric) > 0L) {
    stop(
      paste0(
        "All selected variables must be numeric. ",
        "Non-numeric variables: ",
        paste(non_numeric, collapse = ", "),
        "."
      ),
      call. = FALSE
    )
  }

  variable_names <- names(selected_data)

  complete_rows <- stats::complete.cases(selected_data)

  analysis_data <- selected_data[
    complete_rows,
    ,
    drop = FALSE
  ]

  n_observations <- nrow(analysis_data)
  n_variables <- ncol(analysis_data)
  n_missing_rows <- sum(!complete_rows)

  if (n_observations < 3L) {
    stop(
      "Fewer than three complete observations remain after removing missing values.",
      call. = FALSE
    )
  }

  standard_deviations <- vapply(
    analysis_data,
    stats::sd,
    numeric(1)
  )

  constant_variables <- names(standard_deviations)[
    !is.finite(standard_deviations) |
      standard_deviations <= sqrt(.Machine$double.eps)
  ]

  if (length(constant_variables) > 0L) {
    stop(
      paste0(
        "The following variables have no usable variance: ",
        paste(constant_variables, collapse = ", "),
        "."
      ),
      call. = FALSE
    )
  }

  correlation_matrix <- stats::cor(
    analysis_data,
    use = "everything"
  )

  if (any(!is.finite(correlation_matrix))) {
    stop(
      "The correlation matrix contains non-finite values.",
      call. = FALSE
    )
  }

  eigen_decomposition <- eigen(
    correlation_matrix,
    symmetric = TRUE
  )

  eigenvalues <- eigen_decomposition$values

  tolerance <- max(eigenvalues) *
    n_variables *
    .Machine$double.eps

  if (min(eigenvalues) <= tolerance) {
    stop(
      paste0(
        "The correlation matrix is singular or nearly singular. ",
        "This usually means that variables are redundant or almost ",
        "perfectly correlated."
      ),
      call. = FALSE
    )
  }

  diagnostics <- pca_diagnostics(
    correlation_matrix = correlation_matrix,
    n_observations = n_observations
  )

  parallel_results <- NULL

  if (!is.null(n_components)) {
    if (
      !is.numeric(n_components) ||
        length(n_components) != 1L ||
        is.na(n_components) ||
        n_components %% 1 != 0 ||
        n_components < 1L ||
        n_components > n_variables
    ) {
      stop(
        paste0(
          "`n_components` must be a whole number between 1 and ",
          n_variables,
          "."
        ),
        call. = FALSE
      )
    }

    retained_components <- as.integer(n_components)
    selection_method <- "fixed"
  } else if (determine == "kaiser") {
    retained_components <- sum(eigenvalues > 1)

    if (retained_components == 0L) {
      retained_components <- 1L

      warning(
        paste0(
          "No eigenvalue was greater than 1. ",
          "One component was retained."
        ),
        call. = FALSE
      )
    }

    selection_method <- "kaiser"
  } else {
    parallel_results <- pca_parallel_analysis(
      n_observations = n_observations,
      n_variables = n_variables,
      observed_eigenvalues = eigenvalues,
      repetitions = parallel_repetitions,
      quantile = parallel_quantile,
      seed = seed
    )

    retained_components <- parallel_results$n_components
    selection_method <- "parallel"
  }

  unrotated_loadings <- eigen_decomposition$vectors[
    ,
    seq_len(retained_components),
    drop = FALSE
  ] |>
    sweep(
      MARGIN = 2,
      STATS = sqrt(eigenvalues[seq_len(retained_components)]),
      FUN = "*"
    )

  rownames(unrotated_loadings) <- variable_names

  if (retained_components > 1L) {
    rotation <- stats::varimax(unrotated_loadings)

    rotated_loadings <- unclass(rotation$loadings)
    rotation_matrix <- rotation$rotmat
  } else {
    rotated_loadings <- unrotated_loadings
    rotation_matrix <- matrix(
      1,
      nrow = 1,
      ncol = 1,
      dimnames = list("PC1", "PC1")
    )
  }

  component_variances <- colSums(rotated_loadings^2)

  component_order <- order(
    component_variances,
    decreasing = TRUE
  )

  rotated_loadings <- rotated_loadings[
    ,
    component_order,
    drop = FALSE
  ]

  component_variances <- component_variances[component_order]

  component_names <- paste0(
    "PC",
    seq_len(retained_components)
  )

  colnames(rotated_loadings) <- component_names

  dominant_component_number <- max.col(
    abs(rotated_loadings),
    ties.method = "first"
  )

  dominant_loading <- rotated_loadings[
    cbind(
      seq_len(nrow(rotated_loadings)),
      dominant_component_number
    )
  ]

  communality <- rowSums(rotated_loadings^2)

  variable_order <- order(
    dominant_component_number,
    -abs(dominant_loading),
    variable_names
  )

  loading_table <- rotated_loadings |>
    as.data.frame() |>
    rownames_to_column("variable") |>
    as_tibble() |>
    mutate(
      dominant_component = component_names[
        dominant_component_number
      ],
      dominant_loading = dominant_loading,
      communality = communality,
      uniqueness = 1 - communality
    ) |>
    slice(variable_order) |>
    relocate(
      variable,
      dominant_component,
      dominant_loading,
      all_of(component_names),
      communality,
      uniqueness
    )

  variance_table <- tibble(
    component = component_names,
    sum_squared_loadings = component_variances,
    proportion_variance = component_variances / n_variables,
    cumulative_variance = cumsum(
      component_variances / n_variables
    )
  )

  scree_data <- tibble(
    component = seq_len(n_variables),
    eigenvalue = eigenvalues,
    retained = seq_len(n_variables) <= retained_components
  )

  if (!is.null(parallel_results)) {
    scree_data <- scree_data |>
      mutate(
        parallel_eigenvalue =
          parallel_results$reference_eigenvalues
      )
  }

  scree_plot <- make_pca_scree_plot(
    scree_data = scree_data,
    selection_method = selection_method
  )

  correlation_output <- NULL

  if (isTRUE(correlations)) {
    correlation_output <- correlation_matrix |>
      as.data.frame() |>
      rownames_to_column("variable") |>
      as_tibble()
  }

  result <- list(
    call = match.call(),
    variables = variable_names,
    n_observations = n_observations,
    n_missing_rows = n_missing_rows,
    n_variables = n_variables,
    n_components = retained_components,
    selection_method = selection_method,
    rotation = "varimax",
    loading_cutoff = loading_cutoff,
    loadings = loading_table,
    loading_matrix = rotated_loadings,
    variance = variance_table,
    eigenvalues = scree_data,
    diagnostics = diagnostics,
    correlations = correlation_output,
    scree_plot = scree_plot,
    rotation_matrix = rotation_matrix,
    complete_rows = complete_rows
  )

  class(result) <- "tidycomm_pca"

  result
}

pca_diagnostics <- function(
  correlation_matrix,
  n_observations
) {
  n_variables <- ncol(correlation_matrix)

  inverse_correlation <- solve(correlation_matrix)

  partial_correlations <- -stats::cov2cor(
    inverse_correlation
  )

  diag(partial_correlations) <- 0

  correlations_without_diagonal <- correlation_matrix
  diag(correlations_without_diagonal) <- 0

  squared_correlations <- correlations_without_diagonal^2
  squared_partial_correlations <- partial_correlations^2

  kmo <- sum(squared_correlations) /
    (
      sum(squared_correlations) +
        sum(squared_partial_correlations)
    )

  msa_numerator <- rowSums(squared_correlations)

  msa <- msa_numerator /
    (
      msa_numerator +
        rowSums(squared_partial_correlations)
    )

  log_determinant <- as.numeric(
    determinant(
      correlation_matrix,
      logarithm = TRUE
    )$modulus
  )

  bartlett_chisq <- -(
    n_observations -
      1 -
      (2 * n_variables + 5) / 6
  ) * log_determinant

  bartlett_df <- n_variables *
    (n_variables - 1) /
    2

  bartlett_p <- stats::pchisq(
    bartlett_chisq,
    df = bartlett_df,
    lower.tail = FALSE
  )

  observations_per_variable <-
    n_observations / n_variables

  list(
    summary = tibble(
      measure = c(
        "KMO",
        "Bartlett chi-squared",
        "Bartlett df",
        "Bartlett p-value",
        "Observations",
        "Variables",
        "Observations per variable"
      ),
      value = c(
        kmo,
        bartlett_chisq,
        bartlett_df,
        bartlett_p,
        n_observations,
        n_variables,
        observations_per_variable
      )
    ),
    kmo = kmo,
    msa = tibble(
      variable = colnames(correlation_matrix),
      msa = unname(msa)
    ),
    bartlett = tibble(
      chi_squared = bartlett_chisq,
      df = bartlett_df,
      p_value = bartlett_p
    ),
    observations_per_variable = observations_per_variable
  )
}

pca_parallel_analysis <- function(
  n_observations,
  n_variables,
  observed_eigenvalues,
  repetitions = 500,
  quantile = 0.95,
  seed = NULL
) {
  if (
    !is.numeric(repetitions) ||
      length(repetitions) != 1L ||
      is.na(repetitions) ||
      repetitions %% 1 != 0 ||
      repetitions < 10L
  ) {
    stop(
      "`repetitions` must be a whole number of at least 10.",
      call. = FALSE
    )
  }

  if (
    !is.numeric(quantile) ||
      length(quantile) != 1L ||
      is.na(quantile) ||
      quantile <= 0 ||
      quantile >= 1
  ) {
    stop(
      "`quantile` must be a number between 0 and 1.",
      call. = FALSE
    )
  }

  if (!is.null(seed)) {
    set.seed(seed)
  }

  random_eigenvalues <- replicate(
    repetitions,
    {
      random_data <- matrix(
        stats::rnorm(
          n_observations * n_variables
        ),
        nrow = n_observations,
        ncol = n_variables
      )

      eigen(
        stats::cor(random_data),
        symmetric = TRUE,
        only.values = TRUE
      )$values
    }
  )

  reference_eigenvalues <- apply(
    random_eigenvalues,
    MARGIN = 1,
    FUN = stats::quantile,
    probs = quantile,
    names = FALSE
  )

  retain <- observed_eigenvalues >
    reference_eigenvalues

  n_components <- sum(retain)

  if (n_components == 0L) {
    n_components <- 1L

    warning(
      paste0(
        "Parallel analysis did not retain any component. ",
        "One component was retained."
      ),
      call. = FALSE
    )
  }

  list(
    n_components = n_components,
    reference_eigenvalues = reference_eigenvalues,
    observed_eigenvalues = observed_eigenvalues,
    random_eigenvalues = random_eigenvalues,
    repetitions = repetitions,
    quantile = quantile
  )
}

first_failed_component <- which(
  observed_eigenvalues <= reference_eigenvalues
)[1]

if (is.na(first_failed_component)) {
  n_components <- length(observed_eigenvalues)
} else {
  n_components <- first_failed_component - 1L
}

n_components <- max(1L, n_components)

make_pca_scree_plot <- function(
  scree_data,
  selection_method
) {
  plot <- ggplot(
    scree_data,
    aes(
      x = component,
      y = eigenvalue
    )
  ) +
    geom_hline(
      yintercept = 1,
      linetype = "dashed",
      linewidth = 0.5
    ) +
    geom_line(
      linewidth = 0.7
    ) +
    geom_point(
      aes(shape = retained),
      size = 2.5
    ) +
    scale_x_continuous(
      breaks = scree_data$component
    ) +
    scale_shape_manual(
      values = c(
        `TRUE` = 16,
        `FALSE` = 1
      ),
      labels = c(
        `TRUE` = "Retained",
        `FALSE` = "Not retained"
      )
    ) +
    labs(
      x = "Component",
      y = "Eigenvalue",
      shape = NULL,
      title = "Scree plot",
      subtitle = paste(
        "Number of components selected using",
        selection_method
      )
    ) +
    theme_minimal() +
    theme(
      panel.grid.minor = element_blank(),
      legend.position = "bottom"
    )

  if ("parallel_eigenvalue" %in% names(scree_data)) {
    plot <- plot +
      geom_line(
        aes(y = parallel_eigenvalue),
        linetype = "dotted",
        linewidth = 0.7
      ) +
      geom_point(
        aes(y = parallel_eigenvalue),
        shape = 4,
        size = 2.5
      ) +
      labs(
        caption = paste0(
          "The dotted line shows the random eigenvalue ",
          "threshold used in the parallel analysis."
        )
      )
  }

  plot
}

explore_factors <- function(
  .data,
  ...,
  n_factors = NULL,
  determine = c("kaiser", "parallel"),
  rotation = c("varimax", "none"),
  correlations = FALSE,
  scores = c("none", "regression", "Bartlett"),
  loading_cutoff = 0.30,
  parallel_repetitions = 500,
  parallel_quantile = 0.95,
  seed = NULL
) {
  determine <- match.arg(determine)
  rotation <- match.arg(rotation)
  scores <- match.arg(scores)

  selected_data <- .data |>
    select(...)

  validate_factor_data(selected_data)

  complete_rows <- stats::complete.cases(selected_data)

  analysis_data <- selected_data[
    complete_rows,
    ,
    drop = FALSE
  ]

  n_observations <- nrow(analysis_data)
  n_variables <- ncol(analysis_data)
  variable_names <- names(analysis_data)

  correlation_matrix <- stats::cor(
    analysis_data,
    use = "everything"
  )

  validate_correlation_matrix(correlation_matrix)

  eigenvalues <- eigen(
    correlation_matrix,
    symmetric = TRUE,
    only.values = TRUE
  )$values

  diagnostics <- factor_diagnostics(
    correlation_matrix = correlation_matrix,
    n_observations = n_observations
  )

  parallel_results <- NULL

  if (!is.null(n_factors)) {
    validate_n_factors(
      n_factors = n_factors,
      n_variables = n_variables
    )

    retained_factors <- as.integer(n_factors)
    selection_method <- "fixed"
  } else if (determine == "kaiser") {
    retained_factors <- sum(eigenvalues > 1)
    retained_factors <- max(1L, retained_factors)
    selection_method <- "kaiser"
  } else {
    parallel_results <- parallel_analysis(
      n_observations = n_observations,
      n_variables = n_variables,
      observed_eigenvalues = eigenvalues,
      repetitions = parallel_repetitions,
      quantile = parallel_quantile,
      seed = seed
    )

    retained_factors <- parallel_results$n_factors
    selection_method <- "parallel"
  }

  model <- stats::factanal(
    x = analysis_data,
    factors = retained_factors,
    rotation = rotation,
    scores = scores
  )

  loading_matrix <- unclass(model$loadings)

  processed_loadings <- process_factor_loadings(
    loading_matrix = loading_matrix,
    uniquenesses = model$uniquenesses,
    variable_names = variable_names
  )

  factor_variance <- calculate_factor_variance(
    loading_matrix = processed_loadings$loading_matrix
  )

  eigenvalue_table <- tibble(
    factor = seq_len(n_variables),
    eigenvalue = eigenvalues,
    retained = factor <= retained_factors
  )

  if (!is.null(parallel_results)) {
    eigenvalue_table <- eigenvalue_table |>
      mutate(
        parallel_eigenvalue =
          parallel_results$reference_eigenvalues
      )
  }

  result <- list(
    call = match.call(),
    method = "maximum_likelihood",
    rotation = rotation,
    selection_method = selection_method,
    n_observations = n_observations,
    n_missing_rows = sum(!complete_rows),
    n_variables = n_variables,
    n_factors = retained_factors,
    variables = variable_names,
    model = model,
    loadings = processed_loadings$table,
    loading_matrix = processed_loadings$loading_matrix,
    variance = factor_variance,
    uniquenesses = model$uniquenesses,
    diagnostics = diagnostics,
    eigenvalues = eigenvalue_table,
    correlations = if (correlations) {
      correlation_matrix |>
        as.data.frame() |>
        tibble::rownames_to_column("variable") |>
        as_tibble()
    } else {
      NULL
    },
    scores = if (scores == "none") {
      NULL
    } else {
      as_tibble(model$scores)
    },
    test = tibble(
      statistic = unname(model$STATISTIC),
      df = unname(model$dof),
      p_value = unname(model$PVAL)
    ),
    loading_cutoff = loading_cutoff,
    complete_rows = complete_rows
  )

  class(result) <- "tidycomm_efa"

  result
}