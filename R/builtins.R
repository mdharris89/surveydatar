#' Select top n response options
#'
#' @param var Variable name or expression
#' @param n Number of top options to select (default 1)
#' @return Expression for use in tab()
#' @export
#' @examples
#' tab(data, top_box(satisfaction, 2), gender)
top_box <- function(var, n = 1) {
  match.call()
}

#' Select bottom n response options
#'
#' @param var Variable name or expression
#' @param n Number of bottom options to select (default 1)
#' @return Expression for use in tab()
#' @export
bottom_box <- function(var, n = 1) {
  match.call()
}

#' Index values relative to a base
#'
#' @param var Variable name or expression
#' @param base Base for indexing (default is total)
#' @return Expression for use in tab()
#' @export
index_to <- function(var, base = NULL) {
  match.call()
}

#' Calculate change from baseline
#'
#' @param var Variable name or expression
#' @param condition Condition defining baseline
#' @return Expression for use in tab()
#' @export
change_from <- function(var, condition) {
  match.call()
}

#' Register built-in helpers and statistics
#' @param libname Library name (unused)
#' @param pkgname Package name (unused)
#' @keywords internal
.onLoad <- function(libname = NULL, pkgname = NULL) {
  # Only register if not already registered (avoid duplicates)
  if (!"count" %in% names(.tab_registry$stats)) {

    # STATISTICS --------------------------------------------------------

    # Column percentage
    stat_column_pct <- function(base_array, row_array, col_array, ...) {
      column_snapshot <- base_array * col_array
      final_array <- column_snapshot * row_array

      column_total <- sum(column_snapshot)
      if (column_total == 0) return(NA_real_)
      sum(final_array) / column_total * 100
    }
    create_statistic("column_pct", stat_column_pct,
                     summary_row = "NET",
                     summary_col = NULL,
                     format_fn = function(x) sprintf("%.1f%%", x),
                     requires_values = FALSE
    )

    # Row percentage
    stat_row_pct <- function(base_array, row_array, col_array, ...) {
      row_snapshot <- base_array * row_array
      final_array <- row_snapshot * col_array

      row_total <- sum(row_snapshot)
      if (row_total == 0) return(NA_real_)
      sum(final_array) / row_total * 100
    }
    create_statistic("row_pct", stat_row_pct,
                     summary_row = NULL,
                     summary_col = "NET",
                     format_fn = function(x) sprintf("%.1f%%", x),
                     requires_values = FALSE
    )

    # Count
    stat_count <- function(base_array, row_array, col_array, ...) {
      sum(base_array * row_array * col_array)
    }
    create_statistic("count", stat_count,
                     summary_row = "NET",
                     summary_col = NULL,
                     format_fn = function(x) as.character(x),
                     requires_values = FALSE
    )

    # Mean (requires values parameter)
    stat_mean <- function(base_array, row_array, col_array, values = NULL, ...) {
      final_array <- base_array * row_array * col_array

      if (is.null(values)) {
        stop("Mean statistic requires 'values' parameter")
      }

      # Only count non-NA values for both numerator and denominator
      valid_mask <- !is.na(values)
      final_array_valid <- final_array * valid_mask

      numerator <- sum(final_array_valid * values, na.rm = TRUE)
      denominator <- sum(final_array_valid)
      if (denominator == 0) return(NA_real_)
      numerator / denominator
    }
    create_statistic("mean", stat_mean,
                     summary_row = "Avg",
                     summary_col = NULL,
                     format_fn = function(x) sprintf("%.2f", x),
                     requires_values = TRUE,
                     base_label = "Base (n)"
    )

    # Median
    stat_median <- function(base_array, row_array, col_array, values = NULL, ...) {
      if (is.null(values)) {
        stop("Median statistic requires 'values' parameter")
      }

      final_array <- base_array * row_array * col_array
      valid_mask <- !is.na(values)
      final_array_valid <- final_array * valid_mask

      # Get values for this cell
      cell_values <- values[final_array_valid > 0]

      if (length(cell_values) == 0) return(NA_real_)
      median(cell_values, na.rm = TRUE)
    }
    create_statistic("median", stat_median,
                     summary_row = NULL,
                     summary_col = NULL,
                     format_fn = function(x) sprintf("%.1f", x),
                     requires_values = TRUE,
                     base_label = "Base (n)"
    )

    # Standard Deviation
    stat_sd <- function(base_array, row_array, col_array, values = NULL, ...) {
      if (is.null(values)) {
        stop("Standard deviation statistic requires 'values' parameter")
      }

      final_array <- base_array * row_array * col_array
      valid_mask <- !is.na(values)
      final_array_valid <- final_array * valid_mask

      # Get values for this cell
      cell_values <- values[final_array_valid > 0]

      if (length(cell_values) < 2) return(NA_real_)
      sd(cell_values, na.rm = TRUE)
    }
    create_statistic("sd", stat_sd,
                     summary_row = NULL,
                     summary_col = NULL,
                     format_fn = function(x) sprintf("%.2f", x),
                     requires_values = TRUE,
                     base_label = "Base (n)"
    )

    # Coefficient of Variation (CV)
    stat_cv <- function(base_array, row_array, col_array, values = NULL, ...) {
      if (is.null(values)) {
        stop("Coefficient of variation statistic requires 'values' parameter")
      }

      final_array <- base_array * row_array * col_array
      valid_mask <- !is.na(values)
      final_array_valid <- final_array * valid_mask

      # Get values for this cell
      cell_values <- values[final_array_valid > 0]

      if (length(cell_values) < 2) return(NA_real_)

      mean_val <- mean(cell_values, na.rm = TRUE)
      sd_val <- sd(cell_values, na.rm = TRUE)

      if (mean_val == 0) return(NA_real_)
      (sd_val / abs(mean_val)) * 100
    }
    create_statistic("cv", stat_cv,
                     summary_row = NULL,
                     summary_col = NULL,
                     format_fn = function(x) sprintf("%.1f%%", x),
                     requires_values = TRUE,
                     base_label = "Base (n)"
    )

    # Index (relative to total column)
    stat_index <- function(base_array, row_array, col_array, ...) {
      # Calculate cell percentage
      column_snapshot <- base_array * col_array
      final_array <- column_snapshot * row_array

      column_total <- sum(column_snapshot)
      if (column_total == 0) return(NA_real_)
      cell_pct <- sum(final_array) / column_total * 100

      # Calculate total percentage (all columns)
      total_snapshot <- base_array  # All data
      total_for_row <- total_snapshot * row_array

      total_n <- sum(total_snapshot)
      if (total_n == 0) return(NA_real_)
      total_pct <- sum(total_for_row) / total_n * 100

      # Index = (cell % / total %) * 100
      if (total_pct == 0) return(NA_real_)
      (cell_pct / total_pct) * 100
    }
    create_statistic("index", stat_index,
                     summary_row = "NET",
                     summary_col = NULL,
                     format_fn = function(x) sprintf("%.0f", x),
                     requires_values = FALSE,
                     base_label = "Base (n)"
    )

    # Percentile
    stat_percentile <- function(base_array, row_array, col_array, values = NULL, percentile = 50, ...) {
      if (is.null(values)) {
        stop("Percentile statistic requires 'values' parameter")
      }

      final_array <- base_array * row_array * col_array
      valid_mask <- !is.na(values)
      final_array_valid <- final_array * valid_mask

      # Get values for this cell
      cell_values <- values[final_array_valid > 0]

      if (length(cell_values) == 0) return(NA_real_)
      quantile(cell_values, probs = percentile/100, na.rm = TRUE)
    }
    # Create specific percentile statistics
    create_statistic("p25", function(...) stat_percentile(..., percentile = 25),
                     summary_row = NULL,
                     summary_col = NULL,
                     format_fn = function(x) sprintf("%.1f", x),
                     requires_values = TRUE,
                     base_label = "Base (n)"
    )
    create_statistic("p75", function(...) stat_percentile(..., percentile = 75),
                     summary_row = NULL,
                     summary_col = NULL,
                     format_fn = function(x) sprintf("%.1f", x),
                     requires_values = TRUE,
                     base_label = "Base (n)"
    )

    # Correlation
    stat_correlation <- function(base_array, row_array, col_array, ...) {
      # Filter both arrays by base_array
      valid_mask <- base_array > 0
      x <- row_array[valid_mask]
      y <- col_array[valid_mask]

      # Calculate Pearson correlation
      if (length(x) < 2) return(NA_real_)
      cor(x, y, use = "complete.obs")
    }
    create_statistic("correlation", stat_correlation,
                     summary_row = NULL,
                     summary_col = NULL,
                     format_fn = function(x) sprintf("%.3f", x),
                     requires_values = FALSE,
                     base_label = "Base (n)"
    )

    # HELPERS -----------------------------------------------------------

    # Top box helper
    help_top_box <- function(formula_spec, data, ...) {
      var_name <- as.character(formula_spec$components[[1]])
      n <- formula_spec$components[[2]]

      if (!var_name %in% names(data)) {
        stop("Variable '", var_name, "' not found in data")
      }

      var_data <- data[[var_name]]

      # Handle labelled variables
      if (inherits(var_data, "haven_labelled")) {
        labels <- attr(var_data, "labels")
        top_values <- sort(labels, decreasing = TRUE)[seq_len(n)]
      } else {
        unique_vals <- sort(unique(na.omit(var_data)), decreasing = TRUE)
        top_values <- unique_vals[seq_len(n)]
      }

      as.numeric(var_data %in% top_values)
    }
    create_helper("top_box", help_top_box)

    # Bottom box helper
    help_bottom_box <- function(formula_spec, data, ...) {
      var_name <- as.character(formula_spec$components[[1]])
      n <- formula_spec$components[[2]]

      if (!var_name %in% names(data)) {
        stop("Variable '", var_name, "' not found in data")
      }

      var_data <- data[[var_name]]

      # Handle labelled variables
      if (inherits(var_data, "haven_labelled")) {
        labels <- attr(var_data, "labels")
        bottom_values <- sort(labels)[seq_len(n)]
      } else {
        unique_vals <- sort(unique(na.omit(var_data)))
        bottom_values <- unique_vals[seq_len(n)]
      }

      as.numeric(var_data %in% bottom_values)
    }
    create_helper("bottom_box", help_bottom_box)

    # Value range helper
    help_value_range <- function(formula_spec, data, ...) {
      var_name <- as.character(formula_spec$components[[1]])
      min_val <- formula_spec$components[[2]]
      max_val <- formula_spec$components[[3]]
      inclusive <- if (length(formula_spec$components) >= 4) formula_spec$components[[4]]

      if (!var_name %in% names(data)) {
        stop("Variable '", var_name, "' not found in data")
      }

      var_data <- data[[var_name]]

      if (!is.numeric(var_data)) {
        stop("value_range requires a numeric variable")
      }

      if (inclusive) {
        as.numeric(var_data >= min_val & var_data <= max_val)
      } else {
        as.numeric(var_data > min_val & var_data < max_val)
      }
    }
    create_helper("value_range", help_value_range)

    # Pattern match helper (for text variables)
    help_pattern <- function(formula_spec, data, ...) {
      var_name <- as.character(formula_spec$components[[1]])
      pattern <- formula_spec$components[[2]]
      ignore_case <- if (length(formula_spec$components) >= 3) formula_spec$components[[3]] else FALSE

      if (!var_name %in% names(data)) {
        stop("Variable '", var_name, "' not found in data")
      }

      var_data <- data[[var_name]]

      if (!is.character(var_data) && !is.factor(var_data)) {
        stop("pattern requires a character or factor variable")
      }

      # Convert to character if factor
      if (is.factor(var_data)) {
        var_data <- as.character(var_data)
      }

      as.numeric(grepl(pattern, var_data, ignore.case = ignore_case))
    }
    create_helper("pattern", help_pattern)

    # Percentile selection helper
    help_percentile <- function(formula_spec, data, ...) {
      var_name <- as.character(formula_spec$components[[1]])
      position <- formula_spec$components[[2]]
      percentile <- formula_spec$components[[3]]

      if (!var_name %in% names(data)) {
        stop("Variable '", var_name, "' not found in data")
      }

      var_data <- data[[var_name]]

      if (!is.numeric(var_data)) {
        stop("percentile requires a numeric variable")
      }

      # Calculate percentile threshold
      threshold <- quantile(var_data, probs = percentile/100, na.rm = TRUE)

      if (position == "above") {
        as.numeric(var_data > threshold)
      } else if (position == "below") {
        as.numeric(var_data < threshold)
      } else {
        stop("position must be 'above' or 'below'")
      }
    }
    create_helper("percentile", help_percentile)

    # Not helper (negation)
    help_not <- function(formula_spec, data, ...) {
      expr <- formula_spec$components[[1]]

      # Evaluate the expression
      result <- rlang::eval_tidy(expr, data)

      if (!is.logical(result) && !is.numeric(result)) {
        stop("not() requires an expression that evaluates to logical or numeric")
      }

      # Negate the result
      as.numeric(!as.logical(result))
    }
    create_helper("not", help_not)
  }
}

#' Ensure built-in helpers and statistics are registered
#'
#' This function registers all built-in helpers and statistics. It's called
#' automatically when the package loads, but can also be called explicitly
#' (e.g., during testing) to ensure built-ins are available.
#'
#' @export
ensure_builtins_registered <- function() {
  # Call the same registration logic as .onLoad
  .onLoad(NULL, NULL)
}
