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

      # Validate array types
      if (get_variable_array_type(row_array) != "categorical" || get_variable_array_type(col_array) != "categorical") {
        warning("statistic did not receive expected categorical row and column arrays (0/1 values)")
      }

      column_snapshot <- base_array * col_array
      final_array <- column_snapshot * row_array

      column_total <- sum(column_snapshot)
      if (column_total == 0) return(NA_real_)
      sum(final_array) / column_total * 100
    }
    create_statistic("column_pct", stat_column_pct,
                     summary_row = "NET",
                     summary_col = "NET",
                     format_fn = function(x) sprintf("%.1f%%", x),
                     requires_values = FALSE
    )

    # Row percentage
    stat_row_pct <- function(base_array, row_array, col_array, ...) {

      # Validate array types
      if (get_variable_array_type(row_array) != "categorical" || get_variable_array_type(col_array) != "categorical") {
        warning("statistic did not receive expected categorical row and column arrays (0/1 values)")
      }

      row_snapshot <- base_array * row_array
      final_array <- row_snapshot * col_array

      row_total <- sum(row_snapshot)
      if (row_total == 0) return(NA_real_)
      sum(final_array) / row_total * 100
    }
    create_statistic("row_pct", stat_row_pct,
                     summary_row = "NET",
                     summary_col = "NET",
                     format_fn = function(x) sprintf("%.1f%%", x),
                     requires_values = FALSE,
                     base_label = "Base (n)",
                     base_orientation = "row"
    )

    # Count
    stat_count <- function(base_array, row_array, col_array, ...) {

      # Validate array types
      if (get_variable_array_type(row_array) != "categorical" || get_variable_array_type(col_array) != "categorical") {
        warning("statistic did not receive expected categorical row and column arrays (0/1 values)")
      }

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

      # Validate array types
      if (get_variable_array_type(row_array) != "categorical" || get_variable_array_type(col_array) != "categorical") {
        warning("statistic did not receive expected categorical row and column arrays (0/1 values)")
      }

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

      # Validate array types
      if (get_variable_array_type(row_array) != "categorical" || get_variable_array_type(col_array) != "categorical") {
        warning("statistic did not receive expected categorical row and column arrays (0/1 values)")
      }

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

      # Validate array types
      if (get_variable_array_type(row_array) != "categorical" || get_variable_array_type(col_array) != "categorical") {
        warning("statistic did not receive expected categorical row and column arrays (0/1 values)")
      }

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

      # Validate array types
      if (get_variable_array_type(row_array) != "categorical" || get_variable_array_type(col_array) != "categorical") {
        warning("statistic did not receive expected categorical row and column arrays (0/1 values)")
      }

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

      # Validate array types
      if (get_variable_array_type(row_array) != "categorical" || get_variable_array_type(col_array) != "categorical") {
        warning("statistic did not receive expected categorical row and column arrays (0/1 values)")
      }

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

      # Validate array types
      if (get_variable_array_type(row_array) != "categorical" || get_variable_array_type(col_array) != "categorical") {
        warning("statistic did not receive expected categorical row and column arrays (0/1 values)")
      }

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

      # Validate array types
      if (get_variable_array_type(row_array) != "numeric" || get_variable_array_type(col_array) != "numeric") {
        warning("statistic did not receive expected numeric row and column arrays")
      }

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


    # In .onLoad function, after registering statistics:

    # SIGNIFICANCE TESTS ------------------------------------------------

    # Z-test for proportions
    create_significance_test(
      id = "z_test_proportions",
      name = "Z-test for proportions",
      description = "Two-sample z-test for comparing proportions",
      processor = function(base_array, row_array, col_array_1, col_array_2, ...) {
        # Get counts and totals
        n1 <- sum(base_array * row_array * col_array_1)
        n2 <- sum(base_array * row_array * col_array_2)
        N1 <- sum(base_array * col_array_1)
        N2 <- sum(base_array * col_array_2)

        if (N1 == 0 || N2 == 0) {
          return(list(p_value = NA_real_, statistic = NA_real_))
        }

        # Proportions
        p1 <- n1 / N1
        p2 <- n2 / N2

        # Pooled proportion
        p_pool <- (n1 + n2) / (N1 + N2)

        # Standard error
        se <- sqrt(p_pool * (1 - p_pool) * (1/N1 + 1/N2))

        if (se == 0) {
          return(list(p_value = NA_real_, statistic = NA_real_))
        }

        # Z statistic
        z <- (p2 - p1) / se
        # Two-tailed p-value
        p_value <- 2 * pnorm(-abs(z))

        return(list(
          p_value = p_value,
          statistic = z,
          method = "Two-sample z-test for proportions"
        ))
      }
    )

    # Weighted Z-test for proportions
    create_significance_test(
      id = "z_test_weighted",
      name = "Weighted Z-test for proportions",
      description = "Two-sample z-test for weighted proportions using effective sample sizes",
      processor = function(base_array, row_array, col_array_1, col_array_2, ...) {
        # Weighted counts (numerators)
        n1 <- sum(base_array * row_array * col_array_1)
        n2 <- sum(base_array * row_array * col_array_2)

        # Weighted totals (denominators)
        N1 <- sum(base_array * col_array_1)
        N2 <- sum(base_array * col_array_2)

        if (N1 == 0 || N2 == 0) {
          return(list(p_value = NA_real_, statistic = NA_real_))
        }

        # Weighted proportions
        p1 <- n1 / N1
        p2 <- n2 / N2

        # Get weights for each column (only for observations in that column)
        idx_1 <- col_array_1 > 0
        idx_2 <- col_array_2 > 0

        w1 <- base_array[idx_1]
        w2 <- base_array[idx_2]

        # Remove any zero or negative weights
        w1 <- w1[w1 > 0]
        w2 <- w2[w2 > 0]

        if (length(w1) == 0 || length(w2) == 0) {
          return(list(p_value = NA_real_, statistic = NA_real_))
        }

        # Calculate effective sample sizes: n_eff = (sum(w))^2 / sum(w^2)
        n1_eff <- (sum(w1))^2 / sum(w1^2)
        n2_eff <- (sum(w2))^2 / sum(w2^2)

        # Check minimum effective sample size
        if (n1_eff < 5 || n2_eff < 5) {
          warning(paste0("Weighted z-test: Effective sample sizes too small (n1_eff=",
                         round(n1_eff, 1), ", n2_eff=", round(n2_eff, 1), ")"))
          return(list(p_value = NA_real_, statistic = NA_real_))
        }

        # Pooled proportion (weighted)
        p_pool <- (n1 + n2) / (N1 + N2)

        # Standard error using effective sample sizes
        se <- sqrt(p_pool * (1 - p_pool) * (1/n1_eff + 1/n2_eff))

        if (se == 0 || !is.finite(se)) {
          warning("Weighted z-test: Invalid standard error")
          return(list(p_value = NA_real_, statistic = NA_real_))
        }

        # Z statistic
        z <- (p2 - p1) / se

        # Two-tailed p-value
        p_value <- 2 * pnorm(-abs(z))

        return(list(
          p_value = p_value,
          statistic = z,
          method = "Weighted two-sample z-test for proportions",
          effective_n1 = round(n1_eff, 1),
          effective_n2 = round(n2_eff, 1)
        ))
      }
    )

    # T-test for means
    create_significance_test(
      id = "t_test",
      name = "T-test",
      description = "Two-sample t-test for comparing means",
      processor = function(base_array, row_array, col_array_1, col_array_2, values, ...) {
        # Get values for both groups
        group1_mask <- base_array * row_array * col_array_1 > 0
        group2_mask <- base_array * row_array * col_array_2 > 0

        values1 <- values[group1_mask]
        values2 <- values[group2_mask]

        # Remove NAs
        values1 <- values1[!is.na(values1)]
        values2 <- values2[!is.na(values2)]

        if (length(values1) < 2 || length(values2) < 2) {
          return(list(p_value = NA_real_, statistic = NA_real_))
        }

        # Perform t-test
        test_result <- t.test(values2, values1)

        return(list(
          p_value = test_result$p.value,
          statistic = test_result$statistic,
          method = test_result$method
        ))
      }
    )

    # Weighted T-test for means
    create_significance_test(
      id = "t_test_weighted",
      name = "Weighted T-test",
      description = "Two-sample t-test for weighted means using effective sample sizes",
      processor = function(base_array, row_array, col_array_1, col_array_2, values, ...) {
        if (is.null(values)) {
          stop("Weighted t-test requires 'values' parameter")
        }

        # Get weighted values for both groups
        group1_mask <- base_array * row_array * col_array_1 > 0
        group2_mask <- base_array * row_array * col_array_2 > 0

        values1 <- values[group1_mask]
        values2 <- values[group2_mask]
        weights1 <- base_array[group1_mask]
        weights2 <- base_array[group2_mask]

        # Remove NAs
        valid1 <- !is.na(values1) & !is.na(weights1) & weights1 > 0
        valid2 <- !is.na(values2) & !is.na(weights2) & weights2 > 0

        values1 <- values1[valid1]
        values2 <- values2[valid2]
        weights1 <- weights1[valid1]
        weights2 <- weights2[valid2]

        if (length(values1) < 2 || length(values2) < 2) {
          return(list(p_value = NA_real_, statistic = NA_real_))
        }

        # Weighted means
        mean1 <- sum(weights1 * values1) / sum(weights1)
        mean2 <- sum(weights2 * values2) / sum(weights2)

        # Weighted variances (using population formula, then correcting)
        var1_pop <- sum(weights1 * (values1 - mean1)^2) / sum(weights1)
        var2_pop <- sum(weights2 * (values2 - mean2)^2) / sum(weights2)

        # Effective sample sizes
        n1_eff <- (sum(weights1))^2 / sum(weights1^2)
        n2_eff <- (sum(weights2))^2 / sum(weights2^2)

        # Check minimum effective sample size
        if (n1_eff < 5 || n2_eff < 5) {
          warning(paste0("Weighted t-test: Effective sample sizes too small (n1_eff=",
                         round(n1_eff, 1), ", n2_eff=", round(n2_eff, 1), ")"))
          return(list(p_value = NA_real_, statistic = NA_real_))
        }

        # Convert to sample variances using effective sample size
        var1 <- var1_pop * n1_eff / (n1_eff - 1)
        var2 <- var2_pop * n2_eff / (n2_eff - 1)

        # Standard error of difference in means
        se_diff <- sqrt(var1/n1_eff + var2/n2_eff)

        if (se_diff == 0 || !is.finite(se_diff)) {
          warning("Weighted t-test: Invalid standard error")
          return(list(p_value = NA_real_, statistic = NA_real_))
        }

        # T statistic
        t_stat <- (mean2 - mean1) / se_diff

        # Welch-Satterthwaite degrees of freedom approximation
        s1_sq_over_n1 <- var1 / n1_eff
        s2_sq_over_n2 <- var2 / n2_eff

        df <- (s1_sq_over_n1 + s2_sq_over_n2)^2 /
          (s1_sq_over_n1^2/(n1_eff-1) + s2_sq_over_n2^2/(n2_eff-1))

        # Ensure reasonable degrees of freedom
        df <- max(1, min(df, n1_eff + n2_eff - 2))

        # Two-tailed p-value
        p_value <- 2 * pt(-abs(t_stat), df)

        return(list(
          p_value = p_value,
          statistic = t_stat,
          parameter = df,
          method = "Weighted two-sample t-test",
          effective_n1 = round(n1_eff, 1),
          effective_n2 = round(n2_eff, 1),
          mean1 = round(mean1, 3),
          mean2 = round(mean2, 3)
        ))
      }
    )

    # Chi-square test
    create_significance_test(
      id = "chi_square",
      name = "Chi-square test",
      description = "Chi-square test of independence",
      is_omnibus = TRUE,  # Add this line
      processor = function(base_array, row_arrays, col_arrays, ...) {
        # Build contingency table
        n_rows <- length(row_arrays)
        n_cols <- length(col_arrays)

        cont_table <- matrix(0, nrow = n_rows, ncol = n_cols)
        for (i in 1:n_rows) {
          for (j in 1:n_cols) {
            cont_table[i, j] <- sum(base_array * row_arrays[[i]] * col_arrays[[j]])
          }
        }

        # Perform chi-square test
        if (min(dim(cont_table)) < 2) {
          return(list(p_value = NA_real_, statistic = NA_real_))
        }

        test_result <- chisq.test(cont_table)

        return(list(
          p_value = test_result$p.value,
          statistic = test_result$statistic,
          method = test_result$method
        ))
      }
    )

    # Mann-Whitney U test
    create_significance_test(
      id = "mann_whitney",
      name = "Mann-Whitney U test",
      description = "Non-parametric test for comparing distributions",
      processor = function(base_array, row_array, col_array_1, col_array_2, values, ...) {
        # Get values for both groups
        group1_mask <- base_array * row_array * col_array_1 > 0
        group2_mask <- base_array * row_array * col_array_2 > 0

        values1 <- values[group1_mask]
        values2 <- values[group2_mask]

        # Remove NAs
        values1 <- values1[!is.na(values1)]
        values2 <- values2[!is.na(values2)]

        if (length(values1) < 1 || length(values2) < 1) {
          return(list(p_value = NA_real_, statistic = NA_real_))
        }

        # Perform Mann-Whitney test
        test_result <- wilcox.test(values2, values1)

        return(list(
          p_value = test_result$p.value,
          statistic = test_result$statistic,
          method = test_result$method
        ))
      }
    )
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
