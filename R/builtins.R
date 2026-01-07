# Built-in Tab Helpers and Statistics
#
# This file contains the built-in helper functions and statistics for tab().

#' Set array metadata for semantic matching
#'
#' Attaches metadata to arrays for DSL-based semantic matching and display.
#' 
#' Helpers should explicitly provide the label parameter, e.g. by
#' calling derive_label_from_dsl(dsl_expr, dpdict) before calling this function.
#' This ensures high-quality labels that use variable and value labels from dpdict.
#' 
#' For custom logic that cannot be expressed as DSL, provide dsl = NULL along with
#' variables and tags, and construct the label manually.
#'
#' @param arr Numeric array
#' @param dsl DSL expression (R call/symbol) or NULL for custom logic
#' @param variables Variable name(s) involved (list or character vector)
#' @param tags Arbitrary metadata as named list (for custom matching). Can include:
#'   value, value_label, var_label, suffix
#' @param label Display label. Should be explicitly provided. If NULL, will attempt
#'   low-quality auto-derivation without dpdict (not recommended).
#' @return Array with meta attribute attached
#' @keywords internal
set_array_meta <- function(arr, dsl = NULL, variables = NULL, tags = NULL, label = NULL) {
  # Fallback: auto-derive label from DSL if not provided (low quality - no dpdict)
  if (is.null(label) && !is.null(dsl)) {
    # This produces inferior labels without variable/value labels from dpdict.
    # Callers should derive labels themselves: derive_label_from_dsl(dsl_expr, dpdict)
    label <- tryCatch(
      derive_label_from_dsl(dsl, NULL),
      error = function(e) ""
    )
  }
  
  attr(arr, "meta") <- list(
    dsl = dsl,
    variables = variables,
    tags = tags,
    label = label
  )
  arr
}

#' Resolve helper target to variable names
#'
#' Normalizes different input types (single variable, stem pattern, group name,
#' or explicit vector) into a validated character vector of variable names.
#' Used internally by helpers like top_box(), bottom_box(), etc.
#'
#' @param name_or_names Single variable name, stem (e.g., "q1"), group name,
#'   or character vector of explicit variable names
#' @param data Data frame containing variables
#' @param dpdict Optional data dictionary for resolving question groups
#' @param error_context String to contextualize error messages (e.g., "top_box")
#' @return Character vector of resolved variable names from data
#' @keywords internal
#' @examples
#' # Resolves "q1" stem to c("q1_1", "q1_2", "q1_3")
#' # Resolves "q1_a" to all vars in that question group
resolve_helper_target <- function(name_or_names, data, dpdict = NULL, error_context = "helper target") {
  # Normalise to character vector
  to_chr <- function(x) {
    if (is.symbol(x)) return(as.character(x))
    if (is.character(x)) return(x)
    # Fallback for numbers or other atomic inputs erroneously passed
    as.character(x)
  }

  chr <- to_chr(name_or_names)

  # If a single token, try resolve_to_variables which handles vars, stems, groups
  if (length(chr) == 1) {
    # Exact variable present
    if (chr %in% names(data)) return(chr)

    # Use resolver (handles stems and dpdict groups); will error with good message on failure
    return(resolve_to_variables(chr, data, dpdict, allow_patterns = TRUE, error_context = error_context))
  }

  # Otherwise treat as explicit vector of variable names
  missing_vars <- chr[!chr %in% names(data)]
  if (length(missing_vars) > 0) {
    stop("Could not find variable(s): ", paste(missing_vars, collapse = ", "))
  }
  unique(chr)
}

#' Select top n response options
#'
#' @param var Variable name or expression
#' @param n Number of top options to select (default 1)
#' @return Expression for use in tab()
#' @export
#' @examples
#' top_box(satisfaction, 2)
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

#' Check if any variable in a group is positive
#'
#' Creates a binary indicator showing which respondents have any positive value
#' across the specified variables.
#'
#' @param vars Vector of variable names or single question group name
#' @return Expression for use in tab() rows/cols/filter
#' @export
#' @examples
#' any_positive(c("q1_1", "q1_2", "q1_3"))
#' any_positive("q1")  # if q1 is a question group
any_positive <- function(vars) {
  match.call()
}

#' Apply an explicit universe (eligible base) to a row/column specification
#'
#' This helper lets you define statistics like “consideration among those aware”
#' without encoding eligibility via NA patterns manually.
#'
#' @param x Row/column specification (variable, expression, or helper)
#' @param universe Eligibility definition (logical/numeric expression)
#' @return Expression for use in tab() rows/cols
#' @export
#' @examples
#' \dontrun{
#' tab(data, among(consider_a == 1, aware_a == 1), region)
#' }
among <- function(x, universe) {
  match.call()
}

#' Create a total/base row
#'
#' Returns an indicator that includes all respondents (equivalent to a constant of 1).
#' Useful for creating base rows or total calculations.
#'
#' @return Expression for use in tab() rows/cols/filter
#' @export
#' @examples
#' total()
total <- function() {
  match.call()
}

#' Match responses across multiple variables
#'
#' Creates binary arrays by matching patterns against variable labels or value labels
#' within a group of variables. This is a multi-column helper that produces
#' one array for each pattern match.
#'
#' @param patterns Character vector of patterns to match, or NULL if using get_* parameters
#' @param group_name Question group name to search within
#' @param mode Matching mode: "auto", "variable", or "value"
#' @param pattern_type Type of pattern matching: "regex" or "fixed"
#' @param label_fn Optional function to generate labels
#' @param values Named list mapping labels to value codes (alternative to patterns)
#' @param drop_empty Whether to drop empty results
#' @param get_variable_labels Extract variable labels automatically from this group
#' @param get_value_labels Extract value labels automatically from this group
#' @return Expression for use in tab() rows/cols
#' @export
#' @examples
#' # Match by patterns
#' response_match(c("Daily", "Weekly"), "usage_group")
#'
#' # Extract variable labels automatically
#' response_match(get_variable_labels = "q1")
#'
#' # Extract value labels automatically
#' response_match(get_value_labels = "q1")
response_match <- function(patterns = NULL, group_name = NULL, mode = "auto",
                           pattern_type = "regex", label_fn = NULL, values = NULL,
                           drop_empty = TRUE, get_variable_labels = NULL,
                           get_value_labels = NULL) {
  match.call()
}

#' Filter by numeric value range
#'
#' Creates a binary indicator for respondents whose values fall within
#' the specified numeric range.
#'
#' @param var Variable name
#' @param min_val Minimum value
#' @param max_val Maximum value
#' @param inclusive Whether range endpoints are inclusive (default TRUE)
#' @return Expression for use in tab() rows/cols/filter
#' @export
#' @examples
#' value_range(age, 25, 45)
#' value_range(income, 50000, 100000, inclusive = FALSE)
value_range <- function(var, min_val, max_val, inclusive = TRUE) {
  match.call()
}

#' Filter by text pattern matching
#'
#' Creates a binary indicator for respondents whose text/character values
#' match the specified pattern.
#'
#' @param var Variable name (character or factor)
#' @param pattern Regular expression or fixed string pattern
#' @param ignore_case Whether to ignore case when matching (default FALSE)
#' @return Expression for use in tab() rows/cols/filter
#' @export
#' @examples
#' pattern(city, "New.*")
#' pattern(region, "North|South")
pattern <- function(var, pattern, ignore_case = FALSE) {
  match.call()
}

#' Filter by percentile position
#'
#' Creates a binary indicator for respondents above or below a specified
#' percentile threshold.
#'
#' @param var Variable name (numeric)
#' @param position Either "above" or "below" the percentile
#' @param percentile Percentile threshold (0-100)
#' @return Expression for use in tab() rows/cols/filter
#' @export
#' @examples
#' percentile(income, "above", 75)  # Top quartile
#' percentile(age, "below", 25)     # Bottom quartile
percentile <- function(var, position, percentile) {
  match.call()
}

#' Find all variables matching a pattern
#'
#' Finds all variables in a question group whose labels match a pattern
#' and returns each as a separate row. Unlike response_match which aggregates
#' matches, this returns individual variables that match the pattern.
#'
#' @param pattern Single character string pattern to match against variable labels
#' @param group_name Question group name to search within
#' @param pattern_type Type of pattern matching: "fixed" (default), "regex", or "glob"
#' @param label_mode Label display mode (default "full")
#' @param drop_empty Whether to drop empty results if no matches found (default TRUE)
#' @return Expression for use in tab() rows/cols (multi-column helper)
#' @export
#' @examples
#' all_matching("Microsoft", "A1")
#' all_matching("Google|Microsoft", "A1", pattern_type = "regex")
#' all_matching("*Canva*", "A1", pattern_type = "glob")
all_matching <- function(pattern, group_name, pattern_type = "fixed",
                         label_mode = "full", drop_empty = TRUE) {
  match.call()
}

#' Create banner columns
#'
#' Creates multiple columns by crossing an outer variable with an inner
#' specification (variable, helper, or expression).
#'
#' @param outer_var Name of the outer variable
#' @param inner_spec Inner specification (variable name, helper, or expression)
#' @param subtotals Whether to include subtotal columns for each outer category
#' @param sep Separator between outer and inner labels (default ": ")
#' @return Expression for use in tab() cols
#' @export
#' @examples
#' banner(region, gender)
#' banner(region, top_box(satisfaction, 2))
#' banner(region, satisfaction, subtotals = TRUE)
banner <- function(outer_var, inner_spec, subtotals = FALSE, sep = ": ") {
  match.call()
}

# Banner spec generator - returns column specifications
banner_spec_generator <- function(helper_spec, data, dpdict = NULL, all_helpers = NULL) {
  args <- helper_spec$components

  if (length(args) < 2) {
    stop("Banner requires at least two arguments: outer variable and inner specification")
  }

  # Extract parameters
  outer_var <- args[[1]]
  inner_spec <- args[[2]]
  subtotals <- if ("subtotals" %in% names(args)) args$subtotals else FALSE
  sep <- if ("sep" %in% names(args)) args$sep else ": "

  if (!is.character(outer_var) || length(outer_var) != 1) {
    stop("First argument to banner must be a variable name")
  }

  if (!outer_var %in% names(data)) {
    stop("Outer variable '", outer_var, "' not found in data")
  }

  # Convert symbols to strings for simple variable references
  if (rlang::is_symbol(inner_spec)) {
    inner_spec <- as.character(inner_spec)
  }

  # Get unique values of outer variable
  outer_values <- unique(data[[outer_var]])
  outer_values <- outer_values[!is.na(outer_values)]
  outer_values <- sort(outer_values)

  if (length(outer_values) == 0) {
    stop("No valid values found for outer variable '", outer_var, "'")
  }

  # Get labels for outer variable
  outer_labels <- get_variable_labels(outer_var, outer_values, data, dpdict)

  # Generate column specifications
  col_specs <- list()

  # Handle inner_spec based on its type
  if (is.character(inner_spec) && length(inner_spec) == 1) {
    # Simple variable - expand to all its values
    inner_var <- inner_spec

    if (!inner_var %in% names(data)) {
      stop("Inner variable '", inner_var, "' not found in data")
    }

    inner_values <- unique(data[[inner_var]])
    inner_values <- inner_values[!is.na(inner_values)]
    inner_values <- sort(inner_values)
    inner_labels <- get_variable_labels(inner_var, inner_values, data, dpdict)

    # Create spec for each outer x inner combination
    for (i in seq_along(outer_values)) {
      outer_val <- outer_values[i]
      outer_label <- outer_labels[i]

      for (j in seq_along(inner_values)) {
        inner_val <- inner_values[j]
        inner_label <- inner_labels[j]

        col_label <- paste0(outer_label, sep, inner_label)

        # Create intersection specification
        col_specs[[col_label]] <- list(
          type = "banner_intersection",
          outer_var = outer_var,
          outer_val = outer_val,
          inner_var = inner_var,
          inner_val = inner_val,
          label = col_label
        )
      }
    }

    # Optionally add subtotals
    if (subtotals) {
      for (i in seq_along(outer_values)) {
        subtotal_label <- paste0(outer_labels[i], sep, "Total")
        col_specs[[subtotal_label]] <- list(
          type = "banner_subtotal",
          outer_var = outer_var,
          outer_val = outer_values[i],
          inner_var = inner_var,
          label = subtotal_label
        )
      }
    }

  } else if (rlang::is_call(inner_spec) || rlang::is_symbol(inner_spec)) {
    # Nested helper (e.g., top_box(satisfaction, 2))

    for (i in seq_along(outer_values)) {
      outer_val <- outer_values[i]
      outer_label <- outer_labels[i]

      # We don't evaluate the inner spec yet - just capture the expression
      # Create a label for this combination
      inner_label_text <- deparse(inner_spec)
      col_label <- paste0(outer_label, sep, inner_label_text)

      col_specs[[col_label]] <- list(
        type = "banner_filtered_helper",
        outer_var = outer_var,
        outer_val = outer_val,
        inner_spec = inner_spec,  # Unevaluated expression
        label = col_label
      )
    }

  } else {
    stop("Inner specification must be a variable name or helper expression")
  }

  if (length(col_specs) == 0) {
    stop("Banner produced no column specifications")
  }

  return(col_specs)
}

#' Get value label for a variable-value combination
#'
#' Looks up the label for a specific value of a variable, checking:
#' 1. Value labels in dpdict (if available)
#' 2. Common patterns (e.g., frequency scales)
#' 3. Falls back to string representation
#'
#' @param var Variable name
#' @param value The value to get a label for
#' @param dpdict Data dictionary
#' @return Character string label
#' @keywords internal
get_value_label <- function(var, value, dpdict = NULL) {
  # First check if dpdict has value labels
  if (!is.null(dpdict) && "value_labels" %in% names(dpdict)) {
    var_row <- dpdict[dpdict$variable_names == var, ]
    if (nrow(var_row) > 0 && !is.null(var_row$value_labels[[1]])) {
      labels <- var_row$value_labels[[1]]
      if (as.character(value) %in% names(labels)) {
        return(labels[[as.character(value)]])
      }
    }
  }

  # Common patterns for frequency scales
  if (grepl("^(app_|A2_|A3_|freq_|frequency_)", var)) {
    if (value %in% 1:4) {
      return(c("Daily", "Weekly", "Monthly", "Never")[value])
    } else if (value %in% 1:5) {
      return(c("Daily", "Weekly", "Monthly", "Rarely", "Never")[value])
    }
  }

  # Satisfaction scales
  if (grepl("(satisf|satis)", var, ignore.case = TRUE) && value %in% 1:5) {
    return(c("Very Dissatisfied", "Dissatisfied", "Neutral",
             "Satisfied", "Very Satisfied")[value])
  }

  # Agreement scales
  if (grepl("(agree|agreement)", var, ignore.case = TRUE) && value %in% 1:5) {
    return(c("Strongly Disagree", "Disagree", "Neutral",
             "Agree", "Strongly Agree")[value])
  }

  # Default: return as character
  return(as.character(value))
}

# Extract value labels for a variable from multiple sources
# Falls back to character representation if no labels found
get_variable_labels <- function(var_name, values, data, dpdict) {
  var_data <- data[[var_name]]

  # Handle factors
  if (is.factor(var_data)) {
    result <- character(length(values))
    for (i in seq_along(values)) {
      val <- values[i]
      if (!is.na(val) && val <= length(levels(var_data))) {
        result[i] <- levels(var_data)[val]
      } else {
        result[i] <- as.character(val)
      }
    }
    return(result)
  }

  # Handle haven_labelled objects
  if (inherits(var_data, "haven_labelled") || !is.null(attr(var_data, "labels"))) {
    labels_attr <- attr(var_data, "labels")
    if (!is.null(labels_attr) && length(labels_attr) > 0) {
      result <- character(length(values))
      for (i in seq_along(values)) {
        val <- values[i]
        # For labelled objects, labels are stored as named vector
        label_match <- names(labels_attr)[labels_attr == val]
        if (length(label_match) > 0) {
          result[i] <- label_match[1]
        } else {
          result[i] <- as.character(val)
        }
      }
      return(result)
    }
  }

  # Check dpdict for value labels if available
  if (!is.null(dpdict) && "value_labels" %in% names(dpdict)) {
    var_row <- dpdict[dpdict$variable_names == var_name, ]
    if (nrow(var_row) > 0 && !is.null(var_row$value_labels[[1]])) {
      value_labels <- var_row$value_labels[[1]]
      result <- character(length(values))
      for (i in seq_along(values)) {
        val <- values[i]
        val_char <- as.character(val)
        if (val_char %in% names(value_labels)) {
          result[i] <- value_labels[[val_char]]
        } else {
          result[i] <- val_char
        }
      }
      return(result)
    }
  }

  # Fall back to values as labels
  return(as.character(values))
}

#' Register built-in helpers and statistics
#' @param libname Library name (unused)
#' @param pkgname Package name (unused)
#' @keywords internal
.onLoad <- function(libname = NULL, pkgname = NULL) {

  # Only register if not already registered (avoid duplicates)
  if (!"count" %in% names(.tab_registry$stats)) {

    # SUMMARY CALCULATORS ------------------------------------------------

    calc_union <- function(arrays, base_array) {
      m_mat <- do.call(cbind, lapply(arrays, `[[`, "m"))
      u_mat <- do.call(cbind, lapply(arrays, `[[`, "u"))
      m_out <- as.numeric(rowSums(m_mat) > 0)
      u_out <- as.numeric(rowSums(u_mat) > 0)
      new_tab_arrays(m_out, u_out)
    }

    calc_not_union <- function(arrays, base_array) {
      m_mat <- do.call(cbind, lapply(arrays, `[[`, "m"))
      u_mat <- do.call(cbind, lapply(arrays, `[[`, "u"))
      u_out <- as.numeric(rowSums(u_mat) > 0)
      m_out <- as.numeric(rowSums(m_mat) == 0) * u_out
      new_tab_arrays(m_out, u_out)
    }

    calc_intersection <- function(arrays, base_array) {
      m_mat <- do.call(cbind, lapply(arrays, `[[`, "m"))
      u_mat <- do.call(cbind, lapply(arrays, `[[`, "u"))
      u_out <- as.numeric(rowSums(u_mat) == ncol(u_mat))
      m_out <- as.numeric(rowSums(m_mat) == ncol(m_mat)) * u_out
      new_tab_arrays(m_out, u_out)
    }

    # STATISTICS --------------------------------------------------------

    # Column percentage
    stat_column_pct <- function(base_array, row_m, row_u, col_m, col_u, values = NULL, ...) {

      # Validate array types
      if (get_variable_array_type(row_m) != "categorical" || get_variable_array_type(col_m) != "categorical") {
        warning("statistic did not receive expected categorical row and column arrays (0/1 values)")
      }

      numerator <- sum(base_array * row_m * col_m, na.rm = TRUE)
      denominator <- sum(base_array * col_m * row_u, na.rm = TRUE)
      if (is.na(denominator) || denominator == 0) return(NA_real_)
      numerator / denominator * 100
    }

    stat_column_pct_vectorized <- function(base_array, row_matrix, col_matrix, row_u_matrix = NULL, ...) {
      # For grid questions, each row needs its own column total based on its universe mask
      if (is.null(row_u_matrix)) {
        row_u_matrix <- ifelse(is.na(row_matrix), NA, 1)
      }
      col_totals_per_row <- t(row_u_matrix * base_array) %*% col_matrix
      
      # Compute cell counts
      cell_counts <- t(row_matrix * base_array) %*% col_matrix
      
      # Divide each cell by its row-specific column total
      result <- cell_counts / col_totals_per_row * 100
      result[is.nan(result) | is.infinite(result)] <- NA_real_
      result
    }

    create_statistic("column_pct", stat_column_pct,
                     base_calculator = base_column_total,
                     summary_row = "NET",
                     summary_col = "Total",
                     summary_row_calculator = calc_union,
                     summary_col_calculator = calc_union,
                     format_fn = function(x) sprintf("%.1f%%", x),
                     requires_values = FALSE,
                     base_label = "Base (n)",
                     vectorized_processor = stat_column_pct_vectorized
    )

    # Row percentage
    stat_row_pct <- function(base_array, row_m, row_u, col_m, col_u, values = NULL, ...) {

      # Validate array types
      if (get_variable_array_type(row_m) != "categorical" || get_variable_array_type(col_m) != "categorical") {
        warning("statistic did not receive expected categorical row and column arrays (0/1 values)")
      }

      numerator <- sum(base_array * row_m * col_m, na.rm = TRUE)
      denominator <- sum(base_array * row_m * col_u, na.rm = TRUE)
      if (is.na(denominator) || denominator == 0) return(NA_real_)
      numerator / denominator * 100
    }
    stat_row_pct_vectorized <- function(base_array, row_matrix, col_matrix, ...) {
      # For grid questions, each column needs its own row total based on its NA pattern
      col_mask_matrix <- ifelse(is.na(col_matrix), NA, 1)
      row_totals_per_col <- colSums(base_array * row_matrix * col_mask_matrix, na.rm = TRUE)
      
      # Compute cell counts
      cell_counts <- t(row_matrix * base_array) %*% col_matrix
      
      # Divide each cell by its column-specific row total
      result <- sweep(cell_counts, 2, row_totals_per_col, "/") * 100
      result[is.nan(result) | is.infinite(result)] <- NA_real_
      result
    }
    create_statistic("row_pct", stat_row_pct,
                     base_calculator = base_row_total,
                     summary_row = "NET",
                     summary_col = "Total",
                     summary_row_calculator = calc_union,
                     summary_col_calculator = calc_union,
                     format_fn = function(x) sprintf("%.1f%%", x),
                     requires_values = FALSE,
                     base_label = "Base (n)",
                     vectorized_processor = stat_row_pct_vectorized
    )

    # Count
    stat_count <- function(base_array, row_m, row_u, col_m, col_u, values = NULL, ...) {

      # Validate array types
      if (get_variable_array_type(row_m) != "categorical" || get_variable_array_type(col_m) != "categorical") {
        warning("statistic did not receive expected categorical row and column arrays (0/1 values)")
      }

      sum(base_array * row_m * col_m, na.rm = TRUE)
    }
    stat_count_vectorized <- function(base_array, row_matrix, col_matrix, ...) {
      t(row_matrix * base_array) %*% col_matrix
    }
    create_statistic("count", stat_count,
                     base_calculator = base_grand_total,
                     summary_row = "NET",
                     summary_col = "Total",
                     summary_row_calculator = calc_union,
                     summary_col_calculator = calc_union,
                     format_fn = function(x) as.character(x),
                     requires_values = FALSE,
                     vectorized_processor = stat_count_vectorized
    )

    # Mean (requires values parameter)
    stat_mean <- function(base_array, row_m, row_u, col_m, col_u, values = NULL, ...) {

      # Validate array types
      if (get_variable_array_type(row_m) != "categorical" || get_variable_array_type(col_m) != "categorical") {
        warning("statistic did not receive expected categorical row and column arrays (0/1 values)")
      }

      final_array <- base_array * row_m * col_m

      if (is.null(values)) {
        stop("Mean statistic requires 'values' parameter")
      }

      # Only count non-NA values for both numerator and denominator
      valid_mask <- !is.na(values)
      final_array_valid <- final_array * valid_mask

      numerator <- sum(final_array_valid * values, na.rm = TRUE)
      denominator <- sum(final_array_valid, na.rm = TRUE)
      if (is.na(denominator) || denominator == 0) return(NA_real_)
      numerator / denominator
    }
    create_statistic("mean", stat_mean,
                     base_calculator = base_cell_count_valid,
                     summary_row = "Avg",
                     summary_col = "Avg",
                     summary_row_calculator = calc_union,
                     summary_col_calculator = calc_union,
                     format_fn = function(x) sprintf("%.2f", x),
                     requires_values = TRUE,
                     base_label = "Base (n)"
    )

    # Median
    stat_median <- function(base_array, row_m, row_u, col_m, col_u, values = NULL, ...) {

      # Validate array types
      if (get_variable_array_type(row_m) != "categorical" || get_variable_array_type(col_m) != "categorical") {
        warning("statistic did not receive expected categorical row and column arrays (0/1 values)")
      }

      if (is.null(values)) {
        stop("Median statistic requires 'values' parameter")
      }

      final_array <- base_array * row_m * col_m
      valid_mask <- !is.na(values)
      final_array_valid <- final_array * valid_mask

      # Get values for this cell
      cell_values <- values[final_array_valid > 0]

      if (length(cell_values) == 0) return(NA_real_)
      median(cell_values, na.rm = TRUE)
    }
    create_statistic("median", stat_median,
                     base_calculator = base_cell_count_valid,
                     summary_row = "Total",
                     summary_col = "Total",
                     summary_row_calculator = calc_union,
                     summary_col_calculator = calc_union,
                     format_fn = function(x) sprintf("%.1f", x),
                     requires_values = TRUE,
                     base_label = "Base (n)"
    )

    # Standard Deviation
    stat_sd <- function(base_array, row_m, row_u, col_m, col_u, values = NULL, ...) {

      # Validate array types
      if (get_variable_array_type(row_m) != "categorical" || get_variable_array_type(col_m) != "categorical") {
        warning("statistic did not receive expected categorical row and column arrays (0/1 values)")
      }

      if (is.null(values)) {
        stop("Standard deviation statistic requires 'values' parameter")
      }

      final_array <- base_array * row_m * col_m
      valid_mask <- !is.na(values)
      final_array_valid <- final_array * valid_mask

      # Get values for this cell
      cell_values <- values[final_array_valid > 0]

      if (length(cell_values) < 2) return(NA_real_)
      sd(cell_values, na.rm = TRUE)
    }
    create_statistic("sd", stat_sd,
                     base_calculator = base_cell_count_valid,
                     summary_row = "Total",
                     summary_col = "Total",
                     summary_row_calculator = calc_union,
                     summary_col_calculator = calc_union,
                     format_fn = function(x) sprintf("%.2f", x),
                     requires_values = TRUE,
                     base_label = "Base (n)"
    )

    # Coefficient of Variation (CV)
    stat_cv <- function(base_array, row_m, row_u, col_m, col_u, values = NULL, ...) {

      # Validate array types
      if (get_variable_array_type(row_m) != "categorical" || get_variable_array_type(col_m) != "categorical") {
        warning("statistic did not receive expected categorical row and column arrays (0/1 values)")
      }

      if (is.null(values)) {
        stop("Coefficient of variation statistic requires 'values' parameter")
      }

      final_array <- base_array * row_m * col_m
      valid_mask <- !is.na(values)
      final_array_valid <- final_array * valid_mask

      # Get values for this cell
      cell_values <- values[final_array_valid > 0]

      if (length(cell_values) < 2) return(NA_real_)

      mean_val <- mean(cell_values, na.rm = TRUE)
      sd_val <- sd(cell_values, na.rm = TRUE)

      if (is.na(mean_val) || mean_val == 0) return(NA_real_)
      (sd_val / abs(mean_val)) * 100
    }
    create_statistic("cv", stat_cv,
                     base_calculator = base_cell_count_valid,
                     summary_row = "Total",
                     summary_col = "Total",
                     summary_row_calculator = calc_union,
                     summary_col_calculator = calc_union,
                     format_fn = function(x) sprintf("%.1f%%", x),
                     requires_values = TRUE,
                     base_label = "Base (n)"
    )

    # Index (relative to total column)
    stat_index <- function(base_array, row_m, row_u, col_m, col_u, values = NULL, ...) {

      # Validate array types
      if (get_variable_array_type(row_m) != "categorical" || get_variable_array_type(col_m) != "categorical") {
        warning("statistic did not receive expected categorical row and column arrays (0/1 values)")
      }

      numerator <- sum(base_array * row_m * col_m, na.rm = TRUE)
      denominator <- sum(base_array * col_m * row_u, na.rm = TRUE)
      if (is.na(denominator) || denominator == 0) return(NA_real_)
      cell_pct <- numerator / denominator * 100

      # Calculate total percentage (all columns)
      total_n <- sum(base_array * row_u, na.rm = TRUE)
      if (is.na(total_n) || total_n == 0) return(NA_real_)
      total_pct <- sum(base_array * row_m, na.rm = TRUE) / total_n * 100

      # Index = (cell % / total %) * 100
      if (is.na(total_pct) || total_pct == 0) return(NA_real_)
      (cell_pct / total_pct) * 100
    }
    create_statistic("index", stat_index,
                     base_calculator = base_column_total,
                     summary_row = "NET",
                     summary_col = "NET",
                     summary_row_calculator = calc_union,
                     summary_col_calculator = calc_union,
                     format_fn = function(x) sprintf("%.0f", x),
                     requires_values = FALSE,
                     base_label = "Base (n)"
    )

    # Percentile
    stat_percentile <- function(base_array, row_m, row_u, col_m, col_u, values = NULL, percentile = 50, ...) {

      # Validate array types
      if (get_variable_array_type(row_m) != "categorical" || get_variable_array_type(col_m) != "categorical") {
        warning("statistic did not receive expected categorical row and column arrays (0/1 values)")
      }

      if (is.null(values)) {
        stop("Percentile statistic requires 'values' parameter")
      }

      final_array <- base_array * row_m * col_m
      valid_mask <- !is.na(values)
      final_array_valid <- final_array * valid_mask

      # Get values for this cell
      cell_values <- values[final_array_valid > 0]

      if (length(cell_values) == 0) return(NA_real_)
      quantile(cell_values, probs = percentile/100, na.rm = TRUE)
    }
    # Create specific percentile statistics
    create_statistic("p25", function(...) stat_percentile(..., percentile = 25),
                     base_calculator = base_cell_count_valid,
                     summary_row = "Total",
                     summary_col = "Total",
                     summary_row_calculator = calc_union,
                     summary_col_calculator = calc_union,
                     format_fn = function(x) sprintf("%.1f", x),
                     requires_values = TRUE,
                     base_label = "Base (n)"
    )
    create_statistic("p75", function(...) stat_percentile(..., percentile = 75),
                     base_calculator = base_cell_count_valid,
                     summary_row = "Total",
                     summary_col = "Total",
                     summary_row_calculator = calc_union,
                     summary_col_calculator = calc_union,
                     format_fn = function(x) sprintf("%.1f", x),
                     requires_values = TRUE,
                     base_label = "Base (n)"
    )

    # Correlation
    stat_correlation <- function(base_array, row_m, row_u, col_m, col_u, values = NULL, ...) {

      # Validate array types
      if (get_variable_array_type(row_m) != "numeric" || get_variable_array_type(col_m) != "numeric") {
        warning("statistic did not receive expected numeric row and column arrays")
      }

      # Valid pairs: in table base and valid for both variables
      valid_mask <- !is.na(base_array) & base_array > 0 & row_u > 0 & col_u > 0
      x <- row_m[valid_mask]
      y <- col_m[valid_mask]

      # Calculate Pearson correlation
      if (length(x) < 2) return(NA_real_)
      cor(x, y, use = "complete.obs")
    }
    create_statistic("correlation", stat_correlation,
                     base_calculator = base_pair_total,
                     summary_row = NULL,
                     summary_col = NULL,
                     format_fn = function(x) sprintf("%.3f", x),
                     requires_values = FALSE,
                     base_label = "Base (n)"
    )

    # HELPERS -----------------------------------------------------------

    # Banner helper - using spec generator for tab2() support
    create_helper("banner", banner_spec_generator, returns_specs = TRUE)

    # Top box helper
    help_top_box <- function(formula_spec, data, dpdict = NULL, all_helpers = NULL, ...) {
      n <- formula_spec$components[[2]]

      vars <- resolve_helper_target(formula_spec$components[[1]], data, dpdict, error_context = "top_box")

      make_one <- function(var_name) {
        var_data <- data[[var_name]]

        # Determine top values by label order (haven_labelled) or code order
        if (inherits(var_data, "haven_labelled") && !is.null(attr(var_data, "labels"))) {
          lbls <- attr(var_data, "labels")
          ord_codes <- sort(as.numeric(lbls), decreasing = TRUE)
          top_values <- utils::head(ord_codes, n)
        } else {
          uv <- sort(unique(na.omit(var_data)), decreasing = TRUE)
          top_values <- utils::head(uv, n)
        }

        # Build DSL expression
        dsl_expr <- substitute(VAR %in% VALUES, list(
          VAR = as.symbol(var_name),
          VALUES = top_values
        ))
        
        # Evaluate DSL
        arr <- eval_dsl(dsl_expr, data)
        
        # Set metadata with DSL and return (label auto-derived)
        label <- derive_label_from_dsl(dsl_expr, dpdict)
        set_array_meta(arr, dsl = dsl_expr, label = label)
      }

      if (length(vars) == 1) return(make_one(vars[1]))

      out <- setNames(vector("list", length(vars)), rep("", length(vars)))
      for (i in seq_along(vars)) {
        one <- make_one(vars[i])
        out[[i]] <- one
        names(out)[i] <- attr(one, "meta")$label %||% derive_label_from_dsl(attr(one, "meta")$dsl, dpdict)
      }
      attr(out, "is_multi_column") <- TRUE
      out
    }
    create_helper("top_box", help_top_box)

    # Bottom box helper
    help_bottom_box <- function(formula_spec, data, dpdict = NULL, all_helpers = NULL, ...) {
      n <- formula_spec$components[[2]]

      vars <- resolve_helper_target(formula_spec$components[[1]], data, dpdict, error_context = "bottom_box")

      make_one <- function(var_name) {
        var_data <- data[[var_name]]

        # Determine bottom values by label order (haven_labelled) or code order
        if (inherits(var_data, "haven_labelled") && !is.null(attr(var_data, "labels"))) {
          lbls <- attr(var_data, "labels")
          ord_codes <- sort(as.numeric(lbls), decreasing = FALSE)
          bottom_values <- utils::head(ord_codes, n)
        } else {
          uv <- sort(unique(na.omit(var_data)), decreasing = FALSE)
          bottom_values <- utils::head(uv, n)
        }

        # Build DSL expression
        dsl_expr <- substitute(VAR %in% VALUES, list(
          VAR = as.symbol(var_name),
          VALUES = bottom_values
        ))
        
        # Evaluate and set metadata with label
        arr <- eval_dsl(dsl_expr, data)
        label <- derive_label_from_dsl(dsl_expr, dpdict)
        set_array_meta(arr, dsl = dsl_expr, label = label)
      }

      if (length(vars) == 1) return(make_one(vars[1]))

      out <- setNames(vector("list", length(vars)), rep("", length(vars)))
      for (i in seq_along(vars)) {
        one <- make_one(vars[i])
        out[[i]] <- one
        names(out)[i] <- attr(one, "meta")$label %||% derive_label_from_dsl(attr(one, "meta")$dsl, dpdict)
      }
      attr(out, "is_multi_column") <- TRUE
      out
    }
    create_helper("bottom_box", help_bottom_box)

    # Value range helper
    help_value_range <- function(formula_spec, data, dpdict = NULL, all_helpers = NULL, ...) {
      min_val <- formula_spec$components[[2]]
      max_val <- formula_spec$components[[3]]

      inclusive <- if (length(formula_spec$components) >= 4) {
        formula_spec$components[[4]]
      } else {
        TRUE
      }

      vars <- resolve_helper_target(formula_spec$components[[1]], data, dpdict, error_context = "value_range")

      non_num <- vars[!vapply(vars, function(v) is.numeric(data[[v]]), logical(1))]
      if (length(non_num) > 0) {
        stop("value_range requires numeric variables; non-numeric: ", paste(non_num, collapse = ", "))
      }

      make_one <- function(var_name) {
        # Build DSL expression
        if (inclusive) {
          dsl_expr <- substitute(VAR >= MIN & VAR <= MAX, list(
            VAR = as.symbol(var_name),
            MIN = min_val,
            MAX = max_val
          ))
        } else {
          dsl_expr <- substitute(VAR > MIN & VAR < MAX, list(
            VAR = as.symbol(var_name),
            MIN = min_val,
            MAX = max_val
          ))
        }
        
        # Evaluate and set metadata with label
        arr <- eval_dsl(dsl_expr, data)
        label <- derive_label_from_dsl(dsl_expr, dpdict)
        set_array_meta(arr, dsl = dsl_expr, label = label)
      }

      if (length(vars) == 1) return(make_one(vars[1]))

      out <- setNames(vector("list", length(vars)), rep("", length(vars)))
      for (i in seq_along(vars)) {
        one <- make_one(vars[i])
        out[[i]] <- one
        names(out)[i] <- attr(one, "meta")$label %||% derive_label_from_dsl(attr(one, "meta")$dsl, dpdict)
      }
      attr(out, "is_multi_column") <- TRUE
      out
    }
    create_helper("value_range", help_value_range)

    # Pattern match helper (for text variables)
    help_pattern <- function(formula_spec, data, dpdict = NULL, all_helpers = NULL, ...) {
      patt <- formula_spec$components[[2]]
      ignore_case <- if (length(formula_spec$components) >= 3) formula_spec$components[[3]] else FALSE

      vars <- resolve_helper_target(formula_spec$components[[1]], data, dpdict, error_context = "pattern")

      make_one <- function(var_name) {
        var_data <- data[[var_name]]
        
        # Check if variable is suitable for pattern matching
        is_text_like <- is.character(var_data) || is.factor(var_data) || inherits(var_data, "haven_labelled")
        
        if (!is_text_like) {
          stop("pattern requires a character or factor variable: ", var_name)
        }
        
        # Convert to character for pattern matching
        if (is.factor(var_data)) {
          var_data <- as.character(var_data)
        } else if (inherits(var_data, "haven_labelled")) {
          # For haven_labelled, use the labels if available, otherwise values
          labels_attr <- attr(var_data, "labels")
          if (!is.null(labels_attr) && length(labels_attr) > 0) {
            # Create lookup vector for vectorized mapping (value -> label)
            lookup <- setNames(names(labels_attr), as.character(labels_attr))
            
            # Store original values for fallback
            original_values <- as.character(var_data)
            
            # Vectorized lookup: match all values at once
            var_data <- lookup[original_values]
            
            # For values without labels, use the original value
            no_label <- is.na(var_data)
            var_data[no_label] <- original_values[no_label]
          } else {
            var_data <- as.character(var_data)
          }
        }
        
        arr <- as.numeric(grepl(patt, var_data, ignore.case = ignore_case))
        
        # Pattern matching on transformed data - mark as custom for now
        set_array_meta(arr, dsl = NULL, variables = var_name,
                       tags = list(pattern = patt, ignore_case = ignore_case),
                       label = paste0(get_var_label(var_name, dpdict), " - Pattern: ", patt))
      }

      if (length(vars) == 1) return(make_one(vars[1]))

      out <- setNames(vector("list", length(vars)), rep("", length(vars)))
      for (i in seq_along(vars)) {
        one <- make_one(vars[i])
        out[[i]] <- one
        names(out)[i] <- attr(one, "meta")$label
      }
      attr(out, "is_multi_column") <- TRUE
      out
    }
    create_helper("pattern", help_pattern)

    # Percentile selection helper
    help_percentile <- function(formula_spec, data, dpdict = NULL, all_helpers = NULL, ...) {
      position <- formula_spec$components[[2]]
      pct <- formula_spec$components[[3]]

      vars <- resolve_helper_target(formula_spec$components[[1]], data, dpdict, error_context = "percentile")

      non_num <- vars[!vapply(vars, function(v) is.numeric(data[[v]]), logical(1))]
      if (length(non_num) > 0) {
        stop("percentile requires numeric variables; non-numeric: ", paste(non_num, collapse = ", "))
      }

      make_one <- function(var_name) {
        var_data <- data[[var_name]]
        thr <- stats::quantile(var_data, probs = pct/100, na.rm = TRUE)
        
        # Build DSL expression
        if (identical(position, "above")) {
          dsl_expr <- substitute(VAR > THRESHOLD, list(
            VAR = as.symbol(var_name),
            THRESHOLD = thr
          ))
        } else if (identical(position, "below")) {
          dsl_expr <- substitute(VAR < THRESHOLD, list(
            VAR = as.symbol(var_name),
            THRESHOLD = thr
          ))
        } else {
          stop("position must be 'above' or 'below'")
        }
        
        # Evaluate and set metadata with label
        arr <- eval_dsl(dsl_expr, data)
        label <- derive_label_from_dsl(dsl_expr, dpdict)
        set_array_meta(arr, dsl = dsl_expr, label = label)
      }

      if (length(vars) == 1) return(make_one(vars[1]))

      out <- setNames(vector("list", length(vars)), rep("", length(vars)))
      for (i in seq_along(vars)) {
        one <- make_one(vars[i])
        out[[i]] <- one
        names(out)[i] <- attr(one, "meta")$label %||% derive_label_from_dsl(attr(one, "meta")$dsl, dpdict)
      }
      attr(out, "is_multi_column") <- TRUE
      out
    }
    create_helper("percentile", help_percentile)

    #' Match and aggregate responses across multiple variables
    #'
    #' Creates binary arrays by matching patterns against variable labels or value labels
    #' within a group of variables. Each pattern produces one array indicating which
    #' respondents have ANY positive response for items matching that pattern.
    help_response_match <- function(formula_spec, data, dpdict = NULL, all_helpers = NULL, ...) {

      stopifnot(!is.null(dpdict))

      # -- 0. unpack helper arguments --------------------------------------------
      patterns      <- formula_spec$components[[1]]
      group_name    <- formula_spec$components[[2]]
      if (is.symbol(group_name)) group_name <- as.character(group_name)

      # optional extras
      mode          <- formula_spec$components$mode         %||% "auto"
      pattern_type  <- formula_spec$components$pattern_type %||% "regex"
      label_fn      <- formula_spec$components$label_fn
      values_map    <- formula_spec$components$values       # may be NULL
      drop_empty    <- isTRUE(formula_spec$components$drop_empty %||% TRUE)

      get_variable_labels <- formula_spec$components$get_variable_labels
      get_value_labels    <- formula_spec$components$get_value_labels

      if (!is.null(patterns) && (!is.null(get_variable_labels) || !is.null(get_value_labels))) {
        if ((identical(patterns, get_variable_labels)) || (identical(patterns, get_value_labels))) {
          group_name <- patterns
          patterns <- NULL
          if (is.symbol(group_name)) group_name <- as.character(group_name)
        }
      }

      pattern_sources <- c(!is.null(patterns), !is.null(values_map),
                           !is.null(get_variable_labels), !is.null(get_value_labels))
      if (sum(pattern_sources) > 1) {
        stop("response_match: supply only one of `patterns`, `values`, `get_variable_labels`, or `get_value_labels`.")
      }

      # -- 1. resolve group variables -------------------------------------------
      group_vars <- resolve_to_variables(group_name, data, dpdict,
                                         allow_patterns = TRUE,
                                         error_context  = "question-group")

      # Force fixed=TRUE when using get_variable_labels or get_value_labels
      if (!is.null(get_variable_labels) || !is.null(get_value_labels)) {
        pattern_type <- "fixed"
      }

      # detect / build grepl wrapper
      detect <- switch(pattern_type,
                       regex = grepl,
                       fixed = \(pat, x) grepl(pat, x, fixed = TRUE),
                       glob  = \(pat, x) grepl(glob2rx(pat), x),
                       stop("response_match: unknown pattern_type '", pattern_type, "'")
      )

      get_var_lab <- function(v) dpdict$variable_labels[dpdict$variable_names == v]

      # -- 1.5: Extract labels if requested ---------------------------------
      if (!is.null(get_variable_labels)) {
        # Extract variable labels
        if (is.character(get_variable_labels) && length(get_variable_labels) == 1) {
          # It's a question group name
          if (!get_variable_labels %in% dpdict$question_group) {
            stop("response_match: Question group '", get_variable_labels, "' not found in dpdict")
          }
          # Get all variable labels for this question group
          group_indices <- which(dpdict$question_group == get_variable_labels)
          extracted_labels <- dpdict$variable_labels[group_indices]
          var_names <- dpdict$variable_names[group_indices]
        } else {
          # It's a vector of variable names
          var_indices <- match(get_variable_labels, dpdict$variable_names)
          missing_vars <- get_variable_labels[is.na(var_indices)]
          if (length(missing_vars) > 0) {
            warning("response_match: Variables not found in dpdict: ",
                    paste(missing_vars, collapse = ", "))
          }
          valid_indices <- var_indices[!is.na(var_indices)]
          extracted_labels <- dpdict$variable_labels[valid_indices]
          var_names <- dpdict$variable_names[valid_indices]
        }

        # Use variable names as fallback for empty/NA labels
        extracted_labels[is.na(extracted_labels) | extracted_labels == ""] <-
          var_names[is.na(extracted_labels) | extracted_labels == ""]

        # Convert to named list (maintaining order and duplicates)
        patterns <- as.list(extracted_labels)
        names(patterns) <- extracted_labels

        # Force variable mode
        mode <- "variable"

      } else if (!is.null(get_value_labels)) {
        # Extract value labels
        all_value_labels <- character()

        if (is.character(get_value_labels) && length(get_value_labels) == 1) {
          # It's a question group name
          if (!get_value_labels %in% dpdict$question_group) {
            stop("response_match: Question group '", get_value_labels, "' not found in dpdict")
          }
          # Get all variables in this question group
          group_vars_to_extract <- dpdict$variable_names[dpdict$question_group == get_value_labels]
        } else {
          # It's a vector of variable names
          group_vars_to_extract <- get_value_labels
          missing_vars <- group_vars_to_extract[!group_vars_to_extract %in% dpdict$variable_names]
          if (length(missing_vars) > 0) {
            warning("response_match: Variables not found in dpdict: ",
                    paste(missing_vars, collapse = ", "))
          }
          group_vars_to_extract <- group_vars_to_extract[group_vars_to_extract %in% dpdict$variable_names]
        }

        # Extract value labels for each variable
        for (var in group_vars_to_extract) {
          var_idx <- which(dpdict$variable_names == var)
          if (length(var_idx) > 0) {
            val_labels <- dpdict$value_labels[[var_idx]]
            if (!is.null(val_labels) && !all(is.na(val_labels))) {
              # Handle different value label formats
              if (is.character(val_labels) && is.null(names(val_labels))) {
                # Simple character vector - use as is
                all_value_labels <- c(all_value_labels, val_labels)
              } else if (!is.null(names(val_labels))) {
                # Named vector where names are values and vector elements are labels
                # Extract the labels (the values of the named vector)
                all_value_labels <- c(all_value_labels, as.character(val_labels))
              }
            } else {
              # No value labels - try to get from data using existing function
              var_data <- data[[var]]
              unique_vals <- sort(unique(na.omit(var_data)))
              for (val in unique_vals) {
                label <- get_value_label(var, val, dpdict)
                # Only add if it's not just the string representation of the value
                if (label != as.character(val)) {
                  all_value_labels <- c(all_value_labels, label)
                }
              }
            }
          }
        }

        # Convert to named list
        patterns <- as.list(all_value_labels)
        names(patterns) <- all_value_labels

        # Force value mode
        mode <- "value"
      }

      # -- 2. Build target list ---------------------------------------------------
      if (!is.null(values_map)) {                # CODE-AGGREGATION MODE
        if (is.null(names(values_map)))
          names(values_map) <- as.character(seq_along(values_map))
        targets       <- values_map
        detected_mode <- "value"

      } else {                                   # PATTERN MODE
        # normalise patterns -> named list
        if (is.character(patterns))
          patterns <- as.list(patterns)
        if (is.null(names(patterns)))
          names(patterns) <- as.character(patterns)

        # mode auto-detection ----------------------------------------------------
        hits_var <- hits_val <- logical(length(patterns))

        for (i in seq_along(patterns)) {
          pat <- patterns[[i]]

          # variable labels
          hits_var[i] <- any(vapply(group_vars,
                                    \(v) detect(pat, get_var_lab(v)),
                                    logical(1)))
          # value labels
          for (v in group_vars) {
            codes <- sort(unique(na.omit(data[[v]])))
            for (code in codes) {
              lab <- get_value_label(v, code, dpdict)
              if (detect(pat, lab)) hits_val[i] <- TRUE
            }
          }
        }

        if (mode == "auto") {
          if (all(hits_var)  && !any(hits_val)) detected_mode <- "variable"
          else if (all(hits_val) && !any(hits_var)) detected_mode <- "value"
          else stop("response_match: mixed variable/value patterns - ",
                    "specify `mode=\"variable\"` or `mode=\"value\"`.")
        } else detected_mode <- match.arg(mode, c("variable","value"))

        targets <- patterns
      }

      # -- 3. Generate arrays -----------------------------------------------------
      out <- list()
      n_generated <- 0

      make_label <- function(var_lab, val_lab, bucket, mode) {
        if (is.function(label_fn))
          label_fn(var_lab, val_lab, bucket, mode)
        else if (mode == "variable")            # Var first
          paste0(var_lab, " - ", val_lab)
        else                                    # Value first
          paste0(val_lab, " - ", var_lab)
      }

      # Helper function to attach meta
      attach_meta <- function(arr, meta) {
        # Convert to DSL metadata structure
        # For aggregated arrays, we don't have a clean DSL expression
        vars <- if (!is.null(meta$ivar)) list(meta$ivar) else NULL
        tags <- list(
          value = meta$ival,
          value_label = if (!is.null(meta$ival)) as.character(meta$ival) else NULL
        )
        arr <- set_array_meta(
          arr,
          dsl = NULL,  # Aggregated patterns don't have clean DSL
          variables = vars,
          tags = tags,
          label = meta$label
        )
        arr
      }

      if (detected_mode == "variable") {
        for (bucket in names(targets)) {
          pat <- targets[[bucket]]

          # First, collect ALL variables that match this pattern
          matching_vars <- character(0)
          for (v in group_vars) {
            if (detect(pat, get_var_lab(v))) {
              matching_vars <- c(matching_vars, v)
            }
          }
          if (length(matching_vars) == 0) next

          # Create ONE aggregated array for this pattern
          arr <- rep(0, nrow(data))
          for (v in matching_vars) {
            # Aggregate: set to 1 if ANY matching variable has a positive value
            arr <- pmax(arr, as.numeric(!is.na(data[[v]]) & data[[v]] > 0))
          }

          # Simple label - just the bucket name
          lbl <- bucket  # "Docs", "Presentations", or "Spreadsheets"

          # Attach metadata
          arr <- attach_meta(arr, list(
            ivar = paste(vapply(matching_vars, get_var_lab, character(1)), collapse = "; "),
            ival = NULL,
            label = lbl))

          out[[lbl]] <- arr
          n_generated <- n_generated + 1
        }

      } else {  # value mode -----------------------------------------------------
        if (!is.null(values_map)) {
          for (bucket in names(values_map)) {
            codes_vec <- unname(values_map[[bucket]])

            # Create ONE aggregated array for this bucket
            arr <- rep(0, nrow(data))
            for (v in group_vars) {
              # Aggregate: set to 1 if ANY variable has ANY of the specified codes
              arr <- pmax(arr, as.numeric(data[[v]] %in% codes_vec & !is.na(data[[v]])))
            }

            lbl <- bucket  # Just "Top 2 Box", not variable-specific

            arr <- attach_meta(arr, list(
              ivar = NULL,
              ival = bucket,
              label = lbl))

            attr(arr, "codes") <- codes_vec
            out[[lbl]] <- arr
            n_generated <- n_generated + 1
          }
        } else {                               # pattern matching on values
          for (bucket in names(targets)) {
            pat <- targets[[bucket]]

            # Collect all variable-code pairs that match this value pattern
            matching_pairs <- list()

            for (v in group_vars) {
              var_lab <- get_var_lab(v)
              codes <- sort(unique(na.omit(data[[v]])))

              for (code in codes) {
                val_lab <- get_value_label(v, code, dpdict)
                if (detect(pat, val_lab)) {
                  matching_pairs <- append(matching_pairs,
                                           list(list(var = v, code = code, var_lab = var_lab, val_lab = val_lab)))
                }
              }
            }

            if (length(matching_pairs) == 0) next

            # Create ONE aggregated array for this value pattern
            arr <- rep(0, nrow(data))
            for (pair in matching_pairs) {
              # Create a logical vector: TRUE where this variable equals this specific code
              matches_this_pair <- (data[[pair$var]] == pair$code) & !is.na(data[[pair$var]])
              # Using pmax ensures we get 1 if ANY pair matches (logical OR across all pairs)
              arr <- pmax(arr, as.numeric(matches_this_pair))
            }

            # Simple label - just the bucket name
            lbl <- if (!is.null(label_fn)) {
              # If custom label function provided, use it
              # Pass NULL for var_lab since we're aggregating across multiple variables
              label_fn(NULL, bucket, bucket, "value")
            } else {
              # Default: just use the bucket name
              bucket  # "Daily", "Weekly", or "Monthly"
            }

            # Attach metadata
            arr <- attach_meta(arr, list(
              ivar = NULL,  # Could aggregate variable names if needed
              ival = bucket,
              label = lbl))

            out[[lbl]] <- arr
            n_generated <- n_generated + 1
          }
        }
      }

      # -- 4. Empty-pattern handling --------------------------------------------
      if (n_generated == 0) {
        msg <- "response_match produced no matching arrays."
        if (drop_empty) {
          warning(msg, call. = FALSE)
          attr(out, "is_multi_column") <- TRUE
          return(out[0])
        } else stop(msg)
      }

      attr(out, "is_multi_column") <- TRUE

      out
    }
    create_helper("response_match", help_response_match)

    # Any of helper - checks if any variable in a group/list is positive
    help_any_positive <- function(formula_spec, data, dpdict = NULL, all_helpers = NULL, ...) {
      # Get the input - could be a single string (question group) or vector of strings (variable names)
      vars_input <- formula_spec$components[[1]]

      # Determine if input is a question group or variable list
      if (length(vars_input) == 1 && !is.null(dpdict) && "question_group" %in% names(dpdict)) {
        # Check if it's a question group
        if (vars_input %in% unique(dpdict$question_group)) {
          # It's a question group - get all variables in that group
          vars_to_check <- dpdict$variable_names[dpdict$question_group == vars_input]
          label <- paste0("Any of ", vars_input)
        } else if (vars_input %in% names(data)) {
          # It's a single variable
          vars_to_check <- vars_input
          label <- "Any of 1 variable"
        } else {
          # Assume it's a question group that will be expanded later
          vars_to_check <- vars_input
          label <- paste0("Any of ", vars_input)
        }
      } else {
        # It's a vector of variable names
        vars_to_check <- vars_input
        label <- paste0("Any of ", length(vars_to_check), " variables")
      }

      # Filter to variables that exist in data
      existing_vars <- vars_to_check[vars_to_check %in% names(data)]

      # If no variables exist, return all zeros
      if (length(existing_vars) == 0) {
        result <- rep(0, nrow(data))
        attr(result, "label") <- label
        return(result)
      }

      # Extract the data for these variables
      vars_data <- data[, existing_vars, drop = FALSE]

      # Check for negative values and warn
      has_negatives <- any(vars_data < 0, na.rm = TRUE)
      if (has_negatives) {
        warning("Negative values found in variables for any_positive() helper. Only positive values will be treated as TRUE.")
      }

      # Build DSL: OR chain of var > 0 conditions
      var_conditions <- lapply(existing_vars, function(v) {
        call(">", as.symbol(v), 0)
      })
      
      if (length(var_conditions) == 1) {
        dsl_expr <- var_conditions[[1]]
      } else {
        dsl_expr <- Reduce(function(a, b) call("|", a, b), var_conditions)
      }
      
      # Evaluate DSL
      result <- eval_dsl(dsl_expr, data)
      
      # Ensure NAs become 0
      result[is.na(result)] <- 0
      
      # Set metadata (label already provided)
      set_array_meta(result, dsl = dsl_expr, label = label)

      return(result)
    }
    create_helper("any_positive", help_any_positive)

    # among() helper - attach explicit universe to a spec (m,u)
    help_among <- function(formula_spec, data, dpdict = NULL, all_helpers = NULL, ...) {
      m_input <- formula_spec$components[[1]]
      u_input <- formula_spec$components[[2]]
      
      # Normalize universe to 0/1 numeric, NA -> 0
      u_num <- u_input
      if (is.logical(u_num)) u_num <- as.numeric(u_num)
      if (!is.numeric(u_num)) {
        stop("among(): universe must evaluate to logical or numeric vector")
      }
      if (length(u_num) != nrow(data)) {
        stop("among(): universe must have length nrow(data)")
      }
      u_num[is.na(u_num)] <- 0
      u_num <- as.numeric(u_num > 0)
      
      # Helper arg can be multi-column (list) or single vector/tab_arrays
      if (is.list(m_input) && isTRUE(attr(m_input, "is_multi_column"))) {
        out <- m_input
        for (nm in names(out)) {
          one <- out[[nm]]
          # Convert numeric -> tab_arrays, preserve any meta already on numeric arrays
          if (inherits(one, "tab_arrays")) {
            m_raw <- one$m
            u_raw <- one$u
            u_final <- u_raw * u_num
            out[[nm]] <- structure(list(m = m_raw * u_final, u = u_final), class = "tab_arrays")
          } else {
            m_raw <- one
            if (is.logical(m_raw)) m_raw <- as.numeric(m_raw)
            if (!is.numeric(m_raw) || length(m_raw) != nrow(data)) {
              stop("among(): x must evaluate to numeric/logical vectors of length nrow(data)")
            }
            u_x <- as.numeric(!is.na(m_raw))
            u_final <- u_x * u_num
            m <- m_raw
            m[is.na(m)] <- 0
            m <- m * u_final
            out[[nm]] <- structure(list(m = m, u = u_final), class = "tab_arrays")
          }
        }
        attr(out, "is_multi_column") <- TRUE
        return(out)
      }
      
      # Single spec
      if (inherits(m_input, "tab_arrays")) {
        u_final <- m_input$u * u_num
        return(structure(list(m = m_input$m * u_final, u = u_final), class = "tab_arrays"))
      }
      
      m_raw <- m_input
      if (is.logical(m_raw)) m_raw <- as.numeric(m_raw)
      if (!is.numeric(m_raw)) {
        stop("among(): x must evaluate to logical or numeric vector")
      }
      if (length(m_raw) != nrow(data)) {
        stop("among(): x must have length nrow(data)")
      }
      
      u_x <- as.numeric(!is.na(m_raw))
      u_final <- u_x * u_num
      m <- m_raw
      m[is.na(m)] <- 0
      m <- m * u_final
      
      structure(list(m = m, u = u_final), class = "tab_arrays")
    }
    create_helper("among", help_among)

    help_total <- function(formula_spec, data, dpdict = NULL, all_helpers = NULL, ...) {
      # Return an array of 1s for all respondents
      result <- rep(1, nrow(data))

      # DSL: TRUE (matches all)
      set_array_meta(result, dsl = quote(TRUE), label = "Total")

      return(result)
    }
    create_helper("total", help_total)


    #' All matching helper - returns individual variables matching a pattern
    #'
    #' Finds all variables in a question group whose labels match a pattern
    #' and returns each as a separate row (unlike response_match which aggregates).
    help_all_matching <- function(formula_spec, data, dpdict = NULL, all_helpers = NULL, ...) {

      stopifnot(!is.null(dpdict))

      # -- 0. unpack helper arguments --------------------------------------------
      patterns      <- formula_spec$components[[1]]
      group_name    <- formula_spec$components[[2]]
      if (is.symbol(group_name)) group_name <- as.character(group_name)

      # optional extras
      pattern_type  <- formula_spec$components$pattern_type %||% "fixed"
      label_mode    <- formula_spec$components$label_mode   %||% "full"
      drop_empty    <- isTRUE(formula_spec$components$drop_empty %||% TRUE)

      # Validate inputs
      if (length(patterns) != 1 || !is.character(patterns)) {
        stop("all_matching: pattern must be a single character string")
      }

      # -- 1. resolve group variables -------------------------------------------
      group_vars <- resolve_to_variables(group_name, data, dpdict,
                                         allow_patterns = TRUE,
                                         error_context  = "question-group")

      # build grepl wrapper
      detect <- switch(pattern_type,
                       regex = grepl,
                       fixed = \(pat, x) grepl(pat, x, fixed = TRUE),
                       glob  = \(pat, x) grepl(glob2rx(pat), x),
                       stop("all_matching: unknown pattern_type '", pattern_type, "'")
      )

      get_var_lab <- function(v) {
        idx <- match(v, dpdict$variable_names)
        if (is.na(idx)) return(v)
        label <- dpdict$variable_labels[idx]
        if (is.na(label) || label == "") return(v)
        return(label)
      }

      # -- 2. find matching variables -------------------------------------------
      matching_vars <- character(0)

      for (v in group_vars) {
        if (!v %in% names(data)) next

        var_label <- get_var_lab(v)
        if (detect(patterns, var_label)) {
          matching_vars <- c(matching_vars, v)
        }
      }

      # Check if we found any matches
      if (length(matching_vars) == 0) {
        msg <- paste0("all_matching: no variables in group '", group_name,
                      "' have labels matching pattern '", patterns, "'")
        if (drop_empty) {
          warning(msg, call. = FALSE)
          out <- list()
          attr(out, "is_multi_column") <- TRUE
          return(out)
        } else {
          stop(msg)
        }
      }

      # -- 3. create individual arrays for each matching variable ---------------
      out <- list()

      for (v in matching_vars) {
        if (!v %in% names(data)) next

        var_data <- data[[v]]

        # Convert to numeric if needed, treating positive values as 1
        if (is.logical(var_data)) {
          array_data <- as.numeric(var_data)
        } else if (is.numeric(var_data)) {
          # For numeric data, treat positive values as 1, others as 0
          array_data <- as.numeric(var_data > 0)
        } else if (is.factor(var_data) || is.character(var_data)) {
          # For categorical data, treat non-NA as 1
          array_data <- as.numeric(!is.na(var_data))
        } else {
          # Default: treat non-NA as 1
          array_data <- as.numeric(!is.na(var_data))
        }

        # Handle NAs - keep them as NA
        array_data[is.na(var_data)] <- NA

        # Skip if all zeros/NAs and drop_empty is TRUE
        if (drop_empty && all(array_data == 0 | is.na(array_data), na.rm = TRUE)) {
          next
        }

        # DSL: Identity (just the variable reference)
        dsl_expr <- as.symbol(v)
        
        # Create display label based on label_mode
        display_label <- get_display_label(v, dpdict, label_mode, NULL, data)

        # Set metadata with DSL (label provided)
        array_data <- set_array_meta(array_data, dsl = dsl_expr, label = display_label)

        out[[display_label]] <- array_data
      }

      # Final check for empty results
      if (length(out) == 0) {
        msg <- paste0("all_matching: no variables with positive responses found")
        if (drop_empty) {
          warning(msg, call. = FALSE)
        } else {
          stop(msg)
        }
      }

      attr(out, "is_multi_column") <- TRUE
      return(out)
    }

    # Register the helper
    create_helper("all_matching", help_all_matching)


    # SIGNIFICANCE TESTS ------------------------------------------------

    # Z-test for proportions
    create_significance_test(
      id = "z_test_proportions",
      name = "Z-test for proportions",
      description = "Two-sample z-test for comparing proportions",
      processor = function(base_array, row_m, row_u, col_m_1, col_u_1, col_m_2, col_u_2,
                           values = NULL, ...) {

        # Get counts and totals
        n1 <- sum(base_array * row_m * col_m_1, na.rm = TRUE)
        n2 <- sum(base_array * row_m * col_m_2, na.rm = TRUE)
        N1 <- sum(base_array * col_m_1 * row_u, na.rm = TRUE)
        N2 <- sum(base_array * col_m_2 * row_u, na.rm = TRUE)

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
          effect = p2 - p1,
          method = "Two-sample z-test for proportions"
        ))
      }
    )

    # Weighted Z-test for proportions
    create_significance_test(
      id = "z_test_weighted",
      name = "Weighted Z-test for proportions",
      description = "Two-sample z-test for weighted proportions using effective sample sizes",
      processor = function(base_array, row_m, row_u, col_m_1, col_u_1, col_m_2, col_u_2,
                           values = NULL, ...) {

        # Weighted counts (numerators)
        n1 <- sum(base_array * row_m * col_m_1, na.rm = TRUE)
        n2 <- sum(base_array * row_m * col_m_2, na.rm = TRUE)

        # Weighted totals (denominators)
        N1 <- sum(base_array * col_m_1 * row_u, na.rm = TRUE)
        N2 <- sum(base_array * col_m_2 * row_u, na.rm = TRUE)

        if (N1 == 0 || N2 == 0) {
          return(list(p_value = NA_real_, statistic = NA_real_))
        }

        # Weighted proportions
        p1 <- n1 / N1
        p2 <- n2 / N2

        # Get weights for each column (only for observations in that column)
        idx_1 <- col_m_1 > 0 & row_u > 0
        idx_2 <- col_m_2 > 0 & row_u > 0

        w1 <- base_array[idx_1]
        w2 <- base_array[idx_2]

        # Remove any zero or negative weights
        w1 <- w1[w1 > 0]
        w2 <- w2[w2 > 0]

        if (length(w1) == 0 || length(w2) == 0) {
          return(list(p_value = NA_real_, statistic = NA_real_))
        }

        # Calculate effective sample sizes: n_eff = (sum(w))^2 / sum(w^2)
        n1_eff <- (sum(w1, na.rm = TRUE))^2 / sum(w1^2, na.rm = TRUE)
        n2_eff <- (sum(w2, na.rm = TRUE))^2 / sum(w2^2, na.rm = TRUE)

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
          effect = p2 - p1,
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
      processor = function(base_array, row_m, row_u, col_m_1, col_u_1, col_m_2, col_u_2,
                           values, ...) {
        # Get values for both groups
        group1_mask <- base_array * row_m * col_m_1 > 0
        group2_mask <- base_array * row_m * col_m_2 > 0

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
      processor = function(base_array, row_m, row_u, col_m_1, col_u_1, col_m_2, col_u_2,
                           values, ...) {
        if (is.null(values)) {
          stop("Weighted t-test requires 'values' parameter")
        }

        # Get weighted values for both groups
        group1_mask <- base_array * row_m * col_m_1 > 0
        group2_mask <- base_array * row_m * col_m_2 > 0

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
        mean1 <- sum(weights1 * values1, na.rm = TRUE) / sum(weights1, na.rm = TRUE)
        mean2 <- sum(weights2 * values2, na.rm = TRUE) / sum(weights2, na.rm = TRUE)

        # Weighted variances (using population formula, then correcting)
        var1_pop <- sum(weights1 * (values1 - mean1)^2, na.rm = TRUE) / sum(weights1, na.rm = TRUE)
        var2_pop <- sum(weights2 * (values2 - mean2)^2, na.rm = TRUE) / sum(weights2, na.rm = TRUE)

        # Effective sample sizes
        n1_eff <- (sum(weights1, na.rm = TRUE))^2 / sum(weights1^2, na.rm = TRUE)
        n2_eff <- (sum(weights2, na.rm = TRUE))^2 / sum(weights2^2, na.rm = TRUE)

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
      is_omnibus = TRUE,
      processor = function(base_array, row_arrays, row_u_arrays, col_arrays, col_u_arrays,
                           values = NULL, ...) {
        # Build contingency table
        n_rows <- length(row_arrays)
        n_cols <- length(col_arrays)

        cont_table <- matrix(0, nrow = n_rows, ncol = n_cols)
        for (i in 1:n_rows) {
          for (j in 1:n_cols) {
            cont_table[i, j] <- sum(base_array * row_arrays[[i]] * col_arrays[[j]], na.rm = TRUE)
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
      processor = function(base_array, row_m, row_u, col_m_1, col_u_1, col_m_2, col_u_2,
                           values, ...) {
        # Get values for both groups
        group1_mask <- base_array * row_m * col_m_1 > 0
        group2_mask <- base_array * row_m * col_m_2 > 0

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
  
  # Register derive operations (if derive_operations.R is loaded)
  if (exists("register_builtin_derive_operations")) {
    register_builtin_derive_operations()
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
