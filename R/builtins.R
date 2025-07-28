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

#' Ensure array has meta attribute with standard structure
#' @param arr Numeric array
#' @param ivar Variable label(s)
#' @param ival Value label(s)
#' @param label Display label
#' @return Array with meta attribute attached
#' @keywords internal
.ensure_meta <- function(arr, ivar = NULL, ival = NULL, label = NULL) {
  attr(arr, "meta") <- list(
    ivar  = ivar,
    ival  = ival,
    label = label
  )
  arr
}

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

#' Create a gate that matches on a metadata field
#'
#' Creates a gate function that allows cells to be computed only when
#' the row and column share at least one value for the specified metadata field.
#'
#' @param key_field The metadata field name to match on (e.g., "ival", "ivar")
#' @return A gate function that can be used with calc_if
#' @export
#' @examples
#' # Use with calc_if to create orthogonal tables
#' tab(data,
#'     calc_if(no_mismatch("ival"), response_match(...)),
#'     statistic = "count")
no_mismatch <- function(key_field) {
  # Try to get the gate factory
  gate_factory <- get_gate("no_mismatch")

  # If not found, ensure builtins are registered and try again
  if (is.null(gate_factory)) {
    ensure_builtins_registered()
    gate_factory <- get_gate("no_mismatch")

    if (is.null(gate_factory)) {
      stop("no_mismatch gate not found in registry. This is likely a package initialization issue.")
    }
  }

  gate_factory(key_field)
}

#' Apply a gate to an expression
#'
#' Restricts which cells in a table are computed based on a gate function.
#' The gate function determines whether a cell should be calculated based on
#' the metadata of the row and column.
#'
#' @param gate A gate function (e.g., from no_mismatch())
#' @param expr The expression to be gated
#' @return A gated expression for use in tab()
#' @export
#' @examples
#' # Create orthogonal table where cells only computed when row/col share ival
#' tab(data,
#'     rows = calc_if(no_mismatch("ival"), response_match(...)),
#'     cols = another_var)
calc_if <- function(gate, expr) {
  structure(
    list(
      expr = rlang::enquo(expr),   # quosure holding the original expression
      gate = gate                   # a *fully-constructed* gate function
    ),
    class = "tab_helper",
    id = "calc_if"
  )
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
      combined_matrix <- do.call(cbind, arrays)
      as.numeric(rowSums(combined_matrix) > 0, na.rm = TRUE)
    }

    calc_not_union <- function(arrays, base_array) {
      combined_matrix <- do.call(cbind, arrays)
      as.numeric(rowSums(combined_matrix) == 0, na.rm = TRUE)
    }

    calc_intersection <- function(arrays, base_array) {
      combined_matrix <- do.call(cbind, arrays)
      as.numeric(rowSums(combined_matrix) == ncol(combined_matrix), na.rm = TRUE)
    }

    # STATISTICS --------------------------------------------------------

    # Column percentage
    stat_column_pct <- function(base_array, row_array, col_array, ...) {

      # Validate array types
      if (get_variable_array_type(row_array) != "categorical" || get_variable_array_type(col_array) != "categorical") {
        warning("statistic did not receive expected categorical row and column arrays (0/1 values)")
      }

      column_snapshot <- base_array * col_array
      final_array <- column_snapshot * row_array

      column_total <- sum(column_snapshot, na.rm = TRUE)
      if (is.na(column_total) || column_total == 0) return(NA_real_)
      sum(final_array, na.rm = TRUE) / column_total * 100
    }

    stat_column_pct_vectorized <- function(base_array, row_matrix, col_matrix, ...) {
      col_totals <- colSums(base_array * col_matrix, na.rm = TRUE)
      cell_counts <- t(row_matrix * base_array) %*% col_matrix
      result <- sweep(cell_counts, 2, col_totals, "/") * 100
      result[is.nan(result)] <- NA_real_
      result
    }

    create_statistic("column_pct", stat_column_pct,
                     base_calculator = base_column_total,
                     summary_row = "NET",
                     summary_col = "NET",
                     summary_row_calculator = calc_union,
                     summary_col_calculator = calc_union,
                     format_fn = function(x) sprintf("%.1f%%", x),
                     requires_values = FALSE,
                     base_label = "Base (n)",
                     vectorized_processor = stat_column_pct_vectorized
    )

    # Row percentage
    stat_row_pct <- function(base_array, row_array, col_array, ...) {

      # Validate array types
      if (get_variable_array_type(row_array) != "categorical" || get_variable_array_type(col_array) != "categorical") {
        warning("statistic did not receive expected categorical row and column arrays (0/1 values)")
      }

      row_snapshot <- base_array * row_array
      final_array <- row_snapshot * col_array

      row_total <- sum(row_snapshot, na.rm = TRUE)
      if (is.na(row_total) || row_total == 0) return(NA_real_)
      sum(final_array, na.rm = TRUE) / row_total * 100
    }
    stat_row_pct_vectorized <- function(base_array, row_matrix, col_matrix, ...) {
      row_totals <- colSums(base_array * row_matrix, na.rm = TRUE)
      cell_counts <- t(row_matrix * base_array) %*% col_matrix
      result <- sweep(cell_counts, 1, row_totals, "/") * 100
      result[is.nan(result)] <- NA_real_
      result
    }
    create_statistic("row_pct", stat_row_pct,
                     base_calculator = base_row_total,
                     summary_row = "NET",
                     summary_col = "NET",
                     summary_row_calculator = calc_union,
                     summary_col_calculator = calc_union,
                     format_fn = function(x) sprintf("%.1f%%", x),
                     requires_values = FALSE,
                     base_label = "Base (n)",
                     vectorized_processor = stat_row_pct_vectorized
    )

    # Count
    stat_count <- function(base_array, row_array, col_array, ...) {

      # Validate array types
      if (get_variable_array_type(row_array) != "categorical" || get_variable_array_type(col_array) != "categorical") {
        warning("statistic did not receive expected categorical row and column arrays (0/1 values)")
      }

      sum(base_array * row_array * col_array, na.rm = TRUE)
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
    stat_index <- function(base_array, row_array, col_array, ...) {

      # Validate array types
      if (get_variable_array_type(row_array) != "categorical" || get_variable_array_type(col_array) != "categorical") {
        warning("statistic did not receive expected categorical row and column arrays (0/1 values)")
      }

      # Calculate cell percentage
      column_snapshot <- base_array * col_array
      final_array <- column_snapshot * row_array

      column_total <- sum(column_snapshot, na.rm = TRUE)
      if (is.na(column_total) || column_total == 0) return(NA_real_)
      cell_pct <- sum(final_array, na.rm = TRUE) / column_total * 100

      # Calculate total percentage (all columns)
      total_snapshot <- base_array  # All data
      total_for_row <- total_snapshot * row_array

      total_n <- sum(total_snapshot, na.rm = TRUE)
      if (is.na(column_total) || column_total == 0) return(NA_real_)
      total_pct <- sum(total_for_row, na.rm = TRUE) / total_n * 100

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
    stat_correlation <- function(base_array, row_array, col_array, ...) {

      # Validate array types
      if (get_variable_array_type(row_array) != "numeric" || get_variable_array_type(col_array) != "numeric") {
        warning("statistic did not receive expected numeric row and column arrays")
      }

      # Filter both arrays by base_array
      valid_mask <- !is.na(base_array) & base_array > 0
      x <- row_array[valid_mask]
      y <- col_array[valid_mask]

      # Calculate Pearson correlation
      if (length(x) < 2) return(NA_real_)
      cor(x, y, use = "complete.obs")
    }
    create_statistic("correlation", stat_correlation,
                     base_calculator = base_cell_count,
                     summary_row = NULL,
                     summary_col = NULL,
                     format_fn = function(x) sprintf("%.3f", x),
                     requires_values = FALSE,
                     base_label = "Base (n)"
    )

    # HELPERS -----------------------------------------------------------

    # Top box helper
    help_top_box <- function(formula_spec, data, dpdict = NULL, ...) {
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
    help_bottom_box <- function(formula_spec, data, dpdict = NULL, ...) {
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
    help_value_range <- function(formula_spec, data, dpdict = NULL, ...) {
      var_name <- as.character(formula_spec$components[[1]])
      min_val <- formula_spec$components[[2]]
      max_val <- formula_spec$components[[3]]

      inclusive <- if (length(formula_spec$components) >= 4) {
        formula_spec$components[[4]]
      } else {
        TRUE  # Default to inclusive range when not specified
      }

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
    help_pattern <- function(formula_spec, data, dpdict = NULL, ...) {
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
    help_percentile <- function(formula_spec, data, dpdict = NULL, ...) {
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

    #' Match and aggregate responses across multiple variables
    #'
    #' Creates binary arrays by matching patterns against variable labels or value labels
    #' within a group of variables. Each pattern produces one array indicating which
    #' respondents have ANY positive response for items matching that pattern.
    help_response_match <- function(formula_spec, data, dpdict = NULL, ...) {

      stopifnot(!is.null(dpdict))

      # ── 0. unpack helper arguments --------------------------------------------
      patterns      <- formula_spec$components[[1]]
      group_name    <- formula_spec$components[[2]]
      if (is.symbol(group_name)) group_name <- as.character(group_name)

      # optional extras
      mode          <- formula_spec$components$mode         %||% "auto"
      pattern_type  <- formula_spec$components$pattern_type %||% "regex"
      label_fn      <- formula_spec$components$label_fn
      values_map    <- formula_spec$components$values       # may be NULL
      drop_empty    <- isTRUE(formula_spec$components$drop_empty %||% TRUE)

      # can't mix `patterns` and `values`
      if (!is.null(values_map) && !is.null(patterns))
        stop("response_match: supply either `patterns` or `values`, not both.")

      # ── 1. resolve group variables -------------------------------------------
      group_vars <- resolve_to_variables(group_name, data, dpdict,
                                         allow_patterns = TRUE,
                                         error_context  = "question-group")

      # detect / build grepl wrapper
      detect <- switch(pattern_type,
                       regex = grepl,
                       fixed = \(pat, x) grepl(pat, x, fixed = TRUE),
                       glob  = \(pat, x) grepl(glob2rx(pat), x),
                       stop("response_match: unknown pattern_type '", pattern_type, "'")
      )

      get_var_lab <- function(v) dpdict$variable_labels[dpdict$variable_names == v]

      # ── 2. Build target list ---------------------------------------------------
      if (!is.null(values_map)) {                # CODE-AGGREGATION MODE
        if (is.null(names(values_map)))
          names(values_map) <- as.character(seq_along(values_map))
        targets       <- values_map
        detected_mode <- "value"

      } else {                                   # PATTERN MODE
        # normalise patterns → named list
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
          else stop("response_match: mixed variable/value patterns – ",
                    "specify `mode=\"variable\"` or `mode=\"value\"`.")
        } else detected_mode <- match.arg(mode, c("variable","value"))

        targets <- patterns
      }

      # ── 3. Generate arrays -----------------------------------------------------
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
        arr <- .ensure_meta(
          arr,
          ivar = meta$ivar,
          ival = meta$ival,
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

      # ── 4. Empty-pattern handling --------------------------------------------
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

    # Register (or overwrite) helper in the global registry
    create_helper("response_match", help_response_match)

    # SIGNIFICANCE TESTS ------------------------------------------------

    # Z-test for proportions
    create_significance_test(
      id = "z_test_proportions",
      name = "Z-test for proportions",
      description = "Two-sample z-test for comparing proportions",
      processor = function(base_array, row_array, col_array_1, col_array_2, ...) {
        # Get counts and totals
        n1 <- sum(base_array * row_array * col_array_1, na.rm = TRUE)
        n2 <- sum(base_array * row_array * col_array_2, na.rm = TRUE)
        N1 <- sum(base_array * col_array_1, na.rm = TRUE)
        N2 <- sum(base_array * col_array_2, na.rm = TRUE)

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
        n1 <- sum(base_array * row_array * col_array_1, na.rm = TRUE)
        n2 <- sum(base_array * row_array * col_array_2, na.rm = TRUE)

        # Weighted totals (denominators)
        N1 <- sum(base_array * col_array_1, na.rm = TRUE)
        N2 <- sum(base_array * col_array_2, na.rm = TRUE)

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
      processor = function(base_array, row_arrays, col_arrays, ...) {
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
    # GATE FUNCTIONS
    create_gate(
      "no_mismatch",
      factory_fn = function(key_field) {
        force(key_field)                    # capture the field name
        function(row_meta, col_meta, ...) {   # gate takes row & col meta lists
          rv <- row_meta[[key_field]]
          cv <- col_meta[[key_field]]
          if (is.null(rv) || is.null(cv)) return(TRUE)
          # Treat meta fields as character vectors and look for any overlap
          rv <- as.character(rv);  cv <- as.character(cv)
          length(intersect(rv, cv)) > 0
        }
      }
    )

    # calc_if processor
    help_calc_if <- function(formula_spec, data, dpdict = NULL, helpers = NULL, ...) {

      inner_spec <- parse_table_formula(
        formula_spec$expr,
        data,
        dpdict,
        helpers
      )

      inner_spec$gate <- formula_spec$gate

      inner_spec
    }
    create_helper("calc_if", help_calc_if)
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


