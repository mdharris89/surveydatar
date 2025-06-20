#' Create cross-tabulation tables with flexible formula syntax
#'
#' @param data Data frame or survey_data object
#' @param rows Row specification (variable name, expression, or named list)
#' @param cols Column specification (optional)
#' @param filter Table-wide filter expression (optional)
#' @param weight Weight variable name (optional)
#' @param statistic Type of statistic: "column_pct", "count", "row_pct", "mean"
#' @param values Variable name to aggregate for value-based statistics like mean (optional)
#' @param show_row_nets Whether to show NET of rows (NET row for column_pct, NET column for row_pct)
#' @param show_col_nets Whether to show NET of columns (NET column for column_pct, NET row for row_pct)
#' @param low_base_threshold Numeric, minimum base size required to show a column
#' @param helpers List of custom tab_helper objects
#' @param stats List of custom tab_stat objects
#' @param resolve_report Whether to return variable resolution details
#' @param ... Additional arguments
#' @return Cross-tabulation table as data.frame
#' @export
#' @examples
#' # Traditional syntaxcorrec
#' tab(data, "q1", "gender")
#'
#' # Formula syntax
#' tab(data, q1 * (age > 30), gender)
#'
#' # With table-wide filter
#' tab(data, q1, gender, filter = age > 18)
#'
#' # Multiple row groups
#' tab(data, rows = rows_list("Total" = q1, "Young" = q1 * (age < 30)))
tab <- function(data, rows, cols = NULL, filter = NULL, weight = NULL,
                statistic = c("column_pct", "count", "row_pct", "mean"),
                values = NULL,
                show_row_nets = TRUE, show_col_nets = TRUE,
                low_base_threshold = 0,
                prefix_variable_label = TRUE,
                helpers = NULL, stats = NULL, resolve_report = FALSE,
                ...) {

  # Store original call for later use
  original_call <- match.call()

  # Handle statistic parameter - can be string or tab_stat object
  if (is.character(statistic)) {
    if (length(statistic) > 1) {
      statistic <- match.arg(statistic)
    }
    # Will be resolved to object later
  } else if (!inherits(statistic, "tab_stat")) {
    stop("statistic must be a character string or tab_stat object")
  }

  # Determine statistic ID early for validation
  statistic_id <- if (is.character(statistic)) {
    statistic
  } else if (inherits(statistic, "tab_stat")) {
    statistic$id
  } else {
    "unknown"
  }

  # Validate values parameter based on statistic metadata
  stat_obj <- if (is.character(statistic)) {
    .tab_registry$stats[[statistic_id]]
  } else {
    statistic
  }

  if (!is.null(stat_obj)) {
    if (stat_obj$requires_values && is.null(values)) {
      stop(stat_obj$id, " statistic requires 'values' parameter")
    }

    if (!is.null(values) && !stat_obj$requires_values) {
      warning("Values parameter ignored for ", stat_obj$id, " statistic")
    }
  }

  all_stats <- .tab_registry$stats
  if (!is.null(stats)) {
    if (!is.list(stats)) {
      stop("stats must be a list of tab_stat objects")
    }
    # Check that all items are tab_stat objects
    stat_check <- vapply(stats, inherits, logical(1), "tab_stat")
    if (!all(stat_check)) {
      stop("All items in stats list must be tab_stat objects")
    }
    # Add to registry (with potential overwrites)
    for (stat in stats) {
      all_stats[[stat$id]] <- stat
    }
  }

  # Resolve statistic to object if it's still a string
  if (is.character(statistic)) {
    if (!statistic %in% names(all_stats)) {
      stop("Unknown statistic: '", statistic, "'. Available: ",
           paste(names(all_stats), collapse = ", "))
    }
    statistic <- all_stats[[statistic]]
  }

  # Merge custom helpers and stats with built-ins
  all_helpers <- .tab_registry$helpers
  if (!is.null(helpers)) {
    if (!is.list(helpers)) {
      stop("helpers must be a list of tab_helper objects")
    }
    # Check that all items are tab_helper objects
    helper_check <- vapply(helpers, inherits, logical(1), "tab_helper")
    if (!all(helper_check)) {
      stop("All items in helpers list must be tab_helper objects")
    }
    # Add to registry (with potential overwrites)
    for (helper in helpers) {
      all_helpers[[helper$id]] <- helper
    }
  }

  # Extract data and dpdict if survey_data object
  if (inherits(data, "survey_data")) {
    dpdict <- data$dpdict
    data <- data$dat
  } else {
    dpdict <- NULL
  }

  # Check for empty data
  if (nrow(data) == 0) {
    warning("Data has 0 rows. Returning empty result.")
  }

  if (resolve_report) {
    message("Starting variable resolution...")
  }

  # Extract variable names from expressions for resolution
  var_candidates <- character(0)

  # Extract from rows
  rows_quo <- rlang::enquo(rows)
  rows_expr <- rlang::quo_get_expr(rows_quo)
  if (rlang::is_string(rows_expr)) {
    var_candidates <- c(var_candidates, rows_expr)
  }

  # Extract from cols if provided
  if (!missing(cols)) {
    cols_quo <- rlang::enquo(cols)
    cols_expr <- rlang::quo_get_expr(cols_quo)
    if (rlang::is_string(cols_expr)) {
      var_candidates <- c(var_candidates, cols_expr)
    }
  }

  # Extract from weight if provided
  if (!is.null(weight) && is.character(weight)) {
    var_candidates <- c(var_candidates, weight)
  }

  # Extract from values if provided
  if (!is.null(values) && is.character(values)) {
    var_candidates <- c(var_candidates, values)
  }

  # Resolve variable names if any string candidates found
  if (length(var_candidates) > 0 && resolve_report) {
    tryCatch({
      resolved_vars <- resolve_vars(
        data = data,
        dpdict = dpdict,
        var_names = var_candidates,
        threshold = 0.75,
        report = resolve_report
      )

      if (resolve_report) {
        message("Variable resolution completed:")
        resolution_log <- attr(resolved_vars, "resolution_log")
        if (!is.null(resolution_log)) {
          for (orig_name in names(resolution_log)) {
            log_entry <- resolution_log[[orig_name]]
            message(sprintf("  %s -> %s (via %s)", orig_name, log_entry$result, log_entry$method))
          }
        }
      }

      # Update variable references with resolved names
      if (rlang::is_string(rows_expr) && rows_expr %in% names(resolved_vars)) {
        rows_expr <- resolved_vars[[rows_expr]]
        rows_quo <- rlang::quo(!!rlang::sym(rows_expr))
      }

      if (!missing(cols) && rlang::is_string(cols_expr) && cols_expr %in% names(resolved_vars)) {
        cols_expr <- resolved_vars[[cols_expr]]
        cols_quo <- rlang::quo(!!rlang::sym(cols_expr))
      }

      if (!is.null(weight) && weight %in% names(resolved_vars)) {
        weight <- resolved_vars[[weight]]
      }

      if (!is.null(values) && values %in% names(resolved_vars)) {
        values <- resolved_vars[[values]]
      }
    }, error = function(e){
      # If resolution fails, fall back to original behavior
      # This preserves backward compatibility and existing error messages
      if (resolve_report) {
        message("Variable resolution failed: ", e$message)
        message("Falling back to original variable handling")
      }
    })
  }

  base_array <- rep(1, nrow(data))

  # Apply weights
  if (!is.null(weight)) {
    if (!weight %in% names(data)) {
      stop("Weight variable '", weight, "' not found in data")
    }
    base_array <- base_array * data[[weight]]
  }

  # Apply whole-table filter
  if (!missing(filter)) {
    filter_quo <- rlang::enquo(filter)
    filter_parsed <- parse_table_formula(filter_quo, data, dpdict, all_helpers)
    filter_logic <- formula_to_array(filter_parsed, data)
    base_array <- base_array * filter_logic
  }

  # Parse row specifications
  rows_quo <- rlang::enquo(rows)
  rows_expr <- rlang::quo_get_expr(rows_quo)

  # Check if it's traditional string syntax
  if (rlang::is_string(rows_expr)) {
    rows_parsed <- list(parse_table_formula(rows_quo, data, dpdict, all_helpers))
  } else {
    # Try to evaluate to see if it's a rows_list
    rows_eval <- tryCatch(
      rlang::eval_tidy(rows_quo),
      error = function(e) NULL
    )
    # Check for tab_helper BEFORE checking is_list (since tab_helper inherits from list)
    if (inherits(rows_eval, "tab_helper")) {
      # It's a single helper function
      rows_parsed <- list(parse_table_formula(rows_eval, data, dpdict, all_helpers))
    } else if (rlang::is_list(rows_eval)) {
      # It's a rows_list
      rows_parsed <- lapply(seq_along(rows_eval), function(i) {
        if (rlang::is_quosure(rows_eval[[i]])) {
          expr <- rlang::quo_get_expr(rows_eval[[i]])
          # Only evaluate if it's a function call that might be a helper
          if (rlang::is_call(expr)) {
            fn_name <- as.character(expr[[1]])
            if (fn_name %in% names(all_helpers)) {
              # It's a helper function - evaluate it
              item_eval <- rlang::eval_tidy(rows_eval[[i]], data)
            } else {
              # It's some other function call - pass the quosure
              item_eval <- rows_eval[[i]]
            }
          } else {
            # It's a simple variable or other expression - pass the quosure
            item_eval <- rows_eval[[i]]
          }
        } else {
          item_eval <- rows_eval[[i]]
        }
        parsed <- parse_table_formula(item_eval, data, dpdict, all_helpers)
        nm <- names(rows_eval)[i]
        if (!is.null(nm) && nzchar(nm)) {
          parsed$label <- nm
        }
        parsed
      })
    } else {
      # It's a formula expression
      rows_parsed <- list(parse_table_formula(rows_quo, data, dpdict, all_helpers))
    }
  }

  # Parse column specifications
  if (!missing(cols)) {
    cols_quo <- rlang::enquo(cols)
    cols_expr <- rlang::quo_get_expr(cols_quo)

    if (rlang::is_string(cols_expr)) {
      cols_parsed <- list(parse_table_formula(cols_quo, data, dpdict, all_helpers))
    } else {
      # Try to evaluate safely (in case there are column helper functions in future)
      cols_eval <- tryCatch(
        rlang::eval_tidy(cols_quo),
        error = function(e) NULL
      )
      # Check for tab_helper BEFORE checking is_list (since tab_helper inherits from list)
      if (inherits(cols_eval, "tab_helper")) {
        # It's a single helper function
        cols_parsed <- list(parse_table_formula(cols_eval, data, dpdict, all_helpers))
      } else if (rlang::is_list(cols_eval)) {
        # Handle list-based column specs if implemented in future
        cols_parsed <- lapply(seq_along(cols_eval), function(i) {
          if (rlang::is_quosure(cols_eval[[i]])) {
            expr <- rlang::quo_get_expr(cols_eval[[i]])
            # Only evaluate if it's a function call that might be a helper
            if (rlang::is_call(expr)) {
              fn_name <- as.character(expr[[1]])
              if (fn_name %in% names(all_helpers)) {
                # It's a helper function - evaluate it
                item_eval <- rlang::eval_tidy(cols_eval[[i]], data)
              } else {
                # It's some other function call - pass the quosure
                item_eval <- cols_eval[[i]]
              }
            } else {
              # It's a simple variable or other expression - pass the quosure
              item_eval <- cols_eval[[i]]
            }
          } else {
            item_eval <- cols_eval[[i]]
          }
          parsed <- parse_table_formula(item_eval, data, dpdict, all_helpers)
          nm <- names(cols_eval)[i]
          if (!is.null(nm) && nzchar(nm)) {
            parsed$label <- nm
          }
          parsed
        })
      } else {
        # It's a formula expression
        cols_parsed <- list(parse_table_formula(cols_quo, data, dpdict, all_helpers))
      }
    }
  } else {
    cols_parsed <- NULL
  }

  # Expand variables for rows
  rows_expanded <- list()
  for (row_spec in rows_parsed) {
    expanded <- expand_variables(row_spec, data, dpdict, statistic_id, values_var = NULL, prefix_variable_label)
    for (exp in expanded) {
      # Only set group_label if we're dealing with rows_list (multiple named specs)
      if (!is.null(row_spec$label) && length(rows_parsed) > 1) {
        exp$group_label <- row_spec$label
      }
      # For simple cases like tab(data, gender), group_label stays NULL
      rows_expanded <- append(rows_expanded, list(exp))
    }
  }

  # Expand variables for columns
  if (!is.null(cols_parsed)) {
    cols_expanded <- list()
    for (col_spec in cols_parsed) {
      expanded <- expand_variables(col_spec, data, dpdict, statistic_id, values_var = NULL, prefix_variable_label)
      cols_expanded <- append(cols_expanded, expanded)
    }
  } else {
    cols_expanded <- list(list(type = "total", label = "Total"))
  }

  # validate rows and columns for value statistics
  if(!is.null(stat_obj) && stat_obj$requires_values){
    validate_statistic_variables(stat_obj, rows_expanded, cols_expanded, data, dpdict)
  }

  # Create arrays for each specification
  row_arrays <- lapply(rows_expanded, function(spec) {
    array <- formula_to_array(spec, data)
    return(array)
  })

  col_arrays <- if (!is.null(cols_parsed)) {
    lapply(cols_expanded, function(spec) {
      if (spec$type == "total") {
        array <- rep(1, nrow(data))
      } else {
        array <- formula_to_array(spec, data)
      }
      return(array)
    })
  } else {
    array <- rep(1, nrow(data))
    list(array)
  }

  # Check for all-NA/zero arrays (indicating no valid data)
  if (all(sapply(row_arrays, function(x) all(x == 0 | is.na(x))))) {
    warning("All values are NA or zero for the row variable(s). Results may not be meaningful.")
  }

  if (length(col_arrays) > 1 && all(sapply(col_arrays, function(x) all(x == 0 | is.na(x))))) {
    warning("All values are NA or zero for the column variable(s). Results may not be meaningful.")
  }

  # Build the table
  n_rows <- length(row_arrays)
  n_cols <- length(col_arrays)

  # Initialize result matrix
  result_matrix <- matrix(NA, nrow = n_rows, ncol = n_cols)

  # Create values array if needed
  values_array <- NULL
  if (!is.null(values)) {
    if (!values %in% names(data)) {
      stop("Values variable '", values, "' not found in data")
    }
    values_array <- data[[values]]

    # Validate that values variable is numeric
    if (!is.numeric(values_array)) {
      stop("Values variable must be numeric for mean calculations")
    }
  }

  # Compute each cell
  for (i in seq_len(n_rows)) {
    for (j in seq_along(col_arrays)) {
      result_matrix[i, j] <- compute_cell(
        base_array = base_array,
        row_array = row_arrays[[i]],
        col_array = col_arrays[[j]],
        statistic = statistic,
        values = values_array
      )
    }
  }

  # Add summary rows/columns based on statistic metadata
  stat_obj <- if (is.character(statistic)) {
    .tab_registry$stats[[statistic_id]]
  } else {
    statistic
  }

  # Convert to data frame early
  result_df <- as.data.frame(result_matrix)

  # Add row labels
  row_labels <- vapply(rows_expanded, function(x) {
    if (!is.null(x$group_label) && x$group_label != x$label) {
      paste(x$group_label, x$label, sep = " - ")
    } else {
      x$label
    }
  }, character(1))

  result_df <- cbind(
    data.frame(row_label = row_labels, stringsAsFactors = FALSE),
    result_df
  )

  # Add column names
  col_labels <- vapply(cols_expanded, function(x) {
    if (!is.null(x$group_label) && x$group_label != x$label) {
      paste(x$group_label, x$label, sep = " - ")
    } else {
      x$label
    }
  }, character(1))

  names(result_df)[-1] <- col_labels

  # Calculate bases early for removal decisions
  row_bases <- numeric(length(row_arrays))
  col_bases <- numeric(length(col_arrays))

  # Calculate row bases (sum of base_array where row condition is true)
  for (i in seq_along(row_arrays)) {
    row_bases[i] <- sum(base_array * row_arrays[[i]])
  }

  # Calculate column bases (sum of base_array where column condition is true)
  for (j in seq_along(col_arrays)) {
    col_bases[j] <- sum(base_array * col_arrays[[j]])
  }

  # Determine base orientation from statistic
  base_orientation <- stat_obj$base_orientation %||% "column"

  # Remove low-base rows/columns based on threshold and orientation
  kept_row_indices <- seq_len(nrow(result_df))
  kept_col_indices <- seq_len(ncol(result_df) - 1)  # Excluding row_label column

  if (!is.null(low_base_threshold)) {
    if (base_orientation == "row") {
      # For row statistics, check row bases
      adequate_rows <- if (low_base_threshold == 0) {
        row_bases > 0
      } else {
        row_bases >= low_base_threshold
      }
      if (any(adequate_rows)) {
        kept_row_indices <- which(adequate_rows)
        result_df <- result_df[kept_row_indices, , drop = FALSE]
        row_arrays <- row_arrays[kept_row_indices]
        row_bases <- row_bases[kept_row_indices]
      } else {
        # All rows have inadequate base
        result_df <- result_df[0, , drop = FALSE]
        row_arrays <- list()
        row_bases <- numeric(0)
      }
    } else {
      # For column statistics (default), check column bases
      adequate_cols <- if (low_base_threshold == 0) {
        col_bases > 0
      } else {
        col_bases >= low_base_threshold
      }
      if (any(adequate_cols)) {
        cols_to_keep <- c(1, which(adequate_cols) + 1)
        result_df <- result_df[, cols_to_keep, drop = FALSE]
        kept_col_indices <- which(adequate_cols)
        col_arrays <- col_arrays[kept_col_indices]
        col_bases <- col_bases[kept_col_indices]
      } else {
        # All columns have inadequate base
        result_df <- result_df[, 1, drop = FALSE]
        col_arrays <- list()
        col_bases <- numeric(0)
      }
    }
  }

  # Continue with remaining logic based on remaining data
  summary_row_label <- NULL
  summary_row_array <- NULL

  # Add summary column if specified and we have multiple remaining columns
  summary_col_label <- NULL
  summary_col_array <- NULL

  # For column-oriented stats (column_pct), summary_col is NET of columns
  # For row-oriented stats (row_pct), summary_col is NET of rows
  show_summary_col <- if (stat_obj$base_orientation == "column") {
    show_col_nets
  } else {
    show_row_nets
  }

  if (length(col_arrays) > 1 && !is.null(stat_obj$summary_col) && show_summary_col) {
    summary_col_label <- stat_obj$summary_col
  }

  if (!is.null(summary_col_label)) {
    # Calculate summary column using the statistic's column calculator
    if (is.null(stat_obj$summary_col_calculator)) {
      stop("Statistic '", stat_obj$id, "' has summary_col='", summary_col_label,
           "' but no summary_col_calculator defined")
    }

    summary_col_array <- stat_obj$summary_col_calculator(
      arrays = col_arrays,
      base_array = base_array
    )

    # calculate the statistic for every row
    summary_values <- numeric(nrow(result_df))
    current_row_arrays <- row_arrays
    for (i in seq_len(nrow(result_df))) {
      summary_values[i] <- compute_cell(
        base_array = base_array,
        row_array = current_row_arrays[[i]],
        col_array  = summary_col_array,
        statistic  = statistic,
        values     = values_array
      )
    }

    # tack the column onto the table
    result_df[[summary_col_label]] <- summary_values
    col_arrays <- c(col_arrays, list(summary_col_array))
  }

  # Add summary row if specified, multiple rows exist and requested
  show_summary_row <- if (stat_obj$base_orientation == "column") {
    show_row_nets
  } else {
    show_col_nets
  }

  if (!is.null(stat_obj$summary_row) && length(row_arrays) > 1 && show_summary_row) {
    # Calculate summary row using the statistic's row calculator
    if (is.null(stat_obj$summary_row_calculator)) {
      stop("Statistic '", stat_obj$id, "' has summary_row='", stat_obj$summary_row,
           "' but no summary_row_calculator defined")
    }

    summary_row_array <- stat_obj$summary_row_calculator(
      arrays = row_arrays,
      base_array = base_array
    )

    # Calculate summary values for each column
    summary_values <- numeric(length(col_arrays))
    for (j in seq_along(col_arrays)) {
      summary_values[j] <- compute_cell(
        base_array = base_array,
        row_array = summary_row_array,
        col_array = col_arrays[[j]],
        statistic = statistic,
        values = values_array
      )
    }

    # Add summary row to result
    summary_row_df <- data.frame(
      row_label = stat_obj$summary_row,
      stringsAsFactors = FALSE
    )
    for (i in seq_along(summary_values)) {
      summary_row_df[[names(result_df)[i + 1]]] <- summary_values[i]
    }

    result_df <- rbind(result_df, summary_row_df)
    row_arrays <- c(row_arrays, list(summary_row_array))
  }

  # Add base information based on statistic orientation
  if (base_orientation == "row") {
    # For row statistics, add base as a column
    row_base_values <- numeric(nrow(result_df))
    for (i in seq_len(nrow(result_df))) {
      if (i <= length(row_bases)) {
        row_base_values[i] <- as.integer(row_bases[i])
      } else if (!is.null(summary_row_array)) {
        # For summary rows (NET), calculate their base
        base_val <- sum(base_array * summary_row_array)
        row_base_values[i] <- as.integer(base_val)
      } else {
        row_base_values[i] <- NA_real_
      }
    }
    result_df[[stat_obj$base_label]] <- row_base_values
    attr(result_df, "base_column") <- stat_obj$base_label
  } else {
    # For column statistics, add base as a row (current behavior)
    col_bases <- sapply(col_arrays, function(x) sum(base_array * x))

    base_row <- data.frame(
      row_label = stat_obj$base_label,
      stringsAsFactors = FALSE
    )

    for (i in seq_along(col_bases)) {
      base_row[[names(result_df)[i + 1]]] <- col_bases[i]
    }

    # Add base row
    result_df <- rbind(result_df, base_row)
  }

  # Store arrays for significance testing
  # Include base array as the last row array
  final_row_arrays <- c(row_arrays, list(base_array))
  final_col_arrays <- col_arrays

  # Store statistic
  attr(result_df, "statistic") <- statistic

  # Store values variable name if used
  if (!is.null(values)) {
    attr(result_df, "values_variable") <- values
  }

  # Store summary labels if used
  if (!is.null(stat_obj$summary_row) && length(row_arrays) > 1 && show_summary_row) {
    attr(result_df, "summary_row_label") <- stat_obj$summary_row
  }
  if (!is.null(summary_col_label)) {
    attr(result_df, "summary_col_label") <- summary_col_label
  }

  # Include summary column array if it was added
  final_col_arrays <- col_arrays
  if (!is.null(summary_col_label)) {
    final_col_arrays <- c(final_col_arrays, list(summary_col_array))
  }

  # Add base row array to match the base row that was added to result_df
  # Base row represents all eligible respondents (after weights/filters)
  final_row_arrays <- c(row_arrays, list(base_array))

  # Store arrays for later significance testing
  attr(result_df, "arrays") <- list(
    base_array = base_array,
    row_arrays = final_row_arrays,
    col_arrays = final_col_arrays,
    values_array = values_array
  )

  # Store the original call for sourcing when using copy_tab
  attr(result_df, "call") <- original_call

  class(result_df) <- c("tab_result", "data.frame")

  return(result_df)
}


#' Validate Variable Types for Statistics
#'
#' Validates that variables used in cross-tabulation rows and columns are
#' appropriate for the specified statistic. For value-based statistics like
#' mean, ensures variables are categorical rather than continuous numeric.
#'
#' @param stat_obj A tab_stat object containing statistic metadata
#' @param rows_expanded List of expanded row variable specifications
#' @param cols_expanded List of expanded column variable specifications
#' @param data Data frame being analyzed
#'
#' @return NULL if validation passes, throws error with helpful suggestions if not
#' @keywords internal
#'
#' @details
#' For statistics requiring values (like mean), this function prevents users from
#' accidentally creating meaningless cross-tabs with many numeric categories.
#' Variables with >15 unique values trigger errors with suggestions to use
#' factor(), cut(), add value labels, or use correlation statistics instead.
validate_statistic_variables <- function(stat_obj, rows_expanded, cols_expanded, data, dpdict = NULL) {
  if (stat_obj$requires_values) {
    # For value-based statistics (like mean), require categorical rows/columns

    # Check rows
    for (row_spec in rows_expanded) {
      if (row_spec$type == "simple") {
        var_name <- row_spec$components$var
        if (var_name %in% names(data)) {
          var_data <- data[[var_name]]

          # Check questiontype first if available
          if (!is.null(dpdict) && "questiontype" %in% names(dpdict) && var_name %in% dpdict$variable_names) {
            questiontype <- dpdict$questiontype[dpdict$variable_names == var_name]
            if (questiontype %in% c("numeric", "multinumeric")) {
              stop("Cannot use numeric variable '", var_name, "' (questiontype: ", questiontype, ") in rows for '", stat_obj$id, "' statistic")
            }
          } else {
            # Fallback to R class checking
            if (is.numeric(var_data) &&
                is.null(attr(var_data, "labels")) &&
                !is.factor(var_data) &&
                length(unique(na.omit(var_data))) > 15) {
              stop("Cannot use numeric variable '", var_name, "' with ", length(unique(na.omit(var_data))), " unique values in rows for '", stat_obj$id, "' statistic")
            }
          }
        }
      }
    }

    # Check columns (if not just "Total")
    if (length(cols_expanded) > 1 || cols_expanded[[1]]$type != "total") {
      for (col_spec in cols_expanded) {
        if (col_spec$type == "simple") {
          var_name <- col_spec$components$var
          if (var_name %in% names(data)) {
            var_data <- data[[var_name]]

            if (is.numeric(var_data) &&
                is.null(attr(var_data, "labels")) &&
                !is.factor(var_data) &&
                length(unique(na.omit(var_data))) > 15) {

              stop("Cannot use numeric variable '", var_name, ") in columns for '", stat_obj$id, "' statistic.\n")
            }
          }
        }
      }
    }
  }
}


#' Print method for tab_result
#' @export
print.tab_result <- function(x, ...) {
  statistic <- attr(x, "statistic")
  statistic_id  <- if (is.character(statistic)) statistic else statistic$id
  stat_obj <- if (is.character(statistic)) {
    .tab_registry$stats[[statistic]]
  } else {
    statistic
  }

  # Get values variable name if available
  values_var <- attr(x, "values_variable")

  # Get all significance results
  all_sig <- attr(x, "significance")

  # Construct header
  if (statistic_id == "mean" && !is.null(values_var)) {
    cat("\nCross-tabulation (mean of ", values_var, ")\n", sep = "")
  } else {
    cat("\nCross-tabulation (", statistic_id, ")\n", sep = "")
  }
  cat(rep("-", 50), "\n", sep = "")

  # Create column letter mapping
  col_names <- names(x)[-1]
  base_column <- attr(x, "base_column")

  # Create letters only for non-base columns
  non_base_cols <- if (!is.null(base_column)) {
    col_names[col_names != base_column]
  } else {
    col_names
  }

  col_letters <- LETTERS[seq_along(non_base_cols)]
  names(col_letters) <- non_base_cols

  # Create a copy for formatting to avoid modifying the original
  x_formatted        <- x
  base_row_idx <- which(x_formatted$row_label == stat_obj$base_label)

  for (col in names(x_formatted)[-1]) {
    orig <- x[[col]]
    col_idx <- which(names(x_formatted) == col) - 1  # Adjust for row_label column

    # Check if this column is the base column
    is_base_column <- !is.null(base_column) && col == base_column

    x_formatted[[col]] <- vapply(seq_along(orig), function(i) {
      if (i %in% base_row_idx || is_base_column) {
        as.character(orig[i])
      } else if (is.numeric(orig[i]) && !is.na(orig[i])) {
        formatted_val <- stat_obj$format_fn(orig[i])

        # Add significance indicators from all tests
        if (!is.null(all_sig)) {
          sig_indicators <- character()

          for (test_name in names(all_sig)) {
            sig_result <- all_sig[[test_name]]

            if (i <= nrow(sig_result$levels) && col_idx <= ncol(sig_result$levels)) {
              sig_level <- sig_result$levels[i, col_idx]

              if (sig_level == "higher") {
                # Find which column this test is comparing to
                versus_col <- sig_result$versus
                # Extract column letter
                if (grepl("column", versus_col)) {
                  # Extract column name from versus string
                  versus_col_name <- sub(".*: ", "", versus_col)
                  if (versus_col_name %in% names(col_letters)) {
                    sig_indicators <- c(sig_indicators,
                                        paste0(col_letters[versus_col_name], "+"))
                  }
                }
              } else if (sig_level == "lower") {
                versus_col <- sig_result$versus
                if (grepl("column", versus_col)) {
                  versus_col_name <- sub(".*: ", "", versus_col)
                  if (versus_col_name %in% names(col_letters)) {
                    sig_indicators <- c(sig_indicators,
                                        paste0(col_letters[versus_col_name], "-"))
                  }
                }
              }
            }
          }

          if (length(sig_indicators) > 0) {
            formatted_val <- paste0(formatted_val, " (",
                                    paste(sig_indicators, collapse = ","), ")")
          }
        }
        formatted_val
      } else {
        as.character(orig[i])
      }
    }, character(1))
  }

  # Get base column attribute if present
  base_column <- attr(x, "base_column")

  # Update column names to include letters in brackets (after formatting is complete)
  letter_index <- 1
  for (i in seq_along(col_names)) {
    col_name <- col_names[i]
    if (!is.null(base_column) && col_name == base_column) {
      # Don't add letter to base column
      new_col_name <- col_name
    } else {
      # Add letter for non-base columns
      new_col_name <- paste0(col_name, " (", LETTERS[letter_index], ")")
      letter_index <- letter_index + 1
    }
    names(x_formatted)[i + 1] <- new_col_name  # +1 to skip row_label column
  }

  print.data.frame(x_formatted, row.names = FALSE, ...)

  # Print significance testing legend if available
  if (!is.null(all_sig)) {
    cat("\nSignificance testing:\n")
    cat("Column letters: ", paste(col_letters, "=", names(col_letters), collapse = ", "), "\n")

    for (test_name in names(all_sig)) {
      sig_result <- all_sig[[test_name]]
      cat("\n", test_name, ":\n", sep = "")
      cat("  Test: ", sig_result$test_name, "\n")
      cat("  Level: ", sig_result$config$level, "\n")
      if (!is.null(sig_result$config$adjust) && sig_result$config$adjust != "none") {
        cat("  Adjustment: ", sig_result$config$adjust, "\n")
      }
    }

    cat("\nNotation: X+ = significantly higher than column X, X- = significantly lower than column X\n")
  }
}

#' Parse table formula expressions using NSE
#'
#' @param expr An expression captured using rlang::enquo
#' @param data The data frame to evaluate against
#' @param dpdict Optional data dictionary for metadata
#' @return A list with type, components, and label
#' @keywords internal
parse_table_formula <- function(expr, data, dpdict = NULL, helpers = NULL) {

  # If helpers not provided, use registry
  if (is.null(helpers)) {
    helpers <- .tab_registry$helpers
  }

  # Extract expression from quosure if needed
  if (rlang::is_quosure(expr)) {
    actual_expr <- rlang::quo_get_expr(expr)
  } else {
    actual_expr <- expr
  }

  # Check if it's a tab_helper object (evaluated helper function)
  if (inherits(actual_expr, "tab_helper")) {
    helper_type <- attr(actual_expr, "helper_type")
    label <- paste0(helper_type, "(...)")

    return(list(
      type = "helper",
      helper_type = helper_type,
      args = actual_expr,
      label = label
    ))
  }

  expr_text <- rlang::as_label(expr)

  # Handle simple variable names
  if (rlang::is_symbol(actual_expr)) {
    var_name <- as.character(actual_expr)
    return(list(
      type = "simple",
      components = list(var = var_name),
      label = get_var_label(var_name, dpdict)
    ))
  }

  # Handle multiplication (filters)
  if (rlang::is_call(actual_expr, "*")) {
    args <- rlang::call_args(actual_expr)
    return(list(
      type = "multiplication",
      components = lapply(args, function(x) parse_table_formula(rlang::enquo(x), data, dpdict)),
      label = expr_text
    ))
  }

  # Handle subtraction
  if (rlang::is_call(actual_expr, "-")) {
    args <- rlang::call_args(actual_expr)
    return(list(
      type = "subtraction",
      components = lapply(args, function(x) parse_table_formula(rlang::enquo(x), data, dpdict)),
      label = expr_text
    ))
  }

  # Handle other calls (including helper functions and comparisons)
  if (rlang::is_call(actual_expr)) {
    fn_name <- as.character(actual_expr[[1]])

    # Check if it's a helper function
    if (fn_name %in% names(helpers)) {
      return(list(
        type = "helper",
        helper_type = fn_name,
        args = rlang::call_args(actual_expr),
        label = expr_text
      ))
    }

    # Check if it's a numeric expression (arithmetic operators)
    if (fn_name %in% c("+", "-", "*", "/", "^", "%%", "%/%")) {
      return(list(
        type = "numeric_expression",
        components = list(expr = actual_expr),
        label = expr_text
      ))
    }

    # Otherwise treat as a general expression (like age > 30)
    return(list(
      type = "expression",
      components = list(expr = actual_expr),
      label = expr_text
    ))
  }

  # Handle string literals (for backward compatibility)
  if (rlang::is_string(actual_expr)) {
    return(list(
      type = "simple",
      components = list(var = actual_expr),
      label = get_var_label(actual_expr, dpdict)
    ))
  }

  # Default
  return(list(
    type = "expression",
    components = list(expr = actual_expr),
    label = expr_text
  ))
}

#' Get variable label from dpdict or use variable name
#' @keywords internal
get_var_label <- function(var_name, dpdict = NULL) {
  if (!is.null(dpdict) && var_name %in% dpdict$variable_names) {
    label <- dpdict$variable_labels[dpdict$variable_names == var_name]
    if (!is.na(label) && nzchar(label)) return(label)
  }
  return(var_name)
}

#' Get display label based on prefix_variable_label setting
#' @param var_name Variable name
#' @param dpdict Data dictionary
#' @param prefix_variable_label Whether to include variable prefix
#' @param category_name Category name for expanded variables (NULL for non-expanded)
#' @keywords internal
get_display_label <- function(var_name, dpdict = NULL, prefix_variable_label = TRUE, category_name = NULL) {
  if (prefix_variable_label) {
    base_label <- get_var_label(var_name, dpdict)
    if (!is.null(category_name)) {
      return(paste0(base_label, ": ", category_name))
    } else {
      return(base_label)
    }
  } else {
    if (!is.null(category_name)) {
      # For expanded variables, just return the category name
      return(category_name)
    } else {
      # For non-expanded variables, try to extract suffix
      full_label <- get_var_label(var_name, dpdict)
      separators <- c(" - ", ": ", " | ", " / ")

      for (sep in separators) {
        if (grepl(sep, full_label, fixed = TRUE)) {
          parts <- strsplit(full_label, sep, fixed = TRUE)[[1]]
          if (length(parts) > 1) {
            return(trimws(parts[length(parts)]))
          }
        }
      }

      # If no separator found, return full label
      return(full_label)
    }
  }
}

#' Expand variables and question groups into individual components
#'
#' @param var_spec Variable specification (can be name, expression, or formula)
#' @param data The data frame
#' @param dpdict Optional data dictionary
#' @param statistic_id The ID of the statistic being calculated
#' @return List of expanded variable specifications
#' @keywords internal
expand_variables <- function(var_spec, data, dpdict = NULL, statistic_id = NULL, values_var = NULL, prefix_variable_label = TRUE) {
  # Handle complex expressions by recursively expanding components
  if (is.list(var_spec) && !is.null(var_spec$type)) {

    # Helper functions should not be expanded
    if (var_spec$type == "helper") {
      return(list(var_spec))
    }

    if (var_spec$type == "multiplication") {
      # Expand each component and return all combinations
      expanded_components <- lapply(var_spec$components, function(comp) {
        expand_variables(comp, data, dpdict, statistic_id, values_var, prefix_variable_label)
      })

      # Create combinations of all expanded components
      result <- list()
      for (comp1 in expanded_components[[1]]) {
        for (comp2 in expanded_components[[2]]) {
          combined_spec <- list(
            type = "multiplication",
            components = list(comp1, comp2),
            label = paste(comp1$label, "*", comp2$label)
          )
          result <- append(result, list(combined_spec))
        }
      }
      return(result)
    }

    if (var_spec$type == "simple") {
      var_name <- var_spec$components$var
    } else {
      # For other complex types, return as-is
      return(list(var_spec))
    }
  } else {
    var_name <- as.character(var_spec)
  }

  if (is.na(var_name)) {
    warning("Attempting to expand NA variable name")
    return(list())
  }

  # Check if it's a categorical variable that needs expansion
  if (var_name %in% names(data)) {
    var_data <- data[[var_name]]

    # Priority 1: Use dpdict questiontype if available
    if (!is.null(dpdict) && "questiontype" %in% names(dpdict) && var_name %in% dpdict$variable_names) {
      questiontype <- dpdict$questiontype[dpdict$variable_names == var_name]

      if (questiontype %in% c("categorical", "categorical array")) {
        # Expand to categories using labels or factor levels
        labels <- attr(var_data, "labels")
        if (!is.null(labels) && length(labels) > 0) {
          return(lapply(seq_along(labels), function(i) {
            list(
              type = "expression",
              components = list(expr = call("==", as.name(var_name), labels[i])),
              label = get_display_label(var_name, dpdict, prefix_variable_label, names(labels)[i])
            )
          }))
        } else if (is.factor(var_data)) {
          levs <- levels(var_data)
          return(lapply(seq_along(levs), function(i) {
            list(
              type = "expression",
              components = list(expr = call("==", as.name(var_name), labels[i])),
              label = get_display_label(var_name, dpdict, prefix_variable_label, levs[i])
            )
          }))
        } else {
          stop("Variable '", var_name, "' has questiontype '", questiontype, "' but no labels or factor levels found")
        }
      } else if (questiontype %in% c("multiresponse", "numeric", "multinumeric")) {
        # Don't expand - return as single variable
        if (is.character(var_spec)) {
          return(list(list(
            type = "simple",
            components = list(var = var_spec),
            label = get_display_label(var_spec, dpdict, prefix_variable_label)
          )))
        } else {
          return(list(var_spec))
        }
      } else if (questiontype == "text") {
        stop("Cannot use text variable '", var_name, "' in cross-tabulation")
      }
    }

    # Priority 2: Fallback to R class-based logic
    # Check if it's labelled
    labels <- attr(var_data, "labels")
    if (!is.null(labels) && length(labels) > 0) {
      return(lapply(seq_along(labels), function(i) {
        list(
          type = "expression",
          components = list(expr = call("==", as.name(var_name), labels[i])),
          label = get_display_label(var_name, dpdict, prefix_variable_label, names(labels)[i])
        )
      }))
    }

    # Check if it's a factor
    if (is.factor(var_data)) {
      levs <- levels(var_data)
      return(lapply(levs, function(lev) {
        list(
          type = "expression",
          components = list(expr = call("==", as.name(var_name), lev)),
          label = get_display_label(var_name, dpdict, prefix_variable_label, lev)
        )
      }))
    }

    # Check if it's logical (treat as binary, don't expand)
    if (is.logical(var_data)) {
      if (is.character(var_spec)) {
        return(list(list(
          type = "simple",
          components = list(var = var_spec),
          label = get_display_label(var_spec, dpdict, prefix_variable_label)
        )))
      } else {
        return(list(var_spec))
      }
    }

    # Check if it's numeric (don't expand)
    if (is.numeric(var_data)) {
      if (is.character(var_spec)) {
        return(list(list(
          type = "simple",
          components = list(var = var_spec),
          label = get_display_label(var_spec, dpdict, prefix_variable_label)
        )))
      } else {
        return(list(var_spec))
      }
    }

    # Character variables should error
    if (is.character(var_data)) {
      stop("Cannot use character variable '", var_name, "' in cross-tabulation without labels")
    }
  }

  # Check if it's a question group in dpdict
  if (!is.null(dpdict) && "question_group" %in% names(dpdict) && !var_name %in% names(data)) {
    # Guard against NA var_name
    if (is.na(var_name)) {
      warning("Cannot expand NA variable name")
      return(list())
    }

    # First try exact match to question group name
    exact_match_vars <- dpdict$variable_names[
      !is.na(dpdict$question_group) &
        dpdict$question_group == var_name &
        !is.na(dpdict$variable_names)]
    if (length(exact_match_vars) > 0) {
      # Recursively expand each variable found in the question group
      all_expanded <- list()
      for (v in exact_match_vars) {
        expanded <- expand_variables(v, data, dpdict, statistic_id, values_var, prefix_variable_label)
        all_expanded <- append(all_expanded, expanded)
      }
      return(all_expanded)
    }

    # Then try pattern match
    matching_groups <- unique(dpdict$question_group[
      !is.na(dpdict$question_group) &
        grepl(paste0("^", var_name, "_"), dpdict$question_group)
    ])
    if (length(matching_groups) > 0) {
      group_vars <- dpdict$variable_names[dpdict$question_group %in% matching_groups & !is.na(dpdict$variable_names)]
      # Recursively expand each variable found in the matching groups
      all_expanded <- list()
      for (v in group_vars) {
        expanded <- expand_variables(v, data, dpdict, statistic_id, values_var, prefix_variable_label)
        all_expanded <- append(all_expanded, expanded)
      }
      return(all_expanded)
    }
  }

  # Check if it's a pattern match in data (only if NOT in data directly)
  if (!var_name %in% names(data)) {
    pattern_matches <- names(data)[grepl(paste0("^", var_name, "_\\d+"), names(data))]
    if (length(pattern_matches) > 0) {
      return(lapply(pattern_matches, function(v) {
        list(type = "simple", components = list(var = v), label = get_var_label(v, dpdict))
      }))
    }
  }

  # Default: return as single variable
  if (is.character(var_spec)) {
    return(list(list(
      type = "simple",
      components = list(var = var_spec),
      label = get_display_label(var_spec, dpdict, prefix_variable_label)
    )))
  } else {
    return(list(var_spec))
  }
}

#' Prepare data for expression evaluation by converting haven_labelled to numeric
#' @param data Data frame
#' @return Data frame with haven_labelled variables converted to numeric
#' @keywords internal
prepare_eval_data <- function(data) {
  # Convert haven_labelled to numeric, preserve everything else
  data[] <- lapply(data, function(x) {
    if (inherits(x, "haven_labelled")) {
      as.numeric(x)
    } else {
      x
    }
  })
  data
}

#' Convert formula specification to numeric array

#' Convert formula specification to numeric array
#'
#' @param formula_spec Parsed formula specification
#' @param data Data frame
#' @return Numeric vector of length nrow(data)
#' @keywords internal
formula_to_array <- function(formula_spec, data) {
  n <- nrow(data)

  # Start with identity array
  result <- rep(1, n)

  # Process based on type
  if (formula_spec$type == "simple") {
    var_name <- formula_spec$components$var
    if (!var_name %in% names(data)) {
      stop("Variable '", var_name, "' not found in data")
    }

    var_data <- data[[var_name]]

    # Convert to binary for categorical/logical
    if (is.logical(var_data)) {
      result <- result * as.numeric(var_data)
    } else if (is.numeric(var_data)) {
      # Check if it's binary (0/1)
      unique_vals <- unique(na.omit(var_data))
      if (all(unique_vals %in% c(0, 1))) {
        result <- result * var_data
      } else {
        # For non-binary numeric, keep as is (for means)
        result <- result * var_data
      }
    } else {
      stop("Unsupported variable type for '", var_name, "'")
    }

  } else if (formula_spec$type == "multiplication") {
    # Apply each component multiplicatively
    for (comp in formula_spec$components) {
      comp_array <- formula_to_array(comp, data)
      result <- result * comp_array
    }

  } else if (formula_spec$type == "expression") {
    # Evaluate the expression with numeric-only data
    eval_data <- prepare_eval_data(data)
    expr_result <- rlang::eval_tidy(formula_spec$components$expr, eval_data)
    if (!is.logical(expr_result) && !is.numeric(expr_result)) {
      stop("Expression must evaluate to logical or numeric")
    }
    result <- result * as.numeric(expr_result)

  } else if (formula_spec$type == "helper") {
    # Process helper functions
    result <- result * process_helper(formula_spec, data)

  } else if (formula_spec$type == "numeric_expression") {
    # Evaluate numeric expressions in data context with numeric-only data
    tryCatch({
      eval_data <- prepare_eval_data(data)
      expr_result <- rlang::eval_tidy(formula_spec$components$expr, eval_data)
      if (!is.numeric(expr_result)) {
        stop("Expression must evaluate to numeric values")
      }
      if (length(expr_result) != n) {
        stop("Expression must return vector of length ", n)
      }
      result <- result * expr_result
    }, error = function(e) {
      stop("Error evaluating expression '", formula_spec$label, "': ", e$message, call. = FALSE)
    })

  } else if (formula_spec$type == "subtraction") {
    # Handle subtraction
    comp1_array <- formula_to_array(formula_spec$components[[1]], data)
    comp2_array <- formula_to_array(formula_spec$components[[2]], data)
    result <- result * (comp1_array - comp2_array)
  }

  # Handle NAs
  result[is.na(result)] <- 0

  return(result)
}

#' Process helper functions
#' @keywords internal
process_helper <- function(formula_spec, data) {
  helper_type <- formula_spec$helper_type

  # Get the helper from registry
  helper_obj <- .tab_registry$helpers[[helper_type]]
  if (is.null(helper_obj)) {
    stop("Unknown helper: '", helper_type, "'. Available: ",
         paste(names(.tab_registry$helpers), collapse = ", "))
  }

  # Recursively evaluate arguments
  evaluated_args <- list()
  for (i in seq_along(formula_spec$args)) {
    arg <- formula_spec$args[[i]]

    # If argument is itself a helper or complex expression, evaluate it recursively
    if (rlang::is_call(arg)) {
      fn_name <- as.character(arg[[1]])
      if (fn_name %in% names(.tab_registry$helpers)) {
        # It's a nested helper - parse and evaluate recursively
        nested_spec <- list(
          type = "helper",
          helper_type = fn_name,
          args = rlang::call_args(arg),
          label = rlang::as_label(arg)
        )
        evaluated_args[[i]] <- process_helper(nested_spec, data)  # Recursive call
      } else {
        # It's a regular expression - evaluate in data context
        tryCatch({
          evaluated_args[[i]] <- rlang::eval_tidy(arg, data)
        }, error = function(e) {
          stop("Error evaluating argument ", i, " in helper '", helper_type, "': ", e$message, call. = FALSE)
        })
      }
    } else if (rlang::is_symbol(arg)) {
      # Variable name - convert to string instead of evaluating
      evaluated_args[[i]] <- as.character(arg)
    } else {
      # Simple argument - evaluate directly
      tryCatch({
        evaluated_args[[i]] <- rlang::eval_tidy(arg, data)
      }, error = function(e) {
        stop("Error evaluating argument ", i, " in helper '", helper_type, "': ", e$message, call. = FALSE)
      })
    }
  }

  # Create formula_spec with evaluated components for the processor
  processed_spec <- list(
    type = "helper",
    helper_type = helper_type,
    components = evaluated_args,
    label = formula_spec$label
  )

  # Dispatch to the helper's processor
  tryCatch({
    result <- helper_obj$processor(processed_spec, data)
    if (!is.numeric(result) || length(result) != nrow(data)) {
      stop("Helper '", helper_type, "' returned invalid result. Expected numeric vector of length ", nrow(data))
    }
    return(result)
  }, error = function(e) {
    stop("Error in helper '", helper_type, "': ", e$message, call. = FALSE)
  })
}

#' Create named list of row specifications
#'
#' @param ... Named expressions for row specifications
#' @return List for use in tab()
#' @export
#' @examples
#' tab(data, rows = rows_list("Total" = q1, "Young" = q1 * (age < 30)))
rows_list <- function(...) {
  dots <- rlang::enquos(...)
  if (length(dots) == 0) {
    stop("rows_list() needs at least one expression")
    }
  if (is.null(names(dots))) names(dots) <- rep("", length(dots))
  dots
}

#' Create named list of column specifications
#'
#' @param ... Named expressions for column specifications
#' @return List for use in tab()
#' @export
#' @examples
#' tab(data, gender, cols = cols_list("Total" = q1, "Young" = q1 * (age < 30)))
cols_list <- function(...) {
  dots <- rlang::enquos(...)
  if (length(dots) == 0) {
    stop("cols_list() needs at least one expression")
  }
  if (is.null(names(dots))) names(dots) <- rep("", length(dots))
  dots
}

#' Compute value for a single cell
#'
#' @param row_array Numeric array for row specification
#' @param col_array Numeric array for column specification (NULL for marginals)
#' @param statistic Type of statistic to compute
#' @param values Optional values array for mean calculations
#' @return Computed cell value
#' @keywords internal
compute_cell <- function(base_array, row_array, col_array,
                         statistic = "column_pct",
                         values = NULL) {

  if(!length(base_array) == length(row_array) & length(base_array) == length(col_array)){
    stop("Must provide base_array, row_array and col_array each of equal length. (Provide array of 1s if no other calculation needed.")
  }

  # Convert string statistic to object if needed
  if (is.character(statistic)) {
    stat_obj <- .tab_registry$stats[[statistic]]
    if (is.null(stat_obj)) {
      stop("Unknown statistic: '", statistic, "'. Available: ",
           paste(names(.tab_registry$stats), collapse = ", "))
    }
    statistic <- stat_obj
  }

  # Dispatch to the statistic's processor
  if (inherits(statistic, "tab_stat")) {
    return(statistic$processor(
      base_array = base_array,
      row_array = row_array,
      col_array = col_array,
      values = values
    ))
  } else {
    stop("statistic must be a character string or tab_stat object")
  }
}


#' Copy tab results to clipboard for pasting into Sheets
#'
#' Copies a tab_result object to the clipboard in a format suitable for pasting
#' into Sheets. Percentages are converted to decimals, base sizes are
#' included, and source information is added.
#'
#' @param tab_result A tab_result object from the tab() function
#' @param digits Number of decimal places to round values to. If NULL (default), no rounding is applied
#' @param empty_zeros Logical. If TRUE, cells with exactly 0 values will be displayed as empty instead of "0"
#' @param na_display Character string to display for NA values. Default is empty string ("")
#' @param show_source Logical. If TRUE (default), includes footnote on source.
#' @return Invisibly returns the formatted data that was copied
#' @export
#'
#' @examples
#' \dontrun{
#' result <- tab(data, gender, region)
#' copy_tab(result)
#' # Now paste into Sheets
#' }
copy_tab <- function(tab_result, digits = NULL, empty_zeros = FALSE, na_display = "", show_source = TRUE) {

  # Validate input
  if (!inherits(tab_result, "tab_result")) {
    stop("Input must be a tab_result object from the tab() function")
  }

  # Check if clipr is available
  if (!requireNamespace("clipr", quietly = TRUE)) {
    stop("The 'clipr' package is required for clipboard functionality. ",
         "Please install it with: install.packages('clipr')")
  }

  # Check if clipboard is available
  if (!clipr::clipr_available()) {
    stop("Clipboard is not available. This may occur in non-interactive sessions ",
         "or on systems without clipboard support.")
  }

  # Get call information
  original_call <- attr(tab_result, "call")

  # Get the statistic type
  statistic <- attr(tab_result, "statistic")
  statistic_id <- if (is.character(statistic)) {
    statistic
  } else if (inherits(statistic, "tab_stat")) {
    statistic$id
  } else {
    "unknown"
  }

  # Create a copy of the data for processing
  output_data <- tab_result

  x_formatted <- output_data
  x           <- output_data

  # Apply statistic-specific formatting
  stat_obj <- if (is.character(statistic)) {
    .tab_registry$stats[[statistic_id]]
  } else {
    statistic
  }

  # Identify the base row
  base_row_idx <- which(x_formatted$row_label == stat_obj$base_label)

  # Apply formatting function to all non-base rows
  # Apply formatting function to all non-base rows
  for (col in names(output_data)[-1]) {
    # Get base column name if present
    base_column <- attr(tab_result, "base_column")

    # Skip formatting for base column
    if (!is.null(base_column) && col == base_column) {
      next
    }

    original_values <- output_data[[col]]
    output_data[[col]] <- vapply(
      seq_along(original_values), function(i) {
        if (i %in% base_row_idx) {
          # Handle base row values
          val <- original_values[i]
          if (is.na(val)) {
            return(na_display)
          } else if (empty_zeros && val == 0) {
            return("")
          } else if (!is.null(digits)) {
            # Round base values too, format as integer if digits = 0
            rounded_val <- round(val, digits)
            if (digits == 0) {
              return(as.character(as.integer(rounded_val)))
            } else {
              return(as.character(rounded_val))
            }
          } else {
            return(as.character(val))
          }
        } else if (is.numeric(original_values[i])) {
          val <- original_values[i]
          if (is.na(val)) {
            return(na_display)
          } else if (empty_zeros && val == 0) {
            return("")
          } else {
            # Handle custom formatting or use statistic's format function
            if (!is.null(digits)) {
              rounded_val <- round(val, digits)
              if (digits == 0) {
                return(as.character(as.integer(rounded_val)))
              } else {
                # Format without trailing zeros
                return(as.character(rounded_val))
              }
            } else {
              return(stat_obj$format_fn(val))
            }
          }
        } else {
          return(as.character(original_values[i]))
        }
      }, character(1))
  }

  # Create column headers row
  headers_row <- data.frame(
    row_label = "Row Label",
    stringsAsFactors = FALSE
  )
  for (col_name in names(output_data)[-1]) {
    headers_row[[col_name]] <- col_name
  }

  # Combine headers with data (base row is already included in tab_result)
  output_data <- rbind(headers_row, output_data)

  if(show_source){
    # Add empty row for spacing
    empty_row <- output_data[1, ]
    empty_row[1, ] <- ""
    output_data <- rbind(output_data, empty_row)

    # Add source information row with original call
    if (statistic_id == "mean" && !is.null(attr(tab_result, "values_variable"))) {
      values_var <- attr(tab_result, "values_variable")
      call_text <- deparse(original_call, width.cutoff = 500)
      source_info <- paste0("Table showing mean of '", values_var, "' generated by surveydatar::", paste(call_text, collapse = " "), ")")
    } else {
      call_text <- deparse(original_call, width.cutoff = 500)
      source_info <- paste0("Table showing survey ", statistic_id, " data generated by surveydatar::", paste(call_text, collapse = " "), ")")
    }

    source_row <- output_data[1, ]
    source_row[1, ] <- source_info
    source_row[, -1] <- ""
    output_data <- rbind(output_data, source_row)
  }

  # Convert to character matrix for clipboard
  output_matrix <- as.matrix(output_data)

  # Create tab-delimited string
  output_lines <- apply(output_matrix, 1, function(row) {
    paste(row, collapse = "\t")
  })
  output_string <- paste(output_lines, collapse = "\n")

  # Copy to clipboard
  tryCatch({
    clipr::write_clip(output_string)
    message("Table copied to clipboard. Ready to paste into Sheets.")
  }, error = function(e) {
    stop("Failed to copy to clipboard: ", e$message)
  })

  # Return the formatted data invisibly
  invisible(output_data)
}

#' Calculate summary value based on type
#' @keywords internal
calculate_summary <- function(type, arrays, base_array, col_array, statistic, values) {
  if (type == "NET") {
    # Union of all conditions
    combined_matrix <- do.call(cbind, arrays)
    summary_array <- as.numeric(rowSums(combined_matrix) > 0)
  } else if (type == "Avg") {
    # Union for averaging (same as NET but labeled differently)
    combined_matrix <- do.call(cbind, arrays)
    summary_array <- as.numeric(rowSums(combined_matrix) > 0)
  } else if (type == "Total") {
    # Simple sum
    summary_array <- rep(1, length(base_array))
  } else {
    stop("Unknown summary type: ", type)
  }

  return(summary_array)
}

#' Add significance testing to a tab result
#'
#' @param tab_result A tab_result object from tab()
#' @param versus Column to compare against: "first_col", "last_col", "total",
#'   column name, or column index
#' @param test Test to use: "auto", "z_test_proportions", "t_test", etc.
#' @param level Significance level (default 0.05)
#' @param adjust Multiple comparison adjustment: "none", "bonferroni", "BH", etc.
#' @param name Optional name for this comparison (defaults to versus value)
#' @return The tab_result with significance testing added
#' @export
add_sig <- function(tab_result,
                    versus = "first_col",
                    test = "auto",
                    level = 0.05,
                    adjust = "none",
                    name = NULL) {

  if (!inherits(tab_result, "tab_result")) {
    stop("Input must be a tab_result object")
  }

  # Extract stored arrays
  arrays <- attr(tab_result, "arrays")
  if (is.null(arrays)) {
    stop("No arrays found in tab_result. This tab_result may have been created ",
         "with an older version of tab() that doesn't store arrays.")
  }

  # Extract other needed info
  statistic <- attr(tab_result, "statistic")

  # Get stat object for base row identification
  stat_obj <- if (is.character(statistic)) {
    .tab_registry$stats[[statistic$id]]
  } else {
    statistic
  }

  # Create result matrix (excluding row_label column)
  result_matrix <- as.matrix(tab_result[, -1])

  # Create sig_config
  sig_config <- list(
    test = test,
    versus = versus,
    level = level,
    adjust = adjust
  )

  # Call compute_significance
  sig_result <- compute_significance(
    base_array = arrays$base_array,
    row_arrays = arrays$row_arrays,
    col_arrays = arrays$col_arrays,
    result_matrix = result_matrix,
    statistic = statistic,
    values_array = arrays$values_array,
    sig_config = sig_config,
    row_labels = tab_result$row_label
  )

  # Determine name for this comparison
  # Resolve versus to actual column name
  target_col_name <- resolve_versus_to_column_name(versus, tab_result)

  # Use provided name or default to target column name
  if (is.null(name)) {
    name <- target_col_name
  }

  # Get existing significance results
  all_sig <- attr(tab_result, "significance")
  if (is.null(all_sig)) {
    all_sig <- list()
  }

  # Add new significance result
  all_sig[[name]] <- sig_result
  attr(tab_result, "significance") <- all_sig

  return(tab_result)
}

#' Add significance testing versus all columns
#'
#' @param tab_result A tab_result object from tab()
#' @param test Test to use for all comparisons
#' @param level Significance level
#' @param adjust Multiple comparison adjustment
#' @param exclude Columns to exclude from testing (e.g., "Total")
#' @return The tab_result with significance testing added for all columns
#' @export
add_sig_all <- function(tab_result,
                        test = "auto",
                        level = 0.05,
                        adjust = "none",
                        exclude = "Total") {

  if (!inherits(tab_result, "tab_result")) {
    stop("Input must be a tab_result object")
  }

  # Get all column names except row_label
  col_names <- names(tab_result)[-1]

  # Exclude specified columns
  if (!is.null(exclude)) {
    col_names <- setdiff(col_names, exclude)
  }

  # Add significance vs each column
  for (col in col_names) {
    tab_result <- add_sig(tab_result,
                          versus = col,
                          test = test,
                          level = level,
                          adjust = adjust)
  }

  return(tab_result)
}

#' Compute significance matrix for tab results
#' @keywords internal
compute_significance <- function(base_array, row_arrays, col_arrays,
                                 result_matrix, statistic, values_array = NULL,
                                 sig_config = list(), row_labels = NULL) {

  # Default configuration
  default_config <- list(
    test = "auto",
    versus = "first_col",
    level = 0.05,
    adjust = "none"
  )
  config <- utils::modifyList(default_config, sig_config)

  # Get stat object
  stat_obj <- if (is.character(statistic)) {
    .tab_registry$stats[[statistic$id]]
  } else {
    statistic
  }

  # Determine test to use
  if (config$test == "auto") {
    # Auto-determine based on statistic type
    test_id <- switch(
      stat_obj$id,
      "column_pct" = "z_test_proportions",
      "count" = "z_test_proportions",
      "row_pct" = "z_test_proportions",
      "mean" = "t_test",
      "median" = "mann_whitney",
      "sd" = "t_test",
      "cv" = "t_test",
      stop("No default test for statistic '", stat_obj$id, "'")
    )
  } else {
    test_id <- config$test
  }

  # Get test object
  test_obj <- .tab_registry$sig_tests[[test_id]]
  if (is.null(test_obj)) {
    stop("Unknown significance test: '", test_id, "'. Available: ",
         paste(names(.tab_registry$sig_tests), collapse = ", "))
  }

  # Determine comparison base column index
  n_cols <- ncol(result_matrix)
  col_names <- colnames(result_matrix)

  if (is.character(config$versus)) {
    base_col <- switch(config$versus,
                       "first_col" = 1,
                       "last_col" = n_cols,
                       "total" = {
                         idx <- which(tolower(col_names) == "total")
                         if (length(idx) == 0) stop("No 'Total' column found")
                         idx[1]
                       },
                       # Otherwise treat as column name
                       {
                         idx <- which(col_names == config$versus)
                         if (length(idx) == 0) stop("Column '", config$versus, "' not found")
                         idx[1]
                       }
    )
  } else if (is.numeric(config$versus)) {
    base_col <- as.integer(config$versus)
    if (base_col < 1 || base_col > n_cols) {
      stop("Column index ", base_col, " out of range [1, ", n_cols, "]")
    }
  } else {
    stop("versus must be a character string or numeric index")
  }

  # Identify data rows (exclude base row and any summary rows)
  base_row_idx <- which(row_labels %in% c(stat_obj$base_label, "Base (n)"))
  summary_row_idx <- which(rownames(result_matrix) %in% c("NET", "Total", "Avg"))
  exclude_rows <- union(base_row_idx, summary_row_idx)

  # Determine actual data rows
  all_rows <- seq_len(nrow(result_matrix))
  data_rows <- setdiff(all_rows, exclude_rows)

  # Initialize matrices for data rows only
  n_data_rows <- length(data_rows)
  sig_matrix <- matrix("", nrow = n_data_rows, ncol = n_cols)
  p_matrix <- matrix(NA_real_, nrow = n_data_rows, ncol = n_cols)

  # Set row and column names
  if (length(data_rows) > 0) {
    if (!is.null(row_labels)) {
      rownames(sig_matrix) <- row_labels[data_rows]
      rownames(p_matrix) <- row_labels[data_rows]
    } else {
      rownames(sig_matrix) <- rownames(result_matrix)[data_rows]
      rownames(p_matrix) <- rownames(result_matrix)[data_rows]
    }
  }
  colnames(sig_matrix) <- col_names
  colnames(p_matrix) <- col_names

  # Skip if only one column
  if (n_cols == 1) {
    return(list(
      levels = sig_matrix,
      p_values = p_matrix,
      test_used = test_id,
      test_name = test_obj$name,
      versus = "none",
      config = config
    ))
  }

  # Check if test is omnibus (tests overall association)
  if (!is.null(test_obj$is_omnibus) && test_obj$is_omnibus) {
    # For omnibus tests, we need arrays for data rows only
    data_row_arrays <- row_arrays[data_rows]

    # Run omnibus test once
    test_result <- test_obj$processor(
      base_array = base_array,
      row_arrays = data_row_arrays,
      col_arrays = col_arrays,
      values = values_array
    )

    # For omnibus tests, store the overall p-value
    if (!is.null(test_result$p_value)) {
      # Set all cells to the same p-value (it's an overall test)
      p_matrix[,] <- test_result$p_value

      # Mark all cells as "omnibus" in sig_matrix
      sig_matrix[,] <- if (test_result$p_value < config$level) "significant" else ""
    }

    return(list(
      levels = sig_matrix,
      p_values = p_matrix,
      test_used = test_id,
      test_name = test_obj$name,
      versus = "overall association",
      config = config,
      is_omnibus = TRUE
    ))
  }

  # Perform pairwise tests
  base_col_array <- col_arrays[[base_col]]

  # Test each data row against the base column
  for (i in seq_along(data_rows)) {
    row_idx <- data_rows[i]

    array_idx <- i  # The i-th data row uses the i-th row array

    for (j in 1:n_cols) {
      if (j == base_col) {
        sig_matrix[i, j] <- "base"
        next
      }

      # Run test
      tryCatch({
        test_result <- test_obj$processor(
          base_array = base_array,
          row_array = row_arrays[[array_idx]],  # <-- CORRECT INDEX
          col_array_1 = base_col_array,
          col_array_2 = col_arrays[[j]],
          values = values_array
        )

        p_matrix[i, j] <- test_result$p_value
      }, error = function(e) {
        # If test fails, leave as NA
        warning("Test failed for row ", i, ", col ", j, ": ", e$message)
      })
    }
  }

  # Apply multiple comparison adjustment if requested
  if (config$adjust != "none" && any(!is.na(p_matrix))) {
    # Extract non-base p-values
    p_values_vec <- as.vector(p_matrix[, -base_col])
    p_values_vec <- p_values_vec[!is.na(p_values_vec)]

    if (length(p_values_vec) > 0) {
      p_adjusted <- p.adjust(p_values_vec, method = config$adjust)

      # Put adjusted values back
      idx <- 1
      for (j in 1:n_cols) {
        if (j != base_col) {
          for (i in 1:n_data_rows) {
            if (!is.na(p_matrix[i, j])) {
              p_matrix[i, j] <- p_adjusted[idx]
              idx <- idx + 1
            }
          }
        }
      }
    }
  }

  # Convert p-values to significance levels
  for (i in 1:n_data_rows) {
    row_idx <- data_rows[i]

    for (j in 1:n_cols) {
      if (sig_matrix[i, j] == "base") {
        next
      } else if (!is.na(p_matrix[i, j]) && p_matrix[i, j] < config$level) {
        # Determine direction based on values
        val_current <- result_matrix[row_idx, j]
        val_base <- result_matrix[row_idx, base_col]

        if (!is.na(val_current) && !is.na(val_base)) {
          if (val_current > val_base) {
            sig_matrix[i, j] <- "higher"
          } else if (val_current < val_base) {
            sig_matrix[i, j] <- "lower"
          }
        }
      }
    }
  }

  return(list(
    levels = sig_matrix,
    p_values = p_matrix,
    test_used = test_id,
    test_name = test_obj$name,
    versus = paste0("column ", base_col, ": ", col_names[base_col]),
    config = config
  ))
}

#' Resolve versus parameter to actual column name
#' @keywords internal
resolve_versus_to_column_name <- function(versus, tab_result) {
  col_names <- names(tab_result)[-1]  # Exclude row_label
  n_cols <- length(col_names)

  if (is.character(versus)) {
    target_col_idx <- switch(versus,
                             "first_col" = 1,
                             "last_col" = n_cols,
                             "total" = {
                               idx <- which(tolower(col_names) == "total")
                               if (length(idx) == 0) stop("No 'Total' column found")
                               idx[1]
                             },
                             # Otherwise treat as column name
                             {
                               idx <- which(col_names == versus)
                               if (length(idx) == 0) stop("Column '", versus, "' not found")
                               idx[1]
                             }
    )
  } else if (is.numeric(versus)) {
    target_col_idx <- as.integer(versus)
    if (target_col_idx < 1 || target_col_idx > n_cols) {
      stop("Column index ", target_col_idx, " out of range [1, ", n_cols, "]")
    }
  } else {
    stop("versus must be a character string or numeric index")
  }

  return(col_names[target_col_idx])
}
