# ! TO DOs:
# - Allow for no row parameter if a value is given (calculate value on total sample)
# - Implement sig testing and correlations (will require significant work as doesn't fit neatly into current framework)
# - Visualisation via Flourish


#' Create cross-tabulation tables with flexible formula syntax
#'
#' @param data Data frame or survey_data object
#' @param rows Row specification (variable name, expression, or named list)
#' @param cols Column specification (optional)
#' @param filter Table-wide filter expression (optional)
#' @param weight Weight variable name (optional)
#' @param statistic Type of statistic: "column_pct", "count", "row_pct", "mean"
#' @param values Variable name to aggregate for value-based statistics like mean (optional)
#' @param show_column_net Whether to show column NET/Avg
#' @param show_column_total Whether to show a “Total” column
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
                show_column_net = TRUE, show_column_total = TRUE,
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
        parsed$label <- names(rows_eval)[i]
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
          parsed$label <- names(cols_eval)[i]
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
    expanded <- expand_variables(row_spec, data, dpdict, statistic_id)
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
      expanded <- expand_variables(col_spec, data, dpdict, statistic_id)
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
    for (j in seq_len(n_cols)) {
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

  # Add summary row if specified, multiple rows exist and requested
  if (!is.null(stat_obj$summary_row) && length(row_arrays) > 1 && show_column_net == TRUE) {
    # Calculate summary array based on summary type
    if (stat_obj$summary_row %in% c("NET", "Avg")) {
      # Union of all row conditions
      combined_row_matrix <- do.call(cbind, row_arrays)
      summary_row_array <- as.numeric(rowSums(combined_row_matrix) > 0)
    } else if (stat_obj$summary_row == "Total") {
      # Simple total
      summary_row_array <- rep(1, nrow(data))
    } else {
      # Custom summary types can be added here
      stop("Unknown summary row type: ", stat_obj$summary_row)
    }

    # Calculate summary values for each column
    summary_values <- numeric(n_cols)
    for (j in seq_len(n_cols)) {
      summary_values[j] <- compute_cell(
        base_array = base_array,
        row_array = summary_row_array,
        col_array = col_arrays[[j]],
        statistic = statistic,
        values = values_array
      )
    }

    # Add summary row to result matrix
    result_matrix <- rbind(result_matrix, summary_values)
    n_rows <- n_rows + 1
    row_arrays <- c(row_arrays, list(summary_row_array))
  }

  # Add summary column if specified and multiple columns exist
  summary_col_label <- NULL
  if (length(col_arrays) > 1) {
    if (!is.null(stat_obj$summary_col)) {
      summary_col_label <- stat_obj$summary_col
    } else if (show_column_total) {
      summary_col_label <- "Total"
    }
  }

  if (!is.null(summary_col_label)) {

    # work out which rows belong to the summary column
    if (summary_col_label %in% c("NET", "Avg")) {
      combined_col_matrix <- do.call(cbind, col_arrays)
      summary_col_array   <- as.numeric(rowSums(combined_col_matrix) > 0)
    } else {                         # "Total"
      summary_col_array   <- rep(1, nrow(data))
    }

    # calculate the statistic for every row
    summary_values <- numeric(n_rows)
    for (i in seq_len(n_rows)) {
      summary_values[i] <- compute_cell(
        base_array = base_array,
        row_array  = row_arrays[[i]],
        col_array  = summary_col_array,
        statistic  = statistic,
        values     = values_array
      )
    }

    # tack the column onto the table
    result_matrix <- cbind(result_matrix, summary_values)
    n_cols        <- n_cols + 1
  }

  result_df <- as.data.frame(result_matrix)

  # Add row labels
  row_labels <- sapply(rows_expanded, function(x) {
    if (!is.null(x$group_label)) {
      paste(x$group_label, x$label, sep = " - ")  # BOTH group AND individual
    } else {
      x$label  # Just individual when no group
    }
  })

  # Add summary row label if applicable
  if (!is.null(stat_obj$summary_row) && length(row_arrays) > 1 && show_column_net == TRUE) {
    row_labels <- c(row_labels, stat_obj$summary_row)
  }

  result_df <- cbind(
    data.frame(row_label = row_labels, stringsAsFactors = FALSE),
    result_df
  )

  # Add column names
  col_labels <- if (!is.null(cols_parsed)) {
    sapply(cols_expanded, function(x) x$label)
  } else {
    "Total"
  }
  if (!is.null(summary_col_label))
    col_labels <- c(col_labels, summary_col_label)

  names(result_df)[-1] <- col_labels

  # Add base sizes as a row in the table
  col_bases <- sapply(col_arrays, function(x) sum(base_array * x))
  if (!is.null(summary_col_label)) {
    # reuse summary_col_array computed above
    summary_col_base <- sum(base_array * summary_col_array)
    col_bases        <- c(col_bases, summary_col_base)
  }

  base_row <- data.frame(
    row_label = "Base (n)",
    stringsAsFactors = FALSE
  )

  # Add summary column base if applicable
  if (!is.null(stat_obj$summary_col) && length(col_arrays) > 1) {
    if (stat_obj$summary_col %in% c("NET", "Avg")) {
      combined_col_matrix <- do.call(cbind, col_arrays)
      summary_col_array <- as.numeric(rowSums(combined_col_matrix) > 0)
    } else if (stat_obj$summary_col == "Total") {
      summary_col_array <- rep(1, nrow(data))
    }
    summary_col_base <- sum(base_array * summary_col_array)
    col_bases <- c(col_bases, summary_col_base)
  }

  base_row <- data.frame(
    row_label = stat_obj$base_label,
    stringsAsFactors = FALSE
  )

  # Handle empty data case
  if (nrow(data) == 0 || length(col_bases) == 0) {
    # For empty data, create a minimal base row with NA values
    for (col_name in names(result_df)[-1]) {
      base_row[[col_name]] <- 0
    }
  } else {
    # Add base sizes for each column (normal case)
    for (i in seq_along(col_bases)) {
      if (i + 1 <= ncol(result_df)) {  # Extra safety check
        base_row[[names(result_df)[i + 1]]] <- col_bases[i]
      }
    }
  }

  # Add base row to the result
  result_df <- rbind(result_df, base_row)

  # Store statistic
  attr(result_df, "statistic") <- statistic


  # Store values variable name if used
  if (!is.null(values)) {
    attr(result_df, "values_variable") <- values
  }

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

  # Construct header
  if (statistic_id == "mean" && !is.null(values_var)) {
    cat("\nCross-tabulation (mean of ", values_var, ")\n", sep = "")
  } else {
    cat("\nCross-tabulation (", statistic_id, ")\n", sep = "")
  }
  cat(rep("-", 50), "\n", sep = "")

  # Create a copy for formatting to avoid modifying the original
  x_formatted        <- x
  base_row_idx <- which(x_formatted$row_label == stat_obj$base_label)

  for (col in names(x_formatted)[-1]) {
    orig <- x[[col]]
    x_formatted[[col]] <- vapply(seq_along(orig), function(i) {
      if (i %in% base_row_idx) as.character(orig[i])
      else if (is.numeric(orig[i]) && !is.na(orig[i])) stat_obj$format_fn(orig[i])
      else as.character(orig[i])
    }, character(1))
  }

  print.data.frame(x_formatted, row.names = FALSE, ...)
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


#' Expand variables and question groups into individual components
#'
#' @param var_spec Variable specification (can be name, expression, or formula)
#' @param data The data frame
#' @param dpdict Optional data dictionary
#' @param statistic_id The ID of the statistic being calculated
#' @return List of expanded variable specifications
#' @keywords internal
expand_variables <- function(var_spec, data, dpdict = NULL, statistic_id = NULL, values_var = NULL) {
  # Handle complex expressions by recursively expanding components
  if (is.list(var_spec) && !is.null(var_spec$type)) {

    # Helper functions should not be expanded
    if (var_spec$type == "helper") {
      return(list(var_spec))
    }

    if (var_spec$type == "multiplication") {
      # Expand each component and return all combinations
      expanded_components <- lapply(var_spec$components, function(comp) {
        expand_variables(comp, data, dpdict, statistic_id)
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

  # Check if it's a question group in dpdict (existing logic)
  if (!is.null(dpdict) && "question_group" %in% names(dpdict)) {
    matching_groups <- unique(dpdict$question_group[grepl(paste0("^", var_name, "_"), dpdict$question_group)])
    if (length(matching_groups) > 0) {
      group_vars <- dpdict$variable_names[dpdict$question_group %in% matching_groups]
      return(lapply(group_vars, function(v) {
        list(type = "simple", components = list(var = v), label = get_var_label(v, dpdict))
      }))
    }
  }

  # Check if it's a pattern match in data
  pattern_matches <- names(data)[grepl(paste0("^", var_name, "_\\d+"), names(data))]
  if (length(pattern_matches) > 0) {
    return(lapply(pattern_matches, function(v) {
      list(type = "simple", components = list(var = v), label = get_var_label(v, dpdict))
    }))
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
              label = paste0(get_var_label(var_name, dpdict), ": ", names(labels)[i])
            )
          }))
        } else if (is.factor(var_data)) {
          levs <- levels(var_data)
          return(lapply(levs, function(lev) {
            list(
              type = "expression",
              components = list(expr = call("==", as.name(var_name), lev)),
              label = paste0(get_var_label(var_name, dpdict), ": ", lev)
            )
          }))
        } else {
          stop("Variable '", var_name, "' has questiontype '", questiontype, "' but no labels or factor levels found")
        }
      } else if (questiontype %in% c("multiresponse", "numeric", "multinumeric")) {
        # Don't expand - return as single variable
        return(list(var_spec))
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
          label = paste0(get_var_label(var_name, dpdict), ": ", names(labels)[i])
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
          label = paste0(get_var_label(var_name, dpdict), ": ", lev)
        )
      }))
    }

    # Check if it's logical (treat as binary, don't expand)
    if (is.logical(var_data)) {
      return(list(var_spec))
    }

    # Check if it's numeric (don't expand)
    if (is.numeric(var_data)) {
      return(list(var_spec))
    }

    # Character variables should error
    if (is.character(var_data)) {
      stop("Cannot use character variable '", var_name, "' in cross-tabulation without labels")
    }
  }

  # Default: return as single variable
  return(list(var_spec))
}

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
    # Evaluate the expression
    expr_result <- rlang::eval_tidy(formula_spec$components$expr, data)
    if (!is.logical(expr_result) && !is.numeric(expr_result)) {
      stop("Expression must evaluate to logical or numeric")
    }
    result <- result * as.numeric(expr_result)

  } else if (formula_spec$type == "helper") {
    # Process helper functions
    result <- result * process_helper(formula_spec, data)

  } else if (formula_spec$type == "numeric_expression") {
    # Evaluate numeric expressions in data context
    tryCatch({
      expr_result <- rlang::eval_tidy(formula_spec$components$expr, data)
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
  if (length(names(dots)) == 0 || any(names(dots) == "")) {
    stop("All arguments to rows_list must be named")
  }
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
  if (length(names(dots)) == 0 || any(names(dots) == "")) {
    stop("All arguments to rows_list must be named")
  }
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


#' Copy tab results to clipboard for pasting into Google Sheets
#'
#' Copies a tab_result object to the clipboard in a format suitable for pasting
#' into Google Sheets. Percentages are converted to decimals, base sizes are
#' included, and source information is added.
#'
#' @param tab_result A tab_result object from the tab() function
#' @return Invisibly returns the formatted data that was copied
#' @export
#'
#' @examples
#' \dontrun{
#' result <- tab(data, gender, region)
#' copy_tab(result)
#' # Now paste into Google Sheets
#' }
copy_tab <- function(tab_result) {

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
  for (col in names(output_data)[-1]) {
    original_values <- x[[col]]
    formatted_col <- character(length(original_values))

    for (col in names(output_data)[-1]) {
      original_values <- output_data[[col]]           # <-- use output_data
      output_data[[col]] <- vapply(seq_along(original_values), function(i) {
        if (i %in% base_row_idx) {
          as.character(original_values[i])            # keep base as raw
        } else if (is.numeric(original_values[i]) && !is.na(original_values[i])) {
          stat_obj$format_fn(original_values[i])      # apply % / mean fmt
        } else {
          as.character(original_values[i])
        }
      }, character(1))
    }

    x_formatted[[col]] <- formatted_col
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
    message("Table copied to clipboard. Ready to paste into Google Sheets.")
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
