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
#' @param show_col_nets Whether to show Base row/column
#' @param low_base_threshold Numeric, minimum base size required to show a column
#' @param label_mode Character string specifying how labels should be displayed:
#'   \itemize{
#'     \item \code{"smart"}: Show suffixes for multi-item questions, full labels
#'           for single items
#'     \item \code{"full"}: Always show full variable labels (default)
#'     \item \code{"suffix"}: Show question suffixes when available, otherwise
#'           extract from label
#'   }
#' @param sum_by Controls intelligent collapsing of rows and columns.
#'   Can be:
#'   \itemize{
#'     \item \code{FALSE} or \code{NULL}: No collapsing (default)
#'     \item \code{TRUE}: Auto-detect and collapse both rows and columns based on
#'           metadata patterns (backward compatible with smart_collapse_row_labels)
#'     \item Character string: Collapse rows only by the specified metadata field.
#'           Common values are "ival" (collapse by value) or "ivar" (collapse by variable).
#'     \item List with \code{rows} and/or \code{cols} elements: Separate control for
#'           rows and columns. Each element can be:
#'           \itemize{
#'             \item \code{FALSE} or \code{NULL}: No collapsing for that dimension
#'             \item \code{TRUE}: Auto-detect collapsing for that dimension
#'             \item Character string: Collapse by specified field ("ival" or "ivar")
#'           }
#'   }
#' @param helpers List of custom tab_helper objects
#' @param stats List of custom tab_stat objects
#' @param resolve_report Whether to return variable resolution details
#' @param ... Additional arguments
#' @details
#' Base (n) Calculation:
#' For statistics that use the 'values' parameter (like mean), the base (n)
#' represents the count of non-NA values actually used in calculations.
#' When different rows have varying numbers of valid values within a column
#' (or vice versa for row-oriented statistics), the base will display NA
#' and issue a warning. The full base matrix is stored as an attribute
#' 'base_matrix' on the result object.
#'
#' @return Cross-tabulation table as data.frame
#' @export
#' @examples
#' # String syntax
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
                show_row_nets = TRUE, show_col_nets = TRUE, show_base = TRUE,
                low_base_threshold = 0,
                label_mode = "full",
                sum_by = FALSE,
                helpers = NULL, stats = NULL, resolve_report = FALSE,
                ...) {

  # Store original call for later use
  original_call <- match.call()

  ensure_builtins_registered()

  ## Statistics validation and setup ------------------------------------------

  # Handle custom stats registry first to create effective registry
  effective_stats <- .tab_registry$stats
  if (!is.null(stats)) {
    if (!is.list(stats)) {
      stop("stats must be a list of tab_stat objects")
    }
    if (!all(vapply(stats, inherits, logical(1), "tab_stat"))) {
      stop("All items in stats list must be tab_stat objects")
    }
    # Create temporary registry with overrides
    effective_stats <- c(stats, .tab_registry$stats)
    names(effective_stats) <- c(
      vapply(stats, `[[`, character(1), "id"),
      names(.tab_registry$stats)
    )
    # Remove duplicates, keeping first (custom) version
    effective_stats <- effective_stats[!duplicated(names(effective_stats))]
  }

  # Resolve statistic to object
  if (is.character(statistic)) {
    # Handle default arguments
    if (length(statistic) > 1) {
      statistic <- match.arg(statistic)
    }
    # Look up in registry
    if (!statistic %in% names(effective_stats)) {
      stop("Unknown statistic: '", statistic, "'. Available: ",
           paste(names(effective_stats), collapse = ", "))
    }
    statistic <- effective_stats[[statistic]]
  } else if (!inherits(statistic, "tab_stat")) {
    stop("statistic must be a character string or tab_stat object")
  }

  # Validate values parameter
  if (statistic$requires_values && is.null(values)) {
    stop(statistic$id, " statistic requires 'values' parameter")
  }
  if (!is.null(values) && !statistic$requires_values) {
    warning("Values parameter ignored for ", statistic$id, " statistic")
  }

  ## Custom helpers validation and setup --------------------------------------
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

  ## Data validation and setup ------------------------------------------------
  # Extract data and dpdict if survey_data object
  if (inherits(data, "survey_data")) {
    dpdict <- data$dpdict
    data <- data$dat
  } else {
    dpdict <- NULL
  }

  # Check for empty data
  if (nrow(data) == 0) {
    stop("Data has 0 rows.")
  }

  ## Expression handling ------------------------------------------------------
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

  ## Variable resolution ------------------------------------------------------
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

  ## Prep base array, weights and whole-table filter ---------------------------
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

  ## Parse row and column specs ------------------------------------------------
  # Parse row specifications
  rows_quo <- rlang::enquo(rows)
  rows_expr <- rlang::quo_get_expr(rows_quo)

  # Check if it's string syntax
  if (rlang::is_string(rows_expr)) {
    rows_parsed <- list(parse_table_formula(rows_quo, data, dpdict, all_helpers))
  } else {
    # Try to evaluate to see if it's a rows_list
    rows_eval <- tryCatch(
      rlang::eval_tidy(rows_quo),
      error = function(e) NULL
    )

    # Check for tab_helper before checking is_list (since tab_helper inherits from list)
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
      # Check for tab_helper before checking is_list (since tab_helper inherits from list)
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

  ## Variable expansion -------------------------------------------------------
  # Expand variables for rows
  rows_expanded <- list()
  for (row_spec in rows_parsed) {
    expanded <- expand_variables(row_spec, data, dpdict, statistic$id, values,
                                 label_mode = label_mode)
    for (exp in expanded) {
      if (!is.null(row_spec$label) && length(rows_parsed) > 1) {
        exp$group_label <- row_spec$label
      }
      rows_expanded <- append(rows_expanded, list(exp))
    }
  }

  # Expand variables for columns
  if (!is.null(cols_parsed)) {
    cols_expanded <- list()
    for (col_spec in cols_parsed) {
      expanded <- expand_variables(col_spec, data, dpdict, statistic$id, values_var = NULL, label_mode)
      cols_expanded <- append(cols_expanded, expanded)
    }
  } else {
    cols_expanded <- list(list(type = "total", label = "Total"))
  }

  # validate rows and columns for value statistics
  if(!is.null(statistic) && statistic$requires_values){
    # Throws error if variables inappropriate for statistic, else continues
    validate_statistic_variables(statistic, rows_expanded, cols_expanded, data, dpdict)
  }

  ## Create row, column and value arrays ---------------------------------------
  # Create arrays for each specification
  row_arrays <- list()
  rows_expanded_final <- list()

  for (spec in rows_expanded) {
    array_result <- formula_to_array(spec, data, dpdict)

    # Check if helper returned multiple arrays
    if (is.list(array_result) && isTRUE(attr(array_result, "is_multi_column"))) {
      # Multi-column helper - expand to multiple rows
      for (row_name in names(array_result)) {
        row_arrays <- append(row_arrays, list(array_result[[row_name]]))

        # Create a new spec for each row
        new_spec <- spec
        new_spec$label <- row_name
        new_spec$original_helper <- spec$helper_type
        rows_expanded_final <- append(rows_expanded_final, list(new_spec))
      }
    } else {
      # Single array
      row_arrays <- append(row_arrays, list(array_result))
      rows_expanded_final <- append(rows_expanded_final, list(spec))
    }
  }

  # Update rows_expanded to reflect any expansion from multi-column helpers
  rows_expanded <- rows_expanded_final

  col_arrays <- if (!is.null(cols_parsed)) {
    arrays_list <- list()
    expanded_specs <- list()

    for (spec in cols_expanded) {
      if (spec$type == "total") {
        array <- rep(1, nrow(data))
        arrays_list <- append(arrays_list, list(array))
        expanded_specs <- append(expanded_specs, list(spec))
      } else {
        array_result <- formula_to_array(spec, data, dpdict)

        # Check if helper returned multiple arrays
        if (is.list(array_result) && isTRUE(attr(array_result, "is_multi_column"))) {
          # Multi-column helper - add each array separately
          for (col_name in names(array_result)) {
            arrays_list <- append(arrays_list, list(array_result[[col_name]]))
            # Create a new spec for each column
            new_spec <- spec
            new_spec$label <- col_name
            new_spec$original_helper <- spec$helper_type
            expanded_specs <- append(expanded_specs, list(new_spec))
          }
        } else {
          # Single array
          arrays_list <- append(arrays_list, list(array_result))
          expanded_specs <- append(expanded_specs, list(spec))
        }
      }
    }

    # Update cols_expanded to reflect any expansion from multi-column helpers
    cols_expanded <- expanded_specs
    arrays_list
  } else {
    array <- rep(1, nrow(data))
    list(array)
  }

  # Check for label collisions with Base or summary rows and columns
  meta_labels <- c(statistic$base_label)
  if (!is.null(statistic$summary_row)) {
    meta_labels <- c(meta_labels, statistic$summary_row)
  }
  if (!is.null(statistic$summary_col)) {
    meta_labels <- c(meta_labels, statistic$summary_col)
  }

  # Check row labels
  for (spec in rows_expanded) {
    if (spec$label %in% meta_labels) {
      stop("Label collision detected: Row label '", spec$label,
           "' matches a reserved meta-category label. ",
           "Reserved labels are: ", paste(meta_labels, collapse = ", "), ". ",
           "Please rename your data or use a custom label in rows specification.")
    }
  }

  # Check column labels if they exist
  if (!is.null(cols_parsed) && exists("cols_expanded")) {
    for (spec in cols_expanded) {
      if (spec$label %in% meta_labels) {
        stop("Label collision detected: Column label '", spec$label,
             "' matches a reserved meta-category label. ",
             "Reserved labels are: ", paste(meta_labels, collapse = ", "), ". ",
             "Please rename your data or use a custom label in cols specification.")
      }
    }
  }

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

  ## Calculate table ----------------------------------------------------------
  # Build the table
  n_rows <- length(row_arrays)
  n_cols <- length(col_arrays)

  # Initialize result matrix
  result_matrix <- matrix(NA, nrow = n_rows, ncol = n_cols)

  # Compute each cell
  result_matrix <- compute_cells_vectorized(
    base_array = base_array,
    row_arrays = row_arrays,
    col_arrays = col_arrays,
    statistic = statistic,
    values = values_array
  )

  ## Add row and column labels ------------------------------------------------
  # Add row labels
  row_labels <- vapply(rows_expanded, function(x) {
    if (!is.null(x$group_label) && x$group_label != x$label) {
      paste(x$group_label, x$label, sep = " - ")
    } else {
      x$label
    }
  }, character(1))

  result_df <- as.data.frame(result_matrix, stringsAsFactors = FALSE)
  result_df <- cbind(
    data.frame(row_label = row_labels, stringsAsFactors = FALSE),
    result_df
  )

  # Add column labels
  col_labels <- vapply(cols_expanded, function(x) {
    if (!is.null(x$group_label) && x$group_label != x$label) {
      paste(x$group_label, x$label, sep = " - ")
    } else {
      x$label
    }
  }, character(1))

  names(result_df)[-1] <- col_labels

  # Initial base matrix calculation with gates
  base_matrix <- sync_base_matrix(row_arrays, col_arrays, base_array,
                                  values_array, statistic)

  # smart collapse if requested
  # Parse sum_by parameter
  if (is.null(sum_by) || isFALSE(sum_by)) {
    collapse_rows <- FALSE
    collapse_cols <- FALSE
    row_field <- NULL
    col_field <- NULL
  } else if (isTRUE(sum_by)) {
    # Backward compatibility - auto-detect
    collapse_rows <- TRUE
    collapse_cols <- TRUE
    row_field <- NULL  # Auto-detect
    col_field <- NULL  # Auto-detect
  } else if (is.character(sum_by)) {
    # String means collapse rows only by that field
    collapse_rows <- TRUE
    collapse_cols <- FALSE
    row_field <- sum_by
    col_field <- NULL
  } else if (is.list(sum_by)) {
    # List gives full control
    collapse_rows <- !is.null(sum_by$rows) && sum_by$rows != FALSE
    collapse_cols <- !is.null(sum_by$cols) && sum_by$cols != FALSE

    # Extract field names or use auto-detect
    row_field <- if (is.character(sum_by$rows)) sum_by$rows else NULL
    col_field <- if (is.character(sum_by$cols)) sum_by$cols else NULL
  } else {
    stop("sum_by must be FALSE, TRUE, a character string, or a list")
  }

  # Apply collapses
  if (collapse_rows) {
    collapse_result <- smart_collapse_rows(result_df, row_arrays,
                                           field = row_field)
    result_df <- collapse_result$df
    row_arrays <- collapse_result$row_arrays

    # Resync base matrix after row collapse
    base_matrix <- sync_base_matrix(row_arrays, col_arrays, base_array,
                                    values_array, statistic)
  }

  if (collapse_cols) {
    col_collapse <- smart_collapse_cols(result_df, col_arrays,
                                        field = col_field)
    result_df <- col_collapse$df
    col_arrays <- col_collapse$col_arrays

    # Resync base matrix after column collapse
    base_matrix <- sync_base_matrix(row_arrays, col_arrays, base_array,
                                    values_array, statistic)
  }


  ## Remove low-base rows/columns based on threshold ----
  kept_row_indices <- seq_len(nrow(result_df))
  kept_col_indices <- seq_len(ncol(result_df) - 1)  # Excluding row_label column

  if (!is.null(low_base_threshold) && !is.null(base_matrix)) {
    # Summary rows and columns are added AFTER this filtering step,
    # so they are automatically excluded from the low_base_threshold check

    # Apply threshold to each cell in the result matrix
    # First, create a mask of which cells meet the threshold
    if (low_base_threshold == 0) {
      adequate_base <- base_matrix > 0
    } else {
      adequate_base <- base_matrix >= low_base_threshold
    }

    # Replace inadequate cells with NA in result_df
    for (i in seq_len(nrow(result_df))) {
      for (j in seq_len(ncol(result_df) - 1)) {  # Skip row_label column
        if (!adequate_base[i, j]) {
          result_df[i, j + 1] <- NA  # +1 to account for row_label column
        }
      }
    }

    # Identify rows that have at least one non-NA value
    rows_with_data <- logical(nrow(result_df))
    for (i in seq_len(nrow(result_df))) {
      row_values <- result_df[i, -1, drop = TRUE]  # Exclude row_label
      rows_with_data[i] <- any(!is.na(row_values))
    }

    # Identify columns that have at least one non-NA value
    cols_with_data <- logical(ncol(result_df) - 1)
    for (j in seq_len(ncol(result_df) - 1)) {
      col_values <- result_df[, j + 1, drop = TRUE]
      cols_with_data[j] <- any(!is.na(col_values))
    }

    # Remove rows with all NA values
    if (any(rows_with_data)) {
      kept_row_indices <- which(rows_with_data)
      result_df <- result_df[kept_row_indices, , drop = FALSE]
      row_arrays <- row_arrays[kept_row_indices]
    } else {
      # All rows have inadequate base
      result_df <- result_df[0, , drop = FALSE]
      row_arrays <- list()
    }

    # Remove columns with all NA values
    if (any(cols_with_data)) {
      cols_to_keep <- c(1, which(cols_with_data) + 1)  # +1 for row_label, +1 for 1-based indexing
      result_df <- result_df[, cols_to_keep, drop = FALSE]
      kept_col_indices <- which(cols_with_data)
      col_arrays <- col_arrays[kept_col_indices]
    } else if (length(col_arrays) > 0) {
      # All columns have inadequate base
      result_df <- result_df[, 1, drop = FALSE]
      col_arrays <- list()
    }

    # Final resync after low-base filtering
    if (length(row_arrays) > 0 && length(col_arrays) > 0) {
      base_matrix <- sync_base_matrix(row_arrays, col_arrays, base_array,
                                      values_array, statistic)
    } else {
      base_matrix <- NULL
    }
  }

  ## Build summary rows and columns -------------------------------------------

  # Continue with remaining logic based on remaining data
  summary_row_label <- NULL
  summary_row_array <- NULL

  # Add summary column if specified and we have multiple remaining columns
  summary_col_label <- NULL
  summary_col_array <- NULL

  show_summary_col <- show_col_nets
  show_summary_row <- show_row_nets

  if (length(col_arrays) > 1 && !is.null(statistic$summary_col) && show_summary_col) {
    summary_col_label <- statistic$summary_col

    if (is.null(statistic$summary_col_calculator)) {
      stop("Statistic '", statistic$id, "' has summary_col='", summary_col_label,
           "' but no summary_col_calculator defined")
    }

    summary_col_array <- statistic$summary_col_calculator(
      arrays = col_arrays,
      base_array = base_array
    )

    # Extract labels from the column arrays being summarized
    col_labels <- sapply(col_arrays, function(arr) {
      meta <- attr(arr, "meta")
      if (!is.null(meta) && !is.null(meta$label)) {
        meta$label
      } else {
        "unknown"
      }
    })

    summary_col_array <- .ensure_meta(
      summary_col_array,
      ivar = "_SUMMARY",
      ival = summary_col_label,  # "Total", "NET", etc.
      label = paste0(summary_col_label, " (", paste(col_labels, collapse = " + "), ")")
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

    # Add summary col to result
    result_df[[summary_col_label]] <- summary_values
    col_arrays <- c(col_arrays, list(summary_col_array))

    # Resync base matrix to include summary column
    base_matrix <- sync_base_matrix(row_arrays, col_arrays, base_array,
                                    values_array, statistic)
  }

  if (!is.null(statistic$summary_row) && length(row_arrays) > 1 && show_summary_row) {
    # Calculate summary row using the statistic's row calculator
    if (is.null(statistic$summary_row_calculator)) {
      stop("Statistic '", statistic$id, "' has summary_row='", statistic$summary_row,
           "' but no summary_row_calculator defined")
    }

    summary_row_array <- statistic$summary_row_calculator(
      arrays = row_arrays,
      base_array = base_array
    )

    # Extract labels from the row arrays being summarized
    row_labels <- sapply(row_arrays, function(arr) {
      meta <- attr(arr, "meta")
      if (!is.null(meta) && !is.null(meta$label)) {
        meta$label
      } else {
        "unknown"
      }
    })

    summary_row_label <- statistic$summary_row  # "NET", "Avg", etc.
    summary_row_array <- .ensure_meta(
      summary_row_array,
      ivar = "_SUMMARY",
      ival = summary_row_label,
      label = paste0(summary_row_label, " (", paste(row_labels, collapse = " + "), ")")
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
      row_label = statistic$summary_row,
      stringsAsFactors = FALSE
    )
    for (i in seq_along(summary_values)) {
      summary_row_df[[names(result_df)[i + 1]]] <- summary_values[i]
    }

    result_df <- rbind(result_df, summary_row_df)
    row_arrays <- c(row_arrays, list(summary_row_array))

    # Final resync to include summary row
    base_matrix <- sync_base_matrix(row_arrays, col_arrays, base_array,
                                    values_array, statistic)
  }

  ## Add visible base (using pre-calculated base matrix) ------------------------------
  base_orientation <- NULL
  if (show_base && !is.null(base_matrix)) {
    # Determine orientation from the base_matrix structure
    display_as_row <- TRUE
    display_as_column <- TRUE

    if (ncol(base_matrix) > 1) {
      # Check if bases are constant within columns
      for (j in seq_len(ncol(base_matrix))) {
        if (length(unique(base_matrix[, j])) > 1) {
          display_as_row <- FALSE
          break
        }
      }
    }

    if (nrow(base_matrix) > 1) {
      # Check if bases are constant within rows
      for (i in seq_len(nrow(base_matrix))) {
        if (length(unique(base_matrix[i, ])) > 1) {
          display_as_column <- FALSE
          break
        }
      }
    }

    # Decide on display format
    if (display_as_row && !display_as_column) {
      # Bases are constant within columns - display as row
      base_orientation <- "column"
    } else if (display_as_column && !display_as_row) {
      # Bases are constant within rows - display as column
      base_orientation <- "row"
    } else if (display_as_row && display_as_column) {
      # All bases are the same - default to row display
      base_orientation <- "column"
    } else {
      # Bases vary in both dimensions - default to row but show warning
      base_orientation <- "column"
      warning("Base values vary within both rows and columns. Displaying as row.", call. = FALSE)
    }

    if (base_orientation == "row") {
      # Display base as a column
      row_base_values <- numeric(nrow(result_df))

      # Extract base values for each row from pre-calculated matrix
      for (i in seq_len(nrow(base_matrix))) {
        if (ncol(base_matrix) > 0) {
          # Get the common base for this row
          row_bases_unique <- unique(base_matrix[i, ])
          if (length(row_bases_unique) == 1) {
            row_base_values[i] <- as.integer(row_bases_unique)
          } else {
            # This shouldn't happen given our detection logic
            row_base_values[i] <- as.integer(base_matrix[i, 1])
          }
        } else {
          row_base_values[i] <- as.integer(base_matrix[i, 1])
        }
      }

      result_df[[statistic$base_label]] <- row_base_values

    } else {
      # Display base as a row
      col_base_values <- numeric(ncol(base_matrix))

      # Extract base values for each column from pre-calculated matrix
      for (j in seq_len(ncol(base_matrix))) {
        # Check if all rows have same base for this column
        col_bases_unique <- unique(base_matrix[, j])
        col_bases_unique <- col_bases_unique[!is.na(col_bases_unique)]
        if (length(col_bases_unique) == 1) {
          col_base_values[j] <- as.integer(col_bases_unique)
        } else {
          col_base_values[j] <- NA_integer_
          base_varies_warning <- TRUE
        }
      }

      # Create base row
      base_row <- data.frame(
        row_label = statistic$base_label,
        stringsAsFactors = FALSE
      )

      # Add values for each column (including summary column if present)
      for (i in seq_along(col_base_values)) {
        base_row[[names(result_df)[i + 1]]] <- col_base_values[i]
      }

      # Add base row
      result_df <- rbind(result_df, base_row)
    }
  }

  ## Store arrays for significance testing ------------------------------------
  # Only include arrays for actual data rows (not summary rows)
  # Determine how many row arrays to keep (exclude summary row if present)
  n_data_row_arrays <- length(row_arrays)
  if (!is.null(statistic$summary_row) && length(row_arrays) > 1 && show_summary_row &&
      !is.null(summary_row_array)) {
    # The last row array is the summary row array, exclude it
    n_data_row_arrays <- n_data_row_arrays - 1
  }

  # Only keep data row arrays, then add base array
  data_row_arrays <- row_arrays[seq_len(n_data_row_arrays)]
  final_row_arrays <- c(data_row_arrays, list(base_array))

  # Build final_col_arrays - only include data column arrays
  n_data_col_arrays <- length(col_arrays)
  if (!is.null(summary_col_label) && !is.null(summary_col_array)) {
    # The last col array is the summary col array, exclude it
    n_data_col_arrays <- n_data_col_arrays - 1
  }

  # Only keep data column arrays
  data_col_arrays <- col_arrays[seq_len(n_data_col_arrays)]
  final_col_arrays <- data_col_arrays

  # Include real base_array for base column if it was displayed as a column
  if (show_base && !is.null(base_orientation) && base_orientation == "row") {
    final_col_arrays <- c(final_col_arrays, list(base_array))
  }

  # Store arrays for later significance testing
  attr(result_df, "arrays") <- list(
    base_array = base_array,
    row_arrays = final_row_arrays,
    col_arrays = final_col_arrays,
    values_array = values_array
  )

  # Store metadata as attributes
  attr(result_df, "statistic") <- statistic
  attr(result_df, "weight") <- weight
  attr(result_df, "values_variable") <- values
  attr(result_df, "base_orientation") <- base_orientation

  # Store information about which rows/columns are summary rows/columns
  attr(result_df, "summary_row_label") <- summary_row_label
  attr(result_df, "summary_col_label") <- summary_col_label

  # Store the base matrix as an attribute for potential future use
  attr(result_df, "base_matrix") <- base_matrix

  # Store the original call for sourcing when using copy_tab
  attr(result_df, "call") <- original_call

  class(result_df) <- c("tab_result", "data.frame")

  return(result_df)
}


#' Sync base matrix with current row and column arrays
#'
#' This function recalculates the base matrix whenever the dimensions change,
#' ensuring consistency between arrays and bases. It applies gates during
#' base calculation to maintain orthogonal structure.
#'
#' @param row_arrays List of row arrays
#' @param col_arrays List of column arrays
#' @param base_array Base array for filtering
#' @param values_array Optional values array for mean calculations
#' @param statistic The statistic object containing base_calculator
#' @return Matrix of base values with dimensions matching row/col arrays
#' @keywords internal
sync_base_matrix <- function(row_arrays, col_arrays, base_array,
                             values_array, statistic) {

  n_r <- length(row_arrays)
  n_c <- length(col_arrays)

  if (n_r == 0 || n_c == 0) {
    return(NULL)
  }

  base_matrix <- matrix(NA_real_, n_r, n_c)

  for (i in seq_len(n_r)) {
    r_arr <- row_arrays[[i]]
    r_gate <- attr(r_arr, "cell_gate")
    r_meta <- attr(r_arr, "meta")

    if (is.null(r_gate)) r_gate <- always_true_gate
    if (is.null(r_meta)) r_meta <- list()

    for (j in seq_len(n_c)) {
      c_arr <- col_arrays[[j]]
      c_gate <- attr(c_arr, "cell_gate")
      c_meta <- attr(c_arr, "meta")

      if (is.null(c_gate)) c_gate <- always_true_gate
      if (is.null(c_meta)) c_meta <- list()

      # Create cell context with all required information
      cell_ctx <- list(
        stat_id = statistic$id,
        base_array = base_array,
        values_array = values_array
      )

      # Apply gates: both row and column gates must pass
      cell_ok <- r_gate(r_meta, c_meta, cell_ctx) && c_gate(r_meta, c_meta, cell_ctx)

      if (!cell_ok) {
        base_matrix[i, j] <- 0
      } else {
        base_matrix[i, j] <- statistic$base_calculator(
          base_array = base_array,
          row_array = r_arr,
          col_array = c_arr,
          values_array = values_array
        )
      }
    }
  }

  base_matrix
}


#' Calculate base array applying filter and weights
#'
#' @param data Data frame
#' @param filter_expr Filter expression
#' @param weight_var Weight variable name
#' @param parent_env Parent environment for filter evaluation
#' @return Numeric array with 1s for included rows, 0s for excluded
#' @keywords internal
calculate_base_array <- function(data, filter_expr, weight_var, parent_env) {
  n_rows <- nrow(data)
  base_array <- rep(1, n_rows)

  # Apply filter if specified
  if (!is.null(filter_expr) && !is.symbol(filter_expr) ||
      (is.symbol(filter_expr) && as.character(filter_expr) != "")) {
    filter_result <- eval(filter_expr, data, parent_env)
    if (!is.logical(filter_result) || length(filter_result) != n_rows) {
      stop("Filter expression must return a logical vector of length nrow(data)")
    }
    filter_result[is.na(filter_result)] <- FALSE
    base_array <- base_array * as.numeric(filter_result)
  }

  # Apply weights if specified
  if (!is.null(weight_var)) {
    if (!weight_var %in% names(data)) {
      stop("Weight variable '", weight_var, "' not found in data")
    }
    weight_values <- data[[weight_var]]
    if (!is.numeric(weight_values)) {
      stop("Weight variable must be numeric")
    }
    if (any(weight_values < 0, na.rm = TRUE)) {
      stop("Weight values must be non-negative")
    }
    # Replace NA weights with 0
    weight_values[is.na(weight_values)] <- 0
    base_array <- base_array * weight_values
  }

  base_array
}

.is_orthogonal <- function(mat, by = c("row","col")) {
  by <- match.arg(by)
  if (by == "row")  all(rowSums(mat != 0 & !is.na(mat)) <= 1)
  else              all(colSums(mat != 0 & !is.na(mat)) <= 1)
}

#' Find groups of rows that can be collapsed based on meta attributes
#'
#' @param row_labels Character vector of row labels
#' @param data_cols Data frame of numeric columns
#' @param row_arrays List of row arrays with meta attributes
#' @param common_separators Separators for fallback string matching
#' @param field Optional field to collapse by (e.g., "ival", "ivar")
#' @return List of collapsible groups
#' @keywords internal
find_collapsible_groups_meta <- function(row_labels, data_cols, row_arrays,
                                         common_separators = c(" - ", ": ", " | ", " / "),
                                         field = NULL) {
  groups <- list()

  # Skip if no arrays provided
  if (is.null(row_arrays) || length(row_arrays) == 0) {
    # Fallback to string-based grouping
    return(find_collapsible_groups(row_labels, data_cols, common_separators))
  }

  # If field is specified, only do that pass
  if (!is.null(field)) {
    if (field == "ival") {
      # Only group by ival
      ivals <- vapply(row_arrays, function(arr) {
        meta <- attr(arr, "meta")
        if (!is.null(meta) && !is.null(meta$ival)) {
          if (length(meta$ival) > 1) meta$ival[1] else meta$ival
        } else {
          NA_character_
        }
      }, character(1))

      unique_ivals <- unique(ivals[!is.na(ivals)])

      for (ival in unique_ivals) {
        ival_indices <- which(ivals == ival)

        if (length(ival_indices) >= 2) {
          # Check if this group is orthogonal
          group_data <- data_cols[ival_indices, , drop = FALSE]

          # Check orthogonality: at most one non-zero per column
          is_orthogonal <- TRUE
          for (j in seq_len(ncol(group_data))) {
            col_values <- group_data[, j]
            non_zero_count <- sum(col_values != 0 & !is.na(col_values))
            if (non_zero_count > 1) {
              is_orthogonal <- FALSE
              break
            }
          }

          if (is_orthogonal) {
            groups[[length(groups) + 1]] <- list(
              indices = ival_indices,
              common_prefix = ival,
              separator = NA,
              type = "ival"
            )
          }
        }
      }

      # Return early when field is specified
      return(groups)

    } else if (field == "ivar") {
      # Only group by ivar
      ivars <- vapply(row_arrays, function(arr) {
        meta <- attr(arr, "meta")
        if (!is.null(meta) && !is.null(meta$ivar)) {
          # For multiple ivars, create a key by sorting and pasting
          if (length(meta$ivar) > 1) {
            paste(sort(meta$ivar), collapse = "|")
          } else {
            meta$ivar
          }
        } else {
          NA_character_
        }
      }, character(1))

      unique_ivars <- unique(ivars[!is.na(ivars)])

      for (ivar_key in unique_ivars) {
        ivar_indices <- which(ivars == ivar_key)

        if (length(ivar_indices) >= 2) {
          # Check orthogonality
          group_data <- data_cols[ivar_indices, , drop = FALSE]

          is_orthogonal <- TRUE
          for (j in seq_len(ncol(group_data))) {
            col_values <- group_data[, j]
            non_zero_count <- sum(col_values != 0 & !is.na(col_values))
            if (non_zero_count > 1) {
              is_orthogonal <- FALSE
              break
            }
          }

          if (is_orthogonal) {
            # Extract a sensible label for the group
            first_label <- strsplit(ivar_key, "|", fixed = TRUE)[[1]][1]

            groups[[length(groups) + 1]] <- list(
              indices = ivar_indices,
              common_prefix = first_label,
              separator = NA,
              type = "ivar"
            )
          }
        }
      }

      # Return early when field is specified
      return(groups)

    } else {
      warning("Unknown field '", field, "' for collapsing. Using auto-detect.")
      # Fall through to auto-detect
    }
  }

  # AUTO-DETECT MODE (when field is NULL or unknown)

  # PASS 1: Group by ival (for value-mode expansions like "Daily", "Weekly")
  # Extract ival from each array's meta
  ivals <- vapply(row_arrays, function(arr) {
    meta <- attr(arr, "meta")
    if (!is.null(meta) && !is.null(meta$ival)) {
      # Return first ival if multiple (for merged arrays)
      if (length(meta$ival) > 1) meta$ival[1] else meta$ival
    } else {
      NA_character_
    }
  }, character(1))

  # Group by unique ival values (excluding NAs)
  unique_ivals <- unique(ivals[!is.na(ivals)])

  for (ival in unique_ivals) {
    ival_indices <- which(ivals == ival)

    if (length(ival_indices) >= 2) {
      # Check if this group is orthogonal
      group_data <- data_cols[ival_indices, , drop = FALSE]

      # Check orthogonality: at most one non-zero per column
      is_orthogonal <- TRUE
      for (j in seq_len(ncol(group_data))) {
        col_values <- group_data[, j]
        non_zero_count <- sum(col_values != 0 & !is.na(col_values))
        if (non_zero_count > 1) {
          is_orthogonal <- FALSE
          break
        }
      }

      if (is_orthogonal) {
        groups[[length(groups) + 1]] <- list(
          indices = ival_indices,
          common_prefix = ival,  # Use ival as the collapsed label
          separator = NA,  # Not string-based
          type = "ival"
        )
      }
    }
  }

  # Track which rows have been grouped
  grouped_indices <- unlist(lapply(groups, `[[`, "indices"))
  remaining_indices <- setdiff(seq_along(row_arrays), grouped_indices)

  # PASS 2: For ungrouped rows, try grouping by ivar
  if (length(remaining_indices) > 0) {
    remaining_ivars <- vapply(row_arrays[remaining_indices], function(arr) {
      meta <- attr(arr, "meta")
      if (!is.null(meta) && !is.null(meta$ivar)) {
        # For multiple ivars, create a key by sorting and pasting
        if (length(meta$ivar) > 1) {
          paste(sort(meta$ivar), collapse = "|")
        } else {
          meta$ivar
        }
      } else {
        NA_character_
      }
    }, character(1))

    # Group remaining rows by ivar
    unique_ivars <- unique(remaining_ivars[!is.na(remaining_ivars)])

    for (ivar_key in unique_ivars) {
      ivar_rel_indices <- which(remaining_ivars == ivar_key)
      ivar_indices <- remaining_indices[ivar_rel_indices]

      if (length(ivar_indices) >= 2) {
        # Check orthogonality
        group_data <- data_cols[ivar_indices, , drop = FALSE]

        is_orthogonal <- TRUE
        for (j in seq_len(ncol(group_data))) {
          col_values <- group_data[, j]
          non_zero_count <- sum(col_values != 0 & !is.na(col_values))
          if (non_zero_count > 1) {
            is_orthogonal <- FALSE
            break
          }
        }

        if (is_orthogonal) {
          # For ivar groups, use a sensible label
          # Extract the common part from the first label
          first_label <- row_labels[ivar_indices[1]]
          common_label <- strsplit(ivar_key, "|", fixed = TRUE)[[1]][1]

          groups[[length(groups) + 1]] <- list(
            indices = ivar_indices,
            common_prefix = common_label,
            separator = NA,
            type = "ivar"
          )
        }
      }
    }
  }

  # Update remaining indices after second pass
  grouped_indices <- unlist(lapply(groups, `[[`, "indices"))
  final_remaining <- setdiff(seq_along(row_arrays), grouped_indices)

  # PASS 3: Fallback to string prefix matching for remaining rows
  if (length(final_remaining) > 0) {
    remaining_labels <- row_labels[final_remaining]
    remaining_data <- data_cols[final_remaining, , drop = FALSE]

    # Use original string-based logic on remaining rows
    string_groups <- find_collapsible_groups(remaining_labels, remaining_data, common_separators)

    # Adjust indices to match original positions
    for (sg in string_groups) {
      adjusted_indices <- final_remaining[sg$indices]
      groups[[length(groups) + 1]] <- list(
        indices = adjusted_indices,
        common_prefix = sg$common_prefix,
        separator = sg$separator,
        type = "string"
      )
    }
  }

  # Remove overlapping groups (keep the ones with most rows)
  if (length(groups) > 1) {
    groups <- remove_overlapping_groups(groups)
  }

  return(groups)
}

# Update the existing find_collapsible_groups to use meta when available
find_collapsible_groups <- function(row_labels, data_cols, common_separators) {
  # This is now just the string-based fallback logic
  # (Keep the existing implementation for backward compatibility)
  groups <- list()

  # Try each separator
  for (sep in common_separators) {
    # Find labels containing this separator
    has_sep <- grepl(sep, row_labels, fixed = TRUE)

    if (sum(has_sep) < 2) next

    # Extract prefixes for labels with this separator
    labels_with_sep <- row_labels[has_sep]
    indices_with_sep <- which(has_sep)

    prefixes <- sapply(labels_with_sep, function(label) {
      parts <- strsplit(label, sep, fixed = TRUE)[[1]]
      if (length(parts) >= 2) {
        trimws(parts[1])
      } else {
        NA_character_
      }
    })

    # Group by prefix
    unique_prefixes <- unique(na.omit(prefixes))

    for (prefix in unique_prefixes) {
      prefix_indices <- indices_with_sep[!is.na(prefixes) & prefixes == prefix]

      if (length(prefix_indices) < 2) next

      # Check if this group is collapsible
      group_data <- data_cols[prefix_indices, , drop = FALSE]

      # For each column, check if at most one row has non-zero
      is_collapsible <- TRUE
      for (j in seq_len(ncol(group_data))) {
        col_values <- group_data[, j]
        non_zero_count <- sum(col_values != 0 & !is.na(col_values))
        if (non_zero_count > 1) {
          is_collapsible <- FALSE
          break
        }
      }

      if (is_collapsible) {
        groups[[length(groups) + 1]] <- list(
          indices = prefix_indices,
          common_prefix = prefix,
          separator = sep
        )
      }
    }
  }

  # Remove overlapping groups (keep the ones with most rows)
  if (length(groups) > 1) {
    groups <- remove_overlapping_groups(groups)
  }

  return(groups)
}


#' Smart collapse rows using meta attributes
#'
#' @param result_df The result data frame from tab
#' @param row_arrays List of row arrays with meta attributes
#' @param common_separators Character vector of separators for fallback
#' @return List with collapsed data frame and updated row arrays
#' @keywords internal
smart_collapse_rows <- function(result_df, row_arrays = NULL,
                                field = NULL,  # New parameter
                                common_separators = c(" - ", ": ", " | ", " / ")) {

  if (nrow(result_df) < 2) {
    return(list(df = result_df, row_arrays = row_arrays))
  }

  # Get row labels and data columns
  row_labels <- result_df$row_label
  data_cols <- result_df[, -1, drop = FALSE]

  # Find groups using meta-based logic with field hint
  row_groups <- find_collapsible_groups_meta(row_labels, data_cols, row_arrays,
                                             common_separators, field = field)

  if (length(row_groups) == 0) {
    return(list(df = result_df, row_arrays = row_arrays))
  }

  # Process each group
  new_rows <- list()
  new_row_arrays <- list()
  processed_indices <- integer(0)

  for (group in row_groups) {
    group_indices <- group$indices
    group_label <- group$common_prefix

    # Sum values across rows in the group
    group_data <- data_cols[group_indices, , drop = FALSE]
    collapsed_values <- colSums(group_data, na.rm = TRUE)

    # Create new row
    new_row <- data.frame(
      row_label = group_label,
      t(collapsed_values),
      stringsAsFactors = FALSE
    )
    names(new_row) <- names(result_df)

    new_rows[[length(new_rows) + 1]] <- new_row

    # Update row arrays if provided
    if (!is.null(row_arrays)) {
      # Combine arrays (OR logic)
      collapsed_array <- Reduce(function(x, y) pmax(x, y, na.rm = TRUE),
                                row_arrays[group_indices])

      # Merge metadata
      row_metas <- lapply(row_arrays[group_indices],
                          \(a) attr(a, "meta") %||% list())

      # Use .ensure_meta() to create merged meta
      collapsed_array <- .ensure_meta(
        collapsed_array,
        ivar = unique(unlist(lapply(row_metas, `[[`, "ivar"))),
        ival = unique(unlist(lapply(row_metas, `[[`, "ival"))),
        label = group_label
      )

      # Copy the gate from the first row in the group
      # If that row had no gate, attach the appropriate default gate
      gate <- attr(row_arrays[[group_indices[1]]], "cell_gate")
      if (is.null(gate)) {
        gate_factory <- get_gate("no_mismatch")
        if (!is.null(gate_factory)) {
          # Determine gate type based on what was collapsed
          if (group$type == "ival") {
            gate <- gate_factory("ival")
          } else if (group$type == "ivar") {
            gate <- gate_factory("ivar")
          }
          # For string type, don't add a gate
        }
      }
      if (!is.null(gate)) {
        attr(collapsed_array, "cell_gate") <- gate
      }

      new_row_arrays[[length(new_row_arrays) + 1]] <- collapsed_array
    }

    processed_indices <- c(processed_indices, group_indices)
  }

  # Add uncollapsed rows
  remaining_indices <- setdiff(seq_len(nrow(result_df)), processed_indices)

  if (length(remaining_indices) > 0) {
    remaining_rows <- result_df[remaining_indices, , drop = FALSE]
    rownames(remaining_rows) <- NULL

    if (!is.null(row_arrays)) {
      remaining_arrays <- row_arrays[remaining_indices]
      new_row_arrays <- c(new_row_arrays, remaining_arrays)
    }

    # Combine collapsed and remaining rows
    collapsed_df <- rbind(do.call(rbind, new_rows), remaining_rows)
  } else {
    collapsed_df <- do.call(rbind, new_rows)
  }

  # Reset row names
  rownames(collapsed_df) <- NULL

  return(list(df = collapsed_df, row_arrays = new_row_arrays))
}

#' Find groups of rows that can be collapsed
#'
#' @param row_labels Character vector of row labels
#' @param data_cols Data frame of numeric columns
#' @param common_separators Separators to check
#' @return List of collapsible groups
#' @keywords internal
find_collapsible_groups <- function(row_labels, data_cols, common_separators) {
  groups <- list()

  # Try each separator
  for (sep in common_separators) {
    # Find labels containing this separator
    has_sep <- grepl(sep, row_labels, fixed = TRUE)

    if (sum(has_sep) < 2) next

    # Extract prefixes for labels with this separator
    labels_with_sep <- row_labels[has_sep]
    indices_with_sep <- which(has_sep)

    prefixes <- sapply(labels_with_sep, function(label) {
      parts <- strsplit(label, sep, fixed = TRUE)[[1]]
      if (length(parts) >= 2) {
        trimws(parts[1])
      } else {
        NA_character_
      }
    })

    # Group by prefix
    unique_prefixes <- unique(na.omit(prefixes))

    for (prefix in unique_prefixes) {
      prefix_indices <- indices_with_sep[!is.na(prefixes) & prefixes == prefix]

      if (length(prefix_indices) < 2) next

      # Check if this group is collapsible
      group_data <- data_cols[prefix_indices, , drop = FALSE]

      # For each column, check if at most one row has non-zero
      is_collapsible <- TRUE
      for (j in seq_len(ncol(group_data))) {
        col_values <- group_data[, j]
        non_zero_count <- sum(col_values != 0 & !is.na(col_values))
        if (non_zero_count > 1) {
          is_collapsible <- FALSE
          break
        }
      }

      if (is_collapsible) {
        groups[[length(groups) + 1]] <- list(
          indices = prefix_indices,
          common_prefix = prefix,
          separator = sep
        )
      }
    }
  }

  # Remove overlapping groups (keep the ones with most rows)
  if (length(groups) > 1) {
    groups <- remove_overlapping_groups(groups)
  }

  return(groups)
}

#' Remove overlapping groups, keeping those with most rows
#'
#' @param groups List of groups
#' @return List of non-overlapping groups
#' @keywords internal
remove_overlapping_groups <- function(groups) {
  if (length(groups) <= 1) return(groups)

  # Sort by number of indices (descending)
  group_sizes <- sapply(groups, function(g) length(g$indices))
  groups <- groups[order(group_sizes, decreasing = TRUE)]

  # Keep track of used indices
  used_indices <- integer(0)
  kept_groups <- list()

  for (group in groups) {
    if (!any(group$indices %in% used_indices)) {
      kept_groups[[length(kept_groups) + 1]] <- group
      used_indices <- c(used_indices, group$indices)
    }
  }

  return(kept_groups)
}

#' Find groups of columns that can be collapsed based on meta attributes
#'
#' @param col_labels Character vector of column labels
#' @param data_matrix Matrix of data values
#' @param col_arrays List of column arrays with meta attributes
#' @param common_separators Separators for fallback string matching
#' @param field Optional field to collapse by (e.g., "ival", "ivar")
#' @return Named list of column groups
#' @keywords internal
find_collapsible_col_groups_meta <- function(col_labels, data_matrix, col_arrays,
                                             common_separators = c(" - ", ": ", " | ", " / "),
                                             field = NULL) {

  if (is.null(col_arrays) || length(col_arrays) == 0) {
    # Fallback to string-based grouping
    return(find_collapsible_col_groups_string(col_labels, data_matrix, common_separators))
  }

  groups <- list()

  # If field is specified, only do that pass
  if (!is.null(field)) {
    if (field == "ival") {
      # Only group by ival
      ivals <- vapply(col_arrays, function(arr) {
        meta <- attr(arr, "meta")
        if (!is.null(meta) && !is.null(meta$ival)) {
          if (length(meta$ival) > 1) meta$ival[1] else meta$ival
        } else {
          NA_character_
        }
      }, character(1))

      unique_ivals <- unique(ivals[!is.na(ivals)])

      for (ival in unique_ivals) {
        col_indices <- which(ivals == ival)

        if (length(col_indices) >= 2) {
          # Check orthogonality across rows
          is_orthogonal <- TRUE
          for (i in seq_len(nrow(data_matrix))) {
            row_values <- data_matrix[i, col_indices]
            non_zero_count <- sum(row_values != 0 & !is.na(row_values))
            if (non_zero_count > 1) {
              is_orthogonal <- FALSE
              break
            }
          }

          if (is_orthogonal) {
            # Use ival as the group key
            groups[[ival]] <- col_indices
          }
        }
      }

      # Return early when field is specified
      return(groups)

    } else if (field == "ivar") {
      # Only group by ivar
      ivars <- vapply(col_arrays, function(arr) {
        meta <- attr(arr, "meta")
        if (!is.null(meta) && !is.null(meta$ivar)) {
          if (length(meta$ivar) > 1) {
            paste(sort(meta$ivar), collapse = "|")
          } else {
            meta$ivar
          }
        } else {
          NA_character_
        }
      }, character(1))

      unique_ivars <- unique(ivars[!is.na(ivars)])

      for (ivar_key in unique_ivars) {
        ivar_indices <- which(ivars == ivar_key)

        if (length(ivar_indices) >= 2) {
          # Check orthogonality
          is_orthogonal <- TRUE
          for (i in seq_len(nrow(data_matrix))) {
            row_values <- data_matrix[i, ivar_indices]
            non_zero_count <- sum(row_values != 0 & !is.na(row_values))
            if (non_zero_count > 1) {
              is_orthogonal <- FALSE
              break
            }
          }

          if (is_orthogonal) {
            # Extract a sensible label for the group
            first_label <- strsplit(ivar_key, "|", fixed = TRUE)[[1]][1]
            groups[[first_label]] <- ivar_indices
          }
        }
      }

      # Return early when field is specified
      return(groups)

    } else {
      warning("Unknown field '", field, "' for collapsing. Using auto-detect.")
      # Fall through to auto-detect
    }
  }

  # AUTO-DETECT MODE (when field is NULL or unknown)

  # PASS 1: Group by ival (for columns grouped by value)
  ivals <- vapply(col_arrays, function(arr) {
    meta <- attr(arr, "meta")
    if (!is.null(meta) && !is.null(meta$ival)) {
      if (length(meta$ival) > 1) meta$ival[1] else meta$ival
    } else {
      NA_character_
    }
  }, character(1))

  unique_ivals <- unique(ivals[!is.na(ivals)])

  for (ival in unique_ivals) {
    col_indices <- which(ivals == ival)

    if (length(col_indices) >= 2) {
      # Check orthogonality across rows
      is_orthogonal <- TRUE
      for (i in seq_len(nrow(data_matrix))) {
        row_values <- data_matrix[i, col_indices]
        non_zero_count <- sum(row_values != 0 & !is.na(row_values))
        if (non_zero_count > 1) {
          is_orthogonal <- FALSE
          break
        }
      }

      if (is_orthogonal) {
        # Use ival as the group key
        groups[[ival]] <- col_indices
      }
    }
  }

  # Track which columns have been grouped
  grouped_indices <- unlist(groups)
  remaining_indices <- setdiff(seq_along(col_arrays), grouped_indices)

  # PASS 2: For ungrouped columns, try grouping by ivar
  if (length(remaining_indices) > 0) {
    remaining_ivars <- vapply(col_arrays[remaining_indices], function(arr) {
      meta <- attr(arr, "meta")
      if (!is.null(meta) && !is.null(meta$ivar)) {
        if (length(meta$ivar) > 1) {
          paste(sort(meta$ivar), collapse = "|")
        } else {
          meta$ivar
        }
      } else {
        NA_character_
      }
    }, character(1))

    unique_ivars <- unique(remaining_ivars[!is.na(remaining_ivars)])

    for (ivar_key in unique_ivars) {
      ivar_rel_indices <- which(remaining_ivars == ivar_key)
      ivar_indices <- remaining_indices[ivar_rel_indices]

      if (length(ivar_indices) >= 2) {
        # Check orthogonality
        is_orthogonal <- TRUE
        for (i in seq_len(nrow(data_matrix))) {
          row_values <- data_matrix[i, ivar_indices]
          non_zero_count <- sum(row_values != 0 & !is.na(row_values))
          if (non_zero_count > 1) {
            is_orthogonal <- FALSE
            break
          }
        }

        if (is_orthogonal) {
          # Extract a sensible label for the group
          first_label <- strsplit(ivar_key, "|", fixed = TRUE)[[1]][1]
          groups[[first_label]] <- ivar_indices
        }
      }
    }
  }

  # PASS 3: Fallback to string matching for remaining columns
  final_grouped <- unlist(groups)
  final_remaining <- setdiff(seq_along(col_arrays), final_grouped)

  if (length(final_remaining) > 0) {
    remaining_labels <- col_labels[final_remaining]
    remaining_data <- data_matrix[, final_remaining, drop = FALSE]

    # Use string-based grouping on remaining
    string_groups <- find_collapsible_col_groups_string(
      remaining_labels,
      remaining_data,
      common_separators
    )

    # Adjust indices and merge
    for (prefix in names(string_groups)) {
      adjusted_indices <- final_remaining[string_groups[[prefix]]]
      # Don't overwrite if prefix already exists from meta grouping
      if (!prefix %in% names(groups)) {
        groups[[prefix]] <- adjusted_indices
      }
    }
  }

  return(groups)
}

#' Helper to do string-based column grouping (fallback)
#' @keywords internal
find_collapsible_col_groups_string <- function(col_labels, data_matrix, common_separators) {
  split_prefix <- function(lbl) {
    # Returns text before the *last* recognised separator
    locs <- unlist(lapply(common_separators,
                          function(sep) gregexpr(sep, lbl, fixed = TRUE)))
    locs <- locs[locs > 0]
    if (length(locs))
      trimws(substr(lbl, 1, max(locs) - 1))
    else
      lbl
  }

  prefixes <- vapply(col_labels, split_prefix, character(1))
  groups <- split(seq_along(col_labels), prefixes)

  # Only keep groups that are orthogonal
  collapsible_groups <- list()

  for (pref in names(groups)) {
    idx <- groups[[pref]]

    if (length(idx) < 2) next

    # Check orthogonality
    is_orthogonal <- TRUE
    for (i in seq_len(nrow(data_matrix))) {
      row_values <- data_matrix[i, idx]
      non_zero_count <- sum(row_values != 0 & !is.na(row_values))
      if (non_zero_count > 1) {
        is_orthogonal <- FALSE
        break
      }
    }

    if (is_orthogonal) {
      collapsible_groups[[pref]] <- idx
    }
  }

  return(collapsible_groups)
}

#' Smart collapse columns using meta attributes
#'
#' @param result_df The result data frame from tab
#' @param col_arrays List of column arrays with meta attributes
#' @param common_separators Character vector of separators for fallback
#' @return List with collapsed data frame and updated column arrays
#' @keywords internal
smart_collapse_cols <- function(result_df, col_arrays = NULL,
                                field = NULL,  # New parameter
                                common_separators = c(" - ", ": ", " | ", " / ")) {

  # Nothing to do if we have ≤ 1 data column
  if (ncol(result_df) <= 2) {
    return(list(df = result_df, col_arrays = col_arrays))
  }

  label_col <- names(result_df)[1]  # usually "row_label"
  data_part <- result_df[, -1, drop = FALSE]
  col_labels <- names(data_part)
  data_matrix <- as.matrix(data_part)

  # Find collapsible groups using meta with field hint
  groups <- find_collapsible_col_groups_meta(col_labels, data_matrix, col_arrays,
                                             common_separators, field = field)

  # New containers
  new_df <- result_df[label_col]  # Start with label column
  new_col_arrays <- list()

  # Process each group
  for (group_label in names(groups)) {
    idx <- groups[[group_label]]

    if (length(idx) == 1) {
      # Single column - keep as is
      new_df[[col_labels[idx]]] <- data_part[[idx]]
      if (!is.null(col_arrays)) {
        new_col_arrays <- c(new_col_arrays, col_arrays[idx])
      }
    } else {
      # Multiple columns - collapse
      block <- as.matrix(data_part[, idx, drop = FALSE])
      new_df[[group_label]] <- rowSums(block, na.rm = TRUE)

      if (!is.null(col_arrays)) {
        # Merge arrays
        merged <- Reduce(function(x, y) pmax(x, y, na.rm = TRUE), col_arrays[idx])

        # Merge metadata
        col_metas <- lapply(col_arrays[idx],
                            \(a) attr(a, "meta") %||% list())

        merged <- .ensure_meta(
          merged,
          ivar = unique(unlist(lapply(col_metas, `[[`, "ivar"))),
          ival = unique(unlist(lapply(col_metas, `[[`, "ival"))),
          label = group_label
        )

        # Handle gate
        gate <- attr(col_arrays[[idx[1]]], "cell_gate")
        if (is.null(gate)) {
          gate_factory <- get_gate("no_mismatch")
          if (!is.null(gate_factory)) {
            # Determine appropriate gate based on what was collapsed
            # For columns, often we want to match on ivar
            if (all(!is.na(sapply(col_metas, function(m) m$ivar)))) {
              gate <- gate_factory("ivar")
            } else if (all(!is.na(sapply(col_metas, function(m) m$ival)))) {
              gate <- gate_factory("ival")
            }
          }
        }
        if (!is.null(gate)) {
          attr(merged, "cell_gate") <- gate
        }

        new_col_arrays <- c(new_col_arrays, list(merged))
      }
    }
  }

  # Ensure column order is deterministic
  new_df <- new_df[, c(label_col, setdiff(names(new_df), label_col)), drop = FALSE]

  return(list(
    df = new_df,
    col_arrays = if (is.null(col_arrays)) NULL else new_col_arrays
  ))
}

#' Validate Variable Types for Statistics
#'
#' Validates that variables used in cross-tabulation rows and columns are
#' appropriate for the specified statistic. For value-based statistics like
#' mean, ensures variables are categorical rather than continuous numeric.
#'
#' @param statistic A tab_stat object containing statistic metadata
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
validate_statistic_variables <- function(statistic, rows_expanded, cols_expanded, data, dpdict = NULL) {
  if (statistic$requires_values) {
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
              stop("Cannot use numeric variable '", var_name, "' (questiontype: ", questiontype, ") in rows for '", statistic$id, "' statistic")
            }
          } else {
            # Fallback to R class checking
            if (is.numeric(var_data) &&
                is.null(attr(var_data, "labels")) &&
                !is.factor(var_data) &&
                length(unique(na.omit(var_data))) > 15) {
              stop("Cannot use numeric variable '", var_name, "' with ", length(unique(na.omit(var_data))), " unique values in rows for '", statistic$id, "' statistic")
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

              stop("Cannot use numeric variable '", var_name, ") in columns for '", statistic$id, "' statistic.\n")
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

  # Get values variable name if available
  values_var <- attr(x, "values_variable")

  # Get all significance results
  all_sig <- attr(x, "significance")

  # Construct header
  if (statistic$id == "mean" && !is.null(values_var)) {
    cat("\nCross-tabulation (mean of ", values_var, ")\n", sep = "")
  } else {
    cat("\nCross-tabulation (", statistic$id, ")\n", sep = "")
  }
  cat(rep("-", 50), "\n", sep = "")

  # Create column letter mapping
  col_names <- names(x)[-1]

  non_base_cols <- col_names[col_names != statistic$base_label]

  col_letters <- LETTERS[seq_along(non_base_cols)]
  names(col_letters) <- non_base_cols

  # Create a copy for formatting to avoid modifying the original
  x_formatted        <- x
  base_row_idx <- which(x_formatted$row_label == statistic$base_label)

  for (col in names(x_formatted)[-1]) {
    orig <- x[[col]]
    col_idx <- which(names(x_formatted) == col) - 1  # Adjust for row_label column

    # Check if this column is the base column
    base_orientation <- attr(x_formatted, "base_orientation")
    is_base_column <- FALSE
    if (!is.null(base_orientation) && base_orientation == "row") {
      # If base is displayed as a column (row orientation), check if this is it
      is_base_column <- col == statistic$base_label
    }
    if (is_base_column) {
      next  # Skip formatting for base column
    }

    x_formatted[[col]] <- vapply(seq_along(orig), function(i) {
      if (i %in% base_row_idx || is_base_column) {
        as.character(orig[i])
      } else if (is.numeric(orig[i]) && !is.na(orig[i])) {
        formatted_val <- statistic$format_fn(orig[i])

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

  # Update column names to include letters in brackets (after formatting is complete)
  letter_index <- 1
  for (i in seq_along(col_names)) {
    col_name <- col_names[i]
    if (col_name == statistic$base_label) {
      # Don't add letter to base column
      new_col_name <- col_name
    } else {
      # Add letter for non-base columns
      new_col_name <- paste0(col_name, " (", LETTERS[letter_index], ")")
      letter_index <- letter_index + 1
    }
    names(x_formatted)[i + 1] <- new_col_name
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
    helper_type <- attr(actual_expr, "id")
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
      # Get arguments with their names preserved
      args <- rlang::call_args(actual_expr)
      arg_names <- rlang::call_args_names(actual_expr)

      # Set names on the args list
      if (length(arg_names) > 0) {
        names(args) <- arg_names
      }

      return(list(
        type = "helper",
        helper_type = fn_name,
        args = args,
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

#' Get display label based on label mode
#'
#' @param var_name Variable name
#' @param dpdict Data dictionary
#' @param label_mode One of "full", "suffix", or "smart"
#' @param category_name For expanded categorical variables
#' @param data Optional data frame for separator detection
#' @return Character string label
#' @keywords internal
get_display_label <- function(var_name, dpdict = NULL, label_mode = "full", category_name = NULL, data = NULL) {

  # Get var info from dpdict if available
  var_idx <- if (!is.null(dpdict) && var_name %in% dpdict$variable_names) {
    match(var_name, dpdict$variable_names)
  } else {
    NA
  }

  # Handle each mode
  switch(label_mode,
         "full" = {
           if (!is.null(category_name)) {
             base_label <- if (!is.na(var_idx)) dpdict$variable_labels[var_idx] else var_name
             paste0(base_label, ": ", category_name)
           } else {
             if (!is.na(var_idx)) dpdict$variable_labels[var_idx] else var_name
           }
         },

         "suffix" = {
           if (!is.null(category_name)) {
             category_name
           } else if (!is.na(var_idx)) {
             if ("question_suffix" %in% names(dpdict) &&
                 !is.na(dpdict$question_suffix[var_idx]) &&
                 nzchar(dpdict$question_suffix[var_idx])) {
               dpdict$question_suffix[var_idx]
             } else {
               # Updated call with dpdict and data parameters
               extract_suffix_from_label(dpdict$variable_labels[var_idx], dpdict, data)
             }
           } else {
             var_name
           }
         },

         "smart" = {
           if (!is.null(category_name)) {
             category_name
           } else {
             # Check if multi-item question
             is_multi <- if (!is.na(var_idx) && "singlevariablequestion" %in% names(dpdict)) {
               !isTRUE(dpdict$singlevariablequestion[var_idx])
             } else if (!is.na(var_idx) && "question_group" %in% names(dpdict)) {
               qgroup <- dpdict$question_group[var_idx]
               sum(dpdict$question_group == qgroup, na.rm = TRUE) > 1
             } else {
               FALSE  # Assume single-item, will use full label
             }

             if (is_multi) {
               # Multi-item: use suffix
               if (!is.na(var_idx) && "question_suffix" %in% names(dpdict) &&
                   !is.na(dpdict$question_suffix[var_idx])) {
                 dpdict$question_suffix[var_idx]
               } else if (!is.na(var_idx)) {
                 # Updated call with dpdict and data parameters
                 extract_suffix_from_label(dpdict$variable_labels[var_idx], dpdict, data)
               } else {
                 var_name
               }
             } else {
               # Single item: use full
               if (!is.na(var_idx)) dpdict$variable_labels[var_idx] else var_name
             }
           }
         },

         # Default fallback
         stop("Invalid label_mode: ", label_mode)
  )
}

#' Extract suffix from a label string using detected separator patterns
#' @param label Variable label to extract suffix from
#' @param dpdict Optional data dictionary containing separator patterns
#' @param data Optional data frame to detect separators from if dpdict lacks patterns
#' @keywords internal
extract_suffix_from_label <- function(label, dpdict = NULL, data = NULL) {
  if (is.null(label) || is.na(label) || !nzchar(label)) {
    return(label)
  }

  # Try to get separator patterns from dpdict
  separators <- NULL
  if (!is.null(dpdict)) {
    sep_patterns <- attr(dpdict, "sep_patterns")
    if (!is.null(sep_patterns)) {
      # Use statement_sep first (most relevant for suffixes), then prefix_sep
      if (!is.na(sep_patterns[["statement_sep"]])) {
        separators <- c(sep_patterns[["statement_sep"]])
      }
      if (!is.na(sep_patterns[["prefix_sep"]])) {
        separators <- c(separators, sep_patterns[["prefix_sep"]])
      }
    }
  }

  # If no separators from dpdict, try to detect from data
  if (is.null(separators) && !is.null(data)) {
    sep_analysis <- check_seps(data)
    if (!is.na(sep_analysis$separators[["statement_sep"]])) {
      separators <- c(sep_analysis$separators[["statement_sep"]])
    }
    if (!is.na(sep_analysis$separators[["prefix_sep"]])) {
      separators <- c(separators, sep_analysis$separators[["prefix_sep"]])
    }
  }

  # Fall back to common defaults if no patterns detected
  if (is.null(separators) || length(separators) == 0) {
    separators <- c(" - ", ": ", " | ", " / ")
  }

  # Try each separator
  for (sep in separators) {
    if (grepl(sep, label, fixed = TRUE)) {
      parts <- strsplit(label, sep, fixed = TRUE)[[1]]
      if (length(parts) > 1) {
        suffix <- trimws(parts[length(parts)])
        if (nzchar(suffix)) return(suffix)
      }
    }
  }

  label
}

#' Check if all variables exist in data and provide helpful error message
#' @param vars Character vector of variable names to check
#' @param data Data frame to check against
#' @param context String describing where the variables are being used
#' @keywords internal
check_variables_exist <- function(vars, data, context = "expression") {
  if (length(vars) == 0) return(TRUE)

  missing_vars <- setdiff(vars, names(data))

  if (length(missing_vars) > 0) {
    # Get similar variable names for suggestions
    all_vars <- names(data)
    suggestions <- character()

    for (missing in missing_vars) {
      # Find variables with similar names (using agrep for fuzzy matching)
      similar <- agrep(missing, all_vars, value = TRUE, max.distance = 0.2)
      if (length(similar) > 0) {
        suggestions <- c(suggestions,
                         paste0("  - '", missing, "' -> did you mean: ",
                                paste(paste0("'", similar, "'"), collapse = ", "), "?"))
      } else {
        suggestions <- c(suggestions, paste0("  - '", missing, "' (no similar variables found)"))
      }
    }

    # Build comprehensive error message
    if(length(all_vars) < 20){
      error_msg <- paste0(
        "Variable(s) not found in ", context, ":\n",
        paste(suggestions, collapse = "\n"),
        "\n\nAvailable variables in data:\n",
        paste(strwrap(paste(all_vars, collapse = ", "), width = 70), collapse = "\n  ")
      )
    } else {
      error_msg <- paste0(
        "Variable(s) not found in ", context, ":\n",
        paste(suggestions, collapse = "\n")
      )
    }
    stop(error_msg, call. = FALSE)
  }

  return(TRUE)
}

#' Expand variables and question groups into individual components
#'
#' @param var_spec Variable specification (can be name, expression, or formula)
#' @param data The data frame
#' @param dpdict Optional data dictionary
#' @param statistic$id The ID of the statistic being calculated
#' @return List of expanded variable specifications
#' @keywords internal
expand_variables <- function(var_spec, data, dpdict = NULL, statistic = NULL, values_var = NULL, label_mode = "smart") {
  # Handle complex expressions by recursively expanding components
  if (is.list(var_spec) && !is.null(var_spec$type)) {

    # Helper functions should not be expanded
    if (var_spec$type == "helper") {
      # Special handling for calc_if - expand the inner expression first
      if (var_spec$helper_type == "calc_if") {
        calc_if_obj <- var_spec$args

        # Check if calc_if_obj is already properly structured
        if (!is.list(calc_if_obj) || !all(c("expr", "gate") %in% names(calc_if_obj))) {
          # It's raw arguments - need to construct the calc_if structure
          if (length(calc_if_obj) != 2) {
            stop("calc_if expects exactly 2 arguments: gate and expression")
          }

          # Evaluate the gate argument (first argument)
          gate_arg <- calc_if_obj[[1]]
          if (is.call(gate_arg)) {
            gate_fn <- eval(gate_arg, envir = parent.frame())
          } else {
            gate_fn <- gate_arg
          }

          # The expression is the second argument - parse it
          expr_arg <- calc_if_obj[[2]]
          inner_spec <- parse_table_formula(rlang::enquo(expr_arg), data, dpdict)
        } else {
          gate_fn <- calc_if_obj$gate
          inner_spec <- parse_table_formula(calc_if_obj$expr, data, dpdict)
        }

        # Recursively expand the inner expression
        inner_expanded <- expand_variables(inner_spec, data, dpdict, statistic, values_var, label_mode)

        # Wrap each expanded result with the gate
        result <- list()
        for (exp in inner_expanded) {
          gated_spec <- list(
            type = "gated",
            inner_spec = exp,
            gate = gate_fn,
            label = exp$label  # Use the expanded label
          )
          result <- append(result, list(gated_spec))
        }

        return(result)
      }
      # Other helpers not expanded
      return(list(var_spec))
    }

    if (var_spec$type == "multiplication") {
      # Expand each component and return all combinations
      expanded_components <- lapply(var_spec$components, function(comp) {
        expand_variables(comp, data, dpdict, statistic$id, values_var, label_mode)
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
            val <- labels[i]
            val_label <- names(labels)[i]
            formatted_label <- get_display_label(var_name, dpdict, label_mode, val_label, data)

            child_spec <- list(
              type = "expression",
              components = list(expr = call("==", as.name(var_name), val)),
              label = formatted_label,
              # Add meta attributes where we have full context
              meta = list(
                ivar = get_var_label(var_name, dpdict),
                ival = val_label,
                label = formatted_label
              )
            )
            if (is.list(var_spec) && !is.null(var_spec$gate)) {
              child_spec$gate <- var_spec$gate
            }
            child_spec
          }))
        } else if (is.factor(var_data)) {
          levs <- levels(var_data)
          return(lapply(seq_along(levs), function(i) {
            lev <- levs[i]
            formatted_label <- get_display_label(var_name, dpdict, label_mode, lev, data)

            list(
              type = "expression",
              components = list(expr = call("==", as.name(var_name), i)),  # factors use numeric indices
              label = formatted_label,
              meta = list(
                ivar = get_var_label(var_name, dpdict),
                ival = lev,
                label = formatted_label
              )
            )
          }))
        } else {
          stop("Variable '", var_name, "' has questiontype '", questiontype, "' but no labels or factor levels found")
        }
      } else if (questiontype %in% c("multiresponse", "numeric", "multinumeric")) {
        # Don't expand - return as single variable
        if (is.character(var_spec)) {
          var_label <- get_display_label(var_spec, dpdict, label_mode, NULL, data)
          return(list(list(
            type = "simple",
            components = list(var = var_spec),
            label = var_label,
            meta = list(
              ivar = get_var_label(var_spec, dpdict),
              ival = NULL,  # No categorical value for these types
              label = var_label
            )
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
        val <- labels[i]
        val_label <- names(labels)[i]
        formatted_label <- get_display_label(var_name, dpdict, label_mode, val_label, data)

        list(
          type = "expression",
          components = list(expr = call("==", as.name(var_name), val)),
          label = formatted_label,
          meta = list(
            ivar = get_var_label(var_name, dpdict),
            ival = val_label,
            label = formatted_label
          )
        )
      }))
    }

    # Check if it's a factor
    if (is.factor(var_data)) {
      levs <- levels(var_data)
      return(lapply(levs, function(lev) {
        formatted_label <- get_display_label(var_name, dpdict, label_mode, lev, data)

        list(
          type = "expression",
          components = list(expr = call("==", as.name(var_name), lev)),
          label = formatted_label,
          meta = list(
            ivar = get_var_label(var_name, dpdict),
            ival = lev,
            label = formatted_label
          )
        )
      }))
    }

    # Check if it's logical (treat as binary, don't expand)
    if (is.logical(var_data)) {
      if (is.character(var_spec)) {
        return(list(list(
          type = "simple",
          components = list(var = var_spec),
          label = get_display_label(var_spec, dpdict, label_mode, NULL, data))))
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
          label = get_display_label(var_spec, dpdict, label_mode, NULL, data)
        )))
      } else {
        return(list(var_spec))
      }
    }

    # Character variables should error
    if (is.character(var_data)) {
      stop("Cannot use character variable '", var_name, "' for cross-tabulation")
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
        expanded <- expand_variables(v, data, dpdict, statistic$id, values_var, label_mode)
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
        expanded <- expand_variables(v, data, dpdict, statistic$id, values_var, label_mode)
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
      label = get_display_label(var_spec, dpdict, label_mode, NULL, data)
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

# Specialized array creation functions with meta
create_simple_array <- function(var_data, var_name, spec, dpdict) {
  # Convert to appropriate array type
  if (is.logical(var_data)) {
    arr <- as.numeric(var_data)
  } else if (is.numeric(var_data)) {
    # Check if it's binary (0/1)
    unique_vals <- unique(na.omit(var_data))
    if (all(unique_vals %in% c(0, 1))) {
      arr <- var_data
    } else {
      # For non-binary numeric, keep as is (for means)
      arr <- var_data
    }
  } else {
    stop("Unsupported variable type for '", var_name, "'")
  }

  # Add meta if not already present from spec
  if (!is.null(spec$meta)) {
    # Use meta from expand_variables
    .ensure_meta(arr, spec$meta$ivar, spec$meta$ival, spec$meta$label)
  } else {
    # Add default meta for simple variables
    .ensure_meta(
      arr,
      ivar = get_var_label(var_name, dpdict),
      ival = NULL,
      label = spec$label %||% get_var_label(var_name, dpdict)
    )
  }
}

create_expression_array <- function(expr_result, spec, expr_vars, dpdict) {
  arr <- as.numeric(expr_result)

  # Check if spec has meta from expand_variables
  if (!is.null(spec$meta)) {
    .ensure_meta(arr, spec$meta$ivar, spec$meta$ival, spec$meta$label)
  } else {
    # Add meta for general expressions
    .ensure_meta(
      arr,
      ivar = if (length(expr_vars) > 0) expr_vars else "expression",
      ival = NULL,
      label = spec$label %||% deparse(spec$components$expr)
    )
  }
}

create_total_array <- function(n, spec) {
  arr <- rep(1, n)
  .ensure_meta(
    arr,
    ivar = "_SUMMARY",
    ival = "Total",
    label = spec$label %||% "Total"
  )
}

#' Convert formula specification to numeric array
#'
#' @param formula_spec Parsed formula specification
#' @param data Data frame
#' @param dpdict Optional data dictionary for context
#' @return Numeric vector of length nrow(data)
#' @keywords internal
formula_to_array <- function(formula_spec, data, dpdict = NULL) {
  n <- nrow(data)

  # Start with identity array
  result <- rep(1, n)

  # Process based on type
  if (formula_spec$type == "simple") {
    var_name <- formula_spec$components$var

    check_variables_exist(var_name, data,
                          paste0("expression '", formula_spec$label, "'"))

    return(create_simple_array(data[[var_name]], var_name, formula_spec, dpdict))

  } else if (formula_spec$type == "multiplication") {
    # Apply each component multiplicatively
    for (comp in formula_spec$components) {
      comp_array <- formula_to_array(comp, data)
      result <- result * comp_array
    }

    # For multiplication, preserve meta from first component if available
    if (length(formula_spec$components) > 0) {
      first_array <- formula_to_array(formula_spec$components[[1]], data, dpdict)
      first_meta <- attr(first_array, "meta")
      if (!is.null(first_meta)) {
        result <- .ensure_meta(
          result,
          ivar = first_meta$ivar,
          ival = first_meta$ival,
          label = formula_spec$label %||% first_meta$label
        )
      }
    }
    return(result)

  } else if (formula_spec$type == "expression") {
    # Extract variables referenced in the expression
    expr_vars <- all.vars(formula_spec$components$expr)

    # Check all variables exist before subsetting
    check_variables_exist(expr_vars, data,
                          paste0("expression '", formula_spec$label, "'"))

    # Create subset with only needed variables
    data_subset <- data[, expr_vars, drop = FALSE]
    # Evaluate the expression with numeric-only data for relevant variables
    eval_data <- prepare_eval_data(data_subset)
    expr_result <- rlang::eval_tidy(formula_spec$components$expr, eval_data)
    if (!is.logical(expr_result) && !is.numeric(expr_result)) {
      stop("Expression must evaluate to logical or numeric")
    }
    return(create_expression_array(expr_result, formula_spec, expr_vars, dpdict))

  } else if (formula_spec$type == "helper") {
      helper_result <- process_helper(formula_spec, data, dpdict = dpdict)

      # Check if it's a multi-column helper
      # Multi-column helpers should already have meta
      if (is.list(helper_result) && isTRUE(attr(helper_result, "is_multi_column"))) {
        # For multi-column helpers, return the list directly
        # The calling code will handle the expansion
        return(helper_result)
      } else {
        # Single array helper - ensure it has meta
        if (is.null(attr(helper_result, "meta"))) {
          helper_result <- .ensure_meta(
            helper_result,
            ivar = formula_spec$helper_type,
            ival = NULL,
            label = formula_spec$label %||% formula_spec$helper_type
          )
        }
        return(helper_result)
      }
  } else if (formula_spec$type == "numeric_expression") {
    # Evaluate numeric expressions in data context with numeric-only data
    tryCatch({
      # Extract variables referenced in the expression
      expr_vars <- all.vars(formula_spec$components$expr)

      # Check all variables exist before subsetting
      check_variables_exist(expr_vars, data,
                            paste0("numeric expression '", formula_spec$label, "'"))

      # Create subset with only needed variables
      data_subset <- data[, expr_vars, drop = FALSE]
      eval_data <- prepare_eval_data(data_subset)
      expr_result <- rlang::eval_tidy(formula_spec$components$expr, eval_data)
      if (!is.numeric(expr_result)) {
        stop("Expression must evaluate to numeric values")
      }
      if (length(expr_result) != n) {
        stop("Expression must return vector of length ", n)
      }
      return(create_expression_array(expr_result, formula_spec, expr_vars, dpdict))
    }, error = function(e) {
      stop("Error evaluating expression '", formula_spec$label, "': ", e$message, call. = FALSE)
    })

  } else if (formula_spec$type == "subtraction") {
    # Handle subtraction
    comp1_array <- formula_to_array(formula_spec$components[[1]], data)
    comp2_array <- formula_to_array(formula_spec$components[[2]], data)
    result <- result * (comp1_array - comp2_array)

    # For subtraction, preserve meta from first component if available
    first_meta <- attr(comp1_array, "meta")
    if (!is.null(first_meta)) {
      result <- .ensure_meta(
        result,
        ivar = first_meta$ivar,
        ival = first_meta$ival,
        label = formula_spec$label %||% first_meta$label
      )
    }
    return(result)
  } else if (formula_spec$type == "gated") {
    # Process the inner spec and attach the gate
    inner_result <- formula_to_array(formula_spec$inner_spec, data, dpdict)

    # Check if inner result is multi-column
    if (is.list(inner_result) && isTRUE(attr(inner_result, "is_multi_column"))) {
      # Multi-column result - attach gate to each array
      for (arr_name in names(inner_result)) {
        if (!is.null(inner_result[[arr_name]])) {
          attr(inner_result[[arr_name]], "cell_gate") <- formula_spec$gate
        }
      }
      return(inner_result)
    } else {
      # Single array result - attach gate and return
      attr(inner_result, "cell_gate") <- formula_spec$gate
      return(inner_result)
    }

  } else if (formula_spec$type == "total") {
    # This represents "all data" - when no specific column is specified
    # Not a summary, just an array of 1s representing the full dataset
    arr <- rep(1, n)
    .ensure_meta(
      arr,
      ivar = "ALL",  # More generic than "_SUMMARY"
      ival = NULL,
      label = formula_spec$label %||% "Total"
    )

  } else {
    stop("Unknown formula type: ", formula_spec$type)
  }

  # Handle NAs
  result[is.na(result)] <- 0

  return(result)
}

#' Process helper functions
#' @keywords internal
process_helper <- function(formula_spec, data, dpdict) {

  helper_type <- formula_spec$helper_type

  # Get the helper from registry
  helper_obj <- .tab_registry$helpers[[helper_type]]
  if (is.null(helper_obj)) {
    stop("Unknown helper: '", helper_type, "'. Available: ",
         paste(names(.tab_registry$helpers), collapse = ", "))
  }

  # Recursively evaluate arguments
  evaluated_args <- list()
  arg_names <- names(formula_spec$args)
  for (i in seq_along(formula_spec$args)) {
    arg <- formula_spec$args[[i]]

    # If argument is itself a helper or complex expression, evaluate it recursively
    if (rlang::is_call(arg)) {
      fn_name <- as.character(arg[[1]])
      if (fn_name %in% names(.tab_registry$helpers)) {
        # It's a nested helper - parse and evaluate recursively
        nested_args <- rlang::call_args(arg)
        nested_arg_names <- rlang::call_args_names(arg)
        if (length(nested_arg_names) > 0) {
          names(nested_args) <- nested_arg_names
        }

        nested_spec <- list(
          type = "helper",
          helper_type = fn_name,
          args = nested_args,
          label = rlang::as_label(arg)
        )
        evaluated_args[[i]] <- process_helper(nested_spec, data, dpdict)  # Recursive call
      } else {
        # It's a regular expression - evaluate in data context
        tryCatch({
          # Extract variables referenced in the argument expression
          expr_vars <- all.vars(arg)

          # Check variables exist
          check_variables_exist(expr_vars, data,
                                paste0("helper '", helper_type, "' argument ", i))

          # Create subset with only needed variables
          data_subset <- data[, expr_vars, drop = FALSE]
          eval_data <- prepare_eval_data(data_subset)
          evaluated_args[[i]] <- rlang::eval_tidy(arg, eval_data)
        }, error = function(e) {
          stop("Error evaluating argument ", i, " in helper '", helper_type, "': ", e$message, call. = FALSE)
        })
      }
    } else if (rlang::is_symbol(arg)) {
      # Variable name - convert to string instead of evaluating
      evaluated_args[[i]] <- as.character(arg)
    } else {
      # Simple argument - evaluate directly

      if (length(all.vars(arg)) > 0) {
        # If it contains variables, check they exist
        expr_vars <- all.vars(arg)
        check_variables_exist(expr_vars, data,
                              paste0("helper '", helper_type, "' argument ", i))
      }

      tryCatch({
        # Extract variables referenced in the argument expression
        expr_vars <- all.vars(arg)
        # Create subset with only needed variables
        data_subset <- data[, expr_vars, drop = FALSE]
        eval_data <- prepare_eval_data(data_subset)
        evaluated_args[[i]] <- rlang::eval_tidy(arg, eval_data)
      }, error = function(e) {
        stop("Error evaluating argument ", i, " in helper '", helper_type, "': ", e$message, call. = FALSE)
      })
    }
  }
  # Restore argument names after evaluation
  if (!is.null(arg_names)) {
    names(evaluated_args) <- arg_names
  }

  # Create formula_spec with evaluated components for the processor
  processed_spec <- list(
    type = "helper",
    helper_type = helper_type,
    components = evaluated_args,
    label = formula_spec$label
  )

  if (!is.null(formula_spec$row_labels)) {
    processed_spec$row_labels <- formula_spec$row_labels
  }

  # Dispatch to the helper's processor
  tryCatch({
    result <- helper_obj$processor(processed_spec, data, dpdict = dpdict)

    if (is.list(result) && !is.numeric(result)) {
      # Helper returned a list of arrays (multi-column helper)
      # Validate each array in the list
      for (name in names(result)) {
        if (!is.numeric(result[[name]]) || length(result[[name]]) != nrow(data)) {
          stop("Helper '", helper_type, "' returned invalid result for '", name,
               "'. Expected numeric vector of length ", nrow(data))
        }
      }
      # Return the list with helper metadata
      attr(result, "is_multi_column") <- TRUE
      attr(result, "helper_type") <- helper_type
      return(result)
    } else {
      # Single array return
      if (!is.numeric(result) || length(result) != nrow(data)) {
        stop("Helper '", helper_type, "' returned invalid result. Expected numeric vector of length ", nrow(data))
      }
      return(result)
    }
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
    statistic <- .tab_registry$stats[[statistic]]
    if (is.null(statistic)) {
      stop("Unknown statistic: '", statistic, "'. Available: ",
           paste(names(.tab_registry$stats), collapse = ", "))
    }
    statistic <- statistic
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


#' Compute values for multiple cells using vectorized operations
#'
#' @param base_array Base array for filtering
#' @param row_arrays List of row arrays
#' @param col_arrays List of column arrays
#' @param statistic Type of statistic to compute
#' @param values Optional values array for mean calculations
#' @return Matrix of computed cell values
#' @keywords internal
compute_cells_vectorized <- function(base_array, row_arrays, col_arrays,
                                     statistic = "column_pct", values = NULL) {

  n_rows <- length(row_arrays)
  n_cols <- length(col_arrays)
  res    <- matrix(NA_real_, n_rows, n_cols)

  stat_fun <- statistic$processor

  for (i in seq_len(n_rows)) {
    r_arr   <- row_arrays[[i]]
    r_gate  <- attr(r_arr, "cell_gate")
    r_meta  <- attr(r_arr, "meta")

    if (is.null(r_gate)) r_gate <- always_true_gate
    if (is.null(r_meta)) r_meta <- list()

    for (j in seq_len(n_cols)) {
      c_arr  <- col_arrays[[j]]
      c_gate <- attr(c_arr, "cell_gate")
      c_meta <- attr(c_arr, "meta")

      if (is.null(c_gate)) c_gate <- always_true_gate
      if (is.null(c_meta)) c_meta <- list()

      # Create cell context with all required information
      cell_ctx <- list(
        stat_id = statistic$id,
        base_array = base_array,
        values_array = values
      )

      # Apply gates: both row and column gates must pass
      # This enables orthogonal filtering from both dimensions
      cell_ok <- r_gate(r_meta, c_meta, cell_ctx) && c_gate(r_meta, c_meta, cell_ctx)

      if (!cell_ok) {
        res[i, j] <- 0            # or NA_real_ if you prefer blanks
      } else {
        res[i, j] <- stat_fun(
          base_array   = base_array,
          row_array    = r_arr,
          col_array    = c_arr,
          values = values
        )
      }
    }
  }
  res
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

  # Create a copy of the data for processing
  output_data <- tab_result

  x_formatted <- output_data
  x           <- output_data

  # Identify the base row
  base_row_idx <- which(x_formatted$row_label == statistic$base_label)

  # Apply formatting function to all non-base rows
  # Apply formatting function to all non-base rows
  for (col in names(output_data)[-1]) {
    # Check if base column
    is_base_column <- col == statistic$base_label
    if (is_base_column) {
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
              return(statistic$format_fn(val))
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
    if (statistic$id == "mean" && !is.null(attr(tab_result, "values_variable"))) {
      values_var <- attr(tab_result, "values_variable")
      call_text <- deparse(original_call, width.cutoff = 500)
      source_info <- paste0("Table showing mean of '", values_var, "' generated by surveydatar::", paste(call_text, collapse = " "), ")")
    } else {
      call_text <- deparse(original_call, width.cutoff = 500)
      source_info <- paste0("Table showing survey ", statistic$id, " data generated by surveydatar::", paste(call_text, collapse = " "), ")")
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
    summary_array <- as.numeric(rowSums(combined_matrix, na.rm = TRUE) > 0)
  } else if (type == "Avg") {
    # Union for averaging (same as NET but labeled differently)
    combined_matrix <- do.call(cbind, arrays)
    summary_array <- as.numeric(rowSums(combined_matrix, na.rm = TRUE) > 0)
  } else if (type == "Total") {
    # Simple sum
    summary_array <- rep(1, length(base_array))
  } else {
    stop("Unknown summary type: ", type)
  }

  # Create label showing what was combined
  array_labels <- names(arrays)
  if (is.null(array_labels)) {
    # Try to extract labels from array meta if no names
    array_labels <- sapply(arrays, function(arr) {
      meta <- attr(arr, "meta")
      if (!is.null(meta) && !is.null(meta$label)) {
        meta$label
      } else {
        "unknown"
      }
    })
  }

  combined_label <- paste0(type, " (", paste(array_labels, collapse = " + "), ")")

  summary_array <- .ensure_meta(
    summary_array,
    ivar = "_SUMMARY",
    ival = type,  # "NET", "Total", or "Avg"
    label = combined_label
  )

  return(summary_array)
}

#' Add significance testing to a tab result
#'
#' @param tab_result A tab_result object from tab()
#' @param versus Column to compare against: "first_col", "last_col",
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

  meta_labels <- c(statistic$base_label)
  if (!is.null(statistic$summary_row)) {
    meta_labels <- c(meta_labels, statistic$summary_row)
  }
  if (!is.null(statistic$summary_col)) {
    meta_labels <- c(meta_labels, statistic$summary_col)
  }

  # Identify which columns and rows to exclude
  col_exclude <- which(colnames(result_matrix) %in% meta_labels)
  row_exclude <- which(row_labels %in% meta_labels)

  # Get data-only indices
  data_cols <- setdiff(seq_len(ncol(result_matrix)), col_exclude)
  data_rows <- setdiff(seq_len(nrow(result_matrix)), row_exclude)

  # Filter to data-only elements
  result_matrix <- result_matrix[data_rows, data_cols, drop = FALSE]
  row_arrays <- row_arrays[data_rows]
  col_arrays <- col_arrays[data_cols]
  if (!is.null(row_labels)) {
    row_labels <- row_labels[data_rows]
  }
  col_names <- colnames(result_matrix)

  # Determine test to use
  if (config$test == "auto") {
    # Auto-determine based on statistic type
    test_id <- switch(
      statistic$id,
      "column_pct" = "z_test_proportions",
      "count" = "z_test_proportions",
      "row_pct" = "z_test_proportions",
      "mean" = "t_test",
      "median" = "mann_whitney",
      "sd" = "t_test",
      "cv" = "t_test",
      stop("No default test for statistic '", statistic$id, "'")
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
  exclude_row_labels <- c(statistic$base_label)
  if (!is.null(statistic$summary_row)) {
    exclude_row_labels <- c(exclude_row_labels, statistic$summary_row)
  }

  # Find indices of rows to exclude
  exclude_rows <- which(row_labels %in% exclude_row_labels)

  # Determine actual data rows
  all_rows <- seq_len(nrow(result_matrix))
  data_rows <- setdiff(all_rows, exclude_rows)

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
    # Map data_rows indices to row_arrays indices
    array_indices <- seq_along(data_rows)
    if (length(array_indices) > length(row_arrays)) {
      array_indices <- array_indices[seq_len(length(row_arrays))]
    }
    data_row_arrays <- row_arrays[array_indices]

    # For omnibus tests, also exclude summary columns
    data_col_arrays <- col_arrays
    if (!is.null(statistic$summary_col)) {
      # Find indices of data columns (excluding summary column)
      data_col_indices <- which(col_names != statistic$summary_col)
      data_col_arrays <- col_arrays[data_col_indices]
    }

    # Run omnibus test once
    test_result <- test_obj$processor(
      base_array = base_array,
      row_arrays = data_row_arrays,
      col_arrays = data_col_arrays,
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

  # Remove base array from end of row_arrays if present
  if (length(row_arrays) > n_data_rows) {
    row_arrays <- row_arrays[seq_len(n_data_rows)]
  }

  # Test each data row against the base column
  for (i in seq_len(n_data_rows)) {
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
          row_array = row_arrays[[array_idx]],
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
