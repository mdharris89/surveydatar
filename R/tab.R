#' Create cross-tabulation tables with flexible formula syntax
#'
#' @param data Data frame or survey_data object
#' @param rows Row specification (variable name, expression, or named list)
#' @param cols Column specification (optional)
#' @param filter Table-wide filter expression (optional)
#' @param weight Weight variable name (optional)
#' @param statistic Type of statistic: "column_pct", "count", "row_pct", "mean"
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
                ...) {

  statistic <- match.arg(statistic)

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
    filter_parsed <- parse_table_formula(filter_quo, data, dpdict)
    filter_logic <- formula_to_array(filter_parsed, data)
    base_array <- base_array * filter_logic
  }

  # Parse row specifications
  rows_quo <- rlang::enquo(rows)
  rows_expr <- rlang::quo_get_expr(rows_quo)

  # Check if it's traditional string syntax
  if (rlang::is_string(rows_expr)) {
    rows_parsed <- list(parse_table_formula(rows_quo, data, dpdict))
  } else {
    # Try to evaluate to see if it's a rows_list
    rows_eval <- tryCatch(
      rlang::eval_tidy(rows_quo),
      error = function(e) NULL
    )
    # Check for tab_helper BEFORE checking is_list (since tab_helper inherits from list)
    if (inherits(rows_eval, "tab_helper")) {
      # It's a single helper function
      rows_parsed <- list(parse_table_formula(rows_quo, data, dpdict))
    } else if (rlang::is_list(rows_eval)) {
      # It's a rows_list
      rows_parsed <- lapply(seq_along(rows_eval), function(i) {
        parsed <- parse_table_formula(rows_eval[[i]], data, dpdict)
        parsed$label <- names(rows_eval)[i]
        parsed
      })
    } else {
      # It's a formula expression
      rows_parsed <- list(parse_table_formula(rows_quo, data, dpdict))
    }
  }

  # Parse column specifications
  if (!missing(cols)) {
    cols_quo <- rlang::enquo(cols)
    cols_expr <- rlang::quo_get_expr(cols_quo)

    if (rlang::is_string(cols_expr)) {
      cols_parsed <- list(parse_table_formula(cols_quo, data, dpdict))
    } else {
      # Try to evaluate safely (in case there are column helper functions in future)
      cols_eval <- tryCatch(
        rlang::eval_tidy(cols_quo),
        error = function(e) NULL
      )
      # Check for tab_helper BEFORE checking is_list (since tab_helper inherits from list)
      if (inherits(cols_eval, "tab_helper")) {
        # It's a single helper function
        cols_parsed <- list(parse_table_formula(cols_quo, data, dpdict))
      } else if (rlang::is_list(cols_eval)) {
        # Handle list-based column specs if implemented in future
        cols_parsed <- lapply(seq_along(cols_eval), function(i) {
          parsed <- parse_table_formula(cols_eval[[i]], data, dpdict)
          parsed$label <- names(cols_eval)[i]
          parsed
        })
      } else {
        # It's a formula expression
        cols_parsed <- list(parse_table_formula(cols_quo, data, dpdict))
      }
    }
  } else {
    cols_parsed <- NULL
  }

  # Expand variables for rows
  rows_expanded <- list()
  for (row_spec in rows_parsed) {
    expanded <- expand_variables(row_spec, data, dpdict)
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
      expanded <- expand_variables(col_spec, data, dpdict)
      cols_expanded <- append(cols_expanded, expanded)
    }
  } else {
    cols_expanded <- list(list(type = "total", label = "Total"))
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

  # Compute each cell
  for (i in seq_len(n_rows)) {
    for (j in seq_len(n_cols)) {
      result_matrix[i, j] <- compute_cell(
        base_array = base_array,
        row_array = row_arrays[[i]],
        col_array = col_arrays[[j]],
        statistic = statistic
      )
    }
  }

  # Add NET/Avg row or column based on statistic
  if (statistic %in% c("count", "column_pct")) {
    # Create NET row - union of all row conditions
    if (length(row_arrays) > 1) {
      combined_row_matrix <- do.call(cbind, row_arrays)
      net_row_array <- as.numeric(rowSums(combined_row_matrix) > 0)

      # Calculate NET values for each column
      net_values <- numeric(n_cols)
      for (j in seq_len(n_cols)) {
        net_values[j] <- compute_cell(
          base_array = base_array,
          row_array = net_row_array,
          col_array = col_arrays[[j]],
          statistic = statistic
        )
      }

      # Add NET row to result matrix
      result_matrix <- rbind(result_matrix, net_values)
      n_rows <- n_rows + 1
    }

  } else if (statistic == "row_pct") {
    # Create NET column - union of all column conditions
    if (length(col_arrays) > 1) {
      combined_col_matrix <- do.call(cbind, col_arrays)
      net_col_array <- as.numeric(rowSums(combined_col_matrix) > 0)

      # Calculate NET values for each row
      net_values <- numeric(n_rows)
      for (i in seq_len(n_rows)) {
        net_values[i] <- compute_cell(
          base_array = base_array,
          row_array = row_arrays[[i]],
          col_array = net_col_array,
          statistic = statistic
        )
      }

      # Add NET column to result matrix
      result_matrix <- cbind(result_matrix, net_values)
      n_cols <- n_cols + 1
    }

  } else if (statistic == "mean") {
    # Create Avg row - average across all row conditions
    if (length(row_arrays) > 1) {
      combined_row_matrix <- do.call(cbind, row_arrays)
      avg_row_array <- as.numeric(rowSums(combined_row_matrix) > 0)

      # Calculate Avg values for each column (but we need values array for mean)
      # For now, this will need the values parameter to be implemented in compute_cell
      avg_values <- numeric(n_cols)
      for (j in seq_len(n_cols)) {
        avg_values[j] <- compute_cell(
          base_array = base_array,
          row_array = avg_row_array,
          col_array = col_arrays[[j]],
          statistic = "mean"
          # Note: This will need values parameter when mean is fully implemented
        )
      }

      # Add Avg row to result matrix
      result_matrix <- rbind(result_matrix, avg_values)
      n_rows <- n_rows + 1
    }
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

  # Add NET/Avg label if applicable
  if (statistic %in% c("count", "column_pct") && length(row_arrays) > 1) {
    row_labels <- c(row_labels, "NET")
  } else if (statistic == "mean" && length(row_arrays) > 1) {
    row_labels <- c(row_labels, "Avg")
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

  # Add NET column label if applicable
  if (statistic == "row_pct" && length(col_arrays) > 1) {
    col_labels <- c(col_labels, "NET")
  }

  names(result_df)[-1] <- col_labels

  # Add base sizes as a row in the table
  col_bases <- sapply(col_arrays, function(x) sum(base_array * x))
  base_row <- data.frame(
    row_label = "Base (n)",
    stringsAsFactors = FALSE
  )

  # Add NET column base if applicable
  if (statistic == "row_pct" && length(col_arrays) > 1) {
    combined_col_matrix <- do.call(cbind, col_arrays)
    net_col_array <- as.numeric(rowSums(combined_col_matrix) > 0)
    net_col_base <- sum(base_array * net_col_array)
    col_bases <- c(col_bases, net_col_base)
  }

  base_row <- data.frame(
    row_label = "Base (n)",
    stringsAsFactors = FALSE
  )

  # Add base sizes for each column
  for (i in seq_along(col_bases)) {
    base_row[[names(result_df)[i + 1]]] <- col_bases[i]
  }

  # Add base row to the result
  result_df <- rbind(result_df, base_row)

  # Store statistic
  attr(result_df, "statistic") <- statistic

  # Store the original call for sourcing when using copy_tab
  attr(result_df, "call") <- match.call()

  class(result_df) <- c("tab_result", "data.frame")

  return(result_df)
}

#' Print method for tab_result
#' @export
print.tab_result <- function(x, ...) {
  statistic <- attr(x, "statistic")
  cat("\nCross-tabulation (", statistic, ")\n", sep = "")
  cat(rep("-", 50), "\n", sep = "")

  # Format percentages
  if (statistic %in% c("column_pct", "row_pct")) {
    for (col in names(x)[-1]) {
      x[[col]] <- ifelse(
        is.numeric(x[[col]]),
        sprintf("%.1f%%", x[[col]]),
        as.character(x[[col]])
      )
    }
  } else if (statistic == "mean") {
    for (col in names(x)[-1]) {
      x[[col]] <- ifelse(
        is.numeric(x[[col]]),
        sprintf("%.2f", x[[col]]),
        as.character(x[[col]])
      )
    }
  } else {
    for (col in names(x)[-1]) {
      x[[col]] <- as.character(x[[col]])
    }
  }

  print.data.frame(x, row.names = FALSE, ...)

  # Print base sizes
  col_bases <- attr(x, "col_bases")
  if (!is.null(col_bases)) {
    cat("\nBase: ", paste(col_bases, collapse = " | "), "\n", sep = "")
  }
}


#' Parse table formula expressions using NSE
#'
#' @param expr An expression captured using rlang::enquo
#' @param data The data frame to evaluate against
#' @param dpdict Optional data dictionary for metadata
#' @return A list with type, components, and label
#' @keywords internal
parse_table_formula <- function(expr, data, dpdict = NULL) {
  # Extract expression from quosure if needed
  if (rlang::is_quosure(expr)) {
    actual_expr <- rlang::quo_get_expr(expr)
  } else {
    actual_expr <- expr
  }

  # Check if it's a tab_helper object (evaluated helper function)
  if (inherits(actual_expr, "tab_helper")) {
    helper_type <- attr(actual_expr, "helper_type")
    return(list(
      type = "helper",
      helper_type = helper_type,
      components = actual_expr,  # The helper object itself
      label = paste0(helper_type, "(", deparse(actual_expr$var), ", ", actual_expr$n, ")")
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
    if (fn_name %in% c("top_box", "bottom_box", "index_to", "change_from")) {
      return(list(
        type = "helper",
        helper_type = fn_name,
        components = rlang::call_args(actual_expr),
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
#' @return List of expanded variable specifications
#' @keywords internal
expand_variables <- function(var_spec, data, dpdict = NULL) {
  # Handle complex expressions by recursively expanding components
  if (is.list(var_spec) && !is.null(var_spec$type)) {

    # Helper functions should not be expanded
    if (var_spec$type == "helper") {
      return(list(var_spec))
    }

    if (var_spec$type == "multiplication") {
      # Expand each component and return all combinations
      expanded_components <- lapply(var_spec$components, function(comp) {
        expand_variables(comp, data, dpdict)
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

  # Check if it's a pattern match in data (existing logic)
  pattern_matches <- names(data)[grepl(paste0("^", var_name, "_\\d+"), names(data))]
  if (length(pattern_matches) > 0) {
    return(lapply(pattern_matches, function(v) {
      list(type = "simple", components = list(var = v), label = get_var_label(v, dpdict))
    }))
  }

  # Check if it's a categorical variable that needs expansion (existing logic)
  if (var_name %in% names(data)) {
    var_data <- data[[var_name]]

    # Check if it's labelled
    if (inherits(var_data, "haven_labelled")) {
      labels <- attr(var_data, "labels")
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

  if (inherits(formula_spec$components, "tab_helper")) {
    # Handle already-evaluated helper
    helper_obj <- formula_spec$components

    if (helper_type == "top_box") {
      var_name <- as.character(helper_obj$var)
      n <- helper_obj$n

      if (!var_name %in% names(data)) {
        stop("Variable '", var_name, "' not found in data")
      }

      var_data <- data[[var_name]]

      # Get unique values and determine top n
      if (inherits(var_data, "haven_labelled")) {
        labels <- attr(var_data, "labels")
        top_values <- sort(labels, decreasing = TRUE)[1:n]
      } else {
        unique_vals <- sort(unique(na.omit(var_data)), decreasing = TRUE)
        top_values <- unique_vals[1:n]
      }

      return(as.numeric(var_data %in% top_values))

    } else if (helper_type == "bottom_box") {
      var_name <- as.character(helper_obj$var)
      n <- helper_obj$n

      if (!var_name %in% names(data)) {
        stop("Variable '", var_name, "' not found in data")
      }

      var_data <- data[[var_name]]

      # Get unique values and determine bottom n
      if (inherits(var_data, "haven_labelled")) {
        labels <- attr(var_data, "labels")
        bottom_values <- sort(labels)[1:n]
      } else {
        unique_vals <- sort(unique(na.omit(var_data)))
        bottom_values <- unique_vals[1:n]
      }

      return(as.numeric(var_data %in% bottom_values))
    }

  } else {
    # Handle unevaluated expressions (existing code)
    args <- formula_spec$components

    if (helper_type == "top_box") {
      # Properly extract variable name from quoted expression
      var_name <- if (is.symbol(args[[1]])) {
        as.character(args[[1]])
      } else if (is.call(args[[1]])) {
        deparse(args[[1]])
      } else {
        as.character(args[[1]])
      }

      n <- if (length(args) > 1) args[[2]] else 1

      if (!var_name %in% names(data)) {
        stop("Variable '", var_name, "' not found in data")
      }

      var_data <- data[[var_name]]

      # Get unique values and determine top n
      if (inherits(var_data, "haven_labelled")) {
        labels <- attr(var_data, "labels")
        top_values <- sort(labels, decreasing = TRUE)[1:n]
      } else {
        unique_vals <- sort(unique(na.omit(var_data)), decreasing = TRUE)
        top_values <- unique_vals[1:n]
      }

      return(as.numeric(var_data %in% top_values))

    } else if (helper_type == "bottom_box") {
      # Same fix for bottom_box
      var_name <- if (is.symbol(args[[1]])) {
        as.character(args[[1]])
      } else if (is.call(args[[1]])) {
        deparse(args[[1]])
      } else {
        as.character(args[[1]])
      }

      n <- if (length(args) > 1) args[[2]] else 1

      var_data <- data[[var_name]]

      # Get unique values and determine bottom n
      if (inherits(var_data, "haven_labelled")) {
        labels <- attr(var_data, "labels")
        bottom_values <- sort(labels)[1:n]
      } else {
        unique_vals <- sort(unique(na.omit(var_data)))
        bottom_values <- unique_vals[1:n]
      }

      return(as.numeric(var_data %in% bottom_values))
    }
  }

  # Placeholder for other helpers
  return(rep(1, nrow(data)))
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
  structure(
    list(var = substitute(var), n = n),
    class = "tab_helper",
    helper_type = "top_box"
  )
}

#' Select bottom n response options
#'
#' @param var Variable name or expression
#' @param n Number of bottom options to select (default 1)
#' @return Expression for use in tab()
#' @export
bottom_box <- function(var, n = 1) {
  structure(
    list(var = substitute(var), n = n),
    class = "tab_helper",
    helper_type = "bottom_box"
  )
}

#' Create indexed values relative to a base
#'
#' @param var Variable name or expression
#' @param base Base for indexing (default is total)
#' @return Expression for use in tab()
#' @export
index_to <- function(var, base = NULL) {
  structure(
    list(var = substitute(var), base = substitute(base)),
    class = "tab_helper",
    helper_type = "index_to"
  )
}

#' Calculate change from a baseline
#'
#' @param var Variable name or expression
#' @param condition Condition defining baseline
#' @return Expression for use in tab()
#' @export
change_from <- function(var, condition) {
  structure(
    list(var = substitute(var), condition = substitute(condition)),
    class = "tab_helper",
    helper_type = "change_from"
  )
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

  if (statistic == "column_pct") {
    # Sequential: base -> column snapshot -> apply row condition
    column_snapshot <- base_array * col_array
    final_array <- column_snapshot * row_array

    column_total <- sum(column_snapshot)
    if (column_total == 0) return(NA)
    return(sum(final_array) / column_total * 100)

  } else if (statistic == "row_pct") {
    # Sequential: base -> row snapshot -> apply column condition
    row_snapshot <- base_array * row_array
    final_array <- row_snapshot * col_array

    row_total <- sum(row_snapshot)
    if (row_total == 0) return(NA)
    return(sum(final_array) / row_total * 100)

  } else if (statistic == "count") {
    # Either approach gives same result for counts
    final_array <- base_array * row_array * col_array
    return(sum(final_array))

  } else if (statistic == "mean" && !is.null(values)) {
    # Use count approach for denominator
    final_array <- base_array * row_array * col_array
    numerator <- sum(final_array * values)
    denominator <- sum(final_array)
    if (denominator == 0) return(NA)
    return(numerator / denominator)

  } else {
    stop("Unsupported statistic: ", statistic)
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
  if (is.null(statistic)) statistic <- "unknown"

  # Create a copy of the data for processing
  output_data <- tab_result

  # Convert percentages to decimals for percentage statistics
  if (statistic %in% c("column_pct", "row_pct")) {
    # Convert all numeric columns (excluding row_label) from percentages to decimals
    numeric_cols <- sapply(output_data[, -1, drop = FALSE], is.numeric)
    output_data[, -1][, numeric_cols] <- output_data[, -1][, numeric_cols] / 100
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
  call_text <- deparse(original_call, width.cutoff = 500)
  source_info <- paste0("Table showing survey", statistic, "data generated by surveydatar::", paste(call_text, collapse = " "), ")")
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
