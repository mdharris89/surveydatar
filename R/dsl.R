#### DSL System for Semantic Cell Matching #####
#' Implements DSL for tab cells using natural R expression syntax.
#' Enables semantic introspection, normalization, and matching.

#' Validate DSL Expression
#'
#' Checks that expression uses only allowed operations for semantic matching.
#'
#' @param expr R expression (call, symbol, or literal)
#' @return TRUE if valid (invisibly), errors if invalid
#' @keywords internal
validate_dsl_expr <- function(expr) {
  # Allowed operations for DSL
  allowed_ops <- c(
    # Comparisons
    "==", "!=", ">", "<", ">=", "<=", "%in%",
    # Logical
    "&", "|", "!",
    # Special
    "between",
    # Utility
    "c",  # For creating vectors in %in%
    "("   # Parentheses for grouping
  )
  
  # Base cases
  if (is.symbol(expr)) return(invisible(TRUE))
  if (is.numeric(expr) || is.character(expr) || is.logical(expr)) return(invisible(TRUE))
  if (identical(expr, quote(TRUE)) || identical(expr, quote(FALSE))) return(invisible(TRUE))
  
  if (!is.call(expr)) {
    stop("DSL expression must be a call, symbol, or literal. Got: ", class(expr))
  }
  
  # Check function name
  fn <- as.character(expr[[1]])
  
  if (!fn %in% allowed_ops) {
    stop("DSL expression uses disallowed operation: '", fn, "'\n",
         "Allowed operations: ", paste(allowed_ops, collapse = ", "), "\n",
         "For custom logic, use set_array_meta(arr, dsl = NULL, ...)")
  }
  
  # Recursively validate arguments
  if (length(expr) > 1) {
    for (i in 2:length(expr)) {
      validate_dsl_expr(expr[[i]])
    }
  }
  
  invisible(TRUE)
}

#' Evaluate DSL Expression
#'
#' Evaluates a validated DSL expression in the context of data.
#'
#' @param expr DSL expression
#' @param data Data frame for evaluation context
#' @return Numeric vector of length nrow(data)
#' @keywords internal
eval_dsl <- function(expr, data) {
  # Validate first
  validate_dsl_expr(expr)
  
  # Evaluate in data context
  result <- eval(expr, data, parent.frame())
  
  # Ensure result is appropriate type and length
  if (!is.numeric(result) && !is.logical(result)) {
    stop("DSL expression must evaluate to numeric or logical vector")
  }
  
  if (length(result) != nrow(data)) {
    stop("DSL expression must return vector of length nrow(data)")
  }
  
  # Convert logical to numeric (TRUE → 1, FALSE → 0)
  if (is.logical(result)) {
    result <- as.numeric(result)
  }
  
  return(result)
}

#' Normalize DSL Expression
#'
#' Converts DSL expression to canonical form for semantic matching.
#' Applies data-independent and data-aware normalization rules.
#'
#' @param expr DSL expression
#' @param data Optional data frame for data-aware normalization
#' @param dpdict Optional data dictionary
#' @param max_unique Maximum unique values for threshold-to-values conversion (default 11)
#' @return Normalized expression
#' @keywords internal
normalize_dsl <- function(expr, data = NULL, dpdict = NULL, max_unique = 11) {
  
  # Base cases
  if (is.symbol(expr)) return(expr)
  if (is.numeric(expr) || is.character(expr) || is.logical(expr)) return(expr)
  if (identical(expr, quote(TRUE)) || identical(expr, quote(FALSE))) return(expr)
  if (!is.call(expr)) return(expr)
  
  fn <- as.character(expr[[1]])
  
  # DATA-AWARE NORMALIZATIONS
  
  if (!is.null(data)) {
    # Threshold comparisons → value sets
    if (fn %in% c(">", "<", ">=", "<=")) {
      normalized_threshold <- normalize_threshold_to_values(expr, data, max_unique)
      if (!is.null(normalized_threshold)) return(normalized_threshold)
    }
    
    # NOT with threshold
    if (fn == "!" && is.call(expr[[2]])) {
      inner_fn <- as.character(expr[[2]][[1]])
      if (inner_fn %in% c(">", "<", ">=", "<=")) {
        # Invert comparison
        inverted <- invert_comparison(expr[[2]])
        normalized <- normalize_threshold_to_values(inverted, data, max_unique)
        if (!is.null(normalized)) return(normalized)
      }
    }
  }
  
  # STRUCTURAL NORMALIZATIONS
  
  # OR-equals chain → %in%
  if (fn == "|" && is_or_equals_chain(expr)) {
    var <- extract_common_variable_from_or_chain(expr)
    vals <- sort(extract_all_values_from_or_chain(expr))
    return(call("%in%", var, vals))
  }
  
  # Flatten and sort associative operators
  if (fn %in% c("&", "|")) {
    args <- flatten_operator(expr, fn)
    normalized_args <- lapply(args, function(a) normalize_dsl(a, data, dpdict, max_unique))
    sorted_args <- sort_args_canonical(normalized_args)
    return(build_nary_call(fn, sorted_args))
  }
  
  # Push NOT inward (De Morgan's laws)
  if (fn == "!") {
    inner <- expr[[2]]
    
    # Unwrap parentheses
    while (is.call(inner) && as.character(inner[[1]]) == "(") {
      inner <- inner[[2]]
    }
    
    if (is.call(inner)) {
      inner_fn <- as.character(inner[[1]])
      
      if (inner_fn == "&") {
        # !(a & b) → !a | !b
        return(normalize_dsl(
          call("|", call("!", inner[[2]]), call("!", inner[[3]])),
          data, dpdict, max_unique
        ))
      }
      
      if (inner_fn == "|") {
        # !(a | b) → !a & !b
        return(normalize_dsl(
          call("&", call("!", inner[[2]]), call("!", inner[[3]])),
          data, dpdict, max_unique
        ))
      }
      
      if (inner_fn == "!") {
        # !!a → a
        return(normalize_dsl(inner[[2]], data, dpdict, max_unique))
      }
    }
  }
  
  # Sort values in %in%
  if (fn == "%in%") {
    var <- expr[[2]]
    vals <- expr[[3]]
    if (is.numeric(vals) || is.character(vals)) {
      return(call("%in%", normalize_dsl(var, data, dpdict, max_unique), sort(vals)))
    }
  }
  
  # Variable on left for comparisons
  if (fn %in% c(">", "<", ">=", "<=", "==", "!=")) {
    lhs <- expr[[2]]
    rhs <- expr[[3]]
    
    # If rhs is symbol and lhs is not, swap
    if (is.symbol(rhs) && !is.symbol(lhs)) {
      inverted_op <- switch(fn,
        ">"  = "<",
        "<"  = ">",
        ">=" = "<=",
        "<=" = ">=",
        "==" = "==",
        "!=" = "!="
      )
      return(call(inverted_op, rhs, lhs))
    }
  }
  
  # Recursively normalize arguments
  normalized_args <- lapply(expr[-1], function(a) normalize_dsl(a, data, dpdict, max_unique))
  as.call(c(list(expr[[1]]), normalized_args))
}

#' Normalize Threshold to Value Set (Data-Aware)
#'
#' Converts threshold comparisons to value sets when variable has small number
#' of distinct values. E.g., satisfaction > 3 becomes satisfaction %in% c(4, 5).
#'
#' @param expr Expression like call(">", as.symbol("var"), threshold)
#' @param data Data frame
#' @param max_unique Maximum unique values to convert (default 11)
#' @return Normalized expression or NULL if not applicable
#' @keywords internal
normalize_threshold_to_values <- function(expr, data, max_unique = 11) {
  if (!is.call(expr)) return(NULL)
  
  fn <- as.character(expr[[1]])
  if (!fn %in% c(">", "<", ">=", "<=")) return(NULL)
  
  lhs <- expr[[2]]
  rhs <- expr[[3]]
  
  # Check if lhs is symbol and rhs is literal
  if (!is.symbol(lhs)) return(NULL)
  if (is.symbol(rhs) || is.call(rhs)) return(NULL)
  
  var_name <- as.character(lhs)
  threshold <- rhs
  
  # Check variable exists in data
  if (!var_name %in% names(data)) return(NULL)
  
  # Get unique values
  var_data <- data[[var_name]]
  unique_vals <- sort(unique(stats::na.omit(var_data)))
  
  # Only convert if reasonably small value set
  if (length(unique_vals) > max_unique) return(NULL)
  
  # Find values satisfying condition
  matching_vals <- switch(fn,
    ">"  = unique_vals[unique_vals > threshold],
    "<"  = unique_vals[unique_vals < threshold],
    ">=" = unique_vals[unique_vals >= threshold],
    "<=" = unique_vals[unique_vals <= threshold]
  )
  
  if (length(matching_vals) == 0) {
    return(quote(FALSE))  # Impossible condition
  }
  
  # Return as %in% expression
  call("%in%", lhs, matching_vals)
}

#' Helper: Invert Comparison Operator
#' @keywords internal
invert_comparison <- function(expr) {
  fn <- as.character(expr[[1]])
  inverted_op <- switch(fn,
    ">"  = "<=",
    "<"  = ">=",
    ">=" = "<",
    "<=" = ">",
    "==" = "!=",
    "!=" = "=="
  )
  call(inverted_op, expr[[2]], expr[[3]])
}

#' Check if Expression is OR-Equals Chain
#'
#' Detects pattern: var == val1 | var == val2 | var == val3
#'
#' @param expr Expression to check
#' @return Logical
#' @keywords internal
is_or_equals_chain <- function(expr) {
  if (!is.call(expr)) return(FALSE)
  
  fn <- as.character(expr[[1]])
  if (fn != "|") return(FALSE)
  
  # Recursively check if all components are == on same variable
  components <- extract_or_components(expr)
  
  if (length(components) < 2) return(FALSE)
  
  # Check all are == calls
  all_equals <- all(sapply(components, function(comp) {
    is.call(comp) && as.character(comp[[1]]) == "=="
  }))
  
  if (!all_equals) return(FALSE)
  
  # Check all reference same variable
  vars <- sapply(components, function(comp) deparse(comp[[2]]))
  length(unique(vars)) == 1
}

#' Extract OR Components
#' @keywords internal
extract_or_components <- function(expr) {
  if (!is.call(expr) || as.character(expr[[1]]) != "|") {
    return(list(expr))
  }
  
  # Recursively flatten OR
  lhs <- extract_or_components(expr[[2]])
  rhs <- extract_or_components(expr[[3]])
  c(lhs, rhs)
}

#' Extract Common Variable from OR Chain
#' @keywords internal
extract_common_variable_from_or_chain <- function(expr) {
  components <- extract_or_components(expr)
  # All are == calls, return variable from first
  components[[1]][[2]]
}

#' Extract All Values from OR Chain
#' @keywords internal
extract_all_values_from_or_chain <- function(expr) {
  components <- extract_or_components(expr)
  sapply(components, function(comp) comp[[3]])
}

#' Flatten Associative Operator
#'
#' Converts (a & b) & c to list(a, b, c)
#'
#' @param expr Expression with associative operator
#' @param op Operator to flatten ("&" or "|")
#' @return List of flattened arguments
#' @keywords internal
flatten_operator <- function(expr, op) {
  if (!is.call(expr) || as.character(expr[[1]]) != op) {
    return(list(expr))
  }
  
  # Recursively flatten
  lhs <- flatten_operator(expr[[2]], op)
  rhs <- flatten_operator(expr[[3]], op)
  c(lhs, rhs)
}

#' Sort Arguments in Canonical Order
#'
#' Sorts expression arguments for normalization (alphabetically by deparse).
#'
#' @param args List of expressions
#' @return Sorted list
#' @keywords internal
sort_args_canonical <- function(args) {
  deparsed <- sapply(args, deparse1)
  args[order(deparsed)]
}

#' Build N-ary Call
#'
#' Builds nested binary calls from list: list(a, b, c) with & → a & b & c
#'
#' @param op Operator
#' @param args List of arguments
#' @return Nested call expression
#' @keywords internal
build_nary_call <- function(op, args) {
  if (length(args) == 1) return(args[[1]])
  if (length(args) == 2) return(call(op, args[[1]], args[[2]]))
  
  # Build left-associative: ((a & b) & c) & d
  Reduce(function(acc, arg) call(op, acc, arg), args)
}

#' Derive Label from DSL Expression
#'
#' Auto-generates display label from DSL expression using dpdict for lookups.
#'
#' @param dsl DSL expression
#' @param dpdict Optional data dictionary for label lookup
#' @return Character string label
#' @keywords internal
derive_label_from_dsl <- function(dsl, dpdict = NULL) {
  
  # Simple variable reference
  if (is.symbol(dsl)) {
    var_name <- as.character(dsl)
    if (!is.null(dpdict) && var_name %in% dpdict$variable_names) {
      label <- dpdict$variable_labels[dpdict$variable_names == var_name]
      if (!is.na(label) && nzchar(label)) return(label)
    }
    return(var_name)
  }
  
  # Literal
  if (!is.call(dsl)) {
    return(as.character(dsl))
  }
  
  fn <- as.character(dsl[[1]])
  
  # var %in% c(values)
  if (fn == "%in%") {
    var_sym <- dsl[[2]]
    var_name <- as.character(var_sym)
    var_label <- if (!is.null(dpdict) && var_name %in% dpdict$variable_names) {
      dpdict$variable_labels[dpdict$variable_names == var_name]
    } else {
      var_name
    }
    
    values <- dsl[[3]]
    value_labels <- get_value_labels_for_codes(var_name, values, dpdict)
    
    if (length(value_labels) > 0 && !any(is.na(value_labels))) {
      return(paste0(var_label, ": ", paste(value_labels, collapse = " or ")))
    } else {
      return(paste0(var_label, " %in% c(", paste(values, collapse = ", "), ")"))
    }
  }
  
  # var == value
  if (fn == "==") {
    var_name <- as.character(dsl[[2]])
    value <- dsl[[3]]
    
    var_label <- if (!is.null(dpdict) && var_name %in% dpdict$variable_names) {
      dpdict$variable_labels[dpdict$variable_names == var_name]
    } else {
      var_name
    }
    
    value_label <- get_value_labels_for_codes(var_name, value, dpdict)
    if (length(value_label) > 0 && !is.na(value_label[[1]])) {
      return(paste0(var_label, ": ", value_label[[1]]))
    } else {
      return(paste(var_label, "==", value))
    }
  }
  
  # Threshold comparisons
  if (fn %in% c(">", "<", ">=", "<=")) {
    var_name <- as.character(dsl[[2]])
    value <- dsl[[3]]
    
    var_label <- if (!is.null(dpdict) && var_name %in% dpdict$variable_names) {
      dpdict$variable_labels[dpdict$variable_names == var_name]
    } else {
      var_name
    }
    
    return(paste(var_label, fn, value))
  }
  
  # Compound expressions
  if (fn %in% c("&", "|")) {
    component_labels <- lapply(dsl[-1], function(comp) {
      derive_label_from_dsl(comp, dpdict)
    })
    sep <- if (fn == "&") " AND " else " OR "
    return(paste(component_labels, collapse = sep))
  }
  
  # NOT
  if (fn == "!") {
    inner_label <- derive_label_from_dsl(dsl[[2]], dpdict)
    return(paste("NOT", inner_label))
  }
  
  # Fallback: deparse
  deparse1(dsl)
}

#' Get Value Labels for Codes
#'
#' Looks up value labels from dpdict for given variable and value codes.
#'
#' @param var_name Variable name
#' @param values Value codes
#' @param dpdict Data dictionary
#' @return Character vector of labels
#' @keywords internal
get_value_labels_for_codes <- function(var_name, values, dpdict = NULL) {
  if (is.null(dpdict) || !var_name %in% dpdict$variable_names) {
    return(as.character(values))
  }
  
  var_idx <- match(var_name, dpdict$variable_names)
  value_labels <- dpdict$value_labels[[var_idx]]
  
  if (is.null(value_labels) || all(is.na(value_labels))) {
    return(as.character(values))
  }
  
  # value_labels is a character vector with names being the codes
  # e.g., c("Daily", "Weekly", "Monthly") with names c("1", "2", "3")
  labels <- character(length(values))
  for (i in seq_along(values)) {
    # Match code in names
    code_str <- as.character(values[i])
    if (code_str %in% names(value_labels)) {
      labels[i] <- value_labels[code_str]
    } else {
      labels[i] <- as.character(values[i])
    }
  }
  
  labels
}

#' DSL Get Variables
#'
#' Extracts all variable names from DSL expression.
#'
#' @param dsl DSL expression
#' @return Character vector of variable names
#' @export
dsl_get_variables <- function(dsl) {
  all.vars(dsl)
}

#' DSL Get Values
#'
#' Extracts all literal values from DSL expression where determinable.
#'
#' @param dsl DSL expression
#' @return Vector of values or NULL if not determinable
#' @keywords internal
dsl_get_values <- function(dsl) {
  if (is.numeric(dsl) || is.character(dsl)) return(dsl)
  if (!is.call(dsl)) return(NULL)
  
  fn <- as.character(dsl[[1]])
  
  if (fn %in% c("==", ">", "<", ">=", "<=", "!=")) {
    # Return rhs if it's a literal
    rhs <- dsl[[3]]
    if (is.numeric(rhs) || is.character(rhs)) return(rhs)
  }
  
  if (fn == "%in%") {
    # Return the value vector
    values <- dsl[[3]]
    # If it's a call to c(), evaluate it
    if (is.call(values) && as.character(values[[1]]) == "c") {
      values <- eval(values)
    }
    return(values)
  }
  
  if (fn %in% c("&", "|")) {
    # Try to extract from components
    lhs_vals <- dsl_get_values(dsl[[2]])
    rhs_vals <- dsl_get_values(dsl[[3]])
    
    if (!is.null(lhs_vals) && !is.null(rhs_vals)) {
      return(unique(c(lhs_vals, rhs_vals)))
    }
    
    return(lhs_vals %||% rhs_vals)
  }
  
  NULL
}

#' Extract Primary Variable from DSL
#'
#' Gets the main variable from a DSL expression (first encountered).
#'
#' @param dsl DSL expression
#' @return Character variable name or NULL
#' @keywords internal
extract_primary_variable <- function(dsl) {
  vars <- dsl_get_variables(dsl)
  if (length(vars) > 0) vars[1] else NULL
}

#' DSL References Variable
#'
#' Checks if DSL expression references a specific variable.
#'
#' @param dsl DSL expression
#' @param var_name Variable name to check
#' @return Logical
#' @keywords internal
dsl_references_variable <- function(dsl, var_name) {
  var_name %in% dsl_get_variables(dsl)
}

#' DSL Contains Value
#'
#' Checks if DSL expression selects a specific value (where determinable).
#'
#' @param dsl DSL expression  
#' @param value Value to check
#' @return Logical or NA if not determinable
#' @keywords internal
dsl_contains_value <- function(dsl, value) {
  if (!is.call(dsl)) return(NA)
  
  fn <- as.character(dsl[[1]])
  
  if (fn == "==") {
    return(identical(dsl[[3]], value))
  }
  
  if (fn == "%in%") {
    values <- dsl[[3]]
    # If values is a call to c(), evaluate it
    if (is.call(values) && as.character(values[[1]]) == "c") {
      values <- eval(values)
    }
    return(value %in% values)
  }
  
  if (fn %in% c(">", "<", ">=", "<=")) {
    # Can't determine without data
    return(NA)
  }
  
  if (fn == "|") {
    # Check any component
    lhs <- dsl_contains_value(dsl[[2]], value)
    rhs <- dsl_contains_value(dsl[[3]], value)
    
    if (!is.na(lhs) && lhs) return(TRUE)
    if (!is.na(rhs) && rhs) return(TRUE)
    
    return(NA)
  }
  
  if (fn == "&") {
    # Check all components
    lhs <- dsl_contains_value(dsl[[2]], value)
    rhs <- dsl_contains_value(dsl[[3]], value)
    
    if (!is.na(lhs) && !is.na(rhs)) {
      return(lhs && rhs)
    }
    
    return(NA)
  }
  
  NA
}

#' Convert Expression to DSL
#'
#' Checks if expression is valid DSL.
#'
#' @param expr R expression
#' @param data Optional data for context
#' @param dpdict Optional dpdict
#' @return Expression if valid DSL, NULL otherwise
#' @keywords internal
expr_to_dsl <- function(expr, data = NULL, dpdict = NULL) {
  tryCatch({
    validate_dsl_expr(expr)
    return(expr)
  }, error = function(e) {
    NULL
  })
}

#' Partial Containment Matching
#'
#' Checks if query_dsl is logically contained in cell_dsl (subset relationship).
#' Query matches if it's a subset of what cell computes.
#'
#' @param query_dsl Normalized query DSL
#' @param cell_dsl Normalized cell DSL
#' @param data Optional data for logical evaluation fallback
#' @return Logical
#' @keywords internal
dsl_is_subset <- function(query_dsl, cell_dsl, data = NULL) {
  
  # Exact match
  if (identical(query_dsl, cell_dsl)) return(TRUE)
  
  # Both are %in% on same variable - check value subset
  if (is.call(query_dsl) && is.call(cell_dsl)) {
    query_fn <- as.character(query_dsl[[1]])
    cell_fn <- as.character(cell_dsl[[1]])
    
    if (query_fn == "%in%" && cell_fn == "%in%") {
      query_var <- query_dsl[[2]]
      cell_var <- cell_dsl[[2]]
      
      if (identical(query_var, cell_var)) {
        query_vals <- query_dsl[[3]]
        cell_vals <- cell_dsl[[3]]
        
        # Evaluate c() calls
        if (is.call(query_vals) && as.character(query_vals[[1]]) == "c") {
          query_vals <- eval(query_vals)
        }
        if (is.call(cell_vals) && as.character(cell_vals[[1]]) == "c") {
          cell_vals <- eval(cell_vals)
        }
        
        # Query is subset if all query values in cell values
        return(all(query_vals %in% cell_vals))
      }
    }
    
    # Query is simple condition, cell is AND - check if query matches any component
    if (cell_fn == "&") {
      cell_components <- flatten_operator(cell_dsl, "&")
      return(any(sapply(cell_components, function(comp) {
        dsl_is_subset(query_dsl, comp, data)
      })))
    }
    
    # Different variables
    query_vars <- dsl_get_variables(query_dsl)
    cell_vars <- dsl_get_variables(cell_dsl)
    
    if (length(query_vars) > 0 && length(cell_vars) > 0) {
      if (!any(query_vars %in% cell_vars)) {
        return(FALSE)  # Different variables don't match
      }
    }
  }
  
  # Logical containment fallback (requires data)
  if (!is.null(data)) {
    tryCatch({
      query_array <- eval(query_dsl, data)
      cell_array <- eval(cell_dsl, data)
      
      # Query is subset of cell if: wherever query is TRUE, cell is also TRUE
      # query_array <= cell_array means query doesn't exceed cell
      return(all(query_array <= cell_array, na.rm = TRUE))
    }, error = function(e) {
      return(FALSE)
    })
  }
  
  FALSE
}

##### Metadata Extraction #####
#' Functions to extract hierarchical variable metadata from expanded specifications.
#' Enables semantic matching and operations on cells.

#' Extract variable metadata from expanded specification
#'
#' Extracts hierarchical variable metadata from an expanded specification
#' for semantic matching and cell operations. Works with DSL-based metadata.
#'
#' @param spec Expanded specification from expand stage
#' @param position One of "base", "row", or "col" (for context)
#' @return List of variable metadata lists
#' @keywords internal
extract_variable_metadata <- function(spec, position = "row") {
  if (is.null(spec)) {
    return(list())
  }
  
  # If spec has meta attribute (from expand stage), use it
  if (!is.null(spec$meta)) {
    meta <- spec$meta
    
    # Build hierarchical structure from DSL metadata
    vars_list <- list()
    
    # Extract from DSL-based metadata structure
    if (!is.null(meta$dsl) || !is.null(meta$variables) || !is.null(meta$label)) {
      # Primary: try to extract from DSL expression
      if (!is.null(meta$dsl)) {
        vars_list <- parse_expression_vars(meta$dsl)
      } else if (!is.null(meta$variables)) {
        # Fallback: use variables list
        for (var_name in meta$variables) {
          var_meta <- list(
            ivar = var_name,
            ival = if (!is.null(meta$tags$value)) meta$tags$value else NA,
            ivar_label = if (!is.null(meta$tags$var_label)) meta$tags$var_label else var_name,
            ival_label = if (!is.null(meta$tags$value_label)) meta$tags$value_label else NA
          )
          vars_list[[length(vars_list) + 1]] <- var_meta
        }
      }
      
      # If we extracted metadata, enhance with label if present
      if (length(vars_list) > 0 && !is.null(meta$label)) {
        vars_list[[1]]$label <- meta$label
      }
      
      return(vars_list)
    }
  }
  
  # Fallback: Try to parse expression
  if (!is.null(spec$expr) || !is.null(spec$components$expr)) {
    expr <- spec$expr %||% spec$components$expr
    return(parse_expression_vars(expr))
  }
  
  list()
}

#' Parse R expression to extract variable constraints
#'
#' Analyzes an R expression to identify variable-value relationships.
#'
#' @param expr R expression (quote(...))
#' @return List of variable metadata lists
#' @keywords internal
#' @examples
#' \dontrun{
#' parse_expression_vars(quote(x == 5))
#' # Returns: list(list(ivar = "x", ival = 5))
#'
#' parse_expression_vars(quote(x %in% c(4, 5)))
#' # Returns: list(list(ivar = "x", ival = c(4, 5)))
#'
#' parse_expression_vars(quote(x == 5 & y == 2))
#' # Returns: list(
#' #   list(ivar = "x", ival = 5),
#' #   list(ivar = "y", ival = 2)
#' # )
#' }
parse_expression_vars <- function(expr) {
  if (is.null(expr)) {
    return(list())
  }
  
  vars_list <- list()
  
  # Handle different expression types
  if (is.call(expr)) {
    fn <- as.character(expr[[1]])
    
    # Simple equality: x == 5
    if (fn == "==") {
      var_name <- as.character(expr[[2]])
      value <- eval(expr[[3]])  # Evaluate the value
      
      vars_list[[1]] <- list(
        ivar = var_name,
        ival = value,
        ivar_label = var_name,
        ival_label = as.character(value)
      )
    }
    
    # Set membership: x %in% c(4, 5)
    else if (fn == "%in%") {
      var_name <- as.character(expr[[2]])
      values <- eval(expr[[3]])  # Evaluate the vector
      
      vars_list[[1]] <- list(
        ivar = var_name,
        ival = values,
        ivar_label = var_name,
        ival_label = paste(values, collapse = ", ")
      )
    }
    
    # Logical AND: x == 5 & y == 2
    else if (fn == "&" || fn == "&&") {
      left_vars <- parse_expression_vars(expr[[2]])
      right_vars <- parse_expression_vars(expr[[3]])
      vars_list <- c(left_vars, right_vars)
    }
    
    # Complex comparisons: x > 30, x >= 5, etc.
    else if (fn %in% c(">", "<", ">=", "<=", "!=")) {
      var_name <- as.character(expr[[2]])
      
      vars_list[[1]] <- list(
        ivar = var_name,
        ival_expr = expr,  # Store full expression for complex conditions
        ivar_label = var_name,
        ival_label = deparse(expr)
      )
    }
    
    # Other calls - store as complex expression
    else {
      # Can't parse further - store entire expression
      vars_list[[1]] <- list(
        ivar = "complex",
        ival_expr = expr,
        ivar_label = "Complex Expression",
        ival_label = deparse(expr)
      )
    }
  }
  
  # Symbol (just variable name: x)
  else if (is.symbol(expr)) {
    var_name <- as.character(expr)
    vars_list[[1]] <- list(
      ivar = var_name,
      ival = NA,  # No specific value
      ivar_label = var_name,
      ival_label = "Any"
    )
  }
  
  # Constant (TRUE, FALSE, numeric)
  else {
    vars_list[[1]] <- list(
      ivar = "constant",
      ival = expr,
      ivar_label = "Constant",
      ival_label = as.character(expr)
    )
  }
  
  vars_list
}

#' Extract grouping key for layout matching
#'
#' Extracts the key value used for grouping cells in layout operations.
#'
#' @param meta Cell metadata (the meta component of specification)
#' @param field Which metadata field to extract ("ival", "ivar", etc.)
#' @param var_list Which variable list to search ("row_vars", "col_vars", "base_vars")
#' @return Character grouping key or NULL
#' @keywords internal
extract_grouping_key <- function(meta, field = "ival", var_list = "row_vars") {
  if (is.null(meta) || is.null(meta[[var_list]])) {
    return(NULL)
  }
  
  vars <- meta[[var_list]]
  
  # If no variables, return NULL
  if (length(vars) == 0) {
    return(NULL)
  }
  
  # Use first variable's value for grouping
  first_var <- vars[[1]]
  
  if (field == "ival") {
    ival <- first_var$ival
    
    if (is.null(ival) || all(is.na(ival))) {
      return(NULL)
    }
    
    # Convert to string key
    if (length(ival) == 1) {
      return(as.character(ival))
    } else {
      # Multiple values - create compound key
      return(paste(sort(ival), collapse = "_"))
    }
  }
  
  if (field == "ivar") {
    return(first_var$ivar)
  }
  
  NULL
}

#' Check if cell matches a facet specification
#'
#' Determines if a cell should be included in a facet based on its metadata.
#'
#' @param cell Cell object
#' @param facet_var Variable name for faceting (e.g., "satisfaction")
#' @param facet_value Value to match (e.g., 5)
#' @param var_list Which variable list to check ("row_vars", "col_vars", "base_vars")
#' @return Logical
#' @keywords internal
cell_matches_facet <- function(cell, facet_var, facet_value, var_list = "row_vars") {
  if (is.null(cell$specification$meta[[var_list]])) {
    return(FALSE)
  }
  
  vars <- cell$specification$meta[[var_list]]
  
  # Find matching variable
  matching_var <- Find(function(v) v$ivar == facet_var, vars)
  
  if (is.null(matching_var)) {
    return(FALSE)
  }
  
  # Check if value matches
  if (!is.null(matching_var$ival)) {
    # Simple value or vector
    return(facet_value %in% matching_var$ival)
  } else if (!is.null(matching_var$ival_expr)) {
    # Complex expression - need to evaluate
    # For now, return FALSE (can enhance later)
    return(FALSE)
  }
  
  FALSE
}

#' Compare two specifications for equality
#'
#' Determines if two specification expressions are identical.
#' Used for exact matching when building layout grid.
#'
#' @param spec1 First specification expression
#' @param spec2 Second specification expression
#' @return Logical
#' @keywords internal
specs_equal <- function(spec1, spec2) {
  identical(spec1, spec2)
}