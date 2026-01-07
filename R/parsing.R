# Parsing and Formula Conversion
# 
# This file handles the parse → expand → array pipeline:
# 1. resolve_external_variables: Inline external variable values, preserve helpers
# 2. parse_table_formula: Parse NSE expressions into structured specifications
# 3. expand_variables: Expand categorical variables and question groups
# 4. formula_to_arrays: Convert specifications to (m,u) arrays for computation
# 5. process_helper: Evaluate helper functions to get arrays (full processor)
# 6. process_helper_for_specs: Extract components from spec generators (banner only)
# 7. prepare_eval_data: Convert haven_labelled to numeric for expression evaluation
#
# Internal helpers:
# - .process_banner_arguments: Shared banner argument processing logic


#' Resolve External Variables to Values
#'
#' Walks expression tree and replaces external variables with their values.
#' Preserves data column names and registered helper calls.
#' Errors on unregistered function calls with guidance.
#'
#' @param expr R expression
#' @param data Data frame (to identify data columns)
#' @param env Environment to resolve external variables from
#' @param helpers Character vector of registered helper names (to preserve helper calls)
#' @return Expression with external variables replaced by values
#' @keywords internal
resolve_external_variables <- function(expr, data, env, helpers = NULL) {
  
  # Get helper names if not provided
  if (is.null(helpers)) {
    helpers <- names(.tab_registry$helpers)
  }
  
  # Base cases
  if (is.null(expr)) {
    return(expr)
  }
  
  # Literal values - return as-is
  if (is.numeric(expr) || is.character(expr) || is.logical(expr)) {
    return(expr)
  }
  
  # Symbol: check priority order
  if (is.symbol(expr)) {
    var_name <- as.character(expr)
    
    # Priority 1: Data columns stay as symbols
    if (var_name %in% names(data)) {
      return(expr)
    }
    
    # Priority 2: Check if it's an external variable
    if (!is.null(env) && exists(var_name, envir = env, inherits = TRUE)) {
      value <- get(var_name, envir = env, inherits = TRUE)
      
      # Functions are NOT allowed UNLESS they're from a package/namespace/base
      if (is.function(value)) {
        fn_env <- environment(value)
        # Error only if it's NOT from a package/namespace/base
        if (!is.null(fn_env) && !isNamespace(fn_env) && !identical(fn_env, baseenv())) {
          stop(
            "Function '", var_name, "' used in expression but not registered as helper.\n\n",
            "To use functions in tab():\n",
            "  1. Register as helper: create_helper('", var_name, "', processor = ...)\n",
            "  2. Or evaluate outside tab() and use the result\n\n",
            "Example registration:\n",
            "  create_helper('", var_name, "', function(spec, data, dpdict, ...) {\n",
            "    # Return numeric vector of length nrow(data)\n",
            "  })",
            call. = FALSE
          )
        }
        # Package/base functions: keep as symbol (don't inline)
        return(expr)
      }
      
      # Non-function values get inlined
      return(value)
    }
    
    # Priority 3: Unknown symbols stay as-is (will error later if needed)
    return(expr)
  }
  
  # Call: check if registered helper, then recursively resolve
  if (is.call(expr)) {
    fn <- as.character(expr[[1]])
    
    # Registered helpers stay symbolic (but resolve their arguments)
    if (fn %in% helpers) {
      # Recursively resolve arguments safely (preserving NULLs)
      expr_list <- as.list(expr)
      args <- expr_list[-1]
      resolved_args <- lapply(args, function(arg) {
        resolve_external_variables(arg, data, env, helpers)
      })
      return(as.call(c(list(expr_list[[1]]), resolved_args)))
    }
    
    # Check if it's an unregistered function call
    if (!is.null(env) && exists(fn, envir = env, inherits = TRUE)) {
      fn_value <- get(fn, envir = env, inherits = TRUE)
      
      if (is.function(fn_value)) {
        fn_env <- environment(fn_value)
        # Error only if it's NOT from a package/namespace/base
        if (!is.null(fn_env) && !isNamespace(fn_env) && !identical(fn_env, baseenv())) {
          stop(
            "Function '", fn, "' used in expression but not registered as helper.\n\n",
            "To use functions in tab():\n",
            "  1. Register as helper: create_helper('", fn, "', processor = ...)\n",
            "  2. Or evaluate outside tab() and use the result\n\n",
            "Example registration:\n",
            "  create_helper('", fn, "', function(spec, data, dpdict, ...) {\n",
            "    # Return numeric vector of length nrow(data)\n",
            "  })",
            call. = FALSE
          )
        }
      }
    }
    
    # Standard R operators and other calls - recursively resolve arguments
    expr_list <- as.list(expr)
    args <- expr_list[-1]
    resolved_args <- lapply(args, function(arg) {
      resolve_external_variables(arg, data, env, helpers)
    })
    
    return(as.call(c(list(expr_list[[1]]), resolved_args)))
  }
  
  # For pairlists and other language objects, return as-is
  expr
}


#' Parse table formula expressions using NSE
#'
#' @param expr An expression captured using rlang::enquo
#' @param data The data frame to evaluate against
#' @param dpdict Optional data dictionary for metadata
#' @return A list with type, components, and label
#' @keywords internal
parse_table_formula <- function(expr, data, dpdict = NULL, helpers = NULL, all_helpers = NULL) {
  
  # If helpers not provided, use registry
  if (is.null(helpers)) {
    helpers <- .tab_registry$helpers
  }

  # Extract expression and environment from quosure if needed
  if (rlang::is_quosure(expr)) {
    env <- rlang::quo_get_env(expr)
    actual_expr <- rlang::quo_get_expr(expr)
  } else {
    env <- NULL
    actual_expr <- expr
  }
  
  # Resolve external variables to values before parsing
  # This inlines variable values but preserves helper calls and data columns
  if (!is.null(env)) {
    actual_expr <- resolve_external_variables(
      actual_expr, 
      data, 
      env, 
      helpers = names(helpers)
    )
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

  expr_text <- deparse1(actual_expr)

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
      components = lapply(args, function(x) parse_table_formula(x, data, dpdict, all_helpers = all_helpers)),
      label = expr_text
    ))
  }

  # Handle subtraction
  if (rlang::is_call(actual_expr, "-")) {
    args <- rlang::call_args(actual_expr)
    return(list(
      type = "subtraction",
      components = lapply(args, function(x) parse_table_formula(x, data, dpdict, all_helpers = all_helpers)),
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
        expr = actual_expr,
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

  # Handle string literals (variable names)
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


#' Expand variables and question groups into individual components
#'
#' @param var_spec Variable specification (can be name, expression, or formula)
#' @param data The data frame
#' @param dpdict Optional data dictionary
#' @param statistic Statistic object (tab_stat) or NULL
#' @return List of expanded variable specifications
#' @keywords internal
expand_variables <- function(var_spec, data, dpdict = NULL, statistic = NULL, values_var = NULL, label_mode = "smart", all_helpers = NULL) {
  
  # Handle complex expressions by recursively expanding components
  if (is.list(var_spec) && !is.null(var_spec$type)) {

    # Helper functions should not be expanded
    if (var_spec$type == "helper") {
      return(list(normalize_spec_expression(var_spec)))
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
      
      res <- lapply(result, normalize_spec_expression)
      
      # If we have a user-provided label and the expansion resulted in a single spec,
      # use the user label instead of the generated one
      if (!is.null(var_spec$is_user_label) && isTRUE(var_spec$is_user_label) && length(res) == 1) {
        res[[1]]$label <- var_spec$label
      }
      
      return(res)
    }

    if (var_spec$type == "simple") {
      var_name <- var_spec$components$var
    } else {
      # For other complex types, return as-is
      return(list(normalize_spec_expression(var_spec)))
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
            normalize_spec_expression(child_spec)
          }))
        } else if (is.factor(var_data)) {
          levs <- levels(var_data)
          return(lapply(seq_along(levs), function(i) {
            lev <- levs[i]
            formatted_label <- get_display_label(var_name, dpdict, label_mode, lev, data)

            normalize_spec_expression(list(
              type = "expression",
              components = list(expr = call("==", as.name(var_name), i)),  # factors use numeric indices
              label = formatted_label,
              meta = list(
                ivar = get_var_label(var_name, dpdict),
                ival = lev,
                label = formatted_label
              )
            ))
          }))
        } else {
          stop("Variable '", var_name, "' has questiontype '", questiontype, "' but no labels or factor levels found")
        }
      } else if (questiontype %in% c("multiresponse", "numeric", "multinumeric")) {
        # Don't expand - return as single variable
        if (is.character(var_spec)) {
          var_label <- get_display_label(var_spec, dpdict, label_mode, NULL, data)
          return(list(normalize_spec_expression(list(
            type = "simple",
            components = list(var = var_spec),
            label = var_label,
            meta = list(
              ivar = get_var_label(var_spec, dpdict),
              ival = NULL,  # No categorical value for these types
              label = var_label
            )
          ))))
        } else {
          return(list(normalize_spec_expression(var_spec)))
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

        normalize_spec_expression(list(
          type = "expression",
          components = list(expr = call("==", as.name(var_name), val)),
          label = formatted_label,
          meta = list(
            ivar = get_var_label(var_name, dpdict),
            ival = val_label,
            label = formatted_label
          )
        ))
      }))
    }

    # Check if it's a factor
    if (is.factor(var_data)) {
      levs <- levels(var_data)
      return(lapply(levs, function(lev) {
        formatted_label <- get_display_label(var_name, dpdict, label_mode, lev, data)

        normalize_spec_expression(list(
          type = "expression",
          components = list(expr = call("==", as.name(var_name), lev)),
          label = formatted_label,
          meta = list(
            ivar = get_var_label(var_name, dpdict),
            ival = lev,
            label = formatted_label
          )
        ))
      }))
    }

    # Check if it's logical (treat as binary, don't expand)
    if (is.logical(var_data)) {
      if (is.character(var_spec)) {
        return(list(normalize_spec_expression(list(
          type = "simple",
          components = list(var = var_spec),
          label = get_display_label(var_spec, dpdict, label_mode, NULL, data)))))
      } else {
        return(list(normalize_spec_expression(var_spec)))
      }
    }

    # Check if it's numeric (don't expand)
    if (is.numeric(var_data)) {
      if (is.character(var_spec)) {
        return(list(normalize_spec_expression(list(
          type = "simple",
          components = list(var = var_spec),
          label = get_display_label(var_spec, dpdict, label_mode, NULL, data)
        ))))
      } else {
        return(list(normalize_spec_expression(var_spec)))
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
        normalize_spec_expression(list(type = "simple", components = list(var = v), label = get_var_label(v, dpdict)))
      }))
    }
  }

  # Default: return as single variable
  if (is.character(var_spec)) {
    return(list(normalize_spec_expression(list(
      type = "simple",
      components = list(var = var_spec),
      label = get_display_label(var_spec, dpdict, label_mode, NULL, data)
    ))))
  } else {
    return(list(normalize_spec_expression(var_spec)))
  }
}

#' Normalize spec to ensure canonical expression is in spec$expr
#'
#' Ensures all specs have their canonical expression stored in spec$expr
#' for consistent semantic matching. Handles all spec types.
#'
#' @param spec Specification from parse/expand stage
#' @return Spec with normalized expr field
#' @keywords internal
normalize_spec_expression <- function(spec) {
  # Skip if expr already populated
  if (!is.null(spec$expr)) {
    return(spec)
  }
  
  # Handle different spec types
  if (is.null(spec$type)) {
    # Minimal spec - use label as symbol
    spec$expr <- as.symbol(spec$label)
    return(spec)
  }
  
  if (spec$type == "expression") {
    # Move from components$expr to expr
    if (!is.null(spec$components$expr)) {
      spec$expr <- spec$components$expr
    }
  } else if (spec$type == "simple") {
    # Convert variable name to symbol
    if (!is.null(spec$components$var)) {
      spec$expr <- as.symbol(spec$components$var)
    }
  } else if (spec$type == "multiplication") {
    # For multiplication, construct expression from components
    # This requires components to already be normalized
    if (length(spec$components) == 2) {
      comp1_expr <- spec$components[[1]]$expr
      comp2_expr <- spec$components[[2]]$expr
      if (!is.null(comp1_expr) && !is.null(comp2_expr)) {
        spec$expr <- call("*", comp1_expr, comp2_expr)
      }
    }
  } else if (spec$type == "total") {
    # Total accepts all rows
    spec$expr <- quote(TRUE)
  } else if (spec$type == "helper") {
    # For helpers, use helper type as symbol (or keep NULL)
    # Helpers are special and may not need expression matching
    spec$expr <- as.symbol(spec$helper_type %||% spec$label)
  } else if (spec$type == "banner_intersection") {
    # Banner intersection: outer_var==outer_val & inner_var==inner_val
    # Create composite expression for matching
    spec$expr <- bquote(.(as.symbol(spec$outer_var)) == .(spec$outer_val) & 
                        .(as.symbol(spec$inner_var)) == .(spec$inner_val))
  } else if (spec$type == "banner_subtotal") {
    # Banner subtotal: all inner values for this outer value
    spec$expr <- bquote(.(as.symbol(spec$outer_var)) == .(spec$outer_val))
  } else if (spec$type == "banner_filtered_helper") {
    # Banner with nested helper: outer_var==outer_val & inner_helper
    # Use the label as a unique identifier since inner_spec is complex
    spec$expr <- as.symbol(make.names(spec$label))
  }
  
  return(spec)
}

# ---- Membership/universe arrays (m,u) -----------------------------------------
#
# tab_arrays is the internal representation for a specification:
# - m: membership / contribution (typically 0/1 for categorical specs)
# - u: universe / eligibility (0/1), defining who is in-base for denominators
#
# If u is not explicitly provided, infer u from missingness: u = !is.na(m_raw).
#
new_tab_arrays <- function(m_raw, u = NULL) {
  if (is.logical(m_raw)) {
    m_raw <- as.numeric(m_raw)
  }
  if (!is.numeric(m_raw)) {
    stop("new_tab_arrays: m_raw must be numeric or logical")
  }

  if (is.null(u)) {
    u <- !is.na(m_raw)
  }
  if (is.logical(u)) {
    u <- as.numeric(u)
  }
  if (!is.numeric(u)) {
    stop("new_tab_arrays: u must be numeric or logical")
  }
  if (length(u) != length(m_raw)) {
    stop("new_tab_arrays: u must have the same length as m_raw")
  }

  # Universe is 0/1 with NA treated as 0
  u[is.na(u)] <- 0
  u <- as.numeric(u > 0)

  # Membership has no NA and contributes 0 outside universe
  m <- m_raw
  m[is.na(m)] <- 0
  m <- m * u

  structure(list(m = m, u = u), class = "tab_arrays")
}

is_tab_arrays <- function(x) inherits(x, "tab_arrays")

as_tab_arrays <- function(x, n_expected = NULL) {
  if (is_tab_arrays(x)) {
    if (!is.null(n_expected) && length(x$m) != n_expected) {
      stop("tab_arrays length mismatch: expected ", n_expected, ", got ", length(x$m))
    }
    return(x)
  }
  if (!is.numeric(x)) {
    stop("Expected numeric vector or tab_arrays")
  }
  if (!is.null(n_expected) && length(x) != n_expected) {
    stop("Vector length mismatch: expected ", n_expected, ", got ", length(x))
  }
  new_tab_arrays(x)
}

#' Convert formula specification to (m,u) arrays
#'
#' @param formula_spec Parsed formula specification
#' @param data Data frame
#' @param dpdict Optional data dictionary for context
#' @return A tab_arrays object (membership m and universe u), or a multi-column
#'   list of tab_arrays when the spec expands into multiple arrays.
#' @keywords internal
formula_to_arrays <- function(formula_spec, data, dpdict = NULL, all_helpers = NULL) {
  
  n <- nrow(data)

  # Start with identity
  result_m <- rep(1, n)
  result_u <- rep(1, n)

  # Process based on type
  if (formula_spec$type == "simple") {
    var_name <- formula_spec$components$var

    check_variables_exist(var_name, data,
                          paste0("expression '", formula_spec$label, "'"))

    m_raw <- create_simple_array(data[[var_name]], var_name, formula_spec, dpdict)
    return(new_tab_arrays(m_raw))

  } else if (formula_spec$type == "multiplication") {
    # Apply each component multiplicatively
    for (comp in formula_spec$components) {
      comp_arrays <- as_tab_arrays(formula_to_arrays(comp, data, dpdict, all_helpers), n_expected = n)
      result_m <- result_m * comp_arrays$m
      result_u <- result_u * comp_arrays$u
    }
    arrays <- new_tab_arrays(result_m, result_u)

    # For multiplication, preserve meta from first component if available
    if (length(formula_spec$components) > 0) {
      first_arrays <- as_tab_arrays(formula_to_arrays(formula_spec$components[[1]], data, dpdict, all_helpers = all_helpers), n_expected = n)
      first_meta <- attr(first_arrays$m, "meta")
      if (!is.null(first_meta)) {
        arrays$m <- set_array_meta(
          arrays$m,
          dsl = first_meta$dsl,
          variables = first_meta$variables,
          tags = first_meta$tags,
          label = formula_spec$label %||% first_meta$label
        )
      }
    }
    return(arrays)

  } else if (formula_spec$type == "expression") {
    # Extract variables referenced in the expression
    expr_vars <- all.vars(formula_spec$components$expr)

    # Check all variables exist before subsetting
    check_variables_exist(expr_vars, data,
                          paste0("expression '", formula_spec$label, "'"))

    # Create subset with only needed variables
    data_subset <- data[, expr_vars, drop = FALSE]
    
    # Transform label comparisons (e.g., gender == "Female" -> gender == 1)
    transformed_expr <- transform_label_comparisons(formula_spec$components$expr, data_subset)
    
    # Evaluate the expression with numeric-only data for relevant variables
    eval_data <- prepare_eval_data(data_subset)
    expr_result <- rlang::eval_tidy(transformed_expr, eval_data)
    if (!is.logical(expr_result) && !is.numeric(expr_result)) {
      stop("Expression must evaluate to logical or numeric")
    }
    m_raw <- create_expression_array(expr_result, formula_spec, expr_vars, dpdict)
    return(new_tab_arrays(m_raw))

  } else if (formula_spec$type == "helper") {
      helper_result <- process_helper(formula_spec, data, dpdict, all_helpers)

      # Check if it's a multi-column helper
      # Multi-column helpers should already have meta
      if (is.list(helper_result) && isTRUE(attr(helper_result, "is_multi_column"))) {
        # Wrap each returned array into tab_arrays
        out <- helper_result
        for (nm in names(out)) {
          out[[nm]] <- as_tab_arrays(out[[nm]], n_expected = n)
        }
        attr(out, "is_multi_column") <- TRUE
        attr(out, "helper_type") <- attr(helper_result, "helper_type") %||% formula_spec$helper_type
        return(out)
      } else {
        # Single array helper - ensure it has meta
        if (is.null(attr(helper_result, "meta"))) {
          helper_result <- set_array_meta(
            helper_result,
            dsl = NULL,
            variables = NULL,
            tags = list(helper_type = formula_spec$helper_type),
            label = formula_spec$label %||% formula_spec$helper_type
          )
        }
        return(as_tab_arrays(helper_result, n_expected = n))
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
      
      # Transform label comparisons
      transformed_expr <- transform_label_comparisons(formula_spec$components$expr, data_subset)
      
      eval_data <- prepare_eval_data(data_subset)
      expr_result <- rlang::eval_tidy(transformed_expr, eval_data)
      if (!is.numeric(expr_result)) {
        stop("Expression must evaluate to numeric values")
      }
      if (length(expr_result) != n) {
        stop("Expression must return vector of length ", n)
      }
      m_raw <- create_expression_array(expr_result, formula_spec, expr_vars, dpdict)
      return(new_tab_arrays(m_raw))
    }, error = function(e) {
      stop("Error evaluating expression '", formula_spec$label, "': ", e$message, call. = FALSE)
    })

  } else if (formula_spec$type == "subtraction") {
    # Handle subtraction
    comp1_arrays <- as_tab_arrays(formula_to_arrays(formula_spec$components[[1]], data, dpdict, all_helpers), n_expected = n)
    comp2_arrays <- as_tab_arrays(formula_to_arrays(formula_spec$components[[2]], data, dpdict, all_helpers), n_expected = n)
    result_m <- result_m * (comp1_arrays$m - comp2_arrays$m)
    result_u <- result_u * comp1_arrays$u
    arrays <- new_tab_arrays(result_m, result_u)

    # For subtraction, preserve meta from first component if available
    first_meta <- attr(comp1_arrays$m, "meta")
    if (!is.null(first_meta)) {
      arrays$m <- set_array_meta(
        arrays$m,
        dsl = first_meta$dsl,
        variables = first_meta$variables,
        tags = first_meta$tags,
        label = formula_spec$label %||% first_meta$label
      )
    }
    return(arrays)
  } else if (formula_spec$type == "total") {
    # This represents "all data" - when no specific column is specified
    # Not a summary, just an array of 1s representing the full dataset
    m_raw <- rep(1, n)
    m_raw <- set_array_meta(
      m_raw,
      dsl = quote(TRUE),
      variables = NULL,
      tags = list(type = "total"),
      label = formula_spec$label %||% "Total"
    )
    return(new_tab_arrays(m_raw, rep(1, n)))

  } else if (formula_spec$type == "banner_intersection") {
    # Simple intersection: outer_var==outer_val AND inner_var==inner_val
    outer_match <- data[[formula_spec$outer_var]] == formula_spec$outer_val & 
                   !is.na(data[[formula_spec$outer_var]])
    inner_match <- data[[formula_spec$inner_var]] == formula_spec$inner_val & 
                   !is.na(data[[formula_spec$inner_var]])
    
    # Create intersection array
    result <- as.numeric(outer_match & inner_match)
    
    # Add metadata for base calculation
    dsl_expr <- bquote(.(as.symbol(formula_spec$inner_var)) == .(formula_spec$inner_val))
    result <- set_array_meta(
      result,
      dsl = dsl_expr,
      variables = list(formula_spec$inner_var),
      tags = list(value = formula_spec$inner_val, value_label = as.character(formula_spec$inner_val)),
      label = formula_spec$label
    )
    
    # Banner columns are defined within an outer base context.
    # Membership is the outer+inner intersection; universe is the outer context.
    new_tab_arrays(result, as.numeric(outer_match))

  } else if (formula_spec$type == "banner_subtotal") {
    # Subtotal: all inner values for this outer value
    outer_match <- data[[formula_spec$outer_var]] == formula_spec$outer_val & 
                   !is.na(data[[formula_spec$outer_var]])
    
    result <- as.numeric(outer_match)
    dsl_expr <- bquote(.(as.symbol(formula_spec$outer_var)) == .(formula_spec$outer_val))
    result <- set_array_meta(
      result,
      dsl = dsl_expr,
      variables = list(formula_spec$outer_var),
      tags = list(value = formula_spec$outer_val, value_label = as.character(formula_spec$outer_val)),
      label = formula_spec$label
    )
    
    new_tab_arrays(result, as.numeric(outer_match))

  } else if (formula_spec$type == "banner_filtered_helper") {
    # Nested helper: evaluate helper in context of outer filter
    outer_match <- data[[formula_spec$outer_var]] == formula_spec$outer_val & 
                   !is.na(data[[formula_spec$outer_var]])
    
    # Parse and evaluate the inner helper
    # inner_spec is already an expression, don't enquo it
    inner_parsed <- parse_table_formula(formula_spec$inner_spec, data, dpdict, all_helpers)
    inner_array <- formula_to_arrays(inner_parsed, data, dpdict, all_helpers)
    
    # Create intersection: both outer filter AND inner condition must be true
    inner_arrays <- as_tab_arrays(inner_array, n_expected = n)
    arrays <- new_tab_arrays(
      inner_arrays$m * as.numeric(outer_match),
      inner_arrays$u * as.numeric(outer_match)
    )
    
    # Preserve metadata from inner array
    inner_meta <- attr(inner_arrays$m, "meta")
    arrays$m <- set_array_meta(
      arrays$m,
      dsl = inner_meta$dsl,
      variables = inner_meta$variables %||% list(formula_spec$outer_var),
      tags = inner_meta$tags,
      label = formula_spec$label
    )
    
    return(arrays)

  } else {
    stop("Unknown formula type: ", formula_spec$type)
  }
}

#' Process helper functions
#' @keywords internal
process_helper <- function(formula_spec, data, dpdict, all_helpers = NULL) {
  
  helper_type <- formula_spec$helper_type

  # Get the helper from registry
  helper_obj <- .tab_registry$helpers[[helper_type]]
  if (is.null(helper_obj)) {
    stop("Unknown helper: '", helper_type, "'. Available: ",
         paste(names(.tab_registry$helpers), collapse = ", "))
  }

  # Special handling for banner - keep inner spec unevaluated
  if (helper_type == "banner") {
    evaluated_args <- .process_banner_arguments(formula_spec, data)
  } else {
    # Recursively evaluate arguments
    evaluated_args <- list()
    arg_names <- names(formula_spec$args)
    for (i in seq_along(formula_spec$args)) {
      arg <- formula_spec$args[[i]]

      if (rlang::is_call(arg) && identical(arg[[1]], quote(c))) {
        arg_list <- as.list(arg)[-1]
        converted <- vapply(arg_list, function(x) {
          if (rlang::is_symbol(x)) as.character(x)
          else x
        }, FUN.VALUE = character(1))
        evaluated_args[[i]] <- converted
        next
      }

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
          evaluated_args[[i]] <- process_helper(nested_spec, data, dpdict, all_helpers)  # Recursive call
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
            
            # Transform label comparisons
            transformed_arg <- transform_label_comparisons(arg, data_subset)
            
            eval_data <- prepare_eval_data(data_subset)
            evaluated_args[[i]] <- rlang::eval_tidy(transformed_arg, eval_data)
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
          
          # Transform label comparisons
          transformed_arg <- transform_label_comparisons(arg, data_subset)
          
          eval_data <- prepare_eval_data(data_subset)
          evaluated_args[[i]] <- rlang::eval_tidy(transformed_arg, eval_data)
        }, error = function(e) {
          stop("Error evaluating argument ", i, " in helper '", helper_type, "': ", e$message, call. = FALSE)
        })
      }
    }
    # Restore argument names after evaluation
    if (!is.null(arg_names)) {
      names(evaluated_args) <- arg_names
    }
  }

  # Create formula_spec with evaluated components for the processor
  processed_spec <- list(
    type = "helper",
    helper_type = helper_type,
    components = evaluated_args,
    label = formula_spec$label %||% helper_type
  )

  if (!is.null(formula_spec$row_labels)) {
    processed_spec$row_labels <- formula_spec$row_labels
  }

  # Dispatch to the helper's processor
  tryCatch({
    result <- helper_obj$processor(processed_spec, data, dpdict, all_helpers)

    # Single-array helper can return tab_arrays directly (m,u).
    if (inherits(result, "tab_arrays")) {
      if (!is.numeric(result$m) || length(result$m) != nrow(data)) {
        stop("Helper '", helper_type, "' returned invalid tab_arrays$m. Expected numeric vector of length ", nrow(data))
      }
      if (!is.numeric(result$u) || length(result$u) != nrow(data)) {
        stop("Helper '", helper_type, "' returned invalid tab_arrays$u. Expected numeric vector of length ", nrow(data))
      }
      return(result)
    }

    if (is.list(result) && !is.numeric(result)) {
      # Helper returned a list of arrays (multi-column helper)
      # Validate each array (numeric vector or tab_arrays) in the list
      for (name in names(result)) {
        item <- result[[name]]
        if (inherits(item, "tab_arrays")) {
          if (!is.numeric(item$m) || length(item$m) != nrow(data)) {
            stop("Helper '", helper_type, "' returned invalid tab_arrays$m for '", name,
                 "'. Expected numeric vector of length ", nrow(data))
          }
          if (!is.numeric(item$u) || length(item$u) != nrow(data)) {
            stop("Helper '", helper_type, "' returned invalid tab_arrays$u for '", name,
                 "'. Expected numeric vector of length ", nrow(data))
          }
        } else if (!is.numeric(item) || length(item) != nrow(data)) {
          stop("Helper '", helper_type, "' returned invalid result for '", name,
               "'. Expected numeric vector of length ", nrow(data), " or tab_arrays.")
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

#' Process helper to extract components for spec generators
#' @keywords internal
process_helper_for_specs <- function(formula_spec, data, dpdict = NULL, all_helpers = NULL) {
  helper_type <- formula_spec$helper_type
  
  # Special handling for banner - keep inner spec unevaluated
  if (helper_type == "banner") {
    evaluated_args <- .process_banner_arguments(formula_spec, data)
    
    # Return processed spec with components
    return(list(
      type = "helper",
      helper_type = helper_type,
      components = evaluated_args,
      label = formula_spec$label %||% helper_type
    ))
  }
  
  stop("process_helper_for_specs only supports banner helper")
}

#' Process banner helper arguments (internal)
#'
#' Shared logic for processing banner() special arguments used by both
#' process_helper and process_helper_for_specs.
#'
#' @param formula_spec Formula specification with args
#' @param data Data frame for evaluating additional arguments
#' @return List of evaluated arguments
#' @keywords internal
.process_banner_arguments <- function(formula_spec, data) {
  evaluated_args <- list()
  arg_names <- names(formula_spec$args)
  
  for (i in seq_along(formula_spec$args)) {
    arg <- formula_spec$args[[i]]
    arg_name <- if (!is.null(arg_names) && i <= length(arg_names)) arg_names[i] else ""
    
    if (i == 1) {
      # First argument: outer variable (convert to string)
      if (rlang::is_symbol(arg)) {
        evaluated_args[[i]] <- as.character(arg)
      } else if (is.character(arg) && length(arg) == 1) {
        evaluated_args[[i]] <- arg
      } else {
        stop("First argument to banner must be a variable name")
      }
    } else if (i == 2) {
      # Second argument: inner specification (keep unevaluated)
      evaluated_args[[i]] <- arg
    } else {
      # Additional arguments (usually named like subtotals, sep)
      if (arg_name != "") {
        tryCatch({
          evaluated_args[[arg_name]] <- rlang::eval_tidy(arg, data)
        }, error = function(e) {
          evaluated_args[[arg_name]] <- arg
        })
      } else {
        evaluated_args[[i]] <- rlang::eval_tidy(arg, data)
      }
    }
  }
  
  if (length(evaluated_args) < 2) {
    stop("Banner requires at least two arguments")
  }
  
  return(evaluated_args)
}

#' Transform label comparisons in expressions
#'
#' Walks an expression tree and replaces string literals with their corresponding
#' numeric values from haven_labelled labels. Supports ==, !=, and %in% operators.
#'
#' @param expr Expression to transform
#' @param data Data frame containing variables
#' @return Transformed expression with strings replaced by numeric values
#' @keywords internal
transform_label_comparisons <- function(expr, data) {
  # Base cases: symbols, literals, NULL
  if (is.null(expr) || !is.language(expr)) {
    return(expr)
  }
  
  if (is.symbol(expr)) {
    return(expr)
  }
  
  # Recursive case: function calls
  if (is.call(expr)) {
    fn <- as.character(expr[[1]])
    
    # Handle comparison operators: ==, !=
    if (fn %in% c("==", "!=") && length(expr) == 3) {
      lhs <- expr[[2]]
      rhs <- expr[[3]]
      
      # Check if LHS is a simple variable name
      if (is.symbol(lhs)) {
        var_name <- as.character(lhs)
        
        # Check if variable exists and is haven_labelled
        if (var_name %in% names(data)) {
          var_data <- data[[var_name]]
          
          if (inherits(var_data, "haven_labelled")) {
            labels <- attr(var_data, "labels")
            
            # If RHS is a string literal, try to match it to a label
            if (is.character(rhs) && length(rhs) == 1 && !is.null(labels) && length(labels) > 0) {
              # Look for exact match in label names (case-sensitive)
              matching_idx <- which(names(labels) == rhs)
              
              if (length(matching_idx) == 1) {
                # Replace string with corresponding numeric value
                expr[[3]] <- labels[matching_idx]
                return(expr)
              } else if (length(matching_idx) == 0) {
                # No match found - provide helpful error
                available_labels <- paste(names(labels), collapse = ", ")
                stop("Label '", rhs, "' not found in variable '", var_name, 
                     "'. Available labels: ", available_labels, call. = FALSE)
              } else {
                # Multiple matches (shouldn't happen with exact match, but be safe)
                stop("Label '", rhs, "' matches multiple values in variable '", var_name, "'", 
                     call. = FALSE)
              }
            }
          }
        }
      }
      
      # Recursively transform both sides (in case of nested expressions)
      expr[[2]] <- transform_label_comparisons(expr[[2]], data)
      expr[[3]] <- transform_label_comparisons(expr[[3]], data)
      return(expr)
    }
    
    # Handle %in% operator
    if (fn == "%in%" && length(expr) == 3) {
      lhs <- expr[[2]]
      rhs <- expr[[3]]
      
      # Check if LHS is a simple variable name
      if (is.symbol(lhs)) {
        var_name <- as.character(lhs)
        
        # Check if variable exists and is haven_labelled
        if (var_name %in% names(data)) {
          var_data <- data[[var_name]]
          
          if (inherits(var_data, "haven_labelled")) {
            labels <- attr(var_data, "labels")
            
            if (!is.null(labels) && length(labels) > 0) {
              # Check if RHS is c(...) with character values
              if (is.call(rhs) && as.character(rhs[[1]]) == "c") {
                # Extract arguments from c()
                c_args <- as.list(rhs)[-1]
                
                # Transform each character argument
                transformed_args <- lapply(c_args, function(arg) {
                  if (is.character(arg) && length(arg) == 1) {
                    # Look for exact match in label names
                    matching_idx <- which(names(labels) == arg)
                    
                    if (length(matching_idx) == 1) {
                      return(labels[matching_idx])
                    } else if (length(matching_idx) == 0) {
                      available_labels <- paste(names(labels), collapse = ", ")
                      stop("Label '", arg, "' not found in variable '", var_name, 
                           "'. Available labels: ", available_labels, call. = FALSE)
                    } else {
                      stop("Label '", arg, "' matches multiple values in variable '", var_name, "'", 
                           call. = FALSE)
                    }
                  } else {
                    return(arg)
                  }
                })
                
                # Reconstruct c() call with transformed arguments
                expr[[3]] <- as.call(c(quote(c), transformed_args))
                return(expr)
              }
            }
          }
        }
      }
      
      # Recursively transform both sides
      expr[[2]] <- transform_label_comparisons(expr[[2]], data)
      expr[[3]] <- transform_label_comparisons(expr[[3]], data)
      return(expr)
    }
    
    # For all other calls, recursively transform arguments
    for (i in seq_along(expr)[-1]) {
      expr[[i]] <- transform_label_comparisons(expr[[i]], data)
    }
    return(expr)
  }
  
  # Default: return unchanged
  expr
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
    # Use meta from expand_variables - convert to DSL structure
    dsl_expr <- if (!is.null(spec$meta$dsl)) spec$meta$dsl else as.symbol(var_name)
    tags <- list(
      value = spec$meta$ival,
      value_label = spec$meta$ival_label
    )
    set_array_meta(arr, dsl = dsl_expr, variables = list(var_name), 
                   tags = tags, label = spec$meta$label)
  } else {
    # Add default meta for simple variables
    set_array_meta(
      arr,
      dsl = as.symbol(var_name),
      variables = list(var_name),
      tags = NULL,
      label = spec$label %||% get_var_label(var_name, dpdict)
    )
  }
}

create_expression_array <- function(expr_result, spec, expr_vars, dpdict) {
  arr <- as.numeric(expr_result)

  # Check if spec has meta from expand_variables
  if (!is.null(spec$meta)) {
    dsl_expr <- if (!is.null(spec$meta$dsl)) spec$meta$dsl else spec$components$expr
    tags <- list(
      value = spec$meta$ival,
      value_label = spec$meta$ival_label
    )
    set_array_meta(arr, dsl = dsl_expr, 
                   variables = if (length(expr_vars) > 0) as.list(expr_vars) else list("expression"),
                   tags = tags, label = spec$meta$label)
  } else {
    # Add meta for general expressions
    set_array_meta(
      arr,
      dsl = spec$components$expr,
      variables = if (length(expr_vars) > 0) as.list(expr_vars) else list("expression"),
      tags = NULL,
      label = spec$label %||% deparse(spec$components$expr)
    )
  }
}

create_total_array <- function(n, spec) {
  arr <- rep(1, n)
  set_array_meta(
    arr,
    dsl = quote(TRUE),
    variables = NULL,
    tags = list(type = "total"),
    label = spec$label %||% "Total"
  )
}

# Macro System
#
# Expand tab_macro objects into concrete tab() arguments:
# - Macros detected before tab() execution
# - Can override multiple parameters (rows, cols, filter, etc.)
# - Supports nested macros with iteration limit (10 levels max)
# - Warns when overriding user-supplied arguments

convert_macro_result <- function(value, env) {
  if (is.null(value)) {
    return(NULL)
  }

  if (inherits(value, "tab_macro")) {
    stop("Macro expansion returned another macro. Macros must resolve to concrete tab() arguments.")
  }

  if (inherits(value, "quosure")) {
    return(value)
  }

  rlang::as_quosure(value, env = env)
}

detect_tab_macro <- function(quo, env) {
  if (is.null(quo)) {
    return(NULL)
  }

  tryCatch({
    value <- rlang::eval_tidy(quo, env = env)
    if (inherits(value, "tab_macro")) value else NULL
  }, error = function(e) NULL)
}

expand_macro_at_position <- function(params, position, user_supplied, data, dpdict, user_arg_labels) {
  iteration <- 0

  while (inherits(params[[position]], "tab_macro")) {
    iteration <- iteration + 1
    if (iteration > 10) {
      stop("Macro expansion for parameter '", position, "' exceeded iteration limit (possible recursive macro loop)")
    }

    macro_call <- params[[position]]
    macro <- get_macro(macro_call$macro_id)
    if (is.null(macro)) {
      stop("Unknown macro: '", macro_call$macro_id, "'")
    }

    overrides <- macro$expander(
      macro_args = macro_call$args,
      current_params = params,
      data = data,
      dpdict = dpdict,
      position = position
    )

    if (is.null(overrides)) {
      overrides <- list()
    }
    if (!is.list(overrides)) {
      stop("Macro '", macro_call$macro_id, "' must return a named list of overrides")
    }
    if (length(overrides) > 0 && (is.null(names(overrides)) || any(!nzchar(names(overrides))))) {
      stop("Macro '", macro_call$macro_id, "' must return a named list of overrides")
    }

    for (name in names(overrides)) {
      override_value <- overrides[[name]]
      if (identical(name, position)) {
        params[[name]] <- override_value
      } else {
        user_set <- name %in% user_supplied
        if (user_set) {
          original_label <- user_arg_labels[[name]]
          original_desc <- if (is.null(original_label)) {
            "original value supplied"
          } else {
            paste0("original: ", original_label)
          }
          warning(sprintf(
            "Macro '%s()' overriding user-supplied argument `%s` (%s); macro output will be used.",
            macro_call$macro_id,
            name,
            original_desc
          ), call. = FALSE)
        }
        params[[name]] <- override_value
      }
    }

    if (!position %in% names(overrides)) {
      params[[position]] <- NULL
    }
  }

  params
}

process_tab_macros <- function(params, user_supplied, data, dpdict, user_arg_labels) {
  macro_positions <- intersect(c("rows", "cols", "filter"), names(params))

  for (position in macro_positions) {
    params <- expand_macro_at_position(params, position, user_supplied, data, dpdict, user_arg_labels)
  }

  params
}