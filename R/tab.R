# Main Tab Interface
#
# The tab() function and its core pipeline:
# 1. Macro detection and expansion
# 2. Variable resolution (fuzzy matching via resolve_vars)
# 3. Parsing and expansion (via tab_parsing.R functions)
# 4. Validation (via tab_utilities.R functions)
# 5. Cell-native computation via compute_cells_as_bundle()
# 6. Layout grid construction with semantic cell matching
# 7. Low base filtering if specified
#
# Returns a tab_cell_collection with cells in store and explicit layout grid.

#' Cell-Based Tab Function
#'
#' Create cross-tabulation tables using the cell-based architecture. This function
#' implements the complete unified tab architecture with position-independent cells,
#' semantic references, and full metadata preservation.
#'
#' @section Architecture:
#' tab() implements the complete parse → expand → compute pipeline and returns
#' a cell-based tab_result object. The function returns a bundle of cells with 
#' an explicit layout grid that imposes 2D structure. Use as.data.frame() to 
#' materialize the result into a standard data.frame when needed.
#'
#' **Cell-Native Computation**:
#' tab() computes cells directly from array intersections rather than computing
#' a full matrix first. This avoids intermediate 2D structures and maintains 
#' position-independence throughout the pipeline. Cells are created with their 
#' values, bases, and full metadata in a single pass, enabling true
#' dimension-agnostic operations.
#'
#' **Key Features**:
#' - Returns `tab_cell_collection` (use as.data.frame() to materialize)
#' - Cells stored with stable sequential IDs (c_000001, c_000002, ...)
#' - Each cell preserves full specification (base_expr, row_expr, col_expr)
#' - Hierarchical metadata enables semantic operations
#' - Derive operations are truly additive (add cells, never modify)
#' - Layout operations manipulate grid structure, not data
#' - Computation is cell-native (no intermediate matrices)
#'
#' **Core Functionality**:
#' - Macro support: Detects and expands tab_macro objects
#' - Variable resolution: Fuzzy matching for column names (when fuzzy_match=TRUE)
#' - Validation: Checks statistic requirements and label collisions
#' - Label control: Smart, full, or suffix label modes at creation and display time
#' - Low base filtering: Automatic threshold-based filtering
#' - All helper functions and custom statistics fully supported
#'
#' **Cell Structure**:
#' Each cell contains:
#' - value: Computed statistic value
#' - base: Base count for this cell
#' - specification: What this cell represents (expressions + metadata)
#' - computation: How it was computed (statistic, array references)
#' - derivation: NULL for compute cells, tracks source cells for derived cells
#'
#' @section Usage:
#' Basic usage for cross-tabulation:
#'
#' ```r
#' # Basic crosstab
#' result <- tab(data, satisfaction, gender)
#'
#' # With filter and weight
#' result <- tab(data, satisfaction, gender, filter = age > 18, weight = "wgt")
#'
#' # Materialize to data.frame when needed
#' df <- as.data.frame(result)
#'
#' # Pipe with derive operations
#' result <- tab(data, satisfaction, gender) %>%
#'   derive(delta_vs("Male", "Female")) %>%
#'   arrange_rows(.by = "Male", .sort = "desc") %>%
#'   as.data.frame()
#' ```
#'
#' @section Compatibility:
#' Cell-based results can be materialized to data.frame using `as.data.frame()`.
#' The materialized data.frame is compatible with all utility functions
#' including `add_sig()`, `copy_tab()`, `sort_tab()`, `modify_labels()`, 
#' `tab_to_reactable()`, etc.
#'
#' @param data Data frame or survey_data object
#' @param rows Row specification (variable, expression, or helper)
#' @param cols Column specification (optional)
#' @param filter Table-wide filter expression (optional)
#' @param weight Weight variable name (optional)
#' @param statistic Type of statistic: "column_pct", "count", "row_pct", "mean"
#' @param values Variable name to aggregate for value-based statistics
#' @param show_row_nets Whether to display NET/summary rows (NET row for column_pct, Avg for mean, etc.). Summary rows are always computed, this controls visibility.
#' @param show_col_nets Whether to display NET/summary columns (NET column for row_pct, Total for count, etc.). Summary columns are always computed, this controls visibility.
#' @param show_base Whether to display Base row/column. Base is always computed, this controls visibility.
#' @param low_base_threshold Minimum base count threshold - cells/rows/cols with base below this are filtered out
#' @param label_mode How to display labels: "smart" (default - suffix for multi-item, full for single-item), 
#'   "full" (complete variable label), or "suffix" (extract suffix after separator). Can be overridden 
#'   in as.data.frame(). Stored in result for use during materialization.
#' @param helpers List of custom tab_helper objects
#' @param stats List of custom tab_stat objects
#' @param fuzzy_match Whether to enable fuzzy matching for variable names
#' @param ... Additional arguments
#' @return A tab_result object with class `tab_cell_collection`
#' @export
#' @examples
#' \dontrun{
#' # Basic usage
#' result <- tab(mtcars, factor(cyl), factor(gear))
#' df <- as.data.frame(result)
#'
#' # With derive operations
#' result <- tab(data, satisfaction, gender) %>%
#'   derive(delta_vs("Male", "Female")) %>%
#'   derive(index_vs("Total")) %>%
#'   hide_cols_except("Male|Female|Difference") %>%
#'   as.data.frame()
#'
#' # Full pipeline
#' result <- tab(data, satisfaction, region, filter = age > 18, weight = "wgt") %>%
#'   derive(sum_if("ival", dimension = "cols")) %>%
#'   arrange_rows(.by = "North", .sort = "desc") %>%
#'   hide_rows("Don't know") %>%
#'   as.data.frame()
#' }
tab <- function(data, rows, cols = NULL, filter = NULL, weight = NULL,
                statistic = c("column_pct", "count", "row_pct", "mean"),
                values = NULL,
                show_row_nets = TRUE, show_col_nets = TRUE, show_base = TRUE,
                low_base_threshold = NULL,
                label_mode = c("smart", "full", "suffix"),
                helpers = NULL, stats = NULL, fuzzy_match = FALSE,
                ...) {
  
  # Store original call
  original_call <- match.call(expand.dots = FALSE)
  caller_env <- parent.frame()
  
  ensure_builtins_registered()
  
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
  
  cols_missing <- missing(cols)
  filter_missing <- missing(filter)
  
  rows_quo <- rlang::enquo(rows)
  cols_quo <- if (!cols_missing) rlang::enquo(cols) else NULL
  filter_quo <- if (!filter_missing) rlang::enquo(filter) else NULL
  
  has_cols <- !is.null(cols_quo)
  has_filter <- !is.null(filter_quo)
  
  ## Macro detection and processing (parity with tab()) -----------------------
  
  # Track user-supplied arguments for macro processing
  formal_names <- names(formals(tab))
  call_args <- as.list(original_call)[-1]
  arg_names <- names(call_args)
  if (is.null(arg_names)) {
    arg_names <- character(length(call_args))
  }
  
  expr_label_safe <- function(expr) {
    tryCatch(
      rlang::expr_label(expr),
      error = function(e) {
        if (is.null(expr)) {
          "NULL"
        } else {
          paste(deparse(expr), collapse = " ")
        }
      }
    )
  }
  
  user_arg_labels <- list()
  
  for (i in seq_along(call_args)) {
    name_i <- arg_names[i]
    if (is.null(name_i) || !nzchar(name_i)) {
      if (i <= length(formal_names)) {
        name_i <- formal_names[i]
        arg_names[i] <- name_i
      }
    }
    if (!is.null(name_i) && nzchar(name_i)) {
      user_arg_labels[[name_i]] <- expr_label_safe(call_args[[i]])
    }
  }
  user_supplied_args <- unique(arg_names[nzchar(arg_names)])
  
  # Save original quosures before macro processing
  rows_quo_original <- rows_quo
  cols_quo_original <- cols_quo
  filter_quo_original <- filter_quo
  
  # Build params list for macro processing
  params <- list(
    rows = rows_quo_original,
    cols = if (!cols_missing) cols_quo_original else NULL,
    filter = if (!filter_missing) filter_quo_original else NULL,
    weight = weight,
    statistic = statistic,
    values = values,
    show_row_nets = show_row_nets,
    show_col_nets = show_col_nets,
    show_base = show_base,
    helpers = helpers,
    stats = stats,
    fuzzy_match = fuzzy_match
  )
  
  # Detect and process macros (reuses functions from tab.R)
  macro_rows <- detect_tab_macro(rows_quo_original, caller_env)
  if (!is.null(macro_rows)) {
    params$rows <- macro_rows
  }
  
  if (!cols_missing) {
    macro_cols <- detect_tab_macro(cols_quo_original, caller_env)
    if (!is.null(macro_cols)) {
      params$cols <- macro_cols
    }
  }
  
  if (!filter_missing) {
    macro_filter <- detect_tab_macro(filter_quo_original, caller_env)
    if (!is.null(macro_filter)) {
      params$filter <- macro_filter
    }
  }
  
  # Process macros
  params <- process_tab_macros(
    params = params,
    user_supplied = user_supplied_args,
    data = data,
    dpdict = dpdict,
    user_arg_labels = user_arg_labels
  )
  
  # Convert macro results back to quosures
  rows_quo <- convert_macro_result(params$rows, caller_env)
  cols_quo <- if (!is.null(params$cols)) convert_macro_result(params$cols, caller_env) else NULL
  filter_quo <- if (!is.null(params$filter)) convert_macro_result(params$filter, caller_env) else NULL
  
  # Update other parameters that might have been modified by macros
  weight <- params$weight
  statistic <- params$statistic
  values <- params$values
  show_row_nets <- params$show_row_nets
  show_col_nets <- params$show_col_nets
  show_base <- params$show_base
  helpers <- params$helpers
  stats <- params$stats
  fuzzy_match <- params$fuzzy_match
  
  # Update has_cols and has_filter based on potentially modified quosures
  has_cols <- !is.null(cols_quo)
  has_filter <- !is.null(filter_quo)
  
  ## Statistics validation and setup ------------------------------------------
  
  effective_stats <- .tab_registry$stats
  if (!is.null(stats)) {
    if (!is.list(stats)) {
      stop("stats must be a list of tab_stat objects")
    }
    if (!all(vapply(stats, inherits, logical(1), "tab_stat"))) {
      stop("All items in stats list must be tab_stat objects")
    }
    effective_stats <- c(stats, .tab_registry$stats)
    names(effective_stats) <- c(
      vapply(stats, `[[`, character(1), "id"),
      names(.tab_registry$stats)
    )
    effective_stats <- effective_stats[!duplicated(names(effective_stats))]
  }
  
  if (is.character(statistic)) {
    if (length(statistic) > 1) {
      statistic <- match.arg(statistic)
    }
    if (!statistic %in% names(effective_stats)) {
      stop("Unknown statistic: '", statistic, "'. Available: ",
           paste(names(effective_stats), collapse = ", "))
    }
    statistic_obj <- effective_stats[[statistic]]
  } else if (!inherits(statistic, "tab_stat")) {
    stop("statistic must be a character string or tab_stat object")
  } else {
    statistic_obj <- statistic
  }
  
  if (statistic_obj$requires_values && is.null(values)) {
    stop(statistic_obj$id, " statistic requires 'values' parameter")
  }
  if (!is.null(values) && !statistic_obj$requires_values) {
    warning("Values parameter ignored for ", statistic_obj$id, " statistic")
  }
  
  ## Label mode validation ----------------------------------------------------
  if (length(label_mode) > 1) {
    label_mode <- match.arg(label_mode)
  }
  
  ## Custom helpers validation and setup --------------------------------------
  all_helpers <- .tab_registry$helpers
  if (!is.null(helpers)) {
    if (!is.list(helpers)) {
      stop("helpers must be a list of tab_helper objects")
    }
    helper_check <- vapply(helpers, inherits, logical(1), "tab_helper")
    if (!all(helper_check)) {
      stop("All items in helpers list must be tab_helper objects")
    }
    for (helper in helpers) {
      all_helpers[[helper$id]] <- helper
    }
  }
  
  ## Variable resolution (fuzzy matching) -------------------------------------
  
  # Extract variable names from expressions for resolution
  var_candidates <- character(0)
  
  # Extract from rows
  rows_expr <- rlang::quo_get_expr(rows_quo)
  if (rlang::is_string(rows_expr)) {
    var_candidates <- c(var_candidates, rows_expr)
  }
  
  # Extract from cols if provided
  if (has_cols) {
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
  if (length(var_candidates) > 0 && fuzzy_match) {
    tryCatch({
      resolved_vars <- resolve_vars(
        data = data,
        dpdict = dpdict,
        var_names = var_candidates,
        threshold = 0.75,
        report = FALSE
      )
      
      if (length(resolved_vars) > 0) {
        message("Fuzzy matching applied to ", length(resolved_vars), " variable(s)")
      }
  
      # Update variable references with resolved names
      if (rlang::is_string(rows_expr) && rows_expr %in% names(resolved_vars)) {
        rows_expr <- resolved_vars[[rows_expr]]
        rows_quo <- rlang::quo(!!rlang::sym(rows_expr))
      }
  
      if (has_cols && rlang::is_string(cols_expr) && cols_expr %in% names(resolved_vars)) {
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
      # If resolution fails, fall back to original behavior silently
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
  base_filter_spec <- NULL
  base_filter_expr <- quote(TRUE)
  
  if (has_filter) {
    filter_eval <- tryCatch(
      rlang::eval_tidy(filter_quo, data),
      error = function(e) NULL
    )
    
    if (inherits(filter_eval, "tab_helper")) {
      filter_to_parse <- filter_eval
    } else {
      filter_to_parse <- filter_quo
    }
    
    filter_parsed <- parse_table_formula(filter_to_parse, data, dpdict, all_helpers)
    filter_logic <- formula_to_array(filter_parsed, data, dpdict, all_helpers)
    base_array <- base_array * filter_logic
    
    # Store filter specification for cell metadata
    base_filter_spec <- filter_parsed
    base_filter_expr <- rlang::quo_get_expr(filter_quo)
  }
  
  ## Parse row and column specs ------------------------------------------------
  rows_expr <- rlang::quo_get_expr(rows_quo)
  
  if (rlang::is_string(rows_expr)) {
    rows_parsed <- list(parse_table_formula(rows_quo, data, dpdict, all_helpers))
  } else {
    rows_eval <- tryCatch(
      rlang::eval_tidy(rows_quo),
      error = function(e) NULL
    )
    
    if (inherits(rows_eval, "tab_helper")) {
      rows_parsed <- list(parse_table_formula(rows_eval, data, dpdict, all_helpers))
    } else if (rlang::is_list(rows_eval)) {
      rows_parsed <- lapply(seq_along(rows_eval), function(i) {
        if (rlang::is_quosure(rows_eval[[i]])) {
          expr <- rlang::quo_get_expr(rows_eval[[i]])
          if (rlang::is_call(expr)) {
            fn_name <- as.character(expr[[1]])
            if (fn_name %in% names(all_helpers)) {
              item_eval <- rlang::eval_tidy(rows_eval[[i]], data)
            } else {
              item_eval <- rows_eval[[i]]
            }
          } else {
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
      rows_parsed <- list(parse_table_formula(rows_quo, data, dpdict, all_helpers))
    }
  }
  
  # Parse column specifications
  if (has_cols) {
    cols_expr <- rlang::quo_get_expr(cols_quo)
    
    if (rlang::is_string(cols_expr)) {
      cols_parsed <- list(parse_table_formula(cols_quo, data, dpdict, all_helpers))
    } else {
      cols_eval <- tryCatch(
        rlang::eval_tidy(cols_quo),
        error = function(e) NULL
      )
      if (inherits(cols_eval, "tab_helper")) {
        cols_parsed <- list(parse_table_formula(cols_eval, data, dpdict, all_helpers))
      } else if (rlang::is_list(cols_eval)) {
        cols_parsed <- lapply(seq_along(cols_eval), function(i) {
          if (rlang::is_quosure(cols_eval[[i]])) {
            expr <- rlang::quo_get_expr(cols_eval[[i]])
            if (rlang::is_call(expr)) {
              fn_name <- as.character(expr[[1]])
              if (fn_name %in% names(all_helpers)) {
                item_eval <- rlang::eval_tidy(cols_eval[[i]], data)
              } else {
                item_eval <- cols_eval[[i]]
              }
            } else {
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
    expanded <- expand_variables(row_spec, data, dpdict, statistic_obj$id, values,
                                 label_mode = label_mode, all_helpers = all_helpers)
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
      # Check if this is a spec generator (banner)
      if (col_spec$type == "helper") {
        helper_obj <- .tab_registry$helpers[[col_spec$helper_type]]
        if (!is.null(helper_obj) && isTRUE(helper_obj$returns_specs)) {
          # This helper returns specifications, not arrays
          # First, process the helper to get components populated
          processed_spec <- process_helper_for_specs(col_spec, data, dpdict, all_helpers)
          
          # Call the spec generator to get column specs
          col_specs <- helper_obj$processor(processed_spec, data, dpdict, all_helpers)
          
          # col_specs is now a list of specifications
          # Each spec will be converted to array during compute stage
          for (spec in col_specs) {
            # Normalize the spec to ensure it has an expr field for matching
            normalized_spec <- normalize_spec_expression(spec)
            cols_expanded <- append(cols_expanded, list(normalized_spec))
          }
          next
        }
      }
      
      # Normal expansion
      expanded <- expand_variables(col_spec, data, dpdict, statistic_obj$id, values_var = NULL, 
                                   label_mode = label_mode, all_helpers = all_helpers)
      cols_expanded <- append(cols_expanded, expanded)
    }
  } else {
    cols_expanded <- list(normalize_spec_expression(list(type = "total", label = "Total")))
  }
  
  ## Validate variables for statistics (parity with tab()) --------------------
  
  # Validate rows and columns for value statistics
  if (!is.null(statistic_obj) && statistic_obj$requires_values) {
    # Throws error if variables inappropriate for statistic, else continues
    validate_statistic_variables(statistic_obj, rows_expanded, cols_expanded, data, dpdict)
  }
  
  ## Create row, column and value arrays ---------------------------------------
  # Create arrays for each specification
  
  row_arrays <- list()
  rows_expanded_final <- list()
  
  for (spec in rows_expanded) {
    array_result <- formula_to_array(spec, data, dpdict, all_helpers = all_helpers)
    
    # Check if helper returned multiple arrays
    if (is.list(array_result) && isTRUE(attr(array_result, "is_multi_column"))) {
      for (i in seq_along(names(array_result))) {
        row_name <- names(array_result)[i]
        member_array <- array_result[[row_name]]
        row_arrays <- append(row_arrays, list(member_array))
        
        # Add .member_index to original expression for uniqueness
        specialized_expr <- if (is.call(spec$expr)) {
          expr_copy <- spec$expr
          expr_copy[[length(expr_copy) + 1]] <- i
          names(expr_copy)[length(expr_copy)] <- ".member_index"
          expr_copy
        } else {
          # Wrap non-call expressions
          call("_identity_", spec$expr, .member_index = i)
        }
        
        new_spec <- list(
          type = spec$type,
          helper_type = spec$helper_type,
          label = row_name,
          expr = specialized_expr,
          dsl = attr(member_array, "meta")$dsl
        )
        
        rows_expanded_final <- append(rows_expanded_final, list(new_spec))
      }
    } else {
      row_arrays <- append(row_arrays, list(array_result))
      rows_expanded_final <- append(rows_expanded_final, list(spec))
    }
  }
  
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
        array_result <- formula_to_array(spec, data, dpdict, all_helpers = all_helpers)
        
        if (is.list(array_result) && isTRUE(attr(array_result, "is_multi_column"))) {
          for (i in seq_along(names(array_result))) {
            col_name <- names(array_result)[i]
            member_array <- array_result[[col_name]]
            arrays_list <- append(arrays_list, list(member_array))
            
            # Add .member_index to original expression for uniqueness
            specialized_expr <- if (is.call(spec$expr)) {
              expr_copy <- spec$expr
              expr_copy[[length(expr_copy) + 1]] <- i
              names(expr_copy)[length(expr_copy)] <- ".member_index"
              expr_copy
            } else {
              call("_identity_", spec$expr, .member_index = i)
            }
            
            new_spec <- list(
              type = spec$type,
              helper_type = spec$helper_type,
              label = col_name,
              expr = specialized_expr,
              dsl = attr(member_array, "meta")$dsl
            )
            
            expanded_specs <- append(expanded_specs, list(new_spec))
          }
        } else {
          arrays_list <- append(arrays_list, list(array_result))
          expanded_specs <- append(expanded_specs, list(spec))
        }
      }
    }
    
    cols_expanded <- expanded_specs
    arrays_list
  } else {
    array <- rep(1, nrow(data))
    list(array)
  }
  
  # Create values array if needed
  values_array <- NULL
  if (!is.null(values)) {
    if (!values %in% names(data)) {
      stop("Values variable '", values, "' not found in data")
    }
    values_array <- data[[values]]
    
    if (!is.numeric(values_array)) {
      stop("Values variable must be numeric for mean calculations")
    }
  }
  
  ## Check for label collisions (parity with tab()) ---------------------------
  
  # Check for label collisions with Base or summary rows and columns
  meta_labels <- c(statistic_obj$base_label)
  if (!is.null(statistic_obj$summary_row)) {
    meta_labels <- c(meta_labels, statistic_obj$summary_row)
  }
  if (!is.null(statistic_obj$summary_col)) {
    meta_labels <- c(meta_labels, statistic_obj$summary_col)
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
    for (i in seq_along(cols_expanded)) {
      spec <- cols_expanded[[i]]
      spec_label <- spec$label
      if (spec_label %in% meta_labels) {
        stop("Label collision detected: Column label '", spec_label,
             "' matches a reserved meta-category label. ",
             "Reserved labels are: ", paste(meta_labels, collapse = ", "), ". ",
             "Please rename your data or use a custom label in cols specification.")
      }
    }
  }
  
  ## Create cell bundle directly from arrays ----------------------------------
  # Cell-native computation: create cells directly from array intersections
  # without intermediate matrix structures. This maintains position-independence
  # and enables dimension-agnostic operations.
  compute_result <- compute_cells_as_bundle(
    base_array = base_array,
    row_arrays = row_arrays,
    col_arrays = col_arrays,
    rows_expanded = rows_expanded,
    cols_expanded = cols_expanded,
    base_filter_spec = base_filter_spec,
    base_filter_expr = base_filter_expr,
    statistic_obj = statistic_obj,
    values_array = values_array,
    values_var = values
  )
  
  store <- compute_result$store
  summary_row_array <- compute_result$summary_row_array
  summary_row_spec <- compute_result$summary_row_spec
  summary_col_array <- compute_result$summary_col_array
  summary_col_spec <- compute_result$summary_col_spec
  
  # Store dimensions for layout construction
  n_rows <- length(row_arrays)
  n_cols <- length(col_arrays)
  
  ## Build layout with layout_defs (NEW SYSTEM) --------------------------------
  # Initialize layout_defs from expanded specs
  has_summary_row <- !is.null(summary_row_spec)
  has_summary_col <- !is.null(summary_col_spec)
  
  # Create layout_defs for rows and columns
  # Only include summary specs if they should be shown
  row_summary_spec <- if (has_summary_row && show_row_nets) summary_row_spec else NULL
  col_summary_spec <- if (has_summary_col && show_col_nets) summary_col_spec else NULL
  
  row_defs <- initialize_layout_defs(rows_expanded, row_summary_spec, dimension = "row")
  col_defs <- initialize_layout_defs(cols_expanded, col_summary_spec, dimension = "col")
  
  ## Build filter_rules from parameters ----------------------------------------
  filter_rules <- list()
  
  # Add low base threshold filter if specified
  if (!is.null(low_base_threshold)) {
    filter_rules <- c(filter_rules, list(new_layout_def(
      base_matcher = threshold_matcher(low_base_threshold, ">="),
      label = paste0("base >= ", low_base_threshold),
      dimension = "row"  # Applies to all cells
    )))
  }
  
  # Note: show_row_nets and show_col_nets are now handled by not creating
  # the summary row_def/col_def in the first place (see row_summary_spec/col_summary_spec above)
  # We don't need filter_rules for these anymore.
  
  ## Two-stage allocation ------------------------------------------------------
  # Stage 1: Filter cell pool
  cell_pool <- filter_cell_pool(store, filter_rules)
  
  # Stage 2: Allocate to grid
  layout <- allocate_cells_to_grid(store, row_defs, col_defs, cell_pool)
  
  # Preserve filter_rules in layout
  layout$filter_rules <- filter_rules
  
  ## Store arrays for significance testing ------------------------------------
  arrays <- list(
    base_array = base_array,
    row_arrays = row_arrays,
    col_arrays = col_arrays,
    values_array = values_array,
    summary_row_array = summary_row_array,
    summary_col_array = summary_col_array
  )
  
  ## Build cell-based tab_result -----------------------------------------------
  result <- structure(list(
    cell_store = store,
    layout = layout,
    arrays = arrays,
    
    data = data,
    dpdict = dpdict,
    statistic = statistic_obj,
    show_base = show_base,
    label_mode = label_mode,
    
    derive_operations = list(),
    formatting = list(),
    
    call = original_call
    
  ), class = c("tab_result", "tab_cell_collection"))
  
  ## Note: Visibility is now controlled via filter_rules -------------------------
  # The hide operations (show_row_nets, show_col_nets, show_base) are now handled
  # via filter_rules during allocation. No post-processing needed.
  
  return(result)
}

#' Build explicit grid layout from cell store and specifications
#'
#' @param store Cell store object
#' @param row_specs List of row specifications from expand stage
#' @param col_specs List of column specifications from expand stage
#' @return Layout list with grid, labels, and specifications
#' @keywords internal
build_explicit_grid_layout <- function(store, row_specs, col_specs) {
  n_rows <- length(row_specs)
  n_cols <- length(col_specs)
  grid <- matrix(NA_character_, n_rows, n_cols)
  
  # Get all cells
  all_cell_ids <- all_cell_ids(store)
  all_cells <- get_cells(store, all_cell_ids)
  
  # Match cells to grid positions by exact specification
  for (i in seq_along(row_specs)) {
    row_spec <- row_specs[[i]]
    
    for (j in seq_along(col_specs)) {
      col_spec <- col_specs[[j]]
      
      # Find cell with matching row and col specifications
      matching_cell <- Find(function(cell) {
        row_match <- specs_equal(cell$specification$row_expr, row_spec$expr)
        col_match <- specs_equal(cell$specification$col_expr, col_spec$expr)
        row_match && col_match
      }, all_cells)
      
      if (!is.null(matching_cell)) {
        grid[i, j] <- matching_cell$cell_id
      }
      
      # If multiple cells match, Find() returns first (TO DO: better handling of this)
    }
  }
  
  # Extract labels from specifications
  row_labels <- sapply(row_specs, function(spec) spec$label %||% "")
  col_labels <- sapply(col_specs, function(spec) spec$label %||% "")
  
  list(
    type = "explicit_grid",
    grid = grid,
    row_labels = row_labels,
    col_labels = col_labels
  )
}

#' Filter layout grid based on low base thresholds
#' @param layout Layout list with grid, row_labels, col_labels, etc
#' @param store Cell store
#' @param threshold Minimum base count threshold
#' @keywords internal
filter_low_base_cells <- function(layout, store, threshold) {
  grid <- layout$grid
  n_rows <- nrow(grid)
  n_cols <- ncol(grid)
  
  # Iterate through each cell in the grid
  for (i in 1:n_rows) {
    for (j in 1:n_cols) {
      cell_id <- grid[i, j]
      
      # Skip if already NA
      if (is.na(cell_id)) next
      
      # Check if cell exists in store
      if (!has_cell(store, cell_id)) next
      
      # Get cell base
      cell <- get_cell(store, cell_id)
      
      # Filter: set to NA if base < threshold
      if (!is.na(cell$base) && cell$base < threshold) {
        grid[i, j] <- NA_integer_
      }
    }
  }
  
  # Check for all-NA rows
  all_na_rows <- apply(grid, 1, function(row) all(is.na(row)))
  
  # Check for all-NA columns
  all_na_cols <- apply(grid, 2, function(col) all(is.na(col)))
  
  # Remove all-NA rows (except special rows like Base)
  if (any(all_na_rows)) {
    # Identify special rows (Base, Total, NET) - keep these even if NA
    special_rows <- grepl("^(Base|Total|NET)", layout$row_labels)
    rows_to_keep <- !all_na_rows | special_rows
    
    if (!all(rows_to_keep)) {
      grid <- grid[rows_to_keep, , drop = FALSE]
      layout$row_labels <- layout$row_labels[rows_to_keep]
    }
  }
  
  # Remove all-NA columns
  if (any(all_na_cols)) {
    cols_to_keep <- !all_na_cols
    
    if (!all(cols_to_keep)) {
      grid <- grid[, cols_to_keep, drop = FALSE]
      layout$col_labels <- layout$col_labels[cols_to_keep]
    }
  }
  
  # Update layout
  layout$grid <- grid
  
  # Store filtering metadata
  layout$filtered_cells <- list(
    threshold = threshold,
    timestamp = Sys.time()
  )
  
  return(layout)
}

##### Glue tab #####

#' Glue two tab() results together
#'
#' Combine two tab_cell_collection objects by merging their cell stores and layouts.
#' Preserves the cell-based architecture throughout.
#'
#' @param tab1 First tab_cell_collection object
#' @param tab2 Second tab_cell_collection object
#' @param direction Direction to glue: "cols" (side-by-side) or "rows" (stacked)
#' @param sep Separator for combining labels
#' @param prefix Optional prefix for tab2 labels
#' @return A tab_cell_collection object with merged cells
#' @export
#' @examples
#' \dontrun{
#' # Glue two tables side-by-side
#' tab1 <- tab(data, gender, region)
#' tab2_obj <- tab(data, gender, brand)
#' result <- glue_tab(tab1, tab2_obj, direction = "cols")
#' }
glue_tab <- function(tab1, tab2,
                     direction = c("cols", "rows"),
                     sep = ": ",
                     prefix = NULL) {
  
  direction <- match.arg(direction)
  
  # Validate both are tab_cell_collection
  if (!inherits(tab1, "tab_cell_collection") || !inherits(tab2, "tab_cell_collection")) {
    stop("glue_tab() requires tab_cell_collection objects from tab()")
  }
  
  # Check compatibility
  check_glue_compatibility(tab1, tab2, direction)
  
  if (direction == "cols") {
    result <- glue_tabs_cols_cellbased(tab1, tab2, sep, prefix)
  } else {
    result <- glue_tabs_rows_cellbased(tab1, tab2, sep, prefix)
  }
  
  return(result)
}

#' Check if two tab results can be glued together
#' @keywords internal
check_glue_compatibility <- function(tab1, tab2, direction) {
  # Check statistics match
  stat1_id <- tab1$statistic$id
  stat2_id <- tab2$statistic$id
  
  if (stat1_id != stat2_id) {
    warning("Gluing tabs with different statistics: ", stat1_id, " and ", stat2_id)
  }
  
  if (direction == "cols") {
    # For column gluing, check row compatibility
    if (!identical(tab1$layout$row_labels, tab2$layout$row_labels)) {
      common_rows <- intersect(tab1$layout$row_labels, tab2$layout$row_labels)
      if (length(common_rows) == 0) {
        stop("No common row labels found between tabs")
      }
      warning("Only ", length(common_rows), " common rows. Non-matching rows will be dropped.")
    }
  } else {
    # For row gluing, check column compatibility
    if (!identical(tab1$layout$col_labels, tab2$layout$col_labels)) {
      common_cols <- intersect(tab1$layout$col_labels, tab2$layout$col_labels)
      if (length(common_cols) == 0) {
        stop("No common column labels found between tabs")
      }
      warning("Only ", length(common_cols), " common cols. Non-matching cols will be dropped.")
    }
  }
  
  invisible(TRUE)
}

#' Merge two cell stores
#' @keywords internal
merge_cell_stores <- function(store1, store2) {
  merged <- new_cell_store()
  
  # Copy all cells from store1 (IDs unchanged)
  for (cell_id in all_cell_ids(store1)) {
    cell <- get_cell(store1, cell_id)
    add_cell(merged, cell$value, cell$base, cell$specification, 
             cell$computation, cell$metadata)
  }
  
  # Copy all cells from store2 (track new IDs for remapping)
  id_mapping <- list()
  for (cell_id in all_cell_ids(store2)) {
    cell <- get_cell(store2, cell_id)
    new_id <- add_cell(merged, cell$value, cell$base, cell$specification,
                      cell$computation, cell$metadata)
    id_mapping[[as.character(cell_id)]] <- new_id
  }
  
  list(store = merged, id_mapping = id_mapping)
}

#' Glue tabs side-by-side (columns)
#' @keywords internal
glue_tabs_cols_cellbased <- function(tab1, tab2, sep, prefix) {
  # Merge cell stores
  merged_result <- merge_cell_stores(tab1$cell_store, tab2$cell_store)
  merged_store <- merged_result$store
  id_mapping <- merged_result$id_mapping
  
  # Find common rows
  common_rows <- intersect(tab1$layout$row_labels, tab2$layout$row_labels)
  
  # Get indices of common rows in both tabs
  tab1_row_idx <- match(common_rows, tab1$layout$row_labels)
  tab2_row_idx <- match(common_rows, tab2$layout$row_labels)
  
  # Build new grid
  n_rows <- length(common_rows)
  n_cols1 <- ncol(tab1$layout$grid)
  n_cols2 <- ncol(tab2$layout$grid)
  new_grid <- matrix(NA_integer_, nrow = n_rows, ncol = n_cols1 + n_cols2)
  
  # Copy cells from tab1 (left side, IDs unchanged)
  new_grid[, 1:n_cols1] <- tab1$layout$grid[tab1_row_idx, , drop = FALSE]
  
  # Copy cells from tab2 (right side, IDs remapped)
  tab2_grid_subset <- tab2$layout$grid[tab2_row_idx, , drop = FALSE]
  for (i in 1:nrow(tab2_grid_subset)) {
    for (j in 1:ncol(tab2_grid_subset)) {
      old_id <- tab2_grid_subset[i, j]
      if (!is.na(old_id)) {
        new_id <- id_mapping[[as.character(old_id)]]
        new_grid[i, n_cols1 + j] <- new_id
      }
    }
  }
  
  # Combine labels
  new_col_labels <- c(tab1$layout$col_labels, 
                     if (!is.null(prefix)) {
                       paste0(prefix, sep, tab2$layout$col_labels)
                     } else {
                       tab2$layout$col_labels
                     })
  
  # Build new layout
  new_layout <- list(
    type = "explicit_grid",
    grid = new_grid,
    row_labels = common_rows,
    col_labels = new_col_labels,
    has_summary_row = tab1$layout$has_summary_row || tab2$layout$has_summary_row,
    has_summary_col = tab1$layout$has_summary_col || tab2$layout$has_summary_col
  )
  
  # Merge arrays
  merged_arrays <- list(
    base_array = tab1$arrays$base_array,  # Use tab1's base
    row_arrays = tab1$arrays$row_arrays,
    col_arrays = c(tab1$arrays$col_arrays, tab2$arrays$col_arrays),
    values_array = tab1$arrays$values_array
  )
  
  # Build result
  result <- structure(list(
    cell_store = merged_store,
    layout = new_layout,
    arrays = merged_arrays,
    base_spec = tab1$base_spec,
    data = tab1$data,
    dpdict = tab1$dpdict,
    statistic = tab1$statistic,
    calc_base = tab1$calc_base,
    derive_operations = list(),
    formatting = list(),
    call = call("glue_tab2", quote(tab1), quote(tab2))
  ), class = c("tab_result", "tab_cell_collection"))
  
  return(result)
}

#' Glue tabs vertically (rows)
#' @keywords internal
glue_tabs_rows_cellbased <- function(tab1, tab2, sep, prefix) {
  # Merge cell stores
  merged_result <- merge_cell_stores(tab1$cell_store, tab2$cell_store)
  merged_store <- merged_result$store
  id_mapping <- merged_result$id_mapping
  
  # Find common columns
  common_cols <- intersect(tab1$layout$col_labels, tab2$layout$col_labels)
  
  # Get indices of common columns in both tabs
  tab1_col_idx <- match(common_cols, tab1$layout$col_labels)
  tab2_col_idx <- match(common_cols, tab2$layout$col_labels)
  
  # Build new grid
  n_rows1 <- nrow(tab1$layout$grid)
  n_rows2 <- nrow(tab2$layout$grid)
  n_cols <- length(common_cols)
  new_grid <- matrix(NA_integer_, nrow = n_rows1 + n_rows2, ncol = n_cols)
  
  # Copy cells from tab1 (top, IDs unchanged)
  new_grid[1:n_rows1, ] <- tab1$layout$grid[, tab1_col_idx, drop = FALSE]
  
  # Copy cells from tab2 (bottom, IDs remapped)
  tab2_grid_subset <- tab2$layout$grid[, tab2_col_idx, drop = FALSE]
  for (i in 1:nrow(tab2_grid_subset)) {
    for (j in 1:ncol(tab2_grid_subset)) {
      old_id <- tab2_grid_subset[i, j]
      if (!is.na(old_id)) {
        new_id <- id_mapping[[as.character(old_id)]]
        new_grid[n_rows1 + i, j] <- new_id
      }
    }
  }
  
  # Combine labels
  new_row_labels <- c(tab1$layout$row_labels, 
                     if (!is.null(prefix)) {
                       paste0(prefix, sep, tab2$layout$row_labels)
                     } else {
                       tab2$layout$row_labels
                     })
  
  # Build new layout
  new_layout <- list(
    type = "explicit_grid",
    grid = new_grid,
    row_labels = new_row_labels,
    col_labels = common_cols,
    has_summary_row = tab1$layout$has_summary_row || tab2$layout$has_summary_row,
    has_summary_col = tab1$layout$has_summary_col || tab2$layout$has_summary_col
  )
  
  # Merge arrays
  merged_arrays <- list(
    base_array = tab1$arrays$base_array,  # Use tab1's base
    row_arrays = c(tab1$arrays$row_arrays, tab2$arrays$row_arrays),
    col_arrays = tab1$arrays$col_arrays,
    values_array = tab1$arrays$values_array
  )
  
  # Build result
  result <- structure(list(
    cell_store = merged_store,
    layout = new_layout,
    arrays = merged_arrays,
    base_spec = tab1$base_spec,
    data = tab1$data,
    dpdict = tab1$dpdict,
    statistic = tab1$statistic,
    calc_base = tab1$calc_base,
    derive_operations = list(),
    formatting = list(),
    call = call("glue_tab2", quote(tab1), quote(tab2))
  ), class = c("tab_result", "tab_cell_collection"))
  
  return(result)
}

##### Multi-tab #####

#' Run tab() multiple times with different filters and combine results
#'
#' This function creates multiple tab() results filtered by different groups
#' and combines them using glue_tab(). The `by` parameter specifies how to
#' split the data into groups.
#'
#' @param data Survey data (data.frame or survey_data object)
#' @param rows Row specification (passed to tab)
#' @param cols Column specification (passed to tab)
#' @param by Grouping specification (see Details)
#' @param direction Direction to combine tables: "cols" or "rows"
#' @param include_total Whether to include a total (unfiltered) table
#' @param total_name Label for the total column/row
#' @param sep Separator for combining group names with labels
#' @param statistic Statistic to compute (passed to tab)
#' @param ... Additional arguments passed to tab()
#' @return A tab_cell_collection object with combined results
#' @export
#' @examples
#' \dontrun{
#' # Split by gender
#' result <- multi_tab(data, satisfaction, by = gender, direction = "cols")
#' 
#' # Split by age groups
#' result <- multi_tab(data, brand, by = list(
#'   "Young" = age < 35,
#'   "Middle" = age >= 35 & age < 55,
#'   "Older" = age >= 55
#' ))
#' }
multi_tab <- function(data, rows, cols, by,
                       direction = c("cols", "rows"),
                       include_total = TRUE,
                       total_name = "Total",
                       sep = ": ",
                       statistic = "column_pct",
                       ...) {
  
  direction <- match.arg(direction)
  
  # Extract data frame and dpdict
  if (inherits(data, "survey_data")) {
    df <- data$dat
    dpdict <- data$dpdict
  } else {
    df <- data
    dpdict <- NULL
  }
  
  # Parse by parameter
  by_quo <- rlang::enquo(by)
  groups <- parse_by_parameter(by_quo, df, dpdict)
  
  if (length(groups) == 0) {
    stop("No groups found from 'by' specification")
  }
  
  # Generate tab for each group
  result_tabs <- list()
  
  for (group_name in names(groups)) {
    group_filter <- groups[[group_name]]
    group_mask <- eval(group_filter, df, parent.frame())
    group_mask[is.na(group_mask)] <- FALSE
    
    group_df <- df[group_mask, , drop = FALSE]
    
    if (nrow(group_df) == 0) {
      warning("Group '", group_name, "' has no data, skipping")
      next
    }
    
    # Reconstruct survey_data
    if (!is.null(dpdict)) {
      group_data <- structure(list(dat = group_df, dpdict = dpdict), 
                             class = "survey_data")
    } else {
      group_data <- group_df
    }
    
    # Call tab() for this group
    if (missing(cols)) {
      group_tab <- tab(group_data, rows = {{ rows }}, statistic = statistic, ...)
    } else {
      group_tab <- tab(group_data, rows = {{ rows }}, cols = {{ cols }}, 
                       statistic = statistic, ...)
    }
    
    result_tabs[[group_name]] <- group_tab
  }
  
  # Optionally include total
  if (include_total) {
    if (missing(cols)) {
      total_tab <- tab(data, rows = {{ rows }}, statistic = statistic, ...)
    } else {
      total_tab <- tab(data, rows = {{ rows }}, cols = {{ cols }}, 
                       statistic = statistic, ...)
    }
    
    # Insert total at the beginning or end based on direction
    if (direction == "cols") {
      result_tabs <- c(list(Total = total_tab), result_tabs)
    } else {
      result_tabs <- c(result_tabs, list(Total = total_tab))
    }
  }
  
  # Combine using glue_tab()
  if (length(result_tabs) == 1) {
    result <- result_tabs[[1]]
  } else {
    # For the first tab, we need to apply its prefix manually since glue_tab
    # only applies prefix to the second tab
    result <- result_tabs[[1]]
    first_name <- names(result_tabs)[1]
    
    # Apply prefix based on direction
    if (!is.null(first_name) && nzchar(first_name)) {
      if (direction == "cols") {
        # Prefix column labels
        old_col_labels <- result$layout$col_labels
        result$layout$col_labels <- paste0(first_name, sep, old_col_labels)
      } else {
        # direction == "rows" - prefix row labels
        old_row_labels <- result$layout$row_labels
        result$layout$row_labels <- paste0(first_name, sep, old_row_labels)
      }
    }
    
    # Now glue with remaining tabs
    for (i in 2:length(result_tabs)) {
      result <- glue_tab(result, result_tabs[[i]], 
                         direction = direction, 
                         sep = sep,
                         prefix = names(result_tabs)[i])
    }
  }
  
  # Add metadata
  attr(result, "multi_tab_groups") <- names(groups)
  
  return(result)
}

# Convenience wrappers
#' @rdname multi_tab
#' @export
multi_tab_cols <- function(data, rows, cols = NULL, by, ...) {
  if (missing(cols)) {
    multi_tab(data, rows = {{ rows }},
              by = {{ by }}, direction = "cols", ...)
  } else {
    multi_tab(data, rows = {{ rows }}, cols = {{ cols }},
              by = {{ by }}, direction = "cols", ...)
  }
}

#' @rdname multi_tab
#' @export
multi_tab_rows <- function(data, rows, cols = NULL, by, ...) {
  if (missing(cols)) {
    multi_tab(data, rows = {{ rows }},
              by = {{ by }}, direction = "rows", ...)
  } else {
    multi_tab(data, rows = {{ rows }}, cols = {{ cols }},
              by = {{ by }}, direction = "rows", ...)
  }
}

#' Parse the flexible 'by' parameter into named filter expressions
#'
#' @param by_quo The by parameter as a quosure from multi_tab
#' @param data The data frame
#' @param dpdict Optional data dictionary
#' @return Named list of filter expressions
#' @keywords internal
parse_by_parameter <- function(by_quo, data, dpdict = NULL) {
  # Get the expression from the quosure
  by_expr <- rlang::quo_get_expr(by_quo)

  # Case 1: Simple variable name (splits by all unique values)
  # Examples: by = country, by = region
  if (rlang::is_symbol(by_expr)) {
    var_name <- as.character(by_expr)
    return(create_groups_from_variable(var_name, data, dpdict))
  }

  # Case 2: String variable name
  # Example: by = "country"
  if (rlang::is_string(by_expr)) {
    return(create_groups_from_variable(by_expr, data, dpdict))
  }

  # Case 3: Subsetting expression (e.g., country[c("UK", "US")])
  if (rlang::is_call(by_expr, "[") || rlang::is_call(by_expr, "[[")) {
    var_name <- as.character(by_expr[[2]])

    # Get the values from the subsetting
    values_expr <- by_expr[[3]]
    if (rlang::is_call(values_expr, "c")) {
      # Extract values from c() call
      values <- sapply(rlang::call_args(values_expr), rlang::eval_tidy)
    } else {
      values <- rlang::eval_tidy(values_expr)
    }

    return(create_groups_from_values(var_name, values, data, dpdict))
  }

  # Case 4: %in% expression (e.g., country %in% c("UK", "US"))
  if (rlang::is_call(by_expr, "%in%")) {
    var_name <- as.character(by_expr[[2]])
    values <- rlang::eval_tidy(by_expr[[3]])
    return(create_groups_from_values(var_name, values, data, dpdict))
  }

  # Case 5: Named list of expressions
  # Example: by = list(europe = country %in% c("UK", "DE"), high_income = income > 50000)
  # First try to evaluate the quosure to see if it's a list
  by_eval <- tryCatch(
    rlang::eval_tidy(by_quo),
    error = function(e) NULL
  )

  if (is.list(by_eval) && !is.null(names(by_eval))) {
    if (any(names(by_eval) == "")) {
      stop("When 'by' is a list, all elements must be named")
    }

    # Convert each expression to proper filter
    groups <- list()
    for (name in names(by_eval)) {
      # The list elements are already expressions
      groups[[name]] <- by_eval[[name]]
    }

    return(groups)
  }

  # Case 6: Direct evaluation (for backward compatibility)
  # Try to evaluate the expression - might return a list
  result <- tryCatch(
    rlang::eval_tidy(by_quo, data),
    error = function(e) NULL
  )

  if (is.list(result) && !is.null(names(result))) {
    return(result)
  }

  stop("Invalid 'by' specification. Use a variable name, named list of expressions, ",
       "or a subsetting expression like country[c('UK', 'US')]")
}

#' Create groups from all unique values of a variable
#' @keywords internal
create_groups_from_variable <- function(var_name, data, dpdict = NULL) {
  if (!var_name %in% names(data)) {
    stop("Variable '", var_name, "' not found in data")
  }

  # Get unique values
  values <- unique(data[[var_name]])
  values <- values[!is.na(values)]
  values <- sort(values)

  if (length(values) == 0) {
    stop("No valid values found in '", var_name, "'")
  }

  # Get labels using the existing function from builtins.R
  labels <- get_variable_labels(var_name, values, data, dpdict)

  # Create filter expressions
  groups <- list()
  for (i in seq_along(values)) {
    val <- values[i]
    label <- labels[i]

    # Create filter expression
    if (is.character(val)) {
      filter_expr <- bquote(.(as.name(var_name)) == .(val))
    } else {
      filter_expr <- bquote(.(as.name(var_name)) == .(val))
    }

    groups[[label]] <- filter_expr
  }

  return(groups)
}

#' Create groups from specific values of a variable
#' @keywords internal
create_groups_from_values <- function(var_name, values, data, dpdict = NULL) {
  if (!var_name %in% names(data)) {
    stop("Variable '", var_name, "' not found in data")
  }

  # Validate values exist
  actual_values <- unique(data[[var_name]])
  actual_values <- actual_values[!is.na(actual_values)]
  invalid <- setdiff(values, actual_values)
  if (length(invalid) > 0) {
    warning("Values not found in data: ", paste(invalid, collapse = ", "))
    values <- intersect(values, actual_values)
  }

  if (length(values) == 0) {
    stop("No valid values specified")
  }

  # Get labels
  labels <- get_variable_labels(var_name, values, data, dpdict)

  # Create filter expressions
  groups <- list()
  for (i in seq_along(values)) {
    val <- values[i]
    label <- labels[i]

    # Create filter expression
    if (is.character(val)) {
      filter_expr <- bquote(.(as.name(var_name)) == .(val))
    } else {
      filter_expr <- bquote(.(as.name(var_name)) == .(val))
    }

    groups[[label]] <- filter_expr
  }

  return(groups)
}