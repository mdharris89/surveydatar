# Core Computation Engine
#
# Cell-native computation functions:
# - compute_cells_as_bundle: Direct array → cell computation (no intermediate matrices)
# - build_cell_specification: Attach full metadata to cells for semantic operations
# - calculate_base_array: Apply filters and weights to base
# - Cells store: value, base, specification, computation, derivation metadata
#
# Cell-native approach:
# - Avoids intermediate matrix structures
# - Maintains position-independence
# - Enables dimension-agnostic operations

#' Sync base matrix with current row and column arrays
#'
#' This function recalculates the base matrix whenever the dimensions change,
#' ensuring consistency between arrays and bases.
#'
#' @param row_arrays List of row arrays
#' @param col_arrays List of column arrays
#' @param base_array Base array for filtering
#' @param values_array Optional values array for mean calculations
#' @param statistic The statistic object containing base_calculator
#' @return Matrix of base values with dimensions matching row/col arrays
#' @keywords internal
sync_base_matrix <- function(row_arrays, col_arrays, base_array,
                             row_u_arrays = NULL, col_u_arrays = NULL,
                             values_array, statistic) {

  n_r <- length(row_arrays)
  n_c <- length(col_arrays)

  if (n_r == 0 || n_c == 0) {
    return(NULL)
  }

  base_matrix <- matrix(NA_real_, n_r, n_c)

  for (i in seq_len(n_r)) {
    r_arr <- row_arrays[[i]]
    r_u <- if (!is.null(row_u_arrays) && length(row_u_arrays) >= i) row_u_arrays[[i]] else NULL

    for (j in seq_len(n_c)) {
      c_arr <- col_arrays[[j]]
      c_u <- if (!is.null(col_u_arrays) && length(col_u_arrays) >= j) col_u_arrays[[j]] else NULL

      base_matrix[i, j] <- statistic$base_calculator(
        base_array = base_array,
        row_m = r_arr,
        row_u = r_u,
        col_m = c_arr,
        col_u = c_u,
        values_array = values_array
      )
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
                                     row_u_arrays = NULL, col_u_arrays = NULL,
                                     statistic = "column_pct", values = NULL) {

  n_rows <- length(row_arrays)
  n_cols <- length(col_arrays)
  res    <- matrix(NA_real_, n_rows, n_cols)

  stat_fun <- statistic$processor

  for (i in seq_len(n_rows)) {
    r_arr   <- row_arrays[[i]]
    r_u <- if (!is.null(row_u_arrays) && length(row_u_arrays) >= i) row_u_arrays[[i]] else NULL

    for (j in seq_len(n_cols)) {
      c_arr  <- col_arrays[[j]]
      c_u <- if (!is.null(col_u_arrays) && length(col_u_arrays) >= j) col_u_arrays[[j]] else NULL

      res[i, j] <- stat_fun(
        base_array = base_array,
        row_m = r_arr,
        row_u = r_u,
        col_m = c_arr,
        col_u = c_u,
        values = values
      )
    }
  }
  res
}

#' Compute cells directly as a bundle (cell-based architecture)
#'
#' Creates cell objects directly from array intersections without 
#' intermediate matrix structures. This is the cell-native computation
#' approach for tab2() that maintains position-independence and avoids
#' assuming 2D structure.
#'
#' Unlike the matrix-based approach used in tab(), this function computes
#' each cell's value and base directly from array intersections and immediately
#' stores them in the cell store with full metadata. This enables true
#' dimension-agnostic computation where cells exist independently of layout.
#'
#' @param base_array Base filtering array
#' @param row_arrays List of row arrays
#' @param col_arrays List of column arrays  
#' @param rows_expanded List of expanded row specifications
#' @param cols_expanded List of expanded column specifications
#' @param base_filter_spec Parsed base filter specification
#' @param base_filter_expr Base filter expression
#' @param statistic_obj Statistic object with processor and base_calculator
#' @param values_array Optional values array for mean/aggregate stats
#' @return cell_store object with all computed cells
#' @keywords internal
compute_cells_as_bundle <- function(base_array,
                                   data,
                                   dpdict,
                                   row_arrays,
                                   row_u_arrays,
                                   col_arrays,
                                   col_u_arrays,
                                   rows_expanded,
                                   cols_expanded,
                                   base_filter_spec,
                                   base_filter_expr,
                                   statistic_obj,
                                   values_array = NULL,
                                   values_var = NULL) {
  
  n_cells <- length(row_arrays) * length(col_arrays)
  
  store <- new_cell_store(size_hint = n_cells)
  
  compute_exposure <- function(arr) {
    if (is.null(arr)) return(0)
    contrib <- base_array * arr
    contrib[is.na(contrib)] <- 0
    sum(contrib)
  }
  
  # Row/col exposure is used for layout pruning (hide_empty) and represents
  # membership mass under the table base.
  row_exposures <- if (length(row_arrays) > 0) {
    vapply(row_arrays, compute_exposure, numeric(1))
  } else numeric(0)
  
  col_exposures <- if (length(col_arrays) > 0) {
    vapply(col_arrays, compute_exposure, numeric(1))
  } else numeric(0)
  
  # Compute each cell directly from array intersections
  for (i in seq_along(row_arrays)) {
    row_array <- row_arrays[[i]]
    row_u <- row_u_arrays[[i]]
    row_spec <- rows_expanded[[i]]
    
    for (j in seq_along(col_arrays)) {
      col_array <- col_arrays[[j]]
      col_u <- col_u_arrays[[j]]
      col_spec <- cols_expanded[[j]]
      
      # Compute value directly (no matrix intermediary)
      value <- statistic_obj$processor(
        base_array = base_array,
        row_m = row_array,
        row_u = row_u,
        col_m = col_array,
        col_u = col_u,
        values = values_array
      )
      
      # Compute base directly (no matrix intermediary)
      base <- statistic_obj$base_calculator(
        base_array = base_array,
        row_m = row_array,
        row_u = row_u,
        col_m = col_array,
        col_u = col_u,
        values_array = values_array
      )
      
      # Build specification using helper
      specification <- build_cell_specification(
        row_spec, col_spec,
        row_array, col_array,
        base_filter_spec, base_filter_expr,
        data, dpdict,
        statistic_id = statistic_obj$id,
        values_var = values_var
      )
      
      specification$row_exposure <- row_exposures[i]
      specification$col_exposure <- col_exposures[j]
      
      # Store computation metadata
      computation <- list(
        statistic = statistic_obj$id,
        array_refs = list(
          base = 1,  # Index into arrays$base_array
          row = i,
          col = j
        )
      )
      
      # Add cell to store immediately
      add_cell(
        store,
        value = value,
        base = base,
        specification = specification,
        computation = computation,
        derivation = NULL
      )
    }
  }
  
  # Initialize summary tracking variables
  summary_row_array <- NULL
  summary_row_spec <- NULL
  summary_col_array <- NULL
  summary_col_spec <- NULL
  summary_row_u_array <- NULL
  summary_col_u_array <- NULL
  
  ## Compute summary column cells (always, if statistic supports it) -----------
  if (length(col_arrays) > 1 && !is.null(statistic_obj$summary_col)) {
    
    if (is.null(statistic_obj$summary_col_calculator)) {
      stop("Statistic '", statistic_obj$id, "' has summary_col='", 
           statistic_obj$summary_col, "' but no summary_col_calculator defined")
    }
    
    col_tab_arrays <- Map(function(m, u) {
      structure(list(m = m, u = u), class = "tab_arrays")
    }, col_arrays, col_u_arrays)
    
    summary_col_arrays <- statistic_obj$summary_col_calculator(
      arrays = col_tab_arrays,
      base_array = base_array
    )
    
    summary_col_array <- summary_col_arrays$m
    summary_col_u_array <- summary_col_arrays$u
    
    summary_col_exposure <- compute_exposure(summary_col_array)
    
    # Extract labels from arrays being summarized
    col_labels <- sapply(col_arrays, function(arr) {
      meta <- attr(arr, "meta")
      if (!is.null(meta) && !is.null(meta$label)) meta$label else "unknown"
    })
    
    # Add metadata to summary array
    summary_col_array <- set_array_meta(
      summary_col_array,
      dsl = NULL,
      variables = NULL,
      tags = list(type = statistic_obj$summary_col, summary = TRUE, dimension = "col"),
      label = paste0(statistic_obj$summary_col, " (", 
                     paste(col_labels, collapse = " + "), ")")
    )
    
    # Create specification for summary column
    summary_col_spec <- list(
      type = "summary",
      label = statistic_obj$summary_col,
      expr = as.symbol(statistic_obj$summary_col)
    )
    
    # Compute cells: each data row × summary column
    for (i in seq_along(row_arrays)) {
      value <- statistic_obj$processor(
        base_array = base_array,
        row_m = row_arrays[[i]],
        row_u = row_u_arrays[[i]],
        col_m = summary_col_array,
        col_u = summary_col_u_array,
        values = values_array
      )
      
      base <- statistic_obj$base_calculator(
        base_array = base_array,
        row_m = row_arrays[[i]],
        row_u = row_u_arrays[[i]],
        col_m = summary_col_array,
        col_u = summary_col_u_array,
        values_array = values_array
      )
      
      specification <- build_cell_specification(
        rows_expanded[[i]], summary_col_spec,
        row_arrays[[i]], summary_col_array,
        base_filter_spec, base_filter_expr,
        data, dpdict,
        is_summary_row = FALSE,
        is_summary_col = TRUE,
        statistic_id = statistic_obj$id,
        values_var = values_var
      )
      
      specification$row_exposure <- row_exposures[i]
      specification$col_exposure <- summary_col_exposure
      
      computation <- list(
        statistic = statistic_obj$id,
        array_refs = list(base = 1, row = i, col = "summary")
      )
      
      add_cell(store, value, base, specification, computation, NULL)
    }
  }
  
  ## Compute summary row cells (always, if statistic supports it) --------------
  if (length(row_arrays) > 1 && !is.null(statistic_obj$summary_row)) {
    
    if (is.null(statistic_obj$summary_row_calculator)) {
      stop("Statistic '", statistic_obj$id, "' has summary_row='", 
           statistic_obj$summary_row, "' but no summary_row_calculator defined")
    }
    
    row_tab_arrays <- Map(function(m, u) {
      structure(list(m = m, u = u), class = "tab_arrays")
    }, row_arrays, row_u_arrays)
    
    summary_row_arrays <- statistic_obj$summary_row_calculator(
      arrays = row_tab_arrays,
      base_array = base_array
    )
    
    summary_row_array <- summary_row_arrays$m
    summary_row_u_array <- summary_row_arrays$u
    
    summary_row_exposure <- compute_exposure(summary_row_array)
    
    # Extract labels from arrays being summarized
    row_labels <- sapply(row_arrays, function(arr) {
      meta <- attr(arr, "meta")
      if (!is.null(meta) && !is.null(meta$label)) meta$label else "unknown"
    })
    
    # Add metadata to summary array
    summary_row_array <- set_array_meta(
      summary_row_array,
      dsl = NULL,
      variables = NULL,
      tags = list(type = statistic_obj$summary_row, summary = TRUE, dimension = "row"),
      label = paste0(statistic_obj$summary_row, " (", 
                     paste(row_labels, collapse = " + "), ")")
    )
    
    # Create specification for summary row
    summary_row_spec <- list(
      type = "summary",
      label = statistic_obj$summary_row,
      expr = as.symbol(statistic_obj$summary_row)
    )
    
    # Compute cells: summary row × each data column
    for (j in seq_along(col_arrays)) {
      value <- statistic_obj$processor(
        base_array = base_array,
        row_m = summary_row_array,
        row_u = summary_row_u_array,
        col_m = col_arrays[[j]],
        col_u = col_u_arrays[[j]],
        values = values_array
      )
      
      base <- statistic_obj$base_calculator(
        base_array = base_array,
        row_m = summary_row_array,
        row_u = summary_row_u_array,
        col_m = col_arrays[[j]],
        col_u = col_u_arrays[[j]],
        values_array = values_array
      )
      
      specification <- build_cell_specification(
        summary_row_spec, cols_expanded[[j]],
        summary_row_array, col_arrays[[j]],
        base_filter_spec, base_filter_expr,
        data, dpdict,
        is_summary_row = TRUE,
        is_summary_col = FALSE,
        statistic_id = statistic_obj$id,
        values_var = values_var
      )
      
      specification$row_exposure <- summary_row_exposure
      specification$col_exposure <- col_exposures[j]
      
      computation <- list(
        statistic = statistic_obj$id,
        array_refs = list(base = 1, row = "summary", col = j)
      )
      
      add_cell(store, value, base, specification, computation, NULL)
    }
    
    # Compute summary row × summary col intersection if both exist
    if (!is.null(summary_col_array)) {
      value <- statistic_obj$processor(
        base_array = base_array,
        row_m = summary_row_array,
        row_u = summary_row_u_array,
        col_m = summary_col_array,
        col_u = summary_col_u_array,
        values = values_array
      )
      
      base <- statistic_obj$base_calculator(
        base_array = base_array,
        row_m = summary_row_array,
        row_u = summary_row_u_array,
        col_m = summary_col_array,
        col_u = summary_col_u_array,
        values_array = values_array
      )
      
      specification <- build_cell_specification(
        summary_row_spec, summary_col_spec,
        summary_row_array, summary_col_array,
        base_filter_spec, base_filter_expr,
        data, dpdict,
        is_summary_row = TRUE,
        is_summary_col = TRUE,
        statistic_id = statistic_obj$id,
        values_var = values_var
      )
      
      specification$row_exposure <- summary_row_exposure
      specification$col_exposure <- summary_col_exposure
      
      computation <- list(
        statistic = statistic_obj$id,
        array_refs = list(base = 1, row = "summary", col = "summary")
      )
      
      add_cell(store, value, base, specification, computation, NULL)
    }
  }
  
  return(list(
    store = store,
    summary_row_array = summary_row_array,
    summary_row_spec = summary_row_spec,
    summary_col_array = summary_col_array,
    summary_col_spec = summary_col_spec,
    summary_row_u_array = summary_row_u_array,
    summary_col_u_array = summary_col_u_array
  ))
}

#' Build cell specification from row and column specs
#'
#' Helper function to construct the specification component of a cell object.
#' Extracts metadata from arrays and combines with parsed specifications to
#' create the hierarchical metadata structure needed for cell-based operations.
#'
#' @param row_spec Expanded row specification from parse stage
#' @param col_spec Expanded column specification from parse stage
#' @param row_array Row array with metadata attributes
#' @param col_array Column array with metadata attributes
#' @param base_filter_spec Parsed base filter specification (or NULL)
#' @param base_filter_expr Base filter expression for cell specification
#' @return List with cell specification structure
#' @keywords internal
build_cell_specification <- function(row_spec, col_spec, 
                                     row_array, col_array,
                                     base_filter_spec, base_filter_expr,
                                     data, dpdict,
                                     is_summary_row = FALSE,
                                     is_summary_col = FALSE,
                                     statistic_id = NULL,
                                     values_var = NULL) {
  
  # Extract metadata from arrays
  row_meta <- attr(row_array, "meta")
  col_meta <- attr(col_array, "meta")
  
  # Extract DSL from specs or arrays
  row_dsl <- row_spec$dsl %||% (if (!is.null(row_meta)) row_meta$dsl else NULL) %||% row_spec$expr
  col_dsl <- col_spec$dsl %||% (if (!is.null(col_meta)) col_meta$dsl else NULL) %||% col_spec$expr
  base_dsl <- if (!is.null(base_filter_spec)) {
    base_filter_spec$dsl %||% base_filter_expr
  } else {
    quote(TRUE)
  }
  
  # Normalize with data awareness
  norm_row <- normalize_dsl(row_dsl, data, dpdict)
  norm_col <- normalize_dsl(col_dsl, data, dpdict)
  norm_base <- normalize_dsl(base_dsl, data, dpdict)
  
  # Build complete specification
  result <- list(
    # Syntactic identity
    base_expr = base_filter_expr,
    row_expr = row_spec$expr,
    col_expr = col_spec$expr,
    
    # Semantic identity (normalized DSL)
    dsl = list(
      row = norm_row,
      col = norm_col,
      base = norm_base
    ),
    
    # Fallback metadata (for NULL DSL)
    meta = list(
      row_variables = if (!is.null(row_meta)) row_meta$variables else NULL,
      col_variables = if (!is.null(col_meta)) col_meta$variables else NULL,
      row_tags = if (!is.null(row_meta)) row_meta$tags else NULL,
      col_tags = if (!is.null(col_meta)) col_meta$tags else NULL
    ),
    
    # Computation context
    statistic_id = statistic_id,
    values_var = if (!is.null(values_var)) {
      if (is.character(values_var)) as.symbol(values_var) else values_var
    } else NULL,
    
    # Display
    is_summary_row = is_summary_row,
    is_summary_col = is_summary_col
  )
  
  return(result)
}

.is_orthogonal <- function(mat, by = c("row","col")) {
  by <- match.arg(by)
  if (by == "row")  all(rowSums(mat != 0 & !is.na(mat)) <= 1)
  else              all(colSums(mat != 0 & !is.na(mat)) <= 1)
}