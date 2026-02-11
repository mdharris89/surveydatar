# Derive Operations for Tab Results
#
# Implements additive post-compute transformations of tab results.
# Operations are composable and preserve tab_result structure.

# Registry for derive operations
.derive_registry <- new.env(parent = emptyenv())
.derive_registry$operations <- list()

#' Low-level constructor for derive operations
#' @param id Character identifier for the operation
#' @param processor Function that processes the operation
#' @param description Optional description
#' @keywords internal
new_derive_operation <- function(id, processor, description = NULL) {
  stopifnot(is.character(id), length(id) == 1, nzchar(id))
  stopifnot(is.function(processor))

  structure(
    list(
      id = id,
      processor = processor,
      description = description
    ),
    class = "derive_operation"
  )
}

#' Create and register a derive operation
#' @param id Character identifier for the operation
#' @param processor Function that processes the operation
#'   Signature: function(tab_result, ...)
#' @param description Optional description of what this operation does
#' @return The created operation object (invisibly)
#' @export
create_derive_operation <- function(id, processor, description = NULL) {
  if (id %in% names(.derive_registry$operations)) {
    warning("Overwriting existing derive operation '", id, "'")
  }

  obj <- new_derive_operation(id, processor, description)
  .derive_registry$operations[[id]] <- obj
  invisible(obj)
}

#' Get registered derive operation by ID
#' @param id Character identifier
#' @keywords internal
get_derive_operation <- function(id) {
  .derive_registry$operations[[id]]
}

#' List all registered derive operations
#' @export
list_derive_operations <- function() {
  names(.derive_registry$operations)
}

#' Main derive function - apply a derive operation to a tab_result
#'
#' @param tab_result A tab_result object from tab()
#' @param operation Either a derive operation specification object or a character ID
#' @param ... Additional arguments passed to the operation processor
#' @return Modified tab_result with derive operation applied
#' @export
derive <- function(tab_result, operation, ...) {
  if (!inherits(tab_result, "tab_result")) {
    stop("derive() requires a tab_result object")
  }

  # Get operation ID
  if (is.character(operation)) {
    op_id <- operation
    op_obj <- get_derive_operation(op_id)
    if (is.null(op_obj)) {
      stop("Unknown derive operation: '", op_id, "'. Available: ",
           paste(list_derive_operations(), collapse = ", "))
    }
  } else if (inherits(operation, "derive_spec")) {
    op_id <- operation$op_id
    op_obj <- get_derive_operation(op_id)
    if (is.null(op_obj)) {
      stop("Unknown derive operation: '", op_id, "'")
    }
    # Merge operation parameters with additional args
    op_params <- c(operation$params, list(...))
  } else {
    stop("operation must be a character ID or derive_spec object")
  }

  # Apply the operation
  if (inherits(operation, "derive_spec")) {
    result <- do.call(op_obj$processor, c(list(tab_result = tab_result), op_params))
  } else {
    result <- op_obj$processor(tab_result, ...)
  }

  # Record this operation in the derive_operations attribute
  derive_ops <- attr(result, "derive_operations")
  if (is.null(derive_ops)) {
    derive_ops <- list()
  }

  derive_ops[[length(derive_ops) + 1]] <- list(
    operation_id = op_id,
    timestamp = Sys.time(),
    parameters = if (exists("op_params")) op_params else list(...)
  )
  attr(result, "derive_operations") <- derive_ops

  return(result)
}

#' Create a derive operation specification
#'
#' Helper to create derive specification objects that can be passed to derive()
#' or specified in tab() parameters.
#'
#' @param op_id Operation identifier
#' @param ... Parameters for the operation
#' @keywords internal
new_derive_spec <- function(op_id, ...) {
  structure(
    list(
      op_id = op_id,
      params = list(...)
    ),
    class = "derive_spec"
  )
}

.resolve_measure_id <- function(tab_result, measure = NULL) {
  measures <- tab_result$measures %||% list()
  is_multi <- length(measures) > 1L

  if (is.null(measure)) {
    if (is_multi) {
      stop("This derive operation is ambiguous on multi-measure tables. Supply `measure = ...`.")
    }
    if (length(measures) == 1L) {
      return(measures[[1]]$id)
    }
    return(NULL)
  }

  if (!is.character(measure) || length(measure) != 1 || !nzchar(measure)) {
    stop("measure must be a single non-empty character id/label")
  }

  if (length(measures) == 0L) {
    stop("No measure metadata available on this tab_result")
  }

  ids <- vapply(measures, `[[`, character(1), "id")
  labels <- vapply(measures, function(m) m$label %||% "", character(1))

  if (measure %in% ids) {
    return(measure)
  }
  idx <- which(labels == measure)
  if (length(idx) == 1L) {
    return(ids[idx])
  }

  stop("Unknown measure selector '", measure, "'. Available ids: ", paste(ids, collapse = ", "))
}

.measure_scope_cols <- function(tab_result, measure_id = NULL) {
  map <- tab_result$layout$measure_col_map
  if (is.null(map)) {
    return(seq_len(ncol(tab_result$layout$grid)))
  }
  if (is.null(measure_id)) {
    return(which(!is.na(map) & map != "summary_placeholder"))
  }
  which(map == measure_id)
}

.measure_scope_rows <- function(tab_result, measure_id = NULL) {
  map <- tab_result$layout$measure_row_map
  if (is.null(map)) {
    return(seq_len(nrow(tab_result$layout$grid)))
  }
  if (is.null(measure_id)) {
    return(which(!is.na(map) & map != "summary_placeholder"))
  }
  which(map == measure_id)
}

# ============================================================================
# Built-in Derive Operations
# ============================================================================

#' Conditional sum by metadata field
#'
#' Groups rows or columns by a metadata field and sums cells within each group.
#' This operation is ADDITIVE - it adds new derived rows/columns while keeping
#' all originals. Use hide operations to show only derived cells.
#'
#' @param metadata_field Character string specifying which metadata field to group by
#'   Common values: "ival" (group by value label), "ivar" (group by variable name)
#' @param dimension Which dimension to collapse: "rows" or "cols"
#' @param label_fn Optional function to generate labels for collapsed groups
#'   Signature: function(group_key, group_metadata_list)
#' @return A derive specification object for use with derive()
#' @export
#' @examples
#' \dontrun{
#' # Create a grid table: compute all cells, derive collapsed, hide originals
#' result <- tab(data, response_match(get_variable_labels = A2, A2), A2) %>%
#'   derive(sum_if("ival", dimension = "cols")) %>%
#'   hide_cols_except("Daily|Weekly|Monthly|Never")  # Show only derived columns
#' }
sum_if <- function(metadata_field, dimension = c("rows", "cols"), label_fn = NULL, measure = NULL) {
  dimension <- match.arg(dimension)
  new_derive_spec("sum_if",
                  metadata_field = metadata_field,
                  dimension = dimension,
                  label_fn = label_fn,
                  measure = measure)
}

#' Process sum_if derive operation
#' @keywords internal
process_sum_if <- function(tab_result, metadata_field, dimension, label_fn = NULL, measure = NULL) {
  if (!inherits(tab_result, "tab_result")) {
    stop("tab_result must be a tab_result object")
  }
  
  # Cell-based path
  if (inherits(tab_result, "tab_cell_collection")) {
    store <- tab_result$cell_store
    layout <- tab_result$layout
    measure_id <- .resolve_measure_id(tab_result, measure)
    
    if (dimension == "cols") {
      # Group columns by metadata field
      col_groups <- list()
      scope_cols <- .measure_scope_cols(tab_result, measure_id)
      
      for (j in scope_cols) {
        # Extract from first cell in this column to get metadata
        first_cell_id <- layout$grid[1, j]
        if (is.na(first_cell_id)) next
        
        first_cell <- get_cell(store, first_cell_id)
        
        # Extract grouping key from metadata
        group_key <- extract_grouping_key(
          first_cell$specification$meta,
          field = metadata_field,
          var_list = "col_vars"
        )
        
        if (is.null(group_key)) {
          # Fall back to label if no metadata field match
          group_key <- paste0("group_", j)
        }
        
        if (is.null(col_groups[[group_key]])) {
          col_groups[[group_key]] <- integer()
        }
        col_groups[[group_key]] <- c(col_groups[[group_key]], j)
      }
      
      # For each group, create derived column
      for (group_key in names(col_groups)) {
        group_col_indices <- col_groups[[group_key]]
        new_col_cells <- character(nrow(layout$grid))
        
        # For each row, create derived cell
        for (i in seq_len(nrow(layout$grid))) {
          source_cell_ids <- layout$grid[i, group_col_indices]
          source_cell_ids <- source_cell_ids[!is.na(source_cell_ids)]
          
          if (length(source_cell_ids) == 0) {
            new_col_cells[i] <- NA
            next
          }
          
          # Sum values from source cells
          source_values <- sapply(source_cell_ids, function(id) {
            cell <- get_cell(store, id)
            if (!is.null(cell)) cell$value else NA
          })
          summed_value <- sum(source_values, na.rm = TRUE)
          
          # Create derived cell
          first_cell <- get_cell(store, source_cell_ids[1])
          
          derived_spec <- list(
            base_expr = first_cell$specification$base_expr,
            row_expr = first_cell$specification$row_expr,
            col_expr = call("sum_if", metadata_field, group_key),
            dsl = first_cell$specification$dsl,
            meta = first_cell$specification$meta,
            statistic_id = first_cell$specification$statistic_id,
            measure_id = first_cell$specification$measure_id,
            values_var = first_cell$specification$values_var,
            is_summary_row = FALSE,
            is_summary_col = FALSE
          )
          
          derived_cell_id <- add_cell(
            store,
            value = summed_value,
            base = NA,
            specification = derived_spec,
            computation = list(statistic = first_cell$computation$statistic, array_refs = list()),
            derivation = list(
              operation = "sum_if",
              source_cells = source_cell_ids
            )
          )
          
          new_col_cells[i] <- derived_cell_id
        }
        
        # Create layout_def for new derived column (NEW)
        sum_label <- paste("Sum", group_key)
        derived_col_expr <- call("sum_if", metadata_field, group_key)
        
        new_col_def <- new_layout_def(
          col_expr_matcher = exact_match_matcher(derived_col_expr),
          derivation_matcher = function(deriv) {
            !is.null(deriv) && deriv$operation == "sum_if"
          },
          label = sum_label,
          dimension = "col"
        )
        
        # Add to col_defs
        tab_result$layout$col_defs <- c(tab_result$layout$col_defs, list(new_col_def))
      }
      
      # Refresh layout (reallocate grid with new columns)
      return(refresh_layout(tab_result))
    }
    
    # Group rows by metadata field
    if (dimension == "rows") {
      row_groups <- list()
      scope_rows <- .measure_scope_rows(tab_result, measure_id)
      
      for (i in scope_rows) {
        # Extract from first cell in this row to get metadata
        first_cell_id <- layout$grid[i, 1]
        if (is.na(first_cell_id)) next
        
        first_cell <- get_cell(store, first_cell_id)
        
        # Extract grouping key from metadata
        group_key <- extract_grouping_key(
          first_cell$specification$meta,
          field = metadata_field,
          var_list = "row_vars"
        )
        
        if (is.null(group_key)) {
          # Fall back to label if no metadata field match
          group_key <- paste0("group_", i)
        }
        
        if (is.null(row_groups[[group_key]])) {
          row_groups[[group_key]] <- integer()
        }
        row_groups[[group_key]] <- c(row_groups[[group_key]], i)
      }
      
      # For each group, create derived row
      for (group_key in names(row_groups)) {
        group_row_indices <- row_groups[[group_key]]
        new_row_cells <- character(ncol(layout$grid))
        
        # For each column, create derived cell
        for (j in seq_len(ncol(layout$grid))) {
          source_cell_ids <- layout$grid[group_row_indices, j]
          source_cell_ids <- source_cell_ids[!is.na(source_cell_ids)]
          
          if (length(source_cell_ids) == 0) {
            new_row_cells[j] <- NA
            next
          }
          
          # Sum values from source cells
          source_values <- sapply(source_cell_ids, function(id) {
            cell <- get_cell(store, id)
            if (!is.null(cell)) cell$value else NA
          })
          summed_value <- sum(source_values, na.rm = TRUE)
          
          # Create derived cell
          first_cell <- get_cell(store, source_cell_ids[1])
          
          derived_spec <- list(
            base_expr = first_cell$specification$base_expr,
            row_expr = call("sum_if", metadata_field, group_key),
            col_expr = first_cell$specification$col_expr,
            dsl = first_cell$specification$dsl,
            meta = first_cell$specification$meta,
            statistic_id = first_cell$specification$statistic_id,
            measure_id = first_cell$specification$measure_id,
            values_var = first_cell$specification$values_var,
            is_summary_row = FALSE,
            is_summary_col = FALSE
          )
          
          derived_cell_id <- add_cell(
            store,
            value = summed_value,
            base = NA,
            specification = derived_spec,
            computation = list(statistic = first_cell$computation$statistic, array_refs = list()),
            derivation = list(
              operation = "sum_if",
              source_cells = source_cell_ids
            )
          )
          
          new_row_cells[j] <- derived_cell_id
        }
        
        # Create layout_def for new derived row (NEW)
        sum_label <- paste("Sum", group_key)
        derived_row_expr <- call("sum_if", metadata_field, group_key)
        
        new_row_def <- new_layout_def(
          row_expr_matcher = exact_match_matcher(derived_row_expr),
          derivation_matcher = function(deriv) {
            !is.null(deriv) && deriv$operation == "sum_if"
          },
          label = sum_label,
          dimension = "row"
        )
        
        # Add to row_defs
        tab_result$layout$row_defs <- c(tab_result$layout$row_defs, list(new_row_def))
      }
      
      # Refresh layout (reallocate grid with new rows)
      return(refresh_layout(tab_result))
    }
    
    warning("sum_if: dimension must be 'rows' or 'cols'")
    return(tab_result)
  }
  
  # Data.frame-based path (existing implementation)
  result_df <- tab_result
  arrays <- attr(tab_result, "arrays")

  if (dimension == "cols") {
    # Group columns by metadata field
    col_meta_list <- lapply(arrays$col_arrays, function(arr) attr(arr, "meta"))

    # Extract metadata field values for grouping
    group_keys <- vapply(col_meta_list, function(meta) {
      val <- meta[[metadata_field]]
      if (is.null(val)) return(NA_character_)
      paste(val, collapse = ";")
    }, character(1))

    # Find unique groups
    unique_groups <- unique(group_keys[!is.na(group_keys)])

    if (length(unique_groups) == 0) {
      warning("No groups found for metadata field '", metadata_field, "'")
      return(tab_result)
    }

    # Create new derived columns (ADDITIVE - don't replace originals)
    col_names <- names(result_df)[-1]  # Exclude row_label
    result <- tab_result  # Start with original

    for (group_key in unique_groups) {
      group_indices <- which(group_keys == group_key)

      # Sum values across columns in this group
      group_col_names <- col_names[group_indices]
      summed_values <- rowSums(result_df[, group_col_names, drop = FALSE], na.rm = TRUE)

      # Generate label for this group
      if (!is.null(label_fn)) {
        group_label <- label_fn(group_key, col_meta_list[group_indices])
      } else {
        # Default: use the first non-NULL value of the metadata field
        first_meta <- col_meta_list[[group_indices[1]]]
        # Map old field names to new DSL structure
        if (metadata_field == "ival_label") {
          group_label <- first_meta$tags$value_label %||% group_key
        } else if (metadata_field == "ival") {
          val <- first_meta$tags$value %||% first_meta$ival
          group_label <- if (!is.null(val)) paste(val, collapse = ", ") else group_key
        } else if (metadata_field == "label") {
          group_label <- first_meta$label %||% group_key
        } else {
          group_label <- first_meta[[metadata_field]] %||% group_key
        }
      }

      # Add as new column (keeping originals)
      result[[group_label]] <- summed_values
    }

    # Update layout to include new columns
    layout <- attr(result, "layout")
    n_original_cols <- length(col_names)
    n_new_cols <- length(unique_groups)
    # Original cols stay in their order, new cols added at end
    layout$col_order <- c(layout$col_order, seq(n_original_cols + 1, n_original_cols + n_new_cols))
    attr(result, "layout") <- layout

    # Update visibility to include new columns (all visible by default)
    visibility <- attr(result, "visibility")
    visibility$cols <- c(visibility$cols, rep(TRUE, n_new_cols))
    attr(result, "visibility") <- visibility

    return(result)

  } else {  # dimension == "rows"
    # Group rows by metadata field - ADDITIVE, adds new rows
    row_meta_list <- lapply(arrays$row_arrays, function(arr) attr(arr, "meta"))

    group_keys <- vapply(row_meta_list, function(meta) {
      val <- meta[[metadata_field]]
      if (is.null(val)) return(NA_character_)
      paste(val, collapse = ";")
    }, character(1))

    unique_groups <- unique(group_keys[!is.na(group_keys)])

    if (length(unique_groups) == 0) {
      warning("No groups found for metadata field '", metadata_field, "'")
      return(tab_result)
    }

    # Create new derived rows (keeping originals)
    result <- tab_result
    new_rows <- list()

    for (group_key in unique_groups) {
      group_indices <- which(group_keys == group_key)

      # Sum values across rows in this group
      group_data <- result_df[group_indices, -1, drop = FALSE]
      summed_values <- colSums(group_data, na.rm = TRUE)

      # Generate label
      if (!is.null(label_fn)) {
        group_label <- label_fn(group_key, row_meta_list[group_indices])
      } else {
        first_meta <- row_meta_list[[group_indices[1]]]
        # Map old field names to new DSL structure
        if (metadata_field == "ivar_label") {
          group_label <- first_meta$tags$var_label %||% group_key
        } else if (metadata_field == "ivar") {
          vars <- first_meta$variables %||% first_meta$ivar
          group_label <- if (!is.null(vars)) paste(vars, collapse = ", ") else group_key
        } else if (metadata_field == "label") {
          group_label <- first_meta$label %||% group_key
        } else {
          group_label <- first_meta[[metadata_field]] %||% group_key
        }
      }

      new_row <- data.frame(
        row_label = group_label,
        t(summed_values),
        stringsAsFactors = FALSE
      )
      names(new_row) <- names(result_df)

      new_rows[[length(new_rows) + 1]] <- new_row
    }

    # Add new rows to original (ADDITIVE)
    result <- rbind(result, do.call(rbind, new_rows))
    rownames(result) <- NULL
    
    # Preserve class
    class(result) <- class(tab_result)
    
    # Copy all attributes from original
    for (attr_name in names(attributes(tab_result))) {
      if (!attr_name %in% c("names", "row.names", "class", "dim", "layout", "visibility")) {
        attr(result, attr_name) <- attr(tab_result, attr_name)
      }
    }

    # Update layout to include new rows
    layout <- attr(tab_result, "layout")
    n_original_rows <- nrow(tab_result)
    n_new_rows <- length(new_rows)
    # Original rows stay in their order, new rows added at end
    layout$row_order <- c(layout$row_order, seq(n_original_rows + 1, n_original_rows + n_new_rows))
    attr(result, "layout") <- layout

    # Update visibility to include new rows (all visible by default)
    visibility <- attr(tab_result, "visibility")
    visibility$rows <- c(visibility$rows, rep(TRUE, n_new_rows))
    attr(result, "visibility") <- visibility

    return(result)
  }
}

#' Calculate difference between two columns
#'
#' Adds a new column showing the difference between two existing columns.
#'
#' @param from_col Column name or index to subtract from
#' @param to_col Column name or index to subtract
#' @param label Optional label for the new column
#' @return A derive specification object
#' @export
#' @examples
#' # Basic usage with test data
#' dat <- get_basic_test_dat()
#' result <- tab(dat, labelledordinal, binarycategoricalasfactor) %>%
#'   derive(delta_vs("Yes", "No"))
delta_vs <- function(from_col, to_col = NULL, label = NULL, measure = NULL) {
  new_derive_spec("delta_vs",
                  from_col = from_col,
                  to_col = to_col,
                  label = label,
                  measure = measure)
}

#' Process delta_vs derive operation
#' @keywords internal
process_delta_vs <- function(tab_result, from_col, to_col = NULL, label = NULL, measure = NULL) {
  if (!inherits(tab_result, "tab_result")) {
    stop("tab_result must be a tab_result object")
  }
  
  # Cell-based path
  if (inherits(tab_result, "tab_cell_collection")) {
    store <- tab_result$cell_store
    layout <- tab_result$layout
    measure_id <- .resolve_measure_id(tab_result, measure)
    scope_cols <- .measure_scope_cols(tab_result, measure_id)
    scope_rows <- .measure_scope_rows(tab_result, measure_id)
    
    # Resolve from_col to column index in grid
    from_idx <- if (is.numeric(from_col)) {
      from_col
    } else {
      match(from_col, layout$col_labels)
    }
    
    if (is.na(from_idx) || from_idx < 1 || from_idx > ncol(layout$grid)) {
      stop("from_col '", from_col, "' not found")
    }
    if (!(from_idx %in% scope_cols)) {
      stop("from_col '", from_col, "' is outside selected measure scope")
    }
    
    # For single to_col
    if (!is.null(to_col)) {
      to_idx <- if (is.numeric(to_col)) {
        to_col
      } else {
        match(to_col, layout$col_labels)
      }
      
      if (is.na(to_idx) || to_idx < 1 || to_idx > ncol(layout$grid)) {
        stop("to_col '", to_col, "' not found")
      }
      if (!(to_idx %in% scope_cols)) {
        stop("to_col '", to_col, "' is outside selected measure scope")
      }
      
      # Create derived column: to - from
      new_col_cells <- rep(NA_character_, nrow(layout$grid))
      
      for (i in scope_rows) {
        from_cell_id <- layout$grid[i, from_idx]
        to_cell_id <- layout$grid[i, to_idx]
        
        if (is.na(from_cell_id) || is.na(to_cell_id)) {
          new_col_cells[i] <- NA
          next
        }
        
        from_cell <- get_cell(store, from_cell_id)
        to_cell <- get_cell(store, to_cell_id)
        
        diff_value <- to_cell$value - from_cell$value
        
        # Create derived cell
        derived_spec <- list(
          base_expr = from_cell$specification$base_expr,
          row_expr = from_cell$specification$row_expr,
          col_expr = call("delta", from_col, to_col),
          dsl = from_cell$specification$dsl,
          meta = from_cell$specification$meta,
          statistic_id = from_cell$specification$statistic_id,
          measure_id = from_cell$specification$measure_id,
          values_var = from_cell$specification$values_var,
          is_summary_row = FALSE,
          is_summary_col = FALSE
        )
        
        derived_cell_id <- add_cell(
          store,
          value = diff_value,
          base = NA,
          specification = derived_spec,
          computation = list(statistic = from_cell$computation$statistic, array_refs = list()),
          derivation = list(
            operation = "delta_vs",
            source_cells = c(from_cell_id, to_cell_id)
          )
        )
        
        new_col_cells[i] <- derived_cell_id
      }
      
      # Create layout_def for new derived column (NEW)
      diff_label <- if (!is.null(label)) label else paste(to_col, "-", from_col)
      derived_col_expr <- call("delta", from_col, to_col)
      
      new_col_def <- new_layout_def(
        col_expr_matcher = exact_match_matcher(derived_col_expr),
        measure_id_matcher = exact_match_matcher(measure_id),
        derivation_matcher = function(deriv) {
          !is.null(deriv) && deriv$operation == "delta_vs"
        },
        label = diff_label,
        dimension = "col"
      )
      
      # Add to col_defs (NEW - instead of manually modifying grid)
      tab_result$layout$col_defs <- c(tab_result$layout$col_defs, list(new_col_def))
      
      # Refresh layout (reallocate grid with new column)
      return(refresh_layout(tab_result))
    }
    
    # If to_col is NULL, create diff for all cols vs from_col
    # Loop through all columns except from_col
    for (col_idx in scope_cols) {
      if (col_idx == from_idx) next  # Skip from_col itself
      
      col_def <- layout$col_defs[[col_idx]]
      to_col_label <- col_def$label
      
      # Create derived column: to - from
      new_col_cells <- rep(NA_character_, nrow(layout$grid))
      
      for (i in scope_rows) {
        from_cell_id <- layout$grid[i, from_idx]
        to_cell_id <- layout$grid[i, col_idx]
        
        if (is.na(from_cell_id) || is.na(to_cell_id)) {
          new_col_cells[i] <- NA
          next
        }
        
        from_cell <- get_cell(store, from_cell_id)
        to_cell <- get_cell(store, to_cell_id)
        
        diff_value <- to_cell$value - from_cell$value
        
        # Create derived cell
        derived_spec <- list(
          base_expr = from_cell$specification$base_expr,
          row_expr = from_cell$specification$row_expr,
          col_expr = call("delta", from_col, to_col_label),
          dsl = from_cell$specification$dsl,
          meta = from_cell$specification$meta,
          statistic_id = from_cell$specification$statistic_id,
          measure_id = from_cell$specification$measure_id,
          values_var = from_cell$specification$values_var,
          is_summary_row = FALSE,
          is_summary_col = FALSE
        )
        
        derived_cell_id <- add_cell(
          store,
          value = diff_value,
          base = NA,
          specification = derived_spec,
          computation = list(statistic = from_cell$computation$statistic, array_refs = list()),
          derivation = list(
            operation = "delta_vs",
            source_cells = c(from_cell_id, to_cell_id)
          )
        )
        
        new_col_cells[i] <- derived_cell_id
      }
      
      # Create layout_def for new derived column
      diff_label <- if (!is.null(label)) {
        paste(label, to_col_label, sep = " ")
      } else {
        paste(to_col_label, "-", from_col)
      }
      
      derived_col_expr <- call("delta", from_col, to_col_label)
      
      new_col_def <- new_layout_def(
        col_expr_matcher = exact_match_matcher(derived_col_expr),
        measure_id_matcher = exact_match_matcher(measure_id),
        derivation_matcher = function(deriv) {
          !is.null(deriv) && deriv$operation == "delta_vs"
        },
        label = diff_label,
        dimension = "col"
      )
      
      # Add to col_defs
      tab_result$layout$col_defs <- c(tab_result$layout$col_defs, list(new_col_def))
    }
    
    # Refresh layout (reallocate grid with all new columns)
    return(refresh_layout(tab_result))
  }
  
  # Data.frame-based path (existing implementation)
  col_names <- names(tab_result)[-1]  # Exclude row_label

  # Resolve column names/indices
  from_idx <- if (is.numeric(from_col)) {
    from_col
  } else {
    match(from_col, col_names)
  }

  if (is.na(from_idx) || from_idx < 1 || from_idx > length(col_names)) {
    stop("from_col '", from_col, "' not found")
  }

  # If to_col is NULL, compute difference for all other columns vs from_col
  if (is.null(to_col)) {
    # Add difference column for each column vs from_col
    result <- tab_result

    for (i in seq_along(col_names)) {
      if (i != from_idx) {
        to_col_name <- col_names[i]
        from_col_name <- col_names[from_idx]

        diff_values <- result[[to_col_name]] - result[[from_col_name]]

        diff_label <- if (!is.null(label)) {
          paste0(label, " (", to_col_name, ")")
        } else {
          paste0(to_col_name, " - ", from_col_name)
        }

        result[[diff_label]] <- diff_values

        # Update layout and visibility
        layout <- attr(result, "layout")
        layout$col_order <- c(layout$col_order, ncol(result) - 1)
        attr(result, "layout") <- layout

        visibility <- attr(result, "visibility")
        visibility$cols <- c(visibility$cols, TRUE)
        attr(result, "visibility") <- visibility
      }
    }

    return(result)
  } else {
    # Single difference column
    to_idx <- if (is.numeric(to_col)) {
      to_col
    } else {
      match(to_col, col_names)
    }

    if (is.na(to_idx) || to_idx < 1 || to_idx > length(col_names)) {
      stop("to_col '", to_col, "' not found")
    }

    from_col_name <- col_names[from_idx]
    to_col_name <- col_names[to_idx]

    diff_values <- tab_result[[to_col_name]] - tab_result[[from_col_name]]

    diff_label <- if (!is.null(label)) {
      label
    } else {
      paste0(to_col_name, " - ", from_col_name)
    }

    result <- tab_result
    result[[diff_label]] <- diff_values

    # Update layout and visibility
    layout <- attr(result, "layout")
    layout$col_order <- c(layout$col_order, ncol(result) - 1)
    attr(result, "layout") <- layout

    visibility <- attr(result, "visibility")
    visibility$cols <- c(visibility$cols, TRUE)
    attr(result, "visibility") <- visibility

    return(result)
  }
}

#' Calculate index relative to a base column
#'
#' Adds new columns showing index values (ratio × multiplier) relative to a base column.
#'
#' @param base_col Column name or index to use as base
#' @param multiplier Multiplier for the index (default 100)
#' @param label Optional label pattern for new columns
#' @param cols Optional specific columns to index (default: all except base)
#' @return A derive specification object
#' @export
#' @examples
#' # Basic usage with test data
#' dat <- get_basic_test_dat()
#' result <- tab(dat, labelledordinal, binarycategoricalasfactor, show_col_nets = TRUE) %>%
#'   derive(index_vs("Total"))
index_vs <- function(base_col, multiplier = 100, label = NULL, cols = NULL, measure = NULL) {
  new_derive_spec("index_vs",
                  base_col = base_col,
                  multiplier = multiplier,
                  label = label,
                  cols = cols,
                  measure = measure)
}

#' Process index_vs derive operation
#' @keywords internal
process_index_vs <- function(tab_result, base_col, multiplier = 100, label = NULL, cols = NULL, measure = NULL) {
  if (!inherits(tab_result, "tab_result")) {
    stop("tab_result must be a tab_result object")
  }
  
  # Cell-based path
  if (inherits(tab_result, "tab_cell_collection")) {
    store <- tab_result$cell_store
    layout <- tab_result$layout
    measure_id <- .resolve_measure_id(tab_result, measure)
    scope_cols <- .measure_scope_cols(tab_result, measure_id)
    scope_rows <- .measure_scope_rows(tab_result, measure_id)
    
    # Resolve base column
    base_col_idx <- if (is.numeric(base_col)) {
      base_col
    } else {
      match(base_col, layout$col_labels)
    }
    
    if (is.na(base_col_idx) || base_col_idx < 1 || base_col_idx > ncol(layout$grid)) {
      stop("base_col '", base_col, "' not found")
    }
    if (!(base_col_idx %in% scope_cols)) {
      stop("base_col '", base_col, "' is outside selected measure scope")
    }
    
    # Determine columns to index
    cols_to_index <- if (is.null(cols)) {
      setdiff(scope_cols, base_col_idx)
    } else {
      # Resolve cols to indices
      if (is.numeric(cols)) {
        cols
      } else {
        match(cols, layout$col_labels)
      }
    }
    
    # Remove NAs
    cols_to_index <- cols_to_index[!is.na(cols_to_index)]
    cols_to_index <- intersect(cols_to_index, scope_cols)
    
      # For each column to index, create new derived column
      for (col_idx in cols_to_index) {
        new_col_cells <- rep(NA_character_, nrow(layout$grid))
        
        for (i in scope_rows) {
        base_cell_id <- layout$grid[i, base_col_idx]
        col_cell_id <- layout$grid[i, col_idx]
        
        if (is.na(base_cell_id) || is.na(col_cell_id)) {
          new_col_cells[i] <- NA
          next
        }
        
        base_cell <- get_cell(store, base_cell_id)
        col_cell <- get_cell(store, col_cell_id)
        
        index_value <- (col_cell$value / base_cell$value) * multiplier
        if (is.infinite(index_value)) index_value <- NA
        
        # Create derived cell
        derived_spec <- list(
          base_expr = col_cell$specification$base_expr,
          row_expr = col_cell$specification$row_expr,
          col_expr = call("index_vs", base_col, layout$col_labels[col_idx]),
          dsl = col_cell$specification$dsl,
          meta = col_cell$specification$meta,
          statistic_id = col_cell$specification$statistic_id,
          measure_id = col_cell$specification$measure_id,
          values_var = col_cell$specification$values_var,
          is_summary_row = FALSE,
          is_summary_col = FALSE
        )
        
        derived_cell_id <- add_cell(
          store,
          value = index_value,
          base = NA,
          specification = derived_spec,
          computation = list(statistic = col_cell$computation$statistic, array_refs = list()),
          derivation = list(
            operation = "index_vs",
            source_cells = c(base_cell_id, col_cell_id),
            formula = quote((col_value / base_value) * multiplier),
            base_col = layout$col_labels[base_col_idx],
            multiplier = multiplier
          )
        )
        
        new_col_cells[i] <- derived_cell_id
      }
      
      # Create layout_def for new derived column (NEW)
      index_label <- if (!is.null(label)) {
        paste0(label, " (", layout$col_labels[col_idx], ")")
      } else {
        paste0("Index: ", layout$col_labels[col_idx], " vs ", layout$col_labels[base_col_idx])
      }
      
      derived_col_expr <- call("index_vs", base_col, layout$col_labels[col_idx])
      
      new_col_def <- new_layout_def(
        col_expr_matcher = exact_match_matcher(derived_col_expr),
        measure_id_matcher = exact_match_matcher(measure_id),
        derivation_matcher = function(deriv) {
          !is.null(deriv) && deriv$operation == "index_vs"
        },
        label = index_label,
        dimension = "col"
      )
      
      # Add to col_defs
      tab_result$layout$col_defs <- c(tab_result$layout$col_defs, list(new_col_def))
    }
    
    # Refresh layout (reallocate grid with new columns)
    return(refresh_layout(tab_result))
  }

  col_names <- names(tab_result)[-1]

  # Resolve base column
  base_idx <- if (is.numeric(base_col)) {
    base_col
  } else {
    match(base_col, col_names)
  }

  if (is.na(base_idx) || base_idx < 1 || base_idx > length(col_names)) {
    stop("base_col '", base_col, "' not found")
  }

  base_col_name <- col_names[base_idx]
  base_values <- tab_result[[base_col_name]]

  # Determine which columns to index
  if (is.null(cols)) {
    cols_to_index <- setdiff(seq_along(col_names), base_idx)
  } else {
    cols_to_index <- if (is.numeric(cols)) {
      cols
    } else {
      match(cols, col_names)
    }
    cols_to_index <- cols_to_index[!is.na(cols_to_index)]
  }

  result <- tab_result

  for (idx in cols_to_index) {
    col_name <- col_names[idx]
    col_values <- result[[col_name]]

    # Calculate index: (col / base) * multiplier
    index_values <- (col_values / base_values) * multiplier
    index_values[is.infinite(index_values)] <- NA

    index_label <- if (!is.null(label)) {
      paste0(label, " (", col_name, ")")
    } else {
      paste0("Index: ", col_name, " vs ", base_col_name)
    }

    result[[index_label]] <- index_values

    # Update layout and visibility
    layout <- attr(result, "layout")
    layout$col_order <- c(layout$col_order, ncol(result) - 1)
    attr(result, "layout") <- layout

    visibility <- attr(result, "visibility")
    visibility$cols <- c(visibility$cols, TRUE)
    attr(result, "visibility") <- visibility
  }

  return(result)
}

#' Calculate share of sum
#'
#' Adds columns or rows showing each cell as a percentage of the sum.
#'
#' @param by Direction to calculate share: "row" (each cell / row sum) or
#'   "col" (each cell / column sum)
#' @param label Optional label for the operation
#' @return A derive specification object
#' @export
#' @examples
#' # Basic usage with test data
#' dat <- get_basic_test_dat()
#' result <- tab(dat, labelledordinal, binarycategoricalasfactor) %>%
#'   derive(share_of_sum(by = "row"))
share_of_sum <- function(by = c("row", "col"), label = NULL, measure = NULL) {
  by <- match.arg(by)
  new_derive_spec("share_of_sum",
                  by = by,
                  label = label,
                  measure = measure)
}

#' Process share_of_sum derive operation
#' @keywords internal
process_share_of_sum <- function(tab_result, by, label = NULL, measure = NULL) {
  if (!inherits(tab_result, "tab_result")) {
    stop("tab_result must be a tab_result object")
  }
  
  # Cell-based path
  if (inherits(tab_result, "tab_cell_collection")) {
    store <- tab_result$cell_store
    layout <- tab_result$layout
    measure_id <- .resolve_measure_id(tab_result, measure)
    scope_cols <- .measure_scope_cols(tab_result, measure_id)
    scope_rows <- .measure_scope_rows(tab_result, measure_id)

    if (by == "row") {
      for (j in scope_cols) {
        derived_col_expr <- call("share_of_sum", "row", layout$col_labels[j])
        share_label_suffix <- if (!is.null(label)) label else "% of Row"
        col_label <- paste0(layout$col_labels[j], " ", share_label_suffix)

        for (i in scope_rows) {
          row_cell_ids <- layout$grid[i, scope_cols]
          row_cell_ids <- row_cell_ids[!is.na(row_cell_ids)]
          cell_id <- layout$grid[i, j]
          if (length(row_cell_ids) == 0 || is.na(cell_id)) next

          row_values <- sapply(row_cell_ids, function(id) {
            cell <- get_cell(store, id)
            if (!is.null(cell)) cell$value else NA
          })
          row_total <- sum(row_values, na.rm = TRUE)
          cell <- get_cell(store, cell_id)
          share_value <- if (row_total != 0) (cell$value / row_total) * 100 else NA

          derived_spec <- list(
            base_expr = cell$specification$base_expr,
            row_expr = cell$specification$row_expr,
            col_expr = derived_col_expr,
            dsl = cell$specification$dsl,
            meta = cell$specification$meta,
            statistic_id = cell$specification$statistic_id,
            measure_id = cell$specification$measure_id,
            values_var = cell$specification$values_var,
            is_summary_row = FALSE,
            is_summary_col = FALSE
          )

          add_cell(
            store,
            value = share_value,
            base = NA,
            specification = derived_spec,
            computation = list(statistic = cell$computation$statistic, array_refs = list()),
            derivation = list(
              operation = "share_of_sum",
              by = "row",
              source_cells = row_cell_ids
            )
          )
        }

        tab_result$layout$col_defs <- c(tab_result$layout$col_defs, list(
          new_layout_def(
            col_expr_matcher = exact_match_matcher(derived_col_expr),
            measure_id_matcher = exact_match_matcher(measure_id),
            derivation_matcher = function(deriv) !is.null(deriv) && deriv$operation == "share_of_sum",
            label = col_label,
            dimension = "col"
          )
        ))
      }
      return(refresh_layout(tab_result))
    }

    # by == "col"
    for (i in scope_rows) {
      derived_row_expr <- call("share_of_sum", "col", layout$row_labels[i])
      share_label <- if (!is.null(label)) paste0(layout$row_labels[i], " ", label) else paste0(layout$row_labels[i], " % of Col")

      for (j in scope_cols) {
        col_cell_ids <- layout$grid[scope_rows, j]
        col_cell_ids <- col_cell_ids[!is.na(col_cell_ids)]
        cell_id <- layout$grid[i, j]
        if (length(col_cell_ids) == 0 || is.na(cell_id)) next

        col_values <- sapply(col_cell_ids, function(id) {
          cell <- get_cell(store, id)
          if (!is.null(cell)) cell$value else NA
        })
        col_total <- sum(col_values, na.rm = TRUE)
        cell <- get_cell(store, cell_id)
        share_value <- if (col_total != 0) (cell$value / col_total) * 100 else NA

        derived_spec <- list(
          base_expr = cell$specification$base_expr,
          row_expr = derived_row_expr,
          col_expr = cell$specification$col_expr,
          dsl = cell$specification$dsl,
          meta = cell$specification$meta,
          statistic_id = cell$specification$statistic_id,
          measure_id = cell$specification$measure_id,
          values_var = cell$specification$values_var,
          is_summary_row = FALSE,
          is_summary_col = FALSE
        )

        add_cell(
          store,
          value = share_value,
          base = NA,
          specification = derived_spec,
          computation = list(statistic = cell$computation$statistic, array_refs = list()),
          derivation = list(
            operation = "share_of_sum",
            by = "col",
            source_cells = col_cell_ids
          )
        )
      }

      tab_result$layout$row_defs <- c(tab_result$layout$row_defs, list(
        new_layout_def(
          row_expr_matcher = exact_match_matcher(derived_row_expr),
          measure_id_matcher = exact_match_matcher(measure_id),
          derivation_matcher = function(deriv) !is.null(deriv) && deriv$operation == "share_of_sum",
          label = share_label,
          dimension = "row"
        )
      ))
    }

    return(refresh_layout(tab_result))
  }

  data_cols <- tab_result[, -1, drop = FALSE]

  if (by == "row") {
    # Each cell / row total
    row_totals <- rowSums(data_cols, na.rm = TRUE)

    share_cols <- lapply(names(data_cols), function(col_name) {
      data_cols[[col_name]] / row_totals * 100
    })

    share_label_suffix <- if (!is.null(label)) label else "% of Row"

    result <- tab_result
    for (i in seq_along(share_cols)) {
      col_label <- paste0(names(data_cols)[i], " ", share_label_suffix)
      result[[col_label]] <- share_cols[[i]]

      # Update layout and visibility
      layout <- attr(result, "layout")
      layout$col_order <- c(layout$col_order, ncol(result) - 1)
      attr(result, "layout") <- layout

      visibility <- attr(result, "visibility")
      visibility$cols <- c(visibility$cols, TRUE)
      attr(result, "visibility") <- visibility
    }

    return(result)

  } else {  # by == "col"
    # Each cell / column total
    col_totals <- colSums(data_cols, na.rm = TRUE)

    share_rows <- list()
    for (i in seq_len(nrow(tab_result))) {
      row_data <- as.numeric(data_cols[i, ])
      share_values <- (row_data / col_totals) * 100

      share_label <- if (!is.null(label)) {
        paste0(tab_result$row_label[i], " ", label)
      } else {
        paste0(tab_result$row_label[i], " % of Col")
      }

      new_row <- data.frame(
        row_label = share_label,
        t(share_values),
        stringsAsFactors = FALSE
      )
      names(new_row) <- names(tab_result)

      share_rows[[length(share_rows) + 1]] <- new_row
    }

    # Add new rows to result
    result <- rbind(tab_result, do.call(rbind, share_rows))
    rownames(result) <- NULL

    class(result) <- class(tab_result)

    # Copy attributes
    for (attr_name in names(attributes(tab_result))) {
      if (!attr_name %in% c("names", "row.names", "class", "layout", "visibility")) {
        attr(result, attr_name) <- attr(tab_result, attr_name)
      }
    }

    # Update layout for new row structure
    attr(result, "layout") <- list(
      row_order = seq_len(nrow(result)),
      col_order = attr(tab_result, "layout")$col_order,
      row_groups = list(),
      col_groups = attr(tab_result, "layout")$col_groups
    )

    # Update visibility
    attr(result, "visibility") <- list(
      rows = rep(TRUE, nrow(result)),
      cols = attr(tab_result, "visibility")$cols,
      cells = NULL
    )

    return(result)
  }
}

#' Register built-in derive operations
#'
#' Called automatically on package load, but can be called explicitly if needed.
#' 
#' @export
#' @keywords internal
register_builtin_derive_operations <- function() {
  if (!"sum_if" %in% names(.derive_registry$operations)) {
    create_derive_operation("sum_if", process_sum_if,
                           description = "Conditionally sum cells by metadata field")
    create_derive_operation("delta_vs", process_delta_vs,
                           description = "Calculate difference between columns")
    create_derive_operation("index_vs", process_index_vs,
                           description = "Calculate index relative to base column")
    create_derive_operation("share_of_sum", process_share_of_sum,
                           description = "Calculate share of row or column sum")
  }
}


