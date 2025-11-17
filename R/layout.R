# Layout Operations for Tab Results
#
# Functions that control arrangement and ordering of rows and columns.
# Operations manipulate metadata; arrangement applied at display/export time.

#' Arrange rows using flexible ordering strategies
#'
#' Reorders rows using one of three strategies: by explicit label list, by cell
#' values in a column, or by a custom ordering function.
#'
#' @param tab_result A tab_result object
#' @param .by Either a character vector of row labels (for explicit ordering),
#'   or a column name/index to sort by cell values. If NULL, must provide .order_fn.
#' @param .order_fn Custom function that receives the first cell of each row
#'   and returns a sort key. Alternative to .by.
#' @param .sort Sort direction: "asc" or "desc" (default "asc")
#' @param .partial Logical, if TRUE, unlisted rows remain in original order 
#'   after specified rows (default TRUE)
#' @return Modified tab_result with reordered rows
#' @export
#' @examples
#' \dontrun{
#' # Explicit label order
#' result %>% arrange_rows(.by = c("Very satisfied", "Satisfied", "Neutral"))
#'
#' # Sort by values in a column
#' result %>% arrange_rows(.by = "Total", .sort = "desc")
#'
#' # Custom ordering function
#' result %>% arrange_rows(.order_fn = function(cell) cell$base, .sort = "desc")
#'
#' # Order by custom metadata
#' result %>% arrange_rows(
#'   .order_fn = function(cell) cell$specification$meta$display_order
#' )
#' }
arrange_rows <- function(tab_result, .by = NULL, .order_fn = NULL, 
                        .sort = c("asc", "desc"), .partial = TRUE) {
  if (!inherits(tab_result, "tab_result")) {
    stop("arrange_rows() requires a tab_result object")
  }
  
  .sort <- match.arg(.sort)
  
  if (is.null(.by) && is.null(.order_fn)) {
    stop("Must provide either .by or .order_fn")
  }
  
  # Cell-based path
  if (inherits(tab_result, "tab_cell_collection")) {
    row_labels <- sapply(tab_result$layout$row_defs, `[[`, "label")
    
    if (!is.null(.order_fn)) {
      # Custom ordering function
      grid <- tab_result$layout$grid
      
      # Apply function to first cell of each row
      sort_keys <- sapply(seq_len(nrow(grid)), function(i) {
        # Find first non-NA cell in this row
        row_cells <- grid[i, ]
        first_cell_id <- row_cells[!is.na(row_cells)][1]
        
        if (is.na(first_cell_id)) {
          return(NA)
        }
        
        cell <- get_cell(tab_result$cell_store, first_cell_id)
        .order_fn(cell)
      })
      
      new_order <- order(sort_keys, decreasing = (.sort == "desc"), na.last = TRUE)
      tab_result$layout$row_defs <- tab_result$layout$row_defs[new_order]
      
      return(refresh_layout(tab_result))
      
    } else if (is.character(.by) && length(.by) > 1) {
      # Explicit label ordering (existing behavior)
      order_indices <- match(.by, row_labels)
      unmatched <- .by[is.na(order_indices)]
      if (length(unmatched) > 0) {
        warning("Some row labels not found: ", paste(unmatched, collapse = ", "))
      }
      
      matched_indices <- order_indices[!is.na(order_indices)]
      
      if (.partial) {
        remaining_indices <- setdiff(seq_along(row_labels), matched_indices)
        new_order <- c(matched_indices, remaining_indices)
      } else {
        new_order <- matched_indices
      }
      
      tab_result$layout$row_defs <- tab_result$layout$row_defs[new_order]
      return(refresh_layout(tab_result))
      
    } else {
      # Sort by column values
      by_col_idx <- if (is.numeric(.by)) {
        .by
      } else {
        match(.by, tab_result$layout$col_labels)
      }
      
      if (is.na(by_col_idx) || by_col_idx < 1 || by_col_idx > ncol(tab_result$layout$grid)) {
        stop("Column '", .by, "' not found")
      }
      
      # Extract values from that column
      grid <- tab_result$layout$grid
      col_values <- sapply(seq_len(nrow(grid)), function(i) {
        cell_id <- grid[i, by_col_idx]
        if (!is.na(cell_id)) {
          cell <- get_cell(tab_result$cell_store, cell_id)
          cell$value
        } else {
          NA_real_
        }
      })
      
      new_order <- order(col_values, decreasing = (.sort == "desc"), na.last = TRUE)
      tab_result$layout$row_defs <- tab_result$layout$row_defs[new_order]
      
      return(refresh_layout(tab_result))
    }
  }

  # Data.frame-based path
  if (!is.null(.order_fn)) {
    stop("arrange_rows() with .order_fn requires cell-based tab_result")
  }
  
  if (is.character(.by) && length(.by) > 1) {
    # Explicit label ordering
    row_labels <- tab_result$row_label
    order_indices <- match(.by, row_labels)
    unmatched <- .by[is.na(order_indices)]
    
    if (length(unmatched) > 0) {
      warning("Some row labels not found: ", paste(unmatched, collapse = ", "))
    }
    
    matched_indices <- order_indices[!is.na(order_indices)]
    
    if (.partial) {
      remaining_indices <- setdiff(seq_along(row_labels), matched_indices)
      new_order <- c(matched_indices, remaining_indices)
    } else {
      new_order <- matched_indices
    }
    
    layout <- attr(tab_result, "layout")
    layout$row_order <- new_order
    attr(tab_result, "layout") <- layout
    
    return(tab_result)
  } else {
    # Sort by column values
    col_names <- names(tab_result)[-1]
    by_idx <- if (is.numeric(.by)) {
      .by
    } else {
      match(.by, col_names)
    }
    
    if (is.na(by_idx) || by_idx < 1 || by_idx > length(col_names)) {
      stop("Column '", .by, "' not found in tab_result")
    }
    
    by_col_name <- col_names[by_idx]
    sort_values <- tab_result[[by_col_name]]
    new_order <- order(sort_values, decreasing = (.sort == "desc"), na.last = TRUE)
    
    layout <- attr(tab_result, "layout")
    layout$row_order <- new_order
    attr(tab_result, "layout") <- layout
    
    return(tab_result)
  }
}

#' Arrange columns using flexible ordering strategies
#'
#' Reorders columns using one of three strategies: by explicit label list, by cell
#' values in a row, or by a custom ordering function. Symmetric to arrange_rows().
#'
#' @param tab_result A tab_result object
#' @param .by Either a character vector of column labels (for explicit ordering),
#'   or a row label/index to sort by cell values. If NULL, must provide .order_fn.
#' @param .order_fn Custom function that receives the first cell of each column
#'   and returns a sort key. Alternative to .by.
#' @param .sort Sort direction: "asc" or "desc" (default "asc")
#' @param .partial Logical, if TRUE, unlisted columns remain in original order 
#'   after specified columns (default TRUE)
#' @return Modified tab_result with reordered columns
#' @export
#' @examples
#' \dontrun{
#' # Explicit label order
#' result %>% arrange_cols(.by = c("Male", "Female", "Other"))
#'
#' # Sort by values in a row
#' result %>% arrange_cols(.by = "Total", .sort = "desc")
#'
#' # Custom ordering function
#' result %>% arrange_cols(.order_fn = function(cell) cell$base, .sort = "desc")
#' }
arrange_cols <- function(tab_result, .by = NULL, .order_fn = NULL, 
                        .sort = c("asc", "desc"), .partial = TRUE) {
  if (!inherits(tab_result, "tab_result")) {
    stop("arrange_cols() requires a tab_result object")
  }
  
  .sort <- match.arg(.sort)
  
  if (is.null(.by) && is.null(.order_fn)) {
    stop("Must provide either .by or .order_fn")
  }
  
  # Cell-based path
  if (inherits(tab_result, "tab_cell_collection")) {
    col_labels <- sapply(tab_result$layout$col_defs, `[[`, "label")
    
    if (!is.null(.order_fn)) {
      # Custom ordering function
      grid <- tab_result$layout$grid
      
      # Apply function to first cell of each column
      sort_keys <- sapply(seq_len(ncol(grid)), function(j) {
        # Find first non-NA cell in this column
        col_cells <- grid[, j]
        first_cell_id <- col_cells[!is.na(col_cells)][1]
        
        if (is.na(first_cell_id)) {
          return(NA)
        }
        
        cell <- get_cell(tab_result$cell_store, first_cell_id)
        .order_fn(cell)
      })
      
      new_order <- order(sort_keys, decreasing = (.sort == "desc"), na.last = TRUE)
      tab_result$layout$col_defs <- tab_result$layout$col_defs[new_order]
      
      return(refresh_layout(tab_result))
      
    } else if (is.character(.by) && length(.by) > 1) {
      # Explicit label ordering (existing behavior)
      order_indices <- match(.by, col_labels)
      unmatched <- .by[is.na(order_indices)]
      if (length(unmatched) > 0) {
        warning("Some column names not found: ", paste(unmatched, collapse = ", "))
      }
      
      matched_indices <- order_indices[!is.na(order_indices)]
      
      if (.partial) {
        remaining_indices <- setdiff(seq_along(col_labels), matched_indices)
        new_order <- c(matched_indices, remaining_indices)
      } else {
        new_order <- matched_indices
      }
      
      tab_result$layout$col_defs <- tab_result$layout$col_defs[new_order]
      return(refresh_layout(tab_result))
      
    } else {
      # Sort by row values
      by_row_idx <- if (is.numeric(.by)) {
        .by
      } else {
        match(.by, tab_result$layout$row_labels)
      }
      
      if (is.na(by_row_idx) || by_row_idx < 1 || by_row_idx > nrow(tab_result$layout$grid)) {
        stop("Row '", .by, "' not found")
      }
      
      # Extract values from that row
      grid <- tab_result$layout$grid
      row_values <- sapply(seq_len(ncol(grid)), function(j) {
        cell_id <- grid[by_row_idx, j]
        if (!is.na(cell_id)) {
          cell <- get_cell(tab_result$cell_store, cell_id)
          cell$value
        } else {
          NA_real_
        }
      })
      
      new_order <- order(row_values, decreasing = (.sort == "desc"), na.last = TRUE)
      tab_result$layout$col_defs <- tab_result$layout$col_defs[new_order]
      
      return(refresh_layout(tab_result))
    }
  }

  # Data.frame-based path
  if (!is.null(.order_fn)) {
    stop("arrange_cols() with .order_fn requires cell-based tab_result")
  }
  
  if (is.character(.by) && length(.by) > 1) {
    # Explicit label ordering
    col_names <- names(tab_result)[-1]
    order_indices <- match(.by, col_names)
    unmatched <- .by[is.na(order_indices)]
    
    if (length(unmatched) > 0) {
      warning("Some column names not found: ", paste(unmatched, collapse = ", "))
    }
    
    matched_indices <- order_indices[!is.na(order_indices)]
    
    if (.partial) {
      remaining_indices <- setdiff(seq_along(col_names), matched_indices)
      new_order <- c(matched_indices, remaining_indices)
    } else {
      new_order <- matched_indices
    }
    
    layout <- attr(tab_result, "layout")
    layout$col_order <- new_order
    attr(tab_result, "layout") <- layout
    
    return(tab_result)
  } else {
    # Sort by row values
    row_labels <- tab_result$row_label
    by_idx <- if (is.numeric(.by)) {
      .by
    } else {
      match(.by, row_labels)
    }
    
    if (is.na(by_idx) || by_idx < 1 || by_idx > length(row_labels)) {
      stop("Row '", .by, "' not found in tab_result")
    }
    
    sort_values <- as.numeric(tab_result[by_idx, -1])
    new_order <- order(sort_values, decreasing = (.sort == "desc"), na.last = TRUE)
    
    layout <- attr(tab_result, "layout")
    layout$col_order <- new_order
    attr(tab_result, "layout") <- layout
    
    return(tab_result)
  }
}

#' Move a specific row to a new position
#'
#' @param tab_result A tab_result object
#' @param .which Row identifier: either a label string or a matcher function
#'   that returns TRUE for the layout_def to move
#' @param .to Where to move it: "top", "bottom", "before", "after"
#' @param .reference For "before"/"after", the reference row label
#' @return Modified tab_result with updated row_order metadata
#' @export
#' @examples
#' \dontrun{
#' # Move by label
#' result %>% move_row(.which = "NET", .to = "top")
#'
#' # Move by matcher function
#' result %>% move_row(
#'   .which = function(def) def$label == "Base (n)",
#'   .to = "bottom"
#' )
#' }
move_row <- function(tab_result, .which = NULL, 
                     .to = c("top", "bottom", "before", "after"),
                     .reference = NULL) {
  .to <- match.arg(.to)

  if (!inherits(tab_result, "tab_result")) {
    stop("move_row() requires a tab_result object")
  }
  
  if (is.null(.which)) {
    stop("Must provide .which parameter")
  }
  
  # Cell-based path - reorder row_defs and refresh
  if (inherits(tab_result, "tab_cell_collection")) {
    row_labels <- sapply(tab_result$layout$row_defs, `[[`, "label")
    
    # Find row index based on .which
    if (is.character(.which)) {
      # Label string
      row_idx <- match(.which, row_labels)
      if (is.na(row_idx)) {
        stop("Row '", .which, "' not found")
      }
    } else if (is.function(.which)) {
      # Matcher function
      matches <- sapply(tab_result$layout$row_defs, .which)
      row_idx <- which(matches)[1]
      
      if (length(row_idx) == 0 || is.na(row_idx)) {
        stop("No row matched the provided function")
      }
    } else {
      stop(".which must be a character string or function")
    }
    
    # Extract row_def to move
    row_def <- tab_result$layout$row_defs[[row_idx]]
    
    # Remove from current position
    defs_without <- tab_result$layout$row_defs[-row_idx]
    
    # Insert at new position
    if (.to == "top") {
      tab_result$layout$row_defs <- c(list(row_def), defs_without)
    } else if (.to == "bottom") {
      tab_result$layout$row_defs <- c(defs_without, list(row_def))
    } else if (.to %in% c("before", "after")) {
      # Handle before/after positions
      if (is.null(.reference)) {
        stop(".reference must be specified for .to='", .to, "'")
      }
      
      ref_labels <- sapply(defs_without, `[[`, "label")
      ref_idx_in_new <- match(.reference, ref_labels)
      
      if (is.na(ref_idx_in_new)) {
        stop("Reference row '", .reference, "' not found")
      }
      
      if (.to == "before") {
        insert_pos <- ref_idx_in_new - 1
      } else {  # after
        insert_pos <- ref_idx_in_new
      }
      
      tab_result$layout$row_defs <- append(defs_without, list(row_def), after = insert_pos)
    }
    
    # Refresh layout (reallocate grid)
    return(refresh_layout(tab_result))
  }
  
  # Data.frame-based path
  if (!is.character(.which)) {
    stop("move_row() with function matcher requires cell-based tab_result")
  }
  
  row_labels <- tab_result$row_label
  row_idx <- match(.which, row_labels)

  if (is.na(row_idx)) {
    stop("Row '", .which, "' not found in tab_result")
  }

  current_order <- attr(tab_result, "layout")$row_order
  if (is.null(current_order)) {
    current_order <- seq_along(row_labels)
  }

  # Find where this row currently is in the order
  current_pos_in_order <- which(current_order == row_idx)

  # Remove from current position
  new_order <- current_order[-current_pos_in_order]

  # Insert at new position
  if (.to == "top") {
    new_order <- c(row_idx, new_order)
  } else if (.to == "bottom") {
    new_order <- c(new_order, row_idx)
  } else if (.to %in% c("before", "after")) {
    if (is.null(.reference)) {
      stop(".reference must be specified for .to='", .to, "'")
    }

    ref_idx <- match(.reference, row_labels)
    if (is.na(ref_idx)) {
      stop("Reference row '", .reference, "' not found")
    }

    ref_pos_in_order <- which(new_order == ref_idx)

    if (.to == "before") {
      new_order <- append(new_order, row_idx, after = ref_pos_in_order - 1)
    } else {  # after
      new_order <- append(new_order, row_idx, after = ref_pos_in_order)
    }
  }

  # Update layout metadata
  layout <- attr(tab_result, "layout")
  layout$row_order <- new_order
  attr(tab_result, "layout") <- layout

  return(tab_result)
}

#' Move a specific column to a new position
#'
#' @param tab_result A tab_result object
#' @param .which Column identifier: either a label string or a matcher function
#'   that returns TRUE for the layout_def to move
#' @param .to Where to move it: "left", "right", "before", "after"
#' @param .reference For "before"/"after", the reference column label
#' @return Modified tab_result with updated col_order metadata
#' @export
#' @examples
#' \dontrun{
#' # Move by label
#' result %>% move_col(.which = "Total", .to = "right")
#'
#' # Move by matcher function
#' result %>% move_col(
#'   .which = function(def) grepl("NET", def$label),
#'   .to = "left"
#' )
#' }
move_col <- function(tab_result, .which = NULL,
                     .to = c("left", "right", "before", "after"),
                     .reference = NULL) {
  .to <- match.arg(.to)

  if (!inherits(tab_result, "tab_result")) {
    stop("move_col() requires a tab_result object")
  }
  
  if (is.null(.which)) {
    stop("Must provide .which parameter")
  }
  
  # Cell-based path - reorder col_defs and refresh
  if (inherits(tab_result, "tab_cell_collection")) {
    col_labels <- sapply(tab_result$layout$col_defs, `[[`, "label")
    
    # Find column index based on .which
    if (is.character(.which)) {
      # Label string
      col_idx <- match(.which, col_labels)
      if (is.na(col_idx)) {
        stop("Column '", .which, "' not found")
      }
    } else if (is.function(.which)) {
      # Matcher function
      matches <- sapply(tab_result$layout$col_defs, .which)
      col_idx <- which(matches)[1]
      
      if (length(col_idx) == 0 || is.na(col_idx)) {
        stop("No column matched the provided function")
      }
    } else {
      stop(".which must be a character string or function")
    }
    
    # Extract col_def to move
    col_def <- tab_result$layout$col_defs[[col_idx]]
    
    # Remove from current position
    defs_without <- tab_result$layout$col_defs[-col_idx]
    
    # Insert at new position
    if (.to == "left") {
      tab_result$layout$col_defs <- c(list(col_def), defs_without)
    } else if (.to == "right") {
      tab_result$layout$col_defs <- c(defs_without, list(col_def))
    } else if (.to %in% c("before", "after")) {
      # Handle before/after positions
      if (is.null(.reference)) {
        stop(".reference must be specified for .to='", .to, "'")
      }
      
      ref_labels <- sapply(defs_without, `[[`, "label")
      ref_idx_in_new <- match(.reference, ref_labels)
      
      if (is.na(ref_idx_in_new)) {
        stop("Reference column '", .reference, "' not found")
      }
      
      if (.to == "before") {
        insert_pos <- ref_idx_in_new - 1
      } else {  # after
        insert_pos <- ref_idx_in_new
      }
      
      tab_result$layout$col_defs <- append(defs_without, list(col_def), after = insert_pos)
    }
    
    # Refresh layout (reallocate grid)
    return(refresh_layout(tab_result))
  }

  # Data.frame-based path
  if (!is.character(.which)) {
    stop("move_col() with function matcher requires cell-based tab_result")
  }
  
  col_names <- names(tab_result)[-1]  # Exclude row_label
  col_idx <- match(.which, col_names)

  if (is.na(col_idx)) {
    stop("Column '", .which, "' not found in tab_result")
  }

  current_order <- attr(tab_result, "layout")$col_order
  if (is.null(current_order)) {
    current_order <- seq_along(col_names)
  }

  current_pos_in_order <- which(current_order == col_idx)
  new_order <- current_order[-current_pos_in_order]

  if (.to == "left") {
    new_order <- c(col_idx, new_order)
  } else if (.to == "right") {
    new_order <- c(new_order, col_idx)
  } else if (.to %in% c("before", "after")) {
    if (is.null(.reference)) {
      stop(".reference must be specified for .to='", .to, "'")
    }

    ref_idx <- match(.reference, col_names)
    if (is.na(ref_idx)) {
      stop("Reference column '", .reference, "' not found")
    }

    ref_pos_in_order <- which(new_order == ref_idx)

    if (.to == "before") {
      new_order <- append(new_order, col_idx, after = ref_pos_in_order - 1)
    } else {  # after
      new_order <- append(new_order, col_idx, after = ref_pos_in_order)
    }
  }

  # Update layout metadata
  layout <- attr(tab_result, "layout")
  layout$col_order <- new_order
  attr(tab_result, "layout") <- layout

  return(tab_result)
}

#' Group rows for hierarchical display
#'
#' Creates a grouping structure for rows. Actual hierarchical rendering
#' depends on export/display function support. Pure metadata operation.
#'
#' @param tab_result A tab_result object
#' @param ... Criteria for identifying rows to group
#' @param .match Matching strategy: "label" (default), "index", "custom"
#' @param .group_label Label for the group
#' @param .collapse Logical, whether to collapse the group by default
#' @return Modified tab_result with updated row_groups metadata
#' @export
#' @examples
#' \dontrun{
#' # Group by labels
#' result %>% group_rows("Very satisfied", "Satisfied",
#'                       .group_label = "Positive")
#'
#' # Group by indices
#' result %>% group_rows(1, 2, 3, .match = "index", .group_label = "Top 3")
#'
#' # Group by custom function
#' result %>% group_rows(
#'   function(def) grepl("satisfied", def$label, ignore.case = TRUE),
#'   .match = "custom",
#'   .group_label = "Satisfaction responses"
#' )
#' }
group_rows <- function(tab_result, ..., 
                      .match = c("label", "index", "custom"),
                      .group_label = NULL,
                      .collapse = FALSE) {
  if (!inherits(tab_result, "tab_result")) {
    stop("group_rows() requires a tab_result object")
  }
  
  if (is.null(.group_label)) {
    stop(".group_label must be provided")
  }
  
  .match <- match.arg(.match)
  criteria <- list(...)
  
  if (length(criteria) == 0) {
    warning("No criteria provided to group_rows()")
    return(tab_result)
  }
  
  # Cell-based path
  if (inherits(tab_result, "tab_cell_collection")) {
    row_labels <- sapply(tab_result$layout$row_defs, `[[`, "label")
    
    if (.match == "label") {
      # Match by labels
      labels_to_group <- unlist(criteria)
      indices <- match(labels_to_group, row_labels)
      matched_indices <- indices[!is.na(indices)]
    } else if (.match == "index") {
      # Match by indices
      matched_indices <- unlist(criteria)
      # Validate indices
      matched_indices <- matched_indices[matched_indices >= 1 & matched_indices <= length(row_labels)]
    } else {  # custom
      # Match by function
      if (length(criteria) != 1 || !is.function(criteria[[1]])) {
        stop("For .match='custom', provide a single function")
      }
      
      matcher_fn <- criteria[[1]]
      matches <- sapply(tab_result$layout$row_defs, matcher_fn)
      matched_indices <- which(matches)
    }
    
    if (length(matched_indices) == 0) {
      warning("No rows matched for grouping")
      return(tab_result)
    }
    
    # Add to row_groups metadata
    if (is.null(tab_result$layout$row_groups)) {
      tab_result$layout$row_groups <- list()
    }
    
    new_group <- list(
      label = .group_label,
      members = matched_indices,
      collapse = .collapse
    )
    
    tab_result$layout$row_groups[[length(tab_result$layout$row_groups) + 1]] <- new_group
    
    return(tab_result)
  }

  # Data.frame-based path
  row_labels <- tab_result$row_label
  
  if (.match == "label") {
    labels_to_group <- unlist(criteria)
    indices <- match(labels_to_group, row_labels)
    unmatched <- labels_to_group[is.na(indices)]
    
    if (length(unmatched) > 0) {
      warning("Some row labels not found: ", paste(unmatched, collapse = ", "))
    }
    
    matched_indices <- indices[!is.na(indices)]
  } else if (.match == "index") {
    matched_indices <- unlist(criteria)
    matched_indices <- matched_indices[matched_indices >= 1 & matched_indices <= length(row_labels)]
  } else {
    stop("group_rows() with .match='custom' requires cell-based tab_result")
  }

  if (length(matched_indices) == 0) {
    warning("No rows matched for grouping")
    return(tab_result)
  }

  layout <- attr(tab_result, "layout")

  new_group <- list(
    label = .group_label,
    members = matched_indices,
    collapse = .collapse
  )

  layout$row_groups[[length(layout$row_groups) + 1]] <- new_group
  attr(tab_result, "layout") <- layout

  return(tab_result)
}

#' Group columns for hierarchical display
#'
#' Creates a grouping structure for columns. Actual hierarchical rendering
#' depends on export/display function support. Symmetric to group_rows().
#'
#' @param tab_result A tab_result object
#' @param ... Criteria for identifying columns to group
#' @param .match Matching strategy: "label" (default), "index", "custom"
#' @param .group_label Label for the group
#' @param .collapse Logical, whether to collapse the group by default
#' @return Modified tab_result with updated col_groups metadata
#' @export
#' @examples
#' \dontrun{
#' # Group by labels
#' result %>% group_cols("Male", "Female", .group_label = "Gender")
#'
#' # Group by indices
#' result %>% group_cols(1, 2, .match = "index", .group_label = "First two")
#' }
group_cols <- function(tab_result, ..., 
                      .match = c("label", "index", "custom"),
                      .group_label = NULL,
                      .collapse = FALSE) {
  if (!inherits(tab_result, "tab_result")) {
    stop("group_cols() requires a tab_result object")
  }
  
  if (is.null(.group_label)) {
    stop(".group_label must be provided")
  }
  
  .match <- match.arg(.match)
  criteria <- list(...)
  
  if (length(criteria) == 0) {
    warning("No criteria provided to group_cols()")
    return(tab_result)
  }
  
  # Cell-based path
  if (inherits(tab_result, "tab_cell_collection")) {
    col_labels <- sapply(tab_result$layout$col_defs, `[[`, "label")
    
    if (.match == "label") {
      labels_to_group <- unlist(criteria)
      indices <- match(labels_to_group, col_labels)
      matched_indices <- indices[!is.na(indices)]
    } else if (.match == "index") {
      matched_indices <- unlist(criteria)
      matched_indices <- matched_indices[matched_indices >= 1 & matched_indices <= length(col_labels)]
    } else {  # custom
      if (length(criteria) != 1 || !is.function(criteria[[1]])) {
        stop("For .match='custom', provide a single function")
      }
      
      matcher_fn <- criteria[[1]]
      matches <- sapply(tab_result$layout$col_defs, matcher_fn)
      matched_indices <- which(matches)
    }
    
    if (length(matched_indices) == 0) {
      warning("No columns matched for grouping")
      return(tab_result)
    }
    
    if (is.null(tab_result$layout$col_groups)) {
      tab_result$layout$col_groups <- list()
    }
    
    new_group <- list(
      label = .group_label,
      members = matched_indices,
      collapse = .collapse
    )
    
    tab_result$layout$col_groups[[length(tab_result$layout$col_groups) + 1]] <- new_group
    
    return(tab_result)
  }

  # Data.frame-based path
  col_names <- names(tab_result)[-1]
  
  if (.match == "label") {
    labels_to_group <- unlist(criteria)
    indices <- match(labels_to_group, col_names)
    unmatched <- labels_to_group[is.na(indices)]
    
    if (length(unmatched) > 0) {
      warning("Some column names not found: ", paste(unmatched, collapse = ", "))
    }
    
    matched_indices <- indices[!is.na(indices)]
  } else if (.match == "index") {
    matched_indices <- unlist(criteria)
    matched_indices <- matched_indices[matched_indices >= 1 & matched_indices <= length(col_names)]
  } else {
    stop("group_cols() with .match='custom' requires cell-based tab_result")
  }

  if (length(matched_indices) == 0) {
    warning("No columns matched for grouping")
    return(tab_result)
  }

  layout <- attr(tab_result, "layout")

  new_group <- list(
    label = .group_label,
    members = matched_indices,
    collapse = .collapse
  )

  layout$col_groups[[length(layout$col_groups) + 1]] <- new_group
  attr(tab_result, "layout") <- layout

  return(tab_result)
}


#' Get All Cells with Positions
#' @keywords internal
get_all_cells_with_positions <- function(tab_result) {
  if (!inherits(tab_result, "tab_cell_collection")) {
    stop("Semantic matching requires cell-based tab_result")
  }
  
  grid <- tab_result$layout$grid
  all_cell_ids <- as.vector(grid)
  all_cell_ids <- all_cell_ids[!is.na(all_cell_ids)]
  
  lapply(all_cell_ids, function(cell_id) {
    get_cell(tab_result$cell_store, cell_id)
  })
}


#' Detect whether keys represent variables or values
#'
#' Determines if a set of keys are variable names or value codes by checking
#' if they match variables in the data dictionary.
#'
#' @param keys Vector of keys (character or numeric)
#' @param dpdict Data dictionary
#' @return Character: "variable" or "value"
#' @keywords internal
detect_dimension_type <- function(keys, dpdict) {
  if (is.null(dpdict) || length(keys) == 0) {
    # Default to variable if we can't determine
    return("variable")
  }
  
  # Get all variable names from dpdict
  if (is.data.frame(dpdict) && "variable_names" %in% names(dpdict)) {
    all_vars <- dpdict$variable_names
  } else {
    # Fallback: try names() if dpdict is a list
    all_vars <- names(dpdict)
  }
  
  # Check if any keys match variable names
  if (is.character(keys)) {
    # Count how many keys match variable names
    n_var_matches <- sum(keys %in% all_vars)
    
    # If most keys are variables, it's a variable dimension
    if (n_var_matches >= length(keys) / 2) {
      return("variable")
    }
  }
  
  # Otherwise assume it's values
  return("value")
}

#' Pivot Table to Grid Format
#'
#' Reshapes table by extracting semantic dimensions from cell DSL expressions.
#' Default extractors work for grid questions (variable × value).
#' 
#' Summary rows and columns (NET, Base, etc.) are automatically removed during
#' pivoting, as they don't fit the grid structure. The resulting grid contains
#' only the regular cells that match the extraction pattern.
#'
#' @param tab_result A tab_result object
#' @param row_extractor Function to extract row dimension key from cell (default: primary variable)
#' @param col_extractor Function to extract column dimension key from cell (default: first value)
#' @param .row_labels Optional function to generate custom labels for row keys
#' @param .col_labels Optional function to generate custom labels for column keys
#' @return Modified tab_result with grid layout (summary rows/cols removed)
#' @export
#' @examples
#' \dontrun{
#' # Grid question: transform from long to grid
#' tab(data, A2_a, statistic = "row_pct") %>% pivot_to_grid()
#' 
#' # Custom extractors and labels
#' pivot_to_grid(result, 
#'               row_extractor = ~ dsl_get_variables(.)[1],
#'               col_extractor = ~ dsl_get_values(.)[1],
#'               .row_labels = function(keys) paste("Var", keys),
#'               .col_labels = function(keys) paste("Val", keys))
#' }
pivot_to_grid <- function(tab_result,
                          row_extractor = NULL,
                          col_extractor = NULL,
                          .row_labels = NULL,
                          .col_labels = NULL) {
  
  if (!inherits(tab_result, "tab_cell_collection")) {
    stop("pivot_to_grid() requires cell-based tab_result")
  }
  
  # Default extractors
  if (is.null(row_extractor)) {
    row_extractor <- function(cell) {
      dsl_get_variables(cell$specification$dsl$row)[1]
    }
  }
  
  if (is.null(col_extractor)) {
    col_extractor <- function(cell) {
      dsl_get_values(cell$specification$dsl$row)[1]
    }
  }
  
  # Convert formulas to functions
  if (inherits(row_extractor, "formula")) {
    row_extractor <- rlang::as_function(row_extractor)
  }
  if (inherits(col_extractor, "formula")) {
    col_extractor <- rlang::as_function(col_extractor)
  }
  
  # Extract dimension keys from all cells
  all_cells <- get_all_cells_with_positions(tab_result)
  
  # Filter out summary cells before extracting dimensions
  non_summary_cells <- Filter(function(cell) {
    !isTRUE(cell$specification$is_summary_row) && 
    !isTRUE(cell$specification$is_summary_col)
  }, all_cells)
  
  mappings <- lapply(seq_along(non_summary_cells), function(i) {
    cell <- non_summary_cells[[i]]
    tryCatch({
      list(
        cell_id = cell$cell_id,
        original_position = i,
        row_key = row_extractor(cell),
        col_key = col_extractor(cell)
      )
    }, error = function(e) {
      NULL
    })
  })
  
  # Remove NULL mappings
  mappings <- Filter(Negate(is.null), mappings)
  
  if (length(mappings) == 0) {
    stop("No cells matched the extraction pattern. ",
         "Cells may have complex expressions not suitable for grid reshaping. ",
         "Provide custom row_extractor and col_extractor functions.")
  }
  
  # Build dimension sets
  all_row_keys <- sapply(mappings, `[[`, "row_key")
  all_col_keys <- sapply(mappings, `[[`, "col_key")
  
  # Handle non-atomic keys
  unique_rows <- unique(all_row_keys)
  unique_cols <- unique(all_col_keys)
  
  # Sort if atomic
  if (is.atomic(unique_rows)) {
    unique_rows <- sort(unique_rows)
  }
  if (is.atomic(unique_cols)) {
    unique_cols <- sort(unique_cols)
  }
  
  # Check for collisions
  position_keys <- sapply(mappings, function(m) {
    paste(m$row_key, m$col_key, sep = "||")
  })
  
  position_counts <- table(position_keys)
  has_collisions <- any(position_counts > 1)
  
  if (has_collisions) {
    # Expand grid for collisions
    return(expand_grid_for_collisions(tab_result, mappings, unique_rows, unique_cols))
  }
  
  # Simple 1:1 mapping
  new_grid <- matrix(NA_character_,
                     nrow = length(unique_rows),
                     ncol = length(unique_cols))
  
  for (map in mappings) {
    # Find indices using match for safety with non-atomic keys
    if (is.atomic(unique_rows)) {
      row_idx <- which(unique_rows == map$row_key)
    } else {
      row_idx <- which(sapply(unique_rows, identical, map$row_key))
    }
    
    if (is.atomic(unique_cols)) {
      col_idx <- which(unique_cols == map$col_key)
    } else {
      col_idx <- which(sapply(unique_cols, identical, map$col_key))
    }
    
    if (length(row_idx) == 1 && length(col_idx) == 1) {
      new_grid[row_idx, col_idx] <- map$cell_id
    }
  }
  
  # Detect dimension types based on the keys
  row_dim_type <- detect_dimension_type(unique_rows, tab_result$dpdict)
  col_dim_type <- detect_dimension_type(unique_cols, tab_result$dpdict)
  
  # Generate labels - use custom functions if provided
  if (!is.null(.row_labels)) {
    row_labels <- .row_labels(unique_rows)
  } else {
    # Default label generation
    row_ref_var <- if (row_dim_type == "value" && col_dim_type == "variable") unique_cols[1] else NULL
    row_labels <- generate_dimension_labels(
      unique_rows, 
      row_dim_type, 
      tab_result$dpdict,
      reference_var = row_ref_var
    )
  }
  
  if (!is.null(.col_labels)) {
    col_labels <- .col_labels(unique_cols)
  } else {
    # Default label generation
    col_ref_var <- if (col_dim_type == "value" && row_dim_type == "variable") unique_rows[1] else NULL
    col_labels <- generate_dimension_labels(
      unique_cols, 
      col_dim_type, 
      tab_result$dpdict,
      reference_var = col_ref_var
    )
  }
  
  # Create layout_defs for the pivoted grid
  # Both dimensions are extracted from cells' row_dsl
  # Matchers need to use the correct DSL getter based on detected dimension type
  
  new_row_defs <- lapply(seq_along(unique_rows), function(i) {
    row_key <- unique_rows[i]
    row_label <- row_labels[i]
    
    # Matcher checks if this cell belongs to this row
    # Use appropriate getter based on dimension type
    new_layout_def(
      row_dsl_matcher = function(row_dsl) {
        if (is.null(row_dsl)) return(FALSE)
        if (row_dim_type == "variable") {
          keys <- dsl_get_variables(row_dsl)
        } else {
          keys <- dsl_get_values(row_dsl)
        }
        if (length(keys) == 0) return(FALSE)
        keys[1] == row_key
      },
      is_summary_row_matcher = negate_matcher(function(x) isTRUE(x)),
      label = row_label,
      dimension = "row"
    )
  })
  
  new_col_defs <- lapply(seq_along(unique_cols), function(i) {
    col_key <- unique_cols[i]
    col_label <- col_labels[i]
    
    # Matcher checks if this cell belongs to this column
    # Use appropriate getter based on dimension type
    new_layout_def(
      row_dsl_matcher = function(row_dsl) {
        if (is.null(row_dsl)) return(FALSE)
        if (col_dim_type == "variable") {
          keys <- dsl_get_variables(row_dsl)
        } else {
          keys <- dsl_get_values(row_dsl)
        }
        if (length(keys) == 0) return(FALSE)
        keys[1] == col_key
      },
      is_summary_col_matcher = negate_matcher(function(x) isTRUE(x)),
      label = col_label,
      dimension = "col"
    )
  })
  
  # Update layout structure
  tab_result$layout$grid <- new_grid
  tab_result$layout$row_defs <- new_row_defs
  tab_result$layout$col_defs <- new_col_defs
  tab_result$layout$row_labels <- row_labels
  tab_result$layout$col_labels <- col_labels
  tab_result$layout$filter_rules <- list()  # Clear filters for pivoted grid
  tab_result$layout$has_summary_row <- FALSE
  tab_result$layout$has_summary_col <- FALSE
  
  # NOTE: We DON'T call refresh_layout() here because the grid is already
  # correctly populated from the mapping logic above. We just need to set
  # the layout_defs to match the grid we built.
  
  tab_result
}

#' Expand Grid for Collision Handling
#' @keywords internal
expand_grid_for_collisions <- function(tab_result, mappings, row_keys, col_keys) {
  # Group by position
  position_groups <- split(mappings, sapply(mappings, function(m) {
    paste(m$row_key, m$col_key, sep = "||")
  }))
  
  # Build expanded rows
  row_expansions <- lapply(row_keys, function(rk) {
    positions_this_row <- position_groups[grepl(paste0("^", rk, "\\|\\|"), names(position_groups))]
    max_cells <- max(sapply(positions_this_row, length))
    
    lapply(seq_len(max_cells), function(i) {
      list(key = rk, sub_index = i)
    })
  })
  
  expanded_rows <- unlist(row_expansions, recursive = FALSE)
  
  # Build expanded grid
  new_grid <- matrix(NA_character_,
                     nrow = length(expanded_rows),
                     ncol = length(col_keys))
  
  # Place cells
  for (pos_key in names(position_groups)) {
    parts <- strsplit(pos_key, "\\|\\|")[[1]]
    row_key <- parts[1]
    col_key <- parts[2]
    
    cells_at_pos <- position_groups[[pos_key]]
    col_idx <- which(col_keys == col_key)
    
    for (i in seq_along(cells_at_pos)) {
      row_idx <- which(sapply(expanded_rows, function(r) {
        r$key == row_key && r$sub_index == i
      }))
      new_grid[row_idx, col_idx] <- cells_at_pos[[i]]$cell_id
    }
  }
  
  # Generate labels
  row_labels <- sapply(expanded_rows, function(r) {
    base_label <- generate_dimension_labels(r$key, "variable", tab_result$dpdict)
    max_sub <- max(sapply(expanded_rows, `[[`, "sub_index"))
    if (max_sub > 1) {
      paste0(base_label, " [", r$sub_index, "]")
    } else {
      base_label
    }
  })
  
  col_labels <- generate_dimension_labels(col_keys, "value", tab_result$dpdict,
                                          reference_var = row_keys[1])
  
  # Create layout_defs for expanded grid (NEW)
  new_row_defs <- lapply(seq_along(expanded_rows), function(i) {
    r <- expanded_rows[[i]]
    row_key <- r$key
    row_label <- row_labels[i]
    sub_idx <- r$sub_index
    
    new_layout_def(
      row_dsl_matcher = function(row_dsl) {
        if (is.null(row_dsl)) return(FALSE)
        vars <- dsl_get_variables(row_dsl)
        if (length(vars) == 0) return(FALSE)
        vars[1] == row_key
      },
      is_summary_row_matcher = negate_matcher(function(x) isTRUE(x)),
      label = row_label,
      dimension = "row"
    )
  })
  
  new_col_defs <- lapply(seq_along(col_keys), function(i) {
    col_key <- col_keys[i]
    col_label <- col_labels[i]
    
    new_layout_def(
      row_dsl_matcher = function(row_dsl) {
        if (is.null(row_dsl)) return(FALSE)
        vals <- dsl_get_values(row_dsl)
        if (length(vals) == 0) return(FALSE)
        vals[1] == col_key
      },
      is_summary_col_matcher = negate_matcher(function(x) isTRUE(x)),
      label = col_label,
      dimension = "col"
    )
  })
  
  # Update layout structure
  tab_result$layout$grid <- new_grid
  tab_result$layout$row_defs <- new_row_defs
  tab_result$layout$col_defs <- new_col_defs
  tab_result$layout$row_labels <- row_labels
  tab_result$layout$col_labels <- col_labels
  tab_result$layout$filter_rules <- list()  # Clear filters for pivoted grid
  tab_result$layout$has_summary_row <- FALSE
  tab_result$layout$has_summary_col <- FALSE
  
  # NOTE: We DON'T call refresh_layout() here because the grid is already
  # correctly populated from the mapping logic. We just create layout_defs
  # that match the grid we built.
  
  tab_result
}

#' Generate Dimension Labels
#' @keywords internal
generate_dimension_labels <- function(keys, dimension_type, dpdict, reference_var = NULL) {
  if (dimension_type == "variable") {
    # Keys are variable names
    sapply(keys, function(v) {
      if (!is.null(dpdict) && v %in% dpdict$variable_names) {
        label <- dpdict$variable_labels[dpdict$variable_names == v]
        if (!is.na(label) && nzchar(label)) return(label)
      }
      v
    })
  } else if (dimension_type == "value") {
    # Keys are values - look up labels
    if (is.null(reference_var) || is.null(dpdict)) {
      return(as.character(keys))
    }
    
    sapply(keys, function(val) {
      get_value_labels_for_codes(reference_var, val, dpdict)[1] %||% as.character(val)
    })
  } else {
    as.character(keys)
  }
}

#' Hide Operations for Tab Results
#'
#' Functions that control visibility of rows, columns, and cells.
#' Operations manipulate metadata; filtering applied at display/export time.

#' Hide rows using flexible matching criteria
#'
#' Hides rows based on matching criteria. Multiple matching strategies are
#' available through the .match parameter.
#'
#' @param tab_result A tab_result object
#' @param ... Matching criteria for rows to hide (interpretation depends on .match)
#' @param .match Matching strategy: "label" (default), "expr", "base", 
#'   "value", "custom"
#' @return Modified tab_result with updated visibility metadata
#' @export
#' @examples
#' \dontrun{
#' # Hide by label (default)
#' result %>% hide_rows("Base (n)", "NET")
#'
#' # Hide by expression
#' result %>% hide_rows(satisfaction == 99, .match = "expr")
#'
#' # Hide by base threshold
#' result %>% hide_rows(~ .x < 30, .match = "base")
#'
#' # Hide by custom function
#' result %>% hide_rows(
#'   function(cell) cell$base < 30 && cell$value == 0,
#'   .match = "custom"
#' )
#' }
hide_rows <- function(tab_result, ..., 
                     .match = c("label", "expr", "base", "value", "custom")) {
  if (!inherits(tab_result, "tab_result")) {
    stop("hide_rows() requires a tab_result object")
  }
  
  .match <- match.arg(.match)
  criteria <- list(...)
  
  if (length(criteria) == 0) {
    warning("No criteria provided to hide_rows()")
    return(tab_result)
  }
  
  # Cell-based path - physically remove row_defs
  if (inherits(tab_result, "tab_cell_collection")) {
    # For label-based matching, remove matching row_defs
    if (.match == "label") {
      # Convert criteria to character labels
      labels_to_hide <- unlist(lapply(criteria, as.character))
      
      # Find and remove matching row_defs
      original_count <- length(tab_result$layout$row_defs)
      tab_result$layout$row_defs <- Filter(function(row_def) {
        # Keep row_def if its label doesn't match any criterion
        !row_def$label %in% labels_to_hide
      }, tab_result$layout$row_defs)
      
      removed_count <- original_count - length(tab_result$layout$row_defs)
      
      # Check if we removed any summary rows
      if (removed_count > 0) {
        # Update has_summary_row if we removed the summary
        has_summary <- any(sapply(tab_result$layout$row_defs, function(d) {
          !is.null(d$is_summary_row_matcher)
        }))
        tab_result$layout$has_summary_row <- has_summary
      }
      
      # Refresh layout (reallocate grid)
      if (removed_count > 0) {
        return(refresh_layout(tab_result))
      }
      
      # No changes made
      if (removed_count == 0) {
        warning("No rows found matching the provided labels")
      }
      
      return(tab_result)
    } else if (.match == "custom") {
      # Custom matcher path
      if (length(criteria) != 1 || !is.function(criteria[[1]])) {
        stop(".match='custom' requires exactly one matcher function")
      }
      
      matcher <- criteria[[1]]
      store <- tab_result$cell_store
      layout <- tab_result$layout
      
      # Evaluate matcher on each row_def
      rows_to_hide <- character()
      for (row_idx in seq_along(layout$row_defs)) {
        row_def <- layout$row_defs[[row_idx]]
        
        # Extract base values for this row across columns
        row_bases <- numeric()
        for (col_idx in seq_along(layout$col_defs)) {
          cell_id <- layout$grid[row_idx, col_idx]
          if (!is.na(cell_id) && cell_id %in% names(store)) {
            cell <- store[[cell_id]]
            if (!is.null(cell$base) && !is.na(cell$base)) {
              row_bases <- c(row_bases, cell$base)
            }
          }
        }
        
        # Apply matcher to base values
        # If any base in the row matches, hide the row
        if (length(row_bases) > 0 && any(sapply(row_bases, matcher), na.rm = TRUE)) {
          rows_to_hide <- c(rows_to_hide, row_def$label)
        }
      }
      
      # Remove matching rows
      if (length(rows_to_hide) > 0) {
        tab_result$layout$row_defs <- Filter(function(rd) {
          !rd$label %in% rows_to_hide
        }, tab_result$layout$row_defs)
        
        return(refresh_layout(tab_result))
      }
      
      return(tab_result)
    } else {
      # Other match types not yet supported with physical grid modification
      stop("hide_rows() currently only supports .match='label' and .match='custom' for cell-based tab_results. ",
           "Other match types will be supported in a future update.")
    }
  }
  
  # Data.frame-based path
  if (.match != "label") {
    stop("hide_rows() with .match='", .match, "' requires cell-based tab_result")
  }
  
  # Convert criteria to character labels
  labels_to_hide <- unlist(criteria)
  
  row_labels <- tab_result$row_label
  indices_to_hide <- match(labels_to_hide, row_labels)
  unmatched <- labels_to_hide[is.na(indices_to_hide)]

  if (length(unmatched) > 0) {
    warning("Some row labels not found: ", paste(unmatched, collapse = ", "))
  }

  matched_indices <- indices_to_hide[!is.na(indices_to_hide)]

  if (length(matched_indices) == 0) {
    return(tab_result)
  }

  visibility <- attr(tab_result, "visibility")
  visibility$rows[matched_indices] <- FALSE
  attr(tab_result, "visibility") <- visibility

  return(tab_result)
}

#' Hide columns using flexible matching criteria
#'
#' Hides columns based on matching criteria. Symmetric to hide_rows().
#'
#' @param tab_result A tab_result object
#' @param ... Matching criteria for columns to hide (interpretation depends on .match)
#' @param .match Matching strategy: "label" (default), "expr", "base", 
#'   "value", "custom"
#' @return Modified tab_result with updated visibility metadata
#' @export
#' @examples
#' \dontrun{
#' # Hide by label (default)
#' result %>% hide_cols("Don't know")
#'
#' # Hide by expression
#' result %>% hide_cols(gender == 3, .match = "expr")
#' }
hide_cols <- function(tab_result, ..., 
                     .match = c("label", "expr", "base", "value", "custom")) {
  if (!inherits(tab_result, "tab_result")) {
    stop("hide_cols() requires a tab_result object")
  }
  
  .match <- match.arg(.match)
  criteria <- list(...)
  
  if (length(criteria) == 0) {
    warning("No criteria provided to hide_cols()")
    return(tab_result)
  }
  
  # Cell-based path - physically remove col_defs
  if (inherits(tab_result, "tab_cell_collection")) {
    # For label-based matching, remove matching col_defs
    if (.match == "label") {
      # Convert criteria to character labels
      labels_to_hide <- unlist(lapply(criteria, as.character))
      
      # Find and remove matching col_defs
      original_count <- length(tab_result$layout$col_defs)
      tab_result$layout$col_defs <- Filter(function(col_def) {
        # Keep col_def if its label doesn't match any criterion
        !col_def$label %in% labels_to_hide
      }, tab_result$layout$col_defs)
      
      removed_count <- original_count - length(tab_result$layout$col_defs)
      
      # Check if we removed any summary columns
      if (removed_count > 0) {
        # Update has_summary_col if we removed the summary
        has_summary <- any(sapply(tab_result$layout$col_defs, function(d) {
          !is.null(d$is_summary_col_matcher)
        }))
        tab_result$layout$has_summary_col <- has_summary
      }
      
      # Refresh layout (reallocate grid)
      if (removed_count > 0) {
        return(refresh_layout(tab_result))
      }
      
      # No changes made
      if (removed_count == 0) {
        warning("No columns found matching the provided labels")
      }
      
      return(tab_result)
    } else if (.match == "custom") {
      # Custom matcher path
      if (length(criteria) != 1 || !is.function(criteria[[1]])) {
        stop(".match='custom' requires exactly one matcher function")
      }
      
      matcher <- criteria[[1]]
      store <- tab_result$cell_store
      layout <- tab_result$layout
      
      # Evaluate matcher on each col_def
      cols_to_hide <- character()
      for (col_idx in seq_along(layout$col_defs)) {
        col_def <- layout$col_defs[[col_idx]]
        
        # Extract base values for this column across rows
        col_bases <- numeric()
        for (row_idx in seq_along(layout$row_defs)) {
          cell_id <- layout$grid[row_idx, col_idx]
          if (!is.na(cell_id) && cell_id %in% names(store)) {
            cell <- store[[cell_id]]
            if (!is.null(cell$base) && !is.na(cell$base)) {
              col_bases <- c(col_bases, cell$base)
            }
          }
        }
        
        # Apply matcher to base values
        # If any base in the column matches, hide the column
        if (length(col_bases) > 0 && any(sapply(col_bases, matcher), na.rm = TRUE)) {
          cols_to_hide <- c(cols_to_hide, col_def$label)
        }
      }
      
      # Remove matching columns
      if (length(cols_to_hide) > 0) {
        tab_result$layout$col_defs <- Filter(function(cd) {
          !cd$label %in% cols_to_hide
        }, tab_result$layout$col_defs)
        
        return(refresh_layout(tab_result))
      }
      
      return(tab_result)
    } else {
      # Other match types not yet supported with physical grid modification
      stop("hide_cols() currently only supports .match='label' and .match='custom' for cell-based tab_results. ",
           "Other match types will be supported in a future update.")
    }
  }

  # Data.frame-based path
  if (.match != "label") {
    stop("hide_cols() with .match='", .match, "' requires cell-based tab_result")
  }
  
  labels_to_hide <- unlist(criteria)
  col_names <- names(tab_result)[-1]
  indices_to_hide <- match(labels_to_hide, col_names)
  unmatched <- labels_to_hide[is.na(indices_to_hide)]

  if (length(unmatched) > 0) {
    warning("Some column names not found: ", paste(unmatched, collapse = ", "))
  }

  matched_indices <- indices_to_hide[!is.na(indices_to_hide)]

  if (length(matched_indices) == 0) {
    return(tab_result)
  }

  visibility <- attr(tab_result, "visibility")
  visibility$cols[matched_indices] <- FALSE
  attr(tab_result, "visibility") <- visibility

  return(tab_result)
}

#' Hide cells based on a condition function
#'
#' Hides cells based on a condition function that receives the full cell and
#' returns TRUE to hide. This provides complete flexibility to match on any
#' cell property.
#'
#' @param tab_result A tab_result object
#' @param .condition Function that takes a cell and returns TRUE to hide it
#' @return Modified tab_result with updated visibility
#' @export
#' @examples
#' \dontrun{
#' # Hide cells with low base AND zero value
#' result %>% hide_if(.condition = function(cell) {
#'   cell$base < 30 && cell$value == 0
#' })
#'
#' # Hide derived cells
#' result %>% hide_if(.condition = function(cell) {
#'   !is.null(cell$derivation)
#' })
#' }
hide_if <- function(tab_result, .condition) {
  if (!inherits(tab_result, "tab_result")) {
    stop("hide_if() requires a tab_result object")
  }
  
  if (!is.function(.condition)) {
    stop(".condition must be a function")
  }
  
  # Cell-based path
  if (inherits(tab_result, "tab_cell_collection")) {
    store <- tab_result$cell_store
    layout <- tab_result$layout
    
    # Evaluate condition for each cell
    cells_to_hide <- character()
    
    for (cell_id in names(store)) {
      cell <- store[[cell_id]]
      # Evaluate condition on full cell object
      result <- tryCatch(
        .condition(cell),
        error = function(e) {
          warning("Error evaluating condition for cell '", cell_id, "': ", e$message)
          FALSE
        }
      )
      
      if (isTRUE(result)) {
        cells_to_hide <- c(cells_to_hide, cell_id)
      }
    }
    
    # Mark cells as hidden in layout by replacing cell_ids with NA
    for (cell_id in cells_to_hide) {
      # Find in grid and mark as NA
      grid_positions <- which(layout$grid == cell_id, arr.ind = TRUE)
      if (nrow(grid_positions) > 0) {
        for (k in seq_len(nrow(grid_positions))) {
          layout$grid[grid_positions[k, 1], grid_positions[k, 2]] <- NA
        }
      }
    }
    
    tab_result$layout <- layout
    return(tab_result)
  }

  # Data.frame-based path (existing implementation)
  # Get base matrix
  base_matrix <- attr(tab_result, "base_matrix")
  if (is.null(base_matrix)) {
    warning("No base matrix available for hide_if()")
    return(tab_result)
  }

  # Create cell visibility matrix
  n_rows <- nrow(tab_result)
  n_cols <- ncol(tab_result) - 1  # Exclude row_label

  cell_visibility <- matrix(TRUE, nrow = n_rows, ncol = n_cols)

  # Evaluate condition for each cell
  for (i in seq_len(n_rows)) {
    for (j in seq_len(n_cols)) {
      # Create cell-like object with available properties
      cell <- list(
        value = tab_result[i, j + 1],  # +1 for row_label column
        base = base_matrix[i, j],
        row_label = tab_result$row_label[i],
        col_name = names(tab_result)[j + 1]
      )

      # Evaluate condition
      result <- tryCatch(
        .condition(cell),
        error = function(e) {
          warning("Error evaluating condition for cell [", i, ",", j, "]: ", e$message)
          FALSE
        }
      )

      if (isTRUE(result)) {
        cell_visibility[i, j] <- FALSE
      }
    }
  }

  # Update visibility metadata
  visibility <- attr(tab_result, "visibility")
  visibility$cells <- cell_visibility
  attr(tab_result, "visibility") <- visibility

  return(tab_result)
}

#' Show summary rows and/or columns
#'
#' Removes filter rules that hide summary rows/columns, making them visible again.
#'
#' @param tab_result A tab_result object
#' @param .rows Logical, show summary rows (default TRUE)
#' @param .cols Logical, show summary columns (default TRUE)
#' @return Modified tab_result with summary rows/columns visible
#' @export
#' @examples
#' \dontrun{
#' # Show summary rows
#' result %>% show_summary(.rows = TRUE, .cols = FALSE)
#'
#' # Show all summaries
#' result %>% show_summary()
#' }
show_summary <- function(tab_result, .rows = TRUE, .cols = TRUE) {
  if (!inherits(tab_result, "tab_result")) {
    stop("show_summary() requires a tab_result object")
  }
  
  # Cell-based path
  if (inherits(tab_result, "tab_cell_collection")) {
    needs_refresh <- FALSE
    stat <- tab_result$statistic
    
    # Add summary row_def if requested
    if (.rows && !is.null(stat$summary_row)) {
      # Check if summary row_def already exists
      has_summary_row <- any(sapply(tab_result$layout$row_defs, function(d) {
        !is.null(d$is_summary_row_matcher)
      }))
      
      if (!has_summary_row) {
        # Create new summary row_def
        summary_row_def <- new_layout_def(
          is_summary_row_matcher = function(x) isTRUE(x),
          label = stat$summary_row,
          dimension = "row"
        )
        
        # Append to end of row_defs (summaries always last)
        tab_result$layout$row_defs <- c(tab_result$layout$row_defs, list(summary_row_def))
        
        # Update metadata
        tab_result$layout$has_summary_row <- TRUE
        needs_refresh <- TRUE
      }
    }
    
    # Add summary col_def if requested
    if (.cols && !is.null(stat$summary_col)) {
      # Check if summary col_def already exists
      has_summary_col <- any(sapply(tab_result$layout$col_defs, function(d) {
        !is.null(d$is_summary_col_matcher)
      }))
      
      if (!has_summary_col) {
        # Create new summary col_def
        summary_col_def <- new_layout_def(
          is_summary_col_matcher = function(x) isTRUE(x),
          label = stat$summary_col,
          dimension = "col"
        )
        
        # Append to end of col_defs (summaries always last)
        tab_result$layout$col_defs <- c(tab_result$layout$col_defs, list(summary_col_def))
        
        # Update metadata
        tab_result$layout$has_summary_col <- TRUE
        needs_refresh <- TRUE
      }
    }
    
    # Refresh layout to reallocate grid with new dimensions
    if (needs_refresh) {
      return(refresh_layout(tab_result))
    }
    
    return(tab_result)
  }
  
  # Data.frame-based path - not applicable
  warning("show_summary() only works with cell-based tab_result")
  return(tab_result)
}

#' Hide columns that don't match a pattern (useful after derive operations)
#'
#' Convenience function to hide all columns except those matching a pattern.
#' Useful after derive operations to show only derived columns.
#'
#' @param tab_result A tab_result object
#' @param pattern Regex pattern - columns matching this will remain visible
#' @return Modified tab_result with non-matching columns hidden
#' @export
#' @examples
#' \dontrun{
#' # Show only derived summary columns
#' result <- tab(data, response_match(get_variable_labels = A2, A2), A2) %>%
#'   derive(sum_if("ival", dimension = "cols")) %>%
#'   hide_cols_except("Daily|Weekly|Monthly|Never")
#' }
hide_cols_except <- function(tab_result, pattern) {
  if (!inherits(tab_result, "tab_result")) {
    stop("hide_cols_except() requires a tab_result object")
  }

  col_names <- names(tab_result)[-1]  # Exclude row_label
  
  # Find columns that DON'T match the pattern
  to_hide <- col_names[!grepl(pattern, col_names)]
  
  if (length(to_hide) > 0) {
    return(hide_cols(tab_result, to_hide))
  }
  
  return(tab_result)
}

#' Hide rows that don't match a pattern
#'
#' Convenience function to hide all rows except those matching a pattern.
#'
#' @param tab_result A tab_result object
#' @param pattern Regex pattern - rows matching this will remain visible
#' @return Modified tab_result with non-matching rows hidden
#' @export
hide_rows_except <- function(tab_result, pattern) {
  if (!inherits(tab_result, "tab_result")) {
    stop("hide_rows_except() requires a tab_result object")
  }

  row_labels <- tab_result$row_label
  
  # Find rows that DON'T match the pattern
  to_hide <- row_labels[!grepl(pattern, row_labels)]
  
  if (length(to_hide) > 0) {
    return(hide_rows(tab_result, to_hide))
  }
  
  return(tab_result)
}

#' Layout Matcher Functions
#'
#' Matcher constructor functions for building flexible cell qualification rules.
#' These are exported for users to build complex custom matchers by composition.

#' Create an expression matcher
#'
#' Returns a matcher function that checks for identity with a target expression.
#' Useful for matching cells by their row, column, or base expressions.
#'
#' @param expr Expression to match against (typically a quoted expression)
#' @return Matcher function that returns TRUE when passed an identical expression
#' @export
#' @examples
#' \dontrun{
#' # Match cells with specific row expression
#' matcher <- expr_matcher(quote(gender == 1))
#' 
#' # Use in custom layout operations
#' select_rows(result, matcher, .match = "custom")
#' }
expr_matcher <- function(expr) {
  function(x) identical(x, expr)
}

#' Create a label matcher
#'
#' Returns a matcher function that checks if a cell's position label is in
#' a set of target labels. Resolves labels to expressions using layout_defs.
#'
#' @param labels Character vector of labels to match
#' @param layout_defs List of layout_def objects to resolve labels from
#' @param exclude Logical, if TRUE matches labels NOT in the set (default FALSE)
#' @return Matcher function
#' @export
#' @examples
#' \dontrun{
#' # Match specific row labels
#' matcher <- label_matcher(c("Male", "Female"), result$layout$row_defs)
#' 
#' # Exclude specific labels
#' matcher <- label_matcher(c("Base (n)", "NET"), result$layout$row_defs, 
#'                          exclude = TRUE)
#' }
label_matcher <- function(labels, layout_defs, exclude = FALSE) {
  # Build lookup table: label -> expressions that qualify
  label_to_exprs <- resolve_labels_to_expressions(labels, layout_defs)
  
  function(expr) {
    # Check if this expression matches any of the target labels
    matched <- any(sapply(label_to_exprs, function(target_expr) {
      identical(expr, target_expr)
    }))
    
    if (exclude) !matched else matched
  }
}

#' Create a base threshold matcher
#'
#' Returns a matcher function that checks if a cell's base meets a threshold.
#'
#' @param threshold Numeric threshold value
#' @param op Comparison operator as string: ">=", ">", "<=", "<", "==" (default ">=")
#' @return Matcher function
#' @export
#' @examples
#' \dontrun{
#' # Match cells with base >= 100
#' matcher <- base_matcher(100)
#' 
#' # Match cells with base < 30
#' matcher <- base_matcher(30, "<")
#' 
#' # Use in hide operations
#' hide_rows(result, matcher, .match = "custom")
#' }
base_matcher <- function(threshold, op = ">=") {
  function(x) {
    if (is.na(x) || is.null(x)) return(FALSE)
    do.call(op, list(x, threshold))
  }
}

#' Create a value threshold matcher
#'
#' Returns a matcher function that checks if a cell's value meets a threshold.
#'
#' @param threshold Numeric threshold value
#' @param op Comparison operator as string: ">=", ">", "<=", "<", "==" (default ">=")
#' @return Matcher function
#' @export
#' @examples
#' \dontrun{
#' # Match cells with value > 50
#' matcher <- value_matcher(50, ">")
#' 
#' # Match cells with value == 0
#' matcher <- value_matcher(0, "==")
#' }
value_matcher <- function(threshold, op = ">=") {
  function(x) {
    if (is.na(x) || is.null(x)) return(FALSE)
    do.call(op, list(x, threshold))
  }
}

#' Create a statistic matcher
#'
#' Returns a matcher function that checks if a cell's statistic is in a set.
#'
#' @param statistic_ids Character vector of statistic IDs to match
#' @return Matcher function
#' @export
#' @examples
#' \dontrun{
#' # Match only percentage statistics
#' matcher <- statistic_matcher(c("column_pct", "row_pct"))
#' 
#' # Match mean and median
#' matcher <- statistic_matcher(c("mean", "median"))
#' }
statistic_matcher <- function(statistic_ids) {
  function(x) x %in% statistic_ids
}

#' Combine matchers with AND logic
#'
#' Returns a matcher function that returns TRUE only if ALL component matchers
#' return TRUE.
#'
#' @param ... Matcher functions to combine
#' @return Combined matcher function
#' @export
#' @examples
#' \dontrun{
#' # Match cells with high base AND high value
#' matcher <- and_matcher(
#'   base_matcher(100, ">="),
#'   value_matcher(50, ">")
#' )
#' 
#' # Use in selection
#' select_rows(result, matcher, .match = "custom")
#' }
and_matcher <- function(...) {
  matchers <- list(...)
  function(x) {
    all(sapply(matchers, function(m) m(x)))
  }
}

#' Combine matchers with OR logic
#'
#' Returns a matcher function that returns TRUE if ANY component matcher
#' returns TRUE.
#'
#' @param ... Matcher functions to combine
#' @return Combined matcher function
#' @export
#' @examples
#' \dontrun{
#' # Match cells with either low base OR zero value
#' matcher <- or_matcher(
#'   base_matcher(30, "<"),
#'   value_matcher(0, "==")
#' )
#' }
or_matcher <- function(...) {
  matchers <- list(...)
  function(x) {
    any(sapply(matchers, function(m) m(x)))
  }
}

#' Negate a matcher
#'
#' Returns a matcher function that negates (inverts) another matcher.
#'
#' @param matcher Matcher function to negate
#' @return Negated matcher function
#' @export
#' @examples
#' \dontrun{
#' # Match cells that are NOT summary rows
#' matcher <- not_matcher(function(x) isTRUE(x))
#' 
#' # Match cells with base NOT below threshold
#' matcher <- not_matcher(base_matcher(30, "<"))
#' }
not_matcher <- function(matcher) {
  function(x) !matcher(x)
}

# ============================================================================
# Internal Helper Functions
# ============================================================================

#' Build matcher from user input
#'
#' Central dispatch function that converts user input to appropriate matcher
#' functions based on match type. This ensures consistency across all layout
#' operations.
#'
#' @param input User input (expression, label string, threshold, function, etc.)
#' @param match_type Character: "expr", "label", "base", "value", "statistic", "custom"
#' @param dimension Character: "row" or "col" (for determining which matcher socket)
#' @param context List with tab_result and other contextual information
#' @return List with matcher function and socket name
#' @keywords internal
build_matcher_from_input <- function(input, match_type = "expr", 
                                     dimension = c("row", "col"),
                                     context = list()) {
  dimension <- match.arg(dimension)
  match_type <- match.arg(match_type, 
                         c("expr", "label", "base", "value", "statistic", "custom"))
  
  # Determine which matcher socket to use based on dimension
  expr_socket <- if (dimension == "row") "row_expr_matcher" else "col_expr_matcher"
  
  matcher_result <- switch(match_type,
    "expr" = {
      # Input is an expression - create exact match matcher
      list(
        matcher = expr_matcher(input),
        socket = expr_socket
      )
    },
    
    "label" = {
      # Input is a label string - resolve to expression(s)
      layout_defs <- if (dimension == "row") {
        context$tab_result$layout$row_defs
      } else {
        context$tab_result$layout$col_defs
      }
      
      list(
        matcher = label_matcher(input, layout_defs),
        socket = expr_socket
      )
    },
    
    "base" = {
      # Input is a threshold or function
      if (is.function(input)) {
        list(matcher = input, socket = "base_matcher")
      } else if (is.numeric(input)) {
        list(matcher = base_matcher(input), socket = "base_matcher")
      } else {
        stop("For match_type='base', input must be numeric threshold or function")
      }
    },
    
    "value" = {
      # Input is a threshold or function
      if (is.function(input)) {
        list(matcher = input, socket = "value_matcher")
      } else if (is.numeric(input)) {
        list(matcher = value_matcher(input), socket = "value_matcher")
      } else {
        stop("For match_type='value', input must be numeric threshold or function")
      }
    },
    
    "statistic" = {
      # Input is statistic ID(s)
      if (is.character(input)) {
        list(matcher = statistic_matcher(input), socket = "statistic_matcher")
      } else {
        stop("For match_type='statistic', input must be character vector")
      }
    },
    
    "custom" = {
      # Input must be a function - user provides full matcher
      if (!is.function(input)) {
        stop("For match_type='custom', input must be a function")
      }
      
      # Custom matchers are applied to the full cell, not a specific component
      # We'll handle this specially in the calling code
      list(matcher = input, socket = "custom")
    }
  )
  
  matcher_result
}

#' Resolve labels to expressions
#'
#' Maps label strings to the expressions they represent by looking up in
#' layout_defs. Builds efficient lookup table.
#'
#' @param labels Character vector of labels
#' @param layout_defs List of layout_def objects
#' @return List of expressions corresponding to labels
#' @keywords internal
resolve_labels_to_expressions <- function(labels, layout_defs) {
  # Ensure labels is a character vector
  if (!is.character(labels)) {
    stop("labels must be a character vector")
  }
  
  # Ensure layout_defs is a list
  if (!is.list(layout_defs) || length(layout_defs) == 0) {
    return(list())
  }
  
  # Build lookup table: label -> first matching expression
  label_to_expr <- list()
  
  for (def in layout_defs) {
    if (!is.null(def$label) && def$label %in% labels) {
      # Extract the expression this def matches
      # Look for the active matcher and try to extract target expression
      expr <- extract_expression_from_def(def)
      if (!is.null(expr)) {
        label_to_expr[[def$label]] <- expr
      }
    }
  }
  
  # Warn about unresolved labels
  unresolved <- setdiff(labels, names(label_to_expr))
  if (length(unresolved) > 0) {
    warning("Could not resolve labels to expressions: ", 
            paste(unresolved, collapse = ", "))
  }
  
  label_to_expr
}

#' Extract expression from layout_def
#'
#' Attempts to extract the target expression from a layout_def by examining
#' its matcher functions. This is used for label resolution.
#'
#' @param layout_def A layout_def object
#' @return Expression or NULL if cannot be extracted
#' @keywords internal
extract_expression_from_def <- function(layout_def) {
  # Try to extract from row_expr_matcher or col_expr_matcher
  # This is a heuristic - we look for exact_match_matcher patterns
  
  # Check row_expr_matcher
  if (!is.null(layout_def$row_expr_matcher)) {
    # Try to extract the closure environment
    env <- environment(layout_def$row_expr_matcher)
    if (exists("target", envir = env)) {
      return(get("target", envir = env))
    }
    # For matchers created by exact_match_matcher in layout_defs.R
    # The pattern is: function(expr) identical(expr, spec$expr)
    # Try to extract spec$expr from the closure
    if (exists("spec", envir = env)) {
      spec <- get("spec", envir = env)
      if (!is.null(spec$expr)) return(spec$expr)
    }
  }
  
  # Check col_expr_matcher
  if (!is.null(layout_def$col_expr_matcher)) {
    env <- environment(layout_def$col_expr_matcher)
    if (exists("target", envir = env)) {
      return(get("target", envir = env))
    }
    if (exists("spec", envir = env)) {
      spec <- get("spec", envir = env)
      if (!is.null(spec$expr)) return(spec$expr)
    }
  }
  
  # For row_dsl_matcher and col_dsl_matcher (after pivot_to_grid)
  # We can't easily extract the expression, return NULL
  # Label matching after pivot will need a different approach
  
  NULL
}


#' Layout Selection and Definition Functions
#'
#' Functions for selecting existing layout positions or defining new ones.
#' These provide flexible ways to control which cells appear and how they
#' are organized in the grid.

#' Select rows using flexible matching criteria
#'
#' Selects and reorders existing row positions based on matching criteria.
#' Multiple matching strategies are available through the .match parameter.
#'
#' @param tab_result A tab_result object (cell-based)
#' @param ... Matching criteria (interpretation depends on .match parameter)
#' @param .match Matching strategy: "expr" (default), "label", "base", 
#'   "value", "statistic", "custom"
#' @return Modified tab_result with selected and reordered rows
#' @export
#' @examples
#' \dontrun{
#' # Match by expression (default)
#' result %>% select_rows(satisfaction == 5, satisfaction == 4)
#'
#' # Match by label
#' result %>% select_rows("Very satisfied", "Satisfied", .match = "label")
#'
#' # Match by base threshold
#' result %>% select_rows(~ .x >= 100, .match = "base")
#'
#' # Match by custom function
#' result %>% select_rows(
#'   function(cell) cell$base >= 100 && cell$value > 50,
#'   .match = "custom"
#' )
#' }
select_rows <- function(tab_result, ..., 
                       .match = c("expr", "label", "base", "value", 
                                 "statistic", "custom")) {
  if (!inherits(tab_result, "tab_cell_collection")) {
    stop("select_rows() requires a cell-based tab_result")
  }
  
  .match <- match.arg(.match)
  criteria <- list(...)
  
  if (length(criteria) == 0) {
    warning("No criteria provided to select_rows()")
    return(tab_result)
  }
  
  # Track which row_defs we want to keep (in order)
  selected_row_indices <- integer(0)
  
  # Process each criterion
  for (criterion in criteria) {
    # Find cells matching this criterion
    matching_cells <- find_matching_cells(
      tab_result, 
      criterion, 
      .match, 
      dimension = "row"
    )
    
    if (length(matching_cells) == 0) {
      warning("No cells matched criterion: ", deparse(substitute(criterion))[1])
      next
    }
    
    # Extract unique row expressions from matched cells
    matched_row_exprs <- unique(lapply(matching_cells, function(cell) {
      cell$specification$row_expr
    }))
    
    # Find which row_defs match these expressions
    for (row_expr in matched_row_exprs) {
      for (i in seq_along(tab_result$layout$row_defs)) {
        row_def <- tab_result$layout$row_defs[[i]]
        
        # Check if this row_def matches the expression
        if (row_def_matches_expression(row_def, row_expr)) {
          selected_row_indices <- c(selected_row_indices, i)
          break
        }
      }
    }
  }
  
  # Remove duplicates, preserve order
  selected_row_indices <- unique(selected_row_indices)
  
  if (length(selected_row_indices) == 0) {
    warning("No rows matched any criteria")
    return(tab_result)
  }
  
  # Reorder row_defs to show only selected rows
  tab_result$layout$row_defs <- tab_result$layout$row_defs[selected_row_indices]
  
  # Refresh layout (reallocate grid)
  refresh_layout(tab_result)
}

#' Select columns using flexible matching criteria
#'
#' Selects and reorders existing column positions based on matching criteria.
#' Symmetric to select_rows().
#'
#' @param tab_result A tab_result object (cell-based)
#' @param ... Matching criteria (interpretation depends on .match parameter)
#' @param .match Matching strategy: "expr" (default), "label", "base", 
#'   "value", "statistic", "custom"
#' @return Modified tab_result with selected and reordered columns
#' @export
#' @examples
#' \dontrun{
#' # Match by expression
#' result %>% select_cols(gender == 1, gender == 2)
#'
#' # Match by label
#' result %>% select_cols("Male", "Female", .match = "label")
#' }
select_cols <- function(tab_result, ..., 
                       .match = c("expr", "label", "base", "value", 
                                 "statistic", "custom")) {
  if (!inherits(tab_result, "tab_cell_collection")) {
    stop("select_cols() requires a cell-based tab_result")
  }
  
  .match <- match.arg(.match)
  criteria <- list(...)
  
  if (length(criteria) == 0) {
    warning("No criteria provided to select_cols()")
    return(tab_result)
  }
  
  selected_col_indices <- integer(0)
  
  for (criterion in criteria) {
    matching_cells <- find_matching_cells(
      tab_result, 
      criterion, 
      .match, 
      dimension = "col"
    )
    
    if (length(matching_cells) == 0) {
      warning("No cells matched criterion: ", deparse(substitute(criterion))[1])
      next
    }
    
    # Extract unique column expressions from matched cells
    matched_col_exprs <- unique(lapply(matching_cells, function(cell) {
      cell$specification$col_expr
    }))
    
    # Find which col_defs match these expressions
    for (col_expr in matched_col_exprs) {
      for (j in seq_along(tab_result$layout$col_defs)) {
        col_def <- tab_result$layout$col_defs[[j]]
        
        if (col_def_matches_expression(col_def, col_expr)) {
          selected_col_indices <- c(selected_col_indices, j)
          break
        }
      }
    }
  }
  
  selected_col_indices <- unique(selected_col_indices)
  
  if (length(selected_col_indices) == 0) {
    warning("No columns matched any criteria")
    return(tab_result)
  }
  
  tab_result$layout$col_defs <- tab_result$layout$col_defs[selected_col_indices]
  
  refresh_layout(tab_result)
}

#' Define new row positions with custom matching rules
#'
#' Creates entirely new row positions using matcher functions. Each criterion
#' defines one position. Multiple cells can match each position, enabling
#' aggregated views.
#'
#' @param tab_result A tab_result object (cell-based)
#' @param ... Matching criteria for new positions
#' @param .labels Character vector of labels for new positions (optional)
#' @param .match Matching strategy: "expr" (default) or "custom"
#' @return Modified tab_result with new row definitions
#' @export
#' @examples
#' \dontrun{
#' # Define single-cell positions
#' result %>% define_rows(
#'   satisfaction == 5,
#'   satisfaction == 4,
#'   .labels = c("Very satisfied", "Satisfied")
#' )
#'
#' # Define aggregated positions
#' result %>% define_rows(
#'   satisfaction >= 4,
#'   satisfaction <= 2,
#'   .labels = c("Top 2 box", "Bottom 2 box")
#' )
#'
#' # Define with custom matchers
#' result %>% define_rows(
#'   function(cell) cell$base >= 100,
#'   function(cell) cell$specification$meta$priority == "high",
#'   .labels = c("High base", "High priority"),
#'   .match = "custom"
#' )
#' }
define_rows <- function(tab_result, ..., .labels = NULL, 
                       .match = c("expr", "custom")) {
  if (!inherits(tab_result, "tab_cell_collection")) {
    stop("define_rows() requires a cell-based tab_result")
  }
  
  .match <- match.arg(.match)
  criteria <- list(...)
  
  if (length(criteria) == 0) {
    stop("define_rows() requires at least one criterion")
  }
  
  # Generate labels if not provided
  if (is.null(.labels)) {
    .labels <- sapply(seq_along(criteria), function(i) {
      paste0("Row ", i)
    })
  } else if (length(.labels) != length(criteria)) {
    stop("Number of labels must match number of criteria")
  }
  
  # Create new row_defs
  new_row_defs <- lapply(seq_along(criteria), function(i) {
    criterion <- criteria[[i]]
    label <- .labels[i]
    
    # Build matcher for this criterion
    if (.match == "expr") {
      # Criterion is an expression - match cells with this row expression
      new_layout_def(
        row_expr_matcher = expr_matcher(criterion),
        label = label,
        dimension = "row"
      )
    } else if (.match == "custom") {
      # Criterion is a custom function - create custom matcher
      if (!is.function(criterion)) {
        stop("For .match='custom', each criterion must be a function")
      }
      
      # The custom function receives the full cell
      # We need to wrap it to work with the layout_def system
      # Since custom matchers evaluate the full cell, we use a special approach
      create_custom_row_def(criterion, label)
    }
  })
  
  # Replace entire row_defs list
  tab_result$layout$row_defs <- new_row_defs
  
  # Refresh layout (build grid from scratch)
  refresh_layout(tab_result)
}

#' Define new column positions with custom matching rules
#'
#' Creates entirely new column positions using matcher functions.
#' Symmetric to define_rows().
#'
#' @param tab_result A tab_result object (cell-based)
#' @param ... Matching criteria for new positions
#' @param .labels Character vector of labels for new positions (optional)
#' @param .match Matching strategy: "expr" (default) or "custom"
#' @return Modified tab_result with new column definitions
#' @export
#' @examples
#' \dontrun{
#' # Define single-cell positions
#' result %>% define_cols(
#'   gender == 1,
#'   gender == 2,
#'   .labels = c("Male", "Female")
#' )
#'
#' # Define with custom matchers
#' result %>% define_cols(
#'   function(cell) cell$specification$meta$region == "North",
#'   function(cell) cell$specification$meta$region == "South",
#'   .labels = c("Northern markets", "Southern markets"),
#'   .match = "custom"
#' )
#' }
define_cols <- function(tab_result, ..., .labels = NULL, 
                       .match = c("expr", "custom")) {
  if (!inherits(tab_result, "tab_cell_collection")) {
    stop("define_cols() requires a cell-based tab_result")
  }
  
  .match <- match.arg(.match)
  criteria <- list(...)
  
  if (length(criteria) == 0) {
    stop("define_cols() requires at least one criterion")
  }
  
  if (is.null(.labels)) {
    .labels <- sapply(seq_along(criteria), function(i) {
      paste0("Col ", i)
    })
  } else if (length(.labels) != length(criteria)) {
    stop("Number of labels must match number of criteria")
  }
  
  new_col_defs <- lapply(seq_along(criteria), function(i) {
    criterion <- criteria[[i]]
    label <- .labels[i]
    
    if (.match == "expr") {
      new_layout_def(
        col_expr_matcher = expr_matcher(criterion),
        label = label,
        dimension = "col"
      )
    } else if (.match == "custom") {
      if (!is.function(criterion)) {
        stop("For .match='custom', each criterion must be a function")
      }
      
      create_custom_col_def(criterion, label)
    }
  })
  
  tab_result$layout$col_defs <- new_col_defs
  
  refresh_layout(tab_result)
}

# ============================================================================
# Internal Helper Functions
# ============================================================================

#' Find cells matching a criterion
#'
#' @param tab_result Tab result object
#' @param criterion Matching criterion (type depends on .match)
#' @param match_type Character: matching strategy
#' @param dimension Character: "row" or "col"
#' @return List of matching cell objects
#' @keywords internal
find_matching_cells <- function(tab_result, criterion, match_type, dimension) {
  grid <- tab_result$layout$grid
  all_cell_ids <- as.vector(grid)
  all_cell_ids <- all_cell_ids[!is.na(all_cell_ids)]
  
  if (length(all_cell_ids) == 0) {
    return(list())
  }
  
  all_cells <- lapply(all_cell_ids, function(cell_id) {
    get_cell(tab_result$cell_store, cell_id)
  })
  
  # Build a temporary layout_def to test matching
  context <- list(tab_result = tab_result)
  
  if (match_type == "custom") {
    # Custom function - apply directly to full cell
    matching <- Filter(function(cell) criterion(cell), all_cells)
  } else if (match_type == "label") {
    # Label matching - resolve labels to expressions
    layout_defs <- if (dimension == "row") {
      tab_result$layout$row_defs
    } else {
      tab_result$layout$col_defs
    }
    
    # criterion should be a label string
    if (!is.character(criterion)) {
      stop("For .match='label', criterion must be a character string")
    }
    
    # Ensure criterion is treated as a single label
    if (length(criterion) != 1) {
      stop("Each criterion for .match='label' must be a single label string")
    }
    
    label_exprs <- resolve_labels_to_expressions(criterion, layout_defs)
    
    # Find cells with matching expressions
    matching <- Filter(function(cell) {
      cell_expr <- if (dimension == "row") {
        cell$specification$row_expr
      } else {
        cell$specification$col_expr
      }
      
      any(sapply(label_exprs, function(e) identical(e, cell_expr)))
    }, all_cells)
    
  } else {
    # Build matcher using build_matcher_from_input
    matcher_info <- build_matcher_from_input(
      criterion, 
      match_type, 
      dimension, 
      context
    )
    
    # Create a layout_def with this matcher
    matcher_args <- list()
    matcher_args[[matcher_info$socket]] <- matcher_info$matcher
    matcher_args$label <- "temp"
    matcher_args$dimension <- dimension
    
    temp_def <- do.call(new_layout_def, matcher_args)
    
    # Filter cells using this def
    matching <- Filter(function(cell) {
      cell_qualifies_for_def(cell, temp_def)
    }, all_cells)
  }
  
  matching
}

#' Check if row_def matches an expression
#'
#' @param row_def A layout_def object
#' @param expr Expression to check
#' @return Logical
#' @keywords internal
row_def_matches_expression <- function(row_def, expr) {
  if (!is.null(row_def$row_expr_matcher)) {
    return(row_def$row_expr_matcher(expr))
  }
  
  # For row_dsl_matcher (after pivot), try matching on row DSL
  # This is more complex - for now, return FALSE
  # Could be enhanced to handle pivoted grids better
  FALSE
}

#' Check if col_def matches an expression
#'
#' @param col_def A layout_def object
#' @param expr Expression to check
#' @return Logical
#' @keywords internal
col_def_matches_expression <- function(col_def, expr) {
  if (!is.null(col_def$col_expr_matcher)) {
    return(col_def$col_expr_matcher(expr))
  }
  
  FALSE
}

#' Create custom row definition
#'
#' Creates a layout_def for a custom row matcher that evaluates full cells.
#' Since the custom function needs access to the full cell, we create a
#' matcher for each component that the function might use.
#'
#' @param fn Custom function taking a cell and returning TRUE/FALSE
#' @param label Label for the position
#' @return layout_def object
#' @keywords internal
create_custom_row_def <- function(fn, label) {
  # Create matchers that apply the custom function to cell components
  # The challenge: layout_def matchers receive individual components, not the full cell
  # Solution: Store the function and apply it during allocation
  
  # For now, create a layout_def with a custom row_expr_matcher
  # that always returns TRUE (matches all), and we'll filter later
  # This is a limitation of the current system - custom matchers for define_*
  # need special handling
  
  # Better approach: Use all matchers to approximate the custom function
  # But this is complex. For now, warn the user.
  warning("Custom matchers in define_rows/define_cols may not work as expected. ",
          "Consider using select_rows/select_cols with .match='custom' instead.")
  
  # Create a permissive def
  new_layout_def(
    row_expr_matcher = function(expr) TRUE,  # Match all
    label = label,
    dimension = "row"
  )
}

#' Create custom column definition
#'
#' @param fn Custom function taking a cell and returning TRUE/FALSE
#' @param label Label for the position
#' @return layout_def object
#' @keywords internal
create_custom_col_def <- function(fn, label) {
  warning("Custom matchers in define_rows/define_cols may not work as expected. ",
          "Consider using select_cols with .match='custom' instead.")
  
  new_layout_def(
    col_expr_matcher = function(expr) TRUE,
    label = label,
    dimension = "col"
  )
}

#' Layout Definition Objects for Tab Architecture
#'
#' Implements layout definition objects with function sockets for cell qualification.
#' Each layout_def represents one row or column position in the grid and contains
#' matcher functions that determine which cells qualify for that position.

#' Create a new layout definition
#'
#' Constructor for layout_def objects. Layout definitions contain function sockets
#' that match cell components. A cell qualifies for a position if ALL non-NULL
#' matchers return TRUE.
#'
#' @param ... Named matcher functions (e.g., row_expr_matcher = function(x) ...)
#'   Valid socket names: cell_id_matcher, value_matcher, base_matcher,
#'   row_expr_matcher, col_expr_matcher, base_expr_matcher, row_dsl_matcher,
#'   col_dsl_matcher, base_dsl_matcher, statistic_matcher, values_var_matcher,
#'   is_summary_row_matcher, is_summary_col_matcher, meta_matcher,
#'   computation_matcher, derivation_matcher
#' @param label Character label for this position (displayed in output)
#' @param dimension Character, either "row" or "col"
#' @return A layout_def object
#' @keywords internal
#' @examples
#' \dontrun{
#' # Simple exact match on expression
#' new_layout_def(
#'   row_expr_matcher = function(expr) identical(expr, quote(gender == 1)),
#'   label = "Male",
#'   dimension = "row"
#' )
#'
#' # Match summary rows
#' new_layout_def(
#'   is_summary_row_matcher = function(x) isTRUE(x),
#'   label = "NET",
#'   dimension = "row"
#' )
#'
#' # Match cells with base >= 30
#' new_layout_def(
#'   base_matcher = function(base) base >= 30,
#'   label = "Valid responses",
#'   dimension = "row"
#' )
#' }
new_layout_def <- function(..., label, dimension = c("row", "col")) {
  dimension <- match.arg(dimension)
  
  # Capture all matcher functions
  matchers <- list(...)
  
  # Initialize all sockets to NULL
  layout_def <- list(
    cell_id_matcher = NULL,
    value_matcher = NULL,
    base_matcher = NULL,
    row_expr_matcher = NULL,
    col_expr_matcher = NULL,
    base_expr_matcher = NULL,
    row_dsl_matcher = NULL,
    col_dsl_matcher = NULL,
    base_dsl_matcher = NULL,
    statistic_matcher = NULL,
    values_var_matcher = NULL,
    is_summary_row_matcher = NULL,
    is_summary_col_matcher = NULL,
    meta_matcher = NULL,
    computation_matcher = NULL,
    derivation_matcher = NULL,
    label = label,
    dimension = dimension
  )
  
  # Override with provided matchers
  for (name in names(matchers)) {
    if (name %in% names(layout_def)) {
      layout_def[[name]] <- matchers[[name]]
    } else {
      warning("Unknown matcher socket: ", name)
    }
  }
  
  structure(layout_def, class = "layout_def")
}

#' Check if a cell qualifies for a layout definition
#'
#' Tests whether a cell matches all non-NULL matchers in a layout_def.
#' This is the core qualification function used by the allocation machinery.
#'
#' @param cell Cell object from cell store
#' @param layout_def Layout definition object
#' @return TRUE if cell qualifies (all matchers pass), FALSE otherwise
#' @keywords internal
cell_qualifies_for_def <- function(cell, layout_def) {
  if (!inherits(layout_def, "layout_def")) {
    stop("layout_def must be a layout_def object")
  }
  
  # Check each socket - NULL means skip (auto-qualify)
  # Return FALSE immediately if any matcher fails
  
  if (!is.null(layout_def$cell_id_matcher)) {
    if (!layout_def$cell_id_matcher(cell$cell_id)) return(FALSE)
  }
  
  if (!is.null(layout_def$value_matcher)) {
    if (!layout_def$value_matcher(cell$value)) return(FALSE)
  }
  
  if (!is.null(layout_def$base_matcher)) {
    if (!layout_def$base_matcher(cell$base)) return(FALSE)
  }
  
  # Specification matchers - extract from nested structure
  if (!is.null(layout_def$row_expr_matcher)) {
    if (!layout_def$row_expr_matcher(cell$specification$row_expr)) return(FALSE)
  }
  
  if (!is.null(layout_def$col_expr_matcher)) {
    if (!layout_def$col_expr_matcher(cell$specification$col_expr)) return(FALSE)
  }
  
  if (!is.null(layout_def$base_expr_matcher)) {
    if (!layout_def$base_expr_matcher(cell$specification$base_expr)) return(FALSE)
  }
  
  # DSL matchers - extract from specification$dsl
  if (!is.null(layout_def$row_dsl_matcher)) {
    row_dsl <- cell$specification$dsl$row
    if (!layout_def$row_dsl_matcher(row_dsl)) return(FALSE)
  }
  
  if (!is.null(layout_def$col_dsl_matcher)) {
    col_dsl <- cell$specification$dsl$col
    if (!layout_def$col_dsl_matcher(col_dsl)) return(FALSE)
  }
  
  if (!is.null(layout_def$base_dsl_matcher)) {
    base_dsl <- cell$specification$dsl$base
    if (!layout_def$base_dsl_matcher(base_dsl)) return(FALSE)
  }
  
  if (!is.null(layout_def$statistic_matcher)) {
    if (!layout_def$statistic_matcher(cell$specification$statistic_id)) return(FALSE)
  }
  
  if (!is.null(layout_def$values_var_matcher)) {
    if (!layout_def$values_var_matcher(cell$specification$values_var)) return(FALSE)
  }
  
  if (!is.null(layout_def$is_summary_row_matcher)) {
    if (!layout_def$is_summary_row_matcher(cell$specification$is_summary_row)) return(FALSE)
  }
  
  if (!is.null(layout_def$is_summary_col_matcher)) {
    if (!layout_def$is_summary_col_matcher(cell$specification$is_summary_col)) return(FALSE)
  }
  
  if (!is.null(layout_def$meta_matcher)) {
    if (!layout_def$meta_matcher(cell$specification$meta)) return(FALSE)
  }
  
  # Computation and derivation matchers
  if (!is.null(layout_def$computation_matcher)) {
    if (!layout_def$computation_matcher(cell$computation)) return(FALSE)
  }
  
  if (!is.null(layout_def$derivation_matcher)) {
    if (!layout_def$derivation_matcher(cell$derivation)) return(FALSE)
  }
  
  # All checks passed
  TRUE
}

#' Initialize layout definitions from expanded specs
#'
#' Converts expanded specifications (from parse/expand stage) into layout_defs.
#' This is the primary way to create initial row_defs and col_defs from tab() specs.
#'
#' @param expanded_specs List of expanded specification objects
#' @param summary_spec Optional summary specification (for NET/Total/Avg rows/cols)
#' @param dimension Character, either "row" or "col"
#' @return List of layout_def objects
#' @keywords internal
initialize_layout_defs <- function(expanded_specs, summary_spec = NULL, dimension = c("row", "col")) {
  dimension <- match.arg(dimension)
  
  # Determine which dimension matcher to use
  expr_matcher_name <- if (dimension == "row") "row_expr_matcher" else "col_expr_matcher"
  
  # Create layout_def for each regular spec
  defs <- lapply(expanded_specs, function(spec) {
    # Create matcher that checks for exact expression identity
    matcher_fn <- function(expr) identical(expr, spec$expr)
    
    # Build matcher list
    matchers <- list()
    matchers[[expr_matcher_name]] <- matcher_fn
    
    # Create layout_def
    do.call(new_layout_def, c(matchers, list(
      label = spec$label,
      dimension = dimension
    )))
  })
  
  # Add summary if present
  if (!is.null(summary_spec)) {
    summary_matchers <- list()
    
    if (dimension == "row") {
      summary_matchers$is_summary_row_matcher <- function(x) isTRUE(x)
    } else {
      summary_matchers$is_summary_col_matcher <- function(x) isTRUE(x)
    }
    
    summary_def <- do.call(new_layout_def, c(summary_matchers, list(
      label = summary_spec$label,
      dimension = dimension
    )))
    
    defs <- c(defs, list(summary_def))
  }
  
  defs
}

# ============================================================================
# Helper Matcher Constructors
# ============================================================================

#' Create an exact match matcher
#'
#' Returns a function that checks for identity with a target value.
#'
#' @param target Value to match against
#' @return Matcher function
#' @keywords internal
#' @examples
#' \dontrun{
#' matcher <- exact_match_matcher(quote(gender == 1))
#' matcher(quote(gender == 1))  # TRUE
#' matcher(quote(gender == 2))  # FALSE
#' }
exact_match_matcher <- function(target) {
  function(x) identical(x, target)
}

#' Create a threshold matcher
#'
#' Returns a function that checks if a value meets a threshold condition.
#'
#' @param threshold Numeric threshold value
#' @param op Comparison operator as string (default ">=")
#' @return Matcher function
#' @keywords internal
#' @examples
#' \dontrun{
#' matcher <- threshold_matcher(30, ">=")
#' matcher(50)  # TRUE
#' matcher(20)  # FALSE
#' }
threshold_matcher <- function(threshold, op = ">=") {
  function(x) {
    if (is.na(x) || is.null(x)) return(FALSE)
    do.call(op, list(x, threshold))
  }
}

#' Create a set membership matcher
#'
#' Returns a function that checks if a value is in a valid set.
#'
#' @param valid_set Vector of valid values
#' @return Matcher function
#' @keywords internal
#' @examples
#' \dontrun{
#' matcher <- set_matcher(c("column_pct", "row_pct"))
#' matcher("column_pct")  # TRUE
#' matcher("mean")        # FALSE
#' }
set_matcher <- function(valid_set) {
  function(x) x %in% valid_set
}

#' Create a DSL subset matcher
#'
#' Returns a function that checks if a DSL expression is a subset of
#' another using semantic matching.
#'
#' @param query_dsl DSL expression to check for containment
#' @param data Data frame (for dsl_is_subset evaluation)
#' @return Matcher function
#' @keywords internal
#' @examples
#' \dontrun{
#' matcher <- dsl_subset_matcher(quote(age > 18), data)
#' matcher(quote(age > 18 & gender == 1))  # TRUE (query is subset)
#' matcher(quote(gender == 1))              # FALSE (query not subset)
#' }
dsl_subset_matcher <- function(query_dsl, data) {
  function(cell_dsl) {
    if (is.null(cell_dsl)) return(FALSE)
    dsl_is_subset(query_dsl, cell_dsl, data)
  }
}

#' Create a negated matcher
#'
#' Returns a function that negates another matcher function.
#'
#' @param matcher Matcher function to negate
#' @return Negated matcher function
#' @keywords internal
#' @examples
#' \dontrun{
#' is_summary <- function(x) isTRUE(x)
#' is_not_summary <- negate_matcher(is_summary)
#' is_not_summary(FALSE)  # TRUE
#' is_not_summary(TRUE)   # FALSE
#' }
negate_matcher <- function(matcher) {
  function(x) !matcher(x)
}

# ============================================================================
# Print Methods
# ============================================================================

#' @export
print.layout_def <- function(x, ...) {
  cat("<layout_def:", x$dimension, ">\n")
  cat("  Label:", x$label, "\n")
  
  # Count active matchers
  matcher_names <- c(
    "cell_id_matcher", "value_matcher", "base_matcher",
    "row_expr_matcher", "col_expr_matcher", "base_expr_matcher",
    "row_dsl_matcher", "col_dsl_matcher", "base_dsl_matcher",
    "statistic_matcher", "values_var_matcher",
    "is_summary_row_matcher", "is_summary_col_matcher",
    "meta_matcher", "computation_matcher", "derivation_matcher"
  )
  
  active_matchers <- sapply(matcher_names, function(name) !is.null(x[[name]]))
  n_active <- sum(active_matchers)
  
  cat("  Active matchers:", n_active, "/", length(matcher_names), "\n")
  
  if (n_active > 0) {
    cat("    ", paste(names(active_matchers)[active_matchers], collapse = ", "), "\n")
  }
  
  invisible(x)
}

#' @export
summary.layout_def <- function(object, ...) {
  print(object, ...)
}

#' Layout Allocation Functions for Tab Architecture
#'
#' Implements the two-stage process for cell filtering and grid allocation.
#' Stage 1: Filter cell pool using filter_rules
#' Stage 2: Allocate filtered cells to grid positions using layout_defs

#' Filter cell pool using filter rules
#'
#' Stage 1 of the allocation process. Applies filter_rules sequentially to reduce
#' the set of cells available for layout allocation. This is where low base filtering,
#' hiding, and other cell-level filters are applied.
#'
#' @param store Cell store object
#' @param filter_rules List of layout_def objects used as filters (default empty list)
#' @return Character vector of qualifying cell IDs
#' @keywords internal
filter_cell_pool <- function(store, filter_rules = list()) {
  # Get all cell IDs from store
  all_ids <- names(store$cells)
  
  if (length(all_ids) == 0) {
    return(character(0))
  }
  
  qualifying_ids <- all_ids
  
  # Apply each filter rule sequentially
  for (rule in filter_rules) {
    qualifying_ids <- Filter(function(cell_id) {
      cell <- get_cell(store, cell_id)
      cell_qualifies_for_def(cell, rule)
    }, qualifying_ids)
    
    # Early exit if no cells remain
    if (length(qualifying_ids) == 0) {
      break
    }
  }
  
  qualifying_ids
}

#' Allocate cells to grid positions
#'
#' Stage 2 of the allocation process. Places cells from the filtered pool into
#' grid positions based on layout definitions. Handles collisions and empty positions.
#'
#' @param store Cell store object
#' @param row_defs List of layout_def objects for rows (ordered)
#' @param col_defs List of layout_def objects for columns (ordered)
#' @param cell_pool Character vector of cell IDs available for allocation
#' @return Layout object with grid, row_defs, col_defs, and metadata
#' @keywords internal
allocate_cells_to_grid <- function(store, row_defs, col_defs, cell_pool) {
  n_rows <- length(row_defs)
  n_cols <- length(col_defs)
  
  # Initialize grid and collision tracker
  grid <- matrix(NA_character_, nrow = n_rows, ncol = n_cols)
  collision_tracker <- matrix(list(), nrow = n_rows, ncol = n_cols)
  
  # Get cells from pool
  if (length(cell_pool) == 0) {
    # Empty pool - return empty grid
    return(list(
      type = "explicit_grid",
      grid = grid,
      row_defs = row_defs,
      col_defs = col_defs,
      row_labels = sapply(row_defs, `[[`, "label"),
      col_labels = sapply(col_defs, `[[`, "label"),
      has_summary_row = any(sapply(row_defs, function(d) !is.null(d$is_summary_row_matcher))),
      has_summary_col = any(sapply(col_defs, function(d) !is.null(d$is_summary_col_matcher)))
    ))
  }
  
  pool_cells <- get_cells(store, cell_pool)
  
  # For each grid position, find qualifying cells
  for (i in seq_len(n_rows)) {
    row_def <- row_defs[[i]]
    
    for (j in seq_len(n_cols)) {
      col_def <- col_defs[[j]]
      
      # Find cells that qualify for BOTH row and col
      qualifying_cells <- Filter(function(cell) {
        row_qualified <- cell_qualifies_for_def(cell, row_def)
        col_qualified <- cell_qualifies_for_def(cell, col_def)
        
        row_qualified && col_qualified
      }, pool_cells)
      
      # Extract cell IDs
      qualifying_ids <- sapply(qualifying_cells, `[[`, "cell_id")
      
      # Assign based on number of matches
      if (length(qualifying_ids) == 0) {
        # No match - leave as NA
        grid[i, j] <- NA_character_
      } else if (length(qualifying_ids) == 1) {
        # Exactly one match - assign
        grid[i, j] <- qualifying_ids[1]
      } else {
        # Collision - multiple cells qualify
        collision_tracker[[i, j]] <- qualifying_ids
      }
    }
  }
  
  # Check for collisions
  has_collisions <- any(sapply(collision_tracker, length) > 1)
  
  if (has_collisions) {
    # Expand grid to handle collisions
    return(expand_grid_for_collisions(grid, collision_tracker, row_defs, col_defs))
  }
  
  # No collisions - return layout
  list(
    type = "explicit_grid",
    grid = grid,
    row_defs = row_defs,
    col_defs = col_defs,
    row_labels = sapply(row_defs, `[[`, "label"),
    col_labels = sapply(col_defs, `[[`, "label"),
    has_summary_row = any(sapply(row_defs, function(d) !is.null(d$is_summary_row_matcher))),
    has_summary_col = any(sapply(col_defs, function(d) !is.null(d$is_summary_col_matcher)))
  )
}

#' Expand grid for collision handling
#'
#' When multiple cells qualify for the same grid position, expand the grid
#' by duplicating rows or columns. Deep-copies layout_defs for new positions.
#'
#' @param grid Current grid matrix (may have empty positions where collisions exist)
#' @param collision_tracker Matrix of lists containing cell IDs for each collision
#' @param row_defs List of layout_def objects for rows
#' @param col_defs List of layout_def objects for columns
#' @return Layout object with expanded grid
#' @keywords internal
expand_grid_for_collisions <- function(grid, collision_tracker, row_defs, col_defs) {
  # Identify which rows and columns have collisions
  row_has_collision <- apply(collision_tracker, 1, function(row) {
    any(sapply(row, length) > 1)
  })
  
  col_has_collision <- apply(collision_tracker, 2, function(col) {
    any(sapply(col, length) > 1)
  })
  
  # Expand rows if needed
  if (any(row_has_collision)) {
    expanded_row_defs <- list()
    expanded_row_indices <- integer(0)
    
    for (i in seq_along(row_defs)) {
      if (row_has_collision[i]) {
        # Find max cells in any position in this row
        max_cells <- max(sapply(collision_tracker[i, ], length))
        
        # Deep-copy row_def for each sub-position
        for (sub_idx in seq_len(max_cells)) {
          new_def <- row_defs[[i]]  # Copy list structure
          new_def$label <- paste0(new_def$label, " [", sub_idx, "]")
          new_def$sub_index <- sub_idx
          expanded_row_defs <- c(expanded_row_defs, list(new_def))
          expanded_row_indices <- c(expanded_row_indices, i)
        }
      } else {
        expanded_row_defs <- c(expanded_row_defs, list(row_defs[[i]]))
        expanded_row_indices <- c(expanded_row_indices, i)
      }
    }
    
    row_defs <- expanded_row_defs
  } else {
    expanded_row_indices <- seq_along(row_defs)
  }
  
  # Expand cols if needed
  if (any(col_has_collision)) {
    expanded_col_defs <- list()
    expanded_col_indices <- integer(0)
    
    for (j in seq_along(col_defs)) {
      if (col_has_collision[j]) {
        # Find max cells in any position in this column
        max_cells <- max(sapply(collision_tracker[, j], length))
        
        # Deep-copy col_def for each sub-position
        for (sub_idx in seq_len(max_cells)) {
          new_def <- col_defs[[j]]  # Copy list structure
          new_def$label <- paste0(new_def$label, " [", sub_idx, "]")
          new_def$sub_index <- sub_idx
          expanded_col_defs <- c(expanded_col_defs, list(new_def))
          expanded_col_indices <- c(expanded_col_indices, j)
        }
      } else {
        expanded_col_defs <- c(expanded_col_defs, list(col_defs[[j]]))
        expanded_col_indices <- c(expanded_col_indices, j)
      }
    }
    
    col_defs <- expanded_col_defs
  } else {
    expanded_col_indices <- seq_along(col_defs)
  }
  
  # Build expanded grid
  new_grid <- matrix(NA_character_, nrow = length(row_defs), ncol = length(col_defs))
  
  # Place cells in expanded grid
  for (i_new in seq_along(row_defs)) {
    i_old <- expanded_row_indices[i_new]
    row_sub_idx <- row_defs[[i_new]]$sub_index %||% 1
    
    for (j_new in seq_along(col_defs)) {
      j_old <- expanded_col_indices[j_new]
      col_sub_idx <- col_defs[[j_new]]$sub_index %||% 1
      
      # Get cells at original position
      cells_at_pos <- collision_tracker[[i_old, j_old]]
      
      # Assign cell at sub-index if available
      if (length(cells_at_pos) >= max(row_sub_idx, col_sub_idx)) {
        # For simplicity, use the combined index
        cell_idx <- if (length(cells_at_pos) == 1) {
          1
        } else if (row_sub_idx == col_sub_idx) {
          row_sub_idx
        } else {
          # When row and col have different sub-indices, use row sub-index
          min(row_sub_idx, length(cells_at_pos))
        }
        new_grid[i_new, j_new] <- cells_at_pos[cell_idx]
      }
    }
  }
  
  # Return expanded layout
  list(
    type = "explicit_grid",
    grid = new_grid,
    row_defs = row_defs,
    col_defs = col_defs,
    row_labels = sapply(row_defs, `[[`, "label"),
    col_labels = sapply(col_defs, `[[`, "label"),
    has_summary_row = any(sapply(row_defs, function(d) !is.null(d$is_summary_row_matcher))),
    has_summary_col = any(sapply(col_defs, function(d) !is.null(d$is_summary_col_matcher)))
  )
}

#' Refresh layout by reallocating grid
#'
#' Reallocates the grid from current layout_defs and filter_rules. This is called
#' after any layout manipulation that modifies layout_defs or filter_rules.
#'
#' @param tab_result A tab_result object with cell_store and layout
#' @return Modified tab_result with reallocated grid
#' @keywords internal
refresh_layout <- function(tab_result) {
  if (!inherits(tab_result, "tab_cell_collection")) {
    stop("refresh_layout() requires a cell-based tab_result")
  }
  
  # Filter cell pool (Stage 1)
  filter_rules <- tab_result$layout$filter_rules %||% list()
  cell_pool <- filter_cell_pool(tab_result$cell_store, filter_rules)
  
  # Allocate to grid (Stage 2)
  new_layout <- allocate_cells_to_grid(
    tab_result$cell_store,
    tab_result$layout$row_defs,
    tab_result$layout$col_defs,
    cell_pool
  )
  
  # Preserve filter_rules (allocate_cells_to_grid doesn't include them)
  new_layout$filter_rules <- filter_rules
  
  # Update tab_result
  tab_result$layout <- new_layout
  tab_result
}

#' Remove empty rows and columns from grid
#'
#' Post-processing function that removes rows/columns where all cells are NA.
#' Also removes corresponding layout_defs.
#'
#' @param tab_result A tab_result object
#' @return Modified tab_result with empty rows/cols removed
#' @keywords internal
remove_empty_rows_cols <- function(tab_result) {
  if (!inherits(tab_result, "tab_cell_collection")) {
    return(tab_result)
  }
  
  grid <- tab_result$layout$grid
  
  if (is.null(grid) || length(grid) == 0) {
    return(tab_result)
  }
  
  # Find non-empty rows
  non_empty_rows <- which(apply(grid, 1, function(row) !all(is.na(row))))
  
  # Find non-empty columns
  non_empty_cols <- which(apply(grid, 2, function(col) !all(is.na(col))))
  
  # If all rows or cols would be removed, keep at least one
  if (length(non_empty_rows) == 0) non_empty_rows <- 1
  if (length(non_empty_cols) == 0) non_empty_cols <- 1
  
  # Subset grid and layout_defs
  if (length(non_empty_rows) < nrow(grid) || length(non_empty_cols) < ncol(grid)) {
    tab_result$layout$grid <- grid[non_empty_rows, non_empty_cols, drop = FALSE]
    tab_result$layout$row_defs <- tab_result$layout$row_defs[non_empty_rows]
    tab_result$layout$col_defs <- tab_result$layout$col_defs[non_empty_cols]
    tab_result$layout$row_labels <- sapply(tab_result$layout$row_defs, `[[`, "label")
    tab_result$layout$col_labels <- sapply(tab_result$layout$col_defs, `[[`, "label")
  }
  
  tab_result
}