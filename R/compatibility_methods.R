# Compatibility Methods for Cell-Based Tab Results
#
# S3 methods to make cell-based tab_result objects feel like data.frames
# for common operations, while maintaining cell-based structure internally.

#' Determine which rows are hidden by filter rules
#' @keywords internal
.get_hidden_row_indices <- function(tab_result) {
  if (is.null(tab_result$layout$filter_rules)) return(integer(0))
  
  grid <- tab_result$layout$grid
  if (is.null(grid)) return(integer(0))
  
  hidden_rows <- integer(0)
  
  # Check each row to see if ALL its cells are filtered out
  for (i in seq_len(nrow(grid))) {
    row_cells <- grid[i, ]
    # If all cells in row are NA, check if this is due to filtering
    if (all(is.na(row_cells))) {
      row_label <- tab_result$layout$row_labels[i]
      
      # Check filter rules to see if any specifically hide this row
      for (rule in tab_result$layout$filter_rules) {
        if (is.null(rule$label)) next
        
        # Check if this is a hide_rows rule that mentions this specific row
        if (grepl("hide_rows:", rule$label, fixed = TRUE) && 
            grepl(row_label, rule$label, fixed = TRUE)) {
          hidden_rows <- c(hidden_rows, i)
          break
        }
        
        # Check if this is a hide_summary_rows rule and this row is a summary
        if (rule$label == "hide_summary_rows") {
          # Check if this row is actually a summary row by checking the rule matcher
          if (!is.null(rule$is_summary_row_matcher)) {
            # Find a cell in this row to test
            for (j in seq_len(ncol(grid))) {
              test_cell_id <- grid[i, j]
              if (!is.na(test_cell_id)) break
            }
            # If we didn't find any cell, it might be a summary that was filtered
            # In that case, check if the label suggests it's a summary
            if (row_label %in% c("NET", "Total")) {
              hidden_rows <- c(hidden_rows, i)
              break
            }
          }
        }
      }
    }
  }
  
  unique(hidden_rows)
}

#' Determine which columns are hidden by filter rules
#' @keywords internal
.get_hidden_col_indices <- function(tab_result) {
  if (is.null(tab_result$layout$filter_rules)) return(integer(0))
  
  grid <- tab_result$layout$grid
  if (is.null(grid)) return(integer(0))
  
  hidden_cols <- integer(0)
  
  # Check each column to see if ALL its cells are filtered out
  for (j in seq_len(ncol(grid))) {
    col_cells <- grid[, j]
    # If all cells in column are NA, check if this is due to filtering
    if (all(is.na(col_cells))) {
      col_label <- tab_result$layout$col_labels[j]
      
      # Check filter rules to see if any specifically hide this column
      for (rule in tab_result$layout$filter_rules) {
        if (is.null(rule$label)) next
        
        # Check if this is a hide_cols rule that mentions this specific column
        if (grepl("hide_cols:", rule$label, fixed = TRUE) && 
            grepl(col_label, rule$label, fixed = TRUE)) {
          hidden_cols <- c(hidden_cols, j)
          break
        }
        
        # Check if this is a hide_summary_cols rule and this column is a summary
        if (rule$label == "hide_summary_cols") {
          # Check if the label suggests it's a summary column
          if (col_label %in% c("NET", "Total")) {
            hidden_cols <- c(hidden_cols, j)
            break
          }
        }
      }
    }
  }
  
  unique(hidden_cols)
}

#' Get number of rows in tab_result
#' @param x A tab_result object
#' @export
nrow.tab_result <- function(x) {
  if (inherits(x, "tab_cell_collection")) {
    # Access layout grid directly to avoid recursion
    grid <- x$layout$grid
    if (is.null(grid)) return(0)
    
    # Get specifically hidden row indices
    hidden_rows <- .get_hidden_row_indices(x)
    
    # Return count excluding only the specifically hidden rows
    base::nrow(grid) - length(hidden_rows)
  } else if (inherits(x, "data.frame")) {
    NextMethod()
  } else {
    0
  }
}

#' Get number of columns in tab_result
#' @param x A tab_result object
#' @export
ncol.tab_result <- function(x) {
  if (inherits(x, "tab_cell_collection")) {
    # Access layout grid directly to avoid recursion
    grid <- x$layout$grid
    if (is.null(grid)) return(0)
    
    # Get specifically hidden column indices
    hidden_cols <- .get_hidden_col_indices(x)
    
    # Return count excluding only the specifically hidden columns + 1 for row_label
    base::ncol(grid) - length(hidden_cols) + 1L
  } else if (inherits(x, "data.frame")) {
    NextMethod()
  } else {
    0
  }
}

#' Get column names from tab_result
#' @param x A tab_result object
#' @export
names.tab_result <- function(x) {
  if (inherits(x, "tab_cell_collection")) {
    col_labels <- x$layout$col_labels
    if (is.null(col_labels)) return(character(0))
    
    # Get specifically hidden column indices
    hidden_cols <- .get_hidden_col_indices(x)
    
    # Return names excluding only the specifically hidden columns
    if (length(hidden_cols) > 0) {
      col_labels <- col_labels[-hidden_cols]
    }
    
    c("row_label", col_labels)
  } else if (inherits(x, "data.frame")) {
    NextMethod()
  } else {
    character(0)
  }
}

#' Subset tab_result (materializes first)
#' @param x A tab_result object
#' @param i Row indices
#' @param j Column indices
#' @param drop Logical; passed to base subsetting (default FALSE)
#' @export
`[.tab_result` <- function(x, i, j, drop = FALSE) {
  if (inherits(x, "tab_cell_collection")) {
    df <- as.data.frame(x)
    df[i, j, drop = drop]
  } else {
    NextMethod()
  }
}

#' Extract column or cell from tab_result
#' @param x A tab_result object
#' @param i Element name or index
#' @export
`[[.tab_result` <- function(x, i) {
  if (inherits(x, "tab_cell_collection")) {
    # Check if accessing list element or column
    # Use direct list access to avoid recursion
    list_names <- c("cell_store", "layout", "arrays", "data", "dpdict",
                    "statistic", "measures", "measure_axis",
                    "show_base", "base_spec", "label_mode", "derive_operations", "formatting", "call")
    if (i %in% list_names) {
      # List element access - use unclass to avoid recursion
      unclass(x)[[i]]
    } else {
      # Column access - materialize
      df <- as.data.frame(x)
      df[[i]]
    }
  } else {
    NextMethod()
  }
}

#' Extract element from tab_result using $
#' @param x A tab_result object
#' @param name Element name
#' @export
`$.tab_result` <- function(x, name) {
  if (inherits(x, "tab_cell_collection")) {
    # Use direct list access to avoid recursion
    list_names <- c("cell_store", "layout", "arrays", "data", "dpdict",
                    "statistic", "measures", "measure_axis",
                    "show_base", "base_spec", "label_mode", "derive_operations", "formatting", "call")
    if (name %in% list_names) {
      # List element - use unclass to avoid recursion
      unclass(x)[[name]]
    } else {
      # Column access
      df <- as.data.frame(x)
      df[[name]]
    }
  } else {
    NextMethod()
  }
}

#' Get dimensions of tab_result
#' @param x A tab_result object
#' @export
dim.tab_result <- function(x) {
  if (inherits(x, "tab_cell_collection")) {
    # Access layout grid directly to avoid recursion
    grid <- x$layout$grid
    if (is.null(grid)) return(c(0L, 0L))
    
    # Get specifically hidden row and column indices
    hidden_rows <- .get_hidden_row_indices(x)
    hidden_cols <- .get_hidden_col_indices(x)
    
    # Return dimensions excluding only the specifically hidden elements
    c(base::nrow(grid) - length(hidden_rows), 
      base::ncol(grid) - length(hidden_cols) + 1L)
  } else if (inherits(x, "data.frame")) {
    NextMethod()
  } else {
    NULL
  }
}

#' Get dimension names (row and column names) of tab_result
#' @param x A tab_result object
#' @export
dimnames.tab_result <- function(x) {
  if (inherits(x, "tab_cell_collection")) {
    row_labels <- x$layout$row_labels
    col_labels <- x$layout$col_labels
    if (is.null(row_labels) || is.null(col_labels)) return(NULL)
    
    # Get specifically hidden row and column indices
    hidden_rows <- .get_hidden_row_indices(x)
    hidden_cols <- .get_hidden_col_indices(x)
    
    # Return dimnames excluding only the specifically hidden elements
    if (length(hidden_rows) > 0) {
      row_labels <- row_labels[-hidden_rows]
    }
    if (length(hidden_cols) > 0) {
      col_labels <- col_labels[-hidden_cols]
    }
    
    list(row_labels, c("row_label", col_labels))
  } else if (inherits(x, "data.frame")) {
    NextMethod()
  } else {
    NULL
  }
}

#' Get first n rows of tab_result
#' @param x A tab_result object
#' @param n Number of rows to return (default 6)
#' @param ... Passed to the underlying method
#' @importFrom utils head tail str
#' @export
head.tab_result <- function(x, n = 6L, ...) {
  if (inherits(x, "tab_cell_collection")) {
    df <- as.data.frame(x)
    utils::head(df, n = n, ...)
  } else if (inherits(x, "data.frame")) {
    NextMethod()
  } else {
    df <- as.data.frame(x)
    utils::head(df, n = n, ...)
  }
}

#' Get last n rows of tab_result
#' @param x A tab_result object
#' @param n Number of rows to return (default 6)
#' @param ... Passed to the underlying method
#' @export
tail.tab_result <- function(x, n = 6L, ...) {
  if (inherits(x, "tab_cell_collection")) {
    df <- as.data.frame(x)
    utils::tail(df, n = n, ...)
  } else if (inherits(x, "data.frame")) {
    NextMethod()
  } else {
    df <- as.data.frame(x)
    utils::tail(df, n = n, ...)
  }
}

#' Get column names of tab_result
#' @param x A tab_result object
#' @export
colnames.tab_result <- function(x) {
  if (inherits(x, "tab_cell_collection")) {
    # Access layout labels directly to avoid recursion
    col_labels <- x$layout$col_labels
    if (is.null(col_labels)) return(character(0))
    c("row_label", col_labels)  # First column is always row_label
  } else if (inherits(x, "data.frame")) {
    NextMethod()
  } else {
    NULL
  }
}

#' Get row names of tab_result
#' @param x A tab_result object
#' @export
rownames.tab_result <- function(x) {
  if (inherits(x, "tab_cell_collection")) {
    # Access layout labels directly to avoid recursion
    row_labels <- x$layout$row_labels
    if (is.null(row_labels)) return(NULL)
    as.character(seq_along(row_labels))  # Standard numeric row names
  } else if (inherits(x, "data.frame")) {
    NextMethod()
  } else {
    NULL
  }
}

#' Display structure of tab_result
#' @param object A tab_result object
#' @param ... Passed to the underlying method
#' @export
str.tab_result <- function(object, ...) {
  if (inherits(object, "tab_cell_collection")) {
    cat("tab_result (cell-based):\n")
    df <- as.data.frame(object)
    utils::str(df, ...)
  } else if (inherits(object, "data.frame")) {
    NextMethod()
  } else {
    df <- as.data.frame(object)
    utils::str(df, ...)
  }
}