# Significance Testing
#
# Add statistical significance indicators to tab results:
# - add_sig: Test one column against a reference
# - add_sig_all: Test all columns pairwise
# - compute_significance: Core testing engine (z-tests, t-tests, Mann-Whitney, etc.)
# - Supports multiple comparison adjustments (Bonferroni, BH, etc.)
#
# Test types:
# - Auto-detection based on statistic type
# - z_test_proportions (for counts/percentages)
# - t_test (for means)
# - mann_whitney (for medians)

#' Add significance testing to a tab result
#'
#' Performs statistical significance testing on tab results. Significance is 
#' attached directly to individual cells in the tab_cell_collection, preserving 
#' the cell-based pipeline without requiring materialization.
#'
#' @param tab_result A tab_result object from tab()
#' @param versus Column to compare against: "first_col", "last_col",
#'   column name, or column index
#' @param test Test to use. `"auto"` selects safe defaults for supported
#'   statistics (e.g., `column_pct`, `mean`, and `median` when unweighted).
#'   When weights are present, `"auto"` selects weighted tests where available.
#'   For other statistics (e.g., `count`, `row_pct`, `sd`, `cv`), `"auto"` errors;
#'   choose an explicit test.
#' @param level Significance level (default 0.05)
#' @param adjust Multiple comparison adjustment: "none", "bonferroni", "BH", etc.
#' @param name Optional name for this comparison (defaults to versus value)
#' @return The tab_result (tab_cell_collection) with significance attached to cells
#' 
#' @section Cell-Based Significance:
#' add_sig() attaches significance metadata directly to individual cells without 
#' materializing to data.frame. This enables true pipeline continuity:
#' 
#' \code{tab(data, q1, gender) \%>\% add_sig() \%>\% hide_rows("Don't know")}
#' 
#' Significance is preserved through derive operations and can be materialized
#' later using as.data.frame(). Multiple significance tests can be added by
#' calling add_sig() multiple times with different names.
#' 
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
  
  if (!inherits(tab_result, "tab_cell_collection")) {
    stop("add_sig() requires a cell-based tab_result object. ",
         "All tab() results are now cell-based.")
  }
  
  # Call cell-native implementation
  return(add_sig_cell_native(tab_result, versus, test, level, adjust, name))
}

#' Add significance testing versus all columns
#'
#' @param tab_result A tab_result object from tab()
#' @param test Test to use for all comparisons (see `add_sig()` for `"auto"`
#'   behavior and limitations)
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
  
  if (!inherits(tab_result, "tab_cell_collection")) {
    stop("add_sig_all() requires a cell-based tab_result object. ",
         "All tab() results are now cell-based.")
  }

  # Get statistic info to identify summary columns
  statistic <- tab_result$statistic
  
  # Check for mixed-statistic tabs (cannot perform significance testing)
  if (is.null(statistic)) {
    stop("Cannot perform significance testing on mixed-statistic tabs. ",
         "This tab was created by gluing tabs with different statistics. ",
         "Apply add_sig() before gluing, or glue tabs with the same statistic.")
  }

  # Build list of columns to exclude
  columns_to_exclude <- character()

  # Always exclude base column
  if (!is.null(statistic$base_label)) {
    columns_to_exclude <- c(columns_to_exclude, statistic$base_label)
  }

  # Always exclude summary columns
  if (!is.null(statistic$summary_col)) {
    columns_to_exclude <- c(columns_to_exclude, statistic$summary_col)
  }
  if (!is.null(statistic$summary_row)) {
    # In case summary_row appears as a column somehow
    columns_to_exclude <- c(columns_to_exclude, statistic$summary_row)
  }

  # Add any user-specified exclusions
  if (!is.null(exclude)) {
    columns_to_exclude <- c(columns_to_exclude, exclude)
  }

  # Get all column names from layout
  col_names <- tab_result$layout$col_labels

  # Remove excluded columns
  col_names <- setdiff(col_names, columns_to_exclude)

  # Only proceed if there are columns left to test
  if (length(col_names) == 0) {
    warning("No columns left to test after excluding summary/base columns")
    return(tab_result)
  }
  
  # Resolve 'auto' once up-front (fail early with a single clear error)
  if (identical(test, "auto")) {
    has_weights <- .sig_has_weights(tab_result$arrays$base_array)
    test <- .resolve_auto_sig_test(statistic$id, has_weights)
  }

  # Add significance vs each remaining column
  for (col in col_names) {
    # Use tryCatch to handle any issues with individual columns
    tryCatch({
      tab_result <- add_sig(tab_result,
                            versus = col,
                            test = test,
                            level = level,
                            adjust = adjust,
                            name = col)  # Use column name as test name
    }, error = function(e) {
      warning("Could not add significance for column '", col, "': ", e$message)
    })
  }

  return(tab_result)
}

#' Compute significance matrix for tab results
#' @keywords internal
compute_significance <- function(base_array, row_arrays, row_u_arrays, col_arrays, col_u_arrays,
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
  row_u_arrays <- row_u_arrays[data_rows]
  col_arrays <- col_arrays[data_cols]
  col_u_arrays <- col_u_arrays[data_cols]
  if (!is.null(row_labels)) {
    row_labels <- row_labels[data_rows]
  }
  col_names <- colnames(result_matrix)
  
  # Detect weighting for safer auto selection
  has_weights <- .sig_has_weights(base_array)

  # Determine test to use
  if (config$test == "auto") {
    test_id <- .resolve_auto_sig_test(statistic$id, has_weights)
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
  effect_matrix <- matrix(NA_real_, nrow = n_data_rows, ncol = n_cols)

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
    data_row_u_arrays <- row_u_arrays[array_indices]

    # For omnibus tests, also exclude summary columns
    data_col_arrays <- col_arrays
    data_col_u_arrays <- col_u_arrays
    if (!is.null(statistic$summary_col)) {
      # Find indices of data columns (excluding summary column)
      data_col_indices <- which(col_names != statistic$summary_col)
      data_col_arrays <- col_arrays[data_col_indices]
      data_col_u_arrays <- col_u_arrays[data_col_indices]
    }

    # Run omnibus test once
    test_result <- test_obj$processor(
      base_array = base_array,
      row_arrays = data_row_arrays,
      row_u_arrays = data_row_u_arrays,
      col_arrays = data_col_arrays,
      col_u_arrays = data_col_u_arrays,
      values = values_array
    )

    # For omnibus tests, store the overall p-value
    if (!is.null(test_result$p_value)) {
      # Set all cells to the same p-value (it's an overall test)
      p_matrix[,] <- test_result$p_value

      # Mark all cells as "omnibus" in sig_matrix
      sig_matrix[,] <- if (test_result$p_value < config$level) "omnibus" else ""
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
  base_col_u <- col_u_arrays[[base_col]]

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
          row_m = row_arrays[[array_idx]],
          row_u = row_u_arrays[[array_idx]],
          col_m_1 = base_col_array,
          col_u_1 = base_col_u,
          col_m_2 = col_arrays[[j]],
          col_u_2 = col_u_arrays[[j]],
          values = values_array
        )

        p_matrix[i, j] <- test_result$p_value
        if (!is.null(test_result$effect)) {
          effect_matrix[i, j] <- as.numeric(test_result$effect)
        }
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
        # Determine direction (prefer test-provided effect when available)
        eff <- effect_matrix[i, j]
        if (!is.na(eff) && is.finite(eff)) {
          if (eff > 0) {
            sig_matrix[i, j] <- "higher"
          } else if (eff < 0) {
            sig_matrix[i, j] <- "lower"
          }
        } else {
          # Fallback: compare displayed values
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

# Internal helpers ---------------------------------------------------------------

.sig_has_weights <- function(base_array) {
  if (is.null(base_array) || !is.numeric(base_array)) {
    return(FALSE)
  }
  x <- base_array[!is.na(base_array) & base_array != 0]
  if (length(x) == 0) {
    return(FALSE)
  }
  any(x != 1)
}

.resolve_auto_sig_test <- function(statistic_id, has_weights) {
  if (is.null(statistic_id) || length(statistic_id) != 1 || is.na(statistic_id)) {
    stop("Cannot resolve auto significance test: invalid statistic id")
  }
  
  # Strict defaults: only auto-resolve when the test matches the statistic.
  if (isTRUE(has_weights)) {
    if (identical(statistic_id, "column_pct")) return("z_test_weighted")
    if (identical(statistic_id, "mean")) return("t_test_weighted")
    if (identical(statistic_id, "median")) {
      stop("test = 'auto' is not available for weighted median comparisons. ",
           "Weighted Mann-Whitney is not implemented. ",
           "Choose test = 'mann_whitney' (unweighted) explicitly, or remove weights.")
    }
  } else {
    if (identical(statistic_id, "column_pct")) return("z_test_proportions")
    if (identical(statistic_id, "mean")) return("t_test")
    if (identical(statistic_id, "median")) return("mann_whitney")
  }
  
  if (identical(statistic_id, "count")) {
    stop("test = 'auto' is not available for statistic = 'count'. ",
         "For pairwise proportion tests, use statistic = 'column_pct' with test = 'z_test_proportions' ",
         "(or 'z_test_weighted' when weighted). ",
         "For overall association, use test = 'chi_square'.")
  }
  
  if (identical(statistic_id, "row_pct")) {
    stop("test = 'auto' is not available for statistic = 'row_pct'. ",
         "If you want pairwise comparisons between column groups, use statistic = 'column_pct' ",
         "with an appropriate z-test. ",
         "If you want an overall association test, use test = 'chi_square'.")
  }
  
  if (identical(statistic_id, "sd") || identical(statistic_id, "cv")) {
    stop("test = 'auto' is not available for statistic = '", statistic_id, "'. ",
         "Mean tests (t-test) do not test differences in dispersion. ",
         "Choose an explicit test (none built-in for sd/cv yet), or tab means instead.")
  }
  
  stop("No default significance test for statistic = '", statistic_id, "'. ",
       "Choose an explicit test (see list_tab_significance_tests()).")
}

# Cell-Native Significance Implementation ----------------------------------------

#' Extract result matrix from cell grid without materialization
#' 
#' Reads cell values directly from the grid and builds a numeric matrix
#' without materializing the entire tab_result to data.frame.
#' 
#' @param store A cell_store object
#' @param grid Layout grid matrix containing cell IDs
#' @return Numeric matrix of values matching grid dimensions
#' @keywords internal
extract_result_matrix_from_grid <- function(store, grid) {
  n_rows <- nrow(grid)
  n_cols <- ncol(grid)
  result_matrix <- matrix(NA_real_, n_rows, n_cols)
  
  for (i in seq_len(n_rows)) {
    for (j in seq_len(n_cols)) {
      cell_id <- grid[i, j]
      if (!is.na(cell_id)) {
        cell <- get_cell(store, cell_id)
        if (!is.null(cell)) {
          result_matrix[i, j] <- cell$value
        }
      }
    }
  }
  
  return(result_matrix)
}

#' Identify which rows or columns are meta (summary/base) positions
#' 
#' Checks cells in the grid to identify which rows/columns contain
#' summary rows, summary columns, or base rows that should be excluded
#' from significance testing.
#' 
#' @param store A cell_store object
#' @param grid Layout grid matrix containing cell IDs
#' @param dimension Either "row" or "col"
#' @return Integer vector of indices that are meta positions
#' @keywords internal
identify_meta_indices <- function(store, grid, dimension = c("row", "col")) {
  dimension <- match.arg(dimension)
  
  if (dimension == "row") {
    n <- nrow(grid)
    meta_indices <- integer(0)
    
    # Check first non-NA cell in each row
    for (i in seq_len(n)) {
      # Find first non-NA cell_id in this row
      cell_id <- NA
      for (j in seq_len(ncol(grid))) {
        if (!is.na(grid[i, j])) {
          cell_id <- grid[i, j]
          break
        }
      }
      
      if (!is.na(cell_id)) {
        cell <- get_cell(store, cell_id)
        if (!is.null(cell) && isTRUE(cell$specification$is_summary_row)) {
          meta_indices <- c(meta_indices, i)
        }
      }
    }
  } else {
    n <- ncol(grid)
    meta_indices <- integer(0)
    
    # Check first non-NA cell in each column
    for (j in seq_len(n)) {
      # Find first non-NA cell_id in this column
      cell_id <- NA
      for (i in seq_len(nrow(grid))) {
        if (!is.na(grid[i, j])) {
          cell_id <- grid[i, j]
          break
        }
      }
      
      if (!is.na(cell_id)) {
        cell <- get_cell(store, cell_id)
        if (!is.null(cell) && isTRUE(cell$specification$is_summary_col)) {
          meta_indices <- c(meta_indices, j)
        }
      }
    }
  }
  
  return(meta_indices)
}

#' Build mapping from grid positions to significance matrix positions
#' 
#' Creates an integer vector that maps grid position indices to the
#' corresponding position in the significance matrix, accounting for
#' skipped meta positions.
#' 
#' @param n_positions Total number of positions in grid dimension
#' @param meta_indices Integer vector of positions to skip (meta rows/cols)
#' @return Integer vector where `mapping[grid_pos] = sig_matrix_pos`
#' @keywords internal
build_sig_index_mapping <- function(n_positions, meta_indices) {
  mapping <- integer(n_positions)
  sig_counter <- 1L
  
  for (i in seq_len(n_positions)) {
    if (!(i %in% meta_indices)) {
      mapping[i] <- sig_counter
      sig_counter <- sig_counter + 1L
    }
  }
  
  return(mapping)
}

#' Attach significance results to individual cells in the store
#' 
#' Iterates through the grid and attaches significance metadata to each
#' cell. Cells are modified in-place via environment write-back. Meta
#' positions (summary rows/cols) are skipped.
#' 
#' @param store A cell_store object
#' @param grid Layout grid matrix containing cell IDs
#' @param sig_result Result from compute_significance (list with levels, p_values, etc.)
#' @param test_name Name for this significance test
#' @param meta_row_indices Integer vector of row indices to skip
#' @param meta_col_indices Integer vector of column indices to skip
#' @return NULL (modifies store in place)
#' @keywords internal
attach_sig_to_cells <- function(store, grid, sig_result, test_name,
                                 meta_row_indices, meta_col_indices) {
  # Build index mappings (grid position → sig matrix position)
  grid_to_sig_row <- build_sig_index_mapping(nrow(grid), meta_row_indices)
  grid_to_sig_col <- build_sig_index_mapping(ncol(grid), meta_col_indices)
  
  # Iterate through grid and attach significance
  for (i in seq_len(nrow(grid))) {
    if (i %in% meta_row_indices) next
    
    for (j in seq_len(ncol(grid))) {
      if (j %in% meta_col_indices) next
      
      cell_id <- grid[i, j]
      if (is.na(cell_id)) next
      
      # Get corresponding position in significance matrices
      sig_row <- grid_to_sig_row[i]
      sig_col <- grid_to_sig_col[j]
      
      # Skip if mapping failed (shouldn't happen)
      if (sig_row == 0 || sig_col == 0) next
      
      # Direct cell access and modification
      cell <- store$cells[[cell_id]]
      if (is.null(cell)) next
      
      # Initialize significance list if needed
      if (is.null(cell$significance)) {
        cell$significance <- list()
      }
      
      # Add this test's results
      cell$significance[[test_name]] <- list(
        level = sig_result$levels[sig_row, sig_col],
        p_value = sig_result$p_values[sig_row, sig_col],
        test_used = sig_result$test_used,
        versus = sig_result$versus,
        is_omnibus = sig_result$is_omnibus  # Store is_omnibus flag if present
      )
      
      # Write back to store
      store$cells[[cell_id]] <- cell
    }
  }
  
  invisible(NULL)
}

#' Add significance testing to cell-based tab results (cell-native path)
#' 
#' This function implements the cell-native significance path that avoids
#' materialization to data.frame. Significance metadata is attached directly
#' to individual cells in the store, preserving the cell-based pipeline.
#' 
#' @param tab_result A tab_cell_collection object
#' @param versus Column to compare against
#' @param test Test to use
#' @param level Significance level
#' @param adjust Multiple comparison adjustment
#' @param name Optional name for this comparison
#' @return The tab_result (still tab_cell_collection) with significance attached to cells
#' @keywords internal
add_sig_cell_native <- function(tab_result, versus, test, level, adjust, name) {
  # Extract components from cell-based result
  store <- tab_result$cell_store
  grid <- tab_result$layout$grid
  arrays <- tab_result$arrays
  statistic <- tab_result$statistic
  
  # Check for mixed-statistic tabs (cannot perform significance testing)
  if (is.null(statistic)) {
    stop("Cannot perform significance testing on mixed-statistic tabs. ",
         "This tab was created by gluing tabs with different statistics. ",
         "Apply add_sig() before gluing, or glue tabs with the same statistic.")
  }
  
  # Build result matrix directly from grid (no materialization)
  result_matrix <- extract_result_matrix_from_grid(store, grid)
  
  # Identify meta rows and columns to exclude from significance
  meta_row_indices <- identify_meta_indices(store, grid, "row")
  meta_col_indices <- identify_meta_indices(store, grid, "col")
  
  # Filter result_matrix and arrays to data-only elements
  data_rows <- setdiff(seq_len(nrow(grid)), meta_row_indices)
  data_cols <- setdiff(seq_len(ncol(grid)), meta_col_indices)
  
  result_matrix_data <- result_matrix[data_rows, data_cols, drop = FALSE]
  row_arrays_data <- arrays$row_arrays[data_rows]
  row_u_arrays_data <- arrays$row_u_arrays[data_rows]
  col_arrays_data <- arrays$col_arrays[data_cols]
  col_u_arrays_data <- arrays$col_u_arrays[data_cols]
  
  # Get row labels for data rows
  row_labels_data <- if (!is.null(tab_result$layout$row_labels)) {
    tab_result$layout$row_labels[data_rows]
  } else {
    paste0("Row_", data_rows)
  }
  
  # Get column names for data cols
  col_names_data <- if (!is.null(tab_result$layout$col_labels)) {
    tab_result$layout$col_labels[data_cols]
  } else {
    paste0("Col_", data_cols)
  }
  
  # Set matrix names
  rownames(result_matrix_data) <- row_labels_data
  colnames(result_matrix_data) <- col_names_data
  
  # Create sig_config
  sig_config <- list(
    test = test,
    versus = versus,
    level = level,
    adjust = adjust
  )
  
  # Compute significance using existing function
  sig_result <- compute_significance(
    base_array = arrays$base_array,
    row_arrays = row_arrays_data,
    row_u_arrays = row_u_arrays_data,
    col_arrays = col_arrays_data,
    col_u_arrays = col_u_arrays_data,
    result_matrix = result_matrix_data,
    statistic = statistic,
    values_array = arrays$values_array,
    sig_config = sig_config,
    row_labels = row_labels_data
  )
  
  # Determine test name
  if (is.null(name)) {
    # Resolve versus to column name from data columns
    if (is.character(versus)) {
      name <- switch(versus,
                     "first_col" = col_names_data[1],
                     "last_col" = col_names_data[length(col_names_data)],
                     versus)  # Use as-is if it's already a column name
    } else if (is.numeric(versus)) {
      name <- col_names_data[versus]
    }
  }
  
  # Attach significance to cells
  attach_sig_to_cells(
    store = store,
    grid = grid,
    sig_result = sig_result,
    test_name = name,
    meta_row_indices = meta_row_indices,
    meta_col_indices = meta_col_indices
  )
  
  # Return modified collection (still tab_cell_collection)
  return(tab_result)
}