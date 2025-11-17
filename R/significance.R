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
  
  # Materialize cell-based results first
  if (inherits(tab_result, "tab_cell_collection")) {
    tab_result <- as.data.frame(tab_result)
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

  # Get statistic info to identify summary columns
  statistic <- attr(tab_result, "statistic")

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

  # Get all column names except row_label
  col_names <- names(tab_result)[-1]

  # Remove excluded columns
  col_names <- setdiff(col_names, columns_to_exclude)

  # Only proceed if there are columns left to test
  if (length(col_names) == 0) {
    warning("No columns left to test after excluding summary/base columns")
    return(tab_result)
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