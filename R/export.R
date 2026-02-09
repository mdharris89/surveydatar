# Display and Clipboard Export
#
# Functions for displaying and exporting tab results:
# - print.tab_result: Console display with significance indicators and column letters
# - copy_tab: Copy formatted table to clipboard for pasting into spreadsheets
# - copy_df: Copy any data.frame to clipboard (OS-specific: macOS, Windows)
#
# Display features:
# - Automatic column lettering (A, B, C, ...)
# - Significance notation (e.g., "A+", "B-")
# - Formatted percentages and base counts

#' Print method for tab_result
#' @param x A tab_result object
#' @param label_mode How to display labels: "smart", "full", or "suffix"
#' @param ... Additional arguments (unused)
#' @export
print.tab_result <- function(x, label_mode = "smart", ...) {
  # Materialize cell-based results and apply layout/visibility
  if (inherits(x, "tab_cell_collection")) {
    x <- .materialize_for_export(x, show_base = NULL, label_mode = label_mode)
  } else {
    # Data.frame-based path - apply layout and visibility
    # Apply layout ordering before display
    layout <- attr(x, "layout")
    if (!is.null(layout) && !is.null(layout$row_order)) {
      x <- x[layout$row_order, , drop = FALSE]
    }
    if (!is.null(layout) && !is.null(layout$col_order)) {
      # Reorder columns (keeping row_label first)
      col_order_with_label <- c(1, layout$col_order + 1)
      x <- x[, col_order_with_label, drop = FALSE]
    }

    # Apply visibility filtering before display
    visibility <- attr(x, "visibility")
    if (!is.null(visibility)) {
      if (!is.null(visibility$rows)) {
        x <- x[visibility$rows, , drop = FALSE]
      }
      if (!is.null(visibility$cols)) {
        visible_cols <- c(TRUE, visibility$cols)  # Always keep row_label
        x <- x[, visible_cols, drop = FALSE]
      }
      # Cell-level visibility will be handled during formatting (show as "")
    }
  }

  statistic <- attr(x, "statistic", exact = TRUE)
  statistic_matrix <- attr(x, "statistic_matrix")  # For mixed-statistic tabs
  is_mixed_stat <- is.null(statistic)

  # Get values variable name if available
  values_var <- attr(x, "values_variable")

  # Get all significance results
  all_sig <- attr(x, "significance")

  # Construct header
  if (is_mixed_stat) {
    cat("\nCross-tabulation (mixed statistics)\n", sep = "")
  } else {
    # Defensive check for atomic statistic
    if (!is.list(statistic)) {
      cat("\nCross-tabulation (", as.character(statistic), ")\n", sep = "")
    } else if (statistic$id == "mean" && !is.null(values_var)) {
      cat("\nCross-tabulation (mean of ", values_var, ")\n", sep = "")
    } else {
      cat("\nCross-tabulation (", statistic$id, ")\n", sep = "")
    }
  }
  cat(rep("-", 50), "\n", sep = "")

  # For mixed-stat tabs, get statistic info from first cell for base_label
  if (is_mixed_stat) {
    stat_info <- .extract_stat_info_safe(x)
    statistic <- stat_info$stat  # Use first cell's statistic for structure
  }

  # Create column letter mapping
  col_names <- names(x)[-1]

  # Defensive check for base_label access
  base_label <- if (is.list(statistic)) statistic$base_label else "Base (n)"
  non_base_cols <- col_names[col_names != base_label]

  col_letters <- LETTERS[seq_along(non_base_cols)]
  names(col_letters) <- non_base_cols

  # Create a copy for formatting to avoid modifying the original
  x_formatted        <- x
  base_row_idx <- which(x_formatted$row_label == base_label)

  # Identify data rows (exclude base and summary rows) for proper indexing
  exclude_row_labels <- c(base_label)
  if (is.list(statistic) && !is.null(statistic$summary_row)) {
    exclude_row_labels <- c(exclude_row_labels, statistic$summary_row)
  }
  data_row_mask <- !(x$row_label %in% exclude_row_labels)

  for (col in names(x_formatted)[-1]) {
    orig <- x[[col]]
    col_idx <- which(names(x)[-1] == col)

    # Check if this column is the base column
    base_orientation <- attr(x_formatted, "base_orientation")
    is_base_column <- FALSE
    if (!is.null(base_orientation) && base_orientation == "row") {
      # If base is displayed as a column (row orientation), check if this is it
      is_base_column <- col == base_label
    }
    if (is_base_column) {
      next  # Skip formatting for base column
    }

    x_formatted[[col]] <- vapply(seq_along(orig), function(i) {
      if (i %in% base_row_idx || is_base_column) {
        as.character(orig[i])
      } else if (is.numeric(orig[i]) && !is.na(orig[i])) {
        # For mixed-statistic tabs, lookup cell-specific statistic
        if (is_mixed_stat && !is.null(statistic_matrix)) {
          stat_id <- statistic_matrix[i, col_idx]
          if (!is.na(stat_id)) {
            cell_stat <- get_statistic(stat_id)
            if (!is.null(cell_stat)) {
              formatted_val <- cell_stat$format_fn(orig[i])
            } else {
              formatted_val <- as.character(orig[i])
            }
          } else {
            formatted_val <- as.character(orig[i])
          }
        } else {
          # Defensive check
          if (is.list(statistic)) {
            formatted_val <- statistic$format_fn(orig[i])
          } else {
            formatted_val <- as.character(orig[i])
          }
        }

        # Add significance indicators from all tests
        if (!is.null(all_sig) && data_row_mask[i]) {
          sig_indicators <- character()

          # The sig matrix only contains data rows, so we need to count how many data rows come before the current row
          sig_row_idx <- sum(data_row_mask[1:i])

          for (test_name in names(all_sig)) {
            sig_result <- all_sig[[test_name]]

            if (!is.null(sig_result$levels) &&
                is.matrix(sig_result$levels) &&
                sig_row_idx <= nrow(sig_result$levels) &&
                col_idx <= ncol(sig_result$levels)) {

              sig_level <- sig_result$levels[sig_row_idx, col_idx]

              if (!is.na(sig_level) && nzchar(sig_level)) {
                if (sig_level == "higher") {
                  ref_col_name <- test_name

                  if (!ref_col_name %in% names(col_letters) && !is.null(sig_result$versus)) {
                    # Extract column name from "column N: Name" format
                    if (grepl("^column \\d+: ", sig_result$versus)) {
                      ref_col_name <- sub("^column \\d+: ", "", sig_result$versus)
                    }
                  }

                  if (ref_col_name %in% names(col_letters)) {
                    sig_indicators <- c(sig_indicators,
                                        paste0(col_letters[ref_col_name], "+"))
                  }
                } else if (sig_level == "lower") {
                  ref_col_name <- test_name

                  if (!ref_col_name %in% names(col_letters) && !is.null(sig_result$versus)) {
                    if (grepl("^column \\d+: ", sig_result$versus)) {
                      ref_col_name <- sub("^column \\d+: ", "", sig_result$versus)
                    }
                  }

                  if (ref_col_name %in% names(col_letters)) {
                    sig_indicators <- c(sig_indicators,
                                        paste0(col_letters[ref_col_name], "-"))
                  }
                }
              }
            }
          }

          if (length(sig_indicators) > 0) {
            formatted_val <- paste0(formatted_val, " (",
                                    paste(sig_indicators, collapse = ","), ")")
          }
        }
        formatted_val
      } else {
        as.character(orig[i])
      }
    }, character(1))
  }

  # Update column names to include letters in brackets (after formatting is complete)
  letter_index <- 1
  for (i in seq_along(col_names)) {
    col_name <- col_names[i]
    if (col_name == base_label) {
      # Don't add letter to base column
      new_col_name <- col_name
    } else {
      # Add letter for non-base columns
      new_col_name <- paste0(col_name, " (", LETTERS[letter_index], ")")
      letter_index <- letter_index + 1
    }
    names(x_formatted)[i + 1] <- new_col_name
  }

  print.data.frame(x_formatted, row.names = FALSE, ...)

  # Print significance testing legend if available
  if (!is.null(all_sig)) {
    cat("\nSignificance testing:\n")
    cat("Column letters: ", paste(col_letters, "=", names(col_letters), collapse = ", "), "\n")

    for (test_name in names(all_sig)) {
      sig_result <- all_sig[[test_name]]
      cat("\n", test_name, ":\n", sep = "")
      cat("  Test: ", sig_result$test_name, "\n")
      cat("  Level: ", sig_result$config$level, "\n")
      if (!is.null(sig_result$config$adjust) && sig_result$config$adjust != "none") {
        cat("  Adjustment: ", sig_result$config$adjust, "\n")
      }
    }

    cat("\nNotation: X+ = significantly higher than column X, X- = significantly lower than column X\n")
  }
}

#' Copy tab results to clipboard for pasting into Sheets
#'
#' Copies a tab_result object to the clipboard in a format suitable for pasting
#' into Sheets. Percentages are converted to decimals, base sizes are
#' included, and source information is added.
#'
#' @param tab_result A tab_result object from the tab() function
#' @param digits Number of decimal places to round values to. If NULL (default), no rounding is applied
#' @param empty_zeros Logical. If TRUE, cells with exactly 0 values will be displayed as empty instead of "0"
#' @param na_display Character string to display for NA values. Default is empty string ("")
#' @param show_source Logical. If TRUE (default), includes footnote on source.
#' @param label_mode How to display labels: "smart", "full", or "suffix"
#' @return Invisibly returns the formatted data that was copied
#' @export
#'
#' @examples
#' \dontrun{
#' result <- tab(data, gender, region)
#' copy_tab(result)
#' # Now paste into Sheets
#' }
copy_tab <- function(tab_result, digits = NULL, empty_zeros = FALSE, na_display = "", show_source = TRUE, label_mode = "full") {

  # Validate input
  if (!inherits(tab_result, "tab_result")) {
    stop("Input must be a tab_result object from the tab() function")
  }
  
  # Apply native operations if cell-based, then materialize
  if (inherits(tab_result, "tab_cell_collection")) {
    # Materialize with class preservation (show_base = TRUE to include base)
    tab_result <- .materialize_for_export(tab_result, show_base = TRUE, label_mode = label_mode)
  } else {
    # Data.frame-based path - apply layout and visibility
    # Apply layout ordering before export
    layout <- attr(tab_result, "layout")
    if (!is.null(layout) && !is.null(layout$row_order)) {
      tab_result <- tab_result[layout$row_order, , drop = FALSE]
    }
    if (!is.null(layout) && !is.null(layout$col_order)) {
      col_order_with_label <- c(1, layout$col_order + 1)
      tab_result <- tab_result[, col_order_with_label, drop = FALSE]
    }
    
    # Apply visibility filtering before export
    visibility <- attr(tab_result, "visibility")
    if (!is.null(visibility)) {
      if (!is.null(visibility$rows)) {
        tab_result <- tab_result[visibility$rows, , drop = FALSE]
      }
      if (!is.null(visibility$cols)) {
        visible_cols <- c(TRUE, visibility$cols)
        tab_result <- tab_result[, visible_cols, drop = FALSE]
      }
    }
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
  statistic <- attr(tab_result, "statistic", exact = TRUE)
  statistic_matrix <- attr(tab_result, "statistic_matrix")  # For mixed-statistic tabs
  is_mixed_stat <- is.null(statistic)
  
  # For mixed-stat tabs, get statistic info from first cell for base_label
  if (is_mixed_stat) {
    stat_info <- .extract_stat_info_safe(tab_result)
    statistic <- stat_info$stat  # Use first cell's statistic for structure
  }

  # Create a copy of the data for processing
  output_data <- tab_result

  x_formatted <- output_data

  # Identify the base row
  base_label <- if (is.list(statistic)) statistic$base_label else "Base (n)"
  base_row_idx <- which(x_formatted$row_label == base_label)

  # Apply formatting function to all non-base rows
  # Apply formatting function to all non-base rows
  for (col in names(output_data)[-1]) {
    col_idx <- which(names(output_data)[-1] == col)
    
    # Check if base column
    is_base_column <- col == base_label
    if (is_base_column) {
      next
    }

    original_values <- output_data[[col]]
    output_data[[col]] <- vapply(
      seq_along(original_values), function(i) {
        if (i %in% base_row_idx) {
          # Handle base row values
          val <- original_values[i]
          if (is.na(val)) {
            return(na_display)
          } else if (empty_zeros && val == 0) {
            return("")
          } else if (!is.null(digits)) {
            # Round base values too, format as integer if digits = 0
            rounded_val <- round(val, digits)
            if (digits == 0) {
              return(as.character(as.integer(rounded_val)))
            } else {
              return(as.character(rounded_val))
            }
          } else {
            return(as.character(val))
          }
        } else if (is.numeric(original_values[i])) {
          val <- original_values[i]
          if (is.na(val)) {
            return(na_display)
          } else if (empty_zeros && val == 0) {
            return("")
          } else {
            # Handle custom formatting or use statistic's format function
            if (!is.null(digits)) {
              rounded_val <- round(val, digits)
              if (digits == 0) {
                return(as.character(as.integer(rounded_val)))
              } else {
                # Format without trailing zeros
                return(as.character(rounded_val))
              }
            } else {
              # For mixed-statistic tabs, lookup cell-specific statistic
              if (is_mixed_stat && !is.null(statistic_matrix)) {
                stat_id <- statistic_matrix[i, col_idx]
                if (!is.na(stat_id)) {
                  cell_stat <- get_statistic(stat_id)
                  if (!is.null(cell_stat)) {
                    return(cell_stat$format_fn(val))
                  }
                }
              }
              
              if (is.list(statistic)) {
                return(statistic$format_fn(val))
              } else {
                return(as.character(val))
              }
            }
          }
        } else {
          return(as.character(original_values[i]))
        }
      }, character(1))
  }

  # Create column headers row
  col_names <- names(output_data)[-1]
  if (anyDuplicated(col_names) > 0L) {
    warning("Duplicate column names detected. Header row will be built positionally; ",
            "consider using glue_tab(..., prefix = c('tab1', 'tab2')) to disambiguate.")
  }
  
  headers_values <- c("Row Label", col_names)
  headers_row <- as.data.frame(
    as.list(headers_values),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  names(headers_row) <- names(output_data)

  # Combine headers with data (base row is already included in tab_result)
  output_data <- rbind(headers_row, output_data)

  if(show_source){
    # Add empty row for spacing
    empty_row <- output_data[1, ]
    empty_row[1, ] <- ""
    output_data <- rbind(output_data, empty_row)

    # Add source information row with original call
    if (is.list(statistic) && statistic$id == "mean" && !is.null(attr(tab_result, "values_variable"))) {
      values_var <- attr(tab_result, "values_variable")
      call_text <- deparse(original_call, width.cutoff = 500)
      source_info <- paste0("Table showing mean of '", values_var, "' generated by surveydatar::", paste(call_text, collapse = " "), ")")
    } else {
      call_text <- deparse(original_call, width.cutoff = 500)
      stat_id <- if (is.list(statistic)) statistic$id else as.character(statistic)
      source_info <- paste0("Table showing survey ", stat_id, " data generated by surveydatar::", paste(call_text, collapse = " "), ")")
    }

    source_row <- output_data[1, ]
    source_row[1, ] <- source_info
    source_row[, -1] <- ""
    output_data <- rbind(output_data, source_row)
  }

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
    message("Table copied to clipboard. Ready to paste into Sheets.")
  }, error = function(e) {
    stop("Failed to copy to clipboard: ", e$message)
  })

  # Return the formatted data invisibly
  invisible(output_data)
}

#' Copy a data.frame to the system clipboard
#'
#' Convenience helper for copying a data.frame (often derived from a `tab_result`)
#' to the clipboard for pasting into spreadsheet tools.
#'
#' On macOS this uses `pbcopy`. On Windows it uses the `"clipboard"` connection.
#'
#' @param df A data.frame to copy.
#' @param row.names Logical; include row names (default FALSE).
#' @param sep Field separator (default tab).
#' @return Invisibly returns `df`.
#' @export
#' @examples
#' \dontrun{
#' copy_df(head(mtcars))
#' }
copy_df <- function(df, row.names = FALSE, sep = "\t") {
  # Ensure input is a data frame
  if (!is.data.frame(df)) {
    stop("Input must be a data frame")
  }

  # OS-specific behavior
  os <- Sys.info()[["sysname"]]

  if (os == "Windows") {
    utils::write.table(df, "clipboard", sep = sep, row.names = row.names, col.names = TRUE, quote = FALSE)
    message("Data copied to Windows clipboard.")
  } else if (os == "Darwin") {  # macOS
    clip <- pipe("pbcopy", "w")
    utils::write.table(df, file = clip, sep = sep, row.names = row.names, col.names = TRUE, quote = FALSE)
    close(clip)
    message("Data copied to clipboard.")
  } else {
    stop("Clipboard functionality not implemented on this OS.")
  }
  invisible(df)
}

##### Export Helper Functions #####
#' Functions for manipulating tab_result visibility and exporting to various formats.

#' Hide base row and/or column
#'
#' Removes the base (sample size) row and/or column from the visible grid.
#' Base values remain in cell metadata and can be restored with show_base().
#'
#' @param tab_result A tab_result object
#' @param rows Logical, hide base row if present (default TRUE)
#' @param cols Logical, hide base column if present (default TRUE)
#' @return Modified tab_result with base hidden
#' @export
#' @examples
#' # Hide base in tab results
#' dat <- get_basic_test_dat()
#' tab(dat, labelledordinal, binarycategoricalasfactor) %>%
#'   hide_base()  # Hide both base row and column
#'   
#' tab(dat, labelledordinal, binarycategoricalasfactor) %>%
#'   hide_base(rows = TRUE, cols = FALSE)  # Hide only base row
hide_base <- function(tab_result, rows = TRUE, cols = TRUE) {
  if (!inherits(tab_result, "tab_result")) {
    stop("hide_base() requires a tab_result object")
  }
  
  # Get statistic info (handles mixed-statistic tabs)
  stat_info <- tryCatch(
    .extract_stat_info_safe(tab_result),
    error = function(e) NULL
  )
  
  if (is.null(stat_info) || is.null(stat_info$base_label) || length(stat_info$base_label) == 0) {
    return(tab_result)  # No base to hide
  }
  
  base_label <- stat_info$base_label
  
  # Cell-based path - use filter_rules or hide_rows/hide_cols
  if (inherits(tab_result, "tab_cell_collection")) {
    # Use hide_rows and hide_cols which now use filter_rules
    if (rows && base_label %in% sapply(tab_result$layout$row_defs, `[[`, "label")) {
      tab_result <- hide_rows(tab_result, base_label)
    }
    if (cols && base_label %in% sapply(tab_result$layout$col_defs, `[[`, "label")) {
      tab_result <- hide_cols(tab_result, base_label)
    }
    
    # Update show_base flag so as.data.frame doesn't re-add base
    tab_result$show_base <- FALSE
    
    return(tab_result)
  }
  
  # Data.frame-based path - use hide_rows/hide_cols
  if (rows && base_label %in% tab_result$row_label) {
    tab_result <- hide_rows(tab_result, base_label)
  }
  if (cols && base_label %in% names(tab_result)) {
    tab_result <- hide_cols(tab_result, base_label)
  }
  
  # Update show_base flag for data.frame-based results too
  attr(tab_result, "show_base") <- FALSE
  
  return(tab_result)
}

#' Show base row or column
#'
#' Adds a base (sample size) row or column to the grid. Computes base
#' values from existing cells. Orientation is automatically determined
#' from the base variation pattern, or can be specified explicitly.
#'
#' @param tab_result A tab_result object
#' @param orientation Where to show base: "auto" (default), "row", or "column"
#' @return Modified tab_result with base added
#' @export
#' @examples
#' # Show base in tab results
#' dat <- get_basic_test_dat()
#' tab(dat, labelledordinal, binarycategoricalasfactor, show_base = FALSE) %>%
#'   show_base()  # Add base back with automatic orientation
#'   
#' tab(dat, labelledordinal, binarycategoricalasfactor, show_base = FALSE) %>%
#'   show_base(orientation = "row")  # Force base as row
show_base <- function(tab_result, orientation = c("auto", "row", "column")) {
  if (!inherits(tab_result, "tab_result")) {
    stop("show_base() requires a tab_result object")
  }
  
  orientation <- match.arg(orientation)
  
  # Get statistic info (handles mixed-statistic tabs)
  stat_info <- tryCatch(
    .extract_stat_info_safe(tab_result),
    error = function(e) NULL
  )
  
  if (is.null(stat_info) || is.null(stat_info$base_label) || length(stat_info$base_label) == 0) {
    stop("Cannot show base: statistic has no base_label")
  }
  
  base_label <- stat_info$base_label
  
  # Cell-based path
  if (inherits(tab_result, "tab_cell_collection")) {
    # Check if base already visible
    if (base_label %in% tab_result$layout$row_labels ||
        base_label %in% tab_result$layout$col_labels) {
      message("Base already visible")
      return(tab_result)
    }
    
    grid <- tab_result$layout$grid
    
    # Extract base values from cells
    base_matrix <- matrix(NA_real_, nrow(grid), ncol(grid))
    for (i in seq_len(nrow(grid))) {
      for (j in seq_len(ncol(grid))) {
        cell_id <- grid[i, j]
        if (!is.na(cell_id) && has_cell(tab_result$cell_store, cell_id)) {
          cell <- get_cell(tab_result$cell_store, cell_id)
          base_matrix[i, j] <- cell$base
        }
      }
    }
    
    # Determine orientation
    if (orientation == "auto") {
      # Check if bases constant within columns (show as row)
      # or constant within rows (show as column)
      constant_within_cols <- TRUE
      constant_within_rows <- TRUE
      
      if (ncol(base_matrix) > 1) {
        for (j in seq_len(ncol(base_matrix))) {
          col_bases <- base_matrix[, j]
          col_bases <- col_bases[!is.na(col_bases)]
          if (length(unique(col_bases)) > 1) {
            constant_within_cols <- FALSE
            break
          }
        }
      }
      
      if (nrow(base_matrix) > 1) {
        for (i in seq_len(nrow(base_matrix))) {
          row_bases <- base_matrix[i, ]
          row_bases <- row_bases[!is.na(row_bases)]
          if (length(unique(row_bases)) > 1) {
            constant_within_rows <- FALSE
            break
          }
        }
      }
      
      # Decide orientation
      if (constant_within_cols && !constant_within_rows) {
        orientation <- "row"
      } else {
        orientation <- "column"  # Default
      }
    }
    
    # Store base information for materialization
    attr(tab_result, "base_matrix") <- base_matrix
    attr(tab_result, "base_orientation") <- orientation
    attr(tab_result, "show_base_pending") <- TRUE
    
    # Update show_base flag so as.data.frame knows to include base
    tab_result$show_base <- TRUE
    
    # Note: Actual base row/column will be added during materialization
    # We can't add it to the grid because base isn't computed from cells,
    # it IS the cells' metadata
    
    return(tab_result)
  }
  
  # Data.frame-based path - base already materialized or not available
  if (base_label %in% tab_result$row_label || base_label %in% names(tab_result)) {
    message("Base already visible")
    return(tab_result)
  }
  
  warning("show_base() not fully supported for data.frame tab_results. Use show_base = TRUE when creating tab.")
  return(tab_result)
}

#' Hide summary rows and/or columns
#'
#' Removes summary rows (NET, Avg) and/or columns (Total, NET) from the
#' visible grid. Summary cells remain in cell_store and can be restored
#' with show_summary().
#'
#' @param tab_result A tab_result object
#' @param rows Logical, hide summary rows if present (default TRUE)
#' @param cols Logical, hide summary columns if present (default TRUE)
#' @return Modified tab_result with summaries hidden
#' @export
#' @examples
#' # Hide summary rows/columns
#' dat <- get_basic_test_dat()
#' tab(dat, labelledordinal, binarycategoricalasfactor, show_col_nets = TRUE) %>%
#'   hide_summary(cols = TRUE)  # Hide "Total" column
#'   
#' tab(dat, labelledordinal, binarycategoricalasfactor) %>%
#'   hide_summary()  # Hide both summary row and column if present
hide_summary <- function(tab_result, rows = TRUE, cols = TRUE) {
  if (!inherits(tab_result, "tab_result")) {
    stop("hide_summary() requires a tab_result object")
  }
  
  # Get statistic info (handles mixed-statistic tabs)
  stat_info <- tryCatch(
    .extract_stat_info_safe(tab_result),
    error = function(e) NULL
  )
  
  # Cell-based path - physically remove row_defs/col_defs
  if (inherits(tab_result, "tab_cell_collection")) {
    layout <- tab_result$layout
    needs_refresh <- FALSE
    
    # Remove summary row_defs if requested
    if (rows && isTRUE(layout$has_summary_row)) {
      # Find and remove row_defs with is_summary_row_matcher
      tab_result$layout$row_defs <- Filter(function(row_def) {
        is.null(row_def$is_summary_row_matcher)
      }, tab_result$layout$row_defs)
      
      # Update metadata
      tab_result$layout$has_summary_row <- FALSE
      needs_refresh <- TRUE
    }
    
    # Remove summary col_defs if requested
    if (cols && isTRUE(layout$has_summary_col)) {
      # Find and remove col_defs with is_summary_col_matcher
      tab_result$layout$col_defs <- Filter(function(col_def) {
        is.null(col_def$is_summary_col_matcher)
      }, tab_result$layout$col_defs)
      
      # Update metadata
      tab_result$layout$has_summary_col <- FALSE
      needs_refresh <- TRUE
    }
    
    # Refresh layout to reallocate grid with new dimensions
    if (needs_refresh) {
      tab_result <- refresh_layout(tab_result)
    }
    
    return(tab_result)
  }
  
  # Data.frame-based path
  # Check if we have statistic info
  if (is.null(stat_info)) {
    warning("Cannot hide summary: no statistic information available")
    return(tab_result)
  }
  
  if (rows && !is.null(stat_info$summary_row)) {
    if (stat_info$summary_row %in% tab_result$row_label) {
      tab_result <- hide_rows(tab_result, stat_info$summary_row)
    }
  }
  
  if (cols && !is.null(stat_info$summary_col)) {
    if (stat_info$summary_col %in% names(tab_result)) {
      tab_result <- hide_cols(tab_result, stat_info$summary_col)
    }
  }
  
  return(tab_result)
}

#' Materialize cell-based tab_result for export
#'
#' Simplified materialization that takes the grid as-is and converts to
#' data.frame. Always preserves tab_result class. Used by export functions
#' after they've applied filtering/ordering to the grid.
#'
#' @param tab_result A cell-based tab_result object
#' @param show_base Whether to add base row/column (default NULL = use stored value)
#' @param label_mode Label display mode (default NULL = use stored value)
#' @return data.frame with tab_result class and all attributes preserved
#' @keywords internal
.materialize_for_export <- function(tab_result, show_base = NULL, label_mode = NULL) {
  # This is essentially a streamlined version of as.data.frame.tab_result
  # that assumes the grid is already in its final state
  
  if (!inherits(tab_result, "tab_cell_collection")) {
    # Already materialized
    return(tab_result)
  }
  
  # Use standard as.data.frame (now preserves class thanks to our fix)
  # But pass show_base parameter
  df <- as.data.frame(tab_result, show_base = show_base, label_mode = label_mode)
  
  # Ensure class is preserved (redundant after our fix, but defensive)
  if (!"tab_result" %in% class(df)) {
    class(df) <- c("tab_result", class(df))
  }
  
  return(df)
}

#' Safely extract statistic info from tab_result
#'
#' Works with both cell-based and data.frame tab_results. Returns
#' consistent structure with safe defaults for missing attributes.
#' For mixed-statistic tabs (NULL statistic), extracts from first cell.
#'
#' @param tab_result A tab_result object (cell-based or data.frame)
#' @return List with id, base_label, summary_row, summary_col, stat, is_mixed
#' @keywords internal
.extract_stat_info_safe <- function(tab_result) {
  # For cell-based tab_results, statistic is a list element
  # For data.frame tab_results, it's an attribute
  if (inherits(tab_result, "tab_cell_collection")) {
    stat <- tab_result$statistic
  } else {
    stat <- attr(tab_result, "statistic", exact = TRUE)
  }
  
  # Handle mixed-statistic tabs (NULL statistic from gluing different statistics)
  if (is.null(stat)) {
    # Get statistic from first cell for fallback info
    if (inherits(tab_result, "tab_cell_collection")) {
      # Cell-based - read from first cell
      grid <- tab_result$layout$grid
      if (!is.null(grid) && length(grid) > 0) {
        first_cell_id <- grid[which(!is.na(grid))[1]]
        if (!is.na(first_cell_id)) {
          cell <- get_cell(tab_result$cell_store, first_cell_id)
          if (!is.null(cell) && !is.null(cell$computation$statistic)) {
            stat <- get_statistic(cell$computation$statistic)
          }
        }
      }
    } else {
      # Materialized data.frame - use statistic_matrix if available
      stat_matrix <- attr(tab_result, "statistic_matrix")
      if (!is.null(stat_matrix) && length(stat_matrix) > 0) {
        # Get first non-NA statistic ID from matrix
        first_stat_id <- stat_matrix[which(!is.na(stat_matrix))[1]]
        if (!is.na(first_stat_id)) {
          stat <- get_statistic(first_stat_id)
        }
      }
    }
    # If still NULL, cannot proceed
    if (is.null(stat)) {
      stop("tab_result has no statistic information and could not extract from cells")
    }
    is_mixed <- TRUE
  } else {
    is_mixed <- FALSE
  }
  
  list(
    stat = stat,
    id = stat$id,
    base_label = stat$base_label %||% character(0),
    summary_row = stat$summary_row,
    summary_col = stat$summary_col,
    is_mixed = is_mixed
  )
}

#' Get grid dimensions without materializing
#'
#' Returns effective dimensions accounting for what should be excluded.
#' Works with both cell-based (reads grid) and data.frame (counts rows/cols).
#'
#' @param tab_result A tab_result object
#' @param exclude_base Whether to exclude base from count (default FALSE)
#' @param exclude_summary_rows Whether to exclude summary rows (default FALSE)
#' @param exclude_summary_cols Whether to exclude summary cols (default FALSE)
#' @return List with nrow and ncol
#' @keywords internal
.get_grid_dimensions <- function(tab_result, 
                                 exclude_base = FALSE,
                                 exclude_summary_rows = FALSE,
                                 exclude_summary_cols = FALSE) {
  
  stat_info <- .extract_stat_info_safe(tab_result)
  
  if (inherits(tab_result, "tab_cell_collection")) {
    # Cell-based - read from grid
    nrow <- nrow(tab_result$layout$grid)
    ncol <- ncol(tab_result$layout$grid)
    
    # Adjust for exclusions
    if (exclude_base) {
      base_label <- stat_info$base_label
      if (length(base_label) > 0) {
        if (base_label %in% tab_result$layout$row_labels) nrow <- nrow - 1
        if (base_label %in% tab_result$layout$col_labels) ncol <- ncol - 1
      }
    }
    
    if (exclude_summary_rows && isTRUE(tab_result$layout$has_summary_row)) {
      nrow <- nrow - 1
    }
    
    if (exclude_summary_cols && isTRUE(tab_result$layout$has_summary_col)) {
      ncol <- ncol - 1
    }
    
  } else {
    # Data.frame - count rows and cols
    nrow <- nrow(tab_result)
    ncol <- ncol(tab_result) - 1  # Exclude row_label column
    
    # Adjust for exclusions
    if (exclude_base) {
      base_label <- stat_info$base_label
      if (length(base_label) > 0) {
        if (base_label %in% tab_result$row_label) nrow <- nrow - 1
        if (base_label %in% names(tab_result)) ncol <- ncol - 1
      }
    }
    
    if (exclude_summary_rows) {
      summary_label <- stat_info$summary_row
      if (!is.null(summary_label) && summary_label %in% tab_result$row_label) {
        nrow <- nrow - 1
      }
    }
    
    if (exclude_summary_cols) {
      summary_label <- stat_info$summary_col
      if (!is.null(summary_label) && summary_label %in% names(tab_result)) {
        ncol <- ncol - 1
      }
    }
  }
  
  list(nrow = nrow, ncol = ncol)
}

##### Format Operations for Tab Results #####
#' Functions that control cell value formatting for display.
#' Operations manipulate metadata; formatting applied at display/export time.

#' Set global cell formatting options
#'
#' Controls how all cells are formatted for display.
#'
#' @param tab_result A tab_result object
#' @param digits Number of decimal places (NULL to use statistic default)
#' @param empty_zeros Logical, show empty string for zero values
#' @param na_display String to display for NA values
#' @return Modified tab_result with updated formatting metadata
#' @export
#' @examples
#' # Format cells in tab results
#' dat <- get_basic_test_dat()
#' result <- tab(dat, labelledordinal, binarycategoricalasfactor) %>%
#'   format_cells(digits = 1, empty_zeros = TRUE)
format_cells <- function(tab_result, digits = NULL, empty_zeros = FALSE, na_display = "NA") {
  if (!inherits(tab_result, "tab_result")) {
    stop("format_cells() requires a tab_result object")
  }

  formatting <- attr(tab_result, "formatting")
  if (is.null(formatting)) {
    formatting <- list()
  }

  formatting$digits <- digits
  formatting$empty_zeros <- empty_zeros
  formatting$na_display <- na_display

  attr(tab_result, "formatting") <- formatting

  return(tab_result)
}

#' Set custom format function for a specific row
#'
#' @param tab_result A tab_result object
#' @param row_label Row label to format
#' @param format_fn Function that takes a numeric value and returns a formatted string
#' @return Modified tab_result with updated formatting metadata
#' @export
#' @examples
#' \dontrun{
#' result <- tab(data, satisfaction, gender) %>%
#'   format_row("Satisfied", function(x) sprintf("%.1f", x))
#' }
format_row <- function(tab_result, row_label, format_fn) {
  if (!inherits(tab_result, "tab_result")) {
    stop("format_row() requires a tab_result object")
  }

  if (!is.function(format_fn)) {
    stop("format_fn must be a function")
  }

  row_labels <- tab_result$row_label
  row_idx <- match(row_label, row_labels)

  if (is.na(row_idx)) {
    stop("Row '", row_label, "' not found")
  }

  formatting <- attr(tab_result, "formatting")
  if (is.null(formatting)) {
    formatting <- list()
  }

  if (is.null(formatting$custom_formats)) {
    formatting$custom_formats <- list()
  }

  if (is.null(formatting$custom_formats$rows)) {
    formatting$custom_formats$rows <- list()
  }

  formatting$custom_formats$rows[[as.character(row_idx)]] <- format_fn

  attr(tab_result, "formatting") <- formatting

  return(tab_result)
}

#' Set custom format function for a specific column
#'
#' @param tab_result A tab_result object
#' @param col_label Column name to format
#' @param format_fn Function that takes a numeric value and returns a formatted string
#' @return Modified tab_result with updated formatting metadata
#' @export
#' @examples
#' # Format specific column
#' dat <- get_basic_test_dat()
#' result <- tab(dat, labelledordinal, binarycategoricalasfactor) %>%
#'   format_col("Yes", function(x) sprintf("%.0f%%", x))
format_col <- function(tab_result, col_label, format_fn) {
  if (!inherits(tab_result, "tab_result")) {
    stop("format_col() requires a tab_result object")
  }

  if (!is.function(format_fn)) {
    stop("format_fn must be a function")
  }

  col_names <- names(tab_result)[-1]
  col_idx <- match(col_label, col_names)

  if (is.na(col_idx)) {
    stop("Column '", col_label, "' not found")
  }

  formatting <- attr(tab_result, "formatting")
  if (is.null(formatting)) {
    formatting <- list()
  }

  if (is.null(formatting$custom_formats)) {
    formatting$custom_formats <- list()
  }

  if (is.null(formatting$custom_formats$cols)) {
    formatting$custom_formats$cols <- list()
  }

  formatting$custom_formats$cols[[as.character(col_idx)]] <- format_fn

  attr(tab_result, "formatting") <- formatting

  return(tab_result)
}

##### Export Methods for Cell-Based Tab Results ######
#' Methods to materialize cell-based tab_result objects into data.frame.

#' Convert cell-based tab_result to data.frame
#'
#' Materializes the cell collection into a standard data.frame by reading
#' values from cells according to the layout grid arrangement.
#'
#' @param x A tab_result object (cell-based)
#' @param row.names Optional row names (usually NULL)
#' @param optional Unused (for S3 method compatibility)
#' @param include_metadata Attach full cell objects as attribute (default FALSE)
#' @param show_base Whether to display Base row/column showing sample sizes (default NULL, uses value from tab_result)
#' @param label_mode How to display labels: "smart" (suffix for multi-item, full for single-item), 
#'   "full" (complete variable label), or "suffix" (extract suffix after separator). 
#'   If NULL, uses value stored in tab_result object (default "smart").
#' @param ... Additional arguments
#' @return A data.frame with values from the layout grid
#' @export
#' @exportS3Method as.data.frame tab_result
#' @examples
#' \dontrun{
#' result <- tab2(data, satisfaction, gender)
#' df <- as.data.frame(result)
#' 
#' # Override label mode at display time
#' df_full <- as.data.frame(result, label_mode = "full")
#' df_suffix <- as.data.frame(result, label_mode = "suffix")
#' 
#' # With metadata attached:
#' df <- as.data.frame(result, include_metadata = TRUE)
#' cell <- attr(df, "cell_metadata")[[1, 1]]
#' }
as.data.frame.tab_result <- function(x, 
                                     row.names = NULL,
                                     optional = FALSE,
                                     include_metadata = FALSE,
                                     show_base = NULL,
                                     label_mode = NULL,
                                     ...) {
  # Check if this is already a materialized data.frame (from a previous as.data.frame call)
  if (inherits(x, "data.frame") && !inherits(x, "tab_cell_collection") && 
      isTRUE(attr(x, "cell_based_origin"))) {
    # Already materialized - just return it
    return(x)
  }
  
  # Ensure this is a cell-based tab_result
  if (!inherits(x, "tab_cell_collection") || is.null(x$cell_store)) {
    stop("as.data.frame.tab_result() requires a cell-based tab_result object")
  }
  
  # Determine show_base value: use parameter if provided, otherwise use stored value, default to TRUE
  if (is.null(show_base)) {
    show_base <- x$show_base
    if (is.null(show_base)) {
      show_base <- TRUE
    }
  }
  
  # Determine effective label_mode: use parameter if provided, otherwise use stored value, default to "smart"
  stored_mode <- x$label_mode
  if (is.null(stored_mode)) {
    stored_mode <- "smart"
  }
  
  effective_mode <- if (!is.null(label_mode)) {
    label_mode  # Explicit override
  } else {
    stored_mode  # Use what was stored during tab() creation
  }
  
  # Cell-based tab_result - materialize from cells
  grid <- x$layout$grid
  
  if (is.null(grid)) {
    stop("Layout grid not available - tab_result may be incomplete")
  }
  
  # Build value matrix by reading cells from grid
  n_rows <- base::nrow(grid)
  n_cols <- base::ncol(grid)
  value_matrix <- matrix(NA_real_, n_rows, n_cols)
  
  for (i in seq_len(n_rows)) {
    for (j in seq_len(n_cols)) {
      cell_id <- grid[i, j]
      
      if (!is.na(cell_id) && has_cell(x$cell_store, cell_id)) {
        cell <- get_cell(x$cell_store, cell_id)
        value_matrix[i, j] <- cell$value
      }
    }
  }
  
  # Create data.frame
  df <- as.data.frame(value_matrix, 
                     stringsAsFactors = FALSE, 
                     row.names = row.names)
  
  # Get row labels
  # Only recompute if there's an explicit override that differs from stored mode
  if (!is.null(x$layout$row_labels) && length(x$layout$row_labels) == n_rows) {
    # Start with stored labels
    base_row_labels <- x$layout$row_labels
    
    # Only recompute if label_mode was explicitly overridden and differs from stored mode
    if (!is.null(label_mode) && label_mode != stored_mode) {
      # Recompute labels with the overridden mode
      row_labels <- sapply(1:n_rows, function(i) {
        # Get first cell for metadata
        cell_id <- grid[i, which(!is.na(grid[i, ]))[1]]  # First non-NA cell
        cell <- if (!is.na(cell_id) && has_cell(x$cell_store, cell_id)) {
          get_cell(x$cell_store, cell_id)
        } else {
          NULL
        }
        
        # Try to apply label_mode transformation
        if (!is.null(cell) && !is.null(cell$specification$meta$row_vars) && 
            length(cell$specification$meta$row_vars) > 0) {
          # Extract variable metadata from cell
          row_var_meta <- cell$specification$meta$row_vars[[1]]
          var_name <- row_var_meta$ivar
          # Handle ival: could be NULL, NA, or a vector
          category_name <- if (!is.null(row_var_meta$ival) && length(row_var_meta$ival) > 0 && 
                               !all(is.na(row_var_meta$ival))) {
            row_var_meta$ival_label
          } else {
            NULL
          }
          
          # Check if this is a summary row
          if (isTRUE(cell$specification$is_summary_row)) {
            # Summary rows (NET, Avg, etc.) use their stored label as-is
            base_row_labels[i]
          } else {
            # Apply label transformation from metadata
            tryCatch({
              get_display_label(var_name, x$dpdict, effective_mode, category_name, x$data)
            }, error = function(e) {
              # Fallback to stored label
              base_row_labels[i]
            })
          }
        } else {
          # No metadata available - use stored label
          base_row_labels[i]
        }
      })
    } else {
      # Use stored labels as-is (they're already in the correct mode)
      row_labels <- base_row_labels
    }
  } else {
    # Fallback if no labels stored (shouldn't happen)
    row_labels <- paste0("Row_", seq_len(n_rows))
  }
  
  df <- cbind(
    data.frame(row_label = row_labels, stringsAsFactors = FALSE),
    df
  )
  
  # Get column labels
  # Only recompute if there's an explicit override that differs from stored mode
  if (!is.null(x$layout$col_labels) && length(x$layout$col_labels) == n_cols) {
    # Start with stored labels
    base_col_labels <- x$layout$col_labels
    
    # Only recompute if label_mode was explicitly overridden and differs from stored mode
    if (!is.null(label_mode) && label_mode != stored_mode) {
      # Recompute labels with the overridden mode
      col_names <- sapply(1:n_cols, function(j) {
        # Get first cell for metadata
        cell_id <- grid[which(!is.na(grid[, j]))[1], j]  # First non-NA cell
        cell <- if (!is.na(cell_id) && has_cell(x$cell_store, cell_id)) {
          get_cell(x$cell_store, cell_id)
        } else {
          NULL
        }
        
        # Try to apply label_mode transformation
        if (!is.null(cell) && !is.null(cell$specification$meta$col_vars) && 
            length(cell$specification$meta$col_vars) > 0) {
          # Extract variable metadata from cell
          col_var_meta <- cell$specification$meta$col_vars[[1]]
          var_name <- col_var_meta$ivar
          # Handle ival: could be NULL, NA, or a vector
          category_name <- if (!is.null(col_var_meta$ival) && length(col_var_meta$ival) > 0 && 
                               !all(is.na(col_var_meta$ival))) {
            col_var_meta$ival_label
          } else {
            NULL
          }
          
          # Check if this is a summary column
          if (isTRUE(cell$specification$is_summary_col)) {
            # Summary columns (Total, NET, etc.) use their stored label as-is
            base_col_labels[j]
          } else {
            # Apply label transformation from metadata
            tryCatch({
              get_display_label(var_name, x$dpdict, effective_mode, category_name, x$data)
            }, error = function(e) {
              # Fallback to stored label
              base_col_labels[j]
            })
          }
        } else {
          # No metadata available - use stored label
          base_col_labels[j]
        }
      })
    } else {
      # Use stored labels as-is (they're already in the correct mode)
      col_names <- base_col_labels
    }
  } else {
    # Fallback if no labels stored (shouldn't happen)
    col_names <- paste0("Col_", seq_len(n_cols))
  }
  names(df)[-1] <- col_names
  
  # Grid is now always physical - what's in the grid is what should be shown
  # No need to check filter_rules for hidden columns/rows
  
  # Attach metadata if requested
  if (include_metadata) {
    # Include full cell objects for each grid position
    cell_meta_matrix <- matrix(list(), n_rows, n_cols)
    
    for (i in seq_len(n_rows)) {
      for (j in seq_len(n_cols)) {
        cell_id <- grid[i, j]
        if (!is.na(cell_id)) {
          cell_meta_matrix[[i, j]] <- get_cell(x$cell_store, cell_id)
        }
      }
    }
    
    attr(df, "cell_metadata") <- cell_meta_matrix
  }
  
  # Preserve key attributes for compatibility with existing functions
  attr(df, "statistic") <- x$statistic
  attr(df, "call") <- x$call
  attr(df, "layout") <- x$layout
  if (!is.null(x$layout$row_exposure)) {
    attr(df, "row_exposure") <- x$layout$row_exposure
  }
  if (!is.null(x$layout$col_exposure)) {
    attr(df, "col_exposure") <- x$layout$col_exposure
  }
  
  # Recreate arrays attribute (for add_sig compatibility)
  if (!is.null(x$arrays)) {
    attr(df, "arrays") <- x$arrays
  }
  
  # Recreate base_matrix (for compatibility)
  base_matrix <- matrix(NA_real_, n_rows, n_cols)
  for (i in seq_len(n_rows)) {
    for (j in seq_len(n_cols)) {
      cell_id <- grid[i, j]
      if (!is.na(cell_id)) {
        cell <- get_cell(x$cell_store, cell_id)
        base_matrix[i, j] <- cell$base
      }
    }
  }
  attr(df, "base_matrix") <- base_matrix
  
  # Create statistic_matrix for mixed-statistic tabs (when tab-level statistic is NULL)
  if (is.null(x$statistic)) {
    statistic_matrix <- matrix(NA_character_, n_rows, n_cols)
    for (i in seq_len(n_rows)) {
      for (j in seq_len(n_cols)) {
        cell_id <- grid[i, j]
        if (!is.na(cell_id)) {
          cell <- get_cell(x$cell_store, cell_id)
          if (!is.null(cell) && !is.null(cell$computation$statistic)) {
            statistic_matrix[i, j] <- cell$computation$statistic
          }
        }
      }
    }
    attr(df, "statistic_matrix") <- statistic_matrix
  }
  
  ## Extract significance from cells if present ----------------------------------
  # Check if any cells have significance data
  has_sig <- FALSE
  all_sig_tests <- list()
  
  for (i in seq_len(n_rows)) {
    for (j in seq_len(n_cols)) {
      cell_id <- grid[i, j]
      if (!is.na(cell_id)) {
        cell <- get_cell(x$cell_store, cell_id)
        if (!is.null(cell) && !is.null(cell$significance)) {
          has_sig <- TRUE
          # Collect all test names
          test_names <- names(cell$significance)
          for (test_name in test_names) {
            if (!test_name %in% names(all_sig_tests)) {
              all_sig_tests[[test_name]] <- list()
            }
          }
        }
      }
    }
  }
  
  # If significance found, extract and format it
  if (has_sig) {
    # For each test, build significance matrices
    for (test_name in names(all_sig_tests)) {
      sig_matrix <- matrix("", nrow = n_rows, ncol = n_cols)
      p_matrix <- matrix(NA_real_, nrow = n_rows, ncol = n_cols)
      test_used <- NULL
      versus <- NULL
      is_omnibus <- NULL
      
      # Extract significance from each cell
      for (i in seq_len(n_rows)) {
        for (j in seq_len(n_cols)) {
          cell_id <- grid[i, j]
          if (!is.na(cell_id)) {
            cell <- get_cell(x$cell_store, cell_id)
            if (!is.null(cell) && !is.null(cell$significance[[test_name]])) {
              sig_data <- cell$significance[[test_name]]
              sig_matrix[i, j] <- sig_data$level
              p_matrix[i, j] <- sig_data$p_value
              if (is.null(test_used)) {
                test_used <- sig_data$test_used
                versus <- sig_data$versus
                is_omnibus <- sig_data$is_omnibus
              }
            }
          }
        }
      }
      
      # Set matrix names
      rownames(sig_matrix) <- row_labels
      colnames(sig_matrix) <- col_names
      rownames(p_matrix) <- row_labels
      colnames(p_matrix) <- col_names
      
      # Store in format matching data.frame-based add_sig
      sig_result <- list(
        levels = sig_matrix,
        p_values = p_matrix,
        test_used = test_used,
        versus = versus
      )
      
      # Add is_omnibus flag if it exists
      if (!is.null(is_omnibus)) {
        sig_result$is_omnibus <- is_omnibus
      }
      
      all_sig_tests[[test_name]] <- sig_result
    }
    
    # Attach significance attribute
    attr(df, "significance") <- all_sig_tests
  }
  
  ## Add visible base (using calculated base matrix) ------------------------------
  base_orientation <- NULL
  if (show_base && !is.null(base_matrix) && is.list(x$statistic)) {
    # Determine orientation from the base_matrix structure
    display_as_row <- TRUE
    display_as_column <- TRUE
    
    if (ncol(base_matrix) > 1) {
      # Check if bases are constant within columns
      for (j in seq_len(ncol(base_matrix))) {
        if (length(unique(base_matrix[, j])) > 1) {
          display_as_row <- FALSE
          break
        }
      }
    }
    
    if (nrow(base_matrix) > 1) {
      # Check if bases are constant within rows
      for (i in seq_len(nrow(base_matrix))) {
        if (length(unique(base_matrix[i, ])) > 1) {
          display_as_column <- FALSE
          break
        }
      }
    }
    
    # Decide on display format
    if (display_as_row && !display_as_column) {
      # Bases are constant within columns - display as row
      base_orientation <- "column"
    } else if (display_as_column && !display_as_row) {
      # Bases are constant within rows - display as column
      base_orientation <- "row"
    } else if (display_as_row && display_as_column) {
      # All bases are the same - default to row display
      base_orientation <- "column"
    } else {
      # Bases vary in both dimensions - still default to row display
      base_orientation <- "column"
    }
    
    if (base_orientation == "row") {
      # Display base as a column
      row_base_values <- numeric(nrow(df))
      
      # Extract base values for each row from calculated matrix
      for (i in seq_len(nrow(base_matrix))) {
        if (ncol(base_matrix) > 0) {
          # Get the common base for this row
          row_bases_unique <- unique(base_matrix[i, ])
          if (length(row_bases_unique) == 1) {
            row_base_values[i] <- as.integer(row_bases_unique)
          } else {
            # Inconsistent bases - use NA to maintain numeric type
            row_base_values[i] <- NA_real_
          }
        } else {
          row_base_values[i] <- as.integer(base_matrix[i, 1])
        }
      }
      
      df[[x$statistic$base_label]] <- row_base_values
      
    } else {
      # Display base as a row
      col_base_values <- numeric(ncol(base_matrix))
      
      # Extract base values for each column from calculated matrix
      for (j in seq_len(ncol(base_matrix))) {
        # Check if all rows have same base for this column
        col_bases_unique <- unique(base_matrix[, j])
        col_bases_unique <- col_bases_unique[!is.na(col_bases_unique)]
        if (length(col_bases_unique) == 1) {
          col_base_values[j] <- as.integer(col_bases_unique)
        } else if (length(col_bases_unique) == 0) {
          # No valid bases found - use NA
          col_base_values[j] <- NA_real_
        } else {
          # Inconsistent bases - use NA to maintain numeric type
          col_base_values[j] <- NA_real_
        }
      }
      
      # Create base row with same structure as df
      # df has row_label + n data columns
      n_data_cols <- ncol(df) - 1  # Subtract row_label column
      
      # Check if dimensions match (defensive check - should always match with physical grid)
      if (length(col_base_values) != n_data_cols) {
        # This should not happen with physical grid modification approach
        # Grid dimensions always match df dimensions since hiding physically removes row_defs/col_defs
        warning("Unexpected dimension mismatch: Base matrix columns (", length(col_base_values), 
                ") don't match data frame columns (", n_data_cols, "). ",
                "This may indicate a bug in grid allocation. Adjusting base row to match.")
        # Resize col_base_values to match
        if (length(col_base_values) < n_data_cols) {
          # Pad with NAs (maintain numeric type)
          col_base_values <- c(col_base_values, rep(NA_real_, n_data_cols - length(col_base_values)))
        } else {
          # Truncate
          col_base_values <- col_base_values[1:n_data_cols]
        }
      }
      
      # Create base row with exact same structure as df
      # Start with a copy of the first row of df to get the right structure
      base_row <- df[1, , drop = FALSE]
      
      # Set all values to the base values we calculated
      base_row[1, 1] <- x$statistic$base_label  # row_label column
      for (i in seq_along(col_base_values)) {
        base_row[1, i + 1] <- col_base_values[i]
      }
      
      # Clear row names
      rownames(base_row) <- NULL
      
      # Add base row
      df <- rbind(df, base_row)
    }
  }
  
  # Store base orientation as attribute for downstream use
  attr(df, "base_orientation") <- base_orientation
  
  # Mark as originating from cell-based architecture
  attr(df, "cell_based_origin") <- TRUE
  
  # Preserve tab_result class so downstream functions recognize it
  class(df) <- c("tab_result", "data.frame")
  return(df)
}

#' Extract base matrix from cell-based tab_result
#'
#' Helper function to rebuild base matrix from cells.
#'
#' @param tab_result Cell-based tab_result object
#' @return Matrix of base counts
#' @keywords internal
extract_base_matrix <- function(tab_result) {
  grid <- tab_result$layout$grid
  base_matrix <- matrix(NA_real_, nrow(grid), ncol(grid))
  
  for (i in seq_len(nrow(grid))) {
    for (j in seq_len(ncol(grid))) {
      cell_id <- grid[i, j]
      if (!is.na(cell_id) && has_cell(tab_result$cell_store, cell_id)) {
        cell <- get_cell(tab_result$cell_store, cell_id)
        base_matrix[i, j] <- cell$base
      }
    }
  }
  
  base_matrix
}

#' View base matrix from tab result
#'
#' Extracts and displays the base (sample size) counts for each cell in a tab
#' result. Returns a data.frame with dimensions matching the visible tab layout,
#' showing raw base counts as numeric values. This function respects the current
#' visibility settings (hidden rows and columns are excluded from the output).
#'
#' @param tab_result A tab_result object (either cell-based tab_cell_collection 
#'   or materialized data.frame)
#' @return A data.frame with a row_label column followed by data columns 
#'   containing raw base counts (numeric values). The structure matches the 
#'   visible layout of the tab result.
#' @export
#' @examples
#' \dontrun{
#' # Basic usage with cell-based result
#' result <- tab(data, satisfaction, gender)
#' bases <- view_base_matrix(result)
#' 
#' # Works with materialized results too
#' result_df <- as.data.frame(result)
#' bases <- view_base_matrix(result_df)
#' 
#' # Respects visibility - hidden rows/columns are excluded
#' result <- tab(data, satisfaction, gender) %>%
#'   hide_rows("Don't know") %>%
#'   hide_cols("Prefer not to say")
#' bases <- view_base_matrix(result)  # Excluded rows/cols won't appear
#' 
#' # With derived columns
#' result <- tab(data, satisfaction, gender) %>%
#'   derive(delta_vs("Male", "Female"))
#' bases <- view_base_matrix(result)
#' }
view_base_matrix <- function(tab_result) {
  # Validate input
  if (!inherits(tab_result, "tab_result")) {
    stop("view_base_matrix() requires a tab_result object")
  }
  
  # Path 1: Cell-based tab_result (tab_cell_collection)
  if (inherits(tab_result, "tab_cell_collection")) {
    # Extract full base matrix from cells
    base_matrix <- extract_base_matrix(tab_result)
    
    # Get row and column labels
    row_labels <- tab_result$layout$row_labels
    col_labels <- tab_result$layout$col_labels
    
    # Check for empty grid
    if (is.null(base_matrix) || nrow(base_matrix) == 0 || ncol(base_matrix) == 0) {
      # Return empty data.frame with proper structure
      return(data.frame(row_label = character(0), stringsAsFactors = FALSE))
    }
    
    # Determine which rows and columns are hidden
    hidden_rows <- .get_hidden_row_indices(tab_result)
    hidden_cols <- .get_hidden_col_indices(tab_result)
    
    # Filter base matrix to only visible rows and columns
    if (length(hidden_rows) > 0) {
      base_matrix <- base_matrix[-hidden_rows, , drop = FALSE]
      row_labels <- row_labels[-hidden_rows]
    }
    if (length(hidden_cols) > 0) {
      base_matrix <- base_matrix[, -hidden_cols, drop = FALSE]
      col_labels <- col_labels[-hidden_cols]
    }
    
    # Convert to data.frame with proper structure
    df <- as.data.frame(base_matrix, stringsAsFactors = FALSE)
    
    # Add row_label column at the beginning
    df <- cbind(
      data.frame(row_label = row_labels, stringsAsFactors = FALSE),
      df
    )
    
    # Set column names (skip first column which is row_label)
    names(df)[-1] <- col_labels
    
    # Remove row names
    rownames(df) <- NULL
    
    return(df)
  }
  
  # Path 2: Materialized data.frame tab_result
  if (inherits(tab_result, "data.frame")) {
    # Try to extract base_matrix attribute
    base_matrix <- attr(tab_result, "base_matrix")
    
    if (is.null(base_matrix)) {
      stop("Cannot extract base matrix: tab_result does not have a 'base_matrix' attribute. ",
           "This may occur if the result was created with an older version of surveydatar.")
    }
    
    # Get row labels from the data.frame
    row_labels <- tab_result$row_label
    
    # Get column names (excluding row_label)
    col_names <- names(tab_result)[-1]

    # Determine base label (if a visible base row/column was added during materialization)
    stat_info <- attr(tab_result, "statistic")
    base_label <- "Base (n)"
    if (is.list(stat_info) && !is.null(stat_info$base_label) && nzchar(stat_info$base_label)) {
      base_label <- stat_info$base_label
    }
    
    # Check dimensions match
    # Common case: materialized result includes a presentation "Base (n)" row, but base_matrix is
    # stored only for the data grid. Drop the base row label to align without warning.
    if (nrow(base_matrix) == (length(row_labels) - 1L) && base_label %in% row_labels) {
      base_row_idx <- match(base_label, row_labels)
      if (!is.na(base_row_idx)) {
        row_labels <- row_labels[-base_row_idx]
      }
    }
    if (nrow(base_matrix) != length(row_labels)) {
      warning("Base matrix rows (", nrow(base_matrix), ") don't match data rows (", 
              length(row_labels), "). Using available data.")
      # Adjust to minimum size
      min_rows <- min(nrow(base_matrix), length(row_labels))
      base_matrix <- base_matrix[1:min_rows, , drop = FALSE]
      row_labels <- row_labels[1:min_rows]
    }
    
    # Symmetric case: materialized result includes a presentation base column (less common).
    if (ncol(base_matrix) == (length(col_names) - 1L) && base_label %in% col_names) {
      base_col_idx <- match(base_label, col_names)
      if (!is.na(base_col_idx)) {
        col_names <- col_names[-base_col_idx]
      }
    }
    if (ncol(base_matrix) != length(col_names)) {
      warning("Base matrix columns (", ncol(base_matrix), ") don't match data columns (", 
              length(col_names), "). Using available data.")
      # Adjust to minimum size
      min_cols <- min(ncol(base_matrix), length(col_names))
      base_matrix <- base_matrix[, 1:min_cols, drop = FALSE]
      col_names <- col_names[1:min_cols]
    }
    
    # Convert to data.frame
    df <- as.data.frame(base_matrix, stringsAsFactors = FALSE)
    
    # Add row_label column at the beginning
    df <- cbind(
      data.frame(row_label = row_labels, stringsAsFactors = FALSE),
      df
    )
    
    # Set column names (skip first column which is row_label)
    names(df)[-1] <- col_names
    
    # Remove row names
    rownames(df) <- NULL
    
    return(df)
  }
  
  # Should not reach here, but just in case
  stop("Unexpected tab_result structure")
}