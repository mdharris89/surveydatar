# Post-Tab Operations
#
# Operations that modify tab results after initial creation:
# - modify_labels: Edit labels with gsub patterns after tab() creation
# - add_col_from_lookup: Enrich tables with external lookup data
# - All operations preserve cell store and modify layout/labels only


#' Add a column to tab results from a lookup table
#'
#' Enriches a tab_result object by adding values from a lookup table based on
#' key matching. The new column is positioned directly after the key column.
#' Supports optional transformations of keys and fuzzy partial matching.
#'
#' This function operates on **materialized** tab_result objects only. Cell-based
#' results from `tab()` must first be converted using `as.data.frame()`. The added
#' column is for display/export purposes (e.g., `copy_tab()`, `print()`) and does
#' not participate in layout operations like `arrange_rows()` or `hide_cols()`.
#'
#' @param tab_result A **materialized** tab_result object (use `as.data.frame(tab(...))`)
#' @param lookup_df A data frame containing the lookup table
#' @param tab_key Character string naming the column in tab_result to use as key
#' @param lookup_key Character string naming the column in lookup_df to match against
#' @param lookup_value Character string naming the column in lookup_df with values to add
#' @param new_col_name Character string for the name of the new column to add
#' @param tab_transform Optional function to transform tab_key values before matching
#' @param lookup_transform Optional function to transform lookup_key values before matching
#' @param fuzzy Logical. If TRUE, uses partial string matching instead of exact matching
#'
#' @return A tab_result object with the new column added after the key column
#' @export
#'
#' @examples
#' \dontrun{
#' # Example: Adding theme descriptions to a tab of theme tags
#' # Note: must materialize with as.data.frame() before add_col_from_lookup()
#' themes_lookup <- data.frame(
#'   theme_id = c("satisfaction", "wait_time", "staff_helpful"),
#'   theme_description = c(
#'     "Overall satisfaction with service",
#'     "Wait time was reasonable",
#'     "Staff were helpful and knowledgeable"
#'   )
#' )
#'
#' result <- data %>%
#'   tab(theme_satisfaction, region) %>%
#'   as.data.frame() %>%
#'   add_col_from_lookup(
#'     themes_lookup,
#'     "row_label",
#'     "theme_id",
#'     "theme_description",
#'     "Theme"
#'   ) %>%
#'   copy_tab()  # Ready to paste with descriptions
#'
#' # With transformations to clean up keys
#' result <- data %>%
#'   tab(q1, q2) %>%
#'   as.data.frame() %>%
#'   add_col_from_lookup(
#'     labels_df,
#'     "row_label",
#'     "label_code",
#'     "full_text",
#'     "Question Text",
#'     tab_transform = function(x) gsub("^Q1: ", "", x),
#'     lookup_transform = toupper
#'   )
#'
#' # With fuzzy matching for partial matches
#' result <- data %>%
#'   tab(categories, dept) %>%
#'   as.data.frame() %>%
#'   add_col_from_lookup(
#'     category_descriptions,
#'     "row_label",
#'     "category_pattern",
#'     "long_description",
#'     "Description",
#'     fuzzy = TRUE
#'   )
#' }
add_col_from_lookup <- function(tab_result,
                                lookup_df,
                                tab_key,
                                lookup_key,
                                lookup_value,
                                new_col_name,
                                tab_transform = NULL,
                                lookup_transform = NULL,
                                fuzzy = FALSE) {

  # Input validation
  if (!inherits(tab_result, "tab_result")) {
    stop("First argument must be a tab_result object from the tab() function")
  }
  
  # Cell-based tab_results must be materialized first

  if (inherits(tab_result, "tab_cell_collection")) {
    stop("add_col_from_lookup() requires a materialized tab_result. ",
         "Use as.data.frame(tab(...)) first, then pipe into add_col_from_lookup().")
  }

  if (!is.data.frame(lookup_df)) {
    stop("lookup_df must be a data frame")
  }

  if (!tab_key %in% names(tab_result)) {
    stop("Column '", tab_key, "' not found in tab_result")
  }

  if (!lookup_key %in% names(lookup_df)) {
    stop("Column '", lookup_key, "' not found in lookup_df")
  }

  if (!lookup_value %in% names(lookup_df)) {
    stop("Column '", lookup_value, "' not found in lookup_df")
  }

  if (new_col_name %in% names(tab_result)) {
    warning("Column '", new_col_name, "' already exists in tab_result and will be overwritten")
  }

  # Store original attributes
  original_attrs <- attributes(tab_result)

  # Extract the key values from tab_result
  tab_keys <- tab_result[[tab_key]]

  # Apply transformation to tab keys if provided
  if (!is.null(tab_transform)) {
    if (!is.function(tab_transform)) {
      stop("tab_transform must be a function or NULL")
    }
    tab_keys_transformed <- tab_transform(tab_keys)
  } else {
    tab_keys_transformed <- tab_keys
  }

  # Extract lookup keys and values
  lookup_keys <- lookup_df[[lookup_key]]
  lookup_values <- lookup_df[[lookup_value]]

  # Apply transformation to lookup keys if provided
  if (!is.null(lookup_transform)) {
    if (!is.function(lookup_transform)) {
      stop("lookup_transform must be a function or NULL")
    }
    lookup_keys_transformed <- lookup_transform(lookup_keys)
  } else {
    lookup_keys_transformed <- lookup_keys
  }

  # Perform matching
  if (!fuzzy) {
    # Exact matching
    match_indices <- match(tab_keys_transformed, lookup_keys_transformed)
  } else {
    # Fuzzy partial matching
    match_indices <- integer(length(tab_keys_transformed))

    for (i in seq_along(tab_keys_transformed)) {
      if (is.na(tab_keys_transformed[i])) {
        match_indices[i] <- NA_integer_
        next
      }

      # Convert to character for string matching
      key_str <- as.character(tab_keys_transformed[i])
      lookup_strs <- as.character(lookup_keys_transformed)

      # Find partial matches - check if tab key is contained in any lookup key
      matches <- which(vapply(lookup_strs, function(lookup_str) {
        if (is.na(lookup_str)) return(FALSE)
        grepl(key_str, lookup_str, fixed = TRUE, ignore.case = FALSE)
      }, logical(1)))

      # If no matches, try reverse - check if any lookup key is contained in tab key
      if (length(matches) == 0) {
        matches <- which(vapply(lookup_strs, function(lookup_str) {
          if (is.na(lookup_str)) return(FALSE)
          grepl(lookup_str, key_str, fixed = TRUE, ignore.case = FALSE)
        }, logical(1)))
      }

      if (length(matches) > 0) {
        match_indices[i] <- matches[1]  # Take first match
      } else {
        match_indices[i] <- NA_integer_
      }
    }
  }

  # Extract matched values
  new_column_values <- lookup_values[match_indices]

  # Find position of key column
  key_col_position <- which(names(tab_result) == tab_key)

  # Build new data frame with column inserted after key column
  if (key_col_position == ncol(tab_result)) {
    # Key column is last column - append new column at the end
    tab_result[[new_col_name]] <- new_column_values
  } else {
    # Insert after key column
    before_cols <- names(tab_result)[1:key_col_position]
    after_cols <- names(tab_result)[(key_col_position + 1):ncol(tab_result)]

    # Reconstruct data frame with new column in correct position
    new_df <- tab_result[, before_cols, drop = FALSE]
    new_df[[new_col_name]] <- new_column_values
    new_df[after_cols] <- tab_result[, after_cols, drop = FALSE]

    tab_result <- new_df
  }

  # Restore all original attributes (except names which have changed)
  attrs_to_restore <- setdiff(names(original_attrs), c("names", "row.names"))
  for (attr_name in attrs_to_restore) {
    attr(tab_result, attr_name) <- original_attrs[[attr_name]]
  }

  # Ensure class is preserved
  class(tab_result) <- c("tab_result", "data.frame")

  return(tab_result)
}

#' Modify row and column labels using pattern matching
#'
#' Apply gsub pattern replacements to row and column labels in the layout structure.
#' Labels are pure metadata and modifications persist through all layout operations.
#' Patterns are applied sequentially in the order provided.
#'
#' @param tab_result A tab_result object (cell-based)
#' @param row_labels Named character vector of pattern/replacement pairs for row labels.
#'   Patterns are applied sequentially to labels in layout$row_defs.
#' @param col_labels Named character vector of pattern/replacement pairs for column labels.
#'   Patterns are applied sequentially to labels in layout$col_defs.
#'
#' @return Modified tab_result with updated labels in layout structure
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Basic pattern replacement
#' result %>%
#'   modify_labels(
#'     row_labels = c("Very satisfied" = "Very sat"),
#'     col_labels = c("Male" = "M", "Female" = "F")
#'   )
#'
#' # Regex pattern matching
#' result %>%
#'   modify_labels(
#'     row_labels = c("\\s+" = " ", "^(\\w)" = "\\U\\1"),
#'     col_labels = c("[0-9]+" = "")
#'   )
#'
#' # Labels persist through layout operations
#' result %>%
#'   modify_labels(row_labels = c("Very satisfied" = "VS")) %>%
#'   select_rows("VS", .match = "label") %>%
#'   arrange_rows(.by = "Total", .sort = "desc")
#' }
modify_labels <- function(tab_result, row_labels = NULL, col_labels = NULL) {
  # Validate input
  if (!inherits(tab_result, "tab_cell_collection")) {
    stop("modify_labels() requires a cell-based tab_result")
  }
  
  if (!is.null(row_labels) && (!is.character(row_labels) || is.null(names(row_labels)))) {
    stop("row_labels must be a named character vector")
  }
  
  if (!is.null(col_labels) && (!is.character(col_labels) || is.null(names(col_labels)))) {
    stop("col_labels must be a named character vector")
  }
  
  # Modify row_defs labels if specified
  if (!is.null(row_labels)) {
    for (i in seq_along(tab_result$layout$row_defs)) {
      current_label <- tab_result$layout$row_defs[[i]]$label
      
      # Apply patterns sequentially
      for (j in seq_along(row_labels)) {
        pattern <- names(row_labels)[j]
        replacement <- row_labels[j]
        current_label <- gsub(pattern, replacement, current_label, perl = TRUE)
      }
      
      tab_result$layout$row_defs[[i]]$label <- current_label
    }
    
    # Update cached labels
    tab_result$layout$row_labels <- sapply(tab_result$layout$row_defs, `[[`, "label")
  }
  
  # Modify col_defs labels if specified
  if (!is.null(col_labels)) {
    for (j in seq_along(tab_result$layout$col_defs)) {
      current_label <- tab_result$layout$col_defs[[j]]$label
      
      # Apply patterns sequentially
      for (k in seq_along(col_labels)) {
        pattern <- names(col_labels)[k]
        replacement <- col_labels[k]
        current_label <- gsub(pattern, replacement, current_label, perl = TRUE)
      }
      
      tab_result$layout$col_defs[[j]]$label <- current_label
    }
    
    # Update cached labels
    tab_result$layout$col_labels <- sapply(tab_result$layout$col_defs, `[[`, "label")
  }
  
  # Return modified tab_result
  # NO refresh_layout() call - labels don't affect allocation
  return(tab_result)
}