# Tab Utilities
#
# Utility functions for tab creation and manipulation:
#
# VALIDATION:
# - validate_statistic_variables: Ensure variables appropriate for statistic
# - check_variables_exist: Variable existence with fuzzy suggestions
#
# LABEL HANDLING:
# - get_var_label: Get variable label from dpdict or use variable name
# - get_display_label: Smart, full, or suffix label modes
# - extract_suffix_from_label: Parse labels using separator patterns
#
# SPECIFICATIONS:
# - rows_list/cols_list: Create named row/column specifications
# - calculate_summary: Compute NET/Total/Avg summary values

#' Validate Variable Types for Statistics
#'
#' Validates that variables used in cross-tabulation rows and columns are
#' appropriate for the specified statistic. For value-based statistics like
#' mean, ensures variables are categorical rather than continuous numeric.
#'
#' @param statistic A tab_stat object containing statistic metadata
#' @param rows_expanded List of expanded row variable specifications
#' @param cols_expanded List of expanded column variable specifications
#' @param data Data frame being analyzed
#'
#' @return NULL if validation passes, throws error with helpful suggestions if not
#' @keywords internal
#'
#' @details
#' For statistics requiring values (like mean), this function prevents users from
#' accidentally creating meaningless cross-tabs with many numeric categories.
#' Variables with >15 unique values trigger errors with suggestions to use
#' factor(), cut(), add value labels, or use correlation statistics instead.
validate_statistic_variables <- function(statistic, rows_expanded, cols_expanded, data, dpdict = NULL) {
  if (statistic$requires_values) {
    # For value-based statistics (like mean), require categorical rows/columns

    # Check rows
    for (row_spec in rows_expanded) {
      if (row_spec$type == "simple") {
        var_name <- row_spec$components$var
        if (var_name %in% names(data)) {
          var_data <- data[[var_name]]

          # Check questiontype first if available
          if (!is.null(dpdict) && "questiontype" %in% names(dpdict) && var_name %in% dpdict$variable_names) {
            questiontype <- dpdict$questiontype[dpdict$variable_names == var_name]
            if (questiontype %in% c("numeric", "multinumeric")) {
              stop("Cannot use numeric variable '", var_name, "' (questiontype: ", questiontype, ") in rows for '", statistic$id, "' statistic")
            }
          } else {
            # Fallback to R class checking
            if (is.numeric(var_data) &&
                is.null(attr(var_data, "labels")) &&
                !is.factor(var_data) &&
                length(unique(na.omit(var_data))) > 15) {
              stop("Cannot use numeric variable '", var_name, "' with ", length(unique(na.omit(var_data))), " unique values in rows for '", statistic$id, "' statistic")
            }
          }
        }
      }
    }

    # Check columns (if not just "Total")
    if (length(cols_expanded) > 1 || cols_expanded[[1]]$type != "total") {
      for (col_spec in cols_expanded) {
        if (col_spec$type == "simple") {
          var_name <- col_spec$components$var
          if (var_name %in% names(data)) {
            var_data <- data[[var_name]]

            if (is.numeric(var_data) &&
                is.null(attr(var_data, "labels")) &&
                !is.factor(var_data) &&
                length(unique(na.omit(var_data))) > 15) {

              stop("Cannot use numeric variable '", var_name, ") in columns for '", statistic$id, "' statistic.\n")
            }
          }
      }
    }
  }
}
}

#' Check if all variables exist in data and provide helpful error message
#' @param vars Character vector of variable names to check
#' @param data Data frame to check against
#' @param context String describing where the variables are being used
#' @keywords internal
check_variables_exist <- function(vars, data, context = "expression") {
  if (length(vars) == 0) return(TRUE)

  missing_vars <- setdiff(vars, names(data))

  if (length(missing_vars) > 0) {
    # Get similar variable names for suggestions
    all_vars <- names(data)
    suggestions <- character()

    for (missing in missing_vars) {
      # Find variables with similar names (using agrep for fuzzy matching)
      similar <- agrep(missing, all_vars, value = TRUE, max.distance = 0.2)
      if (length(similar) > 0) {
        suggestions <- c(suggestions,
                         paste0("  - '", missing, "' -> did you mean: ",
                                paste(paste0("'", similar, "'"), collapse = ", "), "?"))
      } else {
        suggestions <- c(suggestions, paste0("  - '", missing, "' (no similar variables found)"))
      }
    }

    # Build comprehensive error message
    if(length(all_vars) < 20){
      error_msg <- paste0(
        "Variable(s) not found in ", context, ":\n",
        paste(suggestions, collapse = "\n"),
        "\n\nAvailable variables in data:\n",
        paste(strwrap(paste(all_vars, collapse = ", "), width = 70), collapse = "\n  ")
      )
    } else {
      error_msg <- paste0(
        "Variable(s) not found in ", context, ":\n",
        paste(suggestions, collapse = "\n")
      )
    }
    stop(error_msg, call. = FALSE)
  }

  return(TRUE)
}

#' Get display label based on label mode
#'
#' @param var_name Variable name
#' @param dpdict Data dictionary
#' @param label_mode One of "full", "suffix", or "smart"
#' @param category_name For expanded categorical variables
#' @param data Optional data frame for separator detection
#' @return Character string label
#' @keywords internal
get_display_label <- function(var_name, dpdict = NULL, label_mode = "full", category_name = NULL, data = NULL) {

  # Get var info from dpdict if available
  var_idx <- if (!is.null(dpdict) && var_name %in% dpdict$variable_names) {
    match(var_name, dpdict$variable_names)
  } else {
    NA
  }

  # Handle each mode
  switch(label_mode,
         "full" = {
           if (!is.null(category_name)) {
             base_label <- if (!is.na(var_idx)) dpdict$variable_labels[var_idx] else var_name
             paste0(base_label, ": ", category_name)
           } else {
             if (!is.na(var_idx)) dpdict$variable_labels[var_idx] else var_name
           }
         },

         "suffix" = {
           if (!is.null(category_name)) {
             category_name
           } else if (!is.na(var_idx)) {
             if ("question_suffix" %in% names(dpdict) &&
                 !is.na(dpdict$question_suffix[var_idx]) &&
                 nzchar(dpdict$question_suffix[var_idx])) {
               dpdict$question_suffix[var_idx]
             } else {
               # Updated call with dpdict and data parameters
               extract_suffix_from_label(dpdict$variable_labels[var_idx], dpdict, data)
             }
           } else {
             var_name
           }
         },

         "smart" = {
           if (!is.null(category_name)) {
             category_name
           } else {
             # Check if multi-item question
             is_multi <- if (!is.na(var_idx) && "singlevariablequestion" %in% names(dpdict)) {
               !isTRUE(dpdict$singlevariablequestion[var_idx])
             } else if (!is.na(var_idx) && "question_group" %in% names(dpdict)) {
               qgroup <- dpdict$question_group[var_idx]
               sum(dpdict$question_group == qgroup, na.rm = TRUE) > 1
             } else {
               FALSE  # Assume single-item, will use full label
             }

             if (is_multi) {
               # Multi-item: use suffix
               if (!is.na(var_idx) && "question_suffix" %in% names(dpdict) &&
                   !is.na(dpdict$question_suffix[var_idx])) {
                 dpdict$question_suffix[var_idx]
               } else if (!is.na(var_idx)) {
                 # Updated call with dpdict and data parameters
                 extract_suffix_from_label(dpdict$variable_labels[var_idx], dpdict, data)
               } else {
                 var_name
               }
             } else {
               # Single item: use full
               if (!is.na(var_idx)) dpdict$variable_labels[var_idx] else var_name
             }
           }
         },

         # Default fallback
         stop("Invalid label_mode: ", label_mode)
  )
}

#' Get variable label from dpdict or use variable name
#' @keywords internal
get_var_label <- function(var_name, dpdict = NULL) {
  if (!is.null(dpdict) && var_name %in% dpdict$variable_names) {
    label <- dpdict$variable_labels[dpdict$variable_names == var_name]
    if (!is.na(label) && nzchar(label)) return(label)
  }
  return(var_name)
}

#' Extract suffix from a label string using detected separator patterns
#' @param label Variable label to extract suffix from
#' @param dpdict Optional data dictionary containing separator patterns
#' @param data Optional data frame to detect separators from if dpdict lacks patterns
#' @keywords internal
extract_suffix_from_label <- function(label, dpdict = NULL, data = NULL) {
  if (is.null(label) || length(label) == 0 || is.na(label) || !nzchar(label)) {
    return(label)
  }

  # Try to get separator patterns from dpdict
  separators <- NULL
  if (!is.null(dpdict)) {
    sep_patterns <- attr(dpdict, "sep_patterns")
    if (!is.null(sep_patterns)) {
      # Use statement_sep first (most relevant for suffixes), then prefix_sep
      if (!is.na(sep_patterns[["statement_sep"]])) {
        separators <- c(sep_patterns[["statement_sep"]])
      }
      if (!is.na(sep_patterns[["prefix_sep"]])) {
        separators <- c(separators, sep_patterns[["prefix_sep"]])
      }
    }
  }

  # If no separators from dpdict, try to detect from data
  if (is.null(separators) && !is.null(data)) {
    sep_analysis <- check_seps(data)
    if (!is.na(sep_analysis$separators[["statement_sep"]])) {
      separators <- c(sep_analysis$separators[["statement_sep"]])
    }
    if (!is.na(sep_analysis$separators[["prefix_sep"]])) {
      separators <- c(separators, sep_analysis$separators[["prefix_sep"]])
    }
  }

  # Fall back to common defaults if no patterns detected
  if (is.null(separators) || length(separators) == 0) {
    separators <- c(" - ", ": ", " | ", " / ")
  }

  # Try each separator
  for (sep in separators) {
    if (grepl(sep, label, fixed = TRUE)) {
      parts <- strsplit(label, sep, fixed = TRUE)[[1]]
      if (length(parts) > 1) {
        suffix <- trimws(parts[length(parts)])
        if (nzchar(suffix)) return(suffix)
      }
    }
  }

  label
}

#' Create named list of row specifications
#'
#' @param ... Named expressions for row specifications
#' @return List for use in tab()
#' @export
#' @examples
#' rows_list("Total" = q1, "Young" = q1 * (age < 30))
rows_list <- function(...) {
  dots <- rlang::enquos(...)
  if (length(dots) == 0) {
    stop("rows_list() needs at least one expression")
    }
  if (is.null(names(dots))) names(dots) <- rep("", length(dots))
  dots
}

#' Create named list of column specifications
#'
#' @param ... Named expressions for column specifications
#' @return List for use in tab()
#' @export
#' @examples
#' cols_list("Total" = q1, "Young" = q1 * (age < 30))
cols_list <- function(...) {
  dots <- rlang::enquos(...)
  if (length(dots) == 0) {
    stop("cols_list() needs at least one expression")
  }
  if (is.null(names(dots))) names(dots) <- rep("", length(dots))
  dots
}

#' Calculate summary value based on type
#' @keywords internal
calculate_summary <- function(type, arrays, base_array, col_array, statistic, values) {
  if (type == "NET") {
    # Union of all conditions
    combined_matrix <- do.call(cbind, arrays)
    summary_array <- as.numeric(rowSums(combined_matrix, na.rm = TRUE) > 0)
  } else if (type == "Avg") {
    # Union for averaging (same as NET but labeled differently)
    combined_matrix <- do.call(cbind, arrays)
    summary_array <- as.numeric(rowSums(combined_matrix, na.rm = TRUE) > 0)
  } else if (type == "Total") {
    # Simple sum
    summary_array <- rep(1, length(base_array))
  } else {
    stop("Unknown summary type: ", type)
  }

  # Create label showing what was combined
  array_labels <- names(arrays)
  if (is.null(array_labels)) {
    # Try to extract labels from array meta if no names
    array_labels <- sapply(arrays, function(arr) {
      meta <- attr(arr, "meta")
      if (!is.null(meta) && !is.null(meta$label)) {
        meta$label
      } else {
        "unknown"
      }
    })
  }

  combined_label <- paste0(type, " (", paste(array_labels, collapse = " + "), ")")

  summary_array <- set_array_meta(
    summary_array,
    dsl = NULL,
    variables = NULL,
    tags = list(type = type, summary = TRUE),
    label = combined_label
  )

  return(summary_array)
}

#' Resolve variable names using exact, prefix, and fuzzy matching
#'
#' @param data Data frame to resolve variables against
#' @param dpdict Optional data dictionary
#' @param var_names Character vector of variable names to resolve
#' @param threshold Fuzzy matching threshold (0-1, higher = more strict)
#' @param report Whether to return resolution details
#' @return Character vector of resolved variable names
#' @keywords internal
resolve_vars <- function(data, dpdict = NULL, var_names, threshold = 0.75, report = FALSE) {

  if (length(var_names) == 0) {
    return(character(0))
  }

  available_vars <- names(data)
  resolution_log <- vector("list", length(var_names))
  names(resolution_log) <- var_names

  resolve_one <- function(term) {
    term_chr <- as.character(term)

    # Skip if already exact match
    if (term_chr %in% available_vars) {
      resolution_log[[term_chr]] <<- list(method = "exact", result = term_chr)
      return(term_chr)
    }

    # Try startsWith matching
    starts_matches <- available_vars[startsWith(available_vars, term_chr)]
    if (length(starts_matches) == 1) {
      resolution_log[[term_chr]] <<- list(method = "prefix", result = starts_matches[1])
      return(starts_matches[1])
    } else if (length(starts_matches) > 1) {
      warning("Ambiguous prefix match for '", term_chr, "': ",
              paste(starts_matches, collapse = ", "), ". Using first match.")
      resolution_log[[term_chr]] <<- list(method = "prefix_ambiguous",
                                          result = starts_matches[1],
                                          alternatives = starts_matches[-1])
      return(starts_matches[1])
    }

    # Try fuzzy matching with adist
    if (length(available_vars) > 0) {
      distances <- utils::adist(term_chr, available_vars)
      max_chars <- pmax(nchar(term_chr), nchar(available_vars))
      scores <- 1 - (distances / max_chars)  # Convert to similarity scores

      best_idx <- which.max(scores)
      best_score <- scores[best_idx]

      if (best_score >= threshold) {
        resolution_log[[term_chr]] <<- list(method = "fuzzy",
                                            result = available_vars[best_idx],
                                            score = best_score)
        return(available_vars[best_idx])
      }
    }

    # If all else fails
    resolution_log[[term_chr]] <<- list(method = "failed", result = NA)
    stop("Could not resolve variable '", term_chr, "'. Available variables: ",
         paste(head(available_vars, 10), collapse = ", "),
         if (length(available_vars) > 10) "..." else "")
  }

  resolved <- vapply(var_names, resolve_one, character(1), USE.NAMES = FALSE)

  if (report) {
    attr(resolved, "resolution_log") <- resolution_log
  }

  resolved
}

#' Resolve a name to variable names in data
#'
#' Takes a name and resolves it to one or more variable names by checking:
#' 1. Exact variable match in data
#' 2. Pattern match for variable stems (optional)
#' 3. Question group match in dpdict (if provided)
#'
#' @param name Character string to resolve
#' @param data Data frame to search in
#' @param dpdict Optional data dictionary for question group lookup
#' @param allow_patterns Logical, whether to try pattern matching
#' @param error_context Character string describing what's being resolved (for error messages)
#' @return Character vector of variable names
#' @keywords internal
resolve_to_variables <- function(name, data, dpdict = NULL,
                                 allow_patterns = TRUE,
                                 error_context = "variable or group") {

  # Ensure name is character and not NA
  if (is.null(name) || is.na(name) || !is.character(name)) {
    stop("Invalid name provided for resolution: ", name)
  }

  # 1. Exact variable match
  if (name %in% names(data)) {
    return(name)
  }

  # 2. Pattern match (if allowed)
  if (allow_patterns) {
    # Try pattern with underscore and digits
    pattern_matches <- names(data)[grepl(paste0("^", name, "_\\d+$"), names(data))]
    if (length(pattern_matches) > 0) {
      return(pattern_matches)
    }

    # Also try pattern with any suffix after underscore
    pattern_matches <- names(data)[grepl(paste0("^", name, "_"), names(data))]
    if (length(pattern_matches) > 0) {
      return(pattern_matches)
    }
  }

  # 3. Question group (if dpdict available)
  if (!is.null(dpdict) && "question_group" %in% names(dpdict)) {
    # Exact match to question group
    group_vars <- dpdict$variable_names[
      !is.na(dpdict$question_group) &
        dpdict$question_group == name &
        !is.na(dpdict$variable_names)
    ]
    if (length(group_vars) > 0) {
      return(group_vars)
    }

    # Pattern match for question groups (e.g., "Q1" matches "Q1_a")
    if (allow_patterns) {
      matching_groups <- unique(dpdict$question_group[
        !is.na(dpdict$question_group) &
          grepl(paste0("^", name, "_"), dpdict$question_group)
      ])
      if (length(matching_groups) > 0) {
        group_vars <- dpdict$variable_names[
          dpdict$question_group %in% matching_groups &
            !is.na(dpdict$variable_names)
        ]
        if (length(group_vars) > 0) {
          return(group_vars)
        }
      }
    }
  }

  # 4. Construct helpful error message
  error_parts <- c(
    paste0("Could not resolve '", name, "' to ", error_context, "."),
    "Not found as variable name"
  )

  if (!is.null(dpdict)) {
    error_parts <- c(error_parts, "or question group")
  }

  if (allow_patterns) {
    error_parts <- c(error_parts, "or matching pattern")
  }

  available_vars <- head(names(data), 10)
  error_parts <- c(
    error_parts,
    paste0(". Available variables: ",
           paste(available_vars, collapse = ", "),
           if (length(names(data)) > 10) "..." else "")
  )

  stop(paste(error_parts, collapse = " "))
}