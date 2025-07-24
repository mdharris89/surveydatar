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
