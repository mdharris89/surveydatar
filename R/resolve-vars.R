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
