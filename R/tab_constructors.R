#' Internal registry for tab helpers and statistics
#' @keywords internal
.tab_registry <- new.env(parent = emptyenv())
.tab_registry$helpers <- list()
.tab_registry$stats <- list()
.tab_registry$sig_tests <- list()

#' Low-level constructor for tab helpers
#' @param id Character identifier for the helper
#' @param processor Function that processes the helper
#' @param ... Additional attributes
#' @keywords internal
new_tab_helper <- function(id, processor, ...) {
  stopifnot(is.character(id), length(id) == 1, nzchar(id))
  stopifnot(is.function(processor))

  structure(
    list(
      id = id,
      processor = processor,
      ...
    ),
    class = "tab_helper"
  )
}

#' Low-level constructor for tab statistics
#' @param id Character identifier for the statistic
#' @param processor Function that processes the statistic
#' @param format_fn Function to format values
#' @param requires_values Whether 'values' parameter required
#' @param base_label Label for Base row
#' @param ... Additional attributes
#' @keywords internal
new_tab_stat <- function(id, processor,
                         summary_row = NULL,
                         summary_col = NULL,
                         format_fn = NULL,
                         requires_values = FALSE,
                         base_label = "Base (n)",
                         base_orientation = "column",
                         ...) {
  stopifnot(is.character(id), length(id) == 1, nzchar(id))
  stopifnot(is.function(processor))

  # Default formatter if none provided
  if (is.null(format_fn)) {
    format_fn <- function(x) as.character(x)
  }

  structure(
    list(
      id = id,
      processor = processor,
      summary_row = summary_row,
      summary_col = summary_col,
      format_fn = format_fn,
      requires_values = requires_values,
      base_label = base_label,
      base_orientation = base_orientation,
      ...
    ),
    class = "tab_stat"
  )
}

#' Create and register a custom helper
#' @param id Character identifier for the helper
#' @param processor Function that processes the helper specification
#' @param ... Additional attributes
#' @return The created helper object (invisibly)
#' @export
create_helper <- function(id, processor, ...) {
  if (id %in% names(.tab_registry$helpers)) {
    warning("Overwriting existing helper '", id, "'")
  }

  obj <- new_tab_helper(id, processor, ...)
  .tab_registry$helpers[[id]] <- obj
  invisible(obj)
}

#' Create and register a custom statistic
#' @param id Character identifier for the statistic
#' @param processor Function that processes the statistic
#' @param summary_row Type of summary row ("NET", "Avg", "Total", or NULL)
#' @param summary_col Type of summary column ("NET", "Avg", "Total", or NULL)
#' @param format_fn Function to format values for display
#' @param requires_values Whether this statistic requires a 'values' parameter
#' @param base_label Label for the base row
#' @param ... Additional attributes
#' @return The created statistic object (invisibly)
#' @export
create_statistic <- function(id, processor,
                             summary_row = NULL,
                             summary_col = NULL,
                             format_fn = NULL,
                             requires_values = FALSE,
                             base_label = "Base (n)",
                             base_orientation = "column",
                             ...) {
  if (id %in% names(.tab_registry$stats)) {
    warning("Overwriting existing statistic '", id, "'")
  }

  obj <- new_tab_stat(id          = id,
                      processor    = processor,
                      summary_row  = summary_row,
                      summary_col  = summary_col,
                      format_fn    = format_fn,
                      requires_values = requires_values,
                      base_label   = base_label,
                      base_orientation = base_orientation,
                      ...)

  .tab_registry$stats[[id]] <- obj
  invisible(obj)
}

#' Get registered helper by ID
#' @param id Character identifier
#' @keywords internal
get_helper <- function(id) {
  .tab_registry$helpers[[id]]
}

#' Get registered statistic by ID
#' @param id Character identifier
#' @keywords internal
get_statistic <- function(id) {
  .tab_registry$stats[[id]]
}

# Add these functions to the existing R/tab-constructors.R file:

#' Clear all registered helpers and statistics (useful for testing)
#' @export
clear_tab_registry <- function() {
  .tab_registry$helpers <- list()
  .tab_registry$stats <- list()
  invisible(NULL)
}

#' List all registered helpers
#' @export
list_tab_helpers <- function() {
  names(.tab_registry$helpers)
}

#' List all registered statistics
#' @export
list_tab_statistics <- function() {
  names(.tab_registry$stats)
}


#' Determine the type of a variable array for statistics validation
#'
#' @param array Numeric vector representing a variable array
#' @return Character string: "categorical", "numeric", or "other"
#' @export
#' @examples
#' get_variable_array_type(c(0, 1, 0, 1, NA))  # "categorical"
#' get_variable_array_type(c(1.5, 2.3, 4.1))   # "numeric"
#' get_variable_array_type(c("a", "b"))         # "other"
get_variable_array_type <- function(array) {
  if (!is.numeric(array)) {
    return("other")
  }

  non_na_values <- array[!is.na(array)]
  if (length(non_na_values) == 0) {
    return("other")
  }

  # Check if all values are exactly 0 or 1
  if (all(non_na_values %in% c(0, 1))) {
    return("categorical")
  }

  return("numeric")
}


#' Low-level constructor for significance tests
#' @keywords internal
new_tab_sig_test <- function(id, processor,
                             name = NULL,
                             description = NULL,
                             ...) {
  stopifnot(is.character(id), length(id) == 1, nzchar(id))
  stopifnot(is.function(processor))

  structure(
    list(
      id = id,
      processor = processor,
      name = name %||% id,
      description = description,
      ...
    ),
    class = "tab_sig_test"
  )
}

#' Create and register a significance test
#' @param id Character identifier for the test
#' @param processor Function that performs the test
#' @param name Human-readable name for the test
#' @param description Description of when to use this test
#' @param ... Additional attributes
#' @return The created test object (invisibly)
#' @export
create_significance_test <- function(id, processor,
                                     name = NULL,
                                     description = NULL,
                                     ...) {
  if (id %in% names(.tab_registry$sig_tests)) {
    warning("Overwriting existing significance test '", id, "'")
  }

  obj <- new_tab_sig_test(id = id,
                          processor = processor,
                          name = name,
                          description = description,
                          ...)

  .tab_registry$sig_tests[[id]] <- obj
  invisible(obj)
}

#' Get registered significance test by ID
#' @keywords internal
get_significance_test <- function(id) {
  .tab_registry$sig_tests[[id]]
}

#' List all registered significance tests
#' @export
list_tab_significance_tests <- function() {
  names(.tab_registry$sig_tests)
}
