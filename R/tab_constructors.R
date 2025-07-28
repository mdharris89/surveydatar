#' Internal registry for tab helpers and statistics
#' @keywords internal
.tab_registry <- new.env(parent = emptyenv())
.tab_registry$helpers <- list()
.tab_registry$stats <- list()
.tab_registry$sig_tests <- list()
.tab_registry$gates <- list()

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
#' @param base_calculator Function that calculates base for each cell
#' @param format_fn Function to format values
#' @param requires_values Whether 'values' parameter required
#' @param base_label Label for Base row
#' @param vectorized_processor Optionally use vectorised statistic computation
#' @param ... Additional attributes
#' @keywords internal
new_tab_stat <- function(id, processor,
                         base_calculator,
                         summary_row = NULL,
                         summary_col = NULL,
                         summary_row_calculator = NULL,
                         summary_col_calculator = NULL,
                         format_fn = NULL,
                         requires_values = FALSE,
                         base_label = "Base (n)",
                         vectorized_processor = NULL,
                         ...) {
  stopifnot(is.character(id), length(id) == 1, nzchar(id))
  stopifnot(is.function(processor))
  stopifnot(is.function(base_calculator))

  # Default formatter if none provided
  if (is.null(format_fn)) {
    format_fn <- function(x) as.character(x)
  }

  structure(
    list(
      id = id,
      processor = processor,
      base_calculator = base_calculator,
      vectorized_processor = vectorized_processor,
      summary_row = summary_row,
      summary_col = summary_col,
      summary_row_calculator = summary_row_calculator,
      summary_col_calculator = summary_col_calculator,
      format_fn = format_fn,
      requires_values = requires_values,
      base_label = base_label,
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
#' @param base_calculator Function that calculates base for each cell
#' @param summary_row Type of summary row ("NET", "Avg", "Total", or NULL)
#' @param summary_col Type of summary column ("NET", "Avg", "Total", or NULL)
#' @param format_fn Function to format values for display
#' @param requires_values Whether this statistic requires a 'values' parameter
#' @param base_label Label for the base row
#' @param vectorized_processor Optionally use vectorised statistic computation
#' @param ... Additional attributes
#' @return The created statistic object (invisibly)
#' @export
create_statistic <- function(id, processor,
                             base_calculator,
                             summary_row = NULL,
                             summary_col = NULL,
                             summary_row_calculator = NULL,
                             summary_col_calculator = NULL,
                             format_fn = NULL,
                             requires_values = FALSE,
                             base_label = "Base (n)",
                             vectorized_processor = NULL,
                             ...) {
  if (id %in% names(.tab_registry$stats)) {
    warning("Overwriting existing statistic '", id, "'")
  }

  obj <- new_tab_stat(id          = id,
                      processor    = processor,
                      base_calculator = base_calculator,
                      vectorized_processor = vectorized_processor,
                      summary_row  = summary_row,
                      summary_col  = summary_col,
                      summary_row_calculator = summary_row_calculator,
                      summary_col_calculator = summary_col_calculator,
                      format_fn    = format_fn,
                      requires_values = requires_values,
                      base_label   = base_label,
                      ...)

  # Validate base calculator
  tryCatch({
    # Test with dummy data
    test_result <- obj$base_calculator(
      base_array = c(1, 1, 1),
      row_array = c(1, 0, 1),
      col_array = c(1, 1, 0),
      values_array = c(1, 2, 3)
    )

    if (!is.numeric(test_result) || length(test_result) != 1) {
      stop("base_calculator must return a single numeric value")
    }

    if (test_result < 0) {
      stop("base_calculator must return a non-negative value")
    }
  }, error = function(e) {
    stop("Invalid base_calculator for statistic '", id, "': ", e$message)
  })

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

#' Clear all registered helpers and statistics (useful for testing)
#' @export
clear_tab_registry <- function() {
  .tab_registry$helpers <- list()
  .tab_registry$stats <- list()
  .tab_registry$gates <- list()
  if ("sig_tests" %in% names(.tab_registry)) {
    .tab_registry$sig_tests <- list()
  }
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


#' Built-in base calculator: Column total
#' @description Base is the total count in the column
#' @keywords internal
base_column_total <- function(base_array, row_array, col_array, values_array = NULL, ...) {
  sum(base_array * col_array, na.rm = TRUE)
}

#' Built-in base calculator: Row total
#' @description Base is the total count in the row
#' @keywords internal
base_row_total <- function(base_array, row_array, col_array, values_array = NULL, ...) {
  sum(base_array * row_array, na.rm = TRUE)
}

#' Built-in base calculator: Cell count
#' @description Base is the count in the specific cell
#' @keywords internal
base_cell_count <- function(base_array, row_array, col_array, values_array = NULL, ...) {
  sum(base_array * row_array * col_array, na.rm = TRUE)
}

#' Built-in base calculator: Grand total
#' @description Base is the total count across all data
#' @keywords internal
base_grand_total <- function(base_array, row_array, col_array, values_array = NULL, ...) {
  sum(base_array, na.rm = TRUE)
}

#' Built-in base calculator: Column total excluding NA values
#' @description Base is the column total excluding NA in values array
#' @keywords internal
base_column_total_valid <- function(base_array, row_array, col_array, values_array = NULL, ...) {
  if (is.null(values_array)) {
    return(sum(base_array * col_array, na.rm = TRUE))
  }
  valid_mask <- !is.na(values_array)
  sum(base_array * col_array * valid_mask, na.rm = TRUE)
}

#' Built-in base calculator: Row total excluding NA values
#' @description Base is the row total excluding NA in values array
#' @keywords internal
base_row_total_valid <- function(base_array, row_array, col_array, values_array = NULL, ...) {
  if (is.null(values_array)) {
    return(sum(base_array * row_array, na.rm = TRUE))
  }
  valid_mask <- !is.na(values_array)
  sum(base_array * row_array * valid_mask, na.rm = TRUE)
}

#' Built-in base calculator: Cell count excluding NA values
#' @description Base is the cell count excluding NA in values array
#' @keywords internal
base_cell_count_valid <- function(base_array, row_array, col_array, values_array = NULL, ...) {
  if (is.null(values_array)) {
    return(sum(base_array * row_array * col_array, na.rm = TRUE))
  }
  valid_mask <- !is.na(values_array)
  sum(base_array * row_array * col_array * valid_mask, na.rm = TRUE)
}

#' Export base calculators for user access
#' @rdname base_calculators
#' @name base_calculators
NULL

#' @export
#' @rdname base_calculators
base_column_total <- base_column_total

#' @export
#' @rdname base_calculators
base_row_total <- base_row_total

#' @export
#' @rdname base_calculators
base_cell_count <- base_cell_count

#' @export
#' @rdname base_calculators
base_grand_total <- base_grand_total

#' @export
#' @rdname base_calculators
base_column_total_valid <- base_column_total_valid

#' @export
#' @rdname base_calculators
base_row_total_valid <- base_row_total_valid

#' @export
#' @rdname base_calculators
base_cell_count_valid <- base_cell_count_valid

#' Low-level constructor for gate factories
#' @param id Character identifier for the gate
#' @param factory_fn Function that returns a gate function
#' @param ... Additional attributes
#' @keywords internal
new_tab_gate <- function(id, factory_fn, ...) {
  stopifnot(is.character(id), length(id) == 1, nzchar(id))
  stopifnot(is.function(factory_fn))

  structure(
    list(
      id = id,
      factory_fn = factory_fn,
      ...
    ),
    class = "tab_gate"
  )
}

#' Create and register a gate function
#' @param id Character identifier for the gate
#' @param factory_fn Function that returns a gate function when called with a field name
#' @param ... Additional attributes
#' @return The created gate object (invisibly)
#' @export
create_gate <- function(id, factory_fn, ...) {
  if (id %in% names(.tab_registry$gates)) {
    warning("Overwriting existing gate '", id, "'")
  }

  # Validate that factory_fn returns a function
  tryCatch({
    test_gate <- factory_fn("test_field")
    if (!is.function(test_gate)) {
      stop("factory_fn must return a function")
    }
    # Test the returned gate function signature
    test_result <- test_gate(list(), list(), list())
    if (!is.logical(test_result) || length(test_result) != 1) {
      stop("gate function must return a single logical value")
    }
  }, error = function(e) {
    stop("Invalid factory_fn for gate '", id, "': ", e$message)
  })

  obj <- new_tab_gate(id = id, factory_fn = factory_fn, ...)
  .tab_registry$gates[[id]] <- obj
  invisible(obj)
}

#' Get registered gate by ID
#' @param id Character identifier
#' @return Gate factory function or NULL if not found
#' @keywords internal
get_gate <- function(id) {
  gate_obj <- .tab_registry$gates[[id]]
  if (!is.null(gate_obj)) {
    return(gate_obj$factory_fn)
  }
  NULL
}

#' List all registered gates
#' @return Character vector of gate IDs
#' @export
list_tab_gates <- function() {
  names(.tab_registry$gates)
}

# Default gate that always returns TRUE
# Used when no gate is specified - allows all cells to be computed
# @keywords internal
always_true_gate <- function(...) TRUE
