# Tab Constructors and Registry
#
# Low-level constructors for tab objects (helpers, statistics, macros).

#' Internal registry for tab helpers and statistics
#' @keywords internal
.tab_registry <- new.env(parent = emptyenv())
.tab_registry$helpers <- list()
.tab_registry$stats <- list()
.tab_registry$sig_tests <- list()
.tab_registry$macros <- list()

#' Low-level constructor for tab macros
#' @param id Character identifier for the macro
#' @param expander Function that expands the macro into tab() parameters
#' @param description Optional description of the macro
#' @keywords internal
new_tab_macro <- function(id, expander, description = NULL) {
  stopifnot(is.character(id), length(id) == 1, nzchar(id))
  stopifnot(is.function(expander))

  structure(
    list(
      id = id,
      expander = expander,
      description = description
    ),
    class = "tab_macro_definition"
  )
}

#' Low-level constructor for tab helpers
#' @param id Character identifier for the helper
#' @param processor Function that processes the helper
#' @param returns_specs Logical, if TRUE processor returns column specs not arrays
#' @param ... Additional attributes
#' @keywords internal
new_tab_helper <- function(id, processor, returns_specs = FALSE, ...) {
  stopifnot(is.character(id), length(id) == 1, nzchar(id))
  stopifnot(is.function(processor))
  stopifnot(is.logical(returns_specs), length(returns_specs) == 1)

  structure(
    list(
      id = id,
      processor = processor,
      returns_specs = returns_specs,
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
#' @param returns_specs Logical, if TRUE processor returns column specs not arrays
#' @param ... Additional attributes
#' @return The created helper object (invisibly)
#' @export
create_helper <- function(id, processor, returns_specs = FALSE, ...) {
  if (id %in% names(.tab_registry$helpers)) {
    warning("Overwriting existing helper '", id, "'")
  }

  obj <- new_tab_helper(id, processor, returns_specs = returns_specs, ...)
  .tab_registry$helpers[[id]] <- obj
  invisible(obj)
}

#' Create a macro call object for tab()
#'
#' Macro constructor functions should call this helper to capture the macro
#' identifier and arguments without evaluating them eagerly.
#'
#' @param macro_id Registered macro identifier
#' @param ... Arguments captured for the macro expander
#' @return An object of class `tab_macro`
#' @export
create_macro_call <- function(macro_id, ...) {
  structure(
    list(
      macro_id = macro_id,
      args = list(...),
      call = match.call(expand.dots = FALSE)
    ),
    class = "tab_macro"
  )
}

#' Create a measure specification for multi-measure tab() calls
#'
#' Measures are used with `tab(measures = list(...))` to compute multiple
#' statistics (and optional value variables) in one table.
#'
#' @param statistic Statistic identifier (character) or `tab_stat` object.
#' @param values Optional values variable name for value-based statistics.
#' @param id Optional stable id for the measure. If omitted, tab() creates one.
#' @param label Optional display label for the measure block.
#' @return A `tab_measure` object.
#' @export
measure <- function(statistic, values = NULL, id = NULL, label = NULL) {
  if (!(is.character(statistic) || inherits(statistic, "tab_stat"))) {
    stop("measure(): statistic must be a character id or tab_stat object")
  }
  if (is.character(statistic) && length(statistic) != 1) {
    stop("measure(): statistic must be length 1 when provided as character")
  }
  if (!is.null(values) && (!is.character(values) || length(values) != 1 || !nzchar(values))) {
    stop("measure(): values must be NULL or a non-empty character scalar")
  }
  if (!is.null(id) && (!is.character(id) || length(id) != 1 || !nzchar(id))) {
    stop("measure(): id must be NULL or a non-empty character scalar")
  }
  if (!is.null(label) && (!is.character(label) || length(label) != 1 || !nzchar(label))) {
    stop("measure(): label must be NULL or a non-empty character scalar")
  }

  structure(
    list(
      statistic = statistic,
      values = values,
      id = id,
      label = label
    ),
    class = "tab_measure"
  )
}

#' Create and register a tab macro
#'
#' @param id Character identifier for the macro
#' @param expander Function that expands macro calls into tab() parameter overrides
#' @param description Optional description of the macro
#' @return The created macro definition (invisibly)
#' @export
create_macro <- function(id, expander, description = NULL) {
  if (id %in% names(.tab_registry$macros)) {
    warning("Overwriting existing macro '", id, "'")
  }

  obj <- new_tab_macro(id, expander, description = description)
  .tab_registry$macros[[id]] <- obj
  invisible(obj)
}

#' Create and register a custom statistic
#' @param id Character identifier for the statistic
#' @param processor Function that processes the statistic
#' @param base_calculator Function that calculates base for each cell
#' @param summary_row Type of summary row ("NET", "Avg", "Total", or NULL)
#' @param summary_col Type of summary column ("NET", "Avg", "Total", or NULL)
#' @param summary_row_calculator Optional base calculator used for the summary row.
#' @param summary_col_calculator Optional base calculator used for the summary column.
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
      row_m = c(1, 0, 1),
      row_u = c(1, 1, 1),
      col_m = c(1, 1, 0),
      col_u = c(1, 1, 1),
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

#' Get registered macro by ID
#' @param id Character identifier
#' @keywords internal
get_macro <- function(id) {
  .tab_registry$macros[[id]]
}

#' Clear all registered helpers and statistics (useful for testing)
#' @export
clear_tab_registry <- function() {
  .tab_registry$helpers <- list()
  .tab_registry$stats <- list()
  .tab_registry$macros <- list()
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

#' List all registered macros
#' @export
list_tab_macros <- function() {
  names(.tab_registry$macros)
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

## Built-in base calculator: Column total
## Base is the total count in the column
base_column_total <- function(base_array, row_m, row_u, col_m, col_u,
                              values_array = NULL, ...) {
  intersection <- base_array * col_m * row_u
  sum(intersection, na.rm = TRUE)
}

## Built-in base calculator: Row total
## Base is the total count in the row
base_row_total <- function(base_array, row_m, row_u, col_m, col_u,
                           values_array = NULL, ...) {
  # Row base is defined by row membership intersected with the column eligibility.
  intersection <- base_array * row_m * col_u
  sum(intersection, na.rm = TRUE)
}

## Built-in base calculator: Cell count
## Base is the count in the specific cell
base_cell_count <- function(base_array, row_m, row_u, col_m, col_u,
                            values_array = NULL, ...) {
  intersection <- base_array * row_m * col_m
  sum(intersection, na.rm = TRUE)
}

## Built-in base calculator: Grand total
## Base is the total count across all data
base_grand_total <- function(base_array, row_m, row_u, col_m, col_u,
                             values_array = NULL, ...) {
  sum(base_array, na.rm = TRUE)
}

## Built-in base calculator: Pairwise valid total
## Base is the total count for respondents valid for both row and column universes
base_pair_total <- function(base_array, row_m, row_u, col_m, col_u,
                            values_array = NULL, ...) {
  sum(base_array * row_u * col_u, na.rm = TRUE)
}

## Built-in base calculator: Column total excluding NA values
## Base is the column total excluding NA in values array
base_column_total_valid <- function(base_array, row_m, row_u, col_m, col_u,
                                    values_array = NULL, ...) {
  intersection <- base_array * col_m * row_u
  if (!is.null(values_array)) {
    intersection <- intersection * (!is.na(values_array))
  }
  sum(intersection, na.rm = TRUE)
}

## Built-in base calculator: Row total excluding NA values
## Base is the row total excluding NA in values array
base_row_total_valid <- function(base_array, row_m, row_u, col_m, col_u,
                                 values_array = NULL, ...) {
  intersection <- base_array * row_m * col_u
  if (!is.null(values_array)) {
    intersection <- intersection * (!is.na(values_array))
  }
  sum(intersection, na.rm = TRUE)
}

## Built-in base calculator: Cell count excluding NA values
## Base is the cell count excluding NA in values array
base_cell_count_valid <- function(base_array, row_m, row_u, col_m, col_u,
                                  values_array = NULL, ...) {
  intersection <- base_array * row_m * col_m
  if (!is.null(values_array)) {
    intersection <- intersection * (!is.na(values_array))
  }
  sum(intersection, na.rm = TRUE)
}

#' Base calculator functions
#'
#' Base calculators are functions that compute the base (denominator, typically an
#' unweighted count) for each cell in a \code{\link{tab}} result. They are used
#' internally by statistics, and can also be supplied when defining custom
#' statistics (see \code{\link{create_statistic}}).
#'
#' All base calculators share the same interface: they take respondent-level
#' \code{base_array} plus row/column membership and eligibility masks, and return a
#' single numeric value.
#'
#' @param base_array Numeric vector (usually 0/1/NA) defining the overall base.
#' @param row_m Numeric vector (usually 0/1) defining row membership.
#' @param row_u Numeric vector (0/1) defining row eligibility for denominators.
#' @param col_m Numeric vector (usually 0/1) defining column membership.
#' @param col_u Numeric vector (0/1) defining column eligibility/base context.
#' @param values_array Optional numeric vector of values for statistics that
#'   require a valid-values base.
#' @param ... Reserved for future use.
#'
#' @return A single numeric value.
#'
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
