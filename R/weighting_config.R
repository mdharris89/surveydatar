#' Create weighting configuration object
#'
#' @description
#' Creates a configuration object for the weighting system that defines
#' column names, constraint type patterns, and backward compatibility mappings.
#' This allows the weighting functions to work with any dataset structure
#' without hardcoding specific column names.
#'
#' @param strata_col Name of stratification column (default "Country").
#'   This column is used for country/market/region-specific constraints
#'   and tolerance calculations in percentage_point mode.
#' @param id_col Name of respondent ID column (default "respondent_id").
#'   Auto-generated if missing in data.
#' @param demo_var_mapping Named list mapping friendly names to actual column names.
#'   Example: list(Gender = "SC1", Age = "SC2merged").
#'   Creates alias columns for easier constraint specification.
#' @param legacy_mappings Named list of automatic column renamings for backward compatibility.
#'   Default: list(QCOUNTRY = "Country") converts QCOUNTRY to Country automatically.
#'   Set to NULL to disable legacy mappings.
#' @param constraint_patterns Named list of regex patterns for identifying constraint types.
#'   Used for diagnostic reporting and constraint categorization.
#'   Default: list(sum = "^Sum:|^Total:", occasion = "^Occ:", demo = "^Demo:")
#'
#' @return A weighting_config object (list with class "weighting_config")
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Default configuration (works with "Country" column)
#' config <- create_weighting_config()
#'
#' # Custom configuration for data using "market" instead of "Country"
#' config <- create_weighting_config(
#'   strata_col = "market",
#'   id_col = "resp_id"
#' )
#'
#' # With demographic variable mapping
#' config <- create_weighting_config(
#'   demo_var_mapping = list(
#'     Gender = "SC1",
#'     Age = "SC2merged",
#'     Region = "QREGION"
#'   )
#' )
#'
#' # Disable legacy mappings
#' config <- create_weighting_config(legacy_mappings = NULL)
#'
#' # Use in weighting
#' result <- run_unified_weighting(
#'   raw_data = my_data,
#'   stages = list(...),
#'   config = config
#' )
#' }
create_weighting_config <- function(
  strata_col = "Country",
  id_col = "respondent_id",
  demo_var_mapping = NULL,
  legacy_mappings = list(QCOUNTRY = "Country"),
  constraint_patterns = list(
    sum = "^Sum:|^Total:",
    occasion = "^Occ:",
    demo = "^Demo:"
  )
) {
  # Validation
  if (!is.character(strata_col) || length(strata_col) != 1) {
    stop("strata_col must be a single character string")
  }
  
  if (!is.character(id_col) || length(id_col) != 1) {
    stop("id_col must be a single character string")
  }
  
  if (!is.null(demo_var_mapping) && !is.list(demo_var_mapping)) {
    stop("demo_var_mapping must be a named list or NULL")
  }
  
  if (!is.null(legacy_mappings) && !is.list(legacy_mappings)) {
    stop("legacy_mappings must be a named list or NULL")
  }
  
  if (!is.list(constraint_patterns)) {
    stop("constraint_patterns must be a named list")
  }
  
  structure(
    list(
      cols = list(
        strata = strata_col,
        id = id_col
      ),
      demo_var_mapping = demo_var_mapping,
      legacy_mappings = legacy_mappings,
      patterns = constraint_patterns
    ),
    class = "weighting_config"
  )
}

#' Get default weighting configuration
#'
#' @description
#' Internal function that returns a default configuration object.
#' Used when config is NULL in weighting functions.
#'
#' @return A weighting_config object with default settings
#' @noRd
default_weighting_config <- function() {
  create_weighting_config()
}

#' Print method for weighting_config
#'
#' @param x A weighting_config object
#' @param ... Additional arguments (ignored)
#' @export
print.weighting_config <- function(x, ...) {
  cat("Weighting Configuration\n")
  cat("=======================\n\n")
  
  cat("Column Names:\n")
  cat(sprintf("  Strata: %s\n", x$cols$strata))
  cat(sprintf("  ID: %s\n", x$cols$id))
  
  if (!is.null(x$demo_var_mapping) && length(x$demo_var_mapping) > 0) {
    cat("\nDemographic Variable Mappings:\n")
    for (name in names(x$demo_var_mapping)) {
      cat(sprintf("  %s -> %s\n", name, x$demo_var_mapping[[name]]))
    }
  }
  
  if (!is.null(x$legacy_mappings) && length(x$legacy_mappings) > 0) {
    cat("\nLegacy Mappings (backward compatibility):\n")
    for (name in names(x$legacy_mappings)) {
      cat(sprintf("  %s -> %s\n", name, x$legacy_mappings[[name]]))
    }
  }
  
  cat("\nConstraint Type Patterns:\n")
  for (name in names(x$patterns)) {
    cat(sprintf("  %s: %s\n", name, x$patterns[[name]]))
  }
  
  invisible(x)
}

