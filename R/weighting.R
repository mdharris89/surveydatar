# HELPER FUNCTIONS ------------------------------------------------------------

#' Check if weighting debug mode is enabled
#' @return Logical indicating if debug mode is active
#' @noRd
is_debug <- function() {
  getOption("surveydatar.weighting.debug", default = FALSE)
}

# DOCUMENTATION ----------------------------------------------------------------

#' @title Unified survey weighting with quadratic optimisation
#'
#' @description
#' Bias-corrected constrained quadratic optimization for survey weighting.
#' Solves: min sum((w_i - w*_i)^2) subject to linear constraints with tolerances.
#'
#' @section Data Flow:
#' raw_data -> run_unified_weighting() -> process constraints/helpers ->
#' build sparse matrix -> CVXR optimization -> weighted data + diagnostics
#'
#' @section Data Format Requirements:
#'
#' **Input:**
#' - respondent_id (auto-generated if missing, configurable via config$cols$id)
#' - Variables referenced in constraints must exist
#'
#' **Configuration:**
#' - Column names are configurable via config parameter (see create_weighting_config())
#' - Default strata column: "Country" (can be changed to "market", "region", etc.)
#' - Default ID column: "respondent_id"
#' - Legacy mappings support backward compatibility (e.g., QCOUNTRY -> Country)
#' - Constraint type patterns are configurable for custom naming conventions
#' - Demographic variable mappings enable user-friendly constraint specification
#'
#' @section Core Objects:
#'
#' **Constraint object:**
#' make_constraint(
#'   formula,              # LHS ~ RHS
#'   type,                 # "demographic", "occasion", etc.
#'   country = NA,         # Country if applicable
#'   metadata = list(),    # Additional info
#'   tolerance = NULL      # Scalar or c(lower, upper)
#' )
#'
#' **Helper pattern:**
#' my_helper <- function(...) {
#'   structure(list(
#'     processor = process_my_helper,
#'     params = list(...)
#'   ), class = c("weighting_helper", "list"))
#' }
#'
#' **Target weights:**
#' - weighting_target_function: Returns selection probabilities -> alpha blending
#' - Numeric vector: Direct target weights (length = nrow)
#' - NULL: Uniform weights
#'
#' @section Usage:
#'
#' run_unified_weighting(
#'   raw_data,
#'   stages = list(          # Single stage or list of stages
#'     constraints = list(), # Formulas and/or helpers
#'     weighting_target = NULL,
#'     alpha = 0.7,          # Bias correction emphasis
#'     tolerance = 0.01      # Overrides default
#'   ),
#'   config = NULL,
#'   alpha = 0.7,            # Default alpha
#'   tolerance = 0.03,       # Default tolerance
#'   cap = 3.5,              # Weight cap
#'   verbose = TRUE
#' )
#'
#' @section Tolerance Configuration:
#'
#' create_tol_config(
#'   default = 0.01,
#'   mode = "relative"       # or "percentage_point"
#' )
#'
#' - relative: % of target value
#' - percentage_point: fixed sample count
#'
#' @section Formula Syntax:
#'
#' age == "18-24" ~ 150                    # Basic
#' gender == "M" ~ 500 %+-% 0.02           # With tolerance
#' education ~ c("HS" = 300, "BA" = 400)   # Auto-expansion
#'
#' @section Output:
#'
#' Data frame with columns:
#' - respondent_id
#' - weight (final)
#' - stage_N_weight (if multi-stage)
#'
#' Attributes: ess, efficiency, cv, solver_status, constraint_report
#'
#' @section Error Handling:
#'
#' Infeasible problems return "weights_infeasible" object with:
#' - Detailed constraint diagnostics
#' - Identification of problematic constraints
#' - Suggested remedies

##### CORE FUNCTIONS #####
# SECTION 0: UTILITY FUNCTIONS ------------------------------------------------

#' Get label-to-value mapping for a variable
#' @noRd
get_label_value_map <- function(var) {
  if (inherits(var, "haven_labelled")) {
    labels <- attr(var, "labels")
    if (!is.null(labels)) {
      # labels is c("Male" = 1, "Female" = 2)
      # names(labels) is c("Male", "Female")
      # as.character(labels) is c("1", "2")
      # We want c("Male" = "1", "Female" = "2")
      return(setNames(as.character(labels), names(labels)))
    }
  } else if (!is.null(attr(var, "labels"))) {
    labels <- attr(var, "labels")
    return(setNames(as.character(labels), names(labels)))
  }
  return(NULL)
}

#' Extract variables from constraint name string
#' @noRd
extract_vars_from_constraint_name <- function(name) {
  # Extract variable names from strings like "A2_1 == 1 & QCOUNTRY == 2"
  # or "Gender == 'Male'"

  # Remove operators and values
  cleaned <- gsub("==|!=|<=|>=|<|>", " ", name)
  cleaned <- gsub("'[^']*'", "", cleaned)  # Remove quoted strings
  cleaned <- gsub("\"[^\"]*\"", "", cleaned)  # Remove double-quoted strings
  cleaned <- gsub("[0-9]+", "", cleaned)  # Remove numbers
  cleaned <- gsub("&|\\||\\*", " ", cleaned)  # Remove logical operators
  cleaned <- gsub("\\(|\\)", " ", cleaned)  # Remove parentheses

  # Split and clean
  vars <- trimws(unlist(strsplit(cleaned, "\\s+")))
  vars <- vars[vars != "" & !vars %in% c("TRUE", "FALSE", "NA")]

  return(unique(vars))
}

#' Extract strata value from a formula expression
#' @param parsed_formula Parsed formula object
#' @param data Data frame
#' @param valid_rows Indices of rows that match the constraint
#' @param strata_col Name of the strata column (default "Country")
#' @return Strata value if constraint is strata-specific, NA otherwise
#' @noRd
extract_strata_from_formula <- function(parsed_formula, data, valid_rows, strata_col = "Country") {
  # Convert expression to string for analysis
  lhs_str <- deparse(parsed_formula$lhs_expr, width.cutoff = 500)

  # Look for strata_col == "value" patterns (build pattern dynamically)
  pattern1 <- sprintf('%s\\s*==\\s*"([^"]+)"', strata_col)
  matches <- regmatches(lhs_str, regexec(pattern1, lhs_str))

  if (length(matches[[1]]) > 1) {
    return(matches[[1]][2])
  }

  # Also check for 'strata_col == value' without quotes (for variables)
  pattern2 <- sprintf("%s\\s*==\\s*([^\\s&*]+)", strata_col)
  matches2 <- regmatches(lhs_str, regexec(pattern2, lhs_str))

  if (length(matches2[[1]]) > 1) {
    strata_val <- matches2[[1]][2]
    # If it's not quoted, it might be a variable - try to evaluate it
    if (!grepl('"', strata_val) && !grepl("'", strata_val)) {
      # Check if all valid rows have the same strata value
      if (length(valid_rows) > 0 && strata_col %in% names(data)) {
        strata_values <- unique(data[[strata_col]][valid_rows])
        if (length(strata_values) == 1) {
          return(as.character(strata_values[1]))
        }
      }
    }
  }

  # If we can't determine strata from formula, check if all matching rows
  # belong to the same strata
  if (length(valid_rows) > 0 && strata_col %in% names(data)) {
    strata_values <- unique(data[[strata_col]][valid_rows])
    if (length(strata_values) == 1) {
      return(as.character(strata_values[1]))
    }
  }

  return(NA_character_)
}

# SECTION 1: CONSTRAINT OBJECTS ------------------------------------------------
#' Create a rich constraint object
#'
#' @description
#' Creates a self-describing constraint that carries all its metadata,
#' eliminating the need for pattern matching and scattered attributes.
#'
#' @param formula Formula object specifying LHS ~ RHS
#' @param type Character string identifying constraint type (e.g., "occasion", "share", "demographic")
#' @param country Country name if constraint is country-specific, NA otherwise
#' @param metadata List of additional metadata (doc_num, category, description, etc.)
#' @param tolerance Numeric tolerance or NULL to use stage/global default
#'
#' @return A constraint object of class "weighting_constraint"
#' @export
make_constraint <- function(
    formula,
    type,
    country = NA_character_,
    metadata = list(),
    tolerance = NULL
) {
  # Validate inputs
  if (!inherits(formula, "formula")) {
    stop("formula must be a formula object")
  }

  if (!is.character(type) || length(type) != 1) {
    stop("type must be a single character string")
  }

  if (!is.na(country) && (!is.character(country) || length(country) != 1)) {
    stop("country must be NA or a single character string")
  }

  if (!is.list(metadata)) {
    stop("metadata must be a list")
  }

  if (!is.null(tolerance) &&
      (!is.numeric(tolerance) || !(length(tolerance) %in% c(1, 2)))) {
    stop("tolerance must be numeric: either a single value (symmetric) ",
         "or a vector of length 2 (c(lower, upper) for asymmetric).")
  }

  structure(
    list(
      formula = formula,
      type = type,
      country = country,
      metadata = metadata,
      tolerance = tolerance,
      # Auto-extract variables for convenience - eliminates need for separate extraction
      variables = all.vars(formula)
    ),
    class = c("weighting_constraint", "list")
  )
}

#' Print method for constraints
#' @export
print.weighting_constraint <- function(x, ...) {
  cat("Weighting Constraint:\n")
  cat("  Formula:", deparse(x$formula), "\n")
  cat("  Type:", x$type, "\n")
  if (!is.na(x$country)) cat("  Country:", x$country, "\n")
  if (!is.null(x$tolerance)) cat("  Tolerance:", x$tolerance, "\n")
  if (length(x$metadata) > 0) {
    cat("  Metadata:\n")
    for (key in names(x$metadata)) {
      cat("   ", key, ":",
          if (is.numeric(x$metadata[[key]])) {
            round(x$metadata[[key]], 4)
          } else {
            as.character(x$metadata[[key]])
          }, "\n")
    }
  }
  cat("  Variables:", paste(x$variables, collapse = ", "), "\n")
  invisible(x)
}

#' Type checking for constraint objects
#'
#' @param x Object to check
#' @return Logical indicating if x is a weighting_constraint
#' @export
is.weighting_constraint <- function(x) {
  inherits(x, "weighting_constraint")
}

#' Extract constraint information into a data frame
#'
#' @description
#' Centralizes all metadata extraction, eliminating need for regex parsing
#' throughout the codebase.
#'
#' @param constraints List of constraint objects
#' @return Data frame with all constraint information
#' @export
extract_constraint_info <- function(constraints) {
  if (length(constraints) == 0) {
    return(data.frame(
      formula = character(),
      type = character(),
      country = character(),
      description = character(),
      tolerance = numeric(),
      stringsAsFactors = FALSE
    ))
  }

  # Extract all standard fields
  df <- data.frame(
    formula = sapply(constraints, function(c) deparse(c$formula)),
    type = sapply(constraints, function(c) c$type),
    country = sapply(constraints, function(c) c$country %||% NA_character_),
    description = sapply(constraints, function(c) {
      c$metadata$description %||% paste(c$type, "constraint")
    }),
    tolerance = sapply(constraints, function(c) c$tolerance %||% NA_real_),
    stringsAsFactors = FALSE
  )

  # Extract any common metadata fields
  common_metadata <- c("doc_num", "category", "is_core", "demo_type")

  for (field in common_metadata) {
    values <- sapply(constraints, function(c) c$metadata[[field]] %||% NA)
    if (!all(is.na(values))) {
      df[[field]] <- values
    }
  }

  return(df)
}

# SECTION 2: TOLERANCE CONFIGURATION ----------------------------------------

#' Tolerance specification operator
#'
#' Infix operator for attaching tolerances to constraint formulas.
#' Enables concise specification: (constraint ~ target) %+-% tolerance
#'
#' @param x Formula object
#' @param tolerance Numeric tolerance (scalar or length-2 vector)
#' @return formula_with_tolerance object
#' @export
`%+-%` <- function(x, tolerance) {
  # Validate tolerance
  if (!is.numeric(tolerance)) {
    stop("Tolerance must be numeric")
  }
  if (length(tolerance) > 2) {
    stop("Tolerance must be a single value or vector of length 2")
  }

  # Add tolerance as an attribute to the formula
  attr(x, "tolerance") <- tolerance
  class(x) <- c("formula_with_tolerance", class(x))
  return(x)
}

#' Print method for formula_with_tolerance
#' @export
print.formula_with_tolerance <- function(x, ...) {
  tolerance <- attr(x, "tolerance")
  # Remove the special class temporarily for deparsing
  formula_only <- x
  class(formula_only) <- setdiff(class(formula_only), "formula_with_tolerance")

  if (length(tolerance) == 1) {
    cat(deparse(formula_only), "%+-%", tolerance, "\n")
  } else {
    cat(deparse(formula_only), "%+-% c(",
        paste(tolerance, collapse = ", "), ")\n")
  }
}

#' Convert formula_with_tolerance to character
#' @export
as.character.formula_with_tolerance <- function(x, ...) {
  tolerance <- attr(x, "tolerance")
  # Remove the special class temporarily for deparsing
  formula_only <- x
  class(formula_only) <- setdiff(class(formula_only), "formula_with_tolerance")

  if (length(tolerance) == 1) {
    paste(deparse(formula_only), "%+-%", tolerance)
  } else {
    paste(deparse(formula_only), "%+-% c(",
          paste(tolerance, collapse = ", "), ")")
  }
}


#' Create tolerance configuration
#'
#' Core function for specifying constraint tolerances in the optimization.
#' Supports symmetric/asymmetric tolerances and two modes: relative
#' (percentage of target) and percentage_point (fixed sample size).
#'
#' @param default Default symmetric tolerance
#' @param sum Tolerance for sum constraint
#' @param default_upper Upper bound for default tolerance
#' @param default_lower Lower bound for default tolerance
#' @param sum_upper Upper bound for sum constraint
#' @param sum_lower Lower bound for sum constraint
#' @param mode "relative" or "percentage_point"
#' @return tol_config object
#' @export
create_tol_config <- function(
    default      = 0.01,
    sum          = 0.001,
    default_upper= NULL,
    default_lower= NULL,
    sum_upper    = NULL,
    sum_lower    = NULL,
    mode = c("relative", "percentage_point")
) {
  mode <- match.arg(mode)

  list(
    default       = default,
    default_upper = default_upper %||% default,
    default_lower = default_lower %||% default,
    sum           = sum,
    sum_upper     = sum_upper %||% sum,
    sum_lower     = sum_lower %||% sum,
    mode          = mode
  )
}

# SECTION 3: BUILDING CONSTRAINTS  -----------------------------------------
#' Process helper functions in formula lists using generic dispatch
#'
#' Core dispatcher for weighting helper objects. Enables modular
#' constraint generation through helper functions that encapsulate
#' complex constraint logic.
#'
#' @param formulas List that may contain helper formula objects or regular formulas
#' @param data Data frame
#' @param config Optional configuration
#' @param stage_weights Weights from previous stage (if multi-stage)
#' @return List with 'constraints' (list of weighting_constraint objects) and 'data' (potentially modified)
#' @noRd
process_helper_formulas <- function(formulas, data, config = NULL, stage_weights = NULL) {
  all_constraints <- list()

  # Flatten the input while preserving helper objects
  flat_formulas <- list()

  flatten_formulas <- function(x) {
    if (inherits(x, "weighting_helper")) {
      # Keep helper objects as single items
      flat_formulas <<- append(flat_formulas, list(x))
    } else if (is.list(x) && !inherits(x, "formula")) {
      # Recursively flatten lists
      lapply(x, flatten_formulas)
    } else if (inherits(x, "formula") || inherits(x, "formula_with_tolerance")) {
      # Add formulas
      flat_formulas <<- append(flat_formulas, list(x))
    } else {
      # Unknown type - will error later with better message
      flat_formulas <<- append(flat_formulas, list(x))
    }
  }

  # Flatten the input
  flatten_formulas(formulas)

  for (f in flat_formulas) {
    if (inherits(f, "weighting_helper")) {
      helper_type <- class(f$params)[1]  # Get specific helper type from params
      message(sprintf("  Processing %s helper", helper_type))

      result <- f$processor(f$params, data, config, stage_weights)

      # Validate result structure
      if (!is.list(result) || !all(c("constraints", "data") %in% names(result))) {
        stop(sprintf("Helper processor for %s must return list with 'constraints' and 'data'",
                     helper_type))
      }

      # Check constraints are proper type
      if (!all(sapply(result$constraints, is.weighting_constraint))) {
        stop(sprintf("Helper %s must return weighting_constraint objects", helper_type))
      }

      # Update data if modified
      if (!identical(result$data, data)) {
        data <- result$data
        message("    Data modified by helper")
      }

      # Add constraints
      all_constraints <- c(all_constraints, result$constraints)
      message(sprintf("    Generated %d constraints", length(result$constraints)))

    } else if (inherits(f, "formula")) {
      all_constraints[[length(all_constraints) + 1]] <- make_constraint(
        formula = f,
        type = "direct",
        tolerance = attr(f, "tolerance"),  # Preserve any %+-% tolerance
        metadata = list(
          description = paste(deparse(f), collapse = " "),
          # Extract variable names for the constraint
          variables = all.vars(f)
        )
      )

    } else if (inherits(f, "formula_with_tolerance")) {
      all_constraints[[length(all_constraints) + 1]] <- make_constraint(
        formula = f$formula,
        type = "direct",
        tolerance = f$tolerance,
        metadata = list(
          description = deparse(f$formula),
          variables = all.vars(f$formula)
        )
      )

    } else {
      # Unknown type
      stop(sprintf("Unknown constraint type: %s. Expected weighting_helper, formula, or formula_with_tolerance",
                   paste(class(f), collapse = ", ")))
    }
  }

  list(
    constraints = all_constraints,
    data = data
  )
}

#' Parse weighting formula
#'
#' Core formula parser extracting LHS/RHS expressions, tolerance
#' specifications, and constraint metadata from formula objects.
#'
#' @param formula Formula object potentially with attributes
#' @param data Data frame for variable validation
#' @return List with parsed components
#' @noRd
parse_weighting_formula <- function(formula, data = NULL) {
  # Check if formula already has tolerance attribute (from %+-% operator)
  if (inherits(formula, "formula_with_tolerance")) {
    tolerance <- attr(formula, "tolerance")
    # Remove the special class
    class(formula) <- setdiff(class(formula), "formula_with_tolerance")
  } else {
    # Check for tolerance as a plain attribute (new approach)
    tolerance <- attr(formula, "tolerance")
  }

  # Handle formula objects
  if (inherits(formula, "formula")) {
    # Extract LHS and RHS from formula object
    if (length(formula) == 3) {
      # Two-sided formula
      lhs_expr <- formula[[2]]
      rhs_expr <- formula[[3]]
    } else {
      # One-sided formula - error
      stop("Formulas must be two-sided (LHS ~ RHS)")
    }

    # Resolve labels in expressions if data is provided
    if (!is.null(data)) {
      lhs_expr <- resolve_labels_in_expr(lhs_expr, data)
      rhs_expr <- resolve_labels_in_expr(rhs_expr, data)
    }

    # Return result for formula objects
    return(list(
      lhs_expr = lhs_expr,
      rhs_expr = rhs_expr,
      tolerance = tolerance,
      original_formula = formula,
      formula = formula,  # Keep both for compatibility
      constraint_name = attr(formula, "constraint_name")
    ))
  }

  # If we get here, it's string input - parse the ~ manually
  formula_str <- paste(as.character(formula), collapse = " ")

  # Check for %+-% in string if we don't already have tolerance
  if (is.null(tolerance) && grepl("%\\+-%", formula_str)) {
    # Parse from string (legacy support)
    parts <- strsplit(formula_str, "%\\+-%")[[1]]
    if (length(parts) != 2) {
      stop("Invalid formula syntax with %+-% operator")
    }
    main_part <- trimws(parts[1])
    tolerance_part <- trimws(parts[2])
    tolerance <- parse_tolerance_spec(tolerance_part)
  } else {
    main_part <- formula_str
  }

  # Find the tilde position
  tilde_pos <- find_delimiter_position(main_part, "~")

  if (length(tilde_pos) != 1) {
    stop("Formula must have exactly one ~ separating LHS and RHS")
  }

  lhs_str <- trimws(substr(main_part, 1, tilde_pos - 1))
  rhs_str <- trimws(substr(main_part, tilde_pos + 1, nchar(main_part)))

  # Parse as R expressions
  lhs_expr <- rlang::parse_expr(lhs_str)
  rhs_expr <- rlang::parse_expr(rhs_str)

  # Resolve labels in expressions if data is provided
  if (!is.null(data)) {
    lhs_expr <- resolve_labels_in_expr(lhs_expr, data)
    rhs_expr <- resolve_labels_in_expr(rhs_expr, data)
  }

  return(list(
    lhs_expr = lhs_expr,
    rhs_expr = rhs_expr,
    tolerance = tolerance,
    original_formula = formula,
    formula = formula,  # Keep both for compatibility
    constraint_name = NULL  # String input doesn't have constraint_name attribute
  ))
}

#' Find position of delimiter outside of parentheses/quotes
#' @noRd
find_delimiter_position <- function(str, delimiter) {
  chars <- strsplit(str, "")[[1]]
  positions <- integer()
  paren_depth <- 0
  in_quote <- FALSE
  quote_char <- NULL

  for (i in seq_along(chars)) {
    char <- chars[i]

    # Track quotes
    if (char %in% c("'", '"') && (i == 1 || chars[i-1] != "\\")) {
      if (!in_quote) {
        in_quote <- TRUE
        quote_char <- char
      } else if (char == quote_char) {
        in_quote <- FALSE
        quote_char <- NULL
      }
    }

    # Track parentheses
    if (!in_quote) {
      if (char == "(") paren_depth <- paren_depth + 1
      if (char == ")") paren_depth <- paren_depth - 1
    }

    # Check for delimiter
    if (!in_quote && paren_depth == 0 && char == delimiter) {
      positions <- c(positions, i)
    }
  }

  positions
}

#' Parse tolerance specification
#' @noRd
parse_tolerance_spec <- function(tol_str) {
  # Remove any remaining whitespace
  tol_str <- trimws(tol_str)

  # Check if it's a vector c(lower, upper)
  if (grepl("^c\\s*\\(", tol_str)) {
    # Parse as R expression
    tol_vals <- eval(rlang::parse_expr(tol_str))
    if (length(tol_vals) != 2) {
      stop("Asymmetric tolerance must have exactly 2 values: c(lower, upper)")
    }
    return(list(lower = tol_vals[1], upper = tol_vals[2], symmetric = FALSE))
  } else {
    # Single value - symmetric tolerance
    tol_val <- as.numeric(tol_str)
    if (is.na(tol_val)) {
      stop("Invalid tolerance value: ", tol_str)
    }
    return(list(lower = tol_val, upper = tol_val, symmetric = TRUE))
  }
}

#' Resolve labels in an expression recursively
#' @description Walks the expression tree and replaces label references with values
#' @noRd
resolve_labels_in_expr <- function(expr, data) {
  if (is.symbol(expr)) {
    return(expr)
  } else if (is.character(expr)) {
    return(expr)
  } else if (is.numeric(expr) || is.logical(expr)) {
    return(expr)
  } else if (is.call(expr)) {
    op <- as.character(expr[[1]])

    # Handle comparison operators
    if (op %in% c("==", "!=", "<", ">", "<=", ">=", "%in%")) {
      if (length(expr) >= 3) {
        lhs <- expr[[2]]
        rhs <- expr[[3]]

        # If LHS is a symbol (variable name)
        if (is.symbol(lhs)) {
          var_name <- as.character(lhs)

          if (var_name %in% names(data)) {
            label_map <- get_label_value_map(data[[var_name]])

            # Check if RHS is a character string that needs conversion
            if (!is.null(label_map) && is.character(rhs) && length(rhs) == 1) {

              if (rhs %in% names(label_map)) {

                # Replace with numeric value
                expr[[3]] <- as.numeric(label_map[rhs])
              }
            }
          }
        }
      }
    }

    # Recursively process all arguments
    if(is.call(expr) && length(expr) >1){
      for (i in 2:length(expr)) {
        expr[[i]] <- resolve_labels_in_expr(expr[[i]], data)
      }
    }
  }

  return(expr)
}

#' Expand auto-constraint syntax
#'
#' #' Core function for expanding compact constraint notation
#' (e.g., var ~ c(A=0.5, B=0.5)) into individual constraints.
#' Enables concise constraint specification.
#'
#' @param constr Constraint object to check for expansion
#' @param data Survey data
#' @return List of expanded constraint objects
#'
#' @noRd
expand_auto_constraint <- function(constr, data) {
  formula <- constr$formula

  # Check if RHS is a named vector (auto-expansion syntax)
  if (length(formula) == 3) {
    rhs <- formula[[3]]
    if (is.call(rhs) && as.character(rhs[[1]]) == "c") {
      # This is auto-expansion syntax
      var_name <- deparse(formula[[2]])

      # Evaluate RHS to get named vector
      targets <- eval(rhs)
      if (!is.numeric(targets) || is.null(names(targets))) {
        return(list(constr))  # Not auto-expansion, return as-is
      }

      # Expand into multiple constraints
      expanded <- list()
      for (level_name in names(targets)) {
        target_value <- targets[level_name]

        # Create new formula
        new_formula <- as.formula(sprintf("%s == '%s' ~ %f",
                                          var_name, level_name, target_value))

        # Create new constraint object preserving metadata
        expanded[[length(expanded) + 1]] <- make_constraint(
          formula = new_formula,
          type = constr$type,
          country = constr$country,
          metadata = c(
            constr$metadata,
            list(
              level = level_name,
              description = sprintf("%s = %s", var_name, level_name),
              expanded_from = deparse(formula)
            )
          ),
          tolerance = constr$tolerance
        )
      }

      return(expanded)
    }
  }

  # Not auto-expansion, return as-is
  return(list(constr))
}

#' Build a single constraint from a parsed formula
#' @param parsed_formula Output from parse_weighting_formula
#' @param data Data frame
#' @param stage_weights Optional weights from previous stage
#' @param config Optional weighting configuration object
#' @return List with rows, values, target, and name for the constraint
#' @noRd
build_constraint_from_parsed_formula <- function(parsed_formula, data,
                                                 stage_weights = NULL,
                                                 config = NULL) {
  n <- nrow(data)

  # Create evaluation environment with all data columns
  eval_env <- as.list(data)

  # Add useful functions to the environment
  eval_env$I <- function(x) as.numeric(x)  # Identity function for clarity

  # Add stage weights if provided
  if (!is.null(stage_weights)) {
    eval_env$`.weights` <- stage_weights
  }

  # Evaluate LHS expression - now can be any numeric array
  lhs_result <- tryCatch({
    rlang::eval_tidy(parsed_formula$lhs_expr, eval_env)
  }, error = function(e) {
    stop(sprintf("Error evaluating formula LHS '%s': %s",
                 deparse(parsed_formula$lhs_expr), e$message))
  })

  # Convert to numeric array
  if (is.logical(lhs_result)) {
    values_array <- as.numeric(lhs_result)
  } else if (is.numeric(lhs_result)) {
    values_array <- lhs_result
  } else {
    stop("LHS expression must evaluate to logical or numeric array")
  }

  # Ensure correct length
  if (length(values_array) != n) {
    stop(sprintf("LHS array length (%d) doesn't match data rows (%d)",
                 length(values_array), n))
  }

  # Handle NA values - treat as 0
  values_array[is.na(values_array)] <- 0

  # Find non-zero entries for sparse matrix
  non_zero_mask <- !is.na(values_array) & values_array != 0
  valid_rows <- which(non_zero_mask)
  values <- values_array[non_zero_mask]

  # Evaluate RHS for target
  rhs_result <- rlang::eval_tidy(parsed_formula$rhs_expr, eval_env)

  target <- as.numeric(rhs_result)

  # if lhs a mask, can infer targets (rhs) < 1 as proportions
  is_mask <- all(values_array %in% c(0, 1))
  if(is_mask && target < 1){
    target <- target * n
  }

  # Tidy formula
  lhs_str <- deparse(parsed_formula$lhs_expr, width.cutoff = 100)[1]
  rhs_str <- deparse(parsed_formula$rhs_expr, width.cutoff = 50)[1]

  # Remove excessive spaces and make it more compact
  lhs_str <- gsub("\\s+", " ", lhs_str)  # Normalize spaces
  lhs_str <- gsub("\\s*==\\s*", "==", lhs_str)  # Clean == operators
  lhs_str <- gsub("\\s*!=\\s*", "!=", lhs_str)  # Clean != operators
  lhs_str <- gsub("\\s*<=\\s*", "<=", lhs_str)  # Clean <= operators
  lhs_str <- gsub("\\s*>=\\s*", ">=", lhs_str)  # Clean >= operators
  lhs_str <- gsub("\\s*<\\s*", "<", lhs_str)    # Clean < operators
  lhs_str <- gsub("\\s*>\\s*", ">", lhs_str)    # Clean > operators
  lhs_str <- gsub("\"([^\"]+)\"", "\\1", lhs_str)

  # Create the full tidy formula
  tidy_formula <- paste0(lhs_str, " ~ ", rhs_str)

  # If it's still too long, truncate intelligently
  if (nchar(tidy_formula) > 80) {
    tidy_formula <- paste0(substr(tidy_formula, 1, 77), "...")
  }

  constraint_name <- parsed_formula$constraint_name
  if (is.null(constraint_name) || constraint_name == "") {
    constraint_name <- tidy_formula
  }

  # For expanded formulas, add the level name if available
  if (!is.null(attr(parsed_formula$formula, "level_name"))) {
    level_name <- attr(parsed_formula$formula, "level_name")
    # Extract the variable and format nicely
    if (grepl("==", constraint_name)) {
      var_part <- trimws(sub("\\s*==.*", "", constraint_name))
      constraint_name <- sprintf("%s==%s", var_part, level_name)
    }
  }

  # Extract strata from formula if present
  strata_col <- if (!is.null(config)) config$cols$strata else "Country"
  country <- extract_strata_from_formula(parsed_formula, data, valid_rows, strata_col)

  list(
    rows = valid_rows,
    values = values,
    target = target,
    formula = tidy_formula,
    name = constraint_name,
    tolerance = parsed_formula$tolerance,
    country = country
  )
}

#' Build constraints from a list of formulas
#'
#' Core pipeline for converting formulas and helpers into constraint
#' matrix format. Handles helper expansion, auto-constraint syntax,
#' and formula parsing.
#'
#' @param formulas List of formula objects and/or helpers
#' @param data Data frame
#' @param stage_weights Optional weights from previous stage
#' @param default_tolerance Default tolerance if not specified
#' @param config Optional configuration
#' @return List with 'constraints' (matrix format) and 'data' (potentially modified)
#' @noRd
build_constraints_from_formulas <- function(formulas, data, stage_weights = NULL,
                                            default_tolerance = 0.01, config = NULL) {

  result <- process_helper_formulas(formulas, data, config, stage_weights)
  all_constraints <- result$constraints
  data <- result$data  # Use potentially modified data

  expanded_constraints <- list()
  for (constr in all_constraints) {
    if (is.weighting_constraint(constr)) {
      # Check if this is an auto-expansion formula
      expanded <- expand_auto_constraint(constr, data)
      expanded_constraints <- c(expanded_constraints, expanded)
    } else {
      expanded_constraints[[length(expanded_constraints) + 1]] <- constr
    }
  }

  matrix_constraints <- list()

  for (i in seq_along(expanded_constraints)) {
    constr <- expanded_constraints[[i]]

    if (!is.weighting_constraint(constr)) {
      stop(sprintf("Expected weighting_constraint object at position %d, got %s",
                   i, class(constr)[1]))
    }

    # Parse formula
    parsed <- parse_weighting_formula(constr$formula, data)

    raw_tolerance <- constr$tolerance %||% parsed$tolerance %||% default_tolerance
    # Extract numeric tolerance value(s) based on what we received
    if (is.list(raw_tolerance) && !is.null(raw_tolerance$mode)) {
      # It's a tol_config object - extract based on constraint type
      constraint_type <- constr$type %||% "default"

      if (constraint_type == "demographic") {
        tol_lower <- raw_tolerance$demographic_lower %||% raw_tolerance$demographic %||% raw_tolerance$default_lower %||% raw_tolerance$default
        tol_upper <- raw_tolerance$demographic_upper %||% raw_tolerance$demographic %||% raw_tolerance$default_upper %||% raw_tolerance$default
      } else if (constraint_type == "occasion") {
        tol_lower <- raw_tolerance$occasion_lower %||% raw_tolerance$occasion %||% raw_tolerance$default_lower %||% raw_tolerance$default
        tol_upper <- raw_tolerance$occasion_upper %||% raw_tolerance$occasion %||% raw_tolerance$default_upper %||% raw_tolerance$default
      } else if (constraint_type == "sum") {
        tol_lower <- raw_tolerance$sum_lower %||% raw_tolerance$sum %||% raw_tolerance$default_lower %||% raw_tolerance$default
        tol_upper <- raw_tolerance$sum_upper %||% raw_tolerance$sum %||% raw_tolerance$default_upper %||% raw_tolerance$default
      } else {
        # Default/unknown type
        tol_lower <- raw_tolerance$default_lower %||% raw_tolerance$default
        tol_upper <- raw_tolerance$default_upper %||% raw_tolerance$default
      }

      # Store as a list with named elements for clarity
      tolerance <- list(lower = tol_lower, upper = tol_upper)

    } else if (is.numeric(raw_tolerance) && length(raw_tolerance) == 2) {
      # Vector of two values [lower, upper]
      tolerance <- list(lower = raw_tolerance[1], upper = raw_tolerance[2])

    } else if (is.numeric(raw_tolerance) && length(raw_tolerance) == 1) {
      # Single numeric value - use for both upper and lower
      tolerance <- raw_tolerance

    } else {
      # Unexpected format - use default
      warning(sprintf("Unexpected tolerance format for constraint %d, using default", i))
      tolerance <- default_tolerance
    }

    parsed$tolerance <- tolerance

    # Build matrix constraint from parsed formula
    matrix_constr <- build_constraint_from_parsed_formula(parsed, data, stage_weights, config)

    # Use constraint's explicit country if set (non-NULL and non-NA)
    if (!is.null(constr$country) && !is.na(constr$country)) {
      matrix_constr$country <- constr$country
    }

    matrix_constr$tolerance <- tolerance
    matrix_constr$type <- constr$type

    matrix_constr$metadata <- constr$metadata

    matrix_constr$name <- constr$metadata$description %||%
      sprintf("%s constraint %d", constr$type, i)

    if (length(matrix_constr$rows) == 0) {
      message(sprintf("  Warning: Constraint '%s' matches no respondents",
                      matrix_constr$name))
    }

    # DEBUG:
    if(is_debug()){
      message(sprintf("\nConstraint %d: %s", i, matrix_constr$formula))
      message(sprintf("  Type: %s", constr$type))
      message(sprintf("  Non-zero entries: %d", length(matrix_constr$rows)))
      if (!is.na(constr$country)) {
        message(sprintf("  Country: %s", constr$country))
      }
    }

    matrix_constraints[[i]] <- matrix_constr
  }

  # DEBUG:
  if(is_debug()){
    readline(prompt = "Built matrix constraints from constraint objects. Press [Enter] to continue...")
  }

  list(
    constraints = matrix_constraints,
    data = data  # Return potentially modified data
  )
}

# SECTION 4: CONSTRAINTS TO MATRIX -----------------------------------
#' Constraints to matrix
#'
#' Core function converting constraint objects into sparse matrix
#' representation for CVXR. Handles tolerance calculation and
#' constraint validation.
#'
#' @param constraints List of matrix constraints (from build_constraints_from_formulas)
#' @param n_resp Number of respondents
#' @param tol_config Tolerance configuration object
#' @param data Data frame (required for percentage_point mode)
#' @param config Optional weighting configuration object
#' @return List with sparse constraint matrix X, targets, tolerances, and labels
#' @noRd
constraints_to_matrix <- function(constraints, n_resp, tol_config = NULL, data = NULL, config = NULL) {

  get_default_tol <- function(tc) {
    if (is.null(tc)) return(0.01)
    tc$default %||% 0.01
  }

  n_by_country <- NULL
  if (!is.null(tol_config) && identical(tol_config$mode, "percentage_point")) {
    if (is.null(data)) {
      stop("data required for percentage_point tolerances")
    }

    strata_col <- if (!is.null(config)) config$cols$strata else "Country"
    if (!strata_col %in% names(data)) {
      stop(sprintf("Strata column '%s' not found in data. Required for percentage_point mode.", strata_col))
    }
    country_vec <- data[[strata_col]]
    if (is.null(country_vec)) {
      stop(sprintf("data$%s is required for percentage_point mode", strata_col))
    }
    n_by_country <- table(country_vec)
  }

  n_constraints <- length(constraints)

  # Build sparse matrix in triplet format
  i_indices <- c()
  j_indices <- c()
  x_values <- c()

  targets <- numeric(n_constraints)
  lower_slack <- numeric(n_constraints)
  upper_slack <- numeric(n_constraints)
  formulas <- character(n_constraints)
  labels <- character(n_constraints)

  # Track valid constraints
  valid_constraints <- logical(n_constraints)

  for (k in seq_along(constraints)) {
    constr <- constraints[[k]]

    labels[k] <- constr$name %||% paste0("Constraint_", k)

    formulas[k] <- if (!is.null(constr$formula)) {
      deparse(constr$formula)
    } else {
      NA_character_
    }

    # Validate constraint structure
    if (length(constr$rows) == 0) {
      warning(sprintf("Constraint '%s' matches no rows", labels[k]))
      targets[k] <- constr$target %||% 0
      lower_slack[k] <- get_default_tol(tol_config) * abs(targets[k])
      upper_slack[k] <- get_default_tol(tol_config) * abs(targets[k])
      valid_constraints[k] <- FALSE
      next
    }

    # Ensure values vector has same length as rows
    if (length(constr$values) != length(constr$rows)) {
      stop(sprintf("Constraint '%s': length of values (%d) doesn't match length of rows (%d)",
                   labels[k], length(constr$values), length(constr$rows)))
    }

    # Add to triplet format
    n_entries <- length(constr$rows)
    if (n_entries > 0) {
      i_indices <- c(i_indices, rep(k, n_entries))
      j_indices <- c(j_indices, constr$rows)
      x_values <- c(x_values, constr$values)
    }

    targets[k] <- constr$target
    valid_constraints[k] <- TRUE

    if (is.list(constr$tolerance) && !is.null(constr$tolerance$lower)) {
      # Asymmetric tolerance specified as list with lower/upper
      tol_lower <- constr$tolerance$lower
      tol_upper <- constr$tolerance$upper
    } else if (is.numeric(constr$tolerance)) {
      # Symmetric tolerance specified as single number
      tol_lower <- constr$tolerance
      tol_upper <- constr$tolerance
    } else {
      # No tolerance specified, use default
      tol_lower <- get_default_tol(tol_config)
      tol_upper <- get_default_tol(tol_config)
    }

    if (identical(tol_config$mode, "percentage_point")) {
      # Get country from constraint metadata
      country_k <- constr$country

      # Determine denominator based on country
      denom <- if (!is.na(country_k) && country_k %in% names(n_by_country)) {
        as.numeric(n_by_country[country_k])
      } else {
        stop("Can't find country denominator for percentage_point tolerances")
      }

      # Use constraint-specific tolerance if available
      lower_slack[k] <- tol_lower * denom
      upper_slack[k] <- tol_upper * denom

    } else {
      # Relative mode - standard percentage of target

      # Handle asymmetric tolerances if specified
      if (!is.null(tol_config)) {
        if (!is.null(constr$type) && constr$type == "sum") {
          # For sum constraints, check if tol_config has specific sum tolerances
          lower_slack[k] <- (tol_config$sum_lower %||% tol_lower) * abs(targets[k])
          upper_slack[k] <- (tol_config$sum_upper %||% tol_upper) * abs(targets[k])
        } else {
          # For other constraints, use default tolerances from config
          lower_slack[k] <- (tol_config$default_lower %||% tol_lower) * abs(targets[k])
          upper_slack[k] <- (tol_config$default_upper %||% tol_upper) * abs(targets[k])
        }
      } else {
        # No tol_config provided, use extracted tolerance values directly
        lower_slack[k] <- tol_lower * abs(targets[k])
        upper_slack[k] <- tol_upper * abs(targets[k])
      }
    }
  }

  # Create sparse matrix
  if (length(i_indices) > 0) {
    X <- Matrix::sparseMatrix(
      i = i_indices,
      j = j_indices,
      x = x_values,
      dims = c(n_constraints, n_resp)
    )
  } else {
    X <- Matrix::sparseMatrix(i = integer(0), j = integer(0), x = numeric(0),
                              dims = c(n_constraints, n_resp))
  }

  # Return list with all components
  list(
    X = X,
    targets = targets,
    lower_slack = lower_slack,
    upper_slack = upper_slack,
    formulas = formulas,
    labels = labels,
    valid_constraints = valid_constraints,
    constraint_metadata = lapply(constraints, function(c) {
      list(type = c$type, country = c$country, metadata = c$metadata)
    })
  )
}

# SECTION 5: BIAS CORRECTION -------------------------------------------------

#' Calculate target weights for bias-corrected optimization
#'
#' Implements the alpha-blended objective: w_target_i = alpha*(1/pi_i) + (1-alpha)*1
#'
#' @param n Sample size
#' @param selection_probs Vector of selection probabilities (pi_i)
#' @param alpha Bias correction emphasis (0 = no correction, 1 = full HT)
#' @param min_prob Minimum probability to prevent extreme weights
#' @param max_weight Cap on individual target weights
#' @param verbose Print diagnostic information
#' @return Vector of target weights
#' @export
calculate_target_weights <- function(
    n,
    selection_probs = NULL,
    alpha = 0.7,
    min_prob = 1e-6,
    max_weight = 100,
    verbose = FALSE) {

  # Default to uniform if no selection probabilities provided
  if (is.null(selection_probs)) {
    return(rep(1, n))
  }

  # Validate length
  if (length(selection_probs) != n) {
    stop(sprintf(
      "Selection probabilities length (%d) must match sample size (%d)",
      length(selection_probs), n
    ))
  }

  # Check for missing values
  if (any(is.na(selection_probs))) {
    n_missing <- sum(is.na(selection_probs))
    stop(sprintf(
      "Selection probabilities contain %d missing values. All respondents must have selection probabilities for bias correction.",
      n_missing
    ))
  }

  # Ensure selection probabilities are valid
  selection_probs <- pmax(selection_probs, min_prob)

  # Calculate Horvitz-Thompson weights
  ht_weights <- 1 / selection_probs

  # Cap extreme HT weights before blending
  ht_weights <- pmin(ht_weights, max_weight)

  # Apply alpha blending
  target_weights <- alpha * ht_weights + (1 - alpha) * 1

  # Normalize to sum to n (maintains interpretation)
  target_weights <- target_weights * n / sum(target_weights)

  # Store attributes for diagnostics
  attr(target_weights, "alpha") <- alpha
  attr(target_weights, "ht_weights") <- ht_weights
  attr(target_weights, "selection_probs") <- selection_probs

  if (verbose) {
    message(sprintf("  Calculating target weights with alpha = %.2f", alpha))
    message(sprintf("  - Selection prob range: [%.4f, %.4f]",
                    min(selection_probs), max(selection_probs)))
    message(sprintf("  - HT weights range: [%.2f, %.2f]",
                    min(ht_weights), max(ht_weights)))
    message(sprintf("  - Target weights range: [%.2f, %.2f]",
                    min(target_weights), max(target_weights)))
    message(sprintf("  - Weights > 2: %d (%.1f%%)",
                    sum(target_weights > 2),
                    mean(target_weights > 2) * 100))

    # Check bias correction effectiveness
    bias_check <- target_weights * selection_probs
    message(sprintf("  - Bias check (median w*pi): %.3f (should be ~ 1)",
                    median(bias_check)))
  }

  return(target_weights)
}

# SECTION 6: FEASIBILITY DIAGNOSTICS -----------------------------------------

#' Create detailed constraint diagnostics report
#' @param constraint_list List containing constraint matrix and metadata
#' @param cap Weight cap
#' @param data_size Number of respondents
#' @param config Optional weighting configuration object
#' @return Data frame with detailed constraint information
#' @noRd
create_constraint_diagnostics <- function(constraint_list, cap, data_size, config = NULL) {
  n_constraints <- nrow(constraint_list$X)

  # Initialize result vectors
  constraint_name <- constraint_list$labels
  target <- constraint_list$targets
  n_respondents <- numeric(n_constraints)
  sum_contributions <- numeric(n_constraints)
  avg_contribution <- numeric(n_constraints)
  max_possible <- numeric(n_constraints)
  min_avg_weight_needed <- numeric(n_constraints)
  feasibility_ratio <- numeric(n_constraints)

  # Analyze each constraint
  for (i in 1:n_constraints) {
    # Get row from sparse matrix
    row_data <- constraint_list$X[i, , drop = TRUE]

    # Find non-zero entries (respondents contributing to this constraint)
    nonzero_idx <- which(row_data != 0)
    n_resp <- length(nonzero_idx)

    n_respondents[i] <- n_resp

    if (n_resp > 0) {
      # Sum of all contributions (coefficients)
      sum_contributions[i] <- sum(abs(row_data[nonzero_idx]))

      # Average contribution per respondent
      avg_contribution[i] <- sum_contributions[i] / n_resp

      # Maximum possible value if all contributing respondents get cap weight
      max_possible[i] <- sum(abs(row_data[nonzero_idx])) * cap

      # Minimum average weight needed to achieve target
      if (sum_contributions[i] > 0) {
        min_avg_weight_needed[i] <- target[i] / sum_contributions[i]
      } else {
        min_avg_weight_needed[i] <- Inf
      }

      # Feasibility ratio: target / max_possible
      if (max_possible[i] > 0) {
        feasibility_ratio[i] <- target[i] / max_possible[i]
      } else {
        feasibility_ratio[i] <- Inf
      }
    } else {
      # No respondents for this constraint
      sum_contributions[i] <- 0
      avg_contribution[i] <- 0
      max_possible[i] <- 0
      min_avg_weight_needed[i] <- Inf
      feasibility_ratio[i] <- Inf
    }
  }

  # Extract constraint type and details from labels
  # Use patterns from config or defaults
  patterns <- if (!is.null(config)) {
    config$patterns
  } else {
    list(sum = "^Sum:|^Total:", occasion = "^Occ:", demo = "^Demo:")
  }
  
  constraint_type <- case_when(
    grepl(patterns$occasion, constraint_name) ~ "occasion",
    grepl(patterns$sum, constraint_name) ~ "sum",
    grepl(patterns$demo, constraint_name) ~ "demographic",
    TRUE ~ "other"
  )

  # Create comprehensive diagnostics data frame
  diagnostics <- data.frame(
    constraint = constraint_name,
    type = constraint_type,
    target = round(target, 2),
    n_respondents = n_respondents,
    pct_respondents = round(n_respondents / data_size * 100, 1),
    sum_contributions = round(sum_contributions, 2),
    avg_contribution = round(avg_contribution, 4),
    min_avg_weight_needed = round(min_avg_weight_needed, 2),
    max_possible_value = round(max_possible, 2),
    feasibility_ratio = round(feasibility_ratio, 3),
    upper_tolerance = round(constraint_list$upper_slack, 4),
    lower_tolerance = round(constraint_list$lower_slack, 4),
    is_empty = n_respondents == 0,
    is_infeasible = feasibility_ratio > 1,
    stringsAsFactors = FALSE
  )

  # Add warning flags
  diagnostics$warning <- case_when(
    diagnostics$is_empty ~ "NO_RESPONDENTS",
    diagnostics$is_infeasible ~ "MATHEMATICALLY_IMPOSSIBLE",
    diagnostics$feasibility_ratio > 0.95 ~ "VERY_CHALLENGING",
    diagnostics$feasibility_ratio > 0.90 ~ "CHALLENGING",
    TRUE ~ ""
  )

  return(diagnostics)
}

# SECTION 7: UNIFIED WEIGHTING WITH TOLERANCE ---------------------------------
#' Unified survey weighting with CVXR
#'
#' Core optimization engine implementing constrained quadratic programming
#' for survey calibration. Minimizes squared distance from target weights
#' subject to linear constraints with tolerances.
#'
#' @param dat_clean Cleaned survey data
#' @param constraint_list Output from constraints_to_matrix()
#' @param target_weights Target weight vector (default: uniform)
#' @param alpha Bias-variance trade-off parameter
#' @param cap Maximum weight bound
#' @param tol_config Tolerance configuration object
#' @param solver CVXR solver ("OSQP", "SCS", "ECOS")
#' @param verbose Print optimization progress
#' @return Data frame with weights and diagnostics
build_survey_weights <- function(dat_clean,
                                 constraint_list = NULL,
                                 target_weights = NULL,
                                 alpha = 0.0, # TO DO: change default to 1.0 but set to 0 if no target weights
                                 cap = 3.5,
                                 tol_config = NULL,
                                 solver = "OSQP",
                                 verbose = TRUE,
                                 config = NULL) {

  if (verbose) {
    message("Running unified constrained quadratic optimization with tolerance...")
  }

  N <- nrow(dat_clean)
  w <- CVXR::Variable(N)

  X <- NULL
  targets <- NULL
  lower_slack <- NULL
  upper_slack <- NULL
  constraint_labels <- NULL

  selection_probs <- NULL

  # Set w_target from target_weights parameter
  if (is.null(target_weights)) {
    w_target <- rep(1, N)  # Default to uniform weights
  } else {
    w_target <- target_weights
  }

  # Bias correction target weights
  if (verbose) {
    message("  Using alpha = ", alpha, " for bias-variance trade-off")
  }

  # Objective: minimize squared distance from target weights
  objective <- CVXR::Minimize(sum((w - w_target)^2))

  # Constraint assessment -----------------------------------------------------
  if (verbose) {
    message(sprintf("\nConstraint system: %d constraints × %d respondents",
                    nrow(constraint_list$X), ncol(constraint_list$X)))

    # Check for problems
    row_sums <- Matrix::rowSums(constraint_list$X)
    empty_constraints <- which(row_sums == 0)
    bad_targets <- which(constraint_list$targets <= 0)

    # Report issues concisely
    if (length(empty_constraints) > 0) {
      message(sprintf("  WARNING: %d empty constraints (no matching respondents)",
                      length(empty_constraints)))
      # Show first few
      for (i in head(empty_constraints, 3)) {
        message(sprintf("     - %s", constraint_list$labels[i]))
      }
    }

    if (length(bad_targets) > 0) {
      message(sprintf("  WARNING: %d constraints with non-positive targets",
                      length(bad_targets)))
    }

    # Feasibility quick check
    max_possible <- Matrix::rowSums(abs(constraint_list$X)) * cap
    tight_constraints <- sum(constraint_list$targets > max_possible * 0.95)
    if (tight_constraints > 0) {
      message(sprintf("  WARNING: %d constraints require >95%% of max possible contribution",
                      tight_constraints))
    }

    # Debug mode only
    if (is_debug()) {
      assign("debug_constraint_list", constraint_list, envir = .GlobalEnv)
      assign("debug_data", dat_clean, envir = .GlobalEnv)
      message("  DEBUG: Saved to global env: debug_constraint_list, debug_data")
    }
  }

  X <- constraint_list$X
  targets <- constraint_list$targets
  lower_slack <- constraint_list$lower_slack
  upper_slack <- constraint_list$upper_slack
  constraint_labels <- constraint_list$labels

  n_constraints <- nrow(X)
  s_lower <- CVXR::Variable(n_constraints, nonneg = TRUE)
  s_upper <- CVXR::Variable(n_constraints, nonneg = TRUE)

  constraints <- list(
    X %*% w == targets + s_upper - s_lower,
    s_lower <= lower_slack,
    s_upper <= upper_slack,
    w >= 0,
    w <= cap
  )

  # Solve
  problem <- CVXR::Problem(objective, constraints)

  if(verbose) {
    message("  Solving optimization problem...")
    message("    Solver: ", solver)
  }

  result <- tryCatch({
    if (solver == "OSQP") {
      CVXR::solve(problem, solver = "OSQP",
                  verbose = FALSE,
                  eps_abs = 1e-6,
                  eps_rel = 1e-6,
                  max_iter = 10000)
    } else if (solver == "ECOS") {
      CVXR::solve(problem, solver = "ECOS",
                  verbose = FALSE,
                  abstol = 1e-7,
                  reltol = 1e-6)
    } else if (solver == "SCS") {
      CVXR::solve(problem, solver = "SCS",
                  verbose = FALSE,
                  eps = 1e-6,
                  max_iters = 5000)
    } else {
      CVXR::solve(problem, verbose = FALSE)
    }
  }, error = function(e) {
    if (solver != "SCS"){
      message("Primary solver failed. Trying SCS...")
      CVXR::solve(problem, solver = "SCS",
                  verbose = verbose,
                  eps = 1e-6,
                  max_iters = 5000)
    } else {
      stop(e)
    }
  })

  # Consolidated status checks and error handling ------------------------------
  if (result$status %in% c("infeasible", "infeasible_inaccurate", "solver_error")){
    constraint_diagnostics <- create_constraint_diagnostics(
      constraint_list,
      cap,
      N,
      config
    )

    # Create result object with infeasibility information
    if(result$status %in% c("infeasible", "infeasible_inaccurate")){
      error_message <- paste0(
        "\n=== OPTIMIZATION FAILED: INFEASIBLE CONSTRAINTS ===\n",
        "The weighting problem cannot be solved because some constraints are\n",
        "mathematically impossible to satisfy given the data and weight cap.\n\n",
        sprintf("Summary:\n"),
        sprintf("  - Total constraints: %d\n", nrow(constraint_diagnostics)),
        sprintf("  - Empty constraints (no respondents): %d\n", sum(constraint_diagnostics$is_empty)),
        sprintf("  - Mathematically impossible: %d\n", sum(constraint_diagnostics$is_infeasible)),
        sprintf("  - Very challenging (>95%% of max): %d\n", sum(constraint_diagnostics$feasibility_ratio > 0.95 & constraint_diagnostics$feasibility_ratio <= 1)),
        sprintf("  - Weight cap: %.1f\n", cap),
        sprintf("  - Sample size: %d\n\n", N),
        "Access detailed diagnostics via $constraint_diagnostics in the returned object."
      )
    } else if (result$status == "solver_error"){
      error_message <- paste0(
        "\n=== OPTIMIZATION FAILED: SOLVER ERROR ===\n",
        "The solver could not find a solution within computational limits.\n\n",
        "This often indicates:\n",
        "- The problem is very close to infeasible\n",
        "- Too many constraints with very few respondents\n",
        "- Numerical conditioning issues\n\n",sprintf("Summary:\n"),
        sprintf("  - Total constraints: %d\n", nrow(constraint_diagnostics)),
        sprintf("  - Empty constraints (no respondents): %d\n", sum(constraint_diagnostics$is_empty)),
        sprintf("  - Mathematically impossible: %d\n", sum(constraint_diagnostics$is_infeasible)),
        sprintf("  - Very challenging (>95%% of max): %d\n", sum(constraint_diagnostics$feasibility_ratio > 0.95 & constraint_diagnostics$feasibility_ratio <= 1)),
        sprintf("  - Weight cap: %.1f\n", cap),
        sprintf("  - Sample size: %d\n\n", N),
        "Access detailed diagnostics via $constraint_diagnostics in the returned object."
      )
    }

    infeasible_result <- list(
      status = "infeasible",
      solver_status = result$status,
      constraint_diagnostics = constraint_diagnostics,
      summary = list(
        n_constraints = nrow(constraint_diagnostics),
        n_empty_constraints = sum(constraint_diagnostics$is_empty),
        n_infeasible_constraints = sum(constraint_diagnostics$is_infeasible),
        n_challenging_constraints = sum(constraint_diagnostics$warning %in% c("VERY_CHALLENGING", "CHALLENGING")),
        cap = cap,
        sample_size = N
      ),
      error_message = error_message
    )

    # Add most problematic constraints to summary
    problem_constraints <- constraint_diagnostics[
      constraint_diagnostics$warning != "" & constraint_diagnostics$warning != "CHALLENGING",
    ]

    if (nrow(problem_constraints) > 0) {
      problem_constraints <- problem_constraints[order(problem_constraints$feasibility_ratio, decreasing = TRUE), ]
      infeasible_result$top_problems <- head(problem_constraints, 10)
    }

    class(infeasible_result) <- c("weights_infeasible", "list")

    # Print error message but return the object instead of stopping
    message(infeasible_result$error_message)

    return(infeasible_result)

  } else if (result$status %in% c("unbounded", "unbounded_inaccurate")) {
    # This shouldn't happen with a weight cap, but just in case
    stop(paste0(
      "\n=== OPTIMIZATION FAILED: UNBOUNDED PROBLEM ===\n",
      "The optimization problem is unbounded.\n",
      "This is unexpected and may indicate a bug in the constraint setup."
    ))
  } else if (result$status != "optimal" && result$status != "optimal_inaccurate") {
    # Any other non-optimal status
    warning(sprintf(
      "Optimization completed with status: %s\n",
      "Results may be unreliable.",
      result$status
    ))
  }

  # Only extract weights if we have an optimal solution -----------------------
  if (result$status %in% c("optimal", "optimal_inaccurate")) {
    weights_raw <- tryCatch({
      as.numeric(result$getValue(w))
    }, error = function(e) {
      stop(sprintf(
        "Failed to extract weights despite optimal status.\n",
        "Solver status: %s\n",
        "Error: %s\n",
        "This is unexpected - please report this issue.",
        result$status,
        e$message
      ))
    })
  } else {
    # For any failure status, we stop here
    stop("Optimization failed - cannot proceed without valid weights.")
  }

  # Create weights data frame
  weights_df <- data.frame(
    respondent_id = seq_len(N),
    weight = weights_raw,
    stringsAsFactors = FALSE
  )

  # Additional reporting to return with the weights ---------------------------
  # Extract slack values for reporting
  slack_lower_vals <- as.numeric(result$getValue(s_lower))
  slack_upper_vals <- as.numeric(result$getValue(s_upper))

  achieved <- as.numeric(X %*% weights_raw)

  # Create detailed constraint report -----------------------------------------

  # Create detailed constraint report -----------------------------------------
  # Create detailed constraint report -----------------------------------------
  constraint_metadata <- constraint_list$constraint_metadata %||% list()

  constraint_report <- data.frame(
    constraint = constraint_labels,
    target = targets,
    achieved = achieved,
    deviation = achieved - targets,
    deviation_pct = ifelse(targets != 0,
                           (achieved - targets) / targets * 100,
                           0),
    lower_slack_used = slack_lower_vals,
    upper_slack_used = slack_upper_vals,
    lower_slack_limit = lower_slack,
    upper_slack_limit = upper_slack,
    binding_lower = slack_lower_vals > (lower_slack * 0.99),
    binding_upper = slack_upper_vals > (upper_slack * 0.99),
    stringsAsFactors = FALSE
  )

  # Add constraint type from metadata
  constraint_report$constraint_type <- sapply(seq_along(constraint_metadata), function(i) {
    constraint_metadata[[i]]$type %||% "unknown"
  })

  # Add country if it's a meaningful field for the analysis
  # (only include if at least some constraints have country info)
  country_vals <- sapply(seq_along(constraint_metadata), function(i) {
    constraint_metadata[[i]]$country %||% NA_character_
  })
  if (!all(is.na(country_vals))) {
    constraint_report$country <- country_vals
  }

  # Add percentage point reporting for better alignment with percentage_point tolerance mode
  # First, always show percentage of total sample
  constraint_report$pct_of_total <- (achieved / N) * 100

  # For percentage calculations relative to the relevant population (country or total)
  # we need to determine the denominator for each constraint
  # This aligns with how percentage_point tolerances work:
  # - Country-specific constraints: percentages are relative to country subsample
  # - Global constraints: percentages are relative to total sample
  if ("country" %in% names(constraint_report) && "Country" %in% names(dat_clean)) {
    # Calculate country sample sizes
    country_sizes <- table(dat_clean$Country)

    # Determine denominator for each constraint
    denominators <- sapply(seq_along(constraint_report$country), function(i) {
      country <- constraint_report$country[i]
      if (!is.na(country) && country %in% names(country_sizes)) {
        as.numeric(country_sizes[country])
      } else {
        N  # Use total sample for non-country-specific constraints
      }
    })
  } else {
    # No country information available, use total N for all
    denominators <- rep(N, nrow(constraint_report))
  }

  # Calculate percentages relative to the relevant population
  constraint_report$achieved_pct <- (achieved / denominators) * 100
  constraint_report$target_pct <- (targets / denominators) * 100
  constraint_report$deviation_pp <- constraint_report$achieved_pct - constraint_report$target_pct

  # Calculate percentage of tolerance used
  constraint_report$tolerance_used_pct <- with(constraint_report, {
    case_when(
      deviation > 0 ~ pmax(0, pmin(100, upper_slack_used / upper_slack_limit * 100)),
      deviation < 0 ~ pmax(0, pmin(100, lower_slack_used / lower_slack_limit * 100)),
      TRUE ~ 0
    )
  })

  # Reorder columns for clarity
  col_order <- c("constraint", "constraint_type",
                 if ("country" %in% names(constraint_report)) "country" else NULL,
                 "target", "achieved", "deviation", "deviation_pct",
                 "pct_of_total", "target_pct", "achieved_pct", "deviation_pp",
                 "tolerance_used_pct",
                 "lower_slack_used", "upper_slack_used",
                 "lower_slack_limit", "upper_slack_limit",
                 "binding_lower", "binding_upper")

  constraint_report <- constraint_report[, col_order]

  # Final return values -------------------------------------------------------
  # Calculate max deviation (as a proportion of tolerance)
  max_deviation <- max(abs(constraint_report$tolerance_used_pct)) / 100

  # Calculate effective sample size and other diagnostics
  ess <- sum(weights_raw)^2 / sum(weights_raw^2)
  efficiency <- ess / N
  cv <- sd(weights_raw) / mean(weights_raw)

  # Add attributes
  attr(weights_df, "ess") <- ess
  attr(weights_df, "efficiency") <- efficiency
  attr(weights_df, "cv") <- cv
  attr(weights_df, "solver_status") <- result$status
  attr(weights_df, "max_deviation") <- max_deviation
  attr(weights_df, "constraint_report") <- constraint_report
  attr(weights_df, "weight_stats") <- summary(weights_raw)
  attr(weights_df, "selection_probs") <- selection_probs

  return(weights_df)
}

#' Identify and adjust weights for fully unconstrained respondents using constraint matrix
#'
#' @description
#' For formula-based weighting, identifies respondents who don't appear in any
#' demographic/occasion constraints by examining the constraint matrix.
#'
#' @param data Data frame with respondent information
#' @param weights Current weight vector
#' @param constraint_list List containing the constraint matrix and labels
#' @param verbose Logical for detailed output
#' @param config Optional weighting configuration object
#' @return Updated weight vector with attributes
adjust_fully_unconstrained <- function(data, weights, constraint_list, verbose = FALSE, config = NULL) {

  if (verbose) message("\n  Checking for fully unconstrained respondents...")

  n <- nrow(data)
  X <- constraint_list$X
  labels <- constraint_list$labels

  # Identify which constraints are demographic/occasion (not sum constraint)
  # Use patterns from config
  patterns <- if (!is.null(config)) {
    config$patterns
  } else {
    list(sum = "^Sum:|^Total:")
  }
  
  # Find sum constraint using dynamic pattern
  sum_constraint_idx <- which(grepl(patterns$sum, labels))

  if (length(sum_constraint_idx) == 0) {
    # Try alternative patterns
    sum_constraint_idx <- which(labels == "Total" | labels == "Sum")
  }

  # Get indices of non-sum constraints
  constraint_indices <- setdiff(1:nrow(X), sum_constraint_idx)

  if (length(constraint_indices) == 0) {
    if (verbose) message("  No demographic/occasion constraints found")
    return(weights)
  }

  # Check which respondents appear in at least one constraint
  constrained <- logical(n)

  # Extract the sub-matrix for non-sum constraints
  X_constraints <- X[constraint_indices, , drop = FALSE]

  # For each respondent, check if they have any non-zero entry
  # Use column sums for efficiency instead of looping
  col_sums <- Matrix::colSums(abs(X_constraints))
  constrained <- col_sums > 0

  # Identify fully unconstrained respondents
  fully_unconstrained <- which(!constrained)

  if (length(fully_unconstrained) == 0) {
    if (verbose) message("  No fully unconstrained respondents found")
    return(weights)
  }

  # Warn user
  warning(sprintf(
    "Found %d fully unconstrained respondents (%.1f%% of sample).\n",
    length(fully_unconstrained),
    length(fully_unconstrained) / n * 100
  ))

  if (verbose) {
    message(sprintf("  Found %d fully unconstrained respondents",
                    length(fully_unconstrained)))

    # Show breakdown by country if available
    if ("Country" %in% names(data)) {
      unconstrained_by_country <- table(data$Country[fully_unconstrained])
      if (length(unconstrained_by_country) > 0) {
        message("  Breakdown by country:")
        for (i in seq_along(unconstrained_by_country)) {
          message(sprintf("    %s: %d respondents",
                          names(unconstrained_by_country)[i],
                          unconstrained_by_country[i]))
        }
      }
    }

    # Show which types of constraints are present
    constraint_types <- unique(gsub(":.*", "", labels[constraint_indices]))
    message(sprintf("  Active constraint types: %s",
                    paste(constraint_types, collapse = ", ")))
  }

  # Apply adjustment method
  if (length(fully_unconstrained) < n) {
    # Use median weight of constrained respondents
    constrained_weights <- weights[-fully_unconstrained]
    median_weight <- median(constrained_weights)

    weights[fully_unconstrained] <- median_weight

    if (verbose) {
      message(sprintf("  Set %d fully unconstrained respondents to median weight = %.3f",
                      length(fully_unconstrained), median_weight))
      message(sprintf("  (median of %d constrained respondents)",
                      length(constrained_weights)))
    }
  } else {
    # All respondents are unconstrained (shouldn't happen with sum constraint)
    weights[fully_unconstrained] <- 1.0

    if (verbose) {
      message("  All respondents are unconstrained - setting weights to 1.0")
    }
  }

  # Add attributes for diagnostics
  attr(weights, "n_fully_unconstrained") <- length(fully_unconstrained)
  attr(weights, "fully_unconstrained_indices") <- fully_unconstrained

  return(weights)
}


# SECTION 8: ORCHESTRATION FUNCTION -------------------------------------------

#' Run unified multi-stage weighting
#'
#' High-level orchestration function for multi-stage survey weighting.
#' Manages stage sequencing, weight propagation, and diagnostic
#' collection across optimization stages.
#'
#' @param raw_data Raw survey data
#' @param stages List of stage specifications
#' @param config Configuration object
#' @param alpha Default bias correction parameter
#' @param tol_config Default tolerance configuration
#' @param tolerance Default tolerance value
#' @param cap Default weight cap
#' @param verbose Progress reporting
#' @return Weighting results with diagnostics
#' @export
run_unified_weighting <- function(raw_data,
                                  stages,
                                  config = NULL,
                                  alpha = 0.7,
                                  tol_config = NULL,
                                  tolerance = 0.03,
                                  cap = 3.5,
                                  verbose = TRUE) {

  message("=== UNIFIED SURVEY WEIGHTING ===\n")

  if(is.null(stages)){
    stop("Must provide weighting stages (even if just one stage, still use stages syntax")
  }

  # Infers a single stage
  if (is.list(stages) && "constraints" %in% names(stages)) {
    stages <- list(stages)
  }

  if (!is.null(stages)) {

    # Initialize variables for tracking stages
    current_weights <- NULL
    all_results <- list()

    # Process stages sequentially
    for (stage_num in seq_along(stages)) {
      stage <- stages[[stage_num]]
      message(sprintf("\n=== STAGE %d ===", stage_num))

      # Extract stage components
      if (!"constraints" %in% names(stage))
        stop(sprintf("Stage %d must have 'constraints'", stage_num))

      stage_constraints <- stage$constraints # formulas
      stage_weighting_target <- stage$weighting_target
      stage_alpha <- stage$alpha %||% alpha

      ## Setup tolerances
      # Start from the pipeline-wide defaults (may be NULL)
      stage_tol_config <- tol_config %||% create_tol_config()

      # (1) If this stage brings its own tol_config, merge it in
      if (!is.null(stage$tol_config)) {
        # Ensure the stage config has all required fields
        stage_config_complete <- create_tol_config()
        stage_config_complete <- modifyList(stage_config_complete, stage$tol_config)
        stage_tol_config <- modifyList(stage_tol_config, stage_config_complete)
      }

      # (2) If the stage supplies a scalar tolerance it should overwrite defaults
      if (!is.null(stage$tolerance)) {
        if (length(stage$tolerance) == 1) {
          # Single value - symmetric tolerance
          stage_tol_config$default       <- stage$tolerance
          stage_tol_config$default_upper <- stage$tolerance
          stage_tol_config$default_lower <- stage$tolerance
        } else if (length(stage$tolerance) == 2) {
          # Vector [lower, upper] - asymmetric tolerance
          stage_tol_config$default_lower <- stage$tolerance[1]
          stage_tol_config$default_upper <- stage$tolerance[2]
          stage_tol_config$default       <- mean(stage$tolerance)
        } else {
          stop("stage$tolerance has unexpected length")
        }
      }

      target_weights <- NULL # will be target weights after any alpha blending
      selection_probs <- NULL # for a weighting target function (if given)

      # If stage 1, first prepare data ----------------------------------------
      if (stage_num == 1) {
        # First stage - prepare data
        message("\nPreparing data...")
        if (inherits(raw_data, "survey_data")) {
          dat_clean <- raw_data$dat
        } else {
          dat_clean <- raw_data
        }
        
        # Use config or create default
        if (is.null(config)) {
          config <- default_weighting_config()
          if (verbose) {
            message("  Using default configuration")
          }
        }
        
        # Apply legacy mappings for backward compatibility
        if (!is.null(config$legacy_mappings)) {
          for (old_name in names(config$legacy_mappings)) {
            new_name <- config$legacy_mappings[[old_name]]
            if (old_name %in% names(dat_clean) && !new_name %in% names(dat_clean)) {
              dat_clean[[new_name]] <- sjlabelled::as_label(dat_clean[[old_name]])
              if (verbose) {
                message(sprintf("  Legacy mapping applied: %s -> %s", old_name, new_name))
              }
            }
          }
        }
        
        # Ensure required columns exist
        id_col <- config$cols$id
        if (!id_col %in% names(dat_clean)) {
          dat_clean[[id_col]] <- seq_len(nrow(dat_clean))
          if (verbose) {
            message(sprintf("  Generated %s column", id_col))
          }
        }
        
        # Verify strata column exists (if needed for constraints)
        strata_col <- config$cols$strata
        if (!strata_col %in% names(dat_clean)) {
          if (verbose) {
            message(sprintf("  Note: Strata column '%s' not found in data. This may cause issues if you have strata-specific constraints.", strata_col))
          }
        }
        
        # Apply demo_var_mapping (alias approach)
        if (!is.null(config$demo_var_mapping)) {
          for (nice_name in names(config$demo_var_mapping)) {
            actual_col <- config$demo_var_mapping[[nice_name]]
            if (actual_col %in% names(dat_clean) && !nice_name %in% names(dat_clean)) {
              dat_clean[[nice_name]] <- dat_clean[[actual_col]]
              if (verbose) {
                message(sprintf("  Created alias: %s -> %s", nice_name, actual_col))
              }
            }
          }
        }
      } # END OF DATA PREP

      # Build constraint matrix from formulas, passing stage weights -----------
      message("\nBuilding constraints from formulas...")
      constraints_result <- build_constraints_from_formulas(
        formulas = stage_constraints,
        data = dat_clean,
        stage_weights = current_weights,
        default_tolerance = stage_tol_config$default,
        config = config
      )

      # Extract the actual constraints and updated data if modified by helpers
      formula_constraints <- constraints_result$constraints
      dat_clean <- constraints_result$data

      # Summarise constraints built from formulas
      if(verbose){
        message(sprintf("\nBuilt %d constraints from formulas", length(formula_constraints)))

        # Show constraint summary
        constraint_summary <- data.frame(
          index = seq_along(formula_constraints),
          name = sapply(formula_constraints, function(x) {
            if (!is.null(x$name)) return(x$name)
            if (!is.null(x$metadata$description)) return(x$metadata$description)
            return("unnamed")
          }),
          n_rows = sapply(formula_constraints, function(x) {
            if (!is.null(x$rows)) return(length(x$rows))
            return(0)
          }),
          target = sapply(formula_constraints, function(x) {
            if (!is.null(x$target)) return(x$target)
            return(NA_real_)
          }),
          stringsAsFactors = FALSE
        )

        # Show first few and any empty ones
        message("\nConstraint summary (first 5):")
        print(head(constraint_summary, 5))

        message("\nConstraint summary (last 5):")
        print(tail(constraint_summary, 5))

        empty_constraints <- constraint_summary[constraint_summary$n_rows == 0, ]
        if (nrow(empty_constraints) > 0) {
          message(sprintf("\nWARNING: %d empty constraints found:", nrow(empty_constraints)))
          print(empty_constraints)
        }
      } # End verbose summary of constraints

      # Convert to matrix format
      constraint_list <- constraints_to_matrix(
        formula_constraints,
        nrow(dat_clean),
        stage_tol_config,
        dat_clean,
        config
      )

      # Add sum constraint
      sum_row <- Matrix::sparseMatrix(
        i = rep(1, nrow(dat_clean)),
        j = 1:nrow(dat_clean),
        x = rep(1, nrow(dat_clean)),
        dims = c(1, nrow(dat_clean))
      )
      constraint_list$X <- rbind(constraint_list$X, sum_row)
      constraint_list$targets <- c(constraint_list$targets, nrow(dat_clean))
      constraint_list$lower_slack <- c(constraint_list$lower_slack,
                                       stage_tol_config$sum * nrow(dat_clean))
      constraint_list$upper_slack <- c(constraint_list$upper_slack,
                                       stage_tol_config$sum * nrow(dat_clean))
      constraint_list$labels <- c(constraint_list$labels, "Sum:Total")
      constraint_list$formulas <- c(constraint_list$formulas, "[Sum constraint: all weights sum to N]")
      if (!is.null(constraint_list$constraint_metadata)) {
        constraint_list$constraint_metadata <- c(
          constraint_list$constraint_metadata,
          list(list(
            type = "sum",
            country = NA_character_,
            metadata = list(description = "Total sample size constraint")
          ))
        )
      }

      if(is_debug()){
        message(sprintf("\nConstraint matrix: %d constraints x %d respondents",
                        nrow(constraint_list$X), ncol(constraint_list$X)))
        message(sprintf("  Sparsity: %.1f%% zeros",
                        sum(constraint_list$X == 0) / length(constraint_list$X) * 100))
        message("  Target range: [%.1f, %.1f]",
                min(constraint_list$targets), max(constraint_list$targets))

        # Show small example of structure
        message("\nExample - first 3 constraints x first 5 respondents:")
        n_show <- min(3, nrow(constraint_list$X))
        example_matrix <- as.matrix(constraint_list$X[1:n_show, 1:5, drop = FALSE])
        rownames(example_matrix) <- constraint_list$labels[1:n_show]
        print(example_matrix)

        message("\nTargets:")
        print(data.frame(
          constraint = constraint_list$labels[1:n_show],
          target = constraint_list$targets[1:n_show]
        ))
        print(data.frame(
          constraint = constraint_list$labels[(nrow(constraint_list$X)-3):nrow(constraint_list$X)],
          target = constraint_list$targets[(nrow(constraint_list$X)-3):nrow(constraint_list$X)]
        ))

        readline("Built constraint matrix and added sum constraint. Press Enter to continue...")
      } # End constraint matrix debug

      # Handle weighting target (bias correction) -----------------------------
      if (!is.null(stage_weighting_target)) {
        message("Processing weighting target for bias correction...")

        # Weighting target can be set either with a weighting_target_function
        # which provides selection probabilities (with stage_weighting_target()),
        # from which we then calculate_target_weights() (including alpha blending)...
        # Or, a full weighting target vector can be provided directly...
        # Or, no weighting target. In which case uniform weights are used.
        if (inherits(stage_weighting_target, "weighting_target_function")) {
          selection_probs <- tryCatch({
            stage_weighting_target(
              data = dat_clean,
              n_mc_iter = 50000,
              verbose = verbose
            )
          }, error = function(e) {
            stop(sprintf("Error in weighting target evaluation: %s", e$message))
          })

          if(is_debug() && !is.null(selection_probs)){
            message(sprintf("\nSelection probabilities:"))
            message(sprintf("  Range: [%.6f, %.6f]",
                            min(selection_probs), max(selection_probs)))
            message(sprintf("  Mean: %.6f, SD: %.6f",
                            mean(selection_probs), sd(selection_probs)))
            message(sprintf("  Unique values: %d", length(unique(selection_probs))))
            # Show distribution
            quants <- quantile(selection_probs, c(0.01, 0.25, 0.5, 0.75, 0.99))
            message(sprintf("  Distribution: 1%%=%.4f, 25%%=%.4f, 50%%=%.4f, 75%%=%.4f, 99%%=%.4f",
                            quants[1], quants[2], quants[3], quants[4], quants[5]))
          }

          # Calculate target weights using alpha blending
          target_weights <- calculate_target_weights(
            n = nrow(dat_clean),
            selection_probs = selection_probs,
            alpha = stage_alpha,
            verbose = verbose
          )

          if (verbose) {
            message(sprintf("  - Alpha = %.2f", stage_alpha))
            message(sprintf("  - Selection prob range: [%.4f, %.4f]",
                            min(selection_probs), max(selection_probs)))
            message(sprintf("  - Target weight range: [%.2f, %.2f]",
                            min(target_weights), max(target_weights)))
          }
        } else if (is.numeric(stage_weighting_target)) {

          # Direct numeric vector provided
          if (length(stage_weighting_target) != nrow(dat_clean)) {
            stop("Weighting target length must match number of respondents")
          }
          selection_probs <- stage_weighting_target  # Keep for diagnostics
          target_weights <- stage_weighting_target
        }
      } else {
        # No weighting target - use uniform weights
        target_weights <- rep(1, nrow(dat_clean))
        selection_probs <- NULL  # No bias correction
      }

      ## Build weights for this stage -----------------------------------------
      message(sprintf("\nBuilding optimized weights..."))
      message(sprintf("  Alpha (bias correction): %.2f", stage_alpha))
      message(sprintf("  Weight cap: %.1f", cap))

      # if successful, build_survey_weights returns a tibble with columns: respondent_id, weight
      stage_weights_result <- build_survey_weights(
        dat_clean = dat_clean,
        constraint_list = constraint_list,
        target_weights = target_weights,
        alpha = stage_alpha,
        cap = cap,
        tol_config = stage_tol_config,
        verbose = verbose,
        config = config
      )

      # Check if optimization failed ------------------------------------------
      # TO DO: record any other unsuccessful statuses here also
      if (inherits(stage_weights_result, "weights_infeasible")) {
        # Add stage information to the infeasible result
        stage_weights_result$stage_failed = stage_num
        stage_weights_result$total_stages = length(stages)
        stage_weights_result$stage_info = sprintf("Failed at stage %d of %d",
                                                  stage_num, length(stages))

        # Add any completed stages to the result
        if (stage_num > 1) {
          stage_weights_result$completed_stages = all_results[1:(stage_num-1)]
        }

        return(stage_weights_result)  # Return the infeasible result
      }

      # Extract weights for next stage and check they're complete -------------
      if (is.data.frame(stage_weights_result) && "weight" %in% names(stage_weights_result)) {
        # It's a tibble/data.frame with weight column
        weights_obj <- stage_weights_result  # Store the full object for later
        current_weights <- stage_weights_result$weight
        dat_clean[[sprintf("stage_%d_weight", stage_num)]] <- current_weights
      } else {
        # Unexpected format
        stop(sprintf("Unexpected format returned from build_survey_weights in stage %d. Expected data.frame with 'weight' column.",
                     stage_num))
      }
      if (length(current_weights) != nrow(dat_clean)) {
        stop(sprintf("Stage %d produced %d weights but data has %d rows",
                     stage_num, length(current_weights), nrow(dat_clean)))
      }

      # Store stage results ---------------------------------------------------
      all_results[[stage_num]] <- list(
        # Stage identification
        stage_num = stage_num,

        # Weights output (full data.frame with attributes)
        weights = weights_obj,

        # Constraint information
        constraints = list(
          used = stage_constraints,        # Original constraint objects
          matrix = constraint_list,        # Sparse matrix representation
          n_constraints = length(stage_constraints),
          achievement = attr(weights_obj, "constraint_report")
        ),

        # Bias correction details (if applicable)
        bias_correction = if (!is.null(selection_probs)) {
          list(
            selection_probs = selection_probs,
            target_weights = attr(target_weights, "ht_weights"),
            alpha = stage_alpha,
            weighting_target_type = class(stage_weighting_target)[1]
          )
        } else NULL,

        # Stage configuration
        config = list(
          tolerance = stage_tol_config$default,
          alpha = stage_alpha,
          cap = stage$cap %||% cap,
          verbose = verbose
        ),

        # Stage metrics (extracted from weights_obj attributes)
        metrics = list(
          ess = attr(weights_obj, "ess"),
          efficiency = attr(weights_obj, "efficiency"),
          cv = attr(weights_obj, "cv"),
          solver_status = attr(weights_obj, "solver_status"),
          weight_stats = attr(weights_obj, "weight_stats")
        )
      )

      # Report stage results
      message(sprintf("\nStage %d complete:", stage_num))
      message(sprintf("  ESS: %.0f (%.1f%% efficiency)",
                      attr(stage_weights_result, "ess"),
                      attr(stage_weights_result, "efficiency") * 100))
      message(sprintf("  Weight CV: %.3f", attr(stage_weights_result, "cv")))
    }

    # Final result uses last stage weights
    final_weights <- current_weights

    # Handle fully unconstrained respondents ----------------------------------
    # Get the last stage's constraint_list
    last_constraint_list <- all_results[[length(all_results)]]$constraint_list
    n_fully_unconstrained <- 0
    fully_unconstrained_indices <- NULL

    if (!is.null(last_constraint_list)) {
      # Run the adjustment on the weight vector
      adjusted_weights <- adjust_fully_unconstrained(
        data = dat_clean,
        weights = final_weights,  # This is the numeric vector
        constraint_list = last_constraint_list,
        verbose = verbose,
        config = config
      )

      # Capture diagnostics from attributes
      n_fully_unconstrained <- attr(adjusted_weights, "n_fully_unconstrained") %||% 0
      fully_unconstrained_indices <- attr(adjusted_weights, "fully_unconstrained_indices")

      # Update the final weights vector
      final_weights <- adjusted_weights

      # Update the weights in the final stage's data.frame
      all_results[[length(all_results)]]$weights$weight <- adjusted_weights

      # Add the unconstrained info as attributes to the weights data.frame
      attr(all_results[[length(all_results)]]$weights, "n_fully_unconstrained") <- n_fully_unconstrained
      attr(all_results[[length(all_results)]]$weights, "fully_unconstrained_indices") <- fully_unconstrained_indices

      if (n_fully_unconstrained > 0) {
        message(sprintf("\nNote: %d respondents (%.1f%%) had no constraints and received median weight",
                        n_fully_unconstrained,
                        n_fully_unconstrained / nrow(dat_clean) * 100))
      }
    }

    # Add final weights to data
    dat_clean$final_weight <- final_weights

    # Create final data object ------------------------------------------------
    if (inherits(raw_data, "survey_data")) {
      # Preserve all fields from original survey_data, updating only the dat field
      final_data <- raw_data
      final_data$dat <- dat_clean
    } else {
      final_data <- dat_clean
    }

    # Calculate overall bias correction effectiveness -------------------------
    if (any(sapply(stages, function(s) !is.null(s$weighting_target)))) {
      # Find the last stage with bias correction
      for (i in rev(seq_along(all_results))) {
        if (!is.null(all_results[[i]]$selection_probs)) {
          sel_probs <- all_results[[i]]$selection_probs
          bias_check <- final_weights * sel_probs

          message(sprintf("\nBias correction effectiveness:"))
          message(sprintf("  Median(w*pi): %.3f (ideal = 1.0)", median(bias_check)))
          message(sprintf("  Mean(w*pi): %.3f, SD: %.3f",
                          mean(bias_check), sd(bias_check)))
          break
        }
      }
    }

    # Print final summary
    message("\n=== WEIGHTING COMPLETE ===")
    message(sprintf("Effective Sample Size: %.0f (%.1f%% efficiency)",
                    attr(all_results[[length(all_results)]]$weights, "ess"),
                    attr(all_results[[length(all_results)]]$weights, "efficiency") * 100))
    message(sprintf("Weight CV: %.3f",
                    attr(all_results[[length(all_results)]]$weights, "cv")))

    # Extract final stage metrics ------------------------------------------
    final_stage_result <- all_results[[length(all_results)]]
    final_weights_df <- final_stage_result$weights

    # Get metrics from the final stage's weights data.frame attributes
    final_ess <- attr(final_weights_df, "ess")
    final_efficiency <- attr(final_weights_df, "efficiency")
    final_cv <- attr(final_weights_df, "cv")
    final_weight_stats <- attr(final_weights_df, "weight_stats")
    final_constraint_report <- attr(final_weights_df, "constraint_report")
    final_solver_status <- attr(final_weights_df, "solver_status")

    # Extract constraint variables from all stages
    all_constraint_vars <- character()

    for (stage_result in all_results) {
      # Method 1: From constraint metadata
      if (!is.null(stage_result$constraint_list) &&
          !is.null(stage_result$constraint_list$constraint_metadata)) {
        for (meta in stage_result$constraint_list$constraint_metadata) {
          if (!is.null(meta$metadata$variables)) {
            all_constraint_vars <- c(all_constraint_vars, meta$metadata$variables)
          }
        }
      }

      # Method 2: From constraints_used formulas
      if (!is.null(stage_result$constraints_used)) {
        for (constraint in stage_result$constraints_used) {
          if (inherits(constraint, "formula")) {
            vars <- all.vars(constraint)
            all_constraint_vars <- c(all_constraint_vars, vars)
          }
        }
      }

      # Method 3: Parse from constraint labels if other methods fail
      if (!is.null(stage_result$constraint_list$labels)) {
        for (label in stage_result$constraint_list$labels) {
          # Skip sum constraint
          if (!grepl("^Sum:", label)) {
            # Extract variable names from patterns like "Gender == Male"
            var_match <- regmatches(label, gregexpr("\\b[A-Za-z_][A-Za-z0-9_]*\\b(?=\\s*==)", label, perl = TRUE))
            if (length(var_match[[1]]) > 0) {
              all_constraint_vars <- c(all_constraint_vars, var_match[[1]])
            }
          }
        }
      }
    }

    # Clean up the variable list
    all_constraint_vars <- unique(all_constraint_vars)
    all_constraint_vars <- setdiff(all_constraint_vars,
                                   c("weight", "final_weight", "stage_weight",
                                     "respondent_id", "N", "n"))

    # Build cleaner structure
    return(structure(
      list(
        # Main outputs
        data = final_data,
        weights = final_weights_df,  # Full data.frame with respondent_id and weight columns

        # Stage details (preserved in full)
        stages = all_results,

        # Top-level summary from final stage
        summary = list(
          n_stages = length(stages),
          n_respondents = if (inherits(final_data, "survey_data")) {
            nrow(final_data$dat)
          } else {
            nrow(final_data)
          },
          n_constraints_final = all_results[[length(all_results)]][["constraints"]][["n_constraints"]],
          optimization_status = final_solver_status,
          n_fully_unconstrained = n_fully_unconstrained,
          fully_unconstrained_indices = fully_unconstrained_indices
        ),

        # Final weighting quality metrics
        metrics = list(
          ess = final_ess,
          efficiency = final_efficiency,
          cv = final_cv,
          weight_stats = final_weight_stats,
          min_weight = min(final_weights[final_weights > 0]),
          max_weight = max(final_weights),
          weight_ratio = max(final_weights) / min(final_weights[final_weights > 0])
        ),

        # Constraint achievement from final stage
        constraint_report = final_constraint_report,

        # Configuration used
        config = list(
          tol_config = tol_config,
          cap = cap,
          alpha = alpha,
          tolerance = tolerance
        )
      ),
      class = "unified_weighting_result"
    ))
  }
}

#' Print method for unified weighting results
#' @export
print.unified_weighting_result <- function(x, ...) {
  cat("=== UNIFIED WEIGHTING RESULT ===\n\n")

  # Summary
  cat("SUMMARY:\n")
  cat(sprintf("  Sample size: %d\n", x$summary$n_respondents))
  cat(sprintf("  Stages: %d\n", x$summary$n_stages))
  cat(sprintf("  Total constraints: %d\n", x$summary$n_constraints_total))
  cat(sprintf("  Constraint variables: %s\n",
              paste(x$summary$constraint_variables, collapse = ", ")))
  cat(sprintf("  Optimization status: %s\n", x$summary$optimization_status))

  # Final metrics
  cat("\nFINAL METRICS:\n")
  cat(sprintf("  ESS: %.0f (%.1f%% efficiency)\n",
              x$metrics$ess, x$metrics$efficiency * 100))
  cat(sprintf("  CV: %.3f\n", x$metrics$cv))
  cat(sprintf("  Weight range: [%.3f, %.3f]\n",
              x$metrics$min_weight, x$metrics$max_weight))
  cat(sprintf("  Weight ratio: %.1f\n", x$metrics$weight_ratio))

  # Stage summaries
  if (x$summary$n_stages > 1) {
    cat("\nSTAGE SUMMARIES:\n")
    for (i in seq_along(x$stages)) {
      stage <- x$stages[[i]]
      cat(sprintf("  Stage %d: %d constraints, ESS = %.0f (%.1f%%)\n",
                  i,
                  stage$constraints$n_constraints,
                  stage$metrics$ess,
                  stage$metrics$efficiency * 100))
    }
  }

  # Constraint achievement summary
  if (!is.null(x$constraint_report) && nrow(x$constraint_report) > 0) {
    n_achieved <- sum(abs(x$constraint_report$deviation_pct) < 1e-6)
    n_within_tol <- sum(x$constraint_report$deviation_pct <=
                          x$constraint_report$tolerance_used_pct)
    cat(sprintf("\nCONSTRAINT ACHIEVEMENT:\n"))
    cat(sprintf("  Exactly achieved: %d/%d\n",
                n_achieved, nrow(x$constraint_report)))
    cat(sprintf("  Within tolerance: %d/%d\n",
                n_within_tol, nrow(x$constraint_report)))
  }

  # Warnings
  if (x$summary$n_fully_unconstrained > 0) {
    cat(sprintf("\nWARNINGS:\n"))
    cat(sprintf("  - %d respondents (%.1f%%) had no active constraints\n",
                x$summary$n_fully_unconstrained,
                x$summary$n_fully_unconstrained / x$summary$n_respondents * 100))
  }

  cat("\nUse str() for detailed structure, or access components directly.\n")
  invisible(x)
}

#' Summary method for unified weighting results
#' @export
summary.unified_weighting_result <- function(object, ...) {
  structure(
    list(
      result = object,
      quality = data.frame(
        Stage = seq_along(object$stages),
        Constraints = sapply(object$stages, function(s) s$constraints$n_constraints),
        ESS = sapply(object$stages, function(s) s$metrics$ess),
        Efficiency = sapply(object$stages, function(s) s$metrics$efficiency),
        CV = sapply(object$stages, function(s) s$metrics$cv),
        Status = sapply(object$stages, function(s) s$metrics$solver_status)
      ),
      final_metrics = object$metrics,
      config = object$config
    ),
    class = "summary.unified_weighting_result"
  )
}

#' Print method for summary of unified weighting results
#' @export
print.summary.unified_weighting_result <- function(x, ...) {
  cat("=== WEIGHTING SUMMARY ===\n\n")

  cat("Stage Progression:\n")
  print(x$quality, row.names = FALSE)

  cat("\nFinal Outcome:\n")
  cat(sprintf("  Effective sample size: %.0f\n", x$final_metrics$ess))
  cat(sprintf("  Overall efficiency: %.1f%%\n", x$final_metrics$efficiency * 100))
  cat(sprintf("  Weight CV: %.3f\n", x$final_metrics$cv))
  cat(sprintf("  Weight range: [%.3f, %.3f]\n",
              x$final_metrics$min_weight, x$final_metrics$max_weight))

  cat("\nConfiguration:\n")
  cat(sprintf("  Tolerance mode: %s\n", x$config$tol_config$mode))
  cat(sprintf("  Default tolerance: %.3f\n", x$config$tolerance))
  cat(sprintf("  Weight cap: %.1f\n", x$config$cap))
  cat(sprintf("  Alpha (bias correction): %.2f\n", x$config$alpha))

  invisible(x)
}

# SECTION 9: FAILED RUN REPORTING ---------------------------------------------
#' Print method for infeasible weighting results
#' @param x weights_infeasible object
#' @param ... Additional arguments (ignored)
#' @export
print.weights_infeasible <- function(x, ...) {
  cat(x$error_message)

  if (!is.null(x$top_problems) && nrow(x$top_problems) > 0) {
    cat("\nMost problematic constraints:\n")
    cat("────────────────────────────\n")

    # Format the top problems nicely
    for (i in 1:min(5, nrow(x$top_problems))) {
      row <- x$top_problems[i, ]
      cat(sprintf("\n%d. %s\n", i, row$constraint))
      cat(sprintf("   Type: %s | Warning: %s\n", row$type, row$warning))
      cat(sprintf("   Target: %.1f | Max possible: %.1f | Feasibility: %.1f%%\n",
                  row$target, row$max_possible_value, row$feasibility_ratio * 100))
      cat(sprintf("   Respondents: %d (%.1f%%) | Min avg weight needed: %.2f\n",
                  row$n_respondents, row$pct_respondents, row$min_avg_weight_needed))
    }

    if (nrow(x$top_problems) > 5) {
      cat(sprintf("\n... and %d more problematic constraints\n", nrow(x$top_problems) - 5))
    }
  }

  cat("\n\nTo see full constraint diagnostics, access $constraint_diagnostics\n")
  invisible(x)
}

#' Summary method for infeasible weighting results
#' @param object weights_infeasible object
#' @param ... Additional arguments (ignored)
#' @export
summary.weights_infeasible <- function(object, ...) {
  cat("Infeasible Weight Optimization Summary\n")
  cat("=====================================\n\n")

  cat("Problem Overview:\n")
  cat(sprintf("  Status: %s\n", object$solver_status))
  cat(sprintf("  Sample size: %d\n", object$summary$sample_size))
  cat(sprintf("  Weight cap: %.1f\n", object$summary$cap))
  cat(sprintf("  Total constraints: %d\n", object$summary$n_constraints))

  cat("\nConstraint Issues:\n")
  cat(sprintf("  Empty constraints (no respondents): %d\n", object$summary$n_empty_constraints))
  cat(sprintf("  Mathematically impossible: %d\n", object$summary$n_infeasible_constraints))
  cat(sprintf("  Very challenging (>95%% of max): %d\n", object$summary$n_challenging_constraints))

  # Breakdown by constraint type
  diag <- object$constraint_diagnostics
  type_summary <- aggregate(
    list(
      count = diag$constraint,
      empty = diag$is_empty,
      infeasible = diag$is_infeasible
    ),
    by = list(type = diag$type),
    FUN = function(x) sum(x != FALSE)
  )

  cat("\nBy Constraint Type:\n")
  print(type_summary, row.names = FALSE)

  invisible(object)
}

