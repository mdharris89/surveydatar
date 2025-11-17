##### functions for managing variable classes, and question types #####

#' Convert Character Vector to Labelled Numeric Vector
#'
#' Converts a character vector to a sjlabelled/haven labelled numeric vector
#' using a lookup dictionary.
#'
#' @param character_vector A character vector to convert to labelled
#' @param new_variable_label Optional label for the created variable (set to NA to use existing label if present)
#' @param lookup_dict Optional data frame with mapping between character values and numeric values
#'                   First column must contain numeric values, second column must contain character labels
#'                   If NULL, creates a lookup based on value frequency
#' @param label_col_name Name of column in lookup_dict containing labels (defaults to second column)
#' @param value_col_name Name of column in lookup_dict containing values (defaults to first column)
#' @param partition_threshold When creating a lookup dictionary, values with frequency above
#'                           this threshold are assigned values first
#'
#' @return A labelled numeric vector
#' @export
#'
#' @examples
#' # With existing lookup dictionary
#' lookup <- data.frame(value = c(1, 2), label = c("Yes", "No"))
#' character_to_labelled_via_dict(c("Yes", "No", "Yes"), "Response", lookup)
#'
#' # Auto-generating lookup dictionary
#' character_to_labelled_via_dict(c("Apple", "Banana", "Apple", "Apple", "Cherry"))
character_to_labelled_via_dict <- function(character_vector, new_variable_label = NULL,
                                           lookup_dict = NULL, label_col_name = NULL,
                                           value_col_name = NULL, partition_threshold = 1) {

  # Input validation
  if (!is.character(character_vector) && !is.factor(character_vector)) {
    stop("character_vector must be a character or factor vector")
  }

  # If input is a factor, convert to character
  if (is.factor(character_vector)) {
    character_vector <- as.character(character_vector)
  }

  # Get existing label if new one not provided
  if (is.null(new_variable_label)) {
    new_variable_label <- attr(character_vector, "label", exact = TRUE)
  }

  # Create lookup dictionary if none provided
  if (is.null(lookup_dict)) {
    freq_table <- as.data.frame(table(character_vector))
    names(freq_table) <- c("character_vector", "Freq")

    # Partition into high frequency and low frequency values
    frequent_values <- freq_table[freq_table$Freq > partition_threshold, ]
    infrequent_values <- freq_table[freq_table$Freq <= partition_threshold, ]

    # Order infrequent values by descending frequency
    infrequent_values <- infrequent_values[order(infrequent_values$Freq, decreasing = TRUE), ]

    # Combine, placing frequent values first
    ordered_names <- c(
      as.character(frequent_values$character_vector),
      as.character(infrequent_values$character_vector)
    )

    # Remove NAs
    ordered_names <- ordered_names[!is.na(ordered_names)]

    # Create lookup dictionary
    lookup_dict <- data.frame(
      value = seq_along(ordered_names),
      label = ordered_names,
      stringsAsFactors = FALSE
    )

    value_col_name <- "value"
    label_col_name <- "label"
  } else {
    # Validate lookup_dict
    if (!is.data.frame(lookup_dict) || ncol(lookup_dict) < 2) {
      stop("lookup_dict must be a data frame with at least two columns")
    }

    # Determine column names if not provided
    if (is.null(value_col_name)) {
      value_col_name <- names(lookup_dict)[1]
    }
    if (is.null(label_col_name)) {
      label_col_name <- names(lookup_dict)[2]
    }

    # Ensure value column is numeric
    if (!is.numeric(lookup_dict[[value_col_name]])) {
      lookup_dict[[value_col_name]] <- as.numeric(lookup_dict[[value_col_name]])
      message("Converted values in lookup dictionary to numeric")
    }
  }

  # Check for missing values in lookup
  missing_values <- setdiff(
    unique(character_vector[!is.na(character_vector)]),
    lookup_dict[[label_col_name]]
  )

  # Add missing values to lookup dictionary
  if (length(missing_values) > 0) {
    for (val in missing_values) {
      new_value <- max(lookup_dict[[value_col_name]], na.rm = TRUE) + 1
      lookup_dict <- rbind(
        lookup_dict,
        setNames(
          list(new_value, val),
          c(value_col_name, label_col_name)
        )
      )
      message(sprintf("Added missing value '%s' to lookup with value %d", val, new_value))
    }
  }

  # Convert character values to numeric using lookup
  numeric_values <- numeric(length(character_vector))
  for (i in seq_along(character_vector)) {
    if (is.na(character_vector[i])) {
      numeric_values[i] <- NA_real_
    } else {
      idx <- match(character_vector[i], lookup_dict[[label_col_name]])
      numeric_values[i] <- if (is.na(idx)) NA_real_ else lookup_dict[[value_col_name]][idx]
    }
  }

  # Create the labels vector correctly: a numeric vector with character names
  labels <- lookup_dict[[value_col_name]]
  names(labels) <- lookup_dict[[label_col_name]]

  # Create the labelled vector
  if (is.na(new_variable_label) || is.null(new_variable_label)) {
    out <- haven::labelled(numeric_values, labels = labels)
  } else {
    out <- haven::labelled(
      numeric_values,
      label = new_variable_label,
      labels = labels
    )
  }

  return(out)
}


#' Convert a single atomic vector to a labelled vector
#'
#' Converts logical, character, factor, and numeric vectors to labelled format.
#'
#' @param x A logical, character, factor or numeric vector to convert
#' @param max_labels Maximum number of distinct values to auto-label
#' @param MRpositivelabel Label for value 1 in multiresponse vars
#' @param MRnegativelabel Label for value 0 in multiresponse vars
#' @param variable_label Optional string to force as the variable's label
#' @param value_label_lookup Optional named list of per-variable lookup tables
#' @param var_name Internal parameter: the name of x, used for lookup in value_label_lookup
#'
#' @return A labelled vector (from haven/sjlabelled), or x unchanged if ineligible
#' @export
realiselabelled_vec <- function(x,
                                 max_labels = 12,
                                 MRpositivelabel = "Selected",
                                 MRnegativelabel = "Not selected",
                                 variable_label = NULL,
                                 value_label_lookup = NULL,
                                 var_name = NULL) {

  # Preserve existing variable label
  existing_label <- sjlabelled::get_label(x)

  # Default multi-response labels
  MRlabels <- c(0, 1)
  names(MRlabels) <- c(MRnegativelabel, MRpositivelabel)


  if (inherits(x, "Date")) {
    if (!is.null(variable_label) && nzchar(variable_label)) {
      attr(x, "label") <- variable_label
    }
    return(x)
  }

  # If already labelled, only update variable label if requested
  if (sjlabelled::is_labelled(x)) {
    if (!is.null(variable_label) && nzchar(variable_label)) {
      x <- sjlabelled::set_label(x, label = variable_label)
    }
    return(x)
  }

  # Process by type
  if (is.logical(x)) {
    y <- haven::labelled(as.numeric(x))

    if (!is.null(value_label_lookup[[var_name]])) {
      y <- sjlabelled::set_labels(y, labels = value_label_lookup[[var_name]])
    } else {
      y <- sjlabelled::set_labels(y, labels = MRlabels)
    }

  } else if (is.character(x)) {
    # Use custom dictionary if provided
    if (!is.null(value_label_lookup[[var_name]])) {
      y <- character_to_labelled_via_dict(
        x,
        new_variable_label = existing_label,
        lookup_dict = value_label_lookup[[var_name]]
      )
    } else if (length(unique(stats::na.omit(x))) <= max_labels) {
      y <- character_to_labelled_via_dict(
        x,
        new_variable_label = existing_label
      )
    } else {
      if (!is.null(variable_label) && nzchar(variable_label)) {
        attr(x, "label") <- variable_label
      }
      return(x)  # Too many unique values
    }

  } else if (is.factor(x)) {
    levs <- levels(x)
    y <- haven::labelled(as.numeric(x))

    if (!is.null(value_label_lookup[[var_name]])) {
      labels_to_use <- value_label_lookup[[var_name]]
    } else {
      labels_to_use <- stats::setNames(seq_along(levs), levs)
    }
    y <- sjlabelled::set_labels(y, labels = labels_to_use)

  } else if (is.numeric(x)) {
    vals_to_check <- x
    if (inherits(vals_to_check, "haven_labelled")) {
      vals_to_check <- haven::zap_labels(vals_to_check)
    }
    unique_vals <- unique(stats::na.omit(vals_to_check))

    if (length(unique_vals) <= 2 && all(unique_vals %in% c(0, 1))) {
      # Multiple-response pattern
      y <- haven::labelled(x)
      if (!is.null(value_label_lookup[[var_name]])) {
        labels_to_use <- value_label_lookup[[var_name]]
      } else {
        labels_to_use <- MRlabels
      }
      y <- sjlabelled::set_labels(y, labels = labels_to_use)

    } else if (!is.null(value_label_lookup[[var_name]])) {
      y <- haven::labelled(x)
      y <- sjlabelled::set_labels(y, labels = value_label_lookup[[var_name]])

    } else if (length(unique_vals) <= max_labels) {
      # Just convert to labelled without value labels
      y <- haven::labelled(x)

    } else {
      if (!is.null(variable_label) && nzchar(variable_label)) {
        attr(x, "label") <- variable_label
      }
      return(x)  # Too many unique values
    }

  } else {
    # Other types left untouched
    return(x)
  }

  # Apply variable label
  if (!is.null(variable_label) && nzchar(variable_label)) {
    y <- sjlabelled::set_label(y, label = variable_label)
  } else if (!is.null(existing_label) && nzchar(existing_label)) {
    y <- sjlabelled::set_label(y, label = existing_label)
  }

  return(y)
}


#' Convert selected columns of a data frame to labelled variables
#'
#' Attempts to convert columns in a data frame to labelled variables where possible.
#' Character variables with up to max_labels unique values are converted to numeric
#' with value labels. Existing labelled variables are preserved.
#'
#' @param data A data frame to process
#' @param cols Columns to transform. Can be column names (character vector),
#'   column indices (integer vector), or tidyselect expression if dplyr is loaded.
#'   If NULL, processes all columns.
#' @param max_labels Maximum number of unique values to consider for labelling
#' @param MRpositivelabel Label to use for positive values in multiresponse variables
#' @param MRnegativelabel Label to use for negative values in multiresponse variables
#' @param variable_labels Optional data frame with columns 'variable name' and
#'   'question with suffix' to override variable labels
#' @param value_label_lookup Optional named list of label lookup tables with names
#'   corresponding to variable names
#'
#' @return A data frame with specified columns converted to labelled variables where appropriate
#' @export
#'
#' @examples
#' test_data <- data.frame(
#'   id = 1:5,
#'   gender = c("Male", "Female", "Female", "Male", "Other"),
#'   is_customer = c(TRUE, FALSE, TRUE, TRUE, FALSE),
#'   score = c(10, 20, 15, 5, 25)
#' )
#' realiselabelled(test_data)
realiselabelled <- function(data,
                             cols = NULL,
                             max_labels = 12,
                             MRpositivelabel = "Selected",
                             MRnegativelabel = "Not selected",
                             variable_labels = NULL,
                             value_label_lookup = NULL) {

  # Input validation
  if (!is.data.frame(data)) {
    stop("'data' must be a data frame")
  }

  if (ncol(data) == 0) {
    return(data)
  }

  # Validate variable_labels if provided
  if (!is.null(variable_labels)) {
    if (!is.data.frame(variable_labels)) {
      stop("'variable_labels' must be a data frame")
    }
    required_cols <- c("variable name", "question with suffix")
    missing_cols <- setdiff(required_cols, names(variable_labels))
    if (length(missing_cols) > 0) {
      stop("'variable_labels' must contain columns: ",
           paste(missing_cols, collapse = ", "))
    }
  }

  # Determine which columns to process
  if (is.null(cols)) {
    cols_to_process <- names(data)
  } else if (is.character(cols)) {
    cols_to_process <- cols
  } else if (is.numeric(cols)) {
    if (any(cols < 1) || any(cols > ncol(data))) {
      stop("Column indices out of range")
    }
    cols_to_process <- names(data)[cols]
  } else {
    # Try tidyselect if available and cols is not simple vector
    if (requireNamespace("dplyr", quietly = TRUE) &&
        requireNamespace("tidyselect", quietly = TRUE)) {
      tryCatch({
        cols_to_process <- names(tidyselect::eval_select(rlang::enquo(cols), data))
      }, error = function(e) {
        stop("Invalid column specification. Use column names, indices, or tidyselect expressions.")
      })
    } else {
      stop("For complex column selection, please install dplyr and tidyselect packages, or use column names/indices directly")
    }
  }

  # Validate that specified columns exist
  missing_cols <- setdiff(cols_to_process, names(data))
  if (length(missing_cols) > 0) {
    stop("Columns not found in data: ", paste(missing_cols, collapse = ", "))
  }

  # Process each specified column
  for (col_name in cols_to_process) {
    # Get variable label if provided
    var_label <- NULL
    if (!is.null(variable_labels)) {
      idx <- match(col_name, variable_labels[["variable name"]])
      if (!is.na(idx)) {
        var_label <- variable_labels[["question with suffix"]][idx]
      }
    }

    # Transform the column
    data[[col_name]] <- realiselabelled_vec(
      data[[col_name]],
      max_labels = max_labels,
      MRpositivelabel = MRpositivelabel,
      MRnegativelabel = MRnegativelabel,
      variable_label = var_label,
      value_label_lookup = value_label_lookup,
      var_name = col_name
    )
  }

  return(data)
}

##### functions for creating or manipulating variables #####

#' update_labelled_values
#'
#' replaces a value in a labelled vector and updates/adds value-labels to keep metadata consistent
#'
#' @param x A labelled vector (anything for which sjlabelled::is_labelled() is TRUE)
#' @param old_value Value to be replaced (can be NA)
#' @param new_value Value to replace old_value with
#' @param new_label Label to attach to new_value (NULL or "" if no label desired)
#' @param rows_to_ignore Logical vector of same length as x indicating which rows to leave unchanged (NULL = none)
#' @param force_add_new_label Logical. If old_value not found, should label for new_value still be added/updated?
#' @param noisy Logical. If TRUE, prints progress notes
#'
#' @return A labelled vector with both values and labels attribute updated
#' @export
#'
#' @examples
#' # Create a labelled vector
#' v <- sjlabelled::set_labels(c(1, NA, 2, NA), labels = c("Yes" = 1, "Maybe" = 2))
#'
#' # Replace NA with 0 and label it
#' v2 <- update_labelled_values(v, NA, 0, "No / missing")
#' sjlabelled::get_labels(v2)
#'
#' # Change specific rows only
#' rows_to_skip <- c(TRUE, TRUE, FALSE, FALSE)
#' v3 <- update_labelled_values(v2, 0, 9, "Definitely maybe", rows_to_skip)
#' sjlabelled::get_labels(v3)
update_labelled_values <- function(x,
                                   old_value,
                                   new_value,
                                   new_label = NULL,
                                   rows_to_ignore = NULL,
                                   force_add_new_label = FALSE,
                                   noisy = FALSE) {

  ### validation
  if (!sjlabelled::is_labelled(x)) {
    if (noisy) message("Input is not a labelled vector - returned unchanged.")
    return(x)
  }

  n <- length(x)
  if (is.null(rows_to_ignore)) {
    rows_to_ignore <- rep(FALSE, n)
  } else {
    if (length(rows_to_ignore) != n) {
      stop("rows_to_ignore must have the same length as x")
    }
  }

  ### identify rows to modify
  target_rows <- !rows_to_ignore & if (is.na(old_value)) {
    is.na(x)
  } else {
    x == old_value
  }

  if (!force_add_new_label && !any(target_rows, na.rm = TRUE)) {
    if (noisy) message("Nothing to replace - returned unchanged.")
    return(x)
  }

  ### replace values
  if (any(target_rows, na.rm = TRUE)) {
    x[target_rows] <- new_value
    if (noisy) message(sum(target_rows, na.rm = TRUE), " value(s) replaced.")
  }

  ### update labels attribute
  labs <- attr(x, "labels")
  if (is.null(labs)) labs <- numeric(0)  # initialize if no labels exist

  # remove label for old_value if it exists and isn't NA
  if (!is.na(old_value) && old_value %in% labs)
    labs <- labs[labs != old_value]

  # remove any existing label for new_value
  labs <- labs[labs != new_value]

  # add new label if provided
  if (!is.null(new_label) && nzchar(new_label))
    labs <- c(labs, stats::setNames(new_value, new_label))

  attr(x, "labels") <- labs

  return(x)
}

#' conditionally_replace_NAs_in_multiresponse
#'
#' converts NAs to a specified code within multi-response question groups, but only where at least one option was selected
#'
#' intended for "select-all-that-apply" questions where each option is stored as a binary variable (1=selected, NA=not answered)
#' after conversion, maintains metadata by properly labelling both the new value (e.g. 0="Not selected") and existing value (1="Selected")
#'
#' @param survey_obj A survey_data object, or NULL if using temp_dat + temp_dpdict directly
#' @param temp_dat A data frame with labelled columns
#' @param temp_dpdict A dpdict with at least variable_names column
#' @param newvalue Value to use in place of NA (typically 0)
#' @param newlabel Label for the new value
#' @param variable_name_sep Separator used in variable names if deriving question_group
#' @param relabelpositives Logical. Whether to also relabel the positive values (1)
#' @param newpositivelabel Label to use for positive values when relabelpositives=TRUE
#' @param noisy Logical. If TRUE, prints progress notes
#'
#' @return Updated temp_dat with NAs replaced and labels updated, or updated survey_obj if provided
#' @export
#'
#' @examples
#' # Create test data
#' dat <- data.frame(
#'   Q1_1 = sjlabelled::set_labels(c(1, NA, NA, 1), labels = c("Yes" = 1)),
#'   Q1_2 = sjlabelled::set_labels(c(NA, 1, NA, NA), labels = c("Yes" = 1)),
#'   Q1_3 = sjlabelled::set_labels(c(NA, NA, 1, NA), labels = c("Yes" = 1)),
#'   Q2_1 = sjlabelled::set_labels(c(NA, NA, NA, NA)),
#'   Q2_2 = sjlabelled::set_labels(c(NA, NA, NA, NA))
#' )
#'
#' # Create dpdict
#' dpdict <- data.frame(
#'   variable_names = c("Q1_1","Q1_2","Q1_3","Q2_1","Q2_2"),
#'   question_group = c("Q1","Q1","Q1","Q2","Q2"),
#'   stringsAsFactors = FALSE
#' )
#'
#' # Replace NAs conditionally
#' out <- conditionally_replace_NAs_in_multiresponse(NULL, dat, dpdict)
#'
#' # Check results
#' table(out$Q1_1, useNA = "ifany")
#' sjlabelled::get_labels(out$Q1_1)
conditionally_replace_NAs_in_multiresponse <- function(survey_obj = NULL,
                                                temp_dat = NULL,
                                                temp_dpdict = NULL,
                                                newvalue = 0,
                                                newlabel = "Not selected",
                                                variable_name_sep = "_",
                                                relabelpositives = TRUE,
                                                newpositivelabel = "Selected",
                                                noisy = FALSE) {

  ### validation
  is_survey <- !is.null(survey_obj) && inherits(survey_obj, "survey_data")

  if (is_survey) {
    temp_dat <- survey_obj$dat
    temp_dpdict <- survey_obj$dpdict
    if (noisy) message("Processing a survey_data object.")
  }

  stopifnot("temp_dat must be a data frame" = is.data.frame(temp_dat),
            "temp_dpdict must be a data frame" = is.data.frame(temp_dpdict),
            "variable_names column is required in temp_dpdict" = "variable_names" %in% names(temp_dpdict))

  if (!requireNamespace("sjlabelled", quietly = TRUE)) {
    stop("Package 'sjlabelled' is required but not installed.")
  }

  ### ensure question_group column exists
  if (!"question_group" %in% names(temp_dpdict)) {
    temp_dpdict$question_group <- sub(paste0(variable_name_sep, ".*"),
                                      "",
                                      temp_dpdict$variable_names)
    if (noisy) message("question_group column derived from variable names.")
  }

  ### process each question group
  group_list <- split(temp_dpdict$variable_names, temp_dpdict$question_group)

  for (vars in group_list) {
    # Only use variables that exist in the data
    vars <- intersect(vars, names(temp_dat))
    if (length(vars) == 0) next

    # Skip if not all variables are numeric and labelled
    all_labelled <- vapply(temp_dat[vars],
                           function(v) is.numeric(v) && sjlabelled::is_labelled(v),
                           logical(1))
    if (!all(all_labelled)) next

    # Make sure the group is binary, i.e. a multiresponse question
    group_vals <- unique(unlist(lapply(temp_dat[vars], function(v) {
      if (inherits(v, "haven_labelled")) {
        v <- haven::zap_labels(v)
      }
      v[!is.na(v)]
    })))
    if (!all(group_vals %in% c(0, 1))) {          # anything else? → skip
      if (noisy)
        message(sprintf("Skipping group '%s': values other than 0/1 detected (%s).",
                        vars,
                        paste(setdiff(group_vals, c(0, 1)), collapse = ", ")))
      next
    }

    # Find rows where at least one option was selected
    any_resp <- rowSums(!is.na(temp_dat[vars])) > 0
    if (!any(any_resp)) next

    # Update each variable in the group
    for (v in vars) {
      # Replace NAs with newvalue
      temp_dat[[v]] <- update_labelled_values(
        x = temp_dat[[v]],
        old_value = NA,
        new_value = newvalue,
        new_label = newlabel,
        rows_to_ignore = !any_resp,
        force_add_new_label = TRUE
      )

      # Optionally relabel positive values
      val_to_check <- temp_dat[[v]]
      if (inherits(val_to_check, "haven_labelled")) {
        val_to_check <- haven::zap_labels(val_to_check)
      }
      if (relabelpositives && any(val_to_check == 1, na.rm = TRUE)) {
        temp_dat[[v]] <- update_labelled_values(
          x = temp_dat[[v]],
          old_value = 1,
          new_value = 1,
          new_label = newpositivelabel,
          force_add_new_label = TRUE
        )
      }
    }
  }

  ### return appropriate object
  if (is_survey) {
    survey_obj$dat <- temp_dat
    survey_obj$dpdict <- temp_dpdict
    return(survey_obj)
  } else {
    return(temp_dat)
  }
}

##### functions for exporting #####

#' Fix Integer/Double Type Issues for SPSS Export
#'
#' Converts numeric columns with integer values to actual integer type
#' while properly preserving labels for SPSS export.
#'
#' @param data A data frame or survey_data object
#' @return A modified data frame ready for haven::write_sav()
#' @export
fix_for_spss_export <- function(data) {
  # Extract data from survey_data object if needed
  if (inherits(data, "survey_data")) {
    data <- data$dat
  }
  # Create a copy to avoid modifying the original
  result <- data
  # Process each column
  for (col_name in names(result)) {
    col <- result[[col_name]]
    # Skip non-numeric columns
    if (!is.numeric(col)) next
    # Check if this column has integer values only
    if (all(is.na(col) | (col == floor(col)))) {
      # Store labels before conversion
      var_label <- attr(col, "label", exact = TRUE)
      value_labels <- attr(col, "labels", exact = TRUE)
      # Convert to integer first (plain conversion without labels)
      result[[col_name]] <- as.integer(col)
      # If it was labelled, rebuild labels properly
      if (!is.null(value_labels)) {
        # Ensure value labels are also integers
        int_labels <- as.integer(value_labels)
        names(int_labels) <- names(value_labels)
        # Apply the labels using haven directly
        result[[col_name]] <- haven::labelled(
          result[[col_name]],
          labels = int_labels
        )
        # Restore variable label if it exists
        if (!is.null(var_label)) {
          attr(result[[col_name]], "label") <- var_label
        }
      }
    }
  }
  return(result)
}

#' Collapse Wide-Format Survey Questions
#'
#' Transforms wide-format survey questions into a single variable by collapsing multiple
#' variables based on a reference variable. This is useful when you have the same question
#' asked across different categories (e.g., satisfaction_male, satisfaction_female) and
#' want to create a single variable using a category variable to determine which value
#' each respondent should receive.
#'
#' @param temp_dat A survey_data object containing the data and metadata to be collapsed.
#' @param vars_to_collapse Character vector of variable names to collapse. These should
#'   follow a consistent naming pattern with the collapse values embedded in the names.
#' @param collapse_on Character string specifying the variable name that contains the
#'   values used to determine which collapsed variable to use for each respondent.
#' @param var_name_sep Character string specifying the separator used in variable names.
#'   Default is "_".
#' @param var_label_sep Character string specifying the separator used in variable labels.
#'   Default is " - ".
#' @param interactive_mode Logical indicating whether to show a preview and ask for
#'   confirmation before collapsing. Default is FALSE.
#'
#' @return A new survey_data object with collapsed variables. The original wide-format
#'   variables are removed and replaced with the collapsed versions. Variable labels
#'   are updated to remove the category-specific portions.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Create sample data with wide-format questions
#' dat <- data.frame(
#'   id = 1:5,
#'   gender = c("male", "female", "male", "female", "other"),
#'   satisfaction_male = c(5, NA, 4, NA, NA),
#'   satisfaction_female = c(NA, 3, NA, 5, NA),
#'   satisfaction_other = c(NA, NA, NA, NA, 4)
#' )
#' survey_obj <- create_survey_data(dat)
#'
#' # Collapse the satisfaction variables based on gender
#' collapsed <- collapse_wide_question(
#'   survey_obj,
#'   vars_to_collapse = c("satisfaction_male", "satisfaction_female", "satisfaction_other"),
#'   collapse_on = "gender"
#' )
#' }
collapse_wide_question <- function(temp_dat, vars_to_collapse, collapse_on,
                                   var_name_sep = "_", var_label_sep = " - ", interactive_mode = FALSE) {

  # Validate input variables exist
  missing_vars <- vars_to_collapse[!vars_to_collapse %in% names(temp_dat$dat)]
  if (length(missing_vars) > 0) {
    cat("Warning: The following variables were not found and will be ignored:\n")
    cat(paste(missing_vars, collapse = ", "), "\n")
    vars_to_collapse <- vars_to_collapse[vars_to_collapse %in% names(temp_dat$dat)]
  }

  if (length(vars_to_collapse) == 0) {
    cat("No valid variables found to collapse.\n")
    return(invisible(NULL))
  }

  # Get all variables
  all_vars <- vars_to_collapse

  if (length(all_vars) == 0) {
    cat("No variables found with prefix:", question_prefix, "\n")
    return(invisible(NULL))
  }

  # Get unique values from collapse_on variable
  collapse_values <- sort(unique(temp_dat$dat[[collapse_on]]))
  collapse_values <- collapse_values[!is.na(collapse_values)]

  # Split all variable names by separator
  var_splits <- strsplit(all_vars, var_name_sep, fixed = TRUE)

  # Find maximum number of parts
  max_parts <- max(sapply(var_splits, length))

  # For each position, check if it contains exactly the collapse values
  collapse_position <- NULL

  for (pos in 1:max_parts) {
    # Extract values at this position across all variables
    pos_values <- unique(sapply(var_splits, function(x) {
      if (length(x) >= pos) x[pos] else NA
    }))
    pos_values <- pos_values[!is.na(pos_values)]

    # Check if these match our collapse values
    if (length(pos_values) == length(collapse_values) &&
        all(pos_values %in% as.character(collapse_values)) &&
        all(as.character(collapse_values) %in% pos_values)) {
      collapse_position <- pos
      break
    }
  }

  if (is.null(collapse_position)) {
    cat("Could not identify position containing collapse values.\n")
    cat("Expected values:", paste(collapse_values, collapse = ", "), "\n")
    return(invisible(NULL))
  }

  # Group variables by their pattern (everything except the collapse position)
  var_groups <- list()

  for (i in seq_along(all_vars)) {
    var <- all_vars[i]
    parts <- var_splits[[i]]

    # Create pattern by replacing collapse position with placeholder
    if (length(parts) >= collapse_position) {
      pattern_parts <- parts
      pattern_parts[collapse_position] <- "COLLAPSE"
      pattern <- paste(pattern_parts, collapse = var_name_sep)

      # Store variable info
      if (!pattern %in% names(var_groups)) {
        var_groups[[pattern]] <- list(
          vars = character(),
          collapse_pos = collapse_position,
          first_var_position = which(names(temp_dat$dat) == var)
        )
      }
      var_groups[[pattern]]$vars <- c(var_groups[[pattern]]$vars, var)
    }
  }

  for (pattern in names(var_groups)) {
    group_info <- var_groups[[pattern]]
    new_var_name <- gsub("COLLAPSE", "", pattern, fixed = TRUE)
    new_var_name <- gsub(paste0(var_name_sep, var_name_sep), var_name_sep, new_var_name, fixed = TRUE)
    new_var_name <- gsub(paste0(var_name_sep, "$"), "", new_var_name)


    if(interactive_mode == TRUE){
      cat(sprintf("Pattern: %s\n", pattern))
      cat(sprintf("  Variables: %s to %s (%d vars)\n",
                  group_info$vars[1],
                  tail(group_info$vars, 1),
                  length(group_info$vars)))
      cat(sprintf("  Will collapse to: %s\n", new_var_name))
    }
  }

  if(interactive_mode == TRUE){
    # Ask for confirmation
    cat("\nProceed with collapsing? (y/n): ")
    response <- readline()
    if (tolower(response) != "y") {
      return(invisible(NULL))
    }
  }

  # Perform the collapsing
  new_dat <- temp_dat$dat
  new_dpdict <- temp_dat$dpdict
  vars_to_remove <- character()
  new_vars_created <- character()
  new_var_positions <- list()

  # Get collapse values for all respondents at once
  respondent_collapse_vals <- as.character(new_dat[[collapse_on]])

  for (pattern in names(var_groups)) {
    group_info <- var_groups[[pattern]]
    group_vars <- group_info$vars

    # Create new variable name
    new_var_name <- gsub("COLLAPSE", "", pattern, fixed = TRUE)
    new_var_name <- gsub(paste0(var_name_sep, var_name_sep), var_name_sep, new_var_name, fixed = TRUE)
    new_var_name <- gsub(paste0(var_name_sep, "$"), "", new_var_name)

    # Create a matrix of all group variables
    var_matrix <- as.matrix(new_dat[, group_vars, drop = FALSE])

    # Create index for which column each respondent should use
    col_indices <- match(respondent_collapse_vals,
                         sapply(strsplit(group_vars, var_name_sep, fixed = TRUE),
                                function(x) x[collapse_position]))

    # Extract values using matrix indexing (vectorized)
    row_indices <- seq_len(nrow(var_matrix))
    valid_indices <- !is.na(col_indices)

    new_values <- rep(NA, nrow(var_matrix))
    if (any(valid_indices)) {
      new_values[valid_indices] <- var_matrix[cbind(row_indices[valid_indices],
                                                    col_indices[valid_indices])]
    }

    new_dat[[new_var_name]] <- new_values
    new_vars_created <- c(new_vars_created, new_var_name)
    new_var_positions[[new_var_name]] <- group_info$first_var_position

    # Copy attributes from the first variable in the group
    first_var <- group_vars[1]
    if (first_var %in% names(new_dat)) {
      attributes(new_dat[[new_var_name]]) <- attributes(new_dat[[first_var]])
    }

    # Handle variable labels - find the non-common part
    var_labels <- new_dpdict$variable_labels[match(group_vars, new_dpdict$variable_names)]
    var_labels <- var_labels[!is.na(var_labels)]

    if (length(var_labels) > 1) {
      # Split labels and find the changing part
      label_splits <- strsplit(var_labels, var_label_sep, fixed = TRUE)

      # Find which part changes across labels
      max_label_parts <- max(sapply(label_splits, length))
      common_parts <- character()

      for (pos in 1:max_label_parts) {
        pos_values <- unique(sapply(label_splits, function(x) {
          if (length(x) >= pos) x[pos] else NA
        }))
        pos_values <- pos_values[!is.na(pos_values)]

        if (length(pos_values) == 1) {
          # This part is common
          common_parts[pos] <- pos_values[1]
        } else {
          # This part varies - mark it for removal
          common_parts[pos] <- NA
        }
      }

      # Reconstruct label without the varying part
      new_label <- paste(common_parts[!is.na(common_parts)], collapse = var_label_sep)
    } else if (length(var_labels) == 1) {
      # Only one label, use it as is
      new_label <- var_labels[1]
    } else {
      # No labels found
      new_label <- new_var_name
    }

    # Update dpdict
    old_label_row <- which(new_dpdict$variable_names == first_var)[1]
    if (!is.na(old_label_row) && !new_var_name %in% new_dpdict$variable_names) {
      new_row <- new_dpdict[old_label_row, ]
      new_row$variable_names <- new_var_name
      new_row$variable_labels <- new_label
      new_dpdict <- rbind(new_dpdict, new_row)
    }
    new_dat <- update_dat_from_dpdict(new_dat, new_dpdict)

    # Mark original variables for removal
    vars_to_remove <- c(vars_to_remove, group_vars)
  }

  # Reorder columns to place new variables in the correct positions
  # Create mapping of first old variable to new variable
  old_to_new <- sapply(var_groups, function(group) {
    new_name <- gsub("COLLAPSE", "", names(which(sapply(var_groups, function(g) identical(g, group)))), fixed = TRUE)
    new_name <- gsub(paste0(var_name_sep, var_name_sep), var_name_sep, new_name, fixed = TRUE)
    new_name <- gsub(paste0(var_name_sep, "$"), "", new_name)
    c(group$vars[1], new_name)
  }, simplify = FALSE)

  # Convert to named vector
  replacement_map <- setNames(sapply(old_to_new, `[`, 2), sapply(old_to_new, `[`, 1))

  # Build new column order
  all_cols <- names(new_dat)
  keep_cols <- !(all_cols %in% vars_to_remove) & !(all_cols %in% new_vars_created)
  new_order <- ifelse(all_cols %in% names(replacement_map), replacement_map[all_cols],
                      ifelse(keep_cols, all_cols, NA))
  new_order <- new_order[!is.na(new_order)]

  # Reorder
  new_dat <- new_dat[, new_order]
  new_dpdict <- new_dpdict[!new_dpdict$variable_names %in% vars_to_remove, ]

  # Reorder dpdict to match the column order in new_dat
  dpdict_order <- match(names(new_dat), new_dpdict$variable_names)
  dpdict_order <- dpdict_order[!is.na(dpdict_order)]
  new_dpdict <- new_dpdict[dpdict_order, ]

  # Update dpdict metadata for new variable(s)
  variables_to_update <- new_dpdict$variable_names == new_vars_created
  new_dpdict <- update_dict_with_metadata(temp_dat = new_dat, temp_dpdict = new_dpdict, variables_to_update = variables_to_update)

  # Create new survey data object
  new_temp_dat <- create_survey_data(new_dat, new_dpdict)

  cat(sprintf("\nCollapsing complete. Reduced %d variables to %d variables.\n",
              length(vars_to_remove),
              length(new_vars_created)))

  return(new_temp_dat)
}
