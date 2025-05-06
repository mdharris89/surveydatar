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


#' Convert Data Frame Columns to Labelled Variables
#'
#' Attempts to convert columns in a data frame to labelled variables where possible.
#' Character variables with up to max_labels unique values are converted to numeric
#' with value labels. Existing labelled variables are preserved.
#'
#' @param data A data frame to process
#' @param max_labels Maximum number of unique values to consider for labelling
#' @param MRpositivelabel Label to use for positive values in multiresponse variables
#' @param MRnegativelabel Label to use for negative values in multiresponse variables
#' @param variable_labels Optional data frame with variable names and labels
#' @param value_label_lookup Optional named list of label lookup tables with names
#'                          corresponding to variable names
#'
#' @return A data frame with columns converted to labelled variables where appropriate
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
realiselabelled <- function(data, max_labels = 12,
                            MRpositivelabel = "Selected",
                            MRnegativelabel = "Not selected",
                            variable_labels = NULL,
                            value_label_lookup = NULL) {

  # Input validation
  if (!is.data.frame(data)) {
    stop("Input must be a data frame")
  }

  if (!is.numeric(max_labels) || max_labels < 1) {
    stop("max_labels must be a positive number")
  }

  # Default multiresponse labels
  MRlabels <- c(0, 1)
  names(MRlabels) <- c(MRnegativelabel, MRpositivelabel)

  # Process each column
  for (var_name in names(data)) {
    # Get existing label if present
    existing_label <- sjlabelled::get_label(data[[var_name]])

    # Skip if already labelled
    if (sjlabelled::is_labelled(data[[var_name]])) {
      next
    }

    # Skip Date variables
    if (inherits(data[[var_name]], "Date")) {
      next
    }

    # Apply value labels based on variable type
    if (is.logical(data[[var_name]])) {
      # Convert logical to numeric
      data[[var_name]] <- haven::labelled(as.numeric(data[[var_name]]))

      # Apply multiresponse labels
      if (!is.null(value_label_lookup) && var_name %in% names(value_label_lookup)) {
        data[[var_name]] <- sjlabelled::set_labels(data[[var_name]],
                                                   labels = value_label_lookup[[var_name]])
      } else {
        data[[var_name]] <- sjlabelled::set_labels(data[[var_name]], labels = MRlabels)
      }

      # Re-apply existing label
      if (!is.null(existing_label) && nchar(existing_label) > 0) {
        data[[var_name]] <- sjlabelled::set_label(data[[var_name]], label = existing_label)
      }

    } else if (is.character(data[[var_name]])) {
      # Apply custom lookup if available
      if (!is.null(value_label_lookup) && var_name %in% names(value_label_lookup)) {
        data[[var_name]] <- character_to_labelled_via_dict(
          data[[var_name]],
          new_variable_label = existing_label,
          lookup_dict = value_label_lookup[[var_name]]
        )
      } else if (length(unique(stats::na.omit(data[[var_name]]))) <= max_labels) {
        # Convert to labelled if fewer than max_labels unique values
        data[[var_name]] <- character_to_labelled_via_dict(
          data[[var_name]],
          new_variable_label = existing_label
        )
      }

    } else if (is.factor(data[[var_name]])) {
      # Get levels for label creation
      factor_levels <- levels(data[[var_name]])

      # Convert factor to labelled
      data[[var_name]] <- haven::labelled(as.numeric(data[[var_name]]))

      if (!is.null(value_label_lookup) && var_name %in% names(value_label_lookup)) {
        data[[var_name]] <- sjlabelled::set_labels(data[[var_name]],
                                                   labels = value_label_lookup[[var_name]])
      } else {
        # Create labels from factor levels
        level_labels <- seq_along(factor_levels)
        names(level_labels) <- factor_levels
        data[[var_name]] <- sjlabelled::set_labels(data[[var_name]], labels = level_labels)
      }

      # Re-apply existing label
      if (!is.null(existing_label) && nchar(existing_label) > 0) {
        data[[var_name]] <- sjlabelled::set_label(data[[var_name]], label = existing_label)
      }

    } else if (is.numeric(data[[var_name]])) {
      unique_values <- unique(stats::na.omit(data[[var_name]]))

      # Check if the variable contains only 0s and 1s (multiple response variable)
      if (length(unique_values) <= 2 && all(unique_values %in% c(0, 1))) {
        # This looks like a multiple response variable
        data[[var_name]] <- haven::labelled(data[[var_name]])

        # Apply multiresponse labels
        if (!is.null(value_label_lookup) && var_name %in% names(value_label_lookup)) {
          data[[var_name]] <- sjlabelled::set_labels(data[[var_name]],
                                                     labels = value_label_lookup[[var_name]])
        } else {
          data[[var_name]] <- sjlabelled::set_labels(data[[var_name]], labels = MRlabels)
        }

        # Re-apply existing label
        if (!is.null(existing_label) && nchar(existing_label) > 0) {
          data[[var_name]] <- sjlabelled::set_label(data[[var_name]], label = existing_label)
        }
      } else {
        # Apply custom lookup if available
        if (!is.null(value_label_lookup) && var_name %in% names(value_label_lookup)) {
          data[[var_name]] <- haven::labelled(data[[var_name]])
          data[[var_name]] <- sjlabelled::set_labels(data[[var_name]],
                                                     labels = value_label_lookup[[var_name]])

          # Re-apply existing label
          if (!is.null(existing_label) && nchar(existing_label) > 0) {
            data[[var_name]] <- sjlabelled::set_label(data[[var_name]], label = existing_label)
          }

        } else if (length(unique_values) <= max_labels) {
          # Convert to labelled if fewer than max_labels unique values
          data[[var_name]] <- haven::labelled(data[[var_name]])

          # Re-apply existing label
          if (!is.null(existing_label) && nchar(existing_label) > 0) {
            data[[var_name]] <- sjlabelled::set_label(data[[var_name]], label = existing_label)
          }
        }
      }
    }

    # Apply variable labels if provided (overrides existing labels)
    if (!is.null(variable_labels) && var_name %in% variable_labels$`variable name`) {
      label_idx <- which(variable_labels$`variable name` == var_name)
      data[[var_name]] <- sjlabelled::set_label(
        data[[var_name]],
        label = variable_labels$`question with suffix`[label_idx]
      )
    }
  }

  return(data)
}
