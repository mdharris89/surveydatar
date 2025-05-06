#' @import rlang
#' @importFrom dplyr filter select mutate
#' @importFrom tidyselect eval_select
NULL

##### FILE DESCRIPTION: various functions for creating, manipulating and viewing survey metadata #####
# includes the following functions (function arguments not shown here):
#
# functions for viewing metadata:
# datamap() - creates a dataframe for viewing with variable names, labels and other useful info as rows (SPSS style)
# datamap_questions() - like datamap, but only shows questions, not every individual variable
#
# functions for creating metadata:
# create_dict() - creates a simple 'dpdict' to enable easy bulk updates to variable names and labels
# create_dict_with_metadata() - creates a dpdict with full metadata
# update_dict_with_metadata() - adds metadata to an existing dpdict
# split_into_question_groups() - attempts to find sensible groupings of variables into question_groups
# get_longest_common_substring() - finds lcs from 2 strings. (there is also get_longest_common_substring_slow, an old, 3x slower implementation.)
# create_questions_dict() - creates a questions_dict, that is, a dataframe with metadata for each unique question in a dpdict
# get_affix_df() - gets candidate prefixes or suffixes, with information on their seps and sep locations
# get_unique_suffixes() - makes a best guess at suffixes to add to each variable's question_group to make it unique
#
# functions for updating metadata:
# update_dat_from_dpdict() - bulk update all variable names, variable labels and value labels in a dat according to a dpdict
# get_questions_dict() - gets a questions_dict from the relevant columns already in a dpdict
# update_aliases() - to update question_alias and alias_with_suffix in a dpdict after manual edits to a questions_dict
# split_grid_labels() - for expanding the question alias in a dpdict for grid questions
#
# functions for checking metadata:
# validate_dat_dpdict_alignment() - simple checks that dat and dpdict are in a suitable and aligned format
# validate_no_dpdict_duplicates() - checks for unique values in variable names and labels, and, if the column exists, alias_with_suffix, in dpdict

##### functions for viewing metadata #####

#' datamap
#'
#' creates a dataframe with variable names, labels and other useful info as rows (SPSS style)
#'
#' useful for quickly understanding structure of a large dataset
#' note: order of unique values is in order they are found, not necessarily corresponding to the order in which they have in labelling
#'
#' @param x A dataframe or survey_data object
#' @param view_or_return "view" to call View, or "return" to return the summary as a dataframe
#'
#' @return Calls View by default, or returns the summary dataframe if view_or_return is "return"
#' @export
#'
#' @examples
#' datamap(get_basic_test_dat(100), view_or_return = "return")
#' survey_obj <- create_survey_data(get_basic_test_dat(100))
#' datamap(survey_obj, view_or_return = "return")
datamap <- function(x, view_or_return = "view") {
  UseMethod("datamap")
}

#' @export
datamap.data.frame <- function(x, view_or_return = "view") {
  datamap_internal(x, view_or_return)
}

#' @export
datamap.survey_data <- function(x, view_or_return = "view") {
  result <- datamap_internal(x$dat, view_or_return)

  # Add additional metadata from dpdict
  result$`question type` <- x$dpdict$questiontype[match(result$variable, x$dpdict$variable_names)]
  result$`alias with suffix` <- x$dpdict$alias_with_suffix[match(result$variable, x$dpdict$variable_names)]

  result <- result[, c("variable", "variable label", "class", "question type", "alias with suffix", "n missing", "n unique",
                       "value labels", "first values", "first unique values")]

  if (view_or_return == "view") {
    if ("tools:rstudio" %in% search() && exists("View", envir = as.environment("tools:rstudio"))) {
      get("View", envir = as.environment("tools:rstudio"))(result, paste0("datamap ", deparse(substitute(x))))
    } else {
      if ("tools:rstudio" %in% search() && exists("View", envir = as.environment("tools:rstudio"))) {
        get("View", envir = as.environment("tools:rstudio"))(result, paste0("datamap ", deparse(substitute(x))))
      } else {
        utils::View(result, paste0("datamap ", deparse(substitute(x))))
      }
    }
  } else {
    return(result)
  }
}

datamap_internal <- function(temp_dat, view_or_return = "view") {
  match.arg(view_or_return, c("view", "return"))

  if (view_or_return == "view" && !(interactive())) {
    view_or_return <- "return"
    warning("View only possible in RStudio. Will instead return the summary")
  }

  n_head <- 5

  out <- dplyr::tibble(
    `variable` = names(temp_dat),
    `variable label` = vapply(temp_dat, function(x) {
      label <- attr(x, "label", exact = TRUE)
      ifelse(is.null(label), "", label)
    }, FUN.VALUE = character(1), USE.NAMES = FALSE),
    `class` = vapply(temp_dat, function(x) paste(class(x), collapse = ", "), FUN.VALUE = character(1), USE.NAMES = FALSE),
    `n missing` = vapply(temp_dat, function(x) sum(is.na(x)), FUN.VALUE = integer(1), USE.NAMES = FALSE),
    `n unique` = vapply(temp_dat, function(x) length(unique(x)), FUN.VALUE = integer(1), USE.NAMES = FALSE),
    `value labels` = vapply(temp_dat, function(x) paste(names(attr(x, "labels", exact = TRUE)), collapse = ", "), FUN.VALUE = character(1), USE.NAMES = FALSE),
    `first values` = vapply(temp_dat, function(x) paste(utils::head(x, n_head), collapse = ", "), FUN.VALUE = character(1), USE.NAMES = FALSE),
    `first unique values` = vapply(temp_dat, function(x) paste(utils::head(unique(x), n_head), collapse = ", "), FUN.VALUE = character(1), USE.NAMES = FALSE)
  )

  if (view_or_return == "view") {
    utils::View(out, paste0("datamap ", deparse(substitute(temp_dat))))
    return(invisible(out))
  } else {
    return(out)
  }
}


#' datamap_questions
#'
#' provides a convenient view similar to SPSS variables like datamap, but by question group instead of individual variables.
#'
#' @param survey_obj A survey_data object containing the survey data and metadata.
#'
#' @return a data frame with columns for question_group, var_count, first_var_label, first_var_name, var_classes, var_values, var_labels, and question_types
#' @export
#'
#' @examples
#' datamap_questions(create_survey_data(get_big_test_dat(10)))
datamap_questions <- function(survey_obj){

  if (!is.survey_data(survey_obj)) {
    stop("Input must be a survey_data object")
  }

  temp_dat <- survey_obj$dat
  temp_dpdict <- survey_obj$dpdict

  if(!"question_group" %in% names(temp_dpdict)){
    stop("question_group not found in dpdict")
  }

  unique_question_groups <- unique(temp_dpdict$question_group)

  out <- data.frame(question_group = unique_question_groups, stringsAsFactors = FALSE)

  out$question_types <- sapply(unique_question_groups, function(x) {
    paste(unique(temp_dpdict$questiontype[temp_dpdict$question_group == x]), collapse = " ")
  })

  out$var_count <- sapply(unique_question_groups, function(x) sum(temp_dpdict$question_group == x))

  first_indices <- match(unique_question_groups, temp_dpdict$question_group)
  out$first_var_label <- temp_dpdict$variable_labels[first_indices]
  out$first_var_name <- temp_dpdict$variable_names[first_indices]

  out$var_classes <- sapply(unique_question_groups, function(x) {
    classes <- unique(sapply(temp_dat[temp_dpdict$variable_names[temp_dpdict$question_group == x]], class))
    paste(classes, collapse = " ")
  })
  out$var_values <- lapply(unique_question_groups, function(x) {
    unique(unlist(lapply(temp_dat[temp_dpdict$variable_names[temp_dpdict$question_group == x]], unique)))
  })
  out$unique_values_count <- sapply(out$var_values, length)

  out$var_labels <- lapply(unique_question_groups, function(x) {
    unique(unlist(lapply(temp_dat[temp_dpdict$variable_names[temp_dpdict$question_group == x]], sjlabelled::get_labels)))
  })

  return(out)
}

##### functions for creating metadata #####
#' create_dict
#'
#' creates a tibble to use as a dictionary to easily update variable names and labels
#'
#' note: uses sjlabelled to get value
#'
#' @param temp_dat a dataframe, assumed to be survey data
#' @param prefill if TRUE (the default), columns variable_names/variable_labels/value_labels will be populated with values from 'old' versions. if FALSE, will leave as NA.
#'
#' @return a tibble with columns for old_variable_names, old_variable_labels, old_value_labels, and new_ versions of each
#' @export
#'
#' @examples
#' create_dict(get_minimal_labelled_test_dat())
#'
create_dict <- function(temp_dat, prefill = TRUE){

  ### validation
  stopifnot("temp_dat must be a dataframe with more than zero columns" = (is.data.frame(temp_dat) && ncol(temp_dat) > 0))

  if (!is.logical(prefill) || length(prefill) != 1) {
    stop("`prefill` must be a single logical value (TRUE/FALSE).")
  }

  temp_dpdict <- dplyr::tibble("old_variable_names" = names(temp_dat),
                      "old_variable_labels" = vapply(temp_dat, function(x) sjlabelled::get_label(x, def.value = NA_character_), FUN.VALUE = character(1), USE.NAMES = FALSE), # set def.value to return NA instead of NULL if no label found
                      "old_value_labels" = lapply(temp_dat, function(x) sjlabelled::get_labels(x, attr.only = TRUE, values = "as.name")),
                      "variable_names" = NA,
                      "variable_labels" = NA,
                      "value_labels" = NA)

  temp_dpdict$old_value_labels[vapply(temp_dpdict$old_value_labels, is.null, FUN.VALUE = logical(1))] <- NA

  if(prefill == TRUE){
    temp_dpdict$variable_names = temp_dpdict$old_variable_names
    temp_dpdict$variable_labels = temp_dpdict$old_variable_labels
    temp_dpdict$value_labels = temp_dpdict$old_value_labels
  }
  return(temp_dpdict)
}


#' check_seps
#'
#' Checks separator patterns in a survey data file
#'
#' Analyses variable names and labels to identify separator patterns and check for consistency.
#' Reports on three types of separators:
#' - Variable name separators (e.g., "_" in "Q1_1" in a varible name). Only a single unique sep is allowed across all variable names.
#' - Prefix separators (e.g., ":" in "Q1: Question text" in a variable label). A prefix is defined as any letter and, optionally, numbers, followed by punctuation, followed by whitespace. (It must be followed by whitespace.)
#' - Statement separators (e.g., " - " in "Question text - Statement" in a variable label). Any prefixes are removed before checking for statement separators. A statement separate is any punctuation surrounded by whitespace.
#'
#' @param temp_dat a survey data frame
#' @param var_name_seps_to_check a vector of strings of potential var name separators to check for. If NULL, checks for any punctuation `[[:punct:]]`
#' @param prefixes_to_check a vector of strings of potential prefix separators to check for. If NULL, checks for `[[:punct:]][[:space:]]`
#' @param statement_seps_to_check a vector of strings of potential statement separators to check for. If NULL, checks for `[[:space:]][[:punct:]][[:space:]]`
#' @param verbose logical. if true, print issues found.
#'
#'
#' @return A list with components:
#'   \item{separators}{Named character vector of detected separators}
#'   \item{consistency}{Named logical vector indicating if each separator type is consistent}
#'   \item{issues}{Character vector of specific inconsistencies found}
#'   \item{examples}{List of example variable names/labels for each pattern}
#'
#' @export
#'
#' @examples
#' temp_dat <- get_big_test_dat_with_prefixes()
#' check_seps(temp_dat)
check_seps <- function(temp_dat,
                             var_name_seps_to_check = NULL,
                             prefixes_to_check = NULL,
                             statement_seps_to_check = NULL,
                       verbose = FALSE) {

  ### vaidation
  if (!is.data.frame(temp_dat)) {
    stop("Input must be a data frame")
  }
  if (!is.null(var_name_seps_to_check) && !is.character(var_name_seps_to_check)) {
    stop("`var_name_seps_to_check` must be NULL or a character vector.")
  }
  if (!is.null(prefixes_to_check) && !is.character(prefixes_to_check)) {
    stop("`prefixes_to_check` must be NULL or a character vector.")
  }
  if (!is.null(statement_seps_to_check) && !is.character(statement_seps_to_check)) {
    stop("`statement_seps_to_check` must be NULL or a character vector.")
  }
  if (!is.logical(verbose) || length(verbose) != 1) {
    stop("`verbose` must be a single logical value (TRUE/FALSE).")
  }

  # initialize results
  results <- list(
    separators = character(),
    consistency = logical(),
    counts = list(),
    issues = character(),
    examples = list()
  )

  # get variable names and labels
  var_names <- names(temp_dat)
  var_labels <- sapply(temp_dat, function(x) attr(x, "label", exact = TRUE))

  # helper function that looks for specified seps at the end of a given regex pattern
  check_seps_from_regex <- function(vector_of_strings, regex_pattern = "^[A-Za-z]+[-_.,0-9A-Za-z]*[A-Za-z0-9]", seps_to_check = NULL) {

    sep_regex <- paste0(regex_pattern, seps_to_check)

    # find which strings have recognisable prefixes
    has_sep <- grepl(sep_regex, vector_of_strings)

    # if none, return early
    if(!any(has_sep)) {
      return(list(
        separators = character(0),
        consistent = TRUE,
        count = NA,
        example = NA_character_))
    }

    # find sep for each string
    match_starts <- regexpr(sep_regex, vector_of_strings) # start position of sep_regex match
    match_ends <- match_starts + attr(match_starts, "match.length") - 1 # end position of sep_regex match
    sep_portions <- mapply(substr, vector_of_strings, match_starts, match_ends)
    # For each string, identify which separator from seps_to_check is present
    ending_seps <- sapply(sep_portions, function(portion) {
      # Find the separator pattern within the portion
      sep_match <- regexpr(seps_to_check, portion)
      if(sep_match > 0) {
        substr(portion,
               sep_match,
               sep_match + attr(sep_match, "match.length") - 1)
      } else {
        NA_character_
      }
    })

    unique_ending_seps <- unique(stats::na.omit(ending_seps))

    # check consistency - should only have one type of sep
    consistent <- length(unique_ending_seps) == 1

    # count occurances of each sep
    sep_count <- stats::setNames(as.vector(table(ending_seps)), names(table(ending_seps)))

    # find examples showing each separator
    examples <- character(length(unique_ending_seps))
    names(examples) <- unique_ending_seps
    for(sep in unique_ending_seps) {
      matches <- which(grepl(sep, vector_of_strings, fixed = TRUE))
      if(length(matches) > 0) {
        examples[sep] <- vector_of_strings[matches[1]]
      } else {
        examples[sep] <- NA_character_
      }
    }

    list(
      separators = unique_ending_seps,
      consistent = consistent,
      count = sep_count,
      example = if(length(unique_ending_seps) > 0) examples else NA_character_
    )
  }

  # get var name patterns
  if(is.null(var_name_seps_to_check)){
    var_name_seps_to_check <- "[[:punct:]]"
  } else {
    var_name_seps_to_check <- puncts_to_pattern(var_name_seps_to_check)
  }
  var_name_patterns <- check_seps_from_regex(var_names, regex_pattern = "^[A-Za-z]+[0-9A-Za-z]*", seps_to_check = var_name_seps_to_check)

  # then get prefix patterns. we assume that whitespace follows any label prefix.
  if(is.null(prefixes_to_check)){
    prefixes_to_check <- "[[:punct:]][[:space:]]"
  } else {
    prefixes_to_check <- puncts_to_pattern(prefixes_to_check)
  }
  prefix_patterns <- check_seps_from_regex(var_labels, regex_pattern = "^[A-Za-z]+[-_.,0-9A-Za-z]*[A-Za-z0-9]", seps_to_check = prefixes_to_check)

  # then look for statement separators. we remove any prefixes first.
  labels_without_prefixes <- var_labels
  if(length(prefix_patterns$separators) > 0) {
    # for each prefix separator found, remove everything up to and including it
    for(sep in prefix_patterns$separators) {
      labels_without_prefixes <- sapply(labels_without_prefixes, function(x) {
        if(grepl(sep, x, fixed = TRUE)) {
          parts <- strsplit(x, sep, fixed = TRUE)[[1]]
          trimws(paste(parts[-1], collapse = sep), which = "left")
        } else {
          x
        }
      })
    }
  }
  if(is.null(statement_seps_to_check)){
    statement_seps_to_check <- "[[:space:]][[:punct:]][[:space:]]"
  } else {
    statement_seps_to_check <- puncts_to_pattern(statement_seps_to_check)
  }
  statement_patterns <- check_seps_from_regex(labels_without_prefixes, regex_pattern = "^.*", seps_to_check = statement_seps_to_check)

  # Compile results
  results$separators <- c(
    var_name_sep = if(length(var_name_patterns$separators) > 0) var_name_patterns$separators[1] else NA_character_,
    prefix_sep = if(length(prefix_patterns$separators) > 0) prefix_patterns$separators[1] else NA_character_,
    statement_sep = if(length(statement_patterns$separators) > 0) statement_patterns$separators[1] else NA_character_
  )

  results$consistency <- c(
    var_name_sep = length(var_name_patterns$separators) <= 1 && var_name_patterns$consistent,
    prefix_sep = length(prefix_patterns$separators) <= 1 && all(prefix_patterns$consistent),
    statement_sep = length(statement_patterns$separators) <= 1 && all(statement_patterns$consistent)
  )

  results$counts <- list(
    var_name_sep = var_name_patterns$count,
    prefix_sep = prefix_patterns$count,
    statement_sep = statement_patterns$count
  )

  results$examples <- list(
    var_name_sep = var_name_patterns$example,
    prefix_sep = prefix_patterns$example,
    statement_sep = statement_patterns$example
  )

  # Add specific issues
  # update these including inconsistent_vars which no longer exists
  if (!results$consistency["var_name_sep"]) {
    inconsistent_vars <- var_names[grepl(paste(var_name_patterns$separators, collapse = "|"), var_names)]
    results$issues <- c(results$issues,
                        sprintf("Inconsistent variable name separators. Examples: %s",
                                paste(utils::head(inconsistent_vars, 3), collapse = ", ")))
  }

  if (!results$consistency["prefix_sep"]) {
    results$issues <- c(results$issues,
                        "Inconsistent prefix separators in variable labels")
  }

  if (!results$consistency["statement_sep"]) {
    results$issues <- c(results$issues,
                        "Inconsistent statement separators in variable labels")
  }

  # Print verbose output if requested
  if (verbose && length(results$issues) > 0) {
    message("Separator detection found the following issues:")
    for (issue in results$issues) {
      message("- ", issue)
    }
  }

  return(results)
}


#' get_updated_seps
#'
#' updates separator patterns in a survey data file
#'
#' by default will update all seps to the most common seps
#'
#' @param temp_dat a survey data frame
#' @param sep_analysis the result of check_seps(temp_dat)
#' @param seps_to_use NULL by default, else provide a list with elements for 'var name', 'prefix', and 'statement' specifying the sep to use instead of the most common sep
#'
#' @return a data frame summarising old and new (updated) variable names and variable labels
#' @export
#'
#' @examples
#' temp_dat <- get_big_test_dat_with_prefixes()
#' get_updated_seps(temp_dat, check_seps(temp_dat))
get_updated_seps <- function(temp_dat, sep_analysis, seps_to_use = NULL) {
  if (!is.data.frame(temp_dat)) {
    stop("temp_dat must be a data frame")
  }
  if (!is.list(sep_analysis) || !all(c("separators", "counts") %in% names(sep_analysis))) {
    stop("sep_analysis must be a list containing 'separators' and 'counts' elements")
  }

  if((!all(c("var_name_sep", "prefix_sep", "statement_sep") %in% names(seps_to_use))) && !is.null(seps_to_use)){
    stop("seps_to_use must be a list containing elements for 'var_name_sep', 'prefix_sep', and 'statement_sep', or null")
  }

  # If no seps to use given, uses most common separators by default

  if(is.null(seps_to_use)){
    get_most_common <- function(counts) {
      if (length(counts) == 0 || all(is.na(counts))) return(NA_character_)
      names(which.max(counts))
    }

    seps_to_use <- list(
      var_name_sep = get_most_common(sep_analysis$counts$var_name_sep),
      prefix_sep = get_most_common(sep_analysis$counts$prefix_sep),
      statement_sep = get_most_common(sep_analysis$counts$statement_sep)
    )
  }

  # Initialize results dataframe
  result <- data.frame(
    old_variable_names = names(temp_dat),
    var_name_seps_found = NA_character_,
    old_variable_labels = vapply(temp_dat, function(x) {
      label <- attr(x, "label", exact = TRUE)
      if (is.null(label)) NA_character_ else label
    }, character(1)),
    prefix_seps_found = NA_character_,
    statement_seps_found = NA_character_,
    new_variable_names = names(temp_dat),
    new_variable_labels = vapply(temp_dat, function(x) {
      label <- attr(x, "label", exact = TRUE)
      if (is.null(label)) NA_character_ else label
    }, character(1)),
    stringsAsFactors = FALSE
  )

  # helper function to update seps using result from check_seps
  update_sep_from_regex <- function(x, seps_to_check, new_sep, regex_pattern = "^([A-Za-z]+[0-9A-Za-z]*)"){

    if(is.null(seps_to_check)){
      return(list(found_sep = NA_character_,
                  new_string = x))
    }

    seps_to_check <- puncts_to_pattern(seps_to_check)
    sep_regex <- paste0(regex_pattern, seps_to_check)

    new_string <- x

    if (grepl(sep_regex, x)) {
      # find current sep position
      match_starts <- regexpr(sep_regex, x) # start position of sep_regex match
      match_ends <- match_starts + attr(match_starts, "match.length") - 1 # end position of sep_regex match
      sep_portion <- substr(x, match_starts, match_ends)
      matched_sep <- regexpr(seps_to_check, sep_portion)
      # swap in new sep
      if(matched_sep > 0) {
        found_sep <- substr(sep_portion,
                            matched_sep,
                            matched_sep + attr(matched_sep, "match.length") - 1)
        substr(new_string,
               matched_sep,
               matched_sep + attr(matched_sep, "match.length") - 1) <- new_sep
      }
    } else {
      found_sep <- NA_character_
    }
    return(list(found_sep = found_sep,
                new_string = new_string))
  }

  # var name seps
  var_name_seps_result <- mapply(
    function(x) update_sep_from_regex(x, names(sep_analysis$counts$var_name_sep), seps_to_use[["var_name_sep"]], regex_pattern = "^([A-Za-z]+[0-9A-Za-z]*)"),
    result$old_variable_names,
    SIMPLIFY = FALSE
  )

  result$var_name_seps_found <- sapply(var_name_seps_result, `[[`, "found_sep")
  result$new_variable_names <- sapply(var_name_seps_result, `[[`, "new_string")

  # prefix seps
  prefix_seps_result <- mapply(
    function(x) update_sep_from_regex(x, names(sep_analysis$counts$prefix_sep), seps_to_use[["prefix_sep"]], regex_pattern = "^[A-Za-z]+[-_.,0-9A-Za-z]*[A-Za-z0-9]"),
    result$old_variable_labels,
    SIMPLIFY = FALSE
  )

  result$prefix_seps_found <- sapply(prefix_seps_result, `[[`, "found_sep")
  result$new_variable_labels <- sapply(prefix_seps_result, `[[`, "new_string")

  # statement seps
  if (!is.na(seps_to_use[["statement_sep"]])) {
    pattern <- paste0(puncts_to_pattern(names(sep_analysis$counts$statement_sep)), collapse = "|")

    result$statement_seps_found <- sapply(result$new_variable_labels, function(label) {
      matched_sep <- regexpr(pattern, label)
      substr(label, matched_sep, matched_sep + attr(matched_sep, "match.length")-1)
    })

    if (!is.na(seps_to_use[["prefix_sep"]]) && any(grepl(seps_to_use[["prefix_sep"]], result$new_variable_labels, fixed = TRUE))) {
      result$new_variable_labels <- sapply(result$new_variable_labels, function(label) {
        parts <- strsplit(label, seps_to_use[["prefix_sep"]], fixed = TRUE)[[1]]
        remainder <- paste(parts[-1], collapse = seps_to_use[["prefix_sep"]])
        remainder <- gsub(pattern, seps_to_use[["statement_sep"]], remainder)
        paste0(parts[1], seps_to_use[["prefix_sep"]], remainder)
      })
    } else {
      result$new_variable_labels <- gsub(pattern, seps_to_use[["statement_sep"]], result$new_variable_labels)
    }
  }

  return(result)
}

#' create_dict_with_metadata
#'
#' a wrapper around update_dict_with_metadata to create a dpdict from dat with metadata.
#'
#' calls create_dict() first then adds metadata, so you can create a dpdict with metadata from a dat with a single function
#'
#' @param temp_dat a survey data frame ('dat')
#' @param noisy integer between 1 and 4, relating to how verbose to print.
#'
#' @return a dpdict with metadata
#' @export
#'
#' @examples create_dict_with_metadata(get_big_test_dat(n=10))
create_dict_with_metadata <- function(temp_dat, noisy = 0){
  # if no temp_dpdict provided, create one and initialize metadata columns
  temp_dpdict <- create_dict(temp_dat, prefill = TRUE)
  temp_dpdict <- update_dict_with_metadata(survey_obj = NULL, temp_dat, temp_dpdict, noisy = noisy)
  return(temp_dpdict)
}


#' update_dict_with_metadata
#'
#' Given a dpdict, updates additional metadata fields for a requested set of variables given by variables_to_update, or if NULL, all variables.
#'
#' @param survey_obj a survey_data object, or NULL, in which case must provide temp_dat and temp_dpdict
#' @param temp_dat survey data dataframe
#' @param temp_dpdict a dpdict to update
#' @param variables_to_update optional logical vector to specify that only certain variables in the existing temp_dpdict should be updated
#' @param seps_to_use optional specifying separators to use for processing:
#'        - variable_name_sep: String that separates each variable within a common question, e.g. "_" for Q1_1, Q1_2.
#'        - prefix_sep: String. If ignorelabelbeforeprefix == TRUE, substring before prefix_sep will be removed before working with variable labels.
#'        - statement_sep: String. For findlongest == FALSE, uses substring prior to statement_sep as commonlabel.
#' @param ignorelabelbeforeprefix Logical. If TRUE, removes substring before prefix_sep before working with variable labels.
#' @param split_into_groups_config options for split_into_question_groups
#' @param edit_aliases logical. whether to allow user to manually edit question_aliases using data_edit before creating alias_with_suffix. (question_folder can be defined at the same time)
#' @param noisy integer between 1 and 4. 1 by default. if 1, signals start of attempts to find question groups, and multiresponse. if noisy == 2 provides timing and updates within question groups and multiresponse hunts. noisy > 2 has settings within function for finding question groups.
#'
#' @details
#' metadata included:
#' \itemize{
#'  \item variable-level metadata: question_group, variable class, and then checks for single variable question, dichotomous variable, variable with value labels, and multiresponse variable, which are in turn used to define questiontype
#'  \item question-level metadata: question_alias, question_description, question_suffix, alias_with_suffix, and question_folder
#'  \item question_alias is intended as a user-defined identifier for the question but takes the values of question_group by default
#'  \item question_description, question_suffix, and alias_with_suffix are useful when creating tables and visualisations
#' }
#'
#' The function performs several steps:
#' \itemize{
#'   \item Standardizes separators in variable names and labels using \code{\link{get_updated_seps}}.
#'   \item Determines question groups using \code{\link{split_into_question_groups}}.
#'   \item Infers variable metadata (class, single/dichotomous, value labels).
#'   \item Attempts to identify multiresponse variable sets (may take time on large datasets). Note: identification relies on values != 0 or NA within a question group.
#'   \item Assigns a `questiontype` based on the inferred metadata (e.g., 'numeric', 'categorical', 'multiresponse', 'text'). Warnings are issued for undefined types.
#'   \item Creates question-level metadata (alias, description, folder) using \code{\link{create_questions_dict}}.
#'   \item Generates unique suffixes for variables within question groups using \code{\link{get_unique_suffixes}} and creates an `alias_with_suffix`.
#' }
#'
#' other notes:
#' \itemize{
#' \item identifies categorical and multiresponse ('select all') variables by the presence of value labels. i.e. if a variable has class labelled but not value labels, it will not be given questiontype multiresponse, categorical or categorical array. (usually it will be a numeric or multinumeric)
#' \item findlongest=TRUE, with splitbycommonlabel=TRUE takes significantly longer (as it attempts to split by analysing strings). may not be necessary for some processing - if correct common label is always the string prior to label_sep.
#' \item variable labels should be hierarchically structured, with unique statements at the end. e.g. "Q1. Question text - Attitude statement - answer code"
#' \item checking for multiresponse variables can also take a while but is required.
#' \item if last variable in a question in returned dict just keeps its full string, min_common_strings may be set too high
#' }
#'
#'
#' @return If `survey_obj` was provided, returns the updated `survey_data` object.
#'         Otherwise, returns the updated `dpdict` data frame
#' @export
#' @examples
#' # For survey_data object
#' survey_obj <- create_survey_data(get_big_test_dat(n=10))
#' survey_obj <- update_dict_with_metadata(survey_obj)
#'
#' # For separate temp_dat and temp_dpdict
#' temp_dat <- get_big_test_dat(n=10)
#' temp_dpdict <- create_dict(temp_dat, prefill = TRUE)
#' temp_dpdict <- update_dict_with_metadata(NULL, temp_dat, temp_dpdict)
update_dict_with_metadata <- function(survey_obj = NULL, temp_dat = NULL, temp_dpdict = NULL,
                                      variables_to_update = NULL,
                                      seps_to_use = NULL, ignorelabelbeforeprefix = TRUE,
                                      split_into_groups_config = NULL,
                                      edit_aliases = FALSE,
                                      noisy = 0){

  ## validation and unpacking arguments
  if (!is.null(survey_obj)) {
    if (!is.survey_data(survey_obj)) {
      stop("survey_obj must be a survey_data object")
    }
    temp_dat <- survey_obj$dat
    temp_dpdict <- survey_obj$dpdict
  } else {
    if (is.null(temp_dat) || is.null(temp_dpdict)) {
      stop("If survey_obj is NULL, both temp_dat and temp_dpdict must be provided")
    }
    if (!is.data.frame(temp_dat) || !is.data.frame(temp_dpdict)) {
      stop("temp_dat and temp_dpdict must be data frames")
    }
  }

  required_cols <- c("variable_names", "variable_labels")
  if (!all(required_cols %in% names(temp_dpdict))) {
    stop("temp_dpdict must contain columns: ", paste(required_cols, collapse = ", "))
  }

  if (!all(temp_dpdict$variable_names %in% names(temp_dat))) {
    stop("Some variable_names in temp_dpdict are not found in temp_dat")
  }
  if (!all(names(temp_dat) %in% temp_dpdict$variable_names)) {
    stop("Some columns in temp_dat are not found in temp_dpdict$variable_names")
  }

  if (!is.null(variables_to_update)) {
    if (!is.logical(variables_to_update) || length(variables_to_update) != nrow(temp_dpdict)) {
      stop("if provided, variables_to_update must be a logical vector of length nrow(temp_dpdict)")
    }
  } else {
    variables_to_update <- rep(TRUE, nrow(temp_dpdict))
  }

  if (!is.null(seps_to_use)) {
    if (!is.list(seps_to_use) || !all(c("var_name_sep", "prefix_sep", "statement_sep") %in% names(seps_to_use))) {
      stop("if provided, seps_to_use must be a list with elements 'var_name_sep', 'prefix_sep', and 'statement_sep'")
    }
  }

  if (!noisy %in% c(0, 1, 2, 3, 4)){
    stop("noisy must be a number between 0 and 4", call)
  }

  if(!is.null(split_into_groups_config)){
    bool_config_params <- c("splitbyclass", "splitbynumlabelledvalues",
                            "splitbynoncontiguous", "splitbycommonlabel",
                            "findlongest")
    for (param in bool_config_params) {
      if (!is.logical(split_into_groups_config[[param]]) || length(split_into_groups_config[[param]]) != 1) {
        stop(sprintf("%s must be a single logical value", param), call. = FALSE)
      }
    }

    if (!split_into_groups_config$variable_compare_mode %in% c("complete", "reduced")) {
      stop("variable_compare_mode must be either 'complete' or 'reduced'", call. = FALSE)
    }

    # Validate min_common_strings
    if (!is.numeric(split_into_groups_config$min_common_strings) || length(split_into_groups_config$min_common_strings) != 1 ||
        split_into_groups_config$min_common_strings < 1) {
      stop("min_common_strings must be a positive integer", call. = FALSE)
    }
  }

  if (!is.logical(edit_aliases) || length(edit_aliases) != 1){
    stop("edit_aliases must be a single logical value", call. = FALSE)
  }
  ## end of validation

  # if no variables_to_update provided, we'll update all
  if (is.null(variables_to_update)){
    variables_to_update <- rep(TRUE, ncol(temp_dat))
  }

  # check seps
  check_seps_result <- check_seps(temp_dat)
  current_seps <- as.list(check_seps_result$separators)

  # if seps_to_use not provided, use current seps
  if(is.null(seps_to_use)) {
    seps_to_use <- current_seps
  }

  # standardise seps in variable names and labels
  standardised_seps <- get_updated_seps(temp_dat, check_seps_result, seps_to_use)
  temp_dpdict$variable_names[variables_to_update] <- standardised_seps$new_variable_names[variables_to_update]
  temp_dpdict$variable_labels[variables_to_update] <- standardised_seps$new_variable_labels[variables_to_update]

  attr(temp_dpdict, "sep_patterns") <- seps_to_use

  # initialize any missing columns
  character_cols_to_initialize <- c("question_group", "question_lcs", "variable_class", "questiontype", "question_suffix", "question_alias", "question_description", "alias_with_suffix", "question_folder")
  for(i in character_cols_to_initialize[!character_cols_to_initialize %in% names(temp_dpdict)]){
    temp_dpdict[[i]] <- NA_character_
  }
  logical_cols_to_initialize <- c("singlevariablequestion", "dichotomousvariable", "has_value_labels", "multiresponse")
  for(i in logical_cols_to_initialize[!logical_cols_to_initialize %in% names(temp_dpdict)]){
    temp_dpdict[[i]] <- NA
  }

  # split into question groups. there are various settings for this, particularly for if attempting to split by commonlabel
  if(noisy >= 1){
    print("Attempting to split into question groups...")
    if(noisy >= 2){start_time <- proc.time()}
  }

  temp_dpdict <- split_into_question_groups(temp_dpdict, temp_dat, variables_to_process = variables_to_update,
                                            seps_to_use, ignorelabelbeforeprefix = TRUE,
                                            config = split_into_groups_config,
                                            noisy = noisy)
  if(noisy >= 2){print(proc.time() - start_time)}

  # define variable metadata
  temp_dpdict$variable_class[variables_to_update] <- vapply(temp_dpdict$variable_names[variables_to_update], function(x) paste(class(temp_dat[[x]]), collapse = ", "), character(1))
  temp_dpdict$singlevariablequestion[variables_to_update] <- vapply(temp_dpdict$question_group[variables_to_update], function(x) sum(temp_dpdict$question_group == x) == 1, logical(1))
  # counts value *labels*, rather than values, to determine whether a dichotomous variable, so e.g. if there are two value labels, for 'Selected' and for 'Not selected', even if no values correspond to 'Not selected' in reality, it will still be considered dichotomous
  temp_dpdict$dichotomousvariable[variables_to_update] <- vapply(temp_dpdict$variable_names[variables_to_update], function(x) length(sjlabelled::get_labels(temp_dat[[x]])) == 2, logical(1))
  temp_dpdict$has_value_labels[variables_to_update] <- vapply(temp_dpdict$value_labels[variables_to_update], function(x) !all(is.na(x)), logical(1))

  # identifying multiresponse: for each variable in temp_dpdict, pass the question group, then check within all variables with the same question group in the dat. if more than 1 variable in the question group that has at least one value that is not 0 or NA, then it's a multiresponse.
  if(noisy >= 1){
    print("Checking for multiresponse variables")
    if(noisy >=2){start_time <- proc.time()}
  }

  for(i in seq_len(nrow(temp_dpdict[variables_to_update,]))){
    if(noisy >= 2){print(paste0("Checking ", temp_dpdict$variable_names[variables_to_update][i], " for multiresponse"))}

    set_of_variable_names_in_question_group <- temp_dpdict$variable_names[temp_dpdict$question_group == temp_dpdict$question_group[i] & variables_to_update]
    question_group_within_dat <- temp_dat[names(temp_dat[,variables_to_update]) %in% set_of_variable_names_in_question_group]

    count_within_question_group <- vapply(question_group_within_dat, function(x) any(!is.na(x) & x != 0), logical(1)) # within each variable in question group, check that there is at least one value that is not NA or 0

    if(sum(count_within_question_group) > 1){
      temp_dpdict$multiresponse[temp_dpdict$variable_names %in% set_of_variable_names_in_question_group & variables_to_update] <- TRUE
      i <- i + length(set_of_variable_names_in_question_group)
    } else {
      temp_dpdict$multiresponse[variables_to_update][i] <- FALSE
      i <- i + 1
    }
  }

  if(noisy >=2){print(proc.time() - start_time)}

  # define question_type based on metadata
  temp_dpdict$questiontype[variables_to_update] <- with(temp_dpdict[variables_to_update,], dplyr::case_when(
    ((grepl("numeric", variable_class) | grepl("integer", variable_class) | grepl("double", variable_class)) & (singlevariablequestion == TRUE)) ~ "numeric",
    ((grepl("numeric", variable_class) | grepl("integer", variable_class) | grepl("double", variable_class)) & (multiresponse == TRUE)) ~ "multinumeric",
    (grepl("POSIXct|POSIXt|Date", variable_class)) ~ "date",
    variable_class == "difftime" ~ "difftime",
    variable_class == "character" ~ "text",
    (grepl("factor", variable_class) & (multiresponse == FALSE)) ~ "categorical",
    (has_value_labels == TRUE & (multiresponse == FALSE)) ~ "categorical",
    grepl("logical", variable_class) ~ "categorical",
    (grepl("factor", variable_class) & (dichotomousvariable == TRUE) & (multiresponse == TRUE)) ~ "multiresponse",
    (has_value_labels == TRUE & (dichotomousvariable == TRUE) & (multiresponse == TRUE)) ~ "multiresponse",
    (grepl("factor", variable_class) & (dichotomousvariable == FALSE) & (multiresponse == TRUE)) ~ "categorical array",
    (has_value_labels == TRUE & (dichotomousvariable == FALSE) & (multiresponse == TRUE)) ~ "categorical array",
    .default = NA
  ))

  # warning messages for undefined question types
  if(any(is.na(temp_dpdict$questiontype))){
    # Get the undefined variables
    undefined_vars <- temp_dpdict$variable_names[is.na(temp_dpdict$questiontype)]
    undefined_groups <- unique(temp_dpdict$question_group[temp_dpdict$variable_names %in% undefined_vars])

    potential_mr_groups <- character(0)
    potential_mr_vars <- character(0)

    for(qgroup in undefined_groups) {
      group_vars <- stats::na.omit(temp_dpdict$variable_names[temp_dpdict$question_group == qgroup])
      # Check if this looks like a multiresponse group (multiple variables in same group)
      if(length(group_vars) > 1) {
        # Check if all variables in this group only have 0s or NAs
        if(all(vapply(temp_dat[group_vars], function(x) all(is.na(x) | x == 0), logical(1)))) {
          potential_mr_groups <- c(potential_mr_groups, qgroup)
          potential_mr_vars <- c(potential_mr_vars, group_vars)
        }
      }
    }

    if(length(potential_mr_vars) > 0) {
      warning("The following variables appear to be part of multiresponse question groups but contain only 0s or NAs and so are currently questiontype undefined: ",
              paste(potential_mr_vars, collapse = ", "),
              "\nCheck if these are multiresponse questions.")

      remaining_undefined <- setdiff(undefined_vars, potential_mr_vars)
      if(length(remaining_undefined) > 0) {
        warning("Additional variables without defined question types: ",
                paste(remaining_undefined, collapse = ", "))
      }
    } else {
      warning("The following variables do not have a defined question type: ",
              paste(undefined_vars, collapse = ", "))
    }
  }

  # define question metadata
  questions_dict <- create_questions_dict(NULL, temp_dpdict, editfirst = FALSE)

  # add question metadata to relevant variables
  temp_dpdict$question_alias[variables_to_update] <- questions_dict$question_alias[match(temp_dpdict$question_group[variables_to_update], questions_dict$question_group)]
  temp_dpdict$question_description[variables_to_update] <- questions_dict$question_description[match(temp_dpdict$question_group[variables_to_update], questions_dict$question_group)]
  temp_dpdict$question_folder[variables_to_update] <- questions_dict$question_folder[match(temp_dpdict$question_group[variables_to_update], questions_dict$question_group)]

  # variable metadata that is dependent on question metadata
  question_suffixes <- get_unique_suffixes(temp_dpdict[variables_to_update,], var_with_unique_id = "variable_names",
                                           var_with_strings = "variable_labels",
                                           var_with_question_groups = "question_group", seps_priority = c("- "))

  temp_dpdict$question_suffix[variables_to_update] <- question_suffixes[variables_to_update]
  temp_dpdict$alias_with_suffix[variables_to_update] <- ifelse(is.na(temp_dpdict$question_suffix[variables_to_update]),
                                          temp_dpdict$question_alias[variables_to_update],
                                          paste0(temp_dpdict$question_alias[variables_to_update], " - ", temp_dpdict$question_suffix[variables_to_update]))

  # move question_folder to the end
  temp_dpdict <- temp_dpdict[, c(setdiff(names(temp_dpdict), "question_folder"), "question_folder")]

  if (is.survey_data(survey_obj)) {
    survey_obj$dpdict <- temp_dpdict
    return(survey_obj)
  } else {
    return(temp_dpdict)
  }
}


#' split_into_question_groups
#'
#' attempts to find a $question_group within a temp_dpdict with a unique suffix that groups variables into sensible questions
#'
#' key function for defining survey metadata (e.g. as part of create_dict_with_metadata)
#' will work within existing question groups if $question_group already exists, else will create starting $question_group from prefix to variable_name_sep
#' will also provide a column question_lcs which is the longest common string found within a question group (this is used to define question groups when splitbycommonlabel == TRUE)
#'
#' note: if last variable in a question in returned dict just keeps its full string, min_common_strings may be set too high
#'
#' @param temp_dpdict A dpdict data frame. Must contain 'variable_names' and
#'        'variable_labels'. A 'question_group' column will be created if it
#'        doesn't exist (based on variable names before `variable_name_sep`)
#' @param temp_dat The corresponding survey data dataframe.
#' @param variables_to_process Optional logical vector (length = nrow(temp_dpdict))
#'        indicating which variables to process. If NULL (default), all are processed.
#'        Note: If any variable in a group is selected, all variables in that
#'        original group will be processed
#' @param seps_to_use List specifying separators used for parsing names/labels.
#'        Defaults are used if not provided. See \code{\link{check_seps}}. Key elements:
#'        'variable_name_sep', 'prefix_sep', 'statement_sep'.
#' @param ignorelabelbeforeprefix Logical. If TRUE, removes substring before prefix_sep before working with variable labels.
#' @param config Optional list with configuration settings. Can include:
#'        - splitbyclass: Logical. If TRUE, every successive new class is given a new unique suffix.
#'        - splitbynumlabelledvalues: Logical. If TRUE, each variable with a different number of labelled values gets a new suffix.
#'        - splitbynoncontiguous: Logical. If TRUE, variables with the same variable name prefix not located adjacent are given a new suffix.
#'        - splitbycommonlabel: Logical. If TRUE, each variable with a different longest common string gets a new suffix.
#'        - findlongest: Logical. If TRUE, finds longest common substring (computationally expensive).
#'        - min_lcs_length: Integer. Any potential lcs shorter than this is disqualified.
#'        - min_common_strings: Integer. For findlongest == TRUE, looks for substrings common to at least this many variables.
#'        - consistent_consecutive_mode: Logical. If TRUE, look for strings common with consecutive variables.
#'        - consecutive_range: Integer. For variable_compare_mode == "reduced", only looks at variables this many before and after.
#'        - variable_compare_mode: String. Either "complete" or "reduced" - affects how variables are compared.
#' @param noisy Integer 0-4 controlling verbosity level.
#' @details
#' Iterates through initial question groups and applies splitting rules
#' based on the `config` settings. A new group (with a new suffix like "_b")
#' is started whenever a condition is met for consecutive variables within the
#' original group.
#'
#' Default `config` settings:
#' \itemize{
#'   \item `splitbyclass = TRUE`: Split if variable class changes.
#'   \item `splitbynumlabelledvalues = TRUE`: Split if the number of defined value labels changes.
#'   \item `splitbynoncontiguous = TRUE`: Split if variables with the same original prefix are not adjacent in the `dpdict`.
#'   \item `splitbycommonlabel = TRUE`: Split if the common part of the variable label changes. How the "common part" is determined depends on `findlongest`.
#'   \item `findlongest = FALSE`: If FALSE (default) and `splitbycommonlabel=TRUE`, the common label is assumed to be the text before `statement_sep`. If TRUE, the function actively searches for the Longest Common Substring (LCS) between labels, which is much slower.
#'   \item `min_lcs_length = 10`: Minimum length for a string to be considered a potential LCS when `findlongest=TRUE`.
#'   \item `min_common_strings = 5`: When `findlongest=TRUE`, requires an LCS candidate to be common to at least this many variables in the group to be prioritized.
#'   \item `consistent_consecutive_mode = FALSE`: Alternative LCS finding logic (experimental/unused by default).
#'   \item `consecutive_range = 10`: Range for comparing variables in 'reduced' mode.
#'   \item `variable_compare_mode = "reduced"`: How variables are compared when `findlongest=TRUE` ("complete" or "reduced").
#' }
#' Finding the LCS (`findlongest = TRUE`) uses \code{\link{get_longest_common_substring}}.
#' Note: If the last variable in a group retains its full label as `question_lcs`, `min_common_strings` might be set too high.
#'
#' @return The input `temp_dpdict` data frame with updated 'question_group'
#'         and 'question_lcs' columns
#' @export
#'
#' @examples
#' temp_dat <- get_big_test_dat(n=10)
#' temp_dpdict <- create_dict(temp_dat, prefill = TRUE)
#' temp_dpdict <- temp_dpdict[, !(names(temp_dpdict) %in% c("old_variable_names",
#'                                                    "old_variable_labels",
#'                                                    "old_value_labels"))]
#' # basic usage
#' temp_dpdict <- split_into_question_groups(temp_dpdict, temp_dat,
#'                                          seps_to_use = list(variable_name_sep = "_",
#'                                          prefix_sep = ": ", statement_sep = " - "))
#'
#' # usage with custom configuration
#' config <- list(
#'   splitbyclass = TRUE,
#'   splitbycommonlabel = TRUE
#' )
#' temp_dpdict <- split_into_question_groups(temp_dpdict, temp_dat,
#'                                          seps_to_use = list(variable_name_sep = "_",
#'                                          prefix_sep = ": ", statement_sep = " - "),
#'                                          config = config)
split_into_question_groups <- function(temp_dpdict, temp_dat, variables_to_process = NULL,
                                       seps_to_use = list(), ignorelabelbeforeprefix = TRUE, config = list(), noisy = 0){


  # Default seps_to_use
  default_seps_to_use <- list(
    variable_name_sep = "_",
    prefix_sep = ": ",
    statement_sep = " - "
  )

  seps_to_use <- utils::modifyList(default_seps_to_use, seps_to_use)

  # Default config
  default_config <- list(
    splitbyclass = TRUE,
    splitbynumlabelledvalues = TRUE,
    splitbynoncontiguous = TRUE,
    splitbycommonlabel = TRUE,
    findlongest = FALSE,
    min_lcs_length = 10,
    min_common_strings = 5,
    consistent_consecutive_mode = FALSE,
    consecutive_range = 10,
    variable_compare_mode = "reduced"
  )
  if(is.null(config)){config <- list()}
  config <- utils::modifyList(default_config, config)

  # Extract parameters for easier access
  variable_name_sep <- seps_to_use$variable_name_sep
  prefix_sep <- seps_to_use$prefix_sep
  statement_sep <- seps_to_use$statement_sep

  splitbyclass <- config$splitbyclass
  splitbynumlabelledvalues <- config$splitbynumlabelledvalues
  splitbynoncontiguous <- config$splitbynoncontiguous
  splitbycommonlabel <- config$splitbycommonlabel
  findlongest <- config$findlongest
  min_lcs_length <- config$min_lcs_length
  min_common_strings <- config$min_common_strings
  consistent_consecutive_mode <- config$consistent_consecutive_mode
  consecutive_range <- config$consecutive_range
  variable_compare_mode <- config$variable_compare_mode

  # Basic type checks
  if (!is.data.frame(temp_dpdict) || !is.data.frame(temp_dat)) {
    stop("temp_dpdict and temp_dat must be data frames")
  }

  if (!is.character(variable_name_sep)) {
    stop("variable_name_sep must be a character string")
  }

  if (!is.numeric(noisy) || length(noisy) != 1 || noisy < 0 || noisy > 4) {
    stop("noisy must be a single integer between 0 and 4")
  }
  ## End of validation

  # Turn off findlongest if splitbycommonlabel is FALSE
  if (splitbycommonlabel == FALSE && findlongest == TRUE) {
    findlongest <- FALSE
    if (noisy >= 1) {
      message("Setting findlongest to FALSE as it's only used if splitbycommonlabel is TRUE")
    }
  }

  # validate variables_to_process
  if(is.null(variables_to_process)){
    variables_to_process <- rep(TRUE, nrow(temp_dpdict))
  } else {
    # validate type
    if (!is.logical(variables_to_process) || length(variables_to_process) != nrow(temp_dpdict)) {
      stop("variables_to_process must be a logical vector of length nrow(temp_dpdict)")
    }
  }

  # initialise question_lcs
  if(!"question_lcs" %in% names(temp_dpdict)){
    temp_dpdict$question_lcs <- NA_character_
  }

  # creates a question group variable if there isn't one already
  if(!"question_group" %in% names(temp_dpdict)){
    temp_dpdict$question_group <- NA_character_
  }

  # Identify rows that need question_group initialisation
  rows_to_update <- variables_to_process & (is.na(temp_dpdict$question_group))

  # initalise question group for those rows
  if(any(rows_to_update)) {
    temp_dpdict$question_group[rows_to_update] <- gsub(paste0(variable_name_sep,".*"), "",
                                                       temp_dpdict$variable_names[rows_to_update])
    # Add suffix to these initialized groups
    temp_dpdict$question_group[rows_to_update] <- paste0(temp_dpdict$question_group[rows_to_update], "_a")
  }

  # Expand variables_to_process to include all variables in a question group if at least one is selected
  if(!all(variables_to_process == TRUE)){
    original_groups <- stats::na.omit(unique(temp_dpdict$question_group[variables_to_process])) # don't expand NAs (e.g. if question_group newly initialized)
    expanded_variables <- temp_dpdict$question_group %in% original_groups

    if (any(expanded_variables == TRUE & variables_to_process == FALSE)) {
      warning("Some question groups were partially selected. All variables in these groups will be processed.")
      variables_to_process[expanded_variables == TRUE & variables_to_process == FALSE] <- TRUE
    }
  }

  # find longest common label for each question group and store in lcs_dict that we can reference
  if(findlongest == TRUE){

    # lcs_dict is a subset of columns in temp_dpdict, with an additional column for lcs
    lcs_dict <- data.frame(variable_names = temp_dpdict$variable_names[variables_to_process],
                       question_group = temp_dpdict$question_group[variables_to_process],
                       lcs = NA_character_,
                       stringsAsFactors = FALSE)

    # loop through each unique question group
    for(i in unique(lcs_dict$question_group)){
      # subset temp_dpdict just for the variables in our current question group
      current_question_dpdict <- temp_dpdict[temp_dpdict$question_group == i,]

      if(noisy >= 3){
        print(paste0("Finding best lcs for question group ", i, "..."))
      }

      # if requested, add a column for variable_labels_less_prefix - for every variable in current question group
      if(ignorelabelbeforeprefix == TRUE){
        current_question_dpdict$variable_labels_less_prefix <- ""
        for(j in 1:nrow(current_question_dpdict)){
          current_question_dpdict$variable_labels_less_prefix[j] <- gsub(paste0("^([^", prefix_sep, "]+)", prefix_sep), "", current_question_dpdict$variable_labels[j])
        }
      }

      # initialize columns for lcs and lcs_distance
      current_question_dpdict$lcs <- ""
      current_question_dpdict$lcs_distance <- NA_integer_

      # go through each row in the current_question_dpdict (i.e. each variable with the same initial question group) and save the best lcs found for that variable
      # the 'best lcs' is one that prioritises a string that is common to other variables in the same question group (or other variables close by, if variable_compare_mode == "reduced")
      # so with variable_compare_mode == "complete", we eventually compare every variable in the question group to every other variable in the question group
      for(j in seq_len(nrow(current_question_dpdict))){

        # we'll use variable name to reference the row in variabledpdict that we're checking in the current loop (we are operating on a dataframe that is a subset of current_question_dpdict so indexes may get confusing)
        j_name <- current_question_dpdict$variable_names[j]

        # we'll create a variabledpdict to store the lcs between j_name and each variable in the question group
        if(variable_compare_mode == "complete" | nrow(current_question_dpdict) == 1){
          # ...creates an entire new dict the size of the question group
          variabledpdict <- current_question_dpdict
        } else if(variable_compare_mode == "reduced"){
          # ...creates a new dict to check rows before and after the current variable
          rows_to_check <- (j - consecutive_range):(j + consecutive_range)
          rows_to_check <- rows_to_check[rows_to_check >= 1 & rows_to_check <= nrow(current_question_dpdict)] # can't index below 1 and can't index above nrow(current_question_dpdict)
          variabledpdict <- current_question_dpdict[rows_to_check,]
        }

        variabledpdict$lcs <- ""
        variabledpdict$lcs_distance <- NA_integer_

        # start with the full label
        variabledpdict$lcs[variabledpdict$variable_names == j_name] <- ifelse(ignorelabelbeforeprefix,
                                                                                  current_question_dpdict$variable_labels_less_prefix[current_question_dpdict$variable_names == j_name],
                                                                                  current_question_dpdict$variable_labels[current_question_dpdict$variable_names == j_name])

        # if more than one variable in variabledpdict the logic is much more involved
        if(nrow(variabledpdict) > 1){

          if(noisy >= 4){
            print(paste0("finding longest common label for variable ", j_name))
          }

          # for each variable in variabledpdict (our dataframe which is either the same as current_question_dpdict or a subet of it, where we will compare each variable to j_name)
          for(k in 1:nrow(variabledpdict)){

            # string1 is the variable we're comparing everything to in our j loop (i.e. run once for each variable in the question group)
            string1 = variabledpdict$lcs[variabledpdict$variable_names==j_name]
            # we will compare string 1 to every other string in the question group in turn, with string2.
            string2 = ifelse(ignorelabelbeforeprefix, gsub(paste0("^([^", prefix_sep, "]+)", prefix_sep), "", variabledpdict$variable_labels[k]),
                             variabledpdict$variable_labels[k])

            temp_new_common_string <- get_longest_common_substring(string1, string2, fromstart = TRUE)

            if(!is.na(temp_new_common_string)){
              # if it meets the min_lcs_length, store the lcs between string1 (j_name) and each variable in the same question group.
              if(nchar(temp_new_common_string) >= min_lcs_length){
                variabledpdict$lcs[k] <- temp_new_common_string
                variabledpdict$lcs_distance[k] <- attr(temp_new_common_string, "lcs_distance")
              }
            }
          }

          # remove any candidate common strings below the min_lcs_length
          variabledpdict <- subset(variabledpdict, nchar(variabledpdict$lcs) >= min_lcs_length)

          if(consistent_consecutive_mode == TRUE){ # intended to speed up finding the best lcs. but it's not always useful as it can be too sensitive to spurious lcs. not used by default

            forward_increments <- (which(variabledpdict$variable_names == j_name) + 1):nrow(variabledpdict)
            backward_increments <- 1:(which(variabledpdict$variable_names == j_name) - 1)

            # check for consistent forward consecutive lcs values
            if(length(unique(variabledpdict$lcs[forward_increments])) == 1 &
               which(variabledpdict$variable_names == j_name) + consecutive_range <= nrow(variabledpdict) &
               which(variabledpdict$variable_names == j_name) - consecutive_range >= 1){
              consistent_forward_consecutives_lcs <- variabledpdict$lcs_distance[forward_increments[1]]
            } else {
              consistent_forward_consecutives_lcs <- 0
            }

            # same for backwards
            if(length(unique(variabledpdict$lcs[backward_increments])) == 1 &
               which(variabledpdict$variable_names == j_name) - consecutive_range >= 1 &
               which(variabledpdict$variable_names == j_name) + consecutive_range <= nrow(variabledpdict)){
              consistent_backward_consecutives_lcs <- variabledpdict$lcs_distance[backward_increments[1]]
            } else {
              consistent_backward_consecutives_lcs <- 0
            }

            if(consistent_forward_consecutives_lcs > consistent_backward_consecutives_lcs){
              if(noisy >= 3){print("consistent forward consecutive lcs used")}
              best_lcs <- variabledpdict$lcs[forward_increments[1]]
              best_lcs_distance <- variabledpdict$lcs_distance[forward_increments[1]]
            } else if(consistent_backward_consecutives_lcs > consistent_forward_consecutives_lcs){
              if(noisy >= 3){print("consistent backward consecutive lcs used")}
              best_lcs <- variabledpdict$lcs[backward_increments[1]]
              best_lcs_distance <- variabledpdict$lcs_distance[backward_increments[1]]
            }
          } else {
            # find the longest common string shared by at least min_common_strings strings in the same question group.

            # this isn't straightforward:
            # e.g. where one question group has a suffix that is common to multiple but not all statements
            # but another question group has too few statements such that min_common_strings is not reached
            # not as simple as just taking the string common to most variables as we also want to maximise length
            # e.g. where there are common substrings e.g. where there is a question, statement and brand separated by seps
            # CURRENT SOLUTION: where no variables found meeting the min_common_strings, we take the most common string, and only then, if multiple, the longest lcs, instead of j_name

            variabledpdict_summary <- variabledpdict[, c("lcs", "lcs_distance")]
            variabledpdict_summary$n <- vapply(variabledpdict_summary$lcs, # n here is the number of variables with matching lcs
                                               function(x) sum(grepl(x, variabledpdict$variable_labels, fixed = TRUE)),
                                               numeric(1))

            if(nrow(subset(variabledpdict_summary, n >= min_common_strings)) < 1){
              variabledpdict_summary <- variabledpdict_summary[which.max(variabledpdict_summary$n), ]
            } else {
              variabledpdict_summary <- subset(variabledpdict_summary, n >= min_common_strings)
            }

            best_lcs_distance <- max(variabledpdict_summary$lcs_distance)
            best_lcs <- variabledpdict_summary$lcs[which.max(variabledpdict_summary$lcs_distance)]

          }

          # save best_lcs from variabledpdict, i.e. the lcs between j_name and every other variable in the question group, to current_question_dpdict
          # also trim statement_sep and whitespace
          current_question_dpdict$lcs[current_question_dpdict$variable_names == j_name] <- trimws(gsub(paste0(statement_sep, "+$"), "", best_lcs))
          current_question_dpdict$lcs_distance[current_question_dpdict$variable_names == j_name] <- best_lcs_distance

          if(noisy >= 4){print(paste0("lcs found for ", j_name, ": ", best_lcs))}
        } else if (nrow(variabledpdict) == 1){ # if only one variable in variabledpdict, there's no need for complex logic to find lcs
          current_question_dpdict$lcs[current_question_dpdict$variable_names == j_name] <- trimws(gsub(paste0(statement_sep, "+$"), "", variabledpdict$lcs))
          current_question_dpdict$lcs_distance[current_question_dpdict$variable_names == j_name] <- variabledpdict$lcs_distance
        }
      }

      if(noisy >= 3 & nrow(current_question_dpdict)>1){
        if(length(unique(current_question_dpdict$lcs))==1){
          print(paste0("common lcs found for question group ", i, ":", current_question_dpdict$lcs[1]))
        }

        print(paste0("current_question_dpdict with lcs for question group ", i, ":"))
        print(current_question_dpdict)
      }

      # update lcs_dict, which contains all variables from temp_dpdict, with the best lcs found for each variable in the question group
      lcs_dict$lcs[lcs_dict$variable_names %in% current_question_dpdict$variable_names] <- current_question_dpdict$lcs
    }
    originaltemp_dpdict <- temp_dpdict
  }

  # for each question group
  for(j in unique(temp_dpdict$question_group[variables_to_process])){
    current_question_dpdict <- temp_dpdict[temp_dpdict$question_group == j & variables_to_process,]
    # and we'll also save the lcs associated with each question group
    current_question_dpdict$question_lcs <- NA

    # initialize some variables before the next loop
    last_class <- ""
    last_question_group <- ""
    last_suffix <- ""
    last_commonlabel <- ""
    last_numlabelledvalues <- ""
    last_index <- 0

    if(ignorelabelbeforeprefix == TRUE){
      current_question_dpdict$variable_labels_less_prefix <- gsub(paste0("^([^", prefix_sep, "]+)", prefix_sep),
                                                          "", current_question_dpdict$variable_labels)
    }

    for(i in seq_len(nrow(current_question_dpdict))){

      if(noisy >= 3){
        print(paste0("finding question group for ", current_question_dpdict$variable_names[i], "..."))
      }

      current_class <- class(temp_dat[[match(TRUE, names(temp_dat) == current_question_dpdict$variable_names[i])]])
      current_question_group <- gsub("_.*", "", current_question_dpdict$question_group[i]) # current_question defined WITHOUT any suffixes

      # ! error here. current_question_dpdict is sometimes empty (mixed of NULLs and NAs)
      current_numlabelledvalues <- length(sjlabelled::get_labels(temp_dat[, current_question_dpdict$variable_names[i], drop = FALSE])[[1]]) # we want character variables to always be given their own question group, so for example if two character variables have value labels and these obviously different, they're grouped separately
      current_index <- match(current_question_dpdict$variable_names[i], temp_dpdict$variable_names)

      if(findlongest == TRUE){
        current_commonlabel <- lcs_dict$lcs[lcs_dict$variable_names == current_question_dpdict$variable_names[i]]
      } else{
        # remove everything after the last label_sep

        if(ignorelabelbeforeprefix == TRUE){
          current_question_dpdict$variable_labels_less_prefix <- gsub(paste0("^([^", prefix_sep, "]+)", prefix_sep),
                                                              "", current_question_dpdict$variable_labels)
          current_commonlabel <- gsub(paste0(statement_sep, "\\s*"), "", strsplit(current_question_dpdict$variable_labels_less_prefix[current_question_dpdict$variable_names == current_question_dpdict$variable_names[i]], paste0(statement_sep, "\\s*"))[[1]][1])
        } else {
          current_commonlabel <- gsub(paste0(statement_sep, "\\s*"), "", strsplit(current_question_dpdict$variable_labels[current_question_dpdict$variable_names == current_question_dpdict$variable_names[i]], paste0(statement_sep, "\\s*"))[[1]][1])
        }
      }
      # save the current lcs, which will be shared by each question group
      current_question_dpdict$question_lcs[i] <- current_commonlabel

      # there are 3 cases: same question group root with same question group suffix, same question group root with different question group suffix, and different question group
      if(current_question_group == last_question_group){
        new_suffix_required <- FALSE

        if(splitbyclass == TRUE && !identical(current_class, last_class)){
          new_suffix_required <- TRUE
          if(noisy >= 4){
            cat(paste0(current_question_dpdict$variable_names[i]," split by class:"), "\n")
            cat(paste0("last_class: ", last_class), "\n")
            cat(paste0("current_class: ", current_class), "\n", "\n")
          }
        }

        if(splitbycommonlabel == TRUE && current_commonlabel != last_commonlabel){
          new_suffix_required <- TRUE
          if(noisy >= 4){
            cat(paste0(current_question_dpdict$variable_names[i]," split by commonlabel:"), "\n")
            cat(paste0("last_commonlabel: ", last_commonlabel), "\n")
            cat(paste0("current_commonlabel: ", current_commonlabel), "\n", "\n")
          }
        }
        if(splitbynumlabelledvalues == TRUE && current_numlabelledvalues != last_numlabelledvalues){
          new_suffix_required <- TRUE
          if(noisy >= 4){
            cat(paste0(current_question_dpdict$variable_names[i]," split by number of labelled values:"), "\n")
            cat(paste0("last_numlabelledvalues: ", last_numlabelledvalues), "\n")
            cat(paste0("current_numlabelledvalues: ", current_numlabelledvalues), "\n", "\n")
          }
        }
        if(splitbynoncontiguous == TRUE && current_index != last_index+1){
          new_suffix_required <- TRUE
          if(noisy >= 4){
            cat(paste0(current_question_dpdict$variable_names[i]," split by non-contiguous indexes:"), "\n")
            cat(paste0("last_index: ", last_index), "\n")
            cat(paste0("current_index: ", current_index), "\n", "\n")
          }
        }

        if(new_suffix_required == TRUE){
          current_suffix <- last_suffix
          current_question_dpdict$question_group[i] <- paste0(gsub("_.*", "", current_question_group), "_", letters702[which(letters702 == current_suffix)+1])
          if(noisy >= 3){
            print(paste0("variable ", current_question_dpdict$variable_names[i], "given new question_group ", current_question_dpdict$question_group[i]))
          }
        } else if (new_suffix_required == FALSE){
          current_suffix <- last_suffix
          current_question_dpdict$question_group[i] <- paste0(gsub("_.*", "", current_question_group), "_", current_suffix)
        }
      }

      last_class <- current_class
      last_question_group <- current_question_group
      last_numlabelledvalues <- current_numlabelledvalues
      last_index <- current_index
      last_commonlabel <- current_commonlabel
      last_suffix <- gsub(".*_", "", current_question_dpdict$question_group[i])

    } # end of loop through current_question_dpdict

    # apply new question groups in current_question_dpdict to full temp_dpdict
    temp_dpdict$question_group[temp_dpdict$variable_names %in% current_question_dpdict$variable_names] <- current_question_dpdict$question_group
    temp_dpdict$question_lcs[temp_dpdict$variable_names %in% current_question_dpdict$variable_names] <- current_question_dpdict$question_lcs

  } # end of loop through question groups

  return(temp_dpdict)
}


#' get_longest_common_substring_slow
#'
#' an old implementation of getting the longest common substring. about 3x slower than get_longest_common_substring()
#'
#' @param string1 a string
#' @param string2 another string
#' @param lcs_mode either "complete", "starting_string_priority" or "starting_string_only".
#' if lcs_mode == "complete" will, for each starting position in string1, attempt to successively add characters to find the longest possible match.
#' if lcs_mode == "starting_string_priority" will look for a match in starting string first...
#'  1. if starting_string is a match in both, will try extending starting string to find a longer lcs. if not found, will assume starting_string is lcs. (i.e. doesn't look for an alternative lcs)
#'  2. if starting_string is not a match in both, will try from next starting location in starting_string (i.e. removing the first character), and then attempt (1) above
#'  3. if still no match, will revert to "complete" above (though checking so as to not repeat checks already made)
#'  if match found within starting_string, superset or subset of starting_string, will finish early, i.e. won't look for a longer lcs outside starting_string
#' if lcs_mode == "starting_string_only" will only look for a match in starting string.
#' @param starting_string string to start with if lcs_mode is starting_string_priority or starting_string_only
#' @param increments_mode "forward", "backward", or "random".
#' if increments == "forward" will add characters successively and check match each time.
#' if increments == "random" will randomise number of characters to add each time, without replacement, bounded by min(nchar(string1), nchar(string2) - so should be faster for longer strings.
#' @param stopping_threshold integer. if iterations since last improvement in lcs exceeds stopping_threshold, returns best lcs so far.
#' @param noisy logical. if true, prints status messages.
#'
#' @return substring which represents the longest common starting substring found between string1 and string2, with an attribute lcs_distance representing the number of characters in the lcs
#' @export
#'
#' @examples
#' get_longest_common_substring_slow("boat", "boar")
get_longest_common_substring_slow <- function(string1, string2, lcs_mode = "complete", increments_mode = "backward", stopping_threshold = 3, starting_string = NULL, noisy = FALSE){

  if(noisy == TRUE){print("------- new call to get_longest_common_string ------- ")}

  if(!is.integer(starting_string) & !is.null(starting_string)){
    lcs_mode <- "complete"
  }

  if(grepl(string1, string2, fixed = TRUE)){ # if first string in second string, just return the first string
    temp_new_common_string <- string1
    attr(temp_new_common_string, "lcs_distance") <- nchar(string1)
  } else if(grepl(string2, string1, fixed = TRUE)){ # if second string in first string, just return the second string
    temp_new_common_string <- string2
    attr(temp_new_common_string, "lcs_distance") <- nchar(string2)
  } else{

    string1_split <- unlist(strsplit(string1, split = ""))
    string2_split <- unlist(strsplit(string2, split = ""))
    if(!is.null(starting_string)){starting_string_split <- unlist(strsplit(starting_string, split = ""))}else{starting_string_split = NULL}
    logical_max_lcs <- min(nchar(string1), nchar(string2))

    # dataframe to store lcs for each starting position for string1
    string1_lcs_lengths <- data.frame(starting_position = string1_split,
                                  lcs = NA_character_,
                                  lcs_length = NA_integer_)

    if(lcs_mode == "complete"){remaining_locations <- 1:nchar(string1)
    } else if (lcs_mode == "starting_string_priority"){ # reorder starting positions to loop through to start with the matching string
      remaining_locations <- c(stats::na.omit(match(starting_string_split, string1_split)), match(string1_split[!string1_split %in% starting_string_split], string1_split))
    } else if (lcs_mode == "starting_string_only"){
      remaining_locations <- stats::na.omit(match(starting_string_split, string1_split))
    }

    # create empty increments list for referencing
    string1_increments_list <- list()
    for(i in 1:nchar(string1)){
      string1_increments_list[[string1_split[[i]]]] <- data.frame(increment = 0:(length(string1_split)-i),
                                                              match_status = rep(NA, length(string1_split)-i+1))}

    get_string_match_by_increments <- function(string1_split, string2_split, string1_starting_position, increments_mode = "forward", lcs_mode = "complete", starting_string_split = NULL){
      # returns a dataframe with match_status for each additional character from the starting_position in string1 (i.e. increment 0 is the starting position)

      # get string2_starting_position (or return NA if doesn't exist)
      string2_starting_position <- match(string1_split[string1_starting_position], string2_split) # faster than str_locate
      if(is.na(string2_starting_position)){
        return(NA)
      }

      # set up dataframe for storing match_status
      out <- data.frame(increment = 0:(length(string1_split)-string1_starting_position),
                    match_status = rep(NA, length(string1_split)-string1_starting_position+1))

      if(lcs_mode == "starting_string_priority" | lcs_mode == "starting_string_only"){
        # if a match to starting_string in string1 and string2 then start incrementally forward from the end of the starting string to see how long lcs could be
        if(stringr::str_detect(paste(string1_split, collapse = ""), paste(starting_string_split, collapse = "")) & stringr::str_detect(paste(string2_split, collapse = ""), paste(starting_string_split, collapse = ""))){
          # find increments after starting string

          # str_locate(..., ...)[2] gives us locaton at end of starting_string in string1, so +1 gives us the next location...
          # but we need to calibrate for increment, not position, by - string1_starting_position
          forward_increments <- (stringr::str_locate(paste(string1_split, collapse = ""), paste(starting_string_split, collapse = ""))[2]+1-string1_starting_position):(length(string1_split)-string1_starting_position)

          remaining_increments <- forward_increments # we only check forward increments in this case because we've already matched starting_string and so know that lower increments will have a lower lcs that starting_string
          # remaining_increments <- c(forward_increments, remaining_increments[!remaining_increments %in% forward_increments]) # not used: moves forward increments so first in order but still keeps others

          out$match_status[!out$increment %in% forward_increments] <- TRUE # and update match_status for full starting_string, because matched
          if(noisy == TRUE){print("starting_string found in string1 and string2 - will check in forward increments")}

        } else { # if no match for whole starting_string, we know longer strings won't match either...
          # if the first character (i.e. increment 0) in string1 and in string2 is in starting_string
          if((string1_split[string1_starting_position] == starting_string_split[1]) & (string2_split[string2_starting_position] == starting_string_split[1])){

            out$match_status[out$increment == 0] <- TRUE # record match for increment == 0 i.e. starting_position
            if(noisy == TRUE){print("partial match for starting_string found in string1 and string2 - will check in backwards increments from full starting_string")}

            remaining_increments <- seq((nrow(out)-1), 0) # we will order remaining_increments to work backwards...
            # and because we know there's no full match, we can at least rule out increments from nchar(starting_string) onwards
            forward_increments <- length(starting_string_split):(length(string1_split)-string1_starting_position)
            remaining_increments <- remaining_increments[!remaining_increments %in% forward_increments]
            out$match_status[out$increment %in% forward_increments] <- FALSE # and update match_status for those removed
          } else {
            # if no match for whole or first character of starting_string, switch to lcs_mode = "complete", or break, depending on settings
            if(lcs_mode == "starting_string_priority"){
              if(noisy == TRUE){print("no match for either whole starting_string or first character - switching to lcs_mode = complete for this starting position in string1")}
              lcs_mode = "complete"
            } else if(lcs_mode == "starting_string_only"){
              if(noisy == TRUE){print("no match for either whole starting_string or first character - stopping attempts for this starting position in string1")}
              return(NA)
            }
          }
        }
      } # end of logic for lcs_mode == "starting_string"
      if(lcs_mode == "complete"){
        if(increments_mode == "forward"){remaining_increments <- 0:(nrow(out)-1)
        } else if (increments_mode == "random"){remaining_increments <- sample(0:(nrow(out)-1))
        } else if (increments_mode == "backward"){remaining_increments <- seq((nrow(out)-1), 0)}
      }

      # while loop so that we can check for matches in random order and still stop early if no match found
      while(length(remaining_increments)>0){
        j = remaining_increments[1] # at the start of each loop, set j to the next increment to check

        if(string1_split[string1_starting_position+j] %in% string2_split[string2_starting_position+j]){
          out$match_status[out$increment == j] <- TRUE
          remaining_increments <- remaining_increments[-1] # remove the increment we've just checked
        } else {
          remaining_increments <- remaining_increments[!remaining_increments >= j] # if no match, remove all increments > the current one which hasn't matched
          out$match_status[out$increment %in% j:nrow(out)] <- FALSE
        }
      } # end of while loop

      return(out)
    }

    get_lcs_from_increments_df <- function(string1_split, increments_df, string1_starting_position){
      # takes a df from get_string_match_by_increments and returns the corresponding lcs

      if(is.data.frame(increments_df)){
        return(paste(string1_split[stats::na.omit(increments_df$increment[increments_df$match_status])+string1_starting_position], collapse = ""))
      } else{
        return(NA)
      }
    }

    stopping_counter = 0 # counts iterations since last improvement. if exceeds stopping_threshold, returns best lcs so far.

    # loop through possible starting locations in string1
    # if(noisy == TRUE){print(paste0("testing characters in positions in string1 in order ", paste(remaining_locations, collapse = ", ")))}
    while(length(remaining_locations)>0){
      i = remaining_locations[1]
      if(noisy == TRUE){print(paste0("next character in string1 is ", string1_split[i], " (position ", i, ")"))}

      # if not logically possible for current starting position to provide the lcs (because not enough characters left), NAs all subsequent locations and ends the loop
      suppressWarnings({ # annoying warning for attempting max on NA returning -Inf, which we account for
        current_max_lcs <- max(string1_lcs_lengths$lcs_length, na.rm=TRUE)
        if(current_max_lcs == -Inf){current_max_lcs <- 0}
      })
      if((logical_max_lcs-i+1)< current_max_lcs){
        if(noisy == TRUE){print("remaining possible starting positions before reaching theoretical max lcs (i.e. min of string1 and string2) is smaller than already registered max lcs. skipping checking this and subsequent starting positions.")}
        for(j in i:nchar(string1)){
          string1_increments_list[[string1_split[[j]]]] <- NA
        }
        string1_lcs_lengths$lcs[i:nchar(string1)] <- NA_character_
        string1_lcs_lengths$lcs_length[nchar(string1)] <- NA_integer_

        remaining_locations <- remaining_locations[!remaining_locations >= i]

      } else { # if logically possible for current starting position to provide the lcs, gets a dataframe showing matches for each possible length of string

        string1_increments_list[[string1_split[[i]]]] <- get_string_match_by_increments(string1_split, string2_split, string1_starting_position = i, increments_mode = increments_mode, lcs_mode = lcs_mode, starting_string_split = starting_string_split)
        # then finds the top lcs and saves to string1_lcs_lengths
        string1_lcs_lengths$lcs[i] <- get_lcs_from_increments_df(string1_split, string1_increments_list[[i]], i)
        if(noisy == TRUE){print(paste0("lcs found for position ", i, ": ", string1_lcs_lengths$lcs[i]))}
        string1_lcs_lengths$lcs_length[i] <- nchar(string1_lcs_lengths$lcs[i])
        if(is.na(string1_lcs_lengths$lcs_length[i])){string1_lcs_lengths$lcs_length[i] <- 0}

        if(string1_lcs_lengths$lcs_length[i] <= current_max_lcs){stopping_counter <- stopping_counter +1}
        if(noisy == TRUE){print(paste0(stopping_counter, " iterations since last new lcs"))}

        remaining_locations <- remaining_locations[-1]
      }
      if(stopping_counter >= stopping_threshold){
        temp_new_common_string <- string1_lcs_lengths$lcs[match(max(string1_lcs_lengths$lcs_length, na.rm=TRUE), string1_lcs_lengths$lcs_length)]
        attr(temp_new_common_string, "lcs_distance") <- nchar(temp_new_common_string)
        return(temp_new_common_string)
      }
    }

    # variables to return
    temp_new_common_string <- string1_lcs_lengths$lcs[match(max(string1_lcs_lengths$lcs_length, na.rm=TRUE), string1_lcs_lengths$lcs_length)]
    attr(temp_new_common_string, "lcs_distance") <- nchar(temp_new_common_string)

  } # end of else for finding lcs

  return(temp_new_common_string)
}


#' get_longest_common_substring
#'
#' finds the long common substring (lcs) from two given strings
#'
#' @param string1 a string
#' @param string2 another string
#' @param fromstart logical. if true, finds longest common substring from the start of string1. if false, looks for lcs at any position.
#'
#' @return substring which represents the longest common starting substring found between string1 and string2, with an attribute lcs_distance representing the number of characters in the lcs
#' @export
#'
#' @examples get_longest_common_substring("boat", "boar")
get_longest_common_substring <- function(string1, string2, fromstart = FALSE) {

  ### validation
  if (!is.character(string1) || length(string1) != 1) {
    stop("`string1` must be a single character string.")
  }
  if (!is.character(string2) || length(string2) != 1) {
    stop("`string2` must be a single character string.")
  }
  if (!is.logical(fromstart) || length(fromstart) != 1) {
    stop("`fromstart` must be a single logical value (TRUE/FALSE).")
  }

  m <- nchar(string1)
  n <- nchar(string2)

  # Create a matrix to store the lengths of common substrings
  lcs_matrix <- matrix(0, nrow = m + 1, ncol = n + 1)

  # Variables to store the maximum length and ending position of the longest common substring
  max_length <- 0
  end_pos <- 0

  # Fill the matrix
  for (i in 1:m) {
    for (j in 1:n) {
      if (substr(string1, i, i) == substr(string2, j, j)) {
        lcs_matrix[i + 1, j + 1] <- lcs_matrix[i, j] + 1

        if(fromstart == TRUE){
          if (lcs_matrix[i + 1, j + 1] > max_length && i == lcs_matrix[i + 1, j + 1]) {
            max_length <- lcs_matrix[i + 1, j + 1]
            end_pos <- i
          }
        } else {
          if (lcs_matrix[i + 1, j + 1] > max_length) {
            max_length <- lcs_matrix[i + 1, j + 1]
            end_pos <- i
          }
        }

      }
    }
  }

  # Extract the longest common substring
  start_pos <- end_pos - max_length + 1
  longest_common_substring <- substr(string1, start_pos, end_pos)

  attr(longest_common_substring, "lcs_distance") <- nchar(longest_common_substring)

  return(longest_common_substring)
}


#' create_questions_dict
#'
#' creates questions_dict from a dpdict. if dpdict doesn't already has a questions_lcs column, creates one.
#'
#' optionally calls data_edit before returning, for any manual edits.
#'
#' @param survey_obj a survey_data object, or NULL, in which case requires a temp_dpdict
#' @param temp_dpdict a dpdict
#' @param editfirst whether to call data_edit before returning, for any manual edits
#'
#' @return a questions_dict with questions metadata corresponding to the given dpdict
#' @export
#'
#' @examples
#' # Using a dpdict
#' temp_dpdict <- create_dict_with_metadata(get_big_test_dat(n=10))
#' questions_dict <- create_questions_dict(NULL, temp_dpdict, editfirst = FALSE)
#'
#' # Using a survey_data object
#' survey_obj <- create_survey_data(get_big_test_dat(n=10))
#' questions_dict <- create_questions_dict(survey_obj, editfirst = FALSE)
create_questions_dict <- function(survey_obj = NULL, temp_dpdict = NULL, editfirst = TRUE){

  if (is.survey_data(survey_obj)) {
    temp_dpdict <- survey_obj$dpdict
  } else {
    stopifnot("Must provide either a survey_data object or a dpdict that is a dataframe" = is.data.frame(temp_dpdict))
  }

  stopifnot("dpdict must include columns 'question_group' and either 'question_lcs' (created as part of split_into_question_groups()) or 'variable_labels'" = "question_group" %in% names(temp_dpdict) && ("variable_labels" %in% names(temp_dpdict) || "question_lcs" %in% names(temp_dpdict)))

  # get lcs for each question if not already provided in temp_dpdict
  if(!"question_lcs" %in% names(temp_dpdict)){

    questions_dict <- data.frame(question_group = unique(temp_dpdict$question_group), question_lcs = NA)
    for(i in questions_dict$question_group){
      current_question_dpdict <- temp_dpdict[temp_dpdict$question_group == i,]
      currentlcs <- current_question_dpdict$variable_labels[1]
      for(j in 1:nrow(current_question_dpdict)){
        currentlcs <- get_longest_common_substring(currentlcs, current_question_dpdict$variable_labels[j])
      }
      questions_dict$question_lcs[questions_dict$question_group == i] <- currentlcs
    }
  } else {
    unique_indices <- !duplicated(temp_dpdict$question_group)
    questions_dict <- temp_dpdict[unique_indices, c("question_group", "question_lcs")]
  }

  questions_dict$question_alias <- questions_dict$question_group
  questions_dict$question_description <- ifelse(questions_dict$question_alias == "",
                                                questions_dict$question_lcs,
                                                paste(sub("_.*", "", questions_dict$question_alias), questions_dict$question_lcs, sep = ". "))
  questions_dict$question_folder <- NA_character_

  if(editfirst == TRUE){
    questions_dict <- DataEditR::data_edit(questions_dict, viewer_height = 900, viewer_width = 1800)
    questions_dict$question_description <- ifelse(questions_dict$question_alias == "",
                                                  questions_dict$question_lcs,
                                                  paste(sub("_.*", "", questions_dict$question_alias), questions_dict$question_lcs, sep = ". "))
  }

  return(questions_dict)
}

#' get_affix_df
#'
#' gets a dataframe summarising seps found in the full_string, the substrings separated, and their locations
#'
#' used by get_unique_suffixes
#'
#' @param full_string a string, e.g. a variable label
#' @param affix_type "suffix" or "prefix"
#' @param seps_priority a vector of strings representing separators to check for (in order)
#' @param strings_to_remove a vector of any strings to remove before working
#' @param filter_results whether to remove any rows where the affix is an empty string
#' @param sort_results whether to sort the final dataframe
#' @param prioritise_caps whether to sort final dataframe prioritising capital letters
#'
#' @details note that sep_count_from_end/ sep_count_from_start is the count within that sep, not the count across all seps
#'
#' @return a dataframe summarising seps found in the full_string, the substrings separated, and their locations
#' @export
#'
#' @examples get_affix_df("Q1: This is a variable - This is a statement")
get_affix_df <- function(full_string, affix_type = "prefix", seps_priority = c("- ", "_", "."), strings_to_remove = c("Selected Choice"),
                         prioritise_caps = TRUE, filter_results = TRUE, sort_results = TRUE) {

  ### Validation
  if (!is.character(full_string) || length(full_string) != 1 || !nzchar(full_string)) {
    stop("`full_string` must be a single non-empty character string.")
  }
  # Validate affix_type (already done in original code) [cite: 219]
  if (!affix_type %in% c("suffix", "prefix")) {
    stop("affix_type must be either 'suffix' or 'prefix'")
  }
  if (!is.character(seps_priority) || length(seps_priority) == 0) {
    stop("`seps_priority` must be a non-empty character vector.")
  }
  if (!is.character(strings_to_remove)) {
    stop("`strings_to_remove` must be a character vector.")
  }
  stopifnot(
    "`prioritise_caps` must be a single logical value" = is.logical(prioritise_caps) && length(prioritise_caps) == 1,
    "`filter_results` must be a single logical value" = is.logical(filter_results) && length(filter_results) == 1,
    "`sort_results` must be a single logical value" = is.logical(sort_results) && length(sort_results) == 1
  )

  # Remove specified strings
  full_string <- gsub(paste(strings_to_remove, collapse="|"), "", full_string, fixed = TRUE)

  # Pre-allocate result list
  max_possible_matches <- nchar(full_string)
  result <- vector("list", max_possible_matches * length(seps_priority))

  # Counter for result list
  count <- 0

  # iterates through each sep in seps_priority, finding all occurances of sep, trimming whitespace, and recording count name and value
  for (sep in seps_priority) {
    sep_matches <- gregexpr(sep, full_string, fixed = TRUE)[[1]]
    if (sep_matches[1] != -1) {
      n_matches <- length(sep_matches)
      for (j in seq_len(n_matches)) {
        count <- count + 1
        if (affix_type == "suffix") {
          affix <- substr(full_string, sep_matches[j] + nchar(sep), nchar(full_string))
          affix_trimmed <- trimws(affix)
          count_name <- "sep_count_from_end"
          count_value <- n_matches - j + 1
        } else {  # prefix
          affix <- substr(full_string, 1, sep_matches[j] - 1)
          affix_trimmed <- trimws(affix)
          count_name <- "sep_count_from_start"
          count_value <- j
        }

        result[[count]] <- list(
          sep_type = sep,
          location_found = sep_matches[j],
          count_value = count_value,
          affix_found = affix_trimmed,
          starts_with_cap = grepl("^[A-Z]", affix_trimmed)
        )
      }
    }
  }

  # Convert list to data frame
  result <- result[seq_len(count)]
  result_df <- do.call(rbind, lapply(result, data.frame, stringsAsFactors = FALSE))

  if (nrow(result_df) == 0 || is.null(result_df)) {
    return(NA)
  }

  # Rename count column based on affix_type
  names(result_df)[names(result_df) == "count_value"] <- count_name

  if (filter_results) {
    result_df <- result_df[result_df$affix_found != "", ]
  }

  if (sort_results) {
    if (prioritise_caps) {
      result_df <- result_df[order(-result_df$starts_with_cap,
                                   if(affix_type == "suffix") -result_df[[count_name]] else result_df[[count_name]]), ]
    } else {
      result_df <- result_df[order(if(affix_type == "suffix") -result_df[[count_name]] else result_df[[count_name]]), ]
    }
  }

  rownames(result_df) <- NULL
  if(nrow(result_df)>0){
    return(result_df)
  } else {
    return(NA)
  }

}


#' get_unique_suffixes
#'
#' takes a dpdict and returns a vector of length nrow(dpdict) which is best-guess for best suffixes to add to each variable's question group label to make it unique
#'
#' wants to find suffix based on a common sep type and, if allow_unmatched_sep_count_from_end == FALSE, common position of sep relative to the end
#' if can't find a suffix in labels, looks in variable name
#' and if can't find a unique suffix anywhere else, just assigns successive numbers
#'
#' @param temp_dpdict a dpdict with cols for uniqueid (e.g. variable name), variable labels and question groups. Values in the `var_with_strings` column should be unique.
#' @param var_with_unique_id name of column to use as unique id (typically variable name)
#' @param var_with_strings name of column with strings to look for a suffix in (typically variable label)
#' @param var_with_question_groups name of column representing question groups of variables that should aim to find common suffix patterns within
#' @param seps_priority priority of separator strings to look for. separator strings determine what is a suffix (typically e.g. " - ")
#' @param allow_unmatched_sep_count_from_end whether to allow for suffixes that are identified by a sep in a different position relative to the end of the string
#' @param noisy either 0, 1, or 2. 1 and 2 provide increasingly noisy updates as the function works.
#'
#' @return A character vector with the same length and order as `temp_dpdict`,
#'         containing the determined suffix for each variable. Returns NA for
#'         single-variable groups where no suffix was identified by separators.
#'         Returns sequential numbers if no other unique suffix pattern was found
#' @export
#'
#' @examples
#' get_unique_suffixes(create_dict_with_metadata(get_big_test_dat()))
get_unique_suffixes <- function(temp_dpdict, var_with_unique_id = "variable_names", var_with_strings = "variable_labels", var_with_question_groups = "question_group", seps_priority = c("- ","_","."), allow_unmatched_sep_count_from_end = TRUE, noisy = 0){

  ### validation
  if (!is.data.frame(temp_dpdict)) {
    stop("`temp_dpdict` must be a data frame.")
  }
  stopifnot(
    "`var_with_unique_id` must be a single character string" = is.character(var_with_unique_id) && length(var_with_unique_id) == 1 && nzchar(var_with_unique_id),
    "`var_with_strings` must be a single character string" = is.character(var_with_strings) && length(var_with_strings) == 1 && nzchar(var_with_strings),
    "`var_with_question_groups` must be a single character string" = is.character(var_with_question_groups) && length(var_with_question_groups) == 1 && nzchar(var_with_question_groups)
  )
  required_cols <- c(var_with_unique_id, var_with_strings, var_with_question_groups)
  if (!all(required_cols %in% names(temp_dpdict))) {
    stop("`temp_dpdict` is missing required columns: ", paste(setdiff(required_cols, names(temp_dpdict)), collapse = ", "))
  }
  if (!is.character(seps_priority) || length(seps_priority) == 0) {
    stop("`seps_priority` must be a non-empty character vector.")
  }
  if (!is.logical(allow_unmatched_sep_count_from_end) || length(allow_unmatched_sep_count_from_end) != 1) {
    stop("`allow_unmatched_sep_count_from_end` must be a single logical value (TRUE/FALSE).")
  }
  if (!is.numeric(noisy) || length(noisy) != 1 || !noisy %in% c(0, 1, 2)) {
    stop("`noisy` must be a single numeric value: 0, 1, or 2.")
  }

  # reduce to just the columns we need
  small_dpdict <- temp_dpdict %>% dplyr::select(dplyr::all_of(c(var_with_unique_id, var_with_strings, var_with_question_groups)))

  # check that every label is unique - stops if not
  duplicated_labels <- unique(small_dpdict$variable_labels[duplicated(small_dpdict$variable_labels)])
  if(length(duplicated_labels) > 0){
    duplicates_table <- lapply(duplicated_labels, function(x) {
      paste(temp_dpdict[[var_with_unique_id]][temp_dpdict[[var_with_strings]] == x], collapse = " ")
    })
    for(i in 1:nrow(duplicates_table)){
      print(paste0(duplicates_table[i,1], " FOUND AT: ", duplicates_table[i,2]))}
    stop("get_unique_suffixes requires unique labels!")
  }

  # get a list of lists with possible suffixes for every variable, or an NA if no suffix is found
  unique_question_groups <- unique(temp_dpdict[[var_with_question_groups]])
  comparison_list <- lapply(unique_question_groups, function(question_group) {
    question_group_data <- temp_dpdict[temp_dpdict[[var_with_question_groups]] == question_group, ]
    question_group_list <- lapply(question_group_data[[var_with_strings]], get_affix_df, affix_type = "suffix", seps_priority = seps_priority, filter_results = FALSE)

    if (all(is.na(question_group_list))) {
      if (noisy >= 1) {
        print(paste0("No suffix found for question_group ", question_group, " e.g. ", question_group_data[[var_with_strings]][1]))
      }
    } else if (any(is.na(question_group_list))) {
      if (noisy >= 1) {
        na_example <- question_group_data[[var_with_strings]][is.na(question_group_list)][1]
        print(paste0("Suffixes found for some but not all variables in question_group ", question_group, " e.g. ", na_example))
      }
    }

    question_group_list
  })
  names(comparison_list) <- unique_question_groups

  # pre-allocate final output
  out <- vector("list", length(comparison_list))
  names(out) <- names(comparison_list)

  # we'll look for unique suffixes for each unique question_group
  for (question_group in names(comparison_list)) {
    if(noisy>=2){print(paste0("question_group ", question_group))}

    question_group_list <- comparison_list[[question_group]]

    # if there's only one variable in the question group we just take the best suffix if there is one, else default to NA
    if (length(question_group_list) == 1) {
      if (is.data.frame(question_group_list[[1]])) {
        if(noisy>=2){print("single variable, suffix found")}
        out[[question_group]] <- question_group_list[[1]]$affix_found[1]
      } else {
        if(noisy>=2){print("single variable, no suffix - default to NA")}
        out[[question_group]] <- NA
      }
    }

    # if multiple variables within a question group we'll need to work a bit harder. we want to find a pattern for suffixes that is common to the whole question group...
    if (length(question_group_list) >1){
      if(noisy>=2){print("multiple variables in question group...")}

      question_group_solved <- FALSE

      if (all(!sapply(question_group_list, is.na))) { # if all questions in question group have candidate suffixes
        # find number of possibilities for a common sep_type and sep_count_from_end across all variables in the same question group
        min_candidates <- min(sapply(question_group_list, nrow))
        # first, attempt filtering for both type of sep and sep in same place
        # start with the first candidate (j)
        for (j in seq_len(min_candidates)) {
           # store sep_type, sep_count_from_end from the first df (i.e. first variable in the question group), which we'll check subsequent variables against.
          candidate_sep_type <- question_group_list[[1]]$sep_type[j]
          candidate_sep_count_from_end <- question_group_list[[1]]$sep_count_from_end[j]
          candidate_suffixes <- question_group_list[[1]]$affix_found[j]

          # for each of the rest of the variables in the question group, check if any seps match the sep at j in the first variable in the question group
          for (k in 2:length(question_group_list)) {
            candidates_df <- question_group_list[[k]]
            # filter down the dataframe to only include qualifying candidates
            candidates_df <- candidates_df[candidates_df$sep_type == candidate_sep_type &
                                             candidates_df$sep_count_from_end == candidate_sep_count_from_end, ]

            if (nrow(candidates_df) == 0) {
              break
            } else {
              # start a vector of suffixes, which we'll add to as we go. (we'll then check for uniques once we've checked for all variables in question group)
              candidate_suffixes <- c(candidate_suffixes, candidates_df$affix_found)
            }
          }

          # once all variables in the question group checked, check if the candidates are all unique within the question group, and if so, append to our out list and break to continue onto the next question group
          if (length(unique(candidate_suffixes)) == length(question_group_list)) {
            if (noisy>=1) print(paste("SUCCESS! labels:", paste(candidate_suffixes, collapse = " ")))
            out[[question_group]] <- candidate_suffixes
            question_group_solved <- TRUE
            break
          }
        }

        # if not yet broken, and allow_unmatched_sep_count_from_end, try again, this time not filtering to match sep count from end
        if (!question_group_solved && allow_unmatched_sep_count_from_end) {
          for (j in seq_len(min_candidates)) {
            candidate_sep_type <- question_group_list[[1]]$sep_type[j]
            candidate_suffixes <- question_group_list[[1]]$affix_found[j]

            for (k in 2:length(question_group_list)) {
              candidates_df <- question_group_list[[k]][question_group_list[[k]]$sep_type == candidate_sep_type, ]
              if (nrow(candidates_df) == 0) {
                break
              } else {
                candidate_suffixes <- c(candidate_suffixes, candidates_df$suffix_found[1])
              }
            }

            if (length(unique(candidate_suffixes)) == length(question_group_list)) {
              if (noisy>=1) print(paste("SUCCESS! labels:", paste(candidate_suffixes, collapse = " ")))
              out[[question_group]] <- candidate_suffixes
              question_group_solved <- TRUE
              break
            }
          }
        }
      }

      # if still no qualifying suffixes found, tries to find in variable names
      if (!question_group_solved) {
        if (noisy>=1) print("no labelled solution - trying unique ids")
        question_group_list_uniqueids <- lapply(temp_dpdict[[var_with_unique_id]][temp_dpdict[[var_with_question_groups]] == question_group],
                                       get_affix_df, affix_type = "suffix", seps_priority = seps_priority, filter_results = FALSE)

        min_candidates <- tryCatch(min(sapply(question_group_list_uniqueids, nrow)), error = function(e) 0)

        for (j in seq_len(min_candidates)) {
          candidate_sep_type <- question_group_list_uniqueids[[1]]$sep_type[j]
          candidate_sep_count_from_end <- question_group_list_uniqueids[[1]]$sep_count_from_end[j]
          candidate_suffixes <- question_group_list_uniqueids[[1]]$suffix_found[j]

          for (k in 2:length(question_group_list_uniqueids)) {
            candidates_df <- question_group_list_uniqueids[[k]]
            candidates_df <- candidates_df[candidates_df$sep_type == candidate_sep_type &
                                             candidates_df$sep_count_from_end == candidate_sep_count_from_end, ]

            if (nrow(candidates_df) == 0) {
              break
            } else {
              candidate_suffixes <- c(candidate_suffixes, candidates_df$suffix_found)
            }
          }

          if (length(unique(candidate_suffixes)) == length(question_group_list_uniqueids)) {
            if (noisy>=1) print("SUCCESS! unique ids")
            out[[question_group]] <- candidate_suffixes
            question_group_solved <- TRUE
            break
          }
        }
      }

      if (!question_group_solved) {
        if (noisy>=1) print("still no uniques. returning arbitrary numbering")
        out[[question_group]] <- as.character(seq_along(question_group_list))
      }
    }
  }
  return(c(unlist(out), use.names = FALSE))
}

##### functions for updating metadata #####
#' update_dat_from_dpdict
#'
#' convenience function for updating all variable names, variable labels and value labels in a dat according to a dpdict
#'
#' @param x survey data object, or a survey data dataframe (a 'dat')
#' @param temp_dpdict dpdict related to the survey_obj or temp_dat
#'
#' @return version of survey_obj or temp_dat with variable names, variable labels and values labels updated according to temp_dpdict
#' @export
#'
#' @examples
#' # Using separate temp_dat and temp_dpdict
#' temp_dat <- get_minimal_labelled_test_dat()
#' temp_dpdict <- create_dict(temp_dat)
#' temp_dpdict$variable_names[1] <- "new_uid"
#' updated_dat <- update_dat_from_dpdict(temp_dat, temp_dpdict)
#'
#' # Using a survey_data object and a modified temp_dpdict
#' survey_obj <- create_survey_data(get_minimal_labelled_test_dat())
#' temp_dpdict <- survey_obj$dpdict
#' temp_dpdict$variable_names[1] <- "new_uid"
#' updated_survey_obj <- update_dat_from_dpdict(survey_obj, temp_dpdict)
update_dat_from_dpdict <- function(x, temp_dpdict = NULL){

  if(is.survey_data(x)){
    temp_dat <- x$dat
  } else if (is.data.frame(x)){
    temp_dat <- x
  } else {
    stopifnot("Must provide either a survey data object, or a temp_dat that is a data frame" = is.data.frame(temp_dat))
  }

  stopifnot("temp_dpdict must be a data frame" = is.data.frame(temp_dpdict))

  if (!all(c("old_variable_names", "variable_names", "variable_labels", "value_labels") %in% names(temp_dpdict))) {
    stop("temp_dpdict must contain 'old_variable_names', 'variable_names', 'variable_labels', and 'value_labels' columns")
  }

  # validate all current names exist dpdict
  name_mapping <- stats::setNames(
    temp_dpdict$variable_names,
    temp_dpdict$old_variable_names
  )

  missing_names <- setdiff(names(temp_dat), names(name_mapping))
  if (length(missing_names) > 0) {
    stop("Some variables in dat not found in dpdict old_variable_names: ",
         paste(missing_names, collapse = ", "))
  }

  dpdict_check <- validate_no_dpdict_duplicates(temp_dpdict, check_variable_names = TRUE,
                               check_variable_labels = TRUE,
                               check_alias_with_suffix = FALSE)
  if (!(dpdict_check)) {
    stop("Duplicate variable names, label or alias_with_suffix found in dpdict.")
  }

  # update with new variable names
  names(temp_dat) <- name_mapping[names(temp_dat)]

  # update variable labels using old names for matching
  new_labels <- vapply(
    temp_dpdict$old_variable_names,
    function(old_name) {
      temp_dpdict$variable_labels[temp_dpdict$old_variable_names == old_name]
    },
    character(1)
  )

  temp_dat <- mapply(sjlabelled::set_label,
                     temp_dat,
                     label = new_labels,
                     SIMPLIFY = FALSE)

  # update value labels using old names for matching
  value_labels_list <- temp_dpdict$value_labels[match(names(temp_dat), temp_dpdict$variable_names)]

  temp_dat <- mapply(
    function(x, labels) {
      if(!anyNA(labels)) {
        sjlabelled::set_labels(x, labels = labels, force.labels = TRUE)
      } else {
        x
      }
    },
    temp_dat,
    value_labels_list,
    SIMPLIFY = FALSE
  )

  temp_dat <- as.data.frame(temp_dat, stringsAsFactors = FALSE)

  if (is.survey_data(x)) {
    return(structure(list(dat = temp_dat, dpdict = temp_dpdict), class = "survey_data"))
  } else {
    return(temp_dat)
  }
}


#' get_questions_dict
#'
#' very simple function to get a questions_dict from a dpdict
#'
#' @param x a survey data object containing a dpdict, or just a dpdict, with columns for question-level metadata, e.g. questiontype, question_alias
#'
#' @return a questions_dict (subset of dpdict with one row per unique question group)
#' @export
#'
#' @examples
#' # Using a temp_dpdict
#' updated_dat <- get_questions_dict(create_dict_with_metadata(get_minimal_labelled_test_dat()))
#' # Using a survey_data object
#' updated_dat <- get_questions_dict(create_survey_data(get_minimal_labelled_test_dat()))
get_questions_dict <- function(x){
  if(is.survey_data(x)){
    temp_dpdict <- x$dpdict
  } else if (is.data.frame(x)){
    temp_dpdict <- x
  } else {
    stop("x must be either a survey data object or a dataframe representing a dpdict")
  }

  stopifnot("dpdict must at a minimum have a question group variable" = "question_group" %in% names(temp_dpdict))
  unique_indices <- !duplicated(temp_dpdict$question_group)
  question_level_columns <- names(temp_dpdict)[names(temp_dpdict) %in% c("question_group", "questiontype", "question_lcs", "question_alias", "question_description", "question_folder")]
  questions_dict <- temp_dpdict[unique_indices, question_level_columns]

  return(questions_dict)
}

#' update_aliases
#'
#' updates question_alias, question_description and alias_with_suffix in a dpdict, based on question_alias and question_suffix
#'
#' useful if e.g. manually editing alias in a questions_dict and want to apply that to a dpdict
#'
#' @param x a survey data object or a dpdict
#' @param questions_dict a questions_dict that must have columns for question_group and question_alias
#' and the same number of values as there are unique question_groups in temp_dpdict
#'
#' @return survey data object or a temp_dpdict with question_aias and alias_with_suffix updated based on the questions_dict
#' @export
#'
#' @examples
#' # Using a temp_dpdict
#' temp_dpdict <- create_dict_with_metadata(get_big_test_dat(n=10))
#' questions_dict <- get_questions_dict(temp_dpdict)
#' questions_dict$question_alias[questions_dict$question_alias == "labelledmultiordinal_a"] <-
#' "newlabelledmultiordinal_a"
#' updated_dpdict <- update_aliases(temp_dpdict, questions_dict)
#' # Using a survey_obj
#' survey_obj <- create_survey_data(get_big_test_dat(n=10))
#' questions_dict <- get_questions_dict(survey_obj)
#' questions_dict$question_alias[questions_dict$question_alias == "labelledmultiordinal_a"] <-
#' "newlabelledmultiordinal_a"
#' updated_survey_obj <- update_aliases(survey_obj, questions_dict)
update_aliases <- function(x, questions_dict){

  if(is.survey_data(x)){
    temp_dpdict <- x$dpdict
  } else if (is.data.frame(x)){
    temp_dpdict <- x
  } else {
    stop("x must be either a survey data object or a dataframe representing a dpdict")
  }

  if((!all(c("question_group", "question_alias") %in% names(questions_dict)) ||
     length(questions_dict$question_group) != length(unique(temp_dpdict$question_group)))){
    stop("Invalid questions_dict: missing required columns or mismatched length")
  }

  old_aliases <- temp_dpdict$question_alias

  # update question aliases from questions_dict
  temp_dpdict$question_alias <- questions_dict$question_alias[match(temp_dpdict$question_group, questions_dict$question_group)]

  temp_dpdict$question_description <- mapply(
    gsub,
    pattern = paste(sub("_[a-z]+$", "", old_aliases)),
    replacement = paste(sub("_[a-z]+$", "", temp_dpdict$question_alias)),
    x = temp_dpdict$question_description
  )

  temp_dpdict$alias_with_suffix <- ifelse(is.na(temp_dpdict$question_suffix),
                                          temp_dpdict$question_alias,
                                          paste0(temp_dpdict$question_alias, " - ", temp_dpdict$question_suffix))

  if (is.survey_data(x)) {
    return(structure(list(dat = x$dat, dpdict = temp_dpdict), class = "survey_data"))
  } else {
    return(temp_dpdict)
  }
}


#' split_grid_labels
#'
#' for expanding the question alias in a dpdict for grid questions, e.g. brand perceptions where a series of statements are rated for each of a series of brands
#'
#' For e.g. moving from question_alias == 'Q1. Brand Image', question_suffix == 'Brand 1 - Statement 1'...
#' to question_alias == 'Q1. Brand Image - Brand 1', question_suffix == 'Statement 1'
#' so that the common question alias is specific to each brand.
#' Does so based on an example 'stem' (e.g. 'Brand 1') and a count_before_repeat, i.e. the number of statements asked for each brand.
#' (Assumes the same number of statements exists for each brand.)
#'
#' @param x a survey data object or a dpdict
#' @param alias_to_split the question_alias in the temp_dpdict to change
#' @param sep the sep used in the question_suffix
#' @param example_stem_to_add as an example, the first stem currently in question_suffix that should be moved to question_alias
#' @param count_before_repeat the number of times the desired question_suffix after the stem has been moved repeats before the next stem
#'
#' @return an updated dpdict
#' @export
#'
#' @examples
#' # Using a temp_dpdict
#' temp_dpdict <- create_dict_with_metadata(get_big_test_dat(n=10))
#' split_grid_labels(temp_dpdict,
#'                   "labelledmultiresponsegrid_a", "statement 1 ", 2)
#' # Using a survey_obj
#' survey_obj <- create_survey_data(get_big_test_dat(n=10))
#' split_grid_labels(survey_obj,
#'                   "labelledmultiresponsegrid_a", "statement 1 ", 2)
split_grid_labels <- function(x, alias_to_split, example_stem_to_add, count_before_repeat, sep = " - "){

  # unpack surveydata
  if(is.survey_data(x)){
    temp_dpdict <- x$dpdict
  } else if (is.data.frame(x)){
    temp_dpdict <- x
  } else {
    stop("x must be either a survey data object or a dataframe representing a dpdict")
  }

  ### validation
  stopifnot(
    "`alias_to_split` must be a single character string" = is.character(alias_to_split) && length(alias_to_split) == 1 && nzchar(alias_to_split),
    "`example_stem_to_add` must be a single character string" = is.character(example_stem_to_add) && length(example_stem_to_add) == 1 && nzchar(example_stem_to_add),
    "`sep` must be a single character string" = is.character(sep) && length(sep) == 1 && nzchar(sep),
    "`count_before_repeat` must be a single positive integer" = is.numeric(count_before_repeat) && length(count_before_repeat) == 1 && count_before_repeat > 0 && count_before_repeat == round(count_before_repeat)
  )
  if (!"question_alias" %in% names(temp_dpdict) || !"question_suffix" %in% names(temp_dpdict)) {
    stop("`temp_dpdict` must contain 'question_alias' and 'question_suffix' columns.")
  }
  if (!alias_to_split %in% temp_dpdict$question_alias) {
    stop("`alias_to_split` ('", alias_to_split, "') not found in `temp_dpdict$question_alias`.")
  }
  if (count_before_repeat > sum(temp_dpdict$question_alias == alias_to_split)) {
    warning("`count_before_repeat` is greater than the number of variables with the specified `alias_to_split`.")
  }

  # extract stems
  stems <- any_gsub(
    gsub(example_stem_to_add, "", temp_dpdict$question_suffix[temp_dpdict$question_alias == alias_to_split][1:count_before_repeat]),
    "", temp_dpdict$question_suffix[temp_dpdict$question_alias == alias_to_split])

  # extract suffices
  suffix_with_sep <- any_gsub(unique(stems), "", temp_dpdict$question_suffix[temp_dpdict$question_alias == alias_to_split])
  suffix_without_sep <- any_gsub(sep, "", suffix_with_sep)

  # update dpdict
  temp_dpdict$question_suffix[temp_dpdict$question_alias == alias_to_split] <- suffix_without_sep
  temp_dpdict$question_alias[temp_dpdict$question_alias == alias_to_split] <- paste0(unname(temp_dpdict$question_alias[temp_dpdict$question_alias == alias_to_split]), sep, stems)

  if (is.survey_data(x)) {
    return(structure(list(dat = x$dat, dpdict = temp_dpdict), class = "survey_data"))
  } else {
    return(temp_dpdict)
  }
}

##### functions for checking metadata #####
#' validate_dat_dpdict_alignment
#'
#' simple checks that variable names and labels in dat and dpdict match and are aligned
#'
#' @param temp_dat survey data dataframe
#' @param temp_dpdict 'dpdict' to check for alignment with temp_dat
#' @param warn_only logical. if TRUE returns warnings instead of errors
#'
#' @return (invisibly) TRUE if passes all checks, else warnings or errors
#' @export
#'
#' @examples
#' temp_dat <- get_minimal_labelled_test_dat()
#' temp_dpdict <- create_dict(temp_dat)
#' validate_dat_dpdict_alignment(temp_dat, temp_dpdict)
validate_dat_dpdict_alignment <- function(temp_dat, temp_dpdict, warn_only = FALSE){

  issues <- character()

  # Check column count alignment
  if (ncol(temp_dat) != nrow(temp_dpdict)) {
    issues <- c(issues,
                sprintf("Number of columns in dat (%d) does not match number of rows in dpdict (%d)",
                        ncol(temp_dat), nrow(temp_dpdict)))
  }

  # Check for required columns in dpdict
  required_cols <- c("variable_names", "variable_labels")
  missing_cols <- required_cols[!required_cols %in% names(temp_dpdict)]
  if (length(missing_cols) > 0) {
    issues <- c(issues,
                sprintf("Required columns missing from dpdict: %s",
                        paste(missing_cols, collapse = ", ")))
  }

  # Only proceed with further checks if required columns exist
  if (length(missing_cols) == 0) {
    # Check variable names alignment
    mismatched_names <- setdiff(names(temp_dat), temp_dpdict$variable_names)
    if (length(mismatched_names) > 0) {
      issues <- c(issues,
                  sprintf("Variables in dat not found in dpdict variable_names: %s",
                          paste(mismatched_names, collapse = ", ")))

      # Check if these variables match old_variable_names
      if ("old_variable_names" %in% names(temp_dpdict)) {
        found_in_old <- mismatched_names %in% temp_dpdict$old_variable_names
        if (any(found_in_old)) {
          issues <- c(issues,
                      "Note: Some variables match old_variable_names in dpdict. Consider updating dat with dpdict.")
        }
      }
    }

    # Check variable labels alignment
    dat_labels <- sapply(temp_dat, function(x) attr(x, "label", exact = TRUE))
    mismatched_labels <- setdiff(dat_labels, temp_dpdict$variable_labels)
    if (length(mismatched_labels) > 0) {
      issues <- c(issues,
                  sprintf("Variable labels in dat not found in dpdict variable_labels: %s",
                          paste(mismatched_labels, collapse = ", ")))

      # Check if these labels match old_variable_labels
      if ("old_variable_labels" %in% names(temp_dpdict)) {
        found_in_old <- mismatched_labels %in% temp_dpdict$old_variable_labels
        if (any(found_in_old)) {
          issues <- c(issues, "Note: Some labels match old_variable_labels in dpdict. Consider updating dat with dpdict.")
        }
      }
    }
  }

  if (length(issues) > 0) {
    msg <- paste("Alignment issues found between dat and dpdict:",
                 paste(issues, collapse = "\n"),
                 sep = "\n")
    if (warn_only) {
      warning(msg)
      return(invisible(FALSE))
    } else {
      stop(msg)
    }
  }

  return(invisible(TRUE))
}


#' validate_no_dpdict_duplicates
#'
#' checks for unique values in variable names and labels, and, if the column exists, alias_with_suffix, in dpdict, reporting back with a message listing any duplicates
#'
#' returns(invisibly) a temp_dpdict filtered for duplicates, so it can be more easily viewed e.g. with View()
#'
#' @param temp_dpdict a dataframe, assumed to be survey data
#' @param check_variable_names logical. whether to check for and report on duplicate variable names.
#' @param check_variable_labels logical. whether to check for and report on duplicate variable labels.
#' @param check_alias_with_suffix logical. whether to check for and report on duplicate aliases with suffixes.
#' @param ignore_variable_name_from_label if TRUE, removes any cases of paste0(new_variable_name, variable_name_sep) from new_variable_label before working
#' for example, would remove "SC1: ", from "SC1: Country"
#' @param variable_name_sep specify the sep used to delineate variable name from label, e.g. ":" in "SC1: Country"
#' @param warn_only logical. if TRUE returns warnings instead of errors
#'
#' @return TRUE (invisibly) if no issues, else warnings or errors
#' @export
#'
#' @examples
#' validate_no_dpdict_duplicates(create_dict_with_metadata(get_big_test_dat(n=10)))
validate_no_dpdict_duplicates <- function(temp_dpdict,
                         check_variable_names = TRUE, check_variable_labels = TRUE, check_alias_with_suffix = TRUE,
                         ignore_variable_name_from_label = FALSE, variable_name_sep = ": ", warn_only = FALSE){

  if(ignore_variable_name_from_label == TRUE){
    temp_dpdict$variable_labels <- mapply(function(label, name) gsub(paste0(name, variable_name_sep), "", label),
                                          temp_dpdict$variable_labels, temp_dpdict$variable_names)
  }

  issues <- character()

  if(check_variable_names) {
    dupe_names <- temp_dpdict$variable_names[duplicated(temp_dpdict$variable_names)]
    if(length(dupe_names) > 0) {
      issues <- c(issues,
                  sprintf("Duplicate variable names found: %s",
                          paste(unique(dupe_names), collapse = ", ")))
    }
  }

  if(check_variable_labels) {
    dupe_labels <- temp_dpdict$variable_labels[duplicated(temp_dpdict$variable_labels)]
    if(length(dupe_labels) > 0) {
      issues <- c(issues,
                  sprintf("Duplicate variable labels found: %s",
                          paste(unique(dupe_labels), collapse = ", ")))
    }
  }

  if(check_alias_with_suffix && "alias_with_suffix" %in% names(temp_dpdict)) {
    dupe_aliases <- temp_dpdict$alias_with_suffix[duplicated(temp_dpdict$alias_with_suffix)]
    if(length(dupe_aliases) > 0) {
      issues <- c(issues,
                  sprintf("Duplicate aliases with suffix found: %s",
                          paste(unique(dupe_aliases), collapse = ", ")))
    }
  }

  if(length(issues) > 0) {
    msg <- paste("Duplicate values found in dpdict:",
                 paste(issues, collapse = "\n"),
                 sep = "\n")
    if(warn_only) {
      warning(msg)
      return(invisible(FALSE))
    } else {
      stop(msg)
    }
  }

  return(invisible(TRUE))
}


#' validate_variable_names
#'
#' Checks that variable names follow a valid pattern
#'
#' - Must start with a letter
#' - If contains underscore(s), each must be followed by a number
#' - Only letters and numbers allowed (other than underscores)
#'
#' @param names Character vector of variable names to validate
#' @param warn_only Logical, if TRUE returns warnings instead of errors
#'
#' @return Invisible TRUE if validation passes, otherwise errors or warnings
#' @export
#'
#' @examples
#' # Valid names
#' validate_variable_names(c("q1", "Q1", "SC1_1", "satisfaction_1", "SC1a_1", "SC1_1oe"))
#'
#' # Invalid names
#' \dontrun{
#' validate_variable_names(c("1q", "q_a", "q.1"))
#' }
validate_variable_names <- function(names, warn_only = FALSE) {
  valid_pattern <- "^[a-zA-Z][a-zA-Z0-9]*(_[0-9][a-zA-Z0-9]*)*$"

  invalid_names <- names[!grepl(valid_pattern, names)]

  if (length(invalid_names) > 0) {
    msg <- sprintf(
      "Found %d invalid variable names: %s",
      length(invalid_names),
      paste(utils::head(invalid_names, 5), collapse = ", ")
    )
    if (length(invalid_names) > 5) {
      msg <- paste0(msg, "...")
    }

    if (warn_only) {
      warning(msg)
      return(invisible(FALSE))
    } else {
      stop(msg)
    }
  }

  return(invisible(TRUE))
}

##### surveydata object definition #####

#' create_survey_data
#'
#' Creates a survey_data object, which combines survey data with its metadata.
#'
#' @param dat A data frame containing the survey data.
#' @param dpdict Optional. A data frame containing the metadata for the survey data. If not provided, it will be created automatically.
#' @return A survey_data object.
#' @export
#'
#' @examples
#' dat <- get_minimal_labelled_test_dat()
#' dpdict <- create_dict(dat)
#' survey_obj <- create_survey_data(dat, dpdict)
create_survey_data <- function(dat, dpdict = NULL) {
  # check that dat is a data frame
  if (!is.data.frame(dat)) {
    stop("'dat' must be a data frame")
  }

  # if dpdict is not provided, create it using create_dict_with_metadata
  if (is.null(dpdict)) {
    dpdict <- create_dict_with_metadata(dat)
  } else if (!is.data.frame(dpdict)) {
    stop("'dpdict' must be a data frame")
  }

  # use validate_dat_dpdict_alignment to verify alignment between dat and dpdict
  if (!validate_dat_dpdict_alignment(dat, dpdict, warn_only = TRUE)) {
    stop("Misalignment between dat and dpdict.")
  }

  # Use validate_no_dpdict_duplicates to verify dpdict integrity
  dpdict_check <- validate_no_dpdict_duplicates(dpdict, check_variable_names = TRUE,
                               check_variable_labels = TRUE,
                               check_alias_with_suffix = FALSE,
                               warn_only = TRUE)
  if (!(dpdict_check)) {
    stop("Duplicate variable names, label or alias_with_suffix found in dpdict.")
  }

  # Create the survey_data object
  structure(
    list(
      dat = dat,
      dpdict = dpdict
    ),
    class = "survey_data"
  )
}

#' is.survey_data
#'
#' Check if an object is a survey_data object
#'
#' @param x An object to check.
#' @return TRUE if the object is a survey_data object, FALSE otherwise.
#' @export
#'
#' @examples
#' dat <- get_minimal_labelled_test_dat()
#' survey_obj <- create_survey_data(dat)
#' is.survey_data(survey_obj)
is.survey_data <- function(x) {
  inherits(x, "survey_data")
}

#' validate_survey_data
#'
#' Validate a survey_data object
#'
#' @param x A survey_data object to validate.
#' @return TRUE if the object is valid, otherwise it throws an error with a description of the problem.
#' @export
#'
#' @examples
#' dat <- get_minimal_labelled_test_dat()
#' survey_obj <- create_survey_data(dat)
#' validate_survey_data(survey_obj)
validate_survey_data <- function(x) {
  if (!is.survey_data(x)) {
    stop("Object is not a survey_data object")
  }

  if (!is.data.frame(x$dat) || !is.data.frame(x$dpdict)) {
    stop("Both 'dat' and 'dpdict' must be data frames")
  }

  # Use validate_dat_dpdict_alignment to verify alignment
  if (!validate_dat_dpdict_alignment(x$dat, x$dpdict, warn_only = TRUE)) {
    stop("Misalignment between dat and dpdict.")
  }

  # Use validate_no_dpdict_duplicates to verify dpdict has unique variable names, variable labels, and, if the column exists, alias_with_suffix
  dpdict_check <- validate_no_dpdict_duplicates(x$dpdict, check_variable_names = TRUE,
                               check_variable_labels = TRUE,
                               check_alias_with_suffix = FALSE,
                               warn_only = TRUE)
  if (!(dpdict_check)) {
    stop("Duplicate variable names, label or alias_with_suffix found in dpdict.")
  }

  return(TRUE)
}


#' Print method for survey_data objects
#'
#' @param x A survey_data object
#' @param ... Additional arguments passed to print
#' @return x invisibly
#' @export
#' @examples
#' survey_obj <- create_survey_data(get_minimal_labelled_test_dat())
#' print(survey_obj)
print.survey_data <- function(x, ...) {
  # Validate input
  if (!is.survey_data(x)) {
    stop("Object must be of class 'survey_data'")
  }

  # Get dimensions and metadata counts
  n_obs <- nrow(x$dat)
  n_vars <- ncol(x$dat)
  n_groups <- length(unique(x$dpdict$question_group))

  # Get separator patterns from dpdict attributes
  seps <- attr(x$dpdict, "sep_patterns")
  if (is.null(seps)) {
    seps <- check_seps(x$dat)$separators
  }

  # Create output string
  cat("Survey data object:\n")
  cat(sprintf(" %d observations of %d variables with %d question groups\n",
              n_obs, n_vars, n_groups))

  cat("Separator patterns:\n")
  if (!is.na(seps["var_name_sep"])) {
    cat(sprintf(" Variable names: '%s' (e.g. Q1%s1)\n",
                seps["var_name_sep"], seps["var_name_sep"]))
  }
  if (!is.na(seps["prefix_sep"]) || !is.na(seps["statement_sep"])) {
    cat(" Variable labels:")
    if (!is.na(seps["prefix_sep"])) {
      cat(sprintf(" '%s' prefix", seps["prefix_sep"]))
    }
    if (!is.na(seps["statement_sep"])) {
      cat(sprintf("%s'%s' between statements",
                  if (!is.na(seps["prefix_sep"])) "," else "",
                  seps["statement_sep"]))
    }
    cat("\n")
  }

  invisible(x)
}


#' Subsetting method for survey_data objects
#'
#' @param x A survey_data object
#' @param i Row indices
#' @param j Column indices
#' @param drop If TRUE, returns a vector when only one column is selected
#' @return A new survey_data object containing only the selected data and corresponding metadata
#' @export
#' @examples
#' survey_obj <- create_survey_data(get_minimal_labelled_test_dat())
#' # Select first 3 rows
#' subset_rows <- survey_obj[1:3, ]
#' # Select specific columns
#' subset_cols <- survey_obj[, c("uid", "csat")]
`[.survey_data` <- function(x, i, j, drop = FALSE) {
  # Validate input
  if (!is.survey_data(x)) {
    stop("Object must be of class 'survey_data'")
  }

  # Handle missing i or j
  if (missing(i)) i <- seq_len(nrow(x$dat))
  if (missing(j)) j <- seq_len(ncol(x$dat))

  # Convert column names to indices if necessary
  if (is.character(j)) {
    j <- match(j, names(x$dat))
    if (any(is.na(j))) {
      stop("Unknown column names: ",
           paste(j[is.na(j)], collapse = ", "))
    }
  }

  # Validate numeric indices
  if (is.numeric(j)) {
    if (any(j > ncol(x$dat)) || any(j < 1)) {
      stop("Column subscript out of bounds")
    }
  }

  # Subset the data
  new_dat <- x$dat[i, j, drop = FALSE]

  # Get the variable names that were selected
  selected_vars <- names(new_dat)

  # Subset the metadata
  new_dpdict <- x$dpdict[x$dpdict$variable_names %in% selected_vars, ]

  # Create new survey_data object
  structure(
    list(
      dat = new_dat,
      dpdict = new_dpdict
    ),
    class = "survey_data"
  )
}

#' Single column extraction method for survey_data objects
#'
#' @param x A survey_data object
#' @param i Name or index of the column to extract
#' @return The selected column from the data
#' @export
#' @examples
#' survey_obj <- create_survey_data(get_minimal_labelled_test_dat())
#' # Extract a column by name
#' uid_vector <- survey_obj[["uid"]]
#' # Extract a column by position
#' first_column <- survey_obj[[1]]
`[[.survey_data` <- function(x, i) {
  if (!is.survey_data(x)) {
    stop("Object must be of class 'survey_data'")
  }

  # Handle numeric indices
  if (is.numeric(i)) {
    if (i > ncol(x$dat) || i < 1) {
      stop("Column subscript out of bounds")
    }
  }

  # Handle character indices
  if (is.character(i) && !(i %in% names(x$dat))) {
    stop("Unknown column name: ", i)
  }

  x$dat[[i]]
}


#' Filter method for survey_data objects
#'
#' Subsets rows of the survey data based on conditions, preserving metadata.
#'
#' @param .data A survey_data object.
#' @param ... Filter conditions passed to dplyr::filter.
#' @importFrom dplyr filter
#' @importFrom rlang enquos !!!
#' @return A new survey_data object with filtered data and unchanged metadata.
#' @export
#' @examples
#' survey_obj <- create_survey_data(get_minimal_labelled_test_dat())
#' # Filter rows where uid > 5
#' filtered <- dplyr::filter(survey_obj, uid > 5)
filter.survey_data <- function(.data, ...) {
  if (!is.survey_data(.data)) stop("'.data' must be a survey_data object")

  # Capture the expressions with quosures to maintain the correct environment
  dots <- rlang::enquos(...)

  new_dat <- dplyr::filter(.data$dat, !!!dots)
  structure(
    list(
      dat = new_dat,
      dpdict = .data$dpdict
    ),
    class = "survey_data"
  )
}

#' Select method for survey_data objects
#'
#' Selects columns from the survey data, updating metadata to reflect selections and renames.
#'
#' @param .data A survey_data object.
#' @param ... Column selections passed to dplyr::select (supports renaming).
#' @importFrom dplyr select
#' @importFrom tidyselect eval_select
#' @importFrom rlang expr
#' @return A new survey_data object with selected columns and updated metadata.
#' @exportS3Method dplyr::select survey_data
#' @examples
#' survey_obj <- create_survey_data(get_minimal_labelled_test_dat())
#' # Select specific columns
#' selected <- dplyr::select(survey_obj, uid, csat)
#' # Select and rename
#' renamed <- dplyr::select(survey_obj, user_id = uid, satisfaction = csat)
select.survey_data <- function(.data, ...) {
  if (!is.survey_data(.data)) stop("'.data' must be a survey_data object")

  # Evaluate selection expressions
  selected <- tidyselect::eval_select(rlang::expr(c(...)), .data$dat)
  new_names <- names(selected)
  positions <- selected
  original_names <- names(.data$dat)[positions]

  # Create mapping for renames
  name_mapping <- stats::setNames(new_names, original_names)

  # Subset and rename data
  new_dat <- .data$dat[, positions, drop = FALSE]
  names(new_dat) <- new_names

  # Update dpdict: filter to selected variables, apply renames, and reorder
  new_dpdict <- .data$dpdict[.data$dpdict$variable_names %in% original_names, ]
  new_dpdict$variable_names <- as.character(name_mapping[new_dpdict$variable_names])
  new_dpdict <- new_dpdict[match(new_names, new_dpdict$variable_names), ]

  structure(
    list(
      dat = new_dat,
      dpdict = new_dpdict
    ),
    class = "survey_data"
  )
}

#' Create a labeled value for use within mutate.survey_data
#'
#' @param x The value expression
#' @param label The variable label to apply
#' @return The value with an attribute indicating the desired label
#' @export
#' @examples
#' # Used within mutate.survey_data for labeling new variables
#' survey_obj <- create_survey_data(get_minimal_labelled_test_dat())
#' result <- dplyr::mutate(survey_obj, new_var = with_label(uid * 2, "Doubled ID"))
with_label <- function(x, label) {
  attr(x, "variable_label") <- label
  x
}

#' Adds or modifies columns in the survey data, automatically updating metadata for new variables
#' using update_dict_with_metadata. Also preserves appropriate metadata for modified variables.
#' Supports custom variable labels using the with_label() function.
#'
#' @param .data A survey_data object.
#' @param ... Mutation expressions passed to dplyr::mutate. Variable labels can be specified
#'   using with_label(expr, "label").
#' @importFrom dplyr mutate
#' @importFrom rlang enquos
#' @return A new survey_data object with mutated data and updated metadata.
#' @exportS3Method dplyr::mutate survey_data
#' @examples
#' # Add a new variable with a custom label
#' survey_obj <- create_survey_data(get_minimal_labelled_test_dat())
#' survey_obj %>%
#'   dplyr::mutate(new_var = with_label(survey_obj$dat$uid + survey_obj$dat$uid, "uid squared"))
#'
#' # Modify an existing variable with a new label
#' survey_obj %>%
#'   dplyr::mutate(uid = with_label(uid * 2, "uid doubled"))
mutate.survey_data <- function(.data, ...) {
  if (!is.survey_data(.data)) {
    stop("'.data' must be a survey_data object")
  }

  # Get expressions to detect which variables are being modified
  dots <- rlang::enquos(...)
  modified_vars <- names(dots)

  # Store original data for comparison
  original_names <- names(.data$dat)
  original_dat <- .data$dat

  # Create environment with with_label function
  with_label_fn <- function(x, label) {
    attr(x, "variable_label") <- label
    x
  }

  # Perform the mutation
  new_dat <- dplyr::mutate(original_dat, ...)

  new_names <- names(new_dat)

  # Identify new variables (not in original dataset)
  new_vars <- setdiff(new_names, original_names)

  # Identify existing variables that were modified
  changed_vars <- modified_vars[modified_vars %in% original_names]

  # Process labels for new and modified variables
  custom_labels <- list()

  # Extract custom labels from attributes created by with_label
  for (var in c(new_vars, changed_vars)) {
    if (!is.null(attr(new_dat[[var]], "variable_label"))) {
      custom_labels[[var]] <- attr(new_dat[[var]], "variable_label")
      # Remove the temporary attribute
      attr(new_dat[[var]], "variable_label") <- NULL
    }
  }

  # Set default labels for new variables that don't have custom labels
  if (length(new_vars) > 0) {
    for (var in new_vars) {
      if (!var %in% names(custom_labels)) {
        # Use variable name as the default label
        attr(new_dat[[var]], "label") <- var
      } else {
        # Apply custom label
        attr(new_dat[[var]], "label") <- custom_labels[[var]]
      }
    }
  }

  # Apply custom labels to modified variables
  if (length(changed_vars) > 0) {
    for (var in changed_vars) {
      if (var %in% names(custom_labels)) {
        attr(new_dat[[var]], "label") <- custom_labels[[var]]
      }
    }
  }

  # Start with existing dpdict
  existing_dpdict <- .data$dpdict

  # Handle new variables if there are any
  if (length(new_vars) > 0) {
    # Initialize new rows with basic info
    new_rows <- data.frame(
      variable_names = new_vars,
      variable_class = sapply(new_dat[new_vars], function(x) paste(class(x), collapse = ", ")),
      stringsAsFactors = FALSE
    )

    # Add other columns with appropriate defaults
    other_cols <- setdiff(names(existing_dpdict), c("variable_names", "variable_class"))
    for (col in other_cols) {
      if (col == "variable_labels") {
        # Use custom labels where available, otherwise use variable name
        new_rows[[col]] <- sapply(new_vars, function(var) {
          if (var %in% names(custom_labels)) {
            custom_labels[[var]]
          } else {
            var
          }
        })
      } else if (col == "question_group") {
        new_rows[[col]] <- NA_character_  # Empty string as placeholder
      } else {
        # Initialize with NA of appropriate type based on the column
        col_type <- class(existing_dpdict[[col]])
        if ("logical" %in% col_type) {
          new_rows[[col]] <- NA
        } else if ("character" %in% col_type) {
          new_rows[[col]] <- NA_character_
        } else if ("numeric" %in% col_type || "integer" %in% col_type) {
          new_rows[[col]] <- NA_real_
        } else {
          new_rows[[col]] <- NA
        }
      }
    }

    # Combine with existing dpdict
    temp_dpdict <- rbind(existing_dpdict, new_rows)

  } else {
    temp_dpdict <- existing_dpdict
  }

  # Update variable_class and labels for changed variables
  if (length(changed_vars) > 0) {
    for (var in changed_vars) {
      # Update class
      temp_dpdict$variable_class[temp_dpdict$variable_names == var] <-
        paste(class(new_dat[[var]]), collapse = ", ")

      # Update label if a custom one was provided
      if (var %in% names(custom_labels)) {
        temp_dpdict$variable_labels[temp_dpdict$variable_names == var] <- custom_labels[[var]]
      }
    }
  }

  # Create variables_to_update logical vector
  vars_to_update <- rep(FALSE, nrow(temp_dpdict))
  for (var in c(new_vars, changed_vars)) {
    vars_to_update[temp_dpdict$variable_names == var] <- TRUE
  }

  # Update metadata for new and changed variables
  if (any(vars_to_update)) {
    updated_dpdict <- update_dict_with_metadata(
      survey_obj = NULL,
      temp_dat = new_dat,
      temp_dpdict = temp_dpdict,
      variables_to_update = vars_to_update
    )
  } else {
    updated_dpdict <- temp_dpdict
  }

  # Ensure dpdict order matches new_dat
  new_dpdict <- updated_dpdict[match(new_names, updated_dpdict$variable_names), ]

  # Generate informative messages
  if (length(new_vars) > 0) {
    labeled_new <- new_vars[new_vars %in% names(custom_labels)]
    if (length(labeled_new) > 0) {
      message("New variables added with custom labels: ",
              paste(labeled_new, collapse = ", "))
    }
    unlabeled_new <- new_vars[!new_vars %in% names(custom_labels)]
    if (length(unlabeled_new) > 0) {
      message("New variables added with default labels: ",
              paste(unlabeled_new, collapse = ", "))
    }
  }

  if (length(changed_vars) > 0) {
    labeled_changed <- changed_vars[changed_vars %in% names(custom_labels)]
    if (length(labeled_changed) > 0) {
      message("Existing variables modified with new labels: ",
              paste(labeled_changed, collapse = ", "))
    }
    unlabeled_changed <- changed_vars[!changed_vars %in% names(custom_labels)]
    if (length(unlabeled_changed) > 0) {
      message("Existing variables modified with preserved labels: ",
              paste(unlabeled_changed, collapse = ", "))
    }
  }

  # Return updated survey_data object
  structure(
    list(
      dat = new_dat,
      dpdict = new_dpdict
    ),
    class = "survey_data"
  )
}
