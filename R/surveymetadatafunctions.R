#' datamap
#' creates a dataframe with variable names, labels and other useful info as rows (SPSS style)
#' useful for quickly understanding structure of a large dataset
#' note: order of value labels and order of unique values does not necessarily correspond
#'
#' @param temp_dat a dataframe, assumed to be survey data
#' @param view_or_return calls View by default - or if "save", return the summary as a dataframe
#'
#' @return calls View by default - or if "return", return the summary
#' @export
#'
#' @examples
#' datamap(get_basic_test_dat(100), view_or_return = "return")
datamap <- function(temp_dat, view_or_return = "view"){
  match.arg(view_or_return, c("view", "return"))

  if (view_or_return == "view" && !(interactive())) {
    view_or_return <- "return"
    warning("View only possible in RStudio. Will instead return the summary")
  }

  n_head <- 5

  out <- dplyr::tibble(`variable` = names(temp_dat),
                    `variable label` = vapply(temp_dat, function(x) attr(x, "label", exact = TRUE), FUN.VALUE = character(1), USE.NAMES = FALSE),
                    `class` = vapply(temp_dat, function(x) paste(class(x), collapse = ", "), FUN.VALUE = character(1), USE.NAMES = FALSE),
                    `n missing` = vapply(temp_dat, function(x) sum(is.na(x)), FUN.VALUE = integer(1), USE.NAMES = FALSE),
                    `n unique` = vapply(temp_dat, function(x) length(unique(x)), FUN.VALUE = integer(1), USE.NAMES = FALSE),
                    `value labels` = vapply(temp_dat, function(x) paste(names(attr(x, "labels", exact = TRUE)), collapse = ", "), FUN.VALUE = character(1), USE.NAMES = FALSE),
                    `first values` = vapply(temp_dat, function(x) paste(utils::head(x, n_head), collapse = ", "), FUN.VALUE = character(1), USE.NAMES = FALSE),
                    `first unique values` = vapply(temp_dat, function(x) paste(utils::head(unique(x), n_head), collapse = ", "), FUN.VALUE = character(1), USE.NAMES = FALSE),
                    )

  if (view_or_return == "view"){
    return(View(out, paste0("datamap ", deparse(substitute(temp_dat)))))
  }
  else if (view_or_return == "return"){
    return(out)
  }
}

#' create_dict
#' creates a tibble to use as a dictionary to easily update variable names and labels
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

  stopifnot("temp_dat must be a dataframe" = is.data.frame(temp_dat))

  temp_dpdict <- dplyr::tibble("old_variable_names" = names(temp_dat),
                      "old_variable_labels" = vapply(temp_dat, function(x) sjlabelled::get_label(x, def.value = NA_character_), FUN.VALUE = character(1), USE.NAMES = FALSE), # set def.value to return NA instead of NULL if no label found
                      "old_value_labels" = lapply(temp_dat, function(x) sjlabelled::get_labels(x, values = "as.name")),
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

#' check_dat_dpdict_alignment
#' simple checks that dat and dpdict are in a suitable and aligned format
#' by default, prints report of results
#'
#' @param temp_dat survey data dataframe
#' @param temp_dpdict 'dpdict' to check for alignment with temp_dat
#' @param report whether to print a report or not
#'
#' @return TRUE if passes all checks
#' @export
#'
#' @examples
#' temp_dat <- get_minimal_labelled_test_dat()
#' temp_dpdict <- create_dict(temp_dat)
#' check_dat_dpdict_alignment(temp_dat, temp_dpdict)
check_dat_dpdict_alignment <- function(temp_dat, temp_dpdict, report = TRUE){

  matching_ncol <- ifelse(ncol(temp_dat)==nrow(temp_dpdict), TRUE, FALSE)
  dpdict_has_variable_names <- ifelse("variable_names" %in% names(temp_dpdict), TRUE, FALSE)
  dpdict_has_variable_labels <- ifelse("variable_labels" %in% names(temp_dpdict), TRUE, FALSE)

  dat_matches_new_names <- ifelse(all(names(temp_dat) %in% temp_dpdict$variable_names), TRUE, FALSE)
  dat_matches_new_labels <- ifelse(all(sjlabelled::get_label(temp_dat) %in% temp_dpdict$variable_labels), TRUE, FALSE)

  if(report == TRUE){
    ifelse(matching_ncol, print("[/] ncol matched"), print("[x] WARNING: dat ncol != to dpdict ncol"))
    ifelse(dpdict_has_variable_names & dpdict_has_variable_labels, print("[/] dpdict has variable names and labels columns"), print("[x] WARNING: dpdict is missing variable_names or variable_labels colunmns"))
    ifelse(dat_matches_new_names, print("[/] dat variable names found in dpdict"), print("[x] WARNING: dat names not found in dpdict variable_names"))
    ifelse(dat_matches_new_labels, print("[/] dat variable labels found in dpdict"), print("[x] MISMATCH: dat variable labels not found in dpdict variable_labels"))

    if(dat_matches_new_names == FALSE & ("old_variable_names" %in% names(temp_dat))){
      dat_matches_old_names <- ifelse(all(names(temp_dat) %in% temp_dpdict$old_variable_names), TRUE, FALSE)
      dat_matches_old_names_not_new_names <- ifelse(dat_matches_old_names &
                                                      !all(names(temp_dat) %in% temp_dpdict$variable_names),
                                                    TRUE, FALSE)
      ifelse(dat_matches_old_names_not_new_names, print("! dat names found in dpdict old_variable_names but not variable_names. Consider updating dat with dpdict"))
    }
    if(dat_matches_new_labels == FALSE & ("old_variable_labels" %in% names(temp_dat))){
      dat_matches_old_labels <- ifelse(all(sjlabelled::get_label(temp_dat) %in% temp_dpdict$old_variable_labels), TRUE, FALSE)
      dat_matches_old_labels_not_new_labels <- ifelse(dat_matches_old_labels &
                                                        !all(sjlabelled::get_label(temp_dat) %in% temp_dpdict$variable_labels),
                                                      TRUE, FALSE)
      ifelse(dat_matches_old_labels_not_new_labels, print("! dat variable labels found in dpdict old_variable_labels but not variable_labels. Consider updating dat with dpdict"))
    }
  }

  checks_successful <- ifelse(matching_ncol & dpdict_has_variable_names & dpdict_has_variable_labels &
                                dat_matches_new_names & dat_matches_new_labels,
                              TRUE, FALSE)
  return(checks_successful)
}

#' update_dat_from_dpdict
#' convenience function for updating all variable names, variable labels and value labels in a dat according to a dpdict
#'
#' @param temp_dat survey data dataframe
#' @param temp_dpdict dpdict related to temp_dat
#'
#' @return version of temp_dat with variable names, variable labels and values labels updated according to temp_dpdict
#' @export
#'
#' @examples
#' temp_dat <- get_minimal_labelled_test_dat()
#' temp_dpdict <- data.frame(
#'   old_variable_names = c("uid", "csat", "nps_response"),
#'   old_variable_labels = c("uid", "Satisfaction", "NPS response"),
#'   variable_names = c("uid", "customer_satisfaction", "nps_rating"),
#'   variable_labels = c("uid", "Customer Satisfaction Score", "NPS rating (0-10)"),
#'   stringsAsFactors = FALSE
#' )
update_dat_from_dpdict <- function(temp_dat, temp_dpdict){

  stopifnot("New variable names must be unique" = length(unique(temp_dpdict$variable_names)) == length(temp_dpdict$variable_names))
  stopifnot("New variable labels must be unique" = length(unique(temp_dpdict$variable_labels)) == length(temp_dpdict$variable_labels))

  temp_dat <- stats::setNames(temp_dat, temp_dpdict$variable_names)

  new_labels <- vapply(names(temp_dat), function(i) query_dict(temp_dpdict, "variable_names", i, "variable_labels")[[1]][[1]], character(1))
  temp_dat <- mapply(sjlabelled::set_label, temp_dat, label = new_labels, SIMPLIFY = FALSE)

  value_labels_list <- vapply(names(temp_dat), function(i) query_dict(temp_dpdict, "variable_names", i, "value_labels")[[1]][[1]], list(1))
  temp_dat <- mapply(function(x, labels) if(!anyNA(labels)) sjlabelled::set_labels(x, labels = labels, force.labels = TRUE) else x, temp_dat, value_labels_list, SIMPLIFY = FALSE)

  return(temp_dat)
}

#' create_dict_with_metadata
#' creates a dpdict with additional metadata fields for question group, variable class and checks for single variable question, dichotomous variable, variable with value labels, and multiresponse variable, which are in turn used to define questiontype
#' these are all calculated automatically based on function arguments
#' then additionally creates fields for question_alias, question_description, question_suffix, alias_with_suffix, and question_folder
#' question_alias is intended as a user-defined identifier for the question but takes the values of question_group by default
#' question_description, question_suffix, and alias_with_suffix are useful when creating tables and visualisations
#' suffixes are identified using question_lcs
#' requires: split_into_question_groups()
#' most arguments are settings for split_into_question_groups()
#'
#' notes:
#' - identifies categorical and multiresponse ('select all') variables by the presence of value labels. i.e. if a variable has class labelled but not value labels, it will not be given questiontype multiresponse, categorical or categorical array. (usually it will be a numeric or multinumeric)
#' - findlongest=TRUE, with splitbycommonlabel=TRUE takes significantly longer (as it attempts to split by analysing strings). may not be necessary for some processing - if correct common label is always the string prior to label_sep.
#' - variable labels should be hierarchically structured, with unique statements at the end. e.g. "Q1. Question text - Attitude statement - answer code"
#' - checking for multiresponse variables can also takes a while but is required.
#' - if last variable in a question in returned dict just keeps its full string, min_common_strings may be set too high
#'
#' @param temp_dat survey data dataframe
#' @param variable_name_sep "_" by default. string that separates each variable within a common question, e.g. if Q1_1, Q1_2 "_" is the correct variable_name_sep.
#' @param splitbyclass boolean. if true, every successive new class is given a new unique suffix
#' @param splitbynumlabelledvalues boolean. if true, each variable with a different number of labelled values is given a new unique suffix (because we would expect multiple variables in the same question to have the same number of options, e.g. battery of likert scales)
#' @param splitbynoncontiguous boolean. if true, variables with the same variable name prefix that are not located adjacent to each other are given a new unique suffix
#' @param splitbycommonlabel boolean. if true, each variable for which the longest common string within the variable label for that variable does not match the longest common string within the question group as a whole, is given a new suffix
#' @param findlongest boolean. if true, applies splitbycommonlabel by looking for the longest common substring. (this is computationally expensive.) if false, takes substring preceding labelsep as the commonlabel.
#' @param variable_compare_mode either "complete" or "reduced'. setting for when findlongest == TRUE. "complete" is computationally cheaper. if "complete", compares within the entire question group to find lcs for each statement. if "reduced", only looks at the variables before and after the variable in question, according to consecutive_range (a parameter in split_into_question_groups)
#' @param min_common_strings integer. setting for when findlongest == TRUE. looks for substrings common to at least min_common_strings other variables in the question group and will take from this subset the longest string.
#' @param labelsep string. setting for when findlongest == FALSE, in which case uses the substring in the variable name prior to labelsep as commonlabel.
#' @param edit_aliases boolean. whether to allow user to manually edit question_aliases using data_edit before creating alias_with_suffix. (question_folder can be defined at the same time)
#' @param noisy integer between 1 and 4. 1 by default. if 1, signals start of attempts to find question groups, and multiresponse. if noisy == 2 provides timing and updates within question groups and multiresponse hunts. noisy > 2 has settings within function for finding question groups.
#'
#' @return a tibble with metadata for temp_dat.
#' @export
#'
#' @examples create_dict_with_metadata(get_big_test_dat(n=10))
create_dict_with_metadata <- function(temp_dat, variable_name_sep = "_",
                                      splitbyclass = TRUE, splitbynumlabelledvalues = TRUE, splitbynoncontiguous = TRUE, splitbycommonlabel = TRUE,
                                      findlongest = TRUE, variable_compare_mode = "reduced", min_common_strings = 3, labelsep = " - ",
                                      edit_aliases = FALSE,
                                      noisy = 0){

  temp_dpdict <- create_dict(temp_dat, prefill = TRUE) # create fresh basic dpdict - but we don't need variables for 'old' names and labels
  temp_dpdict <- temp_dpdict[, !(names(temp_dpdict) %in% c("old_variable_names", "old_variable_labels", "old_value_labels"))]

  if(noisy >= 1){
    print("Attempting to split into question groups...")
    if(noisy >= 2){start_time <- proc.time()}
  }
  # split into question groups. there are various settings for this, particularly for if attempting to split by commonlabel
  temp_dpdict <- split_into_question_groups(temp_dpdict, temp_dat, variable_name_sep = variable_name_sep, ignorelabelbeforeprefix = TRUE, prefix_sep = ": ",
                                          splitbyclass = splitbyclass, splitbynumlabelledvalues = splitbynumlabelledvalues, splitbynoncontiguous = splitbynoncontiguous, splitbycommonlabel = splitbycommonlabel,
                                          labelsep = labelsep, findlongest = findlongest, min_lcs_length = 10, min_common_strings = min_common_strings, consistent_consecutive_mode = FALSE, consecutive_range = 10, variable_compare_mode = variable_compare_mode,
                                          noisy = noisy)
  if(noisy >= 2){print(proc.time() - start_time)}

  temp_dpdict$variable_class <- vapply(temp_dpdict$variable_names, function(x) paste(class(temp_dat[[x]]), collapse = ", "), character(1))
  temp_dpdict$singlevariablequestion <- vapply(temp_dpdict$question_group, function(x) sum(temp_dpdict$question_group == x) == 1, logical(1))
  # counts value *labels*, rather than values, to determine whether a dichotomous variable, so e.g. if there are two value labels, for 'Selected' and for 'Not selected', even if no values correspond to 'Not selected' in reality, it will still be considered dichotomous
  temp_dpdict$dichotomousvariable <- vapply(temp_dpdict$variable_names, function(x) length(sjlabelled::get_labels(temp_dat[[x]])) == 2, logical(1))
  temp_dpdict$has_value_labels <- vapply(temp_dpdict$value_labels, function(x) !all(is.na(x)), logical(1))

  # identifying multiresponse: for each variable in temp_dpdict, pass the question group, then check within all variables with the same question group in the dat. if more than 1 variable in the question group that has at least one value that is not 0 or NA, then it's a multiresponse.
  if(noisy >= 1){
    print("Checking for multiresponse variables")
    if(noisy >=2){start_time <- proc.time()}
  }

  temp_dpdict$multiresponse <- logical(nrow(temp_dpdict))
  for(i in seq_len(nrow(temp_dpdict))){
    if(noisy >= 2){print(paste0("Checking ", temp_dpdict$variable_names[i], " for multiresponse"))}

    set_of_variable_names_in_question_group <- temp_dpdict$variable_names[temp_dpdict$question_group == temp_dpdict$question_group[i]]
    question_group_within_dat <- temp_dat[names(temp_dat) %in% set_of_variable_names_in_question_group]

    count_within_question_group <- vapply(question_group_within_dat, function(x) any(!is.na(x) & x != 0), logical(1)) # within each variable in question group, check that there is at least one value that is not NA or 0

    if(sum(count_within_question_group) >= 1){
      temp_dpdict$multiresponse[temp_dpdict$variable_names %in% set_of_variable_names_in_question_group] <- TRUE
      i <- i + length(set_of_variable_names_in_question_group)
    } else {
      temp_dpdict$multiresponse[i] <- FALSE
      i <- i + 1
    }
  }

  if(noisy >=2){print(proc.time() - start_time)}

  temp_dpdict$questiontype <- with(temp_dpdict, dplyr::case_when(
    ((grepl("numeric", variable_class) | grepl("integer", variable_class) | grepl("double", variable_class)) & (singlevariablequestion == TRUE)) ~ "numeric",
    ((grepl("numeric", variable_class) | grepl("integer", variable_class) | grepl("double", variable_class)) & (multiresponse == TRUE)) ~ "multinumeric",
    (grepl("POSIXct|POSIXt|Date", variable_class)) ~ "date",
    variable_class == "difftime" ~ "difftime",
    variable_class == "character" ~ "text",
    ((variable_class == "factor") & (multiresponse == FALSE)) ~ "categorical",
    (has_value_labels == TRUE & (multiresponse == FALSE)) ~ "categorical",
    variable_class == "logical" ~ "categorical",
    ((variable_class == "factor") & (dichotomousvariable == TRUE) & (multiresponse == TRUE)) ~ "multiresponse",
    (has_value_labels == TRUE & (dichotomousvariable == TRUE) & (multiresponse == TRUE)) ~ "multiresponse",
    ((variable_class == "factor") & (dichotomousvariable == FALSE) & (multiresponse == TRUE)) ~ "categorical array",
    (has_value_labels == TRUE & (dichotomousvariable == FALSE) & (multiresponse == TRUE)) ~ "categorical array",
    .default = NA
  ))

  if(any(is.na(temp_dpdict$questiontype))){
    warning(paste(c("The following variables do not have a defined question type:", unlist(temp_dpdict$variable_names[is.na(temp_dpdict$questiontype)])), collapse = "\n"))
  }

  unique_indices <- !duplicated(temp_dpdict$question_group)
  questions_dict <- temp_dpdict[unique_indices, c("question_group", "question_lcs")]
  # questions_dict <- extract_common_labels_by_question(temp_dpdict, "variable_labels", editfirst = FALSE)

  questions_dict$question_alias <- questions_dict$question_group
  questions_dict$question_description <- ifelse(questions_dict$question_group == "",
                                                questions_dict$question_lcs,
                                                paste(sub("_.*", "", questions_dict$question_group), questions_dict$question_lcs, sep = ". "))

  questions_dict$question_folder <- NA

  if(edit_aliases == TRUE){
    questions_dict <- DataEditR::data_edit(questions_dict, viewer_height = 900, viewer_width = 1800)
  } else {
    questions_dict$question_alias <- questions_dict$question_group
  }

  temp_dpdict$question_alias <- questions_dict$question_alias[match(temp_dpdict$question_group, questions_dict$question_group)]
  temp_dpdict$question_description <- questions_dict$question_description[match(temp_dpdict$question_group, questions_dict$question_group)]
  temp_dpdict$question_folder <- questions_dict$question_folder[match(temp_dpdict$question_group, questions_dict$question_group)]

  question_suffixes <- get_unique_suffixes(temp_dpdict, var_with_unique_id = "variable_names",
                                           var_with_strings = "variable_labels",
                                           var_with_question_groups = "question_group", seps_priority = c("- "))

  temp_dpdict$question_suffix <- question_suffixes
  temp_dpdict$alias_with_suffix <- ifelse(is.na(temp_dpdict$question_suffix),
                                          temp_dpdict$question_alias,
                                          paste0(temp_dpdict$question_alias, " - ", temp_dpdict$question_suffix))

  temp_dpdict <- temp_dpdict[, c(setdiff(names(temp_dpdict), "question_folder"), "question_folder")]


  return(temp_dpdict)
}

#' get_questions_dict
#' very simple function to get a questions_dict from a dpdict
#'
#' @param temp_dpdict a dpdict with columns for question-level metadata, e.g. questiontype, question_alias
#'
#' @return a questions_dict (subset of dpdict with one row per unique question group)
#' @export
#'
#' @examples
#' get_questions_dict(create_dict_with_metadata(get_big_test_dat()))
get_questions_dict <- function(temp_dpdict){
  unique_indices <- !duplicated(temp_dpdict$question_group)
  question_level_columns <- names(temp_dpdict)[names(temp_dpdict) %in% c("question_group", "questiontype", "question_lcs", "question_alias", "question_description", "question_folder")]
  questions_dict <- temp_dpdict[unique_indices, question_level_columns]

  return(questions_dict)
}

#' update_aliases
#'
#' @param temp_dpdict dpdict, e.g. from create_dict_with_metadata
#' @param questions_dict optional. a questions_dict that must have columns for question_group and question_alias
#' and the same number of values as there are unique question_groups in temp_dpdict
#'
#' @return temp_dpdict with alias_with_suffix always updated, and question_alias updated if questions_dict was provided
#' @export
#'
#' @examples
#' update_aliases(create_dict_with_metadata(get_big_test_dat()))
update_aliases <- function(temp_dpdict, questions_dict = NULL){

  if(!is.null(questions_dict)){
    if(!all(c("question_group", "question_alias") %in% names(questions_dict)) ||
       length(questions_dict$question_group) == length(unique(temp_dpdict$question_group))){
      stop("Invalid questions_dict: missing required columns or mismatched length")
    }
    temp_dpdict$question_alias <- questions_dict$question_alias[match(temp_dpdict$question_group, questions_dict$question_group)]
  } else {
    stopifnot("if no questions_dict provided, dpdict must have a column for question_alias" = "question_alias" %in% names(temp_dpdict))
  }

  temp_dpdict$alias_with_suffix <- ifelse(is.na(temp_dpdict$question_suffix),
                                          temp_dpdict$question_alias,
                                          paste0(temp_dpdict$question_alias, " - ", temp_dpdict$question_suffix))

  return(temp_dpdict)
}

#' split_into_question_groups
#' key part of create_dict_with_metadata
#' attempts to find a $question within a temp_dpdict with a unique suffix in an attempt to group variables into sensible questions
#' (will work within existing question groups if $question already exists, else will create starting $question from prefix to variable_name_sep)
#' will also provide a column question_lcs which is the longest common string found within a question group (this is used to define question groups when splitbycommonlabel == TRUE)
#'
#' note: if last variable in a question in returned dict just keeps its full string, min_common_strings may be set too high
#'
#' requires letters_702
#'
#' @param temp_dpdict dpdict for temp_dat. must have question_group variable.
#' @param temp_dat survey data dataframe
#' @param variable_name_sep "_" by default. string that separates each variable within a common question, e.g. if Q1_1, Q1_2 "_" is the correct variable_name_sep.
#' @param ignorelabelbeforeprefix boolean. if true, removes substring before prefix_sep before working with variable labels
#' @param prefix_sep string. if ignorelabelbeforeprefix == TRUE, the substring before prefix_sep will be removed before working with variable labels .
#' @param splitbyclass boolean. if true, every successive new class is given a new unique suffix
#' @param splitbynumlabelledvalues boolean. if true, each variable with a different number of labelled values is given a new unique suffix (because we would expect multiple variables in the same question to have the same number of options, e.g. battery of likert scales)
#' @param splitbynoncontiguous boolean. if true, variables with the same variable name prefix that are not located adjacent to each other are given a new unique suffix
#' @param splitbycommonlabel boolean. if true, each variable for which the longest common string within the variable label for that variable does not match the longest common string within the question group as a whole, is given a new suffix
#' @param findlongest boolean. if true, applies splitbycommonlabel by looking for the longest common substring. (this is computationally expensive.) if false, takes substring preceding labelsep as the commonlabel.
#' @param variable_compare_mode either "complete" or "reduced'. setting for when findlongest == TRUE. "complete" is computationally cheaper. if "complete", compares within the entire question group to find lcs for each statement. if "reduced", only looks at the variables before and after the variable in question, according to consecutive_range (a parameter in split_into_question_groups)
#' @param min_common_strings integer. setting for when findlongest == TRUE. looks for substrings common to at least min_common_strings other variables in the question group and will take from this subset the longest string.
#' @param labelsep string. setting for when findlongest == FALSE, in which case uses the substring in the variable name prior to labelsep as commonlabel.
#' @param min_lcs_length integer. any potential lcs shorter than min_lcs_length is disqualified.
#' @param consistent_consecutive_mode boolean. if true, will look for strings common with consecutive variables within consecutive_range. this is intended to speed up finding the best lcs. but it's not always useful as it can be too sensitive to spurious lcs. not used by default. instead, if consistent_consecutive_mode == FALSE, it will look for strings common to at least min_common_strings other variables in the question group.
#' @param consecutive_range integer. if variable_compare_mode == "reduced", only looks at the variables consecutive_range before and after the variable in question.
#' @param noisy numeric. noisy == 0 by default. if == 1, 2, 3 or 4 will do varying levels of printing for debugging and monitoring
#'
#' @return temp_dpdict with question groups split according to the function settings
#' @export
#'
#' @examples
#' temp_dat <- get_big_test_dat(n=10)
#' temp_dpdict <- create_dict(temp_dat, prefill = TRUE) # create fresh basic dpdict
#' # but we don't need variables for 'old' names and labels
#' temp_dpdict <- temp_dpdict[, !(names(temp_dpdict) %in% c("old_variable_names",
#'                                                    "old_variable_labels",
#'                                                    "old_value_labels"))]
#' temp_dpdict <- split_into_question_groups(temp_dpdict, temp_dat, variable_name_sep = "_",
#'                                        ignorelabelbeforeprefix = TRUE, prefix_sep = ": ",
#'                                        splitbyclass = TRUE, splitbynumlabelledvalues = TRUE,
#'                                        splitbynoncontiguous = TRUE,
#'                                        splitbycommonlabel = TRUE, labelsep = " - ",
#'                                        findlongest = TRUE, min_lcs_length = 10,
#'                                        min_common_strings = 5,
#'                                        consistent_consecutive_mode = FALSE,
#'                                        consecutive_range = 10,
#'                                        variable_compare_mode = "reduced",
#'                                        noisy = 0)
split_into_question_groups <- function(temp_dpdict, temp_dat, variable_name_sep = "_", ignorelabelbeforeprefix = TRUE, prefix_sep = ": ",
                                       splitbyclass = TRUE, splitbynumlabelledvalues = TRUE, splitbynoncontiguous = TRUE, splitbycommonlabel = TRUE,
                                       labelsep = " - ", findlongest = FALSE, min_lcs_length = 10,  min_common_strings = 5, consistent_consecutive_mode = FALSE, variable_compare_mode = "reduced", consecutive_range = 10,
                                       noisy = 0){

  if(splitbycommonlabel == FALSE & findlongest == TRUE){
    findlongest <- FALSE
    print("Setting findlongest to FALSE as only used if splitbycommonlabel is TRUE")
  }

  # creates a question group variable if there isn't one already
  if(!"question_group" %in% names(temp_dpdict)){
    temp_dpdict$question_group <- gsub(paste0(variable_name_sep,".*"), "", temp_dpdict$variable_names)
  }
  # first add a "_a" suffix to all question groups. we will update to e.g. "_b", "_c" as we split into groups.
  temp_dpdict$question_group <- paste0(temp_dpdict$question_group, "_a")

  # we'll also save the lcs associated with each question group
  temp_dpdict$question_lcs <- NA_character_

  # find longest common label for each question group and store in lcs_dict that we can reference
  if(findlongest == TRUE){

    # lcs_dict is a subset of temp_dpdict, with an additional column for lcs
    lcs_dict <- data.frame(variable_names = temp_dpdict$variable_names,
                       question_group = temp_dpdict$question_group,
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
          variabledpdict <- subset(variabledpdict, nchar(lcs) >= min_lcs_length)

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
          # also trim labelsep and whitespace
          current_question_dpdict$lcs[current_question_dpdict$variable_names == j_name] <- trimws(gsub(paste0(labelsep, "+$"), "", best_lcs))
          current_question_dpdict$lcs_distance[current_question_dpdict$variable_names == j_name] <- best_lcs_distance

          if(noisy >= 4){print(paste0("lcs found for ", j_name, ": ", best_lcs))}
        } else if (nrow(variabledpdict) == 1){ # if only one variable in variabledpdict, there's no need for complex logic to find lcs
          current_question_dpdict$lcs[current_question_dpdict$variable_names == j_name] <- trimws(gsub(paste0(labelsep, "+$"), "", variabledpdict$lcs))
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
  for(j in unique(temp_dpdict$question_group)){

    # subset temp_dpdict for just the current question group
    current_question_dpdict <- temp_dpdict[temp_dpdict$question_group == j,]
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

      if(noisy >= 2){
        print(paste0("finding question group for ", current_question_dpdict$variable_names[i], "..."))
      }

      current_class <- class(temp_dat[[match(TRUE, names(temp_dat) == current_question_dpdict$variable_names[i])]])
      current_question_group <- gsub("_.*", "", current_question_dpdict$question_group[i]) # current_question defined WITHOUT any suffixes
      current_numlabelledvalues <- length(sjlabelled::get_labels(temp_dat[, current_question_dpdict$variable_names[i], drop = FALSE])[[1]]) # we want character variables to always be given their own question group, because no such things as a multi character in crunch - so for example if two character variables have value labels and these obviously different, they're grouped separately
      current_index <- match(current_question_dpdict$variable_names[i], temp_dpdict$variable_names)

      if(findlongest == TRUE){
        current_commonlabel <- lcs_dict$lcs[lcs_dict$variable_names == current_question_dpdict$variable_names[i]]
      } else{
        # remove everything after the last label_sep

        if(ignorelabelbeforeprefix == TRUE){
          current_question_dpdict$variable_labels_less_prefix <- gsub(paste0("^([^", prefix_sep, "]+)", prefix_sep),
                                                              "", current_question_dpdict$variable_labels)
          current_commonlabel <- gsub(paste0(labelsep, "\\s*"), "", strsplit(current_question_dpdict$variable_labels_less_prefix[current_question_dpdict$variable_names == current_question_dpdict$variable_names[i]], paste0(labelsep, "\\s*"))[[1]][1])
        } else {
          current_commonlabel <- gsub(paste0(labelsep, "\\s*"), "", strsplit(current_question_dpdict$variable_labels[current_question_dpdict$variable_names == current_question_dpdict$variable_names[i]], paste0(labelsep, "\\s*"))[[1]][1])
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
#' @param noisy boolean. if true, prints status messages.
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
#' @param string1 a string
#' @param string2 another string
#' @param fromstart boolean. if true, finds longest common substring from the start of string1. if false, looks for lcs at any position.
#'
#' @return substring which represents the longest common starting substring found between string1 and string2, with an attribute lcs_distance representing the number of characters in the lcs
#' @export
#'
#' @examples get_longest_common_substring("boat", "boar")
get_longest_common_substring <- function(string1, string2, fromstart = FALSE) {
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


#' datamap_questions
#' provides a convenient view similar to SPSS variables like datamap, but by question group instead of individual variables.
#'
#' @param temp_dat a dataframe, assumed to be survey data
#' @param temp_dpdict a dpdict dataframe corresponding to temp_dat. must have a 'question_group' column.
#'
#' @return a tibble with columns for question_group, var_count, first_var_label, first_var_name, var_classes, var_values, var_labels, and question_types
#' @export
#'
#' @examples
#' temp_dat <- get_big_test_dat(10)
#' temp_dpdict <- create_dict_with_metadata(temp_dat)
#' datamap_questions(temp_dat, temp_dpdict)
datamap_questions <- function(temp_dat, temp_dpdict){

  if(!"question_group" %in% names(temp_dpdict)){
    stop("question_group not found in dpdict")
  }

  unique_question_groups <- unique(temp_dpdict$question_group)

  out <- dplyr::tibble(question_group = unique(temp_dpdict$question_group))

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
  out$question_types <- sapply(unique_question_groups, function(x) {
    paste(unique(temp_dpdict$questiontype[temp_dpdict$question_group == x]), collapse = " ")
  })

  return(out)
}


#' extract_common_labels_by_question
#' creates a dataframe with a column for common labels for each question group, given a dpdict.
#' optionally calls data_edit before returning, for any manual tidying of labels
#'
#' @param temp_dpdict a dpdict
#' @param column_to_extract_from column to look for common labels in
#' @param editfirst whether to call data_edit before returning, for any manual tidying of labels
#'
#' @return dataframe with a row for each unique question group in temp_dpdict and a column with the lcs from column_to_extract_from for each common question group
#' @export
#'
#' @examples
#' temp_dat <- get_big_test_dat(n=10)
#' temp_dpdict <- create_dict(temp_dat, prefill = TRUE) # create fresh basic dpdict
#' temp_dpdict <- temp_dpdict[, !(names(temp_dpdict) %in% c("old_variable_names",
#'                                                    "old_variable_labels",
#'                                                    "old_value_labels"))]
#' if(!"question_group" %in% names(temp_dpdict)){
#'  temp_dpdict$question_group <- gsub(paste0("_",".*"), "", temp_dpdict$variable_names)
#' }
#' temp_dpdict$question_group <- paste0(temp_dpdict$question_group, "_a")
#' extract_common_labels_by_question(temp_dpdict, editfirst = FALSE)
extract_common_labels_by_question <- function(temp_dpdict, column_to_extract_from = "variable_labels", editfirst = TRUE){

  stopifnot("dpdict must include columns 'question_group' and whatever string is given for column_to_extract_from" =  "question_group" %in% names(temp_dpdict) && column_to_extract_from %in% names(temp_dpdict))

  temp <- temp_dpdict[, c("question_group", column_to_extract_from)]

  lcs_dict <- data.frame(question_group = unique(temp$question_group), question_lcs = NA)
  for(i in lcs_dict$question_group){
    current_question_dpdict <- temp[temp$question_group == i,]
    currentlcs <- current_question_dpdict[[column_to_extract_from]][1]
    for(j in 1:nrow(current_question_dpdict)){
      currentlcs <- get_longest_common_substring(currentlcs, current_question_dpdict[[column_to_extract_from]][j])
    }
    lcs_dict$question_lcs[which(lcs_dict$question_group == i)] <- currentlcs
  }

  if(editfirst == TRUE){
    lcs_dict <- DataEditR::data_edit(lcs_dict) # remember to synchronize
  }

  return(lcs_dict)
}

#' get_affix_df
#'
#' @param full_string a string, e.g. a variable label
#' @param affix_type "suffix" or "prefix"
#' @param seps_priority a vector of strings representing separators to check for (in order)
#' @param strings_to_remove a vector of any strings to remove before working
#' @param filter_results whether to remove any rows where the affix is an empty string
#' @param sort_results whether to sort the final dataframe
#' @param prioritise_caps whether to sort final dataframe prioritising capital letters
#'
#' @return a dataframe summarising seps found in the full_string, the substrings separated, and their locations
#' @export
#'
#' @examples get_affix_df("Q1: This is a variable - This is a statement")
get_affix_df <- function(full_string, affix_type = "prefix", seps_priority = c("- ", "_", "."), strings_to_remove = c("Selected Choice"),
                         prioritise_caps = TRUE, filter_results = TRUE, sort_results = TRUE) {

  # Validate affix_type
  if (!affix_type %in% c("suffix", "prefix")) {
    stop("affix_type must be either 'suffix' or 'prefix'")
  }

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
  return(result_df)
}


#' get_unique_suffixes
#' takes a dpdict and returns a vector of length nrow(dpdict) which is best-guess for best suffixes to add to each variable's question group label to make it unique
#' wants to find suffix based on a common same sep type and, if allow_unmatched_sep_count_from_end == FALSE, common position of sep relative to the end
#' if can't find a suffix in labels, looks in variable name
#' and if can't find a unique suffix anywhere else, just assigns successive numbers
#'
#' @param temp_dpdict a dpdict with cols for uniqueid (e.g. variable name), variable labels and question groups
#' @param var_with_unique_id name of column to use as unique id (typically variable name)
#' @param var_with_strings name of column with strings to look for a suffix in (typically variable label)
#' @param var_with_question_groups name of column representing question groups of variables that should aim to find common suffix patterns within
#' @param seps_priority priority of separator strings to look for. separator strings determine what is a suffix (typically e.g. " - ")
#' @param allow_unmatched_sep_count_from_end whether to allow for suffixes that are identified by a sep in a different position relative to the end of the string
#' @param noisy either 0, 1, or 2. 1 and 2 provide increasingly noisy updates as the function works.
#'
#' @return a vector of length nrow(dpdict) which is best-guess for best suffixes to add to each variable's question group label to make it unique
#' @export
#'
#' @examples
#' get_unique_suffixes(create_dict_with_metadata(get_big_test_dat()))
get_unique_suffixes <- function(temp_dpdict, var_with_unique_id = "variable_names", var_with_strings = "variable_labels", var_with_question_groups = "question_group", seps_priority = c("- ","_","."), allow_unmatched_sep_count_from_end = TRUE, noisy = 0){

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


#' check_dpdict
#' checks for unique values in variable names and labels, and, if the column exists, alias_with_suffix, in dpdict, reporting back with a message listing any duplicates
#' and returns(invisibly) a temp_dpdict filtered for duplicates, so it can be more easily viewed e.g. with View()
#'
#' @param temp_dpdict a dataframe, assumed to be survey data
#' @param check_variable_names boolean. whether to check for and report on duplicate variable names.
#' @param check_variable_labels boolean. whether to check for and report on duplicate variable labels.
#' @param check_alias_with_suffix boolean. whether to check for and report on duplicate aliases with suffixes.
#' @param ignore_variable_name_from_label if TRUE, removes any cases of paste0(new_variable_name, variable_name_sep) from new_variable_label before working
#' for example, would remove "SC1: ", from "SC1: Country"
#' @param variable_name_sep specify the sep used to delineate variable name from label, e.g. ":" in "SC1: Country"
#'
#' @return returns, invisibly, temp_dpdict filtered for duplicates
#' @export
#'
#' @examples
#' check_dpdict(create_dict_with_metadata(get_big_test_dat(n=10)))
check_dpdict <- function(temp_dpdict,
                         check_variable_names = TRUE, check_variable_labels = TRUE, check_alias_with_suffix = TRUE,
                         ignore_variable_name_from_label = FALSE, variable_name_sep = ": "){

  if(ignore_variable_name_from_label == TRUE){
    temp_dpdict$variable_labels <- mapply(function(label, name) gsub(paste0(name, variable_name_sep), "", label),
                                          temp_dpdict$variable_labels, temp_dpdict$variable_names)
  }

  if(check_variable_names == TRUE){
    variable_names_with_dupe_variable_names <- temp_dpdict$variable_names[duplicated(temp_dpdict$variable_names)]
    message(paste0("The following duplicate variable names were found: ", paste(variable_names_with_dupe_variable_names, collapse = ", ")))
  } else {
    variable_names_with_dupe_variable_names <- NA
  }

  if(check_variable_labels == TRUE){
    dupe_variable_labels <- temp_dpdict$variable_labels[duplicated(temp_dpdict$variable_labels)]
    variable_names_with_dupe_variable_labels <- temp_dpdict$variable_names[temp_dpdict$variable_labels == dupe_variable_labels]
    message(paste0("The following duplicate variable labels were found: ", paste(dupe_variable_labels, collapse = ", ")))
  } else {
    variable_names_with_dupe_variable_labels <- NA
  }

  if(check_alias_with_suffix == TRUE && "alias_with_suffix" %in% names(temp_dpdict)){
    dupe_aliases_with_suffix <- temp_dpdict$alias_with_suffix[duplicated(temp_dpdict$alias_with_suffix)]
    variable_names_with_dupe_aliases_with_suffix <- temp_dpdict$variable_names[temp_dpdict$alias_with_suffix == dupe_aliases_with_suffix]
    message(paste0("The following duplicate aliases with suffix were found: ", paste(dupe_aliases_with_suffix, collapse = ", ")))
  } else {
    variable_names_with_dupe_aliases_with_suffix <- NA
  }

  any_dupes <- unique(c(variable_names_with_dupe_variable_names,
                   variable_names_with_dupe_variable_labels,
                   variable_names_with_dupe_aliases_with_suffix))

  if(ignore_variable_name_from_label == TRUE){
    temp_dpdict$variable_labels <- paste0(temp_dpdict$variable_names, variable_name_sep, temp_dpdict$variable_labels)
  }

  temp_dpdict <- temp_dpdict[temp_dpdict$variable_names %in% any_dupes,]

  if(check_variable_names == TRUE){
    temp_dpdict$duplicate_variable_name <- ifelse(temp_dpdict$variable_names %in% variable_names_with_dupe_variable_names, TRUE, FALSE)
  }
  if(check_variable_labels == TRUE){
    temp_dpdict$duplicate_variable_label <- ifelse(temp_dpdict$variable_names %in% variable_names_with_dupe_variable_labels, TRUE, FALSE)
  }
  if(check_alias_with_suffix == TRUE){
    temp_dpdict$duplicate_alias_with_suffix <- ifelse(temp_dpdict$variable_names %in% variable_names_with_dupe_aliases_with_suffix, TRUE, FALSE)
  }

  temp_dpdict <- temp_dpdict[, c(names(temp_dpdict)[names(temp_dpdict) %in% c("duplicate_variable_name", "duplicate_variable_label", "duplicate_alias_with_suffix")],
    setdiff(names(temp_dpdict), c("duplicate_variable_name", "duplicate_variable_label", "duplicate_alias_with_suffix")))]

  invisible(temp_dpdict)
}


#' split_grid_labels
#' for expanding the question alias in a dpdict for grid questions, e.g. brand perceptions where a series of statements are rated for each of a series of brands
#' e.g. moving from question_alias == 'Q1. Brand Image', question_suffix == 'Brand 1 - Statement 1'...
#' to question_alias == 'Q1. Brand Image - Brand 1', question_suffix == 'Statement 1'
#' so that the common question alias is specific to each brand.
#' Does so based on an example 'stem' (e.g. 'Brand 1') and a count_before_repeat, i.e. the number of statements asked for each brand.
#' (Assumes the same number of statements exists for each brand.)
#'
#' @param temp_dpdict a 'dpdict' dataframe
#' @param alias_to_split the question_alias in the temp_dpdict to change
#' @param sep the sep used in the question_suffix
#' @param example_stem_to_add as an example, the first stem currently in question_suffix that should be moved to question_alias
#' @param count_before_repeat the number of times the desired question_suffix after the stem has been moved repeats before the next stem
#'
#' @return an updated dpdict
#' @export
#'
#' @examples
#' split_grid_labels(create_dict_with_metadata(get_big_test_dat(n=10)),
#'                   "labelledmultiresponsegrid_a", "statement 1 ", 2)
split_grid_labels <- function(temp_dpdict, alias_to_split, example_stem_to_add, count_before_repeat, sep = " - "){

  # extract stems
  stems <- any_gsub(
    gsub(example_stem_to_add, "", temp_dpdict$question_suffix[temp_dpdict$question_alias == alias_to_split][1:count_before_repeat]),
                           "", temp_dpdict$question_suffix[temp_dpdict$question_alias == alias_to_split])

  # extract suffices
  suffix_with_sep <- any_gsub(unique(stems), "", temp_dpdict$question_suffix[temp_dpdict$question_alias == alias_to_split])

  # update dpdict
  temp_dpdict$question_suffix[temp_dpdict$question_alias == alias_to_split] <- suffix_with_sep
  temp_dpdict$question_alias[temp_dpdict$question_alias == alias_to_split] <- paste0(unname(temp_dpdict$question_alias[temp_dpdict$question_alias == alias_to_split]), sep, stems)

  return(temp_dpdict)
}


# ! set some reasonable fallback logic in create_dpdict_with_metadata, e.g. for case when not every label unique or description within group unique
# ! setup call to small LLM for automatic aliases.

# functions not added:
# split_grid_labels_for_crunch() # this is a simple function that might be useful
# get_common_labels() # this is probably covered in updated create_dict_with_metadata
