# no additional tests created for:
# - datamap()
# - datamap_questions()
# - create_dict()
# - update_dict_with_metadata() # though create_dict_with_metadata, essentially a wrapper around this, is tested
# - get_longest_common_substring()
# - create_questions_dict()
# - get_affix_df()
# - update_dat_from_dpdict()
# - get_questions_dict()
# - update_aliases()
# - split_grid_labels()
#
# TODO: might be nice to have a global 'noisy' parameter that applies consistently across all functions in the package

test_that("split_into_question_groups works", {

  n = 10

  # test 1 - split by class
  temp_dat <- data.frame("Q1_1" = haven::labelled(sample(c(0, 1, NA_real_), n, replace = TRUE), label = "Q1_1: Select all that apply - Statement 1"),
                          "Q1_2" = haven::labelled(sample(c(0, 1, NA_real_), n, replace = TRUE), label = "Q1_2: Select all that apply - Statement 2"),
                          "Q1_3" = haven::labelled(sample(c(0, 1, NA_real_), n, replace = TRUE), label = "Q1_3: Select all that apply - Statement 3"),
                          "Q1_4" = haven::labelled(sample(c("0", "1", NA_character_), n, replace = TRUE), label = "Q1_4: Select all that apply - Statement 5"))

  temp_dict <- create_dict(temp_dat, prefill = TRUE)
  temp_dict <- temp_dict[, !(names(temp_dict) %in% c("old_variable_names", "old_variable_labels", "old_value_labels"))]

  test_result <- split_into_question_groups(temp_dict, temp_dat, variables_to_process = NULL, variable_name_sep = "_",
                                             ignorelabelbeforeprefix = TRUE, prefix_sep = ": ",
                                             splitbyclass = TRUE,
                                             splitbynumlabelledvalues = FALSE,
                                             splitbynoncontiguous = FALSE,
                                             splitbycommonlabel = FALSE,
                                             labelsep = " - ",
                                             findlongest = FALSE,
                                             min_lcs_length = 10,
                                             min_common_strings = 3,
                                             consistent_consecutive_mode = FALSE,
                                             consecutive_range = 10,
                                             variable_compare_mode = "complete")

  expected_test_result <- c("Q1_a", "Q1_a", "Q1_a", "Q1_b")

  expect_equal(test_result$question_group, expected_test_result)

  # test 2 - split by numlabelledvalues
  temp_dat <- data.frame("Q1_1" = haven::labelled(sample(c(0, 1, NA_real_), n, replace = TRUE), label = "Q1_1: Select all that apply - Statement 1", labels = c("Not selected" = 0, "Selected" = 1)),
                         "Q1_2" = haven::labelled(sample(c(0, 1, NA_real_), n, replace = TRUE), label = "Q1_2: Select all that apply - Statement 2", labels = c("Not selected" = 0, "Selected" = 1)),
                         "Q1_3" = haven::labelled(sample(c(0, 1, NA_real_), n, replace = TRUE), label = "Q1_3: Select all that apply - Statement 3", labels = c("Not selected" = 0, "Selected" = 1)),
                         "Q1_4" = haven::labelled(sample(c(0, 1, NA_real_), n, replace = TRUE), label = "Q1_4: Select all that apply - Statement 4"))

  temp_dict <- create_dict(temp_dat, prefill = TRUE)
  temp_dict <- temp_dict[, !(names(temp_dict) %in% c("old_variable_names", "old_variable_labels", "old_value_labels"))]

  test_result <- split_into_question_groups(temp_dict, temp_dat, variables_to_process = NULL, variable_name_sep = "_",
                                             ignorelabelbeforeprefix = TRUE, prefix_sep = ": ",
                                             splitbyclass = FALSE,
                                             splitbynumlabelledvalues = TRUE,
                                             splitbynoncontiguous = FALSE,
                                             splitbycommonlabel = FALSE,
                                             labelsep = " - ",
                                             findlongest = FALSE,
                                             min_lcs_length = 10,
                                             min_common_strings = 3,
                                             consistent_consecutive_mode = FALSE,
                                             consecutive_range = 10,
                                             variable_compare_mode = "complete")

  expected_test_result <- c("Q1_a", "Q1_a", "Q1_a", "Q1_b")

  expect_equal(test_result$question_group, expected_test_result)

  # test 3 - split by noncontiguous
  temp_dat <- data.frame("Q1_1" = haven::labelled(sample(c(0, 1, NA_real_), n, replace = TRUE), label = "Q1_1: Select all that apply - Statement 1"),
                         "Q1_2" = haven::labelled(sample(c(0, 1, NA_real_), n, replace = TRUE), label = "Q1_2: Select all that apply - Statement 2"),
                         "Q2_1" = haven::labelled(sample(c(0, 1, NA_real_), n, replace = TRUE), label = "Q2_1: Select all that apply - Statement 1"),
                         "Q1_3" = haven::labelled(sample(c(0, 1, NA_real_), n, replace = TRUE), label = "Q1_3: Select all that apply - Statement 3"))

  temp_dict <- create_dict(temp_dat, prefill = TRUE)
  temp_dict <- temp_dict[, !(names(temp_dict) %in% c("old_variable_names", "old_variable_labels", "old_value_labels"))]

  test_result <- split_into_question_groups(temp_dict, temp_dat, variables_to_process = NULL, variable_name_sep = "_",
                                            ignorelabelbeforeprefix = TRUE, prefix_sep = ": ",
                                            splitbyclass = FALSE,
                                            splitbynumlabelledvalues = FALSE,
                                            splitbynoncontiguous = TRUE,
                                            splitbycommonlabel = FALSE,
                                            labelsep = " - ",
                                            findlongest = FALSE,
                                            min_lcs_length = 10,
                                            min_common_strings = 3,
                                            consistent_consecutive_mode = FALSE,
                                            consecutive_range = 10,
                                            variable_compare_mode = "complete")

  expected_test_result <- c("Q1_a", "Q1_a", "Q2_a", "Q1_b")

  expect_equal(test_result$question_group, expected_test_result)

  # test 4 - split by commonlabel, no findlongest, no ignoreprefix
  temp_dat <- data.frame("Q1_1" = haven::labelled(sample(c(0, 1, NA_real_), n, replace = TRUE), label = "Q1_1: Select all that apply - Statement 1"),
                         "Q1_2" = haven::labelled(sample(c(0, 1, NA_real_), n, replace = TRUE), label = "Q1_2: Select all that apply - Statement 2"),
                         "Q1_3" = haven::labelled(sample(c(0, 1, NA_real_), n, replace = TRUE), label = "Q1_3: Select all that apply - Statement 3"),
                         "Q1_4" = haven::labelled(sample(c(0, 1, NA_real_), n, replace = TRUE), label = "Q1_4: Click one of these - Statement 4"))

  temp_dict <- create_dict(temp_dat, prefill = TRUE)
  temp_dict <- temp_dict[, !(names(temp_dict) %in% c("old_variable_names", "old_variable_labels", "old_value_labels"))]

  test_result <- split_into_question_groups(temp_dict, temp_dat, variables_to_process = NULL, variable_name_sep = "_",
                                            ignorelabelbeforeprefix = FALSE, prefix_sep = ": ",
                                            splitbyclass = FALSE,
                                            splitbynumlabelledvalues = FALSE,
                                            splitbynoncontiguous = FALSE,
                                            splitbycommonlabel = TRUE,
                                            labelsep = " - ",
                                            findlongest = FALSE,
                                            min_lcs_length = 10,
                                            min_common_strings = 3,
                                            consistent_consecutive_mode = FALSE,
                                            consecutive_range = 10,
                                            variable_compare_mode = "complete")

  expected_test_result <- c("Q1_a", "Q1_b", "Q1_c", "Q1_d")

  expect_equal(test_result$question_group, expected_test_result)

  # test 5 - split by commonlabel, no findlongest, do ignoreprefix
  temp_dat <- data.frame("Q1_1" = haven::labelled(sample(c(0, 1, NA_real_), n, replace = TRUE), label = "Q1_1: Select all that apply - Statement 1"),
                         "Q1_2" = haven::labelled(sample(c(0, 1, NA_real_), n, replace = TRUE), label = "Q1_2: Select all that apply - Statement 2"),
                         "Q1_3" = haven::labelled(sample(c(0, 1, NA_real_), n, replace = TRUE), label = "Q1_3: Select all that apply - Statement 3"),
                         "Q1_4" = haven::labelled(sample(c(0, 1, NA_real_), n, replace = TRUE), label = "Q1_4: Click one of these - Statement 4"))

  temp_dict <- create_dict(temp_dat, prefill = TRUE)
  temp_dict <- temp_dict[, !(names(temp_dict) %in% c("old_variable_names", "old_variable_labels", "old_value_labels"))]

  test_result <- split_into_question_groups(temp_dict, temp_dat, variables_to_process = NULL, variable_name_sep = "_",
                                            ignorelabelbeforeprefix = TRUE, prefix_sep = ": ",
                                            splitbyclass = FALSE,
                                            splitbynumlabelledvalues = FALSE,
                                            splitbynoncontiguous = FALSE,
                                            splitbycommonlabel = TRUE,
                                            labelsep = " - ",
                                            findlongest = FALSE,
                                            min_lcs_length = 10,
                                            min_common_strings = 3,
                                            consistent_consecutive_mode = FALSE,
                                            consecutive_range = 10,
                                            variable_compare_mode = "complete")

  expected_test_result <- c("Q1_a", "Q1_a", "Q1_a", "Q1_b")

  expect_equal(test_result$question_group, expected_test_result)

  # test 6 - split by commonlabel, do findlongest
  temp_dat <- data.frame("Q1_1" = haven::labelled(sample(c(0, 1, NA_real_), n, replace = TRUE), label = "Q1_1: Select all that apply Statement 1"),
                         "Q1_2" = haven::labelled(sample(c(0, 1, NA_real_), n, replace = TRUE), label = "Q1_2: Select all that apply Statement 2"),
                         "Q1_3" = haven::labelled(sample(c(0, 1, NA_real_), n, replace = TRUE), label = "Q1_3: Select all that apply Statement 3"),
                         "Q1_4" = haven::labelled(sample(c(0, 1, NA_real_), n, replace = TRUE), label = "Q1_4: Click one of these Statement 4"))

  temp_dict <- create_dict(temp_dat, prefill = TRUE)
  temp_dict <- temp_dict[, !(names(temp_dict) %in% c("old_variable_names", "old_variable_labels", "old_value_labels"))]

  test_result <- split_into_question_groups(temp_dict, temp_dat, variables_to_process = NULL, variable_name_sep = "_",
                                            ignorelabelbeforeprefix = TRUE, prefix_sep = ": ",
                                            splitbyclass = FALSE,
                                            splitbynumlabelledvalues = FALSE,
                                            splitbynoncontiguous = FALSE,
                                            splitbycommonlabel = TRUE,
                                            labelsep = " - ",
                                            findlongest = TRUE,
                                            min_lcs_length = 10,
                                            min_common_strings = 3,
                                            consistent_consecutive_mode = FALSE,
                                            consecutive_range = 10,
                                            variable_compare_mode = "complete")

  expected_test_result <- c("Q1_a", "Q1_a", "Q1_a", "Q1_b")

  expect_equal(test_result$question_group, expected_test_result)

})

test_that("create_dict_with_metadata works", {

  # tests for correct definition of multiresponse questiontype:

  # test 1: is not multiresponse because there are no non-zero OR NA values:

  temp_dat <- data.frame("Q1_1" = haven::labelled(c(0, 0, 0, NA_real_), label = "Q1_1: Select all that apply - Statement 1"),
                         "Q1_2" = haven::labelled(c(0, 0, 0, 0), label = "Q1_2: Select all that apply - Statement 2"),
                         "Q1_3" = haven::labelled(c(0, 0, 0, 0), label = "Q1_3: Select all that apply - Statement 3"),
                         "Q1_4" = haven::labelled(c(0, 0, NA, 0), label = "Q1_4: Select all that apply - Statement 4"))

  temp_dpdict <- create_dict_with_metadata(temp_dat)

  expect_equal(temp_dpdict$multiresponse, rep(FALSE, 4))

  # test 2: is multiresponse:

  temp_dat <- data.frame("Q1_1" = haven::labelled(c(0, 0, 1, NA_real_), label = "Q1_1: Select all that apply - Statement 1", labels = c("Not selected" = 0, "Selected" = 1)),
                         "Q1_2" = haven::labelled(c(0, 0, 0, 0), label = "Q1_2: Select all that apply - Statement 2", labels = c("Not selected" = 0, "Selected" = 1)),
                         "Q1_3" = haven::labelled(c(0, 1, 0, 0), label = "Q1_3: Select all that apply - Statement 3", labels = c("Not selected" = 0, "Selected" = 1)),
                         "Q1_4" = haven::labelled(c(0, 0, NA, 0), label = "Q1_4: Select all that apply - Statement 4", labels = c("Not selected" = 0, "Selected" = 1)))

  temp_dpdict <- create_dict_with_metadata(temp_dat)

  expect_equal(temp_dpdict$multiresponse, rep(TRUE, 4))

  # ! need to add a lot more tests here, including for ill-formed dats

})

test_that("get_unique_suffixes works", {

  expect_equal(get_unique_suffixes(create_dict_with_metadata(get_big_test_dat())), c(NA, NA, "a single 'word' of random characters", NA, NA, NA, NA,
                                                                                     "statement 1", "statement 2", "statement 1", "statement 2",
                                                                                     "statement 1 - attribute 1", "statement 1 - attribute 2",
                                                                                     "statement 2 - attribute 1", "statement 2 - attribute 2",
                                                                                     NA, NA, "statement 1", "statement 2",
                                                                                     NA, NA, NA, NA, "statement 1", "statement 2",
                                                                                     "1", "2", "3", "4"))

})

test_that("validate_dat_dpdict_alignment works", {

  # setup test data
  temp_dat <- data.frame(
    var1 = 1:3,
    var2 = letters[1:3],
    var3 = LETTERS[1:3]
  )
  attr(temp_dat$var1, "label") <- "Variable 1"
  attr(temp_dat$var2, "label") <- "Variable 2"
  attr(temp_dat$var3, "label") <- "Variable 3"

  # create correctly aligned dpdict
  good_dpdict <- data.frame(
    variable_names = c("var1", "var2", "var3"),
    variable_labels = c("Variable 1", "Variable 2", "Variable 3")
  )

  # test correctly aligned data:
  expect_true(validate_dat_dpdict_alignment(temp_dat, good_dpdict, warn_only = TRUE))

  # test column count mismatch:
  bad_dpdict <- good_dpdict[-1,]
  expect_warning(
    validate_dat_dpdict_alignment(temp_dat, bad_dpdict, warn_only = TRUE),
    "Number of columns in dat \\(3\\) does not match number of rows in dpdict \\(2\\)"
  )

  # test missing required columns:
  incomplete_dpdict <- data.frame(variable_names = c("var1", "var2", "var3"))
  expect_warning(
    validate_dat_dpdict_alignment(temp_dat, incomplete_dpdict, warn_only = TRUE),
    "Required columns missing from dpdict: variable_labels"
  )

  # test variable names mismatch:
  mismatched_names_dpdict <- good_dpdict
  mismatched_names_dpdict$variable_names[1] <- "wrong_name"
  expect_warning(
    validate_dat_dpdict_alignment(temp_dat, mismatched_names_dpdict, warn_only = TRUE),
    "Variables in dat not found in dpdict variable_names: var1"
  )

  # test variable labels mismatch:
  mismatched_labels_dpdict <- good_dpdict
  mismatched_labels_dpdict$variable_labels[1] <- "Wrong Label"
  expect_warning(
    validate_dat_dpdict_alignment(temp_dat, mismatched_labels_dpdict, warn_only = TRUE),
    "Variable labels in dat not found in dpdict variable_labels: Variable 1"
  )
})

test_that("validate_no_dpdict_duplicates works", {

  # setup base dpdict
  base_dpdict <- data.frame(
    variable_names = c("var1", "var2", "var3"),
    variable_labels = c("Label 1", "Label 2", "Label 3"),
    alias_with_suffix = c("alias1", "alias2", "alias3")
  )

  # test no duplicates:
  expect_true(validate_no_dpdict_duplicates(base_dpdict, warn_only = TRUE))

  # test duplicate variable names:
  dup_names_dpdict <- base_dpdict
  dup_names_dpdict$variable_names[3] <- "var1"
  expect_warning(
    validate_no_dpdict_duplicates(dup_names_dpdict,
                                  check_variable_names = TRUE,
                                  check_variable_labels = FALSE,
                                  check_alias_with_suffix = FALSE,
                                  warn_only = TRUE),
    "Duplicate variable names found: var1"
  )

  # test duplicate variable labels:
  dup_labels_dpdict <- base_dpdict
  dup_labels_dpdict$variable_labels[3] <- "Label 1"
  expect_warning(
    validate_no_dpdict_duplicates(dup_labels_dpdict,
                                  check_variable_names = FALSE,
                                  check_variable_labels = TRUE,
                                  check_alias_with_suffix = FALSE,
                                  warn_only = TRUE),
    "Duplicate variable labels found: Label 1"
  )

  # test duplicate aliases:
  dup_aliases_dpdict <- base_dpdict
  dup_aliases_dpdict$alias_with_suffix[3] <- "alias1"
  expect_warning(
    validate_no_dpdict_duplicates(dup_aliases_dpdict,
                                  check_variable_names = FALSE,
                                  check_variable_labels = FALSE,
                                  check_alias_with_suffix = TRUE,
                                  warn_only = TRUE),
    "Duplicate aliases with suffix found: alias1"
  )

  # test ignore_variable_name_from_label:
  label_with_name_dpdict <- base_dpdict
  label_with_name_dpdict$variable_labels <- c("var1: Label 1", "var2: Label 2", "var1: Label 1")
  expect_warning(
    validate_no_dpdict_duplicates(label_with_name_dpdict,
                                  check_variable_names = FALSE,
                                  check_variable_labels = TRUE,
                                  check_alias_with_suffix = FALSE,
                                  warn_only = TRUE),
    "Duplicate variable labels found: var1: Label 1"
  )

  # should pass when ignoring variable names in labels:
  expect_true(
    validate_no_dpdict_duplicates(label_with_name_dpdict,
                                  check_variable_names = FALSE,
                                  check_variable_labels = TRUE,
                                  check_alias_with_suffix = FALSE,
                                  ignore_variable_name_from_label = TRUE,
                                  warn_only = TRUE)
  )
})
