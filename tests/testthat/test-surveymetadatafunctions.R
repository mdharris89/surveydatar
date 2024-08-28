# no tests yet created for:
# - datamap()
# - create_dict()
# - check_dat_dpdict_alignment()
# - update_dat_from_dpdict()
# - get_longest_common_substring_slow()
# - get_longest_common_substring()
# - extract_common_labels_by_question()
# - datamap_questions()
# - get_affix_df()

# ! would be super cool to have a global 'noisy' parameter that applies across all functions in the package

test_that("split_into_question_groups works", {

  n = 10

  # test 1 - split by class
  temp_dat <- data.frame("Q1_1" = haven::labelled(sample(c(0, 1, NA_real_), n, replace = TRUE), label = "Q1_1: Select all that apply - Statement 1"),
                          "Q1_2" = haven::labelled(sample(c(0, 1, NA_real_), n, replace = TRUE), label = "Q1_2: Select all that apply - Statement 2"),
                          "Q1_3" = haven::labelled(sample(c(0, 1, NA_real_), n, replace = TRUE), label = "Q1_3: Select all that apply - Statement 3"),
                          "Q1_4" = haven::labelled(sample(c("0", "1", NA_character_), n, replace = TRUE), label = "Q1_4: Select all that apply - Statement 5"))

  temp_dict <- create_dict(temp_dat, prefill = TRUE)
  temp_dict <- temp_dict[, !(names(temp_dict) %in% c("old_variable_names", "old_variable_labels", "old_value_labels"))]

  test_result <- split_into_question_groups(temp_dict, temp_dat, variable_name_sep = "_",
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

  test_result <- split_into_question_groups(temp_dict, temp_dat, variable_name_sep = "_",
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

  test_result <- split_into_question_groups(temp_dict, temp_dat, variable_name_sep = "_",
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

  test_result <- split_into_question_groups(temp_dict, temp_dat, variable_name_sep = "_",
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

  test_result <- split_into_question_groups(temp_dict, temp_dat, variable_name_sep = "_",
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

  test_result <- split_into_question_groups(temp_dict, temp_dat, variable_name_sep = "_",
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

  temp_dat <- data.frame("Q1_1" = haven::labelled(c(0, 0, 1, NA_real_), label = "Q1_1: Select all that apply - Statement 1"),
                         "Q1_2" = haven::labelled(c(0, 0, 0, 0), label = "Q1_2: Select all that apply - Statement 2"),
                         "Q1_3" = haven::labelled(c(0, 0, 0, 0), label = "Q1_3: Select all that apply - Statement 3"),
                         "Q1_4" = haven::labelled(c(0, 0, NA, 0), label = "Q1_4: Select all that apply - Statement 4"))

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

test_that("check_dpdict works", {

  temp_dpdict <- create_dict_with_metadata(get_big_test_dat(n=10))
  temp_dpdict$variable_names[temp_dpdict$variable_names == "labelledmultiordinal_2"] <- "labelledmultiordinal_1"
  temp_dpdict$variable_names[temp_dpdict$variable_names == "multiresponseascharacter_2"] <- "multiresponseascharacter_1"
  temp_dpdict$variable_labels[temp_dpdict$variable_labels == "Example multiple response question (labelled numeric) - statement 2"] <- "Example multiple response question (labelled numeric) - statement 1"
  temp_dpdict$alias_with_suffix[temp_dpdict$alias_with_suffix == "multiresponseasfactor_a - statement 2"] <- "multiresponseasfactor_a - statement 1"

  # Test:
  test_result <- check_dpdict(temp_dpdict)
  test_result <- test_result[, !names(test_result) %in% c("duplicate_variable_name", "duplicate_variable_label", "duplicate_alias_with_suffix")]
  expected_test_result <- temp_dpdict[temp_dpdict$variable_names %in% c("labelledmultiordinal_1", "labelledmultiresponse_1", "labelledmultiresponse_2", "multiresponseasfactor_1", "multiresponseasfactor_2", "multiresponseascharacter_1"), ]

  expect_equal(test_result, expected_test_result)
})


test_that("update_aliases works", {

  temp_dpdict <- create_dict_with_metadata(get_big_test_dat())
  temp_dpdict$question_alias[temp_dpdict$question_alias == "labelledmultiordinal_a"] <- "lbledmultiordinal_a"

  test_result <- update_aliases(temp_dpdict)
  expected_result <- temp_dpdict
  expected_result$alias_with_suffix[expected_result$question_alias == "lbledmultiordinal_a"] <- c("lbledmultiordinal_a - statement 1", "lbledmultiordinal_a - statement 2")

  expect_equal(test_result, expected_result)

})
