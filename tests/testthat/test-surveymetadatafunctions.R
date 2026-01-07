## Survey metadata tests
##
## survey_data combines raw data (`dat`) with a metadata dictionary (`dpdict`), and many operations depend
## on that pairing staying aligned. The tests in this file check dpdict/dat updates, validation error
## messages, and metadata propagation behaviours.

##### Data/dpdict update functions #####
# Checks functions that apply dpdict changes back onto data (names, labels, value labels) and preserve survey_data.
test_that("update_dat_from_dpdict works", {
  temp_dat <- get_minimal_labelled_test_dat()
  temp_dpdict <- create_dict(temp_dat)

  # Modify dpdict for testing
  temp_dpdict$variable_names[1] <- "new_var1_name"
  temp_dpdict$variable_labels[1] <- "New Var1 Label"
  temp_dpdict$value_labels[[2]] <- c("A"=1, "B"=2, "C"=3) # Add value labels to second var

  updated_dat <- update_dat_from_dpdict(temp_dat, temp_dpdict)

  # Check names
  expect_equal(names(updated_dat)[1], "new_var1_name")
  expect_equal(names(updated_dat)[2], names(temp_dat)[2]) # Unchanged name

  # Check labels
  expect_equal(attr(updated_dat[[1]], "label"), "New Var1 Label")
  expect_equal(attr(updated_dat[[2]], "label"), attr(temp_dat[[2]], "label")) # Unchanged label

  # Check value labels
  expect_null(attr(updated_dat[[1]], "labels")) # Original had no value labels
  expect_equal(attr(updated_dat[[2]], "labels"), c("A"=1, "B"=2, "C"=3))

  # Test with survey_data object
  survey_obj <- create_survey_data(get_minimal_labelled_test_dat())
  updated_survey_obj <- update_dat_from_dpdict(survey_obj, temp_dpdict)
  expect_s3_class(updated_survey_obj, "survey_data")
  expect_equal(names(updated_survey_obj$dat)[1], "new_var1_name")
  expect_equal(attr(updated_survey_obj$dat[[2]], "labels"), c("A"=1, "B"=2, "C"=3))

  # Test error handling
  mismatched_dpdict <- temp_dpdict[-1,]
  expect_error(update_dat_from_dpdict(temp_dat, mismatched_dpdict), "Some data columns don't match either variable_names or old_variable_names in dpdict")
})

##### Alias and question dictionary updates #####
# Checks alias updates and how those changes propagate through dpdict (including survey_data wrappers).
test_that("update_aliases works", {
  temp_dpdict <- create_dict_with_metadata(get_big_test_dat(n=10))
  questions_dict <- get_questions_dict(temp_dpdict)
  original_alias <- questions_dict$question_alias[1]
  original_description <- temp_dpdict$question_description[temp_dpdict$question_alias == original_alias][1]
  original_alias_with_suffix <- temp_dpdict$alias_with_suffix[temp_dpdict$question_alias == original_alias][1]

  # Modify alias
  questions_dict$question_alias[1] <- "new_alias_test_a"
  updated_dpdict <- update_aliases(temp_dpdict, questions_dict)

  # Check updates
  expect_equal(updated_dpdict$question_alias[temp_dpdict$question_group == questions_dict$question_group[1]][1], "new_alias_test_a")
  # Description should update the prefix part
  expect_true(grepl("new_alias_test", updated_dpdict$question_description[temp_dpdict$question_group == questions_dict$question_group[1]][1]))
  expect_false(grepl(sub("_.*", "", original_alias), updated_dpdict$question_description[temp_dpdict$question_group == questions_dict$question_group[1]][1]))
  # Alias with suffix should update
  expect_true(grepl("new_alias_test_a", updated_dpdict$alias_with_suffix[temp_dpdict$question_group == questions_dict$question_group[1]][1]))

  # Test with survey_data object
  survey_obj <- create_survey_data(get_big_test_dat(n=10))
  updated_survey_obj <- update_aliases(survey_obj, questions_dict)
  expect_s3_class(updated_survey_obj, "survey_data")
  expect_equal(updated_survey_obj$dpdict$question_alias[updated_survey_obj$dpdict$question_group == questions_dict$question_group[1]][1], "new_alias_test_a")

  # Error handling
  invalid_questions_dict <- questions_dict[, "question_group", drop=FALSE]
  expect_error(update_aliases(temp_dpdict, invalid_questions_dict), "Invalid questions_dict")
})


##### Validation error-message contracts #####
# Checks that dpdict/dat validation returns detailed, user-actionable errors for common mismatch scenarios.
test_that("validate_dat_dpdict_alignment provides detailed error messages", {

  # Test 1: Dimension mismatch
  test_dat <- data.frame(a = 1:3, b = 4:6, c = 7:9)
  test_dpdict <- data.frame(
    variable_names = c("a", "b"),
    variable_labels = c("Variable A", "Variable B")
  )

  expect_error(
    validate_dat_dpdict_alignment(test_dat, test_dpdict),
    "DIMENSION_MISMATCH.*Expected: dpdict should have one row per dat column \\(3 rows\\).*Actual: dpdict has 2 rows",
    class = "simpleError"
  )

  # Test 2: Missing required columns
  test_dpdict_missing_cols <- data.frame(
    var_names = c("a", "b", "c"),  # Wrong column name
    some_other_col = c("x", "y", "z")
  )

  expect_error(
    validate_dat_dpdict_alignment(test_dat, test_dpdict_missing_cols),
    "MISSING_REQUIRED_COLUMNS.*Expected: dpdict must contain columns: variable_names, variable_labels.*Missing columns: variable_names, variable_labels",
    class = "simpleError"
  )

  # Test 3: Variable names mismatch with detailed positioning
  test_dpdict_name_mismatch <- data.frame(
    variable_names = c("x", "b", "z"),  # 'a' and 'c' don't match
    variable_labels = c("Variable X", "Variable B", "Variable Z")
  )

  expect_error(
    validate_dat_dpdict_alignment(test_dat, test_dpdict_name_mismatch),
    "VARIABLE_NAMES_MISMATCH.*Variables in dat but not in dpdict\\$variable_names.*\\[1\\] 'a'.*\\[3\\] 'c'.*Variables in dpdict\\$variable_names but not in dat.*\\[1\\] 'x'.*\\[3\\] 'z'",
    class = "simpleError"
  )

  # Test 4: Variable names found in old_variable_names with helpful suggestions
  test_dpdict_with_old <- data.frame(
    variable_names = c("new_a", "new_b", "new_c"),
    variable_labels = c("Variable A", "Variable B", "Variable C"),
    old_variable_names = c("a", "b", "c")  # Original names match dat
  )

  error_msg <- expect_error(
    validate_dat_dpdict_alignment(test_dat, test_dpdict_with_old),
    "VARIABLE_NAMES_MISMATCH.*Variables found in dpdict\\$old_variable_names.*'a' -> should be 'new_a' \\(dpdict row 1\\).*Consider running update_dat_from_dpdict\\(\\) first",
    class = "simpleError"
  )

  # Test 5: Variable labels mismatch (using labeled data)
  test_dat_labeled <- test_dat
  attr(test_dat_labeled$a, "label") <- "Label A"
  attr(test_dat_labeled$b, "label") <- "Label B"
  attr(test_dat_labeled$c, "label") <- "Label C"

  test_dpdict_label_mismatch <- data.frame(
    variable_names = c("a", "b", "c"),
    variable_labels = c("Different Label A", "Label B", "Different Label C")
  )

  expect_error(
    validate_dat_dpdict_alignment(test_dat_labeled, test_dpdict_label_mismatch),
    "VARIABLE_LABELS_MISMATCH.*Labels in dat but not in dpdict\\$variable_labels.*'Label A'.*'Label C'",
    class = "simpleError"
  )
})

# Test enhanced validate_no_dpdict_duplicates() function
test_that("validate_no_dpdict_duplicates provides detailed error messages", {

  # Test 1: Duplicate variable names with positioning details
  test_dpdict_dup_names <- data.frame(
    variable_names = c("q1", "q2", "q1", "q3", "q2"),  # q1 and q2 are duplicated
    variable_labels = c("Question 1", "Question 2", "Question 1 Again", "Question 3", "Question 2 Again")
  )

  expect_error(
    validate_no_dpdict_duplicates(test_dpdict_dup_names, check_variable_labels = FALSE, check_alias_with_suffix = FALSE),
    "DUPLICATE_VARIABLE_NAMES.*Variable name: 'q1'.*Appears at dpdict rows: 1, 3.*Associated labels:.*\\[1\\]: 'Question 1'.*\\[3\\]: 'Question 1 Again'.*Variable name: 'q2'.*Appears at dpdict rows: 2, 5",
    class = "simpleError"
  )

  # Test 2: Duplicate variable labels with positioning details
  test_dpdict_dup_labels <- data.frame(
    variable_names = c("q1", "q2", "q3", "q4"),
    variable_labels = c("Same Label", "Different Label", "Same Label", "Another Label")
  )

  expect_error(
    validate_no_dpdict_duplicates(test_dpdict_dup_labels, check_variable_names = FALSE, check_alias_with_suffix = FALSE),
    "DUPLICATE_VARIABLE_LABELS.*Variable label: 'Same Label'.*Appears at dpdict rows: 1, 3.*Associated variable names:.*\\[1\\]: 'q1'.*\\[3\\]: 'q3'",
    class = "simpleError"
  )

  # Test 3: Duplicate alias_with_suffix with context
  test_dpdict_dup_alias <- data.frame(
    variable_names = c("sc1_1", "sc1_2", "sc2_1", "sc2_2"),
    variable_labels = c("Scale 1 Item 1", "Scale 1 Item 2", "Scale 2 Item 1", "Scale 2 Item 2"),
    alias_with_suffix = c("scale_1", "scale_2", "scale_1", "scale_4")  # scale_1 duplicated
  )

  expect_error(
    validate_no_dpdict_duplicates(test_dpdict_dup_alias, check_variable_names = FALSE, check_variable_labels = FALSE),
    "DUPLICATE_ALIAS_WITH_SUFFIX.*Alias with suffix: 'scale_1'.*Appears at dpdict rows: 1, 3.*Associated variables:.*\\[1\\]: 'sc1_1' \\('Scale 1 Item 1'\\).*\\[3\\]: 'sc2_1' \\('Scale 2 Item 1'\\)",
    class = "simpleError"
  )

  # Test 4: Multiple types of duplicates in one validation
  test_dpdict_multiple_dups <- data.frame(
    variable_names = c("q1", "q1", "q3"),  # duplicate names
    variable_labels = c("Label A", "Label B", "Label A"),  # duplicate labels
    alias_with_suffix = c("alias1", "alias2", "alias1")  # duplicate aliases
  )

  error_msg <- expect_error(
    validate_no_dpdict_duplicates(test_dpdict_multiple_dups),
    "DUPLICATE_VARIABLE_NAMES.*DUPLICATE_VARIABLE_LABELS.*DUPLICATE_ALIAS_WITH_SUFFIX",
    class = "simpleError"
  )

  # Test 5: Proper handling of NA values (should not be flagged as duplicates)
  test_dpdict_with_nas <- data.frame(
    variable_names = c("q1", "q2", "q3"),
    variable_labels = c("Label 1", NA, NA),  # NAs should not be considered duplicates
    alias_with_suffix = c("alias1", NA, NA)
  )

  expect_true(validate_no_dpdict_duplicates(test_dpdict_with_nas))
})

# Test enhanced validate_variable_names() function
test_that("validate_variable_names provides detailed error explanations", {

  # Test 1: Names starting with numbers
  expect_error(
    validate_variable_names(c("1q", "2var", "validname")),
    "Variable name validation failed.*\\[1\\] '1q'.*starts with number \\(must start with letter\\).*\\[2\\] '2var'.*starts with number \\(must start with letter\\)",
    class = "simpleError"
  )

  # Test 2: Names with invalid characters
  expect_error(
    validate_variable_names(c("q.1", "var-name", "q@2")),
    "contains invalid character\\(s\\): '\\.'.*contains invalid character\\(s\\): '-'.*contains invalid character\\(s\\): '@'",
    class = "simpleError"
  )

  # Test 3: Names starting with special characters
  expect_error(
    validate_variable_names(c("_var", ".name", "#question")),
    "\\[1\\] '_var'.*starts with underscore \\(must start with letter\\).*\\[2\\] '\\.name'.*starts with special character \\(must start with letter\\).*\\[3\\] '#question'.*starts with special character \\(must start with letter\\)",
    class = "simpleError"
  )

  # Test 4: Incorrect underscore patterns
  expect_error(
    validate_variable_names(c("q_a", "var_", "q__1", "sc_x2")),
    "\\[1\\] 'q_a'.*underscore not followed by number.*\\[2\\] 'var_'.*ends with underscore.*\\[3\\] 'q__1'.*contains consecutive underscores.*\\[4\\] 'sc_x2'.*underscore not followed by number",
    class = "simpleError"
  )

  # Test 5: Multiple violations in single name
  expect_error(
    validate_variable_names(c("1q.a_b")),
    "\\[1\\] '1q\\.a_b'.*starts with number.*contains invalid character\\(s\\): '\\.'.*underscore not followed by number",
    class = "simpleError"
  )

  # Test 6: Valid names should pass without error
  expect_true(validate_variable_names(c("q1", "Q1", "SC1_1", "satisfaction_1", "SC1a_1", "SC1_1oe")))

  # Test 7: Check that the examples in error message are actually valid
  valid_examples <- c("q1", "Q1", "SC1_1", "satisfaction_1", "SC1a_1", "SC1_1oe")
  expect_true(validate_variable_names(valid_examples))
})

# Test warning behavior (warn_only = TRUE)
test_that("validation functions provide same detailed messages for warnings", {

  # Test that warning messages contain the same detail as error messages
  test_dat <- data.frame(a = 1:2, b = 3:4)
  test_dpdict <- data.frame(
    variable_names = c("x", "y"),  # Names don't match
    variable_labels = c("Variable X", "Variable Y")
  )

  expect_warning(
    result <- validate_dat_dpdict_alignment(test_dat, test_dpdict, warn_only = TRUE),
    "VARIABLE_NAMES_MISMATCH.*Variables in dat but not in dpdict\\$variable_names.*\\[1\\] 'a'.*\\[2\\] 'b'"
  )
  expect_false(result)

  # Test warning for duplicates
  test_dpdict_dups <- data.frame(
    variable_names = c("q1", "q1"),
    variable_labels = c("Label 1", "Label 2")
  )

  expect_warning(
    result <- validate_no_dpdict_duplicates(test_dpdict_dups, warn_only = TRUE),
    "DUPLICATE_VARIABLE_NAMES.*Variable name: 'q1'.*Appears at dpdict rows: 1, 2"
  )
  expect_false(result)

  # Test warning for invalid variable names
  expect_warning(
    result <- validate_variable_names(c("1invalid"), warn_only = TRUE),
    "\\[1\\] '1invalid'.*starts with number \\(must start with letter\\)"
  )
  expect_false(result)
})

# Test successful validation (should return TRUE invisibly)
test_that("validation functions return TRUE for valid inputs", {

  # Valid alignment
  test_dat <- data.frame(a = 1:3, b = 4:6)
  attr(test_dat$a, "label") <- "Variable A"
  attr(test_dat$b, "label") <- "Variable B"
  test_dpdict <- data.frame(
    variable_names = c("a", "b"),
    variable_labels = c("Variable A", "Variable B")
  )

  expect_true(validate_dat_dpdict_alignment(test_dat, test_dpdict))

  # No duplicates
  test_dpdict_unique <- data.frame(
    variable_names = c("q1", "q2", "q3"),
    variable_labels = c("Question 1", "Question 2", "Question 3"),
    alias_with_suffix = c("alias1", "alias2", "alias3")
  )

  expect_true(validate_no_dpdict_duplicates(test_dpdict_unique))

  # Valid variable names
  expect_true(validate_variable_names(c("q1", "SC1_1", "satisfaction_2a")))
})

# Test edge cases
test_that("validation functions handle edge cases appropriately", {

  # Empty data
  expect_error(
    validate_dat_dpdict_alignment(data.frame(), data.frame(variable_names = character(), variable_labels = character())),
    NA  # Should not error for empty but aligned data
  )

  # Single column/row
  single_dat <- data.frame(single_var = 1:5)
  attr(single_dat$single_var, "label") <- "Single Variable"
  single_dpdict <- data.frame(
    variable_names = "single_var",
    variable_labels = "Single Variable"
  )
  expect_true(validate_dat_dpdict_alignment(single_dat, single_dpdict))

  # Check validation with missing alias_with_suffix column (should not error)
  dpdict_no_alias <- data.frame(
    variable_names = c("a", "b"),
    variable_labels = c("Label A", "Label B")
  )
  expect_true(validate_no_dpdict_duplicates(dpdict_no_alias, check_alias_with_suffix = TRUE))
})


# Test dpdict functions
test_that("create_dict works", {
  # Basic functionality
  temp_dat <- get_minimal_labelled_test_dat()
  dpdict_prefill <- create_dict(temp_dat, prefill = TRUE)
  dpdict_no_prefill <- create_dict(temp_dat, prefill = FALSE)

  expect_equal(nrow(dpdict_prefill), ncol(temp_dat))
  expect_named(dpdict_prefill, c("old_variable_names", "old_variable_labels", "old_value_labels", "variable_names", "variable_labels", "value_labels"))
  expect_equal(dpdict_prefill$variable_names, dpdict_prefill$old_variable_names)
  expect_equal(dpdict_prefill$variable_labels, dpdict_prefill$old_variable_labels)
  expect_equal(dpdict_prefill$value_labels, dpdict_prefill$old_value_labels)

  expect_true(all(is.na(dpdict_no_prefill$variable_names)))
  expect_true(all(is.na(dpdict_no_prefill$variable_labels)))
  expect_true(all(is.na(dpdict_no_prefill$value_labels)))

  # Edge cases
  empty_dat <- data.frame()
  expect_error(create_dict(empty_dat), "temp_dat must be a dataframe with more than zero columns")

  dat_no_labels <- data.frame(a = 1:3, b = letters[1:3])
  dpdict_no_labels <- create_dict(dat_no_labels)
  expect_true(all(is.na(dpdict_no_labels$old_variable_labels)))
  expect_true(all(is.na(dpdict_no_labels$old_value_labels)))
})

test_that("create_dict_with_metadata works", {

  # tests for correct definition of multiresponse questiontype:

  # test 1: is multiresponse:

  temp_dat <- data.frame("Q1_1" = haven::labelled(c(0, 0, 1, NA_real_), label = "Q1_1: Select all that apply - Statement 1", labels = c("Not selected" = 0, "Selected" = 1)),
                         "Q1_2" = haven::labelled(c(0, 0, 0, 0), label = "Q1_2: Select all that apply - Statement 2", labels = c("Not selected" = 0, "Selected" = 1)),
                         "Q1_3" = haven::labelled(c(0, 1, 0, 0), label = "Q1_3: Select all that apply - Statement 3", labels = c("Not selected" = 0, "Selected" = 1)),
                         "Q1_4" = haven::labelled(c(0, 0, NA, 0), label = "Q1_4: Select all that apply - Statement 4", labels = c("Not selected" = 0, "Selected" = 1)))

  temp_dpdict <- create_dict_with_metadata(temp_dat)

  expect_equal(temp_dpdict$questiontype, rep("multiresponse", 4))

  # test 2: some are multiresponse

  temp_dat <- data.frame(
    # Single-variable group - even with non-zero values, shouldn't be multiresponse
    "Q1_1" = haven::labelled(c(1, 0, 1, NA_real_),
                             label = "Q1_1: Single variable question",
                             labels = c("Not selected" = 0, "Selected" = 1)),

    # Multi-variable group - should be detected as multiresponse
    "Q2_1" = haven::labelled(c(0, 1, 0, NA_real_),
                             label = "Q2_1: Multi variable question - Statement 1",
                             labels = c("Not selected" = 0, "Selected" = 1)),
    "Q2_2" = haven::labelled(c(1, 0, 0, 0),
                             label = "Q2_2: Multi variable question - Statement 2",
                             labels = c("Not selected" = 0, "Selected" = 1))
  )

  # Create dpdict and run update_dict_with_metadata
  temp_dpdict <- create_dict(temp_dat, prefill = TRUE)
  updated_dpdict <- update_dict_with_metadata(NULL, temp_dat, temp_dpdict)

  # Check question groups are assigned correctly
  q1_group <- updated_dpdict$question_group[updated_dpdict$variable_names == "Q1_1"]
  q2_group_1 <- updated_dpdict$question_group[updated_dpdict$variable_names == "Q2_1"]
  q2_group_2 <- updated_dpdict$question_group[updated_dpdict$variable_names == "Q2_2"]

  # First verify the grouping is as expected
  expect_false(q1_group %in% c(q2_group_1, q2_group_2)) # Q1 should be in a different group
  expect_equal(q2_group_1, q2_group_2) # Q2 variables should be in the same group

  # Verify questiontype assignment
  expect_equal(updated_dpdict$questiontype[updated_dpdict$variable_names == "Q1_1"], "categorical")
  expect_equal(updated_dpdict$questiontype[updated_dpdict$variable_names == "Q2_1"], "multiresponse")
  expect_equal(updated_dpdict$questiontype[updated_dpdict$variable_names == "Q2_2"], "multiresponse")

  # Would benefit from more tests here, including for ill-formed dats

})

test_that("update_dict_with_metadata works", {
  temp_dat <- get_big_test_dat(n = 10)
  temp_dpdict <- create_dict(temp_dat, prefill = TRUE)

  # Check basic column creation
  updated_dpdict <- update_dict_with_metadata(NULL, temp_dat, temp_dpdict)
  expect_true("question_group" %in% names(updated_dpdict))
  expect_true("questiontype" %in% names(updated_dpdict))
  expect_true("alias_with_suffix" %in% names(updated_dpdict))
  expect_equal(nrow(updated_dpdict), nrow(temp_dpdict))

  # Test with variables_to_update
  variables_to_update <- rep(FALSE, nrow(temp_dpdict))
  variables_to_update[1:2] <- TRUE
  # expect warnings due to most variables initialising with NA
  expect_warning(updated_partial_dpdict <- update_dict_with_metadata(NULL, temp_dat, temp_dpdict, variables_to_update = variables_to_update))
  # Check if metadata is updated only for the first two variables
  expect_false(any(is.na(updated_partial_dpdict$question_group[1:2])))
  expect_true(all(is.na(updated_partial_dpdict$question_group[3:nrow(updated_partial_dpdict)])))

  # Test error handling for misaligned inputs
  misaligned_dpdict <- temp_dpdict[-1, ]
  expect_error(update_dict_with_metadata(NULL, temp_dat, misaligned_dpdict))
})

test_that("check_seps function works", {
  # Test 1: Basic dataset with consistent separators
  basic_dat <- data.frame(
    Q1_1 = c(1, 2, 3),
    Q1_2 = c(4, 5, 6),
    Q2_1 = c(7, 8, 9)
  )

  # Add labels with consistent separators
  attr(basic_dat$Q1_1, "label") <- "Q1: First question - Statement 1"
  attr(basic_dat$Q1_2, "label") <- "Q1: First question - Statement 2"
  attr(basic_dat$Q2_1, "label") <- "Q2: Second question - Statement 1"

  basic_result <- check_seps(basic_dat)

  expect_true(basic_result$consistency["var_name_sep"])
  expect_true(basic_result$consistency["prefix_sep"])
  expect_true(basic_result$consistency["statement_sep"])
  expect_equal(basic_result$separators[["var_name_sep"]], "_")
  expect_equal(basic_result$separators[["prefix_sep"]], ": ")
  expect_equal(basic_result$separators[["statement_sep"]], " - ")
  expect_length(basic_result$issues, 0)

  # Test 2: Dataset with inconsistent varname separators
  inconsistent_dat <- data.frame(
    Q1_1 = c(1, 2, 3),
    Q1.2 = c(4, 5, 6),
    Q2_1 = c(7, 8, 9)
  )

  attr(inconsistent_dat$Q1_1, "label") <- "Q1: First question - Statement 1"
  attr(inconsistent_dat$Q1.2, "label") <- "Q1: First question - Statement 2"
  attr(inconsistent_dat$Q2_1, "label") <- "Q2: Second question - Statement 1"

  inconsistent_result <- check_seps(inconsistent_dat)

  expect_false(inconsistent_result$consistency["var_name_sep"])
  expect_true(inconsistent_result$consistency["prefix_sep"])
  expect_true(inconsistent_result$consistency["statement_sep"])
  expect_length(inconsistent_result$issues, 1)

  # Test 3: Dataset with inconsistent prefix separators
  inconsistent_dat <- data.frame(
    Q1_1 = c(1, 2, 3),
    Q1_2 = c(4, 5, 6),
    Q2_1 = c(7, 8, 9)
  )

  attr(inconsistent_dat$Q1_1, "label") <- "Q1: First question - Statement 1"
  attr(inconsistent_dat$Q1_2, "label") <- "Q1. First question - Statement 2"
  attr(inconsistent_dat$Q2_1, "label") <- "Q2: Second question - Statement 1"

  inconsistent_result <- check_seps(inconsistent_dat)

  expect_true(inconsistent_result$consistency["var_name_sep"])
  expect_false(inconsistent_result$consistency["prefix_sep"])
  expect_true(inconsistent_result$consistency["statement_sep"])
  expect_length(inconsistent_result$issues, 1)

  # Test 4: Dataset with inconsistent statement separators
  inconsistent_dat <- data.frame(
    Q1_1 = c(1, 2, 3),
    Q1_2 = c(4, 5, 6),
    Q2_1 = c(7, 8, 9)
  )

  attr(inconsistent_dat$Q1_1, "label") <- "Q1: First question - Statement 1"
  attr(inconsistent_dat$Q1_2, "label") <- "Q1: First question : Statement 2"
  attr(inconsistent_dat$Q2_1, "label") <- "Q2: Second question - Statement 1"

  inconsistent_result <- check_seps(inconsistent_dat)

  expect_true(inconsistent_result$consistency["var_name_sep"])
  expect_true(inconsistent_result$consistency["prefix_sep"])
  expect_false(inconsistent_result$consistency["statement_sep"])
  expect_length(inconsistent_result$issues, 1)

  # Test 5: Non-data frame input
  expect_error(check_seps(c(1, 2, 3)), "Input must be a data frame")

  # Test 6: Dataset with no separators
  no_sep_dat <- data.frame(
    Q1 = c(1, 2, 3),
    Q2 = c(4, 5, 6)
  )

  attr(no_sep_dat$Q1, "label") <- "First question"
  attr(no_sep_dat$Q2, "label") <- "Second question"

  no_sep_result <- check_seps(no_sep_dat)

  expect_true(all(is.na(no_sep_result$separators)))
  expect_true(all(no_sep_result$consistency))
  expect_length(no_sep_result$issues, 0)
})

test_that("get_affix_df works", {
  test_string <- "Q1: Main question text - Statement 1 - Sub statement A"
  seps <- c(" - ", ": ")

  # Suffix test
  suffix_df <- get_affix_df(test_string, affix_type = "suffix", seps_priority = seps)
  expect_equal(nrow(suffix_df), 3)
  expect_named(suffix_df, c("sep_type", "location_found", "sep_count_from_end", "affix_found", "starts_with_cap"))
  expect_in(c("Statement 1 - Sub statement A", "Sub statement A", "Main question text - Statement 1 - Sub statement A"), suffix_df$affix_found)
  expect_equal(suffix_df$sep_count_from_end[suffix_df$affix_found == "Statement 1 - Sub statement A"], 2)
  expect_equal(suffix_df$sep_count_from_end[suffix_df$affix_found == "Sub statement A"], 1)
  expect_equal(suffix_df$sep_count_from_end[suffix_df$affix_found == "Main question text - Statement 1 - Sub statement A"], 1)
  expect_equal(suffix_df$sep_type[suffix_df$affix_found == "Statement 1 - Sub statement A"], " - ")
  expect_equal(suffix_df$sep_type[suffix_df$affix_found == "Sub statement A"], " - ")
  expect_equal(suffix_df$sep_type[suffix_df$affix_found == "Main question text - Statement 1 - Sub statement A"], ": ")

  # Prefix test
  prefix_df <- get_affix_df(test_string, affix_type = "prefix", seps_priority = seps)
  expect_equal(nrow(prefix_df), 3)
  expect_named(prefix_df, c("sep_type", "location_found", "sep_count_from_start", "affix_found", "starts_with_cap"))
  expect_in(c("Q1: Main question text", "Q1", "Q1: Main question text - Statement 1"), prefix_df$affix_found)
  expect_equal(prefix_df$sep_count_from_start[prefix_df$affix_found == "Q1: Main question text"], 1)
  expect_equal(prefix_df$sep_count_from_start[prefix_df$affix_found == "Q1"], 1)
  expect_equal(prefix_df$sep_count_from_start[prefix_df$affix_found == "Q1: Main question text - Statement 1"], 2)
  expect_equal(prefix_df$sep_type[prefix_df$affix_found == "Q1: Main question text"], " - ")
  expect_equal(prefix_df$sep_type[prefix_df$affix_found == "Q1"], ": ")
  expect_equal(prefix_df$sep_type[prefix_df$affix_found == "Q1: Main question text - Statement 1"], " - ")

  # Edge cases
  expect_true(is.na(get_affix_df("NoSeps", affix_type = "suffix", seps_priority = seps)))
  expect_error(get_affix_df("", affix_type = "suffix", seps_priority = seps))

  # Test filtering and sorting
  test_string_trailing_sep <- "Text with sep - "
  suffix_df_unfiltered <- get_affix_df(test_string_trailing_sep, affix_type = "suffix", seps_priority = c(" - "), filter_results = FALSE)
  expect_equal(nrow(suffix_df_unfiltered), 1)
  expect_equal(suffix_df_unfiltered$affix_found, "") # Empty suffix
  suffix_df_filtered <- get_affix_df(test_string_trailing_sep, affix_type = "suffix", seps_priority = c(" - "), filter_results = TRUE)
  expect_true(is.na(suffix_df_filtered)) # Should be removed by filter
})

test_that("get_updated_seps works", {
  # Setup inconsistent data
  inconsistent_dat <- data.frame(Q1_1 = 1, Q1.2 = 2, Q3_1 = 3)
  attr(inconsistent_dat$Q1_1, "label") <- "Q1: Question A - Statement 1"
  attr(inconsistent_dat$Q1.2, "label") <- "Q1. Question A - Statement 2" # Inconsistent prefix sep
  attr(inconsistent_dat$Q3_1, "label") <- "Q3: Question B : Statement X" # Inconsistent statement sep

  sep_analysis <- check_seps(inconsistent_dat)
  # Expected analysis: var_name inconsistent (_ vs .), prefix inconsistent (: vs .), statement inconsistent (- vs :)

  # Test updating to most common (should pick "_" for varname, ": " for prefix, " - " for statement if Q1_1 label is more common)
  # Let's assume "_" , ": ", " - " are most common based on analysis (adjust if check_seps logic differs)
  seps_to_use_common <- list(var_name_sep = "_", prefix_sep = ": ", statement_sep = " - ") # Manually set expected most common
  updated_seps_common <- get_updated_seps(inconsistent_dat, sep_analysis, seps_to_use = seps_to_use_common)

  expect_equal(updated_seps_common$new_variable_names, c("Q1_1", "Q1_2", "Q3_1")) # Q1.2 corrected
  expect_equal(updated_seps_common$new_variable_labels, c("Q1: Question A - Statement 1", "Q1: Question A - Statement 2", "Q3: Question B - Statement X")) # Prefix and statement seps corrected

  # Test updating to specific chosen seps
  seps_to_use_specific <- list(var_name_sep = ".", prefix_sep = ". ", statement_sep = " : ")
  updated_seps_specific <- get_updated_seps(inconsistent_dat, sep_analysis, seps_to_use = seps_to_use_specific)

  expect_equal(updated_seps_specific$new_variable_names, c("Q1.1", "Q1.2", "Q3.1"))
  expect_equal(updated_seps_specific$new_variable_labels, c("Q1. Question A : Statement 1", "Q1. Question A : Statement 2", "Q3. Question B : Statement X"))

  # Edge case: No separators present
  no_sep_dat <- data.frame(Q1 = 1, Q2 = 2)
  attr(no_sep_dat$Q1, "label") <- "Question 1"
  attr(no_sep_dat$Q2, "label") <- "Question 2"
  no_sep_analysis <- check_seps(no_sep_dat)
  no_sep_updated <- get_updated_seps(no_sep_dat, no_sep_analysis)
  expect_equal(no_sep_updated$new_variable_names, names(no_sep_dat)) # Should be unchanged
  expect_equal(no_sep_updated$new_variable_labels, c("Question 1", "Question 2")) # Should be unchanged
})

test_that("get_updated_seps correctly handles separator position indexing", {
  # This test specifically addresses the bug where matched_sep (position in sep_portion)
  # was incorrectly used as an index into the full string

  # Test case 1: Variable names with different lengths before separator
  # This ensures the fix correctly converts sep_portion indices to full string indices
  test_dat1 <- data.frame(
    A_1 = 1,           # Short prefix before sep
    ABC_2 = 2,         # Medium prefix before sep
    ABCDEF_3 = 3,      # Long prefix before sep
    Q1.test = 4        # Different sep to be updated
  )
  attr(test_dat1$A_1, "label") <- "A: Label 1 - Statement 1"
  attr(test_dat1$ABC_2, "label") <- "ABC: Label 2 - Statement 2"
  attr(test_dat1$ABCDEF_3, "label") <- "ABCDEF: Label 3 - Statement 3"
  attr(test_dat1$Q1.test, "label") <- "Q1. Label 4 - Statement 4"

  sep_analysis1 <- check_seps(test_dat1)
  seps_to_use1 <- list(var_name_sep = "_", prefix_sep = ": ", statement_sep = " - ")
  result1 <- get_updated_seps(test_dat1, sep_analysis1, seps_to_use = seps_to_use1)

  # All variable names should have consistent "_" separator
  expect_equal(result1$new_variable_names, c("A_1", "ABC_2", "ABCDEF_3", "Q1_test"))

  # All labels should have consistent separators
  expect_equal(result1$new_variable_labels,
               c("A: Label 1 - Statement 1",
                 "ABC: Label 2 - Statement 2",
                 "ABCDEF: Label 3 - Statement 3",
                 "Q1: Label 4 - Statement 4"))

  # Verify the separator was found at the correct position for each variable
  expect_equal(result1$var_name_seps_found, c("_", "_", "_", "."))

  # Test case 2: Mix of separators at different positions
  test_dat2 <- data.frame(
    X.A = 1,
    YY_B = 2,
    ZZZ.C = 3
  )
  attr(test_dat2$X.A, "label") <- "X. Prefix A - Stat"
  attr(test_dat2$YY_B, "label") <- "YY: Prefix B - Stat"
  attr(test_dat2$ZZZ.C, "label") <- "ZZZ. Prefix C - Stat"

  sep_analysis2 <- check_seps(test_dat2)
  seps_to_use2 <- list(var_name_sep = ".", prefix_sep = ". ", statement_sep = " - ")
  result2 <- get_updated_seps(test_dat2, sep_analysis2, seps_to_use = seps_to_use2)

  # Should correctly update separators regardless of prefix length
  expect_equal(result2$new_variable_names, c("X.A", "YY.B", "ZZZ.C"))
  expect_equal(result2$new_variable_labels,
               c("X. Prefix A - Stat",
                 "YY. Prefix B - Stat",
                 "ZZZ. Prefix C - Stat"))

  # Test case 3: Verify actual character replacement happens at correct position
  # Create a case where the bug would manifest if not fixed
  test_dat3 <- data.frame(LONG_VAR_NAME_test = 1)
  attr(test_dat3$LONG_VAR_NAME_test, "label") <- "LONG_VAR_NAME. Something"

  sep_analysis3 <- check_seps(test_dat3)
  seps_to_use3 <- list(var_name_sep = "_", prefix_sep = ": ", statement_sep = " - ")
  result3 <- get_updated_seps(test_dat3, sep_analysis3, seps_to_use = seps_to_use3)

  # The separator before "test" should be correctly identified and replaced
  expect_equal(result3$new_variable_names, "LONG_VAR_NAME_test")
  expect_equal(result3$var_name_seps_found, "_")

  # Ensure the prefix separator is also correctly updated
  expect_equal(result3$new_variable_labels, "LONG_VAR_NAME: Something")
  expect_equal(result3$prefix_seps_found, ".")
})

test_that("split_into_question_groups works", {

  n = 10
  seps_to_use <- list( variable_name_sep = "_", prefix_sep = ": ", statement_sep = " - ")

  # test 1 - split by class
  temp_dat <- data.frame("Q1_1" = haven::labelled(sample(c(0, 1, NA_real_), n, replace = TRUE), label = "Q1_1: Select all that apply - Statement 1"),
                         "Q1_2" = haven::labelled(sample(c(0, 1, NA_real_), n, replace = TRUE), label = "Q1_2: Select all that apply - Statement 2"),
                         "Q1_3" = haven::labelled(sample(c(0, 1, NA_real_), n, replace = TRUE), label = "Q1_3: Select all that apply - Statement 3"),
                         "Q1_4" = haven::labelled(sample(c("0", "1", NA_character_), n, replace = TRUE), label = "Q1_4: Select all that apply - Statement 5"))

  temp_dict <- create_dict(temp_dat, prefill = TRUE)
  temp_dict <- temp_dict[, !(names(temp_dict) %in% c("old_variable_names", "old_variable_labels", "old_value_labels"))]

  config1 <- list(splitbyclass = TRUE,
                  splitbynumlabelledvalues = FALSE,
                  splitbynoncontiguous = FALSE,
                  splitbycommonlabel = FALSE,
                  findlongest = FALSE,
                  min_lcs_length = 10,
                  min_common_strings = 3,
                  consistent_consecutive_mode = FALSE,
                  consecutive_range = 10,
                  variable_compare_mode = "complete")

  test_result <- split_into_question_groups(temp_dict, temp_dat, variables_to_process = NULL, seps_to_use = seps_to_use, ignorelabelbeforeprefix = TRUE, config = config1)

  expected_test_result <- c("Q1_a", "Q1_a", "Q1_a", "Q1_b")

  expect_equal(test_result$question_group, expected_test_result)

  # test 2 - split by numlabelledvalues
  temp_dat <- data.frame("Q1_1" = haven::labelled(sample(c(0, 1, NA_real_), n, replace = TRUE), label = "Q1_1: Select all that apply - Statement 1", labels = c("Not selected" = 0, "Selected" = 1)),
                         "Q1_2" = haven::labelled(sample(c(0, 1, NA_real_), n, replace = TRUE), label = "Q1_2: Select all that apply - Statement 2", labels = c("Not selected" = 0, "Selected" = 1)),
                         "Q1_3" = haven::labelled(sample(c(0, 1, NA_real_), n, replace = TRUE), label = "Q1_3: Select all that apply - Statement 3", labels = c("Not selected" = 0, "Selected" = 1)),
                         "Q1_4" = haven::labelled(sample(c(0, 1, NA_real_), n, replace = TRUE), label = "Q1_4: Select all that apply - Statement 4"))

  temp_dict <- create_dict(temp_dat, prefill = TRUE)
  temp_dict <- temp_dict[, !(names(temp_dict) %in% c("old_variable_names", "old_variable_labels", "old_value_labels"))]

  config2 <- list(splitbyclass = FALSE,
                  splitbynumlabelledvalues = TRUE,
                  splitbynoncontiguous = FALSE,
                  splitbycommonlabel = FALSE,
                  findlongest = FALSE,
                  min_lcs_length = 10,
                  min_common_strings = 3,
                  consistent_consecutive_mode = FALSE,
                  consecutive_range = 10,
                  variable_compare_mode = "complete")

  test_result <- split_into_question_groups(temp_dict, temp_dat, variables_to_process = NULL, seps_to_use = seps_to_use, ignorelabelbeforeprefix = TRUE, config = config2)

  expected_test_result <- c("Q1_a", "Q1_a", "Q1_a", "Q1_b")

  expect_equal(test_result$question_group, expected_test_result)

  # test 3 - split by noncontiguous
  temp_dat <- data.frame("Q1_1" = haven::labelled(sample(c(0, 1, NA_real_), n, replace = TRUE), label = "Q1_1: Select all that apply - Statement 1"),
                         "Q1_2" = haven::labelled(sample(c(0, 1, NA_real_), n, replace = TRUE), label = "Q1_2: Select all that apply - Statement 2"),
                         "Q2_1" = haven::labelled(sample(c(0, 1, NA_real_), n, replace = TRUE), label = "Q2_1: Select all that apply - Statement 1"),
                         "Q1_3" = haven::labelled(sample(c(0, 1, NA_real_), n, replace = TRUE), label = "Q1_3: Select all that apply - Statement 3"))

  temp_dict <- create_dict(temp_dat, prefill = TRUE)
  temp_dict <- temp_dict[, !(names(temp_dict) %in% c("old_variable_names", "old_variable_labels", "old_value_labels"))]

  config3 <- list (splitbyclass = FALSE,
                   splitbynumlabelledvalues = FALSE,
                   splitbynoncontiguous = TRUE,
                   splitbycommonlabel = FALSE,
                   findlongest = FALSE,
                   min_lcs_length = 10,
                   min_common_strings = 3,
                   consistent_consecutive_mode = FALSE,
                   consecutive_range = 10,
                   variable_compare_mode = "complete")

  test_result <- split_into_question_groups(temp_dict, temp_dat, variables_to_process = NULL, seps_to_use = seps_to_use, ignorelabelbeforeprefix = TRUE, config = config3)

  expected_test_result <- c("Q1_a", "Q1_a", "Q2_a", "Q1_b")

  expect_equal(test_result$question_group, expected_test_result)

  # test 4 - split by commonlabel, no findlongest, no ignoreprefix
  temp_dat <- data.frame("Q1_1" = haven::labelled(sample(c(0, 1, NA_real_), n, replace = TRUE), label = "Q1_1: Select all that apply - Statement 1"),
                         "Q1_2" = haven::labelled(sample(c(0, 1, NA_real_), n, replace = TRUE), label = "Q1_2: Select all that apply - Statement 2"),
                         "Q1_3" = haven::labelled(sample(c(0, 1, NA_real_), n, replace = TRUE), label = "Q1_3: Select all that apply - Statement 3"),
                         "Q1_4" = haven::labelled(sample(c(0, 1, NA_real_), n, replace = TRUE), label = "Q1_4: Click one of these - Statement 4"))

  temp_dict <- create_dict(temp_dat, prefill = TRUE)
  temp_dict <- temp_dict[, !(names(temp_dict) %in% c("old_variable_names", "old_variable_labels", "old_value_labels"))]

  config4 <- list(splitbyclass = FALSE,
                  splitbynumlabelledvalues = FALSE,
                  splitbynoncontiguous = FALSE,
                  splitbycommonlabel = TRUE,
                  findlongest = FALSE,
                  min_lcs_length = 10,
                  min_common_strings = 3,
                  consistent_consecutive_mode = FALSE,
                  consecutive_range = 10,
                  variable_compare_mode = "complete")

  test_result <- split_into_question_groups(temp_dict, temp_dat, variables_to_process = NULL, seps_to_use = seps_to_use, ignorelabelbeforeprefix = FALSE, config = config4)

  expected_test_result <- c("Q1_a", "Q1_b", "Q1_c", "Q1_d")

  expect_equal(test_result$question_group, expected_test_result)

  # test 5 - split by commonlabel, no findlongest, do ignoreprefix
  temp_dat <- data.frame("Q1_1" = haven::labelled(sample(c(0, 1, NA_real_), n, replace = TRUE), label = "Q1_1: Select all that apply - Statement 1"),
                         "Q1_2" = haven::labelled(sample(c(0, 1, NA_real_), n, replace = TRUE), label = "Q1_2: Select all that apply - Statement 2"),
                         "Q1_3" = haven::labelled(sample(c(0, 1, NA_real_), n, replace = TRUE), label = "Q1_3: Select all that apply - Statement 3"),
                         "Q1_4" = haven::labelled(sample(c(0, 1, NA_real_), n, replace = TRUE), label = "Q1_4: Click one of these - Statement 4"))

  temp_dict <- create_dict(temp_dat, prefill = TRUE)
  temp_dict <- temp_dict[, !(names(temp_dict) %in% c("old_variable_names", "old_variable_labels", "old_value_labels"))]

  config5 <- list(splitbyclass = FALSE,
                  splitbynumlabelledvalues = FALSE,
                  splitbynoncontiguous = FALSE,
                  splitbycommonlabel = TRUE,
                  findlongest = FALSE,
                  min_lcs_length = 10,
                  min_common_strings = 3,
                  consistent_consecutive_mode = FALSE,
                  consecutive_range = 10,
                  variable_compare_mode = "complete")

  test_result <- split_into_question_groups(temp_dict, temp_dat, variables_to_process = NULL, seps_to_use = seps_to_use, ignorelabelbeforeprefix = TRUE, config = config5)

  expected_test_result <- c("Q1_a", "Q1_a", "Q1_a", "Q1_b")

  expect_equal(test_result$question_group, expected_test_result)

  # test 6 - split by commonlabel, do findlongest
  temp_dat <- data.frame("Q1_1" = haven::labelled(sample(c(0, 1, NA_real_), n, replace = TRUE), label = "Q1_1: Select all that apply Statement 1"),
                         "Q1_2" = haven::labelled(sample(c(0, 1, NA_real_), n, replace = TRUE), label = "Q1_2: Select all that apply Statement 2"),
                         "Q1_3" = haven::labelled(sample(c(0, 1, NA_real_), n, replace = TRUE), label = "Q1_3: Select all that apply Statement 3"),
                         "Q1_4" = haven::labelled(sample(c(0, 1, NA_real_), n, replace = TRUE), label = "Q1_4: Click one of these Statement 4"))

  temp_dict <- create_dict(temp_dat, prefill = TRUE)
  temp_dict <- temp_dict[, !(names(temp_dict) %in% c("old_variable_names", "old_variable_labels", "old_value_labels"))]

  config6 <- list(splitbyclass = FALSE,
                  splitbynumlabelledvalues = FALSE,
                  splitbynoncontiguous = FALSE,
                  splitbycommonlabel = TRUE,
                  findlongest = TRUE,
                  min_lcs_length = 10,
                  min_common_strings = 3,
                  consistent_consecutive_mode = FALSE,
                  consecutive_range = 10,
                  variable_compare_mode = "complete")

  test_result <- split_into_question_groups(temp_dict, temp_dat, variables_to_process = NULL, seps_to_use = seps_to_use, ignorelabelbeforeprefix = TRUE, config = config6)

  expected_test_result <- c("Q1_a", "Q1_a", "Q1_a", "Q1_b")

  expect_equal(test_result$question_group, expected_test_result)

})

test_that("get_longest_common_substring works", {
  expect_equal(get_longest_common_substring("boat", "boar", fromstart=TRUE)[1], "boa")
  expect_equal(attr(get_longest_common_substring("boat", "boar", fromstart=TRUE), "lcs_distance"), 3)

  expect_equal(get_longest_common_substring("banana", "bandana", fromstart=TRUE)[1], "ban")
  expect_equal(get_longest_common_substring("prefix_statement1", "prefix_statement2", fromstart=TRUE)[1], "prefix_statement")

  # Test fromstart = FALSE
  expect_equal(get_longest_common_substring("banana", "cabana", fromstart=FALSE)[1], "bana")
  expect_equal(attr(get_longest_common_substring("banana", "cabana", fromstart=FALSE), "lcs_distance"), 4)
  expect_equal(get_longest_common_substring("abcde", "fghij", fromstart=FALSE)[1], "")
  expect_equal(get_longest_common_substring("testing", "test", fromstart=FALSE)[1], "test")

  # Edge cases
  expect_equal(get_longest_common_substring("", "abc", fromstart=TRUE)[1], "")
  expect_equal(get_longest_common_substring("abc", "", fromstart=FALSE)[1], "")
  expect_equal(get_longest_common_substring("abc", "abc", fromstart=TRUE)[1], "abc")
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

test_that("create_questions_dict works", {
  temp_dpdict <- create_dict_with_metadata(get_big_test_dat(n=10))
  questions_dict <- create_questions_dict(NULL, temp_dpdict, editfirst = FALSE)

  expect_true(is.data.frame(questions_dict))
  expect_named(questions_dict, c("question_group", "question_lcs", "question_alias", "question_description", "question_folder"))
  expect_equal(nrow(questions_dict), length(unique(temp_dpdict$question_group)))
  expect_equal(questions_dict$question_group, unique(temp_dpdict$question_group))
  expect_equal(questions_dict$question_alias, questions_dict$question_group) # Default behaviour

  # Test error handling
  expect_error(create_questions_dict(NULL, temp_dpdict[, "variable_names", drop = FALSE]), "dpdict must include columns 'question_group'")

  # Test behaviour when question_lcs is not present initially (should calculate it)
  temp_dpdict_no_lcs <- temp_dpdict[, !(names(temp_dpdict) %in% "question_lcs")]
  questions_dict_no_lcs <- create_questions_dict(NULL, temp_dpdict_no_lcs, editfirst = FALSE)
  expect_true("question_lcs" %in% names(questions_dict_no_lcs))
  expect_false(any(is.na(questions_dict_no_lcs$question_lcs))) # Should have calculated LCS
})

test_that("get_questions_dict works", {
  temp_dpdict <- create_dict_with_metadata(get_big_test_dat(n=10))
  questions_dict <- get_questions_dict(temp_dpdict)

  expect_true(is.data.frame(questions_dict))
  expect_equal(nrow(questions_dict), length(unique(temp_dpdict$question_group)))
  expect_true(all(c("question_group", "questiontype", "question_alias", "question_description") %in% names(questions_dict)))

  # Check with survey_data object
  survey_obj <- create_survey_data(get_big_test_dat(n=10))
  questions_dict_survey <- get_questions_dict(survey_obj)
  expect_equal(questions_dict, questions_dict_survey)

  # Error handling
  expect_error(get_questions_dict(data.frame(a=1)), "dpdict must at a minimum have a question group variable")
})

test_that("split_grid_labels works", {
  temp_dpdict <- create_dict_with_metadata(get_big_test_dat(n=10))
  alias_to_split <- "labelledmultiresponsegrid_a" # From the example dataset structure
  example_stem <- "statement 1" # Stem to move
  count_before_repeat <- 2 # Number of attributes per statement

  # Find the original alias and suffix for a specific variable in the grid
  original_alias <- temp_dpdict$question_alias[temp_dpdict$question_alias == alias_to_split][1]
  original_suffix <- temp_dpdict$question_suffix[temp_dpdict$question_alias == alias_to_split][1] # e.g., "statement 1 - attribute 1"

  updated_dpdict <- split_grid_labels(temp_dpdict, alias_to_split, example_stem, count_before_repeat)

  # Check the first variable that was part of the split group
  updated_var_row <- updated_dpdict[temp_dpdict$question_alias == alias_to_split, ][1, ]

  # New alias should include the stem
  expect_equal(updated_var_row$question_alias, paste0(original_alias, " - ", example_stem))
  # New suffix should be what was after the stem
  expect_equal(updated_var_row$question_suffix, gsub(paste0(example_stem, " - "), "", original_suffix))

  # Check another variable in the same original grid
  original_suffix_next <- temp_dpdict$question_suffix[temp_dpdict$question_alias == alias_to_split][count_before_repeat + 1] # e.g., "statement 2 - attribute 1"
  next_stem <- gsub(" - .*", "", original_suffix_next) # "statement 2"
  updated_var_row_next <- updated_dpdict[temp_dpdict$question_alias == alias_to_split, ][count_before_repeat + 1, ]
  expect_equal(updated_var_row_next$question_alias, paste0(original_alias, " - ", next_stem))
  expect_equal(updated_var_row_next$question_suffix, gsub(paste0(next_stem, " - "), "", original_suffix_next))

  # Error handling
  expect_error(split_grid_labels(temp_dpdict, "nonexistent_alias", example_stem, count_before_repeat), "`alias_to_split` .* not found")
  expect_warning(split_grid_labels(temp_dpdict, alias_to_split, example_stem, 100), "`count_before_repeat` is greater than the number of variables")
})

# Test survey data object creation and validation
test_that("create_survey_data works", {
  dat <- get_minimal_labelled_test_dat()
  dpdict <- create_dict_with_metadata(dat)

  # Create with explicit dpdict
  survey_obj1 <- create_survey_data(dat, dpdict)
  expect_s3_class(survey_obj1, "survey_data")
  expect_equal(survey_obj1$dat, dat)
  expect_equal(survey_obj1$dpdict, dpdict)

  # Create with automatic dpdict generation
  survey_obj2 <- create_survey_data(dat)
  expect_s3_class(survey_obj2, "survey_data")
  expect_equal(survey_obj2$dat, dat)
  expect_true(is.data.frame(survey_obj2$dpdict))
  expect_equal(nrow(survey_obj2$dpdict), ncol(dat)) # dpdict created

  # Error handling
  expect_error(create_survey_data(as.matrix(dat)), "'dat' must be a data frame")
  expect_error(create_survey_data(dat, as.list(dpdict)), "'dpdict' must be a data frame")
})

test_that("is.survey_data works", {
  survey_obj <- create_survey_data(get_minimal_labelled_test_dat())
  expect_true(is.survey_data(survey_obj))
  expect_false(is.survey_data(survey_obj$dat))
  expect_false(is.survey_data(list(dat=1, dpdict=2)))
})

test_that("validate_survey_data works", {
  survey_obj <- create_survey_data(get_minimal_labelled_test_dat())
  expect_true(validate_survey_data(survey_obj))

  # Test invalid structures
  bad_obj1 <- list(dat=survey_obj$dat, dpdict=as.matrix(survey_obj$dpdict))
  class(bad_obj1) <- "survey_data"
  expect_error(validate_survey_data(bad_obj1), "Both 'dat' and 'dpdict' must be data frames")

  bad_obj2 <- survey_obj
  bad_obj2$dpdict <- bad_obj2$dpdict[-1, ] # Misalign dpdict
  expect_warning(expect_error(validate_survey_data(bad_obj2), "Misalignment between dat and dpdict"))

  bad_obj3 <- survey_obj
  names(bad_obj3$dat)[2] <- names(bad_obj3$dat)[1] # Duplicate names
  bad_obj3$dpdict$variable_names[2] <- bad_obj3$dpdict$variable_names[1] # Duplicate names
  expect_warning(expect_error(validate_survey_data(bad_obj3), "Duplicate variable names"))
})

# Test survey data object methods
test_that("print.survey_data works", {
  survey_obj <- create_survey_data(get_big_test_dat(n=10))
  expect_output(print(survey_obj), "Survey data object:")
  expect_invisible(print(survey_obj)) # Should return object invisibly
})

test_that("[.survey_data works", {
  survey_obj <- create_survey_data(get_big_test_dat(n=5))
  original_names <- names(survey_obj$dat)
  original_nrow <- nrow(survey_obj$dat)

  # Row subsetting
  sub_row <- survey_obj[1:3, ]
  expect_s3_class(sub_row, "survey_data")
  expect_equal(nrow(sub_row$dat), 3)
  expect_equal(ncol(sub_row$dat), ncol(survey_obj$dat))
  expect_equal(nrow(sub_row$dpdict), ncol(survey_obj$dat)) # dpdict rows shouldn't change

  # Column subsetting by index
  sub_col_idx <- survey_obj[, 1:2]
  expect_s3_class(sub_col_idx, "survey_data")
  expect_equal(nrow(sub_col_idx$dat), original_nrow)
  expect_equal(ncol(sub_col_idx$dat), 2)
  expect_equal(names(sub_col_idx$dat), original_names[1:2])
  expect_equal(nrow(sub_col_idx$dpdict), 2)
  expect_equal(sub_col_idx$dpdict$variable_names, original_names[1:2])

  # Column subsetting by name
  sub_col_name <- survey_obj[, c(original_names[1], original_names[3])]
  expect_s3_class(sub_col_name, "survey_data")
  expect_equal(ncol(sub_col_name$dat), 2)
  expect_equal(names(sub_col_name$dat), c(original_names[1], original_names[3]))
  expect_equal(nrow(sub_col_name$dpdict), 2)
  expect_equal(sub_col_name$dpdict$variable_names, c(original_names[1], original_names[3]))

  # Combined subsetting
  sub_both <- survey_obj[1, 1, drop = FALSE] # Ensure drop=FALSE keeps structure
  expect_s3_class(sub_both, "survey_data")
  expect_equal(nrow(sub_both$dat), 1)
  expect_equal(ncol(sub_both$dat), 1)
  expect_equal(nrow(sub_both$dpdict), 1)

  # Error handling
  expect_error(survey_obj[, 1000], "Column subscript out of bounds")
  expect_error(survey_obj[, "nonexistent_col"], "Unknown column names")
})

test_that("[[.survey_data works", {
  survey_obj <- create_survey_data(get_minimal_labelled_test_dat())
  # Extract by name
  expect_equal(survey_obj[["uid"]], survey_obj$dat[["uid"]])
  # Extract by index
  expect_equal(survey_obj[[2]], survey_obj$dat[[2]])

  # Error handling
  expect_error(survey_obj[[100]], "Column subscript out of bounds")
  expect_error(survey_obj[["nonexistent"]], "Unknown column name")
})

test_that("filter.survey_data works", {
  survey_obj <- create_survey_data(get_minimal_labelled_test_dat()) # Assumes 'uid' column exists
  filtered_obj <- filter(survey_obj, uid > 1)

  expect_s3_class(filtered_obj, "survey_data")
  expect_equal(nrow(filtered_obj$dat), nrow(survey_obj$dat) - 1)
  expect_equal(ncol(filtered_obj$dat), ncol(survey_obj$dat))
  expect_equal(filtered_obj$dpdict, survey_obj$dpdict) # dpdict should be unchanged
})

test_that("select.survey_data works", {
  survey_obj <- create_survey_data(get_minimal_labelled_test_dat())
  original_names <- names(survey_obj$dat)

  # Select columns
  selected_obj <- select(survey_obj, uid, csat)
  expect_s3_class(selected_obj, "survey_data")
  expect_equal(names(selected_obj$dat), c("uid", "csat"))
  expect_equal(nrow(selected_obj$dpdict), 2)
  expect_equal(selected_obj$dpdict$variable_names, c("uid", "csat"))

  # Select and rename
  renamed_obj <- select(survey_obj, user_id = uid, csat)
  expect_s3_class(renamed_obj, "survey_data")
  expect_equal(names(renamed_obj$dat), c("user_id", "csat"))
  expect_equal(nrow(renamed_obj$dpdict), 2)
  expect_equal(renamed_obj$dpdict$variable_names, c("user_id", "csat"))
  # Check that the row corresponding to 'uid' in the original dpdict now has name 'user_id'
  expect_equal(renamed_obj$dpdict$variable_labels[renamed_obj$dpdict$variable_names == "user_id"],
               survey_obj$dpdict$variable_labels[survey_obj$dpdict$variable_names == "uid"])
})

test_that("mutate.survey_data works", {
  survey_obj <- create_survey_data(get_minimal_labelled_test_dat()) # Has 'uid' column

  # Add a new column with a custom label
  custom_label <- "Multiplied ID Value"
  mutated_obj_custom_label <- mutate(survey_obj, new_col = realiselabelled_vec(uid * 10, variable_label = custom_label))

  expect_s3_class(mutated_obj_custom_label, "survey_data")
  expect_true("new_col" %in% names(mutated_obj_custom_label$dat))
  expect_equal(sjlabelled::remove_label(mutated_obj_custom_label$dat$new_col),
               sjlabelled::remove_label(survey_obj$dat$uid * 10))

  # Check if custom label was applied to the variable
  expect_equal(sjlabelled::get_label(mutated_obj_custom_label$dat$new_col), custom_label)

  # Check if custom label was recorded in dpdict
  new_row_dpdict <- mutated_obj_custom_label$dpdict[mutated_obj_custom_label$dpdict$variable_names == "new_col", ]
  expect_equal(new_row_dpdict$variable_labels, custom_label)

  # Modify an existing column with a new label
  new_uid_label <- "Modified User ID"
  mutated_obj_relabel <- mutate(survey_obj, uid = realiselabelled_vec(uid + 1, variable_label = new_uid_label))

  expect_s3_class(mutated_obj_relabel, "survey_data")
  # Corrected test - compare values after removing labels
  expect_equal(sjlabelled::remove_label(mutated_obj_relabel$dat$uid),
               sjlabelled::remove_label(survey_obj$dat$uid + 1))

  # Check if the new label was applied to the modified variable
  expect_equal(sjlabelled::get_label(mutated_obj_relabel$dat$uid), new_uid_label)

  # Check if the new label was recorded in dpdict
  modified_row_dpdict <- mutated_obj_relabel$dpdict[mutated_obj_relabel$dpdict$variable_names == "uid", ]
  expect_equal(modified_row_dpdict$variable_labels, new_uid_label)

  # Test multiple operations in one mutate call
  # Only run if get_minimal_labelled_test_dat() has at least one column of each type
  skip_if_not(all(c("uid", "csat", "nps_response") %in% names(survey_obj$dat)),
              "Test data doesn't have required columns")

  multi_obj <- mutate(survey_obj,
                      uid = realiselabelled_vec(uid + 1, variable_label = "New UID Label"),
                      new_var1 = realiselabelled_vec(csat * 2, variable_label = "Double CSAT"),
                      new_var2 = nps_response * 3)  # No custom label

  expect_true(all(c("uid", "new_var1", "new_var2") %in% names(multi_obj$dat)))
  expect_equal(sjlabelled::get_label(multi_obj$dat$uid), "New UID Label")
  expect_equal(sjlabelled::get_label(multi_obj$dat$new_var1), "Double CSAT")
  expect_equal(sjlabelled::get_label(multi_obj$dat$new_var2), "NPS response (1)")  # Default label
})

# Test: Value labels preserved with !!!-spliced variables
test_that("mutate preserves value labels from spliced variables", {

  # Create original survey data
  original_dat <- data.frame(
    id = 1:3,
    existing_var = c(1, 2, 1)
  )
  attr(original_dat$existing_var, "label") <- "Existing Variable"
  attr(original_dat$existing_var, "labels") <- c("No" = 1, "Yes" = 2)

  survey_obj <- suppressWarnings(create_survey_data(original_dat))

  # Create a named list with value labels (like a1a_wide)
  new_vars_list <- list(
    new_var1 = c(2, 1, 2),
    new_var2 = c(1, 1, 2)
  )

  # Add value labels to the new variables
  attr(new_vars_list$new_var1, "label") <- "New Variable 1"
  attr(new_vars_list$new_var1, "labels") <- c("Low" = 1, "High" = 2)

  attr(new_vars_list$new_var2, "label") <- "New Variable 2"
  attr(new_vars_list$new_var2, "labels") <- c("Bad" = 1, "Good" = 2)

  # Test the mutation with splicing
  result <- suppressWarnings(survey_obj %>%
    mutate(!!!new_vars_list))

  # Check that new variables exist
  expect_true("new_var1" %in% names(result$dat))
  expect_true("new_var2" %in% names(result$dat))

  # Check that value labels are preserved
  expect_equal(attr(result$dat$new_var1, "labels"), c("Low" = 1, "High" = 2))
  expect_equal(attr(result$dat$new_var2, "labels"), c("Bad" = 1, "Good" = 2))

  # Check that variable labels are preserved
  expect_equal(attr(result$dat$new_var1, "label"), "New Variable 1")
  expect_equal(attr(result$dat$new_var2, "label"), "New Variable 2")

  # Check that original variable labels are still intact
  expect_equal(attr(result$dat$existing_var, "labels"), c("No" = 1, "Yes" = 2))
  expect_equal(attr(result$dat$existing_var, "label"), "Existing Variable")

  # Check that dpdict is updated correctly
  expect_true("new_var1" %in% result$dpdict$variable_names)
  expect_true("new_var2" %in% result$dpdict$variable_names)
  expect_equal(result$dpdict$variable_labels[result$dpdict$variable_names == "new_var1"], "New Variable 1")
  expect_equal(result$dpdict$variable_labels[result$dpdict$variable_names == "new_var2"], "New Variable 2")
})


test_that("mutate updates dpdict when transforming existing labeled variables", {
  # Create initial survey data with labeled variables
  dat <- get_minimal_labelled_test_dat(50)
  survey_obj <- create_survey_data(dat)

  # Verify original csat has value labels in both data and dpdict
  original_csat_labels <- sjlabelled::get_labels(survey_obj$dat$csat, attr.only = TRUE, values = "as.name")
  csat_idx <- which(survey_obj$dpdict$variable_names == "csat")
  original_dpdict_labels <- survey_obj$dpdict$value_labels[[csat_idx]]

  expect_false(all(is.na(original_csat_labels)))
  expect_false(all(is.na(original_dpdict_labels)))

  # Use mutate to create a copy of csat (simple assignment that should preserve labels)
  survey_obj <- suppressWarnings(survey_obj %>%
    dplyr::mutate(csat_copy = csat))  # Simple assignment should preserve labels

  # Check that value labels are preserved in the data
  copy_labels_in_data <- sjlabelled::get_labels(survey_obj$dat$csat_copy, attr.only = TRUE, values = "as.name")
  expect_false(all(is.na(copy_labels_in_data)))
  expect_equal(copy_labels_in_data, original_csat_labels)

  # Check that value labels are captured in dpdict
  copy_idx <- which(survey_obj$dpdict$variable_names == "csat_copy")
  copy_labels_in_dpdict <- survey_obj$dpdict$value_labels[[copy_idx]]

  expect_false(all(is.na(copy_labels_in_dpdict)))
  expect_equal(copy_labels_in_dpdict, copy_labels_in_data)

  # Check that has_value_labels is TRUE
  expect_true(survey_obj$dpdict$has_value_labels[copy_idx])
})


test_that("left_join works for survey_data objects", {
  # Create base survey_data object
  base_data <- get_minimal_labelled_test_dat()
  survey1 <- create_survey_data(base_data)

  # Test 1: Join with another survey_data object
  survey2_dat <- data.frame(
    uid = c(1, 3, 5),
    new_var = c("a", "b", "c"),
    stringsAsFactors = FALSE
  )
  attr(survey2_dat$new_var, "label") <- "New Variable"
  survey2 <- suppressWarnings(create_survey_data(survey2_dat))

  result1 <- left_join(survey1, survey2, by = "uid")

  expect_s3_class(result1, "survey_data")
  expect_true("new_var" %in% names(result1$dat))
  expect_equal(nrow(result1$dat), nrow(survey1$dat))
  expect_equal(attr(result1$dat$new_var, "label"), "New Variable")
  expect_true("new_var" %in% result1$dpdict$variable_names)

  # Test 2: Join with regular data frame
  df2 <- data.frame(
    uid = 1:10,
    score = rnorm(10)
  )

  result2 <- left_join(survey1, df2, by = "uid")

  expect_s3_class(result2, "survey_data")
  expect_true("score" %in% names(result2$dat))
  expect_true("score" %in% result2$dpdict$variable_names)
  expect_equal(result2$dpdict$variable_labels[result2$dpdict$variable_names == "score"], "score")

  # Test 3: Handle column conflicts
  survey3_dat <- data.frame(
    uid = 1:5,
    csat = 5:1  # Conflicts with existing csat
  )
  attr(survey3_dat$csat, "label") <- "Customer Satisfaction (New)"
  survey3 <- suppressWarnings(create_survey_data(survey3_dat))

  expect_message(
    result3 <- left_join(survey1, survey3, by = "uid"),
    "naming conflicts"
  )

  expect_true("csat.x" %in% names(result3$dat))
  expect_true("csat.y" %in% names(result3$dat))
  expect_true(all(c("csat.x", "csat.y") %in% result3$dpdict$variable_names))
  expect_false("csat" %in% result3$dpdict$variable_names)

  # Check that the suffixed versions have appropriate labels
  expect_true(grepl("\\(x\\)", result3$dpdict$variable_labels[result3$dpdict$variable_names == "csat.x"]))
  expect_true(grepl("\\(y\\)", result3$dpdict$variable_labels[result3$dpdict$variable_names == "csat.y"]))
})
