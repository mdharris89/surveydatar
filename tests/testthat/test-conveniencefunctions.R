test_that("any_grepl works", {

  # Error cases
  expect_error(any_grepl(1:3, c("big cat", "small dog", "red fish")), "must be character vectors")
  expect_error(any_grepl(c("cat", "dog"), 1:3), "must be character vectors")
  expect_error(any_grepl(character(0), c("cat", "dog")), "must have length > 0")
  expect_error(any_grepl(c("cat", "dog"), character(0)), "must have length > 0")

  # Simple match
  expect_equal(any_grepl(c("cat"), c("cat dog", "hat dog")), c(TRUE, FALSE))

  # Test with multiple patterns, none found
  expect_equal(any_grepl(c("apple", "pear"), c("cat dog", "hat dog")), c(FALSE, FALSE))

  # Overlapping patterns
  expect_equal(any_grepl(c("dog", "do"), c("dog", "cat", "do")), c(TRUE, FALSE, TRUE))

  # NA handling
  expect_equal(any_grepl(c("cat", NA, "dog"), c("big cat", NA, "small dog", "red fish", NA)), c(TRUE, TRUE, TRUE, FALSE, TRUE))

  # Empty string handling
  expect_equal(any_grepl(c("cat", "", "dog"), c("big cat", "", "small dog", "red fish", "")), c(TRUE, TRUE, TRUE, FALSE, TRUE))

  # Only NA in pattern_vector
  expect_equal(any_grepl(NA_character_, c("big cat", NA, "small dog", "red fish", NA)), c(FALSE, TRUE, FALSE, FALSE, TRUE))

  # Multiple patterns, multiple found
  expect_equal(any_grepl(c("cat", "shoe"), c("shoe dog", "hat dog", "cat dog")), c(TRUE, FALSE, TRUE))

  # Case sensitivity
  expect_equal(any_grepl(c("CAT"), c("cat", "Cat", "CAT"), ignore.case = FALSE), c(FALSE, FALSE, TRUE))
  expect_equal(any_grepl(c("CAT"), c("cat", "Cat", "CAT"), ignore.case = TRUE), c(TRUE, TRUE, TRUE))

  # Special characters
  expect_equal(any_grepl(c("^cat"), c("cat", "1 cat", "cattle")), c(TRUE, FALSE, TRUE))
  expect_equal(any_grepl(c("^cat"), c("cat", "1 cat", "cattle"), fixed = TRUE), c(FALSE, FALSE, FALSE))

  # perl
  expect_equal(any_grepl("foo(?=bar)", c("foobar", "foobaz", "barfoo"), perl = TRUE), c(TRUE, FALSE, FALSE))
})

test_that("any_gsub works", {

  # Error cases
  expect_error(any_gsub(1:3, "ANIMAL", c("big cat", "small dog", "red fish"), trimws_at_end = TRUE), "must be character vectors")
  expect_error(any_gsub(c("cat", "dog"), "ANIMAL", 1:3, trimws_at_end = TRUE), "must be character vectors")
  expect_error(any_gsub(character(0), "ANIMAL", c("cat", "dog"), trimws_at_end = TRUE), "must have length > 0")
  expect_error(any_gsub(c("cat", "dog"), "ANIMAL", character(0), trimws_at_end = TRUE), "must have length > 0")

  # Simple match
  expect_equal(any_gsub(c("cat"), "", c("cat dog", "hat dog"), trimws_at_end = TRUE), c("dog", "hat dog"))

  # Multiple patterns, multiple found
  expect_equal(any_gsub(c("cat", "shoe"), "", c("shoe dog", "hat dog", "cat dog"), trimws_at_end = TRUE), c("dog", "hat dog", "dog"))

  # Test with multiple patterns, none found
  expect_equal(any_gsub(c("apple", "pear"), "", c("cat dog", "hat dog"), trimws_at_end = TRUE), c("cat dog", "hat dog"))

  # Overlapping patterns
  expect_equal(any_gsub(c("at", "dog"), "", c("shoe dog", "hat dog", "cat dog"), trimws_at_end = TRUE), c("shoe", "h", "c"))

  # NA handling
  expect_equal(
    any_gsub(c("cat", NA, "dog"), "REPLACED", c("big cat", NA, "small dog", "red fish", NA), trimws_at_end = TRUE),
    c("big REPLACED", "REPLACED", "small REPLACED", "red fish", "REPLACED")
  )

  # Empty string handling
  expect_equal(
    any_gsub(c("cat", "", "dog"), "REPLACED", c("big cat", "", "small dog", "red fish", ""), trimws_at_end = TRUE),
    c("big REPLACED", "REPLACED", "small REPLACED", "red fish", "REPLACED")
  )

  # Only NA in pattern_vector
  expect_equal(
    any_gsub(NA_character_, "REPLACED", c("big cat", NA, "small dog", "red fish", NA), trimws_at_end = TRUE),
    c("big cat", "REPLACED", "small dog", "red fish", "REPLACED")
  )

  # Only empty string in pattern_vector
  expect_equal(
    any_gsub("", "REPLACED", c("big cat", "", "small dog", "red fish", ""), trimws_at_end = TRUE),
    c("big cat", "REPLACED", "small dog", "red fish", "REPLACED")
  )

  # Complex case with mix of NAs, empty strings, and regular patterns
  expect_equal(
    any_gsub(c("cat", NA, "", "fish"), "REPLACED", c("big cat", NA, "", "small dog", "red fish", NA, ""), trimws_at_end = TRUE),
    c("big REPLACED", "REPLACED", "REPLACED", "small dog", "red REPLACED", "REPLACED", "REPLACED")
  )

  # Trim option false
  expect_equal(
    any_gsub(c("cat", "dog"), "ANIMAL ", c("big cat ", "small dog", "red fish "), trimws_at_end = FALSE),
    c("big ANIMAL  ", "small ANIMAL ", "red fish ")
  )

  # Case sensitivity
  expect_equal(any_gsub(c("CAT"), "replacement", c("cat", "Cat", "CAT"), trimws_at_end = TRUE, ignore.case = FALSE), c("cat", "Cat", "replacement"))
  expect_equal(any_gsub(c("CAT"), "replacement", c("cat", "Cat", "CAT"), trimws_at_end = TRUE, ignore.case = TRUE), c("replacement", "replacement", "replacement"))

  # Special characters
  expect_equal(any_gsub(c("^cat"), "", c("cat", "1 cat", "cattle"), trimws_at_end = TRUE), c("", "1 cat", "tle"))
  expect_equal(any_gsub(c("^cat"), "", c("cat", "1 cat", "cattle"), trimws_at_end = TRUE, fixed = TRUE), c("cat", "1 cat", "cattle"))

  # perl
  expect_equal(any_gsub("foo(?=bar)", "", c("foobar", "foobaz", "barfoo"), trimws_at_end = TRUE, perl = TRUE), c("bar", "foobaz", "barfoo"))
})

test_that("strsplit_keep_delim works", {

  # Simple case
  expect_equal(strsplit_keep_delim("a-b-c", "-"), c("a", "-", "b", "-", "c"))

  # Where delimiter not found
  expect_equal(strsplit_keep_delim("abc", "-"), c("abc"))

  # Special character delimiter
  expect_equal(strsplit_keep_delim("a|b|c", "|"), c("a", "|", "b", "|", "c"))

  # Where delimiter also at start
  expect_equal(strsplit_keep_delim("-a-b-c", "-"), c("-", "a", "-", "b", "-", "c"))

  # Where delimiter at end
  expect_equal(strsplit_keep_delim("abc-", "-"), c("abc", "-"))

  # Where delimiter at start and end
  expect_equal(strsplit_keep_delim("-a-b-c-", "-"), c("-", "a", "-", "b", "-", "c", "-"))

  # Repeated delimiters
  expect_equal(strsplit_keep_delim("a--b-c", "-"), c("a", "-", "-", "b", "-", "c"))

  # Delimiter of length 0
  expect_equal(strsplit_keep_delim("abc", ""), c("a", "b", "c"))

  # No string given
  expect_error(strsplit_keep_delim("", "-"))
})

test_that("insert_into_str works", {

  # Simple single insert before
  expect_equal(insert_into_str("this is a test", "new ", "test"), "this is a new test")

  # Insert after
  expect_equal(insert_into_str("this is a test", " 2.0", "test", position = "after"), "this is a test 2.0")

  # Multiple insert all before
  expect_equal(insert_into_str("this is a test, a good test, not a bad test", "new ", "test"), "this is a new test, a good new test, not a bad new test")

  # Multiple insert specific instances before
  expect_equal(insert_into_str("this is a test, a good test, not a bad test", "new ", "test", instances_to_insert_at = c(1, 3)), "this is a new test, a good test, not a bad new test")

  # Insert at start
  expect_equal(insert_into_str("testing... yes this is a test", "new ", "test"), "new testing... yes this is a new test")

  # No string_to_find given
  expect_warning(
    result <- insert_into_str("this is a", " test", "elephant"),
    "'elephant' not found in 'this is a'. Inserting at end instead."
    )
  expect_equal(result, "this is a test")

  # more instances_to_insert_at than instances found, strict_mode == FALSE
  expect_equal(insert_into_str("this is a test, a good test, not a bad test", "new ", "test", strict_mode = FALSE, instances_to_insert_at = c(1, 3, 4)), "this is a new test, a good test, not a bad new test")

  # more instances_to_insert_at than instances found, strict_mode == TRUE
  expect_error(insert_into_str("this is a test, a good test, not a bad test", "new ", "test", strict_mode = TRUE, instances_to_insert_at = c(1, 3, 4)))
})

test_that("gsub_by_dict works", {

  # data frame
  expect_equal(gsub_by_dict(data.frame(find = c("an apple", "a carrot"), replace = c("a fruit", "a vegetable")),
                         "I have an apple and a carrot.",
                         "find", "replace"), "I have a fruit and a vegetable.")

  # named list
  expect_equal(gsub_by_dict(list("an apple" = "a fruit", "a carrot" = "a vegetable"),
                            "I have an apple and a carrot."), "I have a fruit and a vegetable.")

  # neither a data frame nor a named list
  expect_error(gsub_by_dict(c("an apple", "a carrot"),
                            "I have an apple and a carrot."))

  # duplicate keys in data frame
  expect_warning(
    result <- gsub_by_dict(data.frame(find = c("an apple", "an apple"), replace = c("a fruit", "a vegetable")),
                            "I have an apple and a carrot.",
                            "find", "replace"),
               "Found duplicate values in reference_dict\\[\\[find_column_name\\]\\]. will only use first match"
    )
  expect_equal(result, "I have a fruit and a carrot.")

  # duplicate keys in data frame
  expect_equal(gsub_by_dict(list("an apple" = "a fruit", "a carrot" = "a vegetable"),
                            "I have an apple and a carrot."), "I have a fruit and a vegetable.")
})

test_that("query_dict works", {

  # Simple single match
  expect_equal(query_dict(data.frame(item = c("apple", "carrot"), price = c("£0.20","£0.50")),
                          key_label = "item",
                          key = "apple",
                          value_label = "price"), "£0.20")

  # Multiple matching key values
  expect_equal(query_dict(data.frame(item = c("apple", "apple", "carrot"), price = c("£0.20", "£0.30", "£0.50")),
             key_label = "item",
             key = "apple",
             value_label = "price"), c("£0.20", "£0.30"))

})

test_that("remove_trailing_characters works", {

  # Simple case
  expect_equal(remove_trailing_characters("SC10ab", c("a", "b")), "SC10")
})

test_that("number_to_word_rank works", {

  expect_equal(number_to_word_rank(1), "first")
})

test_that("word_rank_to_number works", {

  # Number word
  expect_equal(word_rank_to_number("one"), 1)
  expect_equal(word_rank_to_number("fifty"), 50)

  # Ranking word
  expect_equal(word_rank_to_number("first"), 1)
  expect_equal(word_rank_to_number("fiftieth"), 50)
})

test_that("null_to_NA works", {
  expect_equal(null_to_NA(list(NULL, 1, 2, 3, NULL)), list(NA, 1, 2, 3, NA))
})

test_that("check_for_common_alphabets works", {
  expect_equal(check_for_common_alphabets("words, слова, كلمات, 漢語, বাংলা, देवनागरी"), "Latin, Cyrillic, Arabic, Chinese, Bengali, Devanagari")
})

test_that("sub_char_by_index works", {

  # Single character
  expect_equal(sub_char_by_index("testing", 3, "x"), "texting")

  # Removal
  expect_equal(sub_char_by_index("testing", 3, ""), "teting")

  # Range
  expect_equal(sub_char_by_index("testing", 3:4, "em"), "teeming")
})

test_that("closest_matching_string works", {

  # Single match
  expect_equal(closest_matching_string(c("ant", "dog", "cat"), "dug"), "dog")

  # multiple matches
  expect_warning(
    result <- closest_matching_string(c("ant", "dog", "cat"), "ruf"),
                           "Multiple strings with the same distance found. Returning the first instance."
  )
  expect_equal(result, "ant")
})

test_that("bind_cols preserves variable/value labels and other attributes", {

  v1 <- sjlabelled::set_labels(c(1, 2), labels = c("Yes" = 1, "No" = 2))
  attr(v1, "label") <- "Q1 - response"
  attr(v1, "foo") <- "bar"

  v2 <- sjlabelled::set_labels(c(1, 2), labels = c("A" = 1, "B" = 2))
  attr(v2, "label") <- "Q2 - response"

  df1 <- data.frame(a = v1)
  df2 <- data.frame(b = v2)

  res <- surveydatar:::bind_cols(df1, df2)

  expect_equal(sjlabelled::get_label(res$a), "Q1 - response")
  expect_equal(unname(sjlabelled::get_labels(res$a, values = "as.name")["1"]), "Yes")
  expect_equal(unname(sjlabelled::get_labels(res$a, values = "as.name")["2"]), "No")
  expect_equal(attr(res$a, "foo"), "bar")

  expect_equal(sjlabelled::get_label(res$b), "Q2 - response")
  expect_equal(unname(sjlabelled::get_labels(res$b, values = "as.name")["1"]), "A")
  expect_equal(unname(sjlabelled::get_labels(res$b, values = "as.name")["2"]), "B")
})

test_that("bind_rows preserves metadata, merges value labels, and prefers first on conflicts", {

  df1 <- data.frame(
    a = sjlabelled::set_labels(c(1, 1), labels = c("Yes" = 1))
  )
  attr(df1$a, "label") <- "Label A"

  # Conflicting variable label; value labels add a new code and conflict on code 1 text
  df2 <- data.frame(
    a = sjlabelled::set_labels(c(1, 2), labels = c("Yea" = 1, "No" = 2))
  )
  attr(df2$a, "label") <- "Label A (changed)"

  expect_warning(
    res <- surveydatar:::bind_rows(df1, df2, .id = "src"),
    "bind_rows metadata conflicts detected"
  )

  # Variable label prefers first dataset
  expect_equal(sjlabelled::get_label(res$a), "Label A")

  # Value labels merged; code 1 retains first dataset's text, code 2 added from second
  labs <- sjlabelled::get_labels(res$a, values = "as.name")
  expect_equal(unname(labs["1"]), "Yes")
  expect_equal(unname(labs["2"]), "No")
})

test_that("bind_rows with identical metadata emits no warning and preserves labels", {

  df1 <- data.frame(a = sjlabelled::set_labels(c(1, 2), labels = c("Yes" = 1, "No" = 2)))
  attr(df1$a, "label") <- "Label A"

  df2 <- data.frame(a = sjlabelled::set_labels(c(2, 1), labels = c("Yes" = 1, "No" = 2)))
  attr(df2$a, "label") <- "Label A"

  expect_silent({ res <- surveydatar:::bind_rows(df1, df2) })
  expect_equal(sjlabelled::get_label(res$a), "Label A")
  labs <- sjlabelled::get_labels(res$a, values = "as.name")
  expect_equal(unname(labs["1"]), "Yes")
  expect_equal(unname(labs["2"]), "No")
})

test_that("bind_rows works with survey_data and preserves dpdict", {

  s1 <- create_survey_data(get_minimal_labelled_test_dat())
  s2 <- create_survey_data(get_minimal_labelled_test_dat())

  expect_silent({ res <- surveydatar:::bind_rows(s1, s2, .id = "src") })

  expect_true(is.survey_data(res))
  # dpdict should be unchanged (identical to first's dpdict)
  expect_equal(res$dpdict, s1$dpdict)

  # .id should be present on dat and reflect two sources
  expect_true("src" %in% names(res$dat))
  expect_setequal(unique(res$dat$src), c("1", "2"))
})

test_that("bind_cols works with survey_data and updates dpdict", {

  s <- create_survey_data(get_minimal_labelled_test_dat())

  newvec <- rep(c(1, 2), length.out = nrow(s$dat))
  newvec <- sjlabelled::set_labels(newvec, labels = c("X" = 1, "Y" = 2))
  attr(newvec, "label") <- "New Var"

  res <- surveydatar:::bind_cols(s, data.frame(newvar = newvec))

  expect_true(is.survey_data(res))
  expect_true("newvar" %in% names(res$dat))
  expect_equal(sjlabelled::get_label(res$dat$newvar), "New Var")

  # dpdict includes new variable with correct label
  expect_true("newvar" %in% res$dpdict$variable_names)
  idx <- which(res$dpdict$variable_names == "newvar")
  expect_equal(res$dpdict$variable_labels[idx], "New Var")
})

test_that("labelled_map_dfc works", {

  unlabelled_test_dat <- get_minimal_unlabelled_test_dat()
  labelled_test_dat <- get_minimal_labelled_test_dat()

  # Fully labelled
  labelled_result <- labelled_map_dfc(labelled_test_dat, function(x) ifelse(is.na(x), 0, x))

  expect_equal(sjlabelled::get_label(labelled_result$uid), sjlabelled::get_label(labelled_test_dat$uid))
  expect_equal(sjlabelled::get_label(labelled_result$csat), sjlabelled::get_label(labelled_test_dat$csat))
  expect_equal(sjlabelled::get_labels(labelled_result$csat), sjlabelled::get_labels(labelled_test_dat$csat))

  # No labels
  unlabelled_result <- labelled_map_dfc(unlabelled_test_dat, function(x) ifelse(is.na(x), 0, x))

  expect_null(attr(unlabelled_result$uid, "label"))
  expect_null(attr(unlabelled_result$csat, "label"))
  expect_null(attr(unlabelled_result$csat, "labels"))

  # Mix of labelled and unlabelled
  partially_labelled_test_dat <- unlabelled_test_dat
  partially_labelled_test_dat$uid <- sjlabelled::remove_label(partially_labelled_test_dat$uid)

  partially_labelled_result <- labelled_map_dfc(partially_labelled_test_dat, function(x) ifelse(is.na(x), 0, x))

  expect_null(attr(partially_labelled_result$uid, "label"))
  expect_equal(sjlabelled::get_label(partially_labelled_result$csat), sjlabelled::get_label(partially_labelled_test_dat$csat))
  expect_equal(sjlabelled::get_labels(partially_labelled_result$csat), sjlabelled::get_labels(partially_labelled_test_dat$csat))

})

test_that("value_from_labels works", {

  labelled_test_dat <- get_minimal_labelled_test_dat()

  # Simple match
  expect_equal(value_from_labels(labelled_test_dat$csat, "Neutral"), 3)

  # No match
  expect_equal(value_from_labels(labelled_test_dat$csat, "shoe"), NA)

})

test_that("merge_left function works correctly", {

  # Create sample data frames
  df1 <- data.frame(id = c(1, 2, 3), x = c("a", "b", "c"), y = c(10, 20, 30))
  df2 <- data.frame(id = c(1, 2, 4), x = c("A", "B", "D"), z = c(100, 200, 400))

  # Test overwrite = TRUE
  result1 <- merge_left(df1, df2, by = "id", overwrite = TRUE)
  expected1 <- data.frame(id = c(1, 2, 3), x = c("A", "B", "c"), y = c(10, 20, 30), z = c(100, 200, NA))
  expect_equal(result1, expected1)

  # Test overwrite = FALSE
  result2 <- merge_left(df1, df2, by = "id", overwrite = FALSE)
  expected2 <- data.frame(id = c(1, 2, 3), x = c("a", "b", "c"), y = c(10, 20, 30), z = c(100, 200, NA))
  expect_equal(result2, expected2)

})

test_that("check_key_consistency function works correctly", {

  # Create sample data frames
  consistent_df <- data.frame(id = c(1, 1, 2, 2, 3),
                    value1 = c("A", "A", "B", "B", "C"),
                    value2 = c(10, 10, 20, 20, 30))

  consistent_result <- TRUE

  one_inconsistent_df <- data.frame(id = c(1, 1, 2, 2, 3),
                    value1 = c("A", "A", "B", "C", "D"),
                    value2 = c(10, 10, 20, 20, 30))

  one_inconsistent_result <- one_inconsistent_df[c(3, 4), c(1, 2)]

  two_inconsistent_df <- data.frame(id = c(1, 1, 2, 2, 3),
                                    value1 = c("A", "A", "B", "C", "D"),
                                    value2 = c(10, 15, 20, 20, 30))

  two_inconsistent_result <- two_inconsistent_df[c(1, 2, 3, 4), c(1, 2, 3)]

  # consistent
  result1 <- check_key_consistency(consistent_df, "id")
  expect_equal(result1, consistent_result)

  # one inconsistent
  result2 <- check_key_consistency(one_inconsistent_df, "id")
  expect_equal(result2$status, "Key is inconsistent")
  expect_equal(result2$problematic_data, one_inconsistent_result)

  # two inconsistent
  result3 <- check_key_consistency(two_inconsistent_df, "id")
  expect_equal(result3$status, "Key is inconsistent")
  expect_equal(result3$problematic_data, two_inconsistent_result)

})

test_that("concatenate_by_group works correctly", {
  temp_dat <- dplyr::tibble(
    ID = c(1, 1, 2, 2, 3),
    Name = c("John", "John", "Alice", "Alice", "Bob"),
    City = c("New York", "Los Angeles", "Chicago", "Chicago", "Houston"),
    Age = c(25, 25, 30, 30, 35)
  )

  expected_output <- dplyr::tibble(
    ID = c(1, 2, 3),
    concat_Name = c("John", "Alice", "Bob"),
    concat_City = c("Los Angeles, New York", "Chicago", "Houston"),
    concat_Age = c("25", "30", "35")
  )

  result <- concatenate_by_group(temp_dat, "ID")
  expect_equal(result, expected_output)
})
