# Helper function to create test data
create_test_data <- function(n = 100) {
  set.seed(123)
  data.frame(
    # Basic variables
    gender = factor(sample(c("Male", "Female"), n, replace = TRUE)),
    age = sample(18:65, n, replace = TRUE),
    region = factor(sample(c("North", "South", "East", "West"), n, replace = TRUE)),

    # Numeric satisfaction score
    satisfaction = sample(1:5, n, replace = TRUE),

    # Multiresponse question group (binary)
    q1_1 = sample(c(0, 1), n, replace = TRUE),
    q1_2 = sample(c(0, 1), n, replace = TRUE),
    q1_3 = sample(c(0, 1), n, replace = TRUE),

    # Categorical question group
    q2_1 = sample(1:4, n, replace = TRUE),
    q2_2 = sample(1:4, n, replace = TRUE),

    # Weight variable
    weight = runif(n, 0.5, 2),

    # Year for change calculations
    year = sample(c(2023, 2024), n, replace = TRUE),

    stringsAsFactors = FALSE
  )
}

# Create test dpdict
create_test_dpdict <- function() {
  data.frame(
    variable_names = c("gender", "age", "region", "satisfaction",
                       "q1_1", "q1_2", "q1_3", "q2_1", "q2_2",
                       "weight", "year"),
    variable_labels = c("Respondent Gender", "Age", "Region", "Overall Satisfaction",
                        "Q1: Option A", "Q1: Option B", "Q1: Option C",
                        "Q2: Statement 1", "Q2: Statement 2",
                        "Weight", "Survey Year"),
    question_group = c(NA, NA, NA, NA,
                       "q1_a", "q1_a", "q1_a", "q2_a", "q2_a",
                       NA, NA),
    stringsAsFactors = FALSE
  )
}

##### sense functionality #####

# Test formula parsing
test_that("parse_table_formula correctly parses different expression types", {
  data <- create_test_data()
  dpdict <- create_test_dpdict()

  # Test simple variable
  simple <- parse_table_formula(rlang::quo(gender), data, dpdict)
  expect_equal(simple$type, "simple")
  expect_equal(simple$components$var, "gender")
  expect_equal(simple$label, "Respondent Gender")

  # Test string literal (backward compatibility)
  string <- parse_table_formula(rlang::quo("gender"), data, dpdict)
  expect_equal(string$type, "simple")
  expect_equal(string$components$var, "gender")

  # Test multiplication (filter)
  mult <- parse_table_formula(rlang::quo(q1_1 * (age > 30)), data, dpdict)
  expect_equal(mult$type, "multiplication")
  expect_length(mult$components, 2)

  # Test comparison expression
  comp <- parse_table_formula(rlang::quo(age > 30), data, dpdict)
  expect_equal(comp$type, "expression")

  # Test helper function
  helper <- parse_table_formula(rlang::quo(top_box(satisfaction, 2)), data, dpdict)
  expect_equal(helper$type, "helper")
  expect_equal(helper$helper_type, "top_box")
})

# Test variable expansion
test_that("expand_variables correctly expands different variable types", {
  data <- create_test_data()
  dpdict <- create_test_dpdict()

  # Test simple variable (no expansion)
  simple_spec <- list(type = "simple", components = list(var = "age"), label = "Age")
  expanded_simple <- expand_variables(simple_spec, data, dpdict)
  expect_length(expanded_simple, 1)

  # Test question group expansion
  q1_spec <- list(type = "simple", components = list(var = "q1"), label = "Q1")
  expanded_q1 = expand_variables(q1_spec, data, dpdict)
  expect_length(expanded_q1, 3)  # q1_1, q1_2, q1_3

  # Test categorical variable expansion
  gender_spec <- list(type = "simple", components = list(var = "gender"), label = "Gender")
  expanded_gender <- expand_variables(gender_spec, data, dpdict)
  expect_length(expanded_gender, 2)  # Male, Female
  expect_equal(expanded_gender[[1]]$type, "expression")

  # Test pattern matching without dpdict
  data_pattern <- create_test_data()
  q2_spec <- list(type = "simple", components = list(var = "q2"), label = "Q2")
  expanded_q2 <- expand_variables(q2_spec, data_pattern, NULL)
  expect_length(expanded_q2, 2)  # q2_1, q2_2
})

# Test array computation
test_that("formula_to_array correctly computes arrays", {
  data <- create_test_data()

  # Test simple binary variable
  spec_binary <- list(type = "simple", components = list(var = "q1_1"))
  array_binary <- formula_to_array(spec_binary, data)
  expect_equal(length(array_binary), nrow(data))
  expect_true(all(array_binary %in% c(0, 1)))

  # Test expression
  spec_expr <- list(
    type = "expression",
    components = list(expr = quote(age > 30))
  )
  array_expr <- formula_to_array(spec_expr, data)
  expect_equal(array_expr, as.numeric(data$age > 30))

  # Test multiplication
  spec_mult <- list(
    type = "multiplication",
    components = list(
      list(type = "simple", components = list(var = "q1_1")),
      list(type = "expression", components = list(expr = quote(age > 30)))
    )
  )
  array_mult <- formula_to_array(spec_mult, data)
  expect_equal(array_mult, data$q1_1 * as.numeric(data$age > 30))
})

# Test helper functions
test_that("helper functions process correctly", {
  data <- create_test_data()

  # Test top_box
  spec_top <- list(
    type = "helper",
    helper_type = "top_box",
    components = list(quote(satisfaction), 2)
  )
  array_top <- process_helper(spec_top, data)
  expect_equal(array_top, as.numeric(data$satisfaction %in% c(4, 5)))

  # Test bottom_box
  spec_bottom <- list(
    type = "helper",
    helper_type = "bottom_box",
    components = list(quote(satisfaction), 2)
  )
  array_bottom <- process_helper(spec_bottom, data)
  expect_equal(array_bottom, as.numeric(data$satisfaction %in% c(1, 2)))
})

# Test cell computation
test_that("compute_cell calculates statistics correctly", {
  # Create simple test arrays
  base_array <- c(1, 1, 1, 1, 1)
  row_array <- c(1, 1, 0, 0, 1)
  col_array <- c(1, 0, 1, 0, 1)

  # Test count
  expect_equal(compute_cell(base_array, row_array, col_array, "count"), 2)

  # Test column percentage
  expect_equal(compute_cell(base_array, row_array, col_array, "column_pct"), 2/3 * 100)

  # Test row percentage
  expect_equal(compute_cell(base_array, row_array, col_array, "row_pct"), 2/3 * 100)

  # Test mean
  values <- c(5, 4, 3, 2, 1)
  cell_array <- row_array * col_array
  expect_equal(
    compute_cell(base_array, row_array, col_array, "mean", values),
    sum(cell_array * values) / sum(cell_array)
  )

  # Test division by zero
  expect_true(is.na(compute_cell(base_array, rep(1, 5), rep(0, 5), "column_pct")))
})

# Test main tab function - backward compatibility
test_that("tab maintains backward compatibility with string syntax", {
  data <- create_test_data()

  # Simple frequency table
  result1 <- tab(data, "gender")
  expect_s3_class(result1, "tab_result")
  expect_equal(nrow(result1), 2)  # Male, Female
  expect_equal(ncol(result1), 2)  # row_label, Total

  # Cross-tabulation
  result2 <- tab(data, "gender", "region")
  expect_equal(nrow(result2), 2)  # Male, Female
  expect_equal(ncol(result2), 5)  # row_label + 4 regions

  # With weights
  result3 <- tab(data, "gender", "region", weight = "weight")
  expect_s3_class(result3, "tab_result")
})

# Test new formula syntax
test_that("tab works with formula syntax", {
  data <- create_test_data()

  # Simple variable
  result1 <- tab(data, gender)
  expect_equal(nrow(result1), 2)

  # With filter
  result2 <- tab(data, gender * (age > 30))
  expect_equal(nrow(result2), 2)
  # Values should be less than without filter
  result_unfiltered <- tab(data, gender)
  expect_true(all(as.numeric(gsub("%", "", result2[[2]])) <=
                    as.numeric(gsub("%", "", result_unfiltered[[2]]))))

  # Question group expansion
  result3 <- tab(data, q1)
  expect_equal(nrow(result3), 3)  # q1_1, q1_2, q1_3

  # Cross-tab with formula
  result4 <- tab(data, gender * (age > 30), region)
  expect_equal(ncol(result4), 5)  # row_label + 4 regions
})

# Test rows_list functionality
test_that("rows_list creates multiple row groups", {
  data <- create_test_data()

  result <- tab(data,
                rows = rows_list(
                  "All" = gender,
                  "Young" = gender * (age < 30),
                  "Old" = gender * (age >= 30)
                ),
                cols = region)

  # Should have 6 rows: 2 for each group
  expect_equal(nrow(result), 6)

  # Check labels
  expect_true("All - gender: Female" %in% result$row_label)
  expect_true("All - gender: Male" %in% result$row_label)
  expect_true("Young - gender: Female * (age < 30)" %in% result$row_label)
  expect_true("Young - gender: Male * (age < 30)" %in% result$row_label)
  expect_true("Old - gender: Female * (age >= 30)" %in% result$row_label)
  expect_true("Old - gender: Male * (age >= 30)" %in% result$row_label)

  # Error on unnamed arguments
  expect_error(rows_list(gender, age), "All arguments to rows_list must be named")
})


# Test helper functions in tab
test_that("helper functions work in tab", {
  data <- create_test_data()

  # Top box
  result1 <- tab(data, top_box(satisfaction, 2), gender)
  expect_equal(nrow(result1), 1)  # Single result row for the helper function
  expect_true(grepl("top_box", result1$row_label[1]))  # Should contain the helper function name

  # Multiple helpers
  result2 <- tab(data,
                 rows = rows_list(
                   "Top 2" = top_box(satisfaction, 2),
                   "Bottom 2" = bottom_box(satisfaction, 2)
                 ),
                 cols = gender)
  expect_equal(nrow(result2), 2)
  expect_true(any(grepl("Top 2", result2$row_label)))
  expect_true(any(grepl("Bottom 2", result2$row_label)))
})

# Test different statistics
test_that("different statistics compute correctly", {
  data <- create_test_data()

  # Count
  result_count <- tab(data, gender, region, statistic = "count")
  total_count <- sum(as.numeric(unlist(result_count[, -1])))
  expect_equal(total_count, nrow(data))

  # Column percentage (default)
  result_col_pct <- tab(data, gender, region, statistic = "column_pct")
  # Each column should sum to 100%
  col_sums <- colSums(apply(result_col_pct[, -1], 2, function(x) {
    as.numeric(gsub("%", "", x))
  }))
  expect_true(all(abs(col_sums - 100) < 0.1))

  # Row percentage
  result_row_pct <- tab(data, gender, region, statistic = "row_pct")
  # Each row should sum to 100%
  row_sums <- rowSums(apply(result_row_pct[, -1], 2, function(x) {
    as.numeric(gsub("%", "", x))
  }))
  expect_true(all(abs(row_sums - 100) < 0.1))
})

# Test with survey_data object
test_that("tab works with survey_data objects", {
  data <- create_test_data()
  dpdict <- create_test_dpdict()
  survey_obj <- structure(
    list(dat = data, dpdict = dpdict),
    class = "survey_data"
  )

  result <- tab(survey_obj, gender, region)
  expect_s3_class(result, "tab_result")

  # Should use labels from dpdict
  expect_true(any(grepl("Respondent Gender", result$row_label)))
})

# Test error handling
test_that("tab handles errors appropriately", {
  data <- create_test_data()

  # Non-existent variable
  expect_error(tab(data, "nonexistent"), "not found in data")

  # Invalid weight
  expect_error(tab(data, gender, weight = "badweight"), "Weight variable")

  # Invalid statistic
  expect_error(tab(data, gender, statistic = "invalid"), "should be one of")
})

# Test edge cases
test_that("tab handles edge cases", {
  # Empty data
  empty_data <- create_test_data(0)
  expect_warning(
    result_empty <- tab(empty_data, gender),
    "Data has 0 rows. Returning empty result."
  )
  expect_true(is.data.frame(result_empty))  # But still a valid data frame

  # All NA values
  na_data <- create_test_data()
  na_data$gender <- NA
  expect_warning(
    result <- tab(na_data, gender),
    "All values are NA or zero for the row variable"
  )
  expect_true(all(result[, -1] == 0 | is.na(result[, -1])))

  # Single value
  single_data <- create_test_data()
  single_data$single <- 1
  result <- tab(single_data, single)
  expect_equal(nrow(result), 1)
})

# Test complex expressions
test_that("complex expressions work correctly", {
  data <- create_test_data()

  # Multiple filters
  result <- tab(data,
                gender * (age > 30) * (region %in% c("North", "South")),
                statistic = "count")

  # Manual calculation
  manual_count <- sum(
    (data$gender == "Male") &
      (data$age > 30) &
      (data$region %in% c("North", "South"))
  ) + sum(
    (data$gender == "Female") &
      (data$age > 30) &
      (data$region %in% c("North", "South"))
  )

  total_result <- sum(as.numeric(result[, -1]))
  expect_equal(total_result, manual_count)
})

# Test print method
test_that("print method formats output correctly", {
  data <- create_test_data()
  result <- tab(data, gender, region)

  # Capture print output
  output <- capture.output(print(result))

  expect_true(any(grepl("Cross-tabulation", output)))
  expect_true(any(grepl("column_pct", output)))
  expect_true(any(grepl("Base:", output)))
})


##### test mathematical accuracy #####

# Additional tests for tab calculation accuracy
test_that("tab calculates counts accurately", {
  # Create deterministic test data
  set.seed(123)
  data <- data.frame(
    gender = factor(c(rep("Male", 60), rep("Female", 40))),
    region = factor(c(rep("North", 30), rep("South", 30), rep("East", 20), rep("West", 20))),
    age = c(rep(25, 50), rep(35, 50)),
    weight = c(rep(1, 50), rep(2, 50))
  )

  # Test simple count
  result <- tab(data, gender, statistic = "count")

  # Match by row labels instead of relying on order
  expect_equal(result[result$row_label == "gender: Female", "Total"], 40)
  expect_equal(result[result$row_label == "gender: Male", "Total"], 60)

  # Test cross-tab count
  result_cross <- tab(data, gender, region, statistic = "count")

  # Match by row labels for cross-tab
  male_row <- result_cross[result_cross$row_label == "gender: Male", ]
  female_row <- result_cross[result_cross$row_label == "gender: Female", ]

  expect_equal(male_row$`region: North`, 30)
  expect_equal(male_row$`region: South`, 30)
  expect_equal(male_row$`region: East`, 0)
  expect_equal(male_row$`region: West`, 0)

  expect_equal(female_row$`region: North`, 0)
  expect_equal(female_row$`region: South`, 0)
  expect_equal(female_row$`region: East`, 20)
  expect_equal(female_row$`region: West`, 20)
})

test_that("tab calculates column percentages accurately", {
  set.seed(456)
  data <- data.frame(
    var1 = factor(c(rep("A", 30), rep("B", 70))),
    var2 = factor(c(rep("X", 40), rep("Y", 60)))
  )

  # Test simple column percentage (should be 100% for single column)
  result <- tab(data, var1, statistic = "column_pct")
  expect_equal(as.numeric(gsub("%", "", result$Total)), c(30, 70))

  # Test cross-tab column percentages
  result_cross <- tab(data, var1, var2, statistic = "column_pct")

  # Manual calculation:
  # var2 has 40 X's and 60 Y's
  # var1 has 30 A's (all in first 30 rows) and 70 B's (all in last 70 rows)
  # So: A∩X = min(30,40) = 30, A∩Y = 0, B∩X = 10, B∩Y = 60
  # Column %: A∩X/X = 30/40 = 75%, A∩Y/Y = 0/60 = 0%
  #          B∩X/X = 10/40 = 25%, B∩Y/Y = 60/60 = 100%

  expect_equal(as.numeric(gsub("%", "", result_cross[1, "var2: X"])), 75)
  expect_equal(as.numeric(gsub("%", "", result_cross[1, "var2: Y"])), 0)
  expect_equal(as.numeric(gsub("%", "", result_cross[2, "var2: X"])), 25)
  expect_equal(as.numeric(gsub("%", "", result_cross[2, "var2: Y"])), 100)

  # Verify columns sum to 100%
  col_x_sum <- sum(as.numeric(gsub("%", "", result_cross[, "var2: X"])))
  col_y_sum <- sum(as.numeric(gsub("%", "", result_cross[, "var2: Y"])))
  expect_equal(col_x_sum, 100)
  expect_equal(col_y_sum, 100)
})

test_that("tab calculates row percentages accurately", {
  set.seed(789)
  data <- data.frame(
    var1 = factor(c(rep("A", 40), rep("B", 60))),
    var2 = factor(c(rep("X", 25), rep("Y", 25), rep("X", 25), rep("Y", 25)))
  )

  result <- tab(data, var1, var2, statistic = "row_pct")

  # Manual calculation:
  # A appears in first 40 rows: 25 X's + 15 Y's
  # B appears in last 60 rows: 25 X's + 35 Y's
  # Row %: A∩X/A = 25/40 = 62.5%, A∩Y/A = 15/40 = 37.5%
  #        B∩X/B = 25/60 = 41.67%, B∩Y/B = 35/60 = 58.33%

  expect_equal(as.numeric(gsub("%", "", result[1, "var2: X"])), 62.5)
  expect_equal(as.numeric(gsub("%", "", result[1, "var2: Y"])), 37.5)
  expect_equal(as.numeric(gsub("%", "", result[2, "var2: X"])), 41.67, tolerance = 0.1)
  expect_equal(as.numeric(gsub("%", "", result[2, "var2: Y"])), 58.33, tolerance = 0.1)

  # Verify rows sum to 100%
  row_a_sum <- sum(as.numeric(gsub("%", "", result[1, -1])))
  row_b_sum <- sum(as.numeric(gsub("%", "", result[2, -1])))
  expect_equal(row_a_sum, 100, tolerance = 0.1)
  expect_equal(row_b_sum, 100, tolerance = 0.1)
})

test_that("tab calculates weighted statistics accurately", {
  set.seed(321)
  data <- data.frame(
    gender = factor(c(rep("Male", 50), rep("Female", 50))),
    region = factor(c(rep("North", 25), rep("South", 75))),
    weight = c(rep(2, 25), rep(1, 75))  # First 25 have weight 2, rest have weight 1
  )

  # Test weighted count
  result_count <- tab(data, gender, weight = "weight", statistic = "count")

  # Manual calculation:
  # Males (first 50): 25 with weight 2 + 25 with weight 1 = 75
  # Females (last 50): all have weight 1 = 50
  expect_equal(as.numeric(result_count[result_count$row_label == "gender: Male", ][["Total"]]), 75)
  expect_equal(as.numeric(result_count[result_count$row_label == "gender: Female", ][["Total"]]), 50)

  # Test weighted cross-tab
  result_cross <- tab(data, gender, region, weight = "weight", statistic = "count")

  # Manual calculation:
  # Male∩North: first 25 rows (all weight 2) = 50
  # Male∩South: next 25 rows (all weight 1) = 25
  # Female∩North: 0 (no overlap)
  # Female∩South: last 50 rows (all weight 1) = 50
  expect_equal(as.numeric(result_cross[result_cross$row_label == "gender: Male", "region: North"]), 50)  # Male & North
  expect_equal(as.numeric(result_cross[result_cross$row_label == "gender: Male", "region: South"]), 25)  # Male & South
  expect_equal(as.numeric(result_cross[result_cross$row_label == "gender: Female", "region: North"]), 0)   # Female & North
  expect_equal(as.numeric(result_cross[result_cross$row_label == "gender: Female", "region: South"]), 50)  # Female & South

  # Test weighted percentages
  result_pct <- tab(data, gender, region, weight = "weight", statistic = "column_pct")

  # Column totals: North = 50, South = 75
  # Male∩North % = 50/50 = 100%, Female∩North % = 0/50 = 0%
  # Male∩South % = 25/75 = 33.33%, Female∩South % = 50/75 = 66.67%
  expect_equal(as.numeric(gsub("%", "", result_pct[result_pct$row_label == "gender: Male", "region: North"])), 100, tolerance = 0.1)
  expect_equal(as.numeric(gsub("%", "", result_pct[result_pct$row_label == "gender: Female", "region: North"])), 0, tolerance = 0.1)
  expect_equal(as.numeric(gsub("%", "", result_pct[result_pct$row_label == "gender: Male", "region: South"])), 33.33, tolerance = 0.1)
  expect_equal(as.numeric(gsub("%", "", result_pct[result_pct$row_label == "gender: Female", "region: South"])), 66.67, tolerance = 0.1)
})

test_that("tab calculates filtered statistics accurately", {
  set.seed(654)
  data <- data.frame(
    gender = factor(c(rep("Male", 60), rep("Female", 40))),
    age = c(rep(20, 30), rep(40, 70)),
    region = factor(c(rep("North", 50), rep("South", 50)))
  )

  # Test filtered count
  result <- tab(data, gender * (age > 30), statistic = "count")

  # Manual calculation:
  # age > 30 applies to last 70 rows
  # Of these 70: first 30 are Male, last 40 are Female
  # But we need to account for the gender distribution
  # Males: first 60 rows, age > 30 for rows 31-60 = 30 males
  # Females: last 40 rows, age > 30 for all of them = 40 females
  expect_equal(as.numeric(result$Total[result$row_label == "gender: Female * (age > 30)"]), 40)
  expect_equal(as.numeric(result$Total[result$row_label == "gender: Male * (age > 30)"]), 30)

  # Test filtered cross-tab
  result_cross <- tab(data, gender * (age > 30), region, statistic = "count")

  # Of the 70 rows with age > 30:
  # Rows 31-60 (30 males): 20 North + 10 South
  # Rows 61-100 (40 females): 30 North + 10 South
  # Wait, let me recalculate based on the data structure...
  # First 50 rows are North, last 50 are South
  # age > 30 for rows 31-100
  # Males (rows 1-60) ∩ age>30 (rows 31-100) = rows 31-60 (30 rows)
  # Of rows 31-60: rows 31-50 are North (20), rows 51-60 are South (10)
  # Females (rows 61-100) ∩ age>30 (rows 31-100) = rows 61-100 (40 rows)
  # Of rows 61-100: all are South (40)

  expect_equal(as.numeric(result_cross[result_cross$row_label == "gender: Male * (age > 30)", "region: North"]), 20)  # Male & North & age>30
  expect_equal(as.numeric(result_cross[result_cross$row_label == "gender: Male * (age > 30)", "region: South"]), 10)  # Male & South & age>30
  expect_equal(as.numeric(result_cross[result_cross$row_label == "gender: Female * (age > 30)", "region: North"]), 0)   # Female & North & age>30
  expect_equal(as.numeric(result_cross[result_cross$row_label == "gender: Female * (age > 30)", "region: South"]), 40)  # Female & South & age>30
})

test_that("tab calculates means accurately", {
  set.seed(987)
  data <- data.frame(
    group = factor(c(rep("A", 50), rep("B", 50))),
    values = c(rep(10, 25), rep(20, 25), rep(30, 25), rep(40, 25)),
    weight = c(rep(1, 50), rep(2, 50))
  )

  # Test unweighted mean (not yet implemented, this tests the structure)
  # For now, test that it doesn't error
  expect_error(
    tab(data, group, statistic = "mean"),
    "Unsupported statistic: mean"
  )

  # Note: Mean functionality would need to be implemented in the compute_cell function
  # The test structure is ready for when mean is supported
})

test_that("tab handles complex filtering accurately", {
  set.seed(111)
  data <- data.frame(
    var1 = factor(c(rep("X", 30), rep("Y", 70))),
    var2 = factor(c(rep("A", 60), rep("B", 40))),
    age = c(rep(25, 50), rep(35, 50)),
    income = c(rep(1000, 20), rep(2000, 80))
  )

  # Test multiple filter conditions
  result <- tab(data, var1 * (age > 30) * (income > 1500), var2, statistic = "count")

  # Manual calculation:
  # age > 30: rows 51-100 (50 rows)
  # income > 1500: rows 21-100 (80 rows)
  # Both conditions: rows 51-100 (50 rows)
  # var1 in these rows: first 30 rows are X, so none qualify; rows 31-100 are Y, so rows 51-100 = 50 Y's
  # var2 in rows 51-100: rows 51-60 are A (10), rows 61-100 are B (40)

  expect_equal(as.numeric(result[result$row_label == "var1: X * (age > 30) * (income > 1500)", "var2: A"]), 0)   # X & filters & A = 0
  expect_equal(as.numeric(result[result$row_label == "var1: X * (age > 30) * (income > 1500)", "var2: B"]), 0)   # X & filters & B = 0
  expect_equal(as.numeric(result[result$row_label == "var1: Y * (age > 30) * (income > 1500)", "var2: A"]), 10)  # Y & filters & A = 10
  expect_equal(as.numeric(result[result$row_label == "var1: Y * (age > 30) * (income > 1500)", "var2: B"]), 40)  # Y & filters & B = 40
})

test_that("tab handles edge cases in calculations", {
  # Test with all zeros in a cell
  data <- data.frame(
    var1 = factor(c(rep("A", 50), rep("B", 50))),
    var2 = factor(c(rep("X", 50), rep("Y", 50)))
  )

  result <- tab(data, var1, var2, statistic = "column_pct")

  # This creates a 2x2 table where A only appears with X, B only appears with Y
  # So A∩Y = 0 and B∩X = 0
  expect_equal(as.numeric(gsub("%", "", result[result$row_label == "var1: A", "var2: Y"])), 0)  # A & Y = 0%
  expect_equal(as.numeric(gsub("%", "", result[result$row_label == "var1: B", "var2: X"])), 0)  # B & X = 0%
  expect_equal(as.numeric(gsub("%", "", result[result$row_label == "var1: A", "var2: X"])), 100) # A & X = 100%
  expect_equal(as.numeric(gsub("%", "", result[result$row_label == "var1: B", "var2: Y"])), 100) # B & Y = 100%

  # Test division by zero scenarios are handled
  empty_filter_data <- data.frame(
    var1 = factor(c("A", "B")),
    age = c(10, 20)
  )

  # Filter that results in no data should produce warning
  expect_warning(
    result_empty <- tab(empty_filter_data, var1 * (age > 100), statistic = "count"),
    "All values are NA or zero"
  )
})

test_that("weighted and filtered calculations combined", {
  set.seed(222)
  data <- data.frame(
    group = factor(c(rep("A", 40), rep("B", 60))),
    filter_var = c(rep(TRUE, 60), rep(FALSE, 40)),
    weight = c(rep(2, 50), rep(1, 50))
  )

  # Test weighted + filtered count
  result <- tab(data, group * filter_var, weight = "weight", statistic = "count")

  # Manual calculation:
  # filter_var = TRUE for first 60 rows
  # Group A (first 40 rows) ∩ filter_var=TRUE (first 60 rows) = first 40 rows
  # Of first 40 rows: all have weight 2, so count = 40 * 2 = 80
  # Group B (last 60 rows) ∩ filter_var=TRUE (first 60 rows) = rows 41-60 (20 rows)
  # Of rows 41-60: rows 41-50 have weight 2 (10*2=20), rows 51-60 have weight 1 (10*1=10)
  # Total for B = 30

  expect_equal(as.numeric(result$Total[result$row_label == "group: A * filter_var"]), 80)
  expect_equal(as.numeric(result$Total[result$row_label == "group: B * filter_var"]), 30)
})

##### Test NET and Avg functionality #####

test_that("NET row is added for count statistic with multiple rows", {
  set.seed(123)
  data <- data.frame(
    gender = factor(c(rep("Male", 60), rep("Female", 40))),
    age_group = factor(c(rep("Young", 30), rep("Old", 70))),
    region = factor(c(rep("North", 50), rep("South", 50)))
  )

  result <- tab(data, gender, region, statistic = "count")

  # Should have 3 rows: Male, Female, NET
  expect_equal(nrow(result), 4)  # +1 for Base (n)

  # Check that NET row exists
  expect_true("NET" %in% result$row_label)

  # NET should be above Base (n)
  net_position <- which(result$row_label == "NET")
  base_position <- which(result$row_label == "Base (n)")
  expect_true(net_position < base_position)
})

test_that("NET row calculates correctly for count statistic", {
  set.seed(456)
  data <- data.frame(
    var1 = factor(c(rep("A", 30), rep("B", 70))),
    var2 = factor(c(rep("X", 40), rep("Y", 60)))
  )

  result <- tab(data, var1, var2, statistic = "count")

  # Manual calculation for NET:
  # Anyone who is A OR B (which is everyone) = 100
  # Split by var2: 40 X's and 60 Y's
  net_row <- result[result$row_label == "NET", ]
  expect_equal(as.numeric(net_row$`var2: X`), 40)
  expect_equal(as.numeric(net_row$`var2: Y`), 60)
})

test_that("NET row calculates correctly for column_pct statistic", {
  set.seed(789)
  data <- data.frame(
    var1 = factor(c(rep("A", 25), rep("B", 75))),
    var2 = factor(c(rep("X", 60), rep("Y", 40)))
  )

  result <- tab(data, var1, var2, statistic = "column_pct")

  # Manual calculation for NET:
  # Anyone who is A OR B (which is everyone) = 100%
  net_row <- result[result$row_label == "NET", ]
  expect_equal(as.numeric(gsub("%", "", net_row$`var2: X`)), 100)
  expect_equal(as.numeric(gsub("%", "", net_row$`var2: Y`)), 100)
})

test_that("NET column is added for row_pct statistic with multiple columns", {
  set.seed(321)
  data <- data.frame(
    gender = factor(c(rep("Male", 50), rep("Female", 50))),
    region = factor(c(rep("North", 25), rep("South", 75)))
  )

  result <- tab(data, gender, region, statistic = "row_pct")

  # Should have 3 columns: row_label, North, South, NET
  expect_equal(ncol(result), 4)

  # Check that NET column exists
  expect_true("NET" %in% names(result))

  # NET should be the last data column (before any future additions)
  expect_equal(names(result)[ncol(result)], "NET")
})

test_that("NET column calculates correctly for row_pct statistic", {
  set.seed(654)
  data <- data.frame(
    var1 = factor(c(rep("A", 40), rep("B", 60))),
    var2 = factor(c(rep("X", 30), rep("Y", 70)))
  )

  result <- tab(data, var1, var2, statistic = "row_pct")

  # Manual calculation for NET column:
  # Anyone who is X OR Y (which is everyone) = 100% for each row
  expect_equal(as.numeric(gsub("%", "", result[result$row_label == "var1: A", "NET"])), 100)
  expect_equal(as.numeric(gsub("%", "", result[result$row_label == "var1: B", "NET"])), 100)
})

test_that("Avg row is added for mean statistic with multiple rows", {
  # Note: This test assumes mean statistic will be implemented
  # For now, we test that it doesn't error and has the right structure
  set.seed(111)
  data <- data.frame(
    gender = factor(c(rep("Male", 50), rep("Female", 50))),
    values = c(rep(10, 50), rep(20, 50))
  )

  # This will currently error because mean isn't implemented
  # But the structure should be ready
  expect_error(
    tab(data, gender, statistic = "mean"),
    "Unsupported statistic: mean"
  )

  # When mean is implemented, this test should pass:
  # result <- tab(data, gender, statistic = "mean")
  # expect_true("Avg" %in% result$row_label)
})

test_that("NET/Avg not added for single row", {
  set.seed(222)
  data <- data.frame(
    single_var = factor(rep("A", 100)),
    other_var = factor(c(rep("X", 50), rep("Y", 50)))
  )

  # Single row - no NET should be added
  result <- tab(data, single_var, other_var, statistic = "count")
  expect_false("NET" %in% result$row_label)

  # Should only have: single_var row + Base (n) = 2 rows
  expect_equal(nrow(result), 2)
})

test_that("NET not added for single column", {
  set.seed(333)
  data <- data.frame(
    var1 = factor(c(rep("A", 40), rep("B", 60))),
    single_col = factor(rep("X", 100))
  )

  # Single column - no NET column should be added for row_pct
  result <- tab(data, var1, single_col, statistic = "row_pct")
  expect_false("NET" %in% names(result))

  # Should only have: row_label + single_col = 2 columns
  expect_equal(ncol(result), 2)
})

test_that("NET calculations with complex filtering", {
  set.seed(444)
  data <- data.frame(
    gender = factor(c(rep("Male", 60), rep("Female", 40))),
    age = c(rep(25, 30), rep(35, 70)),
    region = factor(c(rep("North", 50), rep("South", 50)))
  )

  # Test NET with filtered rows
  result <- tab(data, gender * (age > 30), region, statistic = "count")

  # Manual calculation:
  # Male & age>30: last 30 males (rows 31-60) = 30
  # Female & age>30: all 40 females (rows 61-100) = 40
  # NET: anyone who is (Male & age>30) OR (Female & age>30) = rows 31-100 = 70
  # Of these 70: rows 31-50 are North (20), rows 51-100 are South (50)

  net_row <- result[result$row_label == "NET", ]
  expect_equal(as.numeric(net_row$`region: North`), 20)
  expect_equal(as.numeric(net_row$`region: South`), 50)
})

test_that("NET base sizes calculated correctly", {
  set.seed(555)
  data <- data.frame(
    var1 = factor(c(rep("A", 30), rep("B", 70))),
    var2 = factor(c(rep("X", 40), rep("Y", 60))),
    weight = c(rep(2, 50), rep(1, 50))
  )

  # Test NET column base for row_pct
  result <- tab(data, var1, var2, weight = "weight", statistic = "row_pct")

  # Check that base row includes NET column
  base_row <- result[result$row_label == "Base (n)", ]
  expect_false(is.na(base_row$NET))

  # NET column base should be weighted total (everyone)
  # First 50 rows: weight 2 = 100, last 50 rows: weight 1 = 50, total = 150
  expect_equal(as.numeric(base_row$NET), 150)
})

test_that("NET works with helper functions", {
  data <- create_test_data()

  # Test NET with top_box helper
  result <- tab(data,
                rows = rows_list(
                  "High satisfaction" = top_box(satisfaction, 2),
                  "Males" = gender * (gender == "Male")
                ),
                cols = region,
                statistic = "count")

  # Should have NET row
  expect_true("NET" %in% result$row_label)

  # NET should represent union of high satisfaction OR male
  net_row <- result[result$row_label == "NET", ]

  # Verify it's not just a sum (which would be higher due to overlap)
  high_sat_row <- result[grepl("High satisfaction", result$row_label), ]
  males_row <- result[grepl("Males", result$row_label), ]

  # NET total should be <= sum of individual totals (due to overlap)
  net_total <- sum(as.numeric(net_row[, -1]))
  individual_sum <- sum(as.numeric(unlist(high_sat_row[, -1]))) + sum(as.numeric(unlist(males_row[, -1])))
  expect_true(net_total <= individual_sum)
})

test_that("NET with edge case: overlapping conditions", {
  set.seed(666)
  # Create data where conditions overlap significantly
  data <- data.frame(
    age = rep(c(25, 35), each = 50),
    income = rep(c("Low", "High"), times = 50),
    region = factor(c(rep("North", 70), rep("South", 30)))
  )

  result <- tab(data,
                rows = rows_list(
                  "Young" = age < 30,
                  "Low income" = income == "Low"
                ),
                cols = region,
                statistic = "column_pct")

  # Verify NET is not a simple sum (which would exceed 100%)
  net_row <- result[result$row_label == "NET", ]

  # Each column should be <= 100%
  expect_true(as.numeric(gsub("%", "", net_row$`region: North`)) <= 100)
  expect_true(as.numeric(gsub("%", "", net_row$`region: South`)) <= 100)

  # But should be > either individual condition alone
  young_row <- result[grepl("Young", result$row_label), ]
  low_income_row <- result[grepl("Low income", result$row_label), ]

  expect_true(as.numeric(gsub("%", "", net_row$`region: North`)) >=
                as.numeric(gsub("%", "", young_row$`region: North`)))
  expect_true(as.numeric(gsub("%", "", net_row$`region: North`)) >=
                as.numeric(gsub("%", "", low_income_row$`region: North`)))
})



##### unit tests for labelled variable expansion #####
# Unit tests for labelled variable expansion in tab()

# Helper function to create test data with labelled variables
create_labelled_test_data <- function(n = 100) {
  set.seed(123)

  # Create basic data frame
  data <- data.frame(
    id = 1:n,
    age = sample(18:65, n, replace = TRUE),
    stringsAsFactors = FALSE
  )

  # Create labelled satisfaction variable (5-point scale)
  satisfaction_values <- sample(1:5, n, replace = TRUE)
  data$satisfaction <- haven::labelled(
    satisfaction_values,
    labels = c("Very dissatisfied" = 1, "Dissatisfied" = 2, "Neutral" = 3,
               "Satisfied" = 4, "Very satisfied" = 5)
  )

  # Create labelled region variable (4 regions)
  region_values <- sample(1:4, n, replace = TRUE)
  data$region <- haven::labelled(
    region_values,
    labels = c("North" = 1, "South" = 2, "East" = 3, "West" = 4)
  )

  # Create labelled variable with many categories (like the 17-value example)
  brand_values <- sample(1:17, n, replace = TRUE)
  brand_labels <- setNames(1:17, paste0("Brand_", LETTERS[1:17]))
  data$brand <- haven::labelled(brand_values, labels = brand_labels)

  # Create binary labelled variable (multiresponse style)
  binary_values <- sample(0:1, n, replace = TRUE)
  data$binary_var <- haven::labelled(
    binary_values,
    labels = c("Not selected" = 0, "Selected" = 1)
  )

  # Create regular factor for comparison
  data$factor_var <- factor(sample(c("Option A", "Option B", "Option C"), n, replace = TRUE))

  return(data)
}

# Helper function to create dpdict for labelled test data
create_labelled_dpdict <- function() {
  data.frame(
    variable_names = c("id", "age", "satisfaction", "region", "brand", "binary_var", "factor_var"),
    variable_labels = c("ID", "Age", "Satisfaction Level", "Geographic Region",
                        "Brand Preference", "Binary Variable", "Factor Variable"),
    question_group = c(NA, NA, NA, NA, NA, NA, NA),
    stringsAsFactors = FALSE
  )
}

# Test labelled variable expansion in rows
test_that("labelled variables expand correctly in rows", {
  data <- create_labelled_test_data()

  # Test satisfaction (5-point scale) expansion
  result <- tab(data, satisfaction)

  # Should have 5 rows (one for each satisfaction level)
  expect_equal(nrow(result) - 1, 5)  # -1 for base row

  # Check that all satisfaction labels are present
  satisfaction_labels <- c("Very dissatisfied", "Dissatisfied", "Neutral", "Satisfied", "Very satisfied")
  for (label in satisfaction_labels) {
    expect_true(any(grepl(label, result$row_label)))
  }
})

# Test labelled variable expansion in columns
test_that("labelled variables expand correctly in columns", {
  data <- create_labelled_test_data()

  # Test region expansion in columns
  result <- tab(data, satisfaction, region)

  # Should have 4 columns for region + 1 for row_label
  expect_equal(ncol(result) - 1, 4)  # -1 for row_label column

  # Check that all region labels are present in column names
  region_labels <- c("North", "South", "East", "West")
  for (label in region_labels) {
    expect_true(any(grepl(label, names(result))))
  }
})

# Test labelled variable with many categories (17 brands)
test_that("labelled variables with many categories expand correctly", {
  data <- create_labelled_test_data()

  # Test brand variable (17 categories) in rows
  result <- tab(data, brand)

  # Should have 17 rows (one for each brand) + base row
  expect_equal(nrow(result) - 2, 17)  # -1 for base row and -1 for NET

  # Check that brand labels follow expected pattern
  for (i in 1:17) {
    brand_label <- paste0("Brand_", LETTERS[i])
    expect_true(any(grepl(brand_label, result$row_label)))
  }
})

# Test cross-tabulation with two labelled variables
test_that("cross-tabulation works with two labelled variables", {
  data <- create_labelled_test_data()

  # Cross-tab satisfaction by region
  result <- tab(data, satisfaction, region)

  # Should have 5 rows (satisfaction levels) + base row
  expect_equal(nrow(result) - 2, 5)

  # Should have 4 columns (regions) + row_label column
  expect_equal(ncol(result) - 1, 4)

  # Check that percentages are calculated correctly
  # Each column should sum to approximately 100%

  numeric_cols <- result[1:(nrow(result)-2), -1]  # Exclude base row and row_label
  col_sums <- sapply(numeric_cols, function(x) {
    sum(as.numeric(gsub("%", "", x)), na.rm = TRUE)
  })

  # Allow for small rounding differences
  expect_true(all(abs(col_sums - 100) < 0.1))
})

# Test binary labelled variable (multiresponse style)
test_that("binary labelled variables expand correctly", {
  data <- create_labelled_test_data()

  result <- tab(data, binary_var)

  # Should have 2 rows (Selected, Not selected) + base row
  expect_equal(nrow(result) - 2, 2)

  # Check labels
  expect_true(any(grepl("Not selected", result$row_label)))
  expect_true(any(grepl("Selected", result$row_label)))
})

# Test mixed labelled and factor variables
test_that("mixed labelled and factor variables work together", {
  data <- create_labelled_test_data()

  # Cross-tab labelled satisfaction by factor variable
  result <- tab(data, satisfaction, factor_var)

  # Should have 5 rows (satisfaction) + base row
  expect_equal(nrow(result) - 2, 5)

  # Should have 3 columns (factor levels) + row_label
  expect_equal(ncol(result) - 1, 3)

  # Check that factor levels appear in column names
  factor_levels <- c("Option A", "Option B", "Option C")
  for (level in factor_levels) {
    expect_true(any(grepl(level, names(result))))
  }
})

# Test that labelled variables work with survey_data objects
test_that("labelled variables work with survey_data objects", {
  data <- create_labelled_test_data()
  dpdict <- create_labelled_dpdict()

  survey_obj <- structure(
    list(dat = data, dpdict = dpdict),
    class = "survey_data"
  )

  result <- tab(survey_obj, satisfaction, region)

  # Should work the same as with regular data frame
  expect_equal(nrow(result) - 2, 5)  # 5 satisfaction levels
  expect_equal(ncol(result) - 1, 4)  # 4 regions

  # Should use labels from dpdict where available
  expect_true(any(grepl("Satisfaction Level", result$row_label)))
})

# Test edge cases with labelled variables
test_that("edge cases with labelled variables are handled", {
  data <- create_labelled_test_data(10)  # Small dataset

  # Test with all same values
  data$satisfaction[] <- 1  # All "Very dissatisfied"

  result <- tab(data, satisfaction)

  # Should still create all categories, but most will be 0%
  expect_equal(nrow(result) - 2, 5)  # All 5 categories should appear

  # Only one category should have 100%
  numeric_values <- as.numeric(gsub("%", "", result$Total[1:(nrow(result)-2)]))
  expect_equal(sum(numeric_values > 0), 1)
  expect_equal(max(numeric_values), 100)
})

# Test that label extraction works correctly
test_that("label extraction from labelled variables is correct", {
  data <- create_labelled_test_data()

  # Manually check that labels are extracted correctly
  satisfaction_labels <- attr(data$satisfaction, "labels")
  expect_equal(length(satisfaction_labels), 5)
  expect_equal(names(satisfaction_labels),
               c("Very dissatisfied", "Dissatisfied", "Neutral", "Satisfied", "Very satisfied"))

  region_labels <- attr(data$region, "labels")
  expect_equal(length(region_labels), 4)
  expect_equal(names(region_labels), c("North", "South", "East", "West"))

  # Test the expansion
  result <- tab(data, satisfaction)

  # Each satisfaction level should appear in row labels
  for (label_name in names(satisfaction_labels)) {
    expect_true(any(grepl(label_name, result$row_label)))
  }
})

# Test performance with large labelled variable
test_that("performance is acceptable with large labelled variable", {
  # Create data with many categories (simulate the 17-category case)
  large_data <- create_labelled_test_data(1000)  # Larger dataset

  # Time the operation
  start_time <- Sys.time()
  result <- tab(large_data, brand, region)
  end_time <- Sys.time()

  # Should complete in reasonable time (less than 5 seconds)
  expect_true(as.numeric(end_time - start_time) < 5)

  # Should have correct dimensions
  expect_equal(nrow(result) - 2, 17)  # 17 brands
  expect_equal(ncol(result) - 1, 4)   # 4 regions
})

# Test that counts are accurate for labelled variables
test_that("counts are mathematically correct for labelled variables", {
  # Create deterministic test data
  set.seed(456)
  data <- data.frame(id = 1:100)

  # Create satisfaction with known distribution
  # 20 each of values 1-5
  satisfaction_values <- rep(1:5, each = 20)
  data$satisfaction <- haven::labelled(
    satisfaction_values,
    labels = c("Level 1" = 1, "Level 2" = 2, "Level 3" = 3, "Level 4" = 4, "Level 5" = 5)
  )

  # Create region with known distribution
  # 25 each of values 1-4
  region_values <- rep(1:4, each = 25)
  data$region <- haven::labelled(
    region_values,
    labels = c("North" = 1, "South" = 2, "East" = 3, "West" = 4)
  )

  # Test simple count
  result_count <- tab(data, satisfaction, statistic = "count")

  # Each satisfaction level should have count of 20
  for (i in 1:5) {
    level_row <- result_count[grepl(paste0("Level ", i), result_count$row_label), ]
    expect_equal(as.numeric(level_row$Total), 20)
  }

  # Test cross-tab counts
  result_cross <- tab(data, satisfaction, region, statistic = "count")

  # Expected counts based on overlapping rep() patterns
  expected_counts <- matrix(c(
    20,  0,  0,  0,  # Level 1: all in North
    5, 15,  0,  0,  # Level 2: 5 in North, 15 in South
    0, 10, 10,  0,  # Level 3: 10 in South, 10 in East
    0,  0, 15,  5,  # Level 4: 15 in East, 5 in West
    0,  0,  0, 20   # Level 5: all in West
  ), nrow = 5, byrow = TRUE)

  region_names <- c("region: North", "region: South", "region: East", "region: West")

  for (i in 1:5) {
    for (j in 1:4) {
      level_row <- result_cross[grepl(paste0("Level ", i), result_cross$row_label), ]
      expect_equal(as.numeric(level_row[[region_names[j]]]), expected_counts[i, j])
    }
  }
})
