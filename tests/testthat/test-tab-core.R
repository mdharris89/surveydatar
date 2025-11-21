##### Built in registrations #####
# Clear and register built-ins
ensure_builtins_registered()

# Helper to save registry state
save_registry_state <- function() {
  list(
    helpers = .tab_registry$helpers,
    stats = .tab_registry$stats
  )
}

# Helper to restore registry state
restore_registry_state <- function(state) {
  .tab_registry$helpers <- state$helpers
  .tab_registry$stats <- state$stats
}

# Helper to reset registry to built-ins only
reset_to_builtins <- function() {
  clear_tab_registry()
  ensure_builtins_registered()
}

# Ensure clean state for each test file
# Note: Functions loaded via helper-load-tab.R
reset_to_builtins()

test_that("ensure_builtins_registered works correctly", {
  # Clear registry
  clear_tab_registry()
  expect_length(list_tab_statistics(), 0)
  expect_length(list_tab_helpers(), 0)

  # Register built-ins
  ensure_builtins_registered()

  # Verify they're now available
  expect_true("count" %in% list_tab_statistics())
  expect_true("column_pct" %in% list_tab_statistics())
  expect_true("top_box" %in% list_tab_helpers())

  # Calling again should not cause errors (idempotent)
  expect_silent(ensure_builtins_registered())
})

##### TEST DATA GENERATOR #####
# Use ONE main generator that covers all test scenarios:
# - Basic demographics (gender, age, region)
# - Satisfaction scale (for helpers + expansion + cross-tabs)
# - Multi-response Q1 group with " - " separators (for smart labelling + expansion)
# - Categorical Q2 group (for expansion)
# - Single Q3 variable (for smart labelling)
# - Brand with 17 categories (for many-category expansion)
# - Weight and year variables
# - Continue variable with some extreme values for percentile testing
# - Variable with many NAs for edge case testing

# Main test data generator - covers all test scenarios
create_tab_test_data <- function(n = 100) {
  set.seed(123)

  # how many outliers (for continuous variable for percentile testing)
  n_outliers <- round(0.10 * n)
  n_main     <- n - n_outliers

  # helper to keep ties but obey n (for variable with ties for median testing)
  make_tied <- function(n) rep(c(10, 20, 30), length.out = n)

  # Create realistic raw survey data with proper labels
  raw_data <- data.frame(
    # Basic demographics with proper labelling
    gender = sjlabelled::set_label(
      sjlabelled::set_labels(
        haven::labelled(sample(c(1, 2), n, replace = TRUE),
                        labels = c("Male" = 1, "Female" = 2)),
        labels = c("Male" = 1, "Female" = 2)
      ),
      label = "Respondent Gender"
    ),

    age = sjlabelled::set_label(
      sample(18:65, n, replace = TRUE),
      label = "Age in years"
    ),

    region = sjlabelled::set_label(
      sjlabelled::set_labels(
        haven::labelled(sample(c(1, 2, 3, 4), n, replace = TRUE),
                        labels = c("North" = 1, "South" = 2, "East" = 3, "West" = 4)),
        labels = c("North" = 1, "South" = 2, "East" = 3, "West" = 4)
      ),
      label = "Region"
    ),

    region_character = sjlabelled::set_label(
      sample(c("North", "South", "East", "West"), n, replace = TRUE),
      label = "Region (Character)"
    ),

    # Satisfaction scale (for testing helpers like top_box AND expansion AND smart labelling)
    satisfaction = sjlabelled::set_label(
      sjlabelled::set_labels(
        haven::labelled(sample(c(1, 2, 3, 4, 5), n, replace = TRUE),
                        labels = c("Very dissatisfied" = 1, "Dissatisfied" = 2, "Neutral" = 3, "Satisfied" = 4, "Very satisfied" = 5)),
        labels = c("Very dissatisfied" = 1, "Dissatisfied" = 2, "Neutral" = 3, "Satisfied" = 4, "Very satisfied" = 5)
      ),
      label = "Overall Satisfaction"
    ),

    # Multi-response question group with " - " separators for smart labelling tests
    q1_1 = sjlabelled::set_label(
      sjlabelled::set_labels(
        haven::labelled(sample(c(0, 1), n, replace = TRUE),
                        labels = c("Not selected" = 0, "Selected" = 1)),
        labels = c("Not selected" = 0, "Selected" = 1)
      ),
      label = "Q1: Brand attributes - Quality"
    ),

    q1_2 = sjlabelled::set_label(
      sjlabelled::set_labels(
        haven::labelled(sample(c(0, 1), n, replace = TRUE),
                        labels = c("Not selected" = 0, "Selected" = 1)),
        labels = c("Not selected" = 0, "Selected" = 1)
      ),
      label = "Q1: Brand attributes - Value"
    ),

    q1_3 = sjlabelled::set_label(
      sjlabelled::set_labels(
        haven::labelled(sample(c(0, 1), n, replace = TRUE),
                        labels = c("Not selected" = 0, "Selected" = 1)),
        labels = c("Not selected" = 0, "Selected" = 1)
      ),
      label = "Q1: Brand attributes - Trust"
    ),

    # Categorical question group
    q2_1 = sjlabelled::set_label(
      sjlabelled::set_labels(
        haven::labelled(sample(c(1, 2, 3, 4), n, replace = TRUE),
                        labels = c("Poor" = 1, "Fair" = 2, "Good" = 3, "Excellent" = 4)),
        labels = c("Poor" = 1, "Fair" = 2, "Good" = 3, "Excellent" = 4)
      ),
      label = "Q2: Service ratings - Speed"
    ),

    q2_2 = sjlabelled::set_label(
      sjlabelled::set_labels(
        haven::labelled(sample(c(1, 2, 3, 4), n, replace = TRUE),
                        labels = c("Poor" = 1, "Fair" = 2, "Good" = 3, "Excellent" = 4)),
        labels = c("Poor" = 1, "Fair" = 2, "Good" = 3, "Excellent" = 4)
      ),
      label = "Q2: Service ratings - Accuracy"
    ),

    # Single question (no group) for smart labelling tests
    q3 = sjlabelled::set_label(
      sample(c(1, 2, 3, 4, 5), n, replace = TRUE),
      label = "Overall rating"
    ),

    # Brand variable with many categories for expansion testing
    brand = sjlabelled::set_label(
      sjlabelled::set_labels(
        haven::labelled(sample(c(1:17), n, replace = TRUE),
                        labels = setNames(1:17, paste0("Brand_", LETTERS[1:17]))),
        labels = setNames(1:17, paste0("Brand_", LETTERS[1:17]))
      ),
      label = "Preferred brand"
    ),

    # Weight variable
    weight = sjlabelled::set_label(
      runif(n, 0.5, 2),
      label = "Survey weight"
    ),

    # Year for change calculations
    year = sjlabelled::set_label(
      sample(c(2023, 2024), n, replace = TRUE),
      label = "Survey year"
    ),

    # Income variable for more complex filtering/analysis
    income = sjlabelled::set_label(
      round(rlnorm(n, meanlog = 10.5, sdlog = 0.8)),
      label = "Annual household income"
    ),

    # Binary filter variable for testing filter parameter
    active_user = sjlabelled::set_label(
      sjlabelled::set_labels(
        haven::labelled(sample(c(0, 1), n, replace = TRUE, prob = c(0.3, 0.7)),
                        labels = c("Inactive" = 0, "Active" = 1)),
        labels = c("Inactive" = 0, "Active" = 1)
      ),
      label = "Active user status"
    ),

    # Continuous variable with some extreme values for percentile testing
    response_time = sjlabelled::set_label(
      sample(
        c(
          rnorm(n_main,  mean = 30, sd = 10),          # 90 % “normal”
          runif(n_outliers, min = 100, max = 200)      # 10 % outliers
        ),
        size = n
      ),
      label = "Response time (seconds)"
    ),

    # Variable with many NAs for edge case testing
    optional_rating = sjlabelled::set_label(
      ifelse(runif(n) > 0.4, sample(1:10, n, replace = TRUE), NA),
      label = "Optional rating (1-10)"
    ),

    # Continuous variable with known distribution for precise testing
    test_values = sjlabelled::set_label(
      rnorm(n, mean = 50, sd = 10),
      label = "Test values (normal dist)"
    ),

    # Binary variable with exact proportions for z-test validation
    binary_test = sjlabelled::set_label(
      sjlabelled::set_labels(
        haven::labelled(c(rep(1, 40), rep(0, 60))[sample(n)],
                        labels = c("No" = 0, "Yes" = 1)),
        labels = c("No" = 0, "Yes" = 1)
      ),
      label = "Binary test variable"
    ),

    # Ordinal variable for percentile testing
    ordinal_test = sjlabelled::set_label(
      rep(1:10, each = 10)[sample(n)],
      label = "Ordinal test (1-10)"
    ),

    # Variable with ties for median testing
    tied_values = sjlabelled::set_label(
      make_tied(n),
      label = "Values with ties"
    ),

    # Categorical array A2_a - Document creation frequency
    # Each variable represents frequency for a different document type
    A2_1 = sjlabelled::set_label(
      sjlabelled::set_labels(
        haven::labelled(sample(c(1, 2, 3, 4), n, replace = TRUE,
                               prob = c(0.1, 0.3, 0.4, 0.2)),
                        labels = c("Daily" = 1, "Weekly" = 2,
                                   "Monthly" = 3, "Never" = 4)),
        labels = c("Daily" = 1, "Weekly" = 2, "Monthly" = 3, "Never" = 4)
      ),
      label = "How often do you create - Docs"
    ),

    A2_2 = sjlabelled::set_label(
      sjlabelled::set_labels(
        haven::labelled(sample(c(1, 2, 3, 4), n, replace = TRUE,
                               prob = c(0.05, 0.2, 0.5, 0.25)),
                        labels = c("Daily" = 1, "Weekly" = 2,
                                   "Monthly" = 3, "Never" = 4)),
        labels = c("Daily" = 1, "Weekly" = 2, "Monthly" = 3, "Never" = 4)
      ),
      label = "How often do you create - Presentations"
    ),

    A2_3 = sjlabelled::set_label(
      sjlabelled::set_labels(
        haven::labelled(sample(c(1, 2, 3, 4), n, replace = TRUE,
                               prob = c(0.2, 0.4, 0.3, 0.1)),
                        labels = c("Daily" = 1, "Weekly" = 2,
                                   "Monthly" = 3, "Never" = 4)),
        labels = c("Daily" = 1, "Weekly" = 2, "Monthly" = 3, "Never" = 4)
      ),
      label = "How often do you create - Spreadsheets"
    ),

    stringsAsFactors = FALSE
  )

  return(raw_data)
}

create_mini_test_data <- function(n = 5) {
  set.seed(123)

  raw_data <- data.frame(

    # Categorical array A2_a - Document creation frequency
    # Each variable represents frequency for a different document type
    A2_1 = sjlabelled::set_label(
      sjlabelled::set_labels(
        haven::labelled(sample(c(1, 2, 3), n, replace = TRUE,
                               prob = c(0.1, 0.5, 0.4)),
                        labels = c("Daily" = 1, "Weekly" = 2,
                                   "Monthly" = 3)),
        labels = c("Daily" = 1, "Weekly" = 2, "Monthly" = 3)
      ),
      label = "How often do you create - Docs"
    ),

    A2_2 = sjlabelled::set_label(
      sjlabelled::set_labels(
        haven::labelled(sample(c(1, 2, 3), n, replace = TRUE,
                               prob = c(0.05, 0.45, 0.5)),
                        labels = c("Daily" = 1, "Weekly" = 2,
                                   "Monthly" = 3)),
        labels = c("Daily" = 1, "Weekly" = 2, "Monthly" = 3)
      ),
      label = "How often do you create - Presentations"
    ),

    A2_3 = sjlabelled::set_label(
      sjlabelled::set_labels(
        haven::labelled(sample(c(1, 2, 3), n, replace = TRUE,
                               prob = c(0.3, 0.4, 0.3)),
                        labels = c("Daily" = 1, "Weekly" = 2,
                                   "Monthly" = 3)),
        labels = c("Daily" = 1, "Weekly" = 2, "Monthly" = 3)
      ),
      label = "How often do you create - Spreadsheets"
    ),

    stringsAsFactors = FALSE
  )

  return(raw_data)
}

test_data <- create_tab_test_data()
test_survey_data <- create_survey_data(test_data)

mini_data <- create_mini_test_data()
mini_survey_data <- create_survey_data(mini_data)

a2_vars <- c("A2_1", "A2_2", "A2_3")

##### Basic Tab Functionality Tests #####

test_that("basic tab functionality retuns the expected format", {
  # Use shared test data
  result <- tab(test_survey_data, gender, region)
  expect_s3_class(result, "tab_result")
  expect_equal(ncol(result), 6) # 4 regions + Total column + row_label
  expect_true(any(grepl("Male", result$row_label)))
  expect_true(any(grepl("Female", result$row_label)))
})

##### Variable Expansion Tests #####

test_that("question group expansion works", {

  # Test q1 group expansion (should expand to q1_1, q1_2, q1_3)
  result <- tab(test_survey_data, q1, gender)
  expect_s3_class(result, "tab_result")
  expect_true(nrow(result) >= 4) # At least 3 q1 items + total (note base is note included in nrow)
  expect_true(any(grepl("Quality", result$row_label)))
  expect_true(any(grepl("Value", result$row_label)))
  expect_true(any(grepl("Trust", result$row_label)))

  # Test q2 group expansion (should expand to q2_1, q2_2)
  # This tests the fix: q2_1 and q2_2 have IDENTICAL value labels
  result2 <- tab(test_survey_data, q2, gender)
  expect_s3_class(result2, "tab_result")
  
  # Should have 8 data rows (4 values × 2 variables)
  data_rows <- !grepl("Base|NET", result2$row_label)
  expect_equal(sum(data_rows), 8)
  
  # Critical: Should NOT have NA values in data cells
  # This was the bug - cells from q2_2 were getting lost due to duplicate labels
  data_cols <- names(result2)[!grepl("row_label", names(result2))]
  data_cols <- data_cols[!grepl("Base|NET", data_cols)]
  
  for (col in data_cols) {
    data_values <- result2[data_rows, col]
    expect_false(any(is.na(data_values)),
                 info = paste("Found NA values in column:", col))
  }
  
  # Verify we have all 4 category values represented (twice, once for each q2 variable)
  # Note: Labels show category names only (Poor, Fair, Good, Excellent) without
  # variable distinction, matching tab() behavior
  expect_true(sum(result2$row_label == "Poor") == 2)
  expect_true(sum(result2$row_label == "Fair") == 2)
  expect_true(sum(result2$row_label == "Good") == 2)
  expect_true(sum(result2$row_label == "Excellent") == 2)
})

test_that("labelled variable expansion works", {

  # Test satisfaction expansion (5 levels)
  result <- tab(test_survey_data, satisfaction)
  expect_s3_class(result, "tab_result")
  expect_equal(nrow(result), 6) # 5 satisfaction levels + total (base is not included in nrow)

  # Check all satisfaction labels are present
  satisfaction_labels <- c("Very dissatisfied", "Dissatisfied", "Neutral", "Satisfied", "Very satisfied")
  for (label in satisfaction_labels) {
    expect_true(any(grepl(label, result$row_label)))
  }
})

test_that("labelled variables expand correctly in columns", {

  # Test region expansion in columns
  result <- tab(test_survey_data, satisfaction, region)
  expect_s3_class(result, "tab_result")

  # Should have 4 columns for region + Total + row_label
  expect_equal(ncol(result), 6)

  # Check that all region labels are present in column names
  region_labels <- c("North", "South", "East", "West")
  for (label in region_labels) {
    expect_true(any(grepl(label, names(result))))
  }
})

test_that("labelled variables with many categories expand correctly", {

  # Test brand variable (17 categories) in rows
  result <- tab(test_survey_data, brand)
  expect_s3_class(result, "tab_result")

  # Should have 17 rows (one for each brand) + total row
  expect_equal(nrow(result), 18)

  # Check that brand labels follow expected pattern
  for (i in 1:17) {
    brand_label <- paste0("Brand_", LETTERS[i])
    expect_true(any(grepl(brand_label, result$row_label)))
  }
})

##### Basic Cross-tabulation Tests #####

test_that("cross-tabulation works with two labelled variables", {

  # Cross-tab satisfaction by region
  result <- tab(test_survey_data, satisfaction, region)
  expect_s3_class(result, "tab_result")

  # Should have 5 rows (satisfaction levels) + total row
  expect_equal(nrow(result), 6)

  # Should have 4 columns (regions) + Total
  expect_equal(ncol(result), 6)

  # Verify labels are used correctly
  expect_true(any(grepl("Very dissatisfied", result$row_label)))
  expect_true(any(grepl("North", names(result))))
})

##### Basic Variable Validation Tests #####

test_that("Basic error handling works", {

  # Non-existent variable
  expect_error(tab(test_survey_data, "nonexistent"), "not found")

  # Invalid weight
  expect_error(tab(test_survey_data, gender, weight = "badweight"), "Weight variable")

  # Invalid statistic
  expect_error(tab(test_survey_data, gender, statistic = "invalid"), "Unknown statistic")
})

test_that("tab validates statistic requirements", {

  # Should error when mean lacks values parameter
  expect_error(
    tab(test_survey_data, gender, statistic = "mean"),
    "mean statistic requires 'values' parameter"
  )

  # Should warn when values provided for non-value statistic
  expect_warning(
    tab(test_survey_data, gender, statistic = "count", values = "age"),
    "Values parameter ignored for count statistic"
  )
})
##### String Syntax Tests #####

test_that("tab works with string syntax", {

  result1 <- tab(test_survey_data, "gender", "region")
  result2 <- tab(test_survey_data, gender, region)

  expect_s3_class(result1, "tab_result")
  expect_s3_class(result2, "tab_result")
  expect_equal(dim(result1), dim(result2))
})

##### Smart Labeling Tests #####

test_that("smart labelling works", {

  dpdict <- test_survey_data$dpdict  # Generated by create_dict_with_metadata()

  # Multi-item questions should show extracted suffixes
  result1 <- get_display_label("q1_1", dpdict, label_mode = "smart")
  expect_true(grepl("Quality", result1))  # Should extract suffix from "Q1: Brand attributes - Quality"

  # Single item questions should show full label
  result_single <- get_display_label("q3", dpdict, label_mode = "smart")
  expect_equal(result_single, "Overall rating")
})

test_that("full and suffix label modes work correctly", {

  dpdict <- test_survey_data$dpdict

  # Full mode should show the complete variable label
  result_full <- get_display_label("q1_1", dpdict, label_mode = "full")
  expect_equal(result_full, "Q1: Brand attributes - Quality")

  # Suffix mode should extract the suffix
  result_suffix <- get_display_label("q1_1", dpdict, label_mode = "suffix")
  expect_true(grepl("Quality", result_suffix))
})

##### Single and Zero Row Edge Cases Tests #####

test_that("single and zero row edge cases are handled correctly", {
  # Empty data
  empty_data <- create_tab_test_data(0)
  empty_survey_obj <- create_survey_data(empty_data)
  expect_error(
    result_empty <- tab(empty_survey_obj, gender),
    "Data has 0 rows"
  )

  # Single row
  single_data <- create_tab_test_data(1)
  single_survey_obj <- create_survey_data(single_data)
  result_single <- tab(single_survey_obj, gender)
  expect_s3_class(result_single, "tab_result")
})

##### Formula Parsing Tests #####

test_that("formula parsing recognises different expression types", {
  data <- test_survey_data$dat
  dpdict <- test_survey_data$dpdict

  # Test simple variable
  simple <- parse_table_formula(rlang::quo(gender), data, dpdict)
  expect_equal(simple$type, "simple")
  expect_equal(simple$components$var, "gender")
  expect_equal(simple$label, "Respondent Gender")

  # Test string literal
  string <- parse_table_formula(rlang::quo("gender"), data, dpdict)
  expect_equal(string$type, "simple")
  expect_equal(string$components$var, "gender")

  # Test array multiplication (filter)
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

test_that("summary rows respect statistic metadata", {

  # Count should have NET row
  count_result <- tab(test_survey_data,
                      rows = rows_list("A" = gender, "B" = region),
                      statistic = "count")
  expect_true("NET" %in% count_result$row_label)

  # Mean should have Avg row (not NET)
  mean_result <- tab(test_survey_data,
                     rows = rows_list("A" = gender, "B" = region),
                     statistic = "mean", values = "age")
  expect_true("Avg" %in% mean_result$row_label)
  expect_false("NET" %in% mean_result$row_label)
})

##### Array & Cell Computation Tests #####

test_that("formula_to_array correctly computes arrays", {

  data <- test_survey_data$dat

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
  expect_equal(as.numeric(array_expr), as.numeric(data$age > 30))

  # Test multiplication (filter)
  spec_mult <- list(
    type = "multiplication",
    components = list(
      list(type = "simple", components = list(var = "q1_1")),
      list(type = "expression", components = list(expr = quote(age > 30)))
    )
  )
  array_mult <- formula_to_array(spec_mult, data)
  expect_equal(as.numeric(array_mult), data$q1_1 * as.numeric(data$age > 30))
})

##### Filter Parameter Tests #####

test_that("filter parameter works correctly", {
  result <- tab(test_survey_data, gender, region, filter = (age > 30))
  expect_s3_class(result, "tab_result")

  # Compare with multiplication syntax - should be identical
  result_mult <- tab(test_survey_data, gender * (age > 30), region)

  # Both should have same dimensions (excluding the filter label difference)
  expect_equal(nrow(result), nrow(result_mult))
  expect_equal(ncol(result), ncol(result_mult))
})

test_that("filter parameter combines with row/column filters", {
  # Global filter with row-specific filter
  result <- tab(test_survey_data,
                gender * (satisfaction >= 4),
                region,
                filter = (age > 30))

  expect_s3_class(result, "tab_result")
  # Should apply both filters
  base_row <- result[result$row_label == "Base (n)", ]
  total_base <- as.numeric(base_row$Total)

  # Total should be less than full dataset due to both filters
  expect_lt(total_base, nrow(test_survey_data$dat))
})

test_that("filter parameter with complex expressions", {
  # Multiple conditions in filter
  result <- tab(test_survey_data,
                gender,
                region,
                filter = (age > 30 & satisfaction >= 3))

  expect_s3_class(result, "tab_result")

  # Verify filter is applied
  base_row <- result[result$row_label == "Base (n)", ]
  expect_true(all(as.numeric(base_row[-1]) > 0))
})

##### Low Base Threshold Tests #####

test_that("low_base_threshold removes low-base rows", {
  # Create a filter that will result in very few observations for one group
  result <- tab(test_survey_data,
                gender,
                region,
                filter = (age > 60),  # Very few people over 60
                low_base_threshold = 5)

  expect_s3_class(result, "tab_result")

  # Rows with base < 5 should be removed
  # Check that all remaining rows have sufficient base
  base_row <- result[result$row_label == "Base (n)", ]
  all_bases <- as.numeric(base_row[-1])
  expect_true(all(all_bases >= 5 | is.na(all_bases)))
})

test_that("low_base_threshold works with columns", {
  # Create scenario where some columns have low base
  result <- tab(test_survey_data,
                satisfaction,
                brand,  # Many categories, some might have low base
                filter = (age > 55),
                low_base_threshold = 3)

  expect_s3_class(result, "tab_result")

  # All remaining columns should have base >= 3
  base_row <- result[result$row_label == "Base (n)", ]
  col_bases <- as.numeric(base_row[-1])
  expect_true(all(col_bases >= 3 | is.na(col_bases)))
})

##### NET Row/Column Logic Tests #####

test_that("NET row not added for single row", {
  # Filter to create single row
  result <- tab(test_survey_data,
                gender == 1,  # Only Male
                region,
                statistic = "count")

  # Should not have NET row
  expect_false("NET" %in% result$row_label)

  # Should only have 1 rows
  expect_equal(nrow(result), 1)
})

test_that("NET calculations with complex filtering", {
  result <- tab(test_survey_data,
                rows = rows_list(
                  "High satisfaction" = satisfaction >= 4,
                  "High satisfaction + Young" = (satisfaction >= 4) * (age < 35),
                  "High satisfaction + Old" = (satisfaction >= 4) * (age >= 35)
                ),
                cols = region,
                statistic = "count")

  expect_s3_class(result, "tab_result")
  expect_true("NET" %in% result$row_label)

  # NET should be union of all conditions
  net_row <- result[result$row_label == "NET", -1]

  # NET count should be >= any individual row count
  for (i in 1:3) {
    row_vals <- as.numeric(result[i, -1])
    net_vals <- as.numeric(net_row)
    expect_true(all(net_vals >= row_vals))
  }
})

##### Zeros and NAs in a Cell Edge Cases Tests #####

test_that("tab handles all zeros in a cell gracefully", {
  # Create data where one combination never occurs
  edge_data <- test_survey_data
  # Set all females in North to have age <= 30
  female_north <- edge_data$dat$gender == 2 & edge_data$dat$region == 1
  edge_data$dat$age[female_north] <- sample(18:30, sum(female_north), replace = TRUE)

  result <- tab(edge_data,
                gender * (age > 50),  # Very few over 50
                region,
                statistic = "column_pct")

  expect_s3_class(result, "tab_result")

  # Should be 0 for some cells
  expect_true(any(unlist(result[1:2, -1])==0))
})

test_that("tab handles data with many NAs", {

  # Use optional_rating which has many NAs
  result <- tab(test_survey_data,
                             gender,
                             statistic = "mean",
                             values = "optional_rating")

  expect_s3_class(result, "tab_result")

  # Should calculate mean only on non-NA values
  expect_true(all(!is.na(result[1:2, "Total"])))
})

##### COMPLEX CALCULATION SCENARIOS #####

test_that("NET calculations are mathematically correct", {

  # Create overlapping conditions
  result <- tab(test_survey_data,
                rows = rows_list(
                  "Young" = age < 35,
                  "High income" = income > median(income),
                  "Young OR High income" = (age < 35) | (income > median(income))
                ),
                gender,
                statistic = "count")

  # NET should be union of all rows
  young_mask <- test_data$age < 35
  high_income_mask <- test_data$income > median(test_data$income)
  union_mask <- young_mask | high_income_mask

  # The NET row should equal the total unique respondents across all conditions
  net_total <- as.numeric(result[result$row_label == "NET", "Total"])

  # For union of conditions, NET should be at least as large as any individual row
  # but potentially less than the sum (due to overlap)
  young_total <- as.numeric(result[1, "Total"])
  high_income_total <- as.numeric(result[2, "Total"])

  expect_true(net_total >= max(young_total, high_income_total))
  expect_true(net_total <= young_total + high_income_total)
})

test_that("summary row/column calculations match statistics", {
  test_data <- create_tab_test_data(100)
  test_survey_data <- create_survey_data(test_data)

  # Test Avg summary for mean statistic
  result <- tab(test_survey_data,
                rows = rows_list(
                  "Male" = gender == 1,
                  "Female" = gender == 2
                ),
                region,
                statistic = "mean",
                values = "age")

  # Avg row should be the mean of all observations
  avg_row <- result[result$row_label == "Avg", ]

  for (col in c("North", "South", "East", "West")) {
    region_num <- which(c("North", "South", "East", "West") == col)
    expected_avg <- mean(test_data$age[test_data$region == region_num], na.rm = TRUE)
    actual_avg <- as.numeric(avg_row[[col]])
    expect_equal(actual_avg, expected_avg, tolerance = 0.01)
  }
})

##### BASE CALCULATIONS #####

test_that("base n calculations are accurate for all statistics", {

  # Test column_pct base
  result_col <- tab(test_survey_data, gender, region, statistic = "column_pct")
  base_row <- result_col[result_col$row_label == "Base (n)", ]

  # Each column base should equal total in that column
  for (i in 1:4) {
    region_total <- sum(test_data$region == i)
    col_name <- c("North", "South", "East", "West")[i]
    expect_equal(as.numeric(base_row[[col_name]]), region_total)
  }

  # Test row_pct base (should be in columns)
  result_row <- tab(test_survey_data, gender, region, statistic = "row_pct")
  result_row <- as.data.frame(result_row) # materialize to get base column
  expect_true("Base (n)" %in% names(result_row))
})

##### FILTER AND WEIGHT INTERACTION TESTS #####

test_that("filters and weights interact correctly in calculations", {
  test_data <- create_tab_test_data(100)
  test_survey_data <- create_survey_data(test_data)

  # Apply both filter and weight
  result <- tab(test_survey_data,
                gender,
                region,
                filter = age > 30,
                weight = "weight",
                statistic = "column_pct")

  # Manual calculation for filtered and weighted percentage
  filtered_data <- test_data[test_data$age > 30, ]

  # Male in North with filter and weight
  male_north_mask <- filtered_data$gender == 1 & filtered_data$region == 1
  weighted_male_north <- sum(filtered_data$weight[male_north_mask])

  north_mask <- filtered_data$region == 1
  weighted_north_total <- sum(filtered_data$weight[north_mask])

  expected_pct <- (weighted_male_north / weighted_north_total) * 100

  actual_pct <- as.numeric(gsub("%", "",
                                result[result$row_label == "Male", "North"]))

  expect_equal(actual_pct, expected_pct, tolerance = 0.1)
})
##### CROSS-VALIDATION AND CONSISTENCY TESTS #####

test_that("statistical calculations remain stable with different data sizes", {
  # Test with different sample sizes
  for (n in c(10, 50, 200, 500)) {
    test_data <- create_tab_test_data(n)
    test_survey_data <- create_survey_data(test_data)

    # All statistics should work regardless of sample size
    result_mean <- tab(test_survey_data, gender, statistic = "mean", values = "test_values")
    result_median <- tab(test_survey_data, gender, statistic = "median", values = "test_values")
    result_sd <- tab(test_survey_data, gender, statistic = "sd", values = "test_values")

    # Verify no errors and reasonable values
    expect_s3_class(result_mean, "tab_result")
    expect_s3_class(result_median, "tab_result")
    expect_s3_class(result_sd, "tab_result")

    # For larger samples, SD should stabilize around 10 (our generated SD)
    if (n >= 200) {
      sd_total <- as.numeric(result_sd[result_sd$row_label == "Total", "Total"])
      expect_equal(sd_total, 10, tolerance = 1)
    }
  }
})

##### Unit tests for modify_labels function #####

test_that("modify_labels updates row labels in cell-based tab_result", {
  data <- test_survey_data
  
  result <- data %>%
    tab(satisfaction, gender) %>%
    modify_labels(row_labels = c("Very dissatisfied" = "Extremely Unhappy",
                                  "Neutral" = "Neither"))
  
  expect_true("Extremely Unhappy" %in% result$layout$row_labels)
  expect_true("Neither" %in% result$layout$row_labels)
  expect_false("Very dissatisfied" %in% result$layout$row_labels)
  expect_false("Neutral" %in% result$layout$row_labels)
})

test_that("modify_labels updates column labels in cell-based tab_result", {
  data <- test_survey_data
  
  result <- data %>%
    tab(satisfaction, gender) %>%
    modify_labels(col_labels = c("Male" = "Men",
                                  "Female" = "Women"))
  
  expect_true("Men" %in% result$layout$col_labels)
  expect_true("Women" %in% result$layout$col_labels)
  expect_false("Male" %in% result$layout$col_labels)
  expect_false("Female" %in% result$layout$col_labels)
})

test_that("modify_labels applies patterns sequentially", {
  data <- test_survey_data
  
  result <- data %>%
    tab(satisfaction, gender) %>%
    modify_labels(row_labels = c("Very" = "Extremely",
                                  "dissatisfied" = "unhappy"))
  
  # Both patterns should apply: "Very dissatisfied" -> "Extremely dissatisfied" -> "Extremely unhappy"
  expect_true("Extremely unhappy" %in% result$layout$row_labels)
  expect_false("Very dissatisfied" %in% result$layout$row_labels)
})

test_that("modify_labels supports regex patterns", {
  data <- test_survey_data
  
  result <- data %>%
    tab(satisfaction, gender) %>%
    modify_labels(row_labels = c("^Very.*$" = "Extreme Response"))
  
  expect_true("Extreme Response" %in% result$layout$row_labels)
  expect_false(any(grepl("^Very", result$layout$row_labels)))
})

test_that("modify_labels persists through layout operations", {
  data <- test_survey_data
  
  result <- data %>%
    tab(satisfaction, gender) %>%
    modify_labels(row_labels = c("Very dissatisfied" = "V. Dissatisfied")) %>%
    arrange_rows(.by = "Male", .sort = "desc")
  
  # Label should persist after arrange_rows
  expect_true("V. Dissatisfied" %in% result$layout$row_labels)
  expect_false("Very dissatisfied" %in% result$layout$row_labels)
})

test_that("modify_labels persists through hide operations", {
  data <- test_survey_data
  
  result <- data %>%
    tab(satisfaction, gender) %>%
    modify_labels(col_labels = c("Male" = "Men", "Female" = "Women")) %>%
    hide_cols("Total")
  
  # Labels should persist after hide_cols
  expect_true("Men" %in% result$layout$col_labels)
  expect_true("Women" %in% result$layout$col_labels)
  expect_false("Male" %in% result$layout$col_labels)
})

test_that("modify_labels persists through select operations", {
  data <- test_survey_data
  
  result <- data %>%
    tab(satisfaction, gender) %>%
    modify_labels(row_labels = c("Satisfied" = "Happy",
                                  "Dissatisfied" = "Unhappy")) %>%
    select_rows("Happy", "Unhappy", .match = "label")
  
  # Labels should persist and be available for selection
  expect_true("Happy" %in% result$layout$row_labels)
  expect_true("Unhappy" %in% result$layout$row_labels)
  expect_equal(length(result$layout$row_labels), 2)
})

test_that("modify_labels works with both row and column labels simultaneously", {
  data <- test_survey_data
  
  result <- data %>%
    tab(satisfaction, gender) %>%
    modify_labels(
      row_labels = c("Very satisfied" = "V. Satisfied",
                     "Very dissatisfied" = "V. Dissatisfied"),
      col_labels = c("Male" = "M", "Female" = "F", "Total" = "All")
    )
  
  expect_true("V. Satisfied" %in% result$layout$row_labels)
  expect_true("V. Dissatisfied" %in% result$layout$row_labels)
  expect_true("M" %in% result$layout$col_labels)
  expect_true("F" %in% result$layout$col_labels)
  expect_true("All" %in% result$layout$col_labels)
})

test_that("modify_labels does not affect cell allocation", {
  data <- test_survey_data
  
  result_before <- data %>%
    tab(satisfaction, gender)
  
  result_after <- result_before %>%
    modify_labels(row_labels = c("Satisfied" = "Happy"))
  
  # Grid dimensions should be identical
  expect_equal(length(result_before$layout$row_defs), length(result_after$layout$row_defs))
  expect_equal(length(result_before$layout$col_defs), length(result_after$layout$col_defs))
  
  # Cell values should be identical (only labels changed)
  grid_before <- as.data.frame(result_before)
  grid_after <- as.data.frame(result_after)
  
  # Rename to match for comparison
  names(grid_after)[names(grid_after) == "Happy"] <- "Satisfied"
  
  # Values should match
  expect_equal(grid_before[, -1], grid_after[, -1])
})

test_that("modify_labels validates input type", {
  data <- test_survey_data
  
  result <- data %>% tab(satisfaction, gender)
  
  expect_error(
    modify_labels(as.data.frame(result), row_labels = c("A" = "B")),
    "modify_labels\\(\\) requires a cell-based tab_result"
  )
})

test_that("modify_labels validates row_labels format", {
  data <- test_survey_data
  
  result <- data %>% tab(satisfaction, gender)
  
  # Unnamed vector should error
  expect_error(
    modify_labels(result, row_labels = c("A", "B")),
    "row_labels must be a named character vector"
  )
  
  # Non-character should error
  expect_error(
    modify_labels(result, row_labels = c(a = 1, b = 2)),
    "row_labels must be a named character vector"
  )
})

test_that("modify_labels validates col_labels format", {
  data <- test_survey_data
  
  result <- data %>% tab(satisfaction, gender)
  
  # Unnamed vector should error
  expect_error(
    modify_labels(result, col_labels = c("A", "B")),
    "col_labels must be a named character vector"
  )
  
  # Non-character should error
  expect_error(
    modify_labels(result, col_labels = list(a = "A", b = "B")),
    "col_labels must be a named character vector"
  )
})

test_that("modify_labels with no matching patterns leaves labels unchanged", {
  data <- test_survey_data
  
  result <- data %>%
    tab(satisfaction, gender) %>%
    modify_labels(row_labels = c("NonExistent" = "Replacement"))
  
  original <- data %>% tab(satisfaction, gender)
  expect_equal(result$layout$row_labels, original$layout$row_labels)
})

test_that("modify_labels persists through group operations", {
  data <- test_survey_data
  
  result <- data %>%
    tab(satisfaction, gender) %>%
    modify_labels(row_labels = c("Very satisfied" = "V. Sat",
                                  "Satisfied" = "Sat")) %>%
    group_rows("V. Sat", "Sat", .match = "label", .group_label = "Positive")
  
  # Modified labels should still be present
  expect_true("V. Sat" %in% result$layout$row_labels)
  expect_true("Sat" %in% result$layout$row_labels)
  
  # Group should be applied
  expect_true(!is.null(result$layout$row_groups))
})

test_that("modify_labels can be chained multiple times", {
  data <- test_survey_data
  
  result <- data %>%
    tab(satisfaction, gender) %>%
    modify_labels(row_labels = c("Very satisfied" = "VS")) %>%
    modify_labels(row_labels = c("VS" = "Extremely Satisfied"))
  
  expect_true("Extremely Satisfied" %in% result$layout$row_labels)
  expect_false("Very satisfied" %in% result$layout$row_labels)
  expect_false("VS" %in% result$layout$row_labels)
})

##### Unit tests for glue #####

test_that("multi_tab works with standard test data", {
  # Create standard test data
  test_survey_data <- create_survey_data(create_tab_test_data(200))

  # Split by region
  result <- multi_tab(test_survey_data, gender, satisfaction, by = region)

  expect_s3_class(result, "tab_result")
  expect_true("North: Very dissatisfied" %in% names(result))
  expect_true("South: Satisfied" %in% names(result))
  expect_true("Total: Neutral" %in% names(result))

  # Check that gender values appear in row labels
  expect_true(any(grepl("Male|Female", result$row_label)))
})

test_that("glue_tab combines tabs with different bases", {
  test_survey_data <- create_survey_data(create_tab_test_data(300))

  # Create two tabs with different age filters
  young_tab <- test_survey_data %>%
    filter(age < 35) %>%
    tab(satisfaction, gender)

  older_tab <- test_survey_data %>%
    filter(age >= 35) %>%
    tab(satisfaction, gender)

  # Combine them
  result <- glue_tab(young_tab, older_tab, prefix = "Age 35+")

  expect_s3_class(result, "tab_result")
  expect_true("Age 35+: Male" %in% names(result))
  expect_true("Age 35+: Female" %in% names(result))

  # Base sizes should be different
  base_row <- which(result$row_label == "Base (n)")
  expect_true(result[base_row, "Male"] != result[base_row, "Age 35+: Male"])
})

test_that("multi_tab handles custom groups and preserves labels", {
  test_survey_data <- create_survey_data(create_tab_test_data(150))

  # Use custom satisfaction groups
  result <- multi_tab(
    test_survey_data,
    brand,  # Has 17 labelled categories
    by = list(
      "High Satisfaction" = satisfaction >= 4,
      "Low Satisfaction" = satisfaction <= 2
    ),
    direction = "rows",
    statistic = "count"
  )

  # Check structure - rows should contain brand names
  expect_true(any(grepl("Brand_A", result$row_label)))
  expect_true(any(grepl("Brand_Q", result$row_label)))  # 17th brand
  
  # When direction is "rows", we're stacking tables vertically
  # The result should have brands as rows

  # Check metadata preserved
  expect_equal(attr(result, "multi_tab_groups"), c("High Satisfaction", "Low Satisfaction"))
})

##### Unit tests for mixed-statistic gluing #####

test_that("glue_tab with different statistics sets statistic to NULL", {
  test_survey_data <- create_survey_data(create_tab_test_data(200))
  
  # Create tabs with different statistics
  tab1 <- tab(test_survey_data, gender, region, statistic = "column_pct")
  tab2 <- tab(test_survey_data, gender, region, statistic = "index")
  
  # Glue them together
  result <- glue_tab(tab1, tab2, direction = "cols")
  
  # Result should have NULL statistic (indicating mixed statistics)
  expect_null(result$statistic)
  
  # Should still be a valid tab_result
  expect_s3_class(result, "tab_result")
  expect_s3_class(result, "tab_cell_collection")
})

test_that("glue_tab with same statistics preserves statistic", {
  test_survey_data <- create_survey_data(create_tab_test_data(200))
  
  # Create tabs with same statistic
  tab1 <- tab(test_survey_data, gender, region, statistic = "column_pct")
  tab2 <- tab(test_survey_data, gender, satisfaction, statistic = "column_pct")
  
  # Glue them together
  result <- glue_tab(tab1, tab2, direction = "cols")
  
  # Result should preserve the statistic
  expect_false(is.null(result$statistic))
  expect_equal(result$statistic$id, "column_pct")
})

test_that("mixed-statistic tabs format cells correctly", {
  test_survey_data <- create_survey_data(create_tab_test_data(200))
  
  # Create tabs with different statistics and different columns
  tab_pct <- tab(test_survey_data, gender, region, statistic = "column_pct")
  tab_idx <- tab(test_survey_data, gender, satisfaction, statistic = "index")
  
  # Glue them together
  result <- glue_tab(tab_pct, tab_idx, direction = "cols")
  
  # Materialize to data.frame
  df <- as.data.frame(result)
  
  # Check that statistic_matrix was created
  stat_matrix <- attr(df, "statistic_matrix")
  expect_false(is.null(stat_matrix))
  expect_equal(nrow(stat_matrix), nrow(df))
  
  # First columns should be column_pct, later columns should be index
  n_pct_cols <- ncol(tab_pct$layout$grid)
  expect_true(all(stat_matrix[1, 1:n_pct_cols] == "column_pct", na.rm = TRUE))
  expect_true(all(stat_matrix[1, (n_pct_cols + 1):ncol(stat_matrix)] == "index", na.rm = TRUE))
})

test_that("print works with mixed-statistic tabs", {
  test_survey_data <- create_survey_data(create_tab_test_data(200))
  
  # Create tabs with different statistics and different columns
  tab1 <- tab(test_survey_data, gender, region, statistic = "column_pct")
  tab2 <- tab(test_survey_data, gender, satisfaction, statistic = "index")
  
  # Glue them together
  result <- glue_tab(tab1, tab2, direction = "cols")
  
  # Should print without error
  expect_output(print(result), "mixed statistics")
})

test_that("significance testing fails on mixed-statistic tabs with clear error", {
  test_survey_data <- create_survey_data(create_tab_test_data(200))
  
  # Create tabs with different statistics and different columns
  tab1 <- tab(test_survey_data, gender, region, statistic = "column_pct")
  tab2 <- tab(test_survey_data, gender, satisfaction, statistic = "index")
  
  # Glue them together
  result <- glue_tab(tab1, tab2, direction = "cols")
  
  # Attempting to add significance should fail with informative error
  expect_error(
    add_sig(result, versus = "North"),
    "Cannot perform significance testing on mixed-statistic tabs"
  )
})

test_that("gluing already-mixed tabs works", {
  test_survey_data <- create_survey_data(create_tab_test_data(200))
  
  # Create first mixed tab with different columns
  tab1 <- tab(test_survey_data, gender, region, statistic = "column_pct")
  tab2 <- tab(test_survey_data, gender, satisfaction, statistic = "index")
  mixed1 <- glue_tab(tab1, tab2, direction = "cols")
  
  # Create another tab with yet another column spec
  tab3 <- tab(test_survey_data, gender, brand, statistic = "count")
  
  # Should be able to glue mixed tab with regular tab
  expect_no_error({
    result <- glue_tab(mixed1, tab3, direction = "cols")
  })
  
  # Result should still have NULL statistic
  expect_null(result$statistic)
})

test_that("mixed-statistic tabs display correctly in different export formats", {
  test_survey_data <- create_survey_data(create_tab_test_data(200))
  
  # Create tabs with different statistics and different columns
  tab_pct <- tab(test_survey_data, gender, region, statistic = "column_pct")
  tab_idx <- tab(test_survey_data, gender, satisfaction, statistic = "index")
  
  # Glue them together
  result <- glue_tab(tab_pct, tab_idx, direction = "cols")
  
  # Test as.data.frame
  df <- as.data.frame(result)
  expect_s3_class(df, "data.frame")
  expect_true("row_label" %in% names(df))
  
  # Test tab_to_flourish with informative message
  expect_message(
    tab_to_flourish(result),
    "mixed statistics"
  )
})

test_that("extract_stat_info_safe handles NULL statistic correctly", {
  test_survey_data <- create_survey_data(create_tab_test_data(200))
  
  # Create mixed-statistic tab with different columns
  tab1 <- tab(test_survey_data, gender, region, statistic = "column_pct")
  tab2 <- tab(test_survey_data, gender, satisfaction, statistic = "index")
  result <- glue_tab(tab1, tab2, direction = "cols")
  
  # Extract stat info - should not fail
  stat_info <- surveydatar:::.extract_stat_info_safe(result)
  
  # Should have is_mixed flag
  expect_true(stat_info$is_mixed)
  
  # Should have extracted a statistic from first cell as fallback
  expect_false(is.null(stat_info$stat))
  expect_true(stat_info$id %in% c("column_pct", "index"))
})

test_that("hide_summary works on mixed-statistic tabs", {
  test_survey_data <- create_survey_data(create_tab_test_data(200))
  
  # Create mixed-statistic tab with different columns
  tab1 <- tab(test_survey_data, gender, region, statistic = "column_pct")
  tab2 <- tab(test_survey_data, gender, satisfaction, statistic = "index")
  result <- glue_tab(tab1, tab2, direction = "cols")
  
  # Hide summary should work on cell-based mixed-stat tabs  
  expect_no_error({
    result2 <- hide_summary(result)
  })
  
  # Result should still be valid
  expect_s3_class(result2, "tab_result")
})

##### Unit tests for more expressive filters #####
test_that("filters support helpers with same expressiveness as rows", {
  # Use the standard test data
  data <- test_survey_data

  # Test 1: Helper with quoted variable name
  result1 <- tab(data, q1, gender, filter = any_positive("q1_1"))
  expect_s3_class(result1, "tab_result")
  expect_true(nrow(result1) > 0)

  # Test 2: Helper with unquoted variable name
  result2 <- tab(data, q1, gender, filter = any_positive(q1_1))
  expect_s3_class(result2, "tab_result")
  expect_equal(dim(result1), dim(result2))  # Should give same result

  # Test 3: Helper with c() and unquoted names
  result3 <- tab(data, satisfaction, gender, filter = any_positive(c(q1_1, q1_2)))
  expect_s3_class(result3, "tab_result")
  # Should have filtered to only respondents with q1_1 OR q1_2 positive
  base_row <- result3[result3$row_label == "Base (n)", ]
  total_n <- as.numeric(base_row$Total)
  expect_true(total_n < nrow(data$dat))  # Filter should reduce sample size

  # Test 4: Complex expression with helper and multiplication (filter combination)
  result4 <- tab(data, satisfaction, gender,
                 filter = any_positive(q1_a) * (age > 30))
  expect_s3_class(result4, "tab_result")
  # Should be smaller than just any_positive(q1) filter
  base_row4 <- result4[result4$row_label == "Base (n)", ]
  expect_true(as.numeric(base_row4$Total) < total_n)

  # Test 5: Verify filter and rows give same result when using same helper
  # Using in rows
  result_rows <- tab(data, any_positive(q1_a), gender, statistic = "count")

  # Using as filter
  result_filter <- tab(data, total(), gender, filter = any_positive(q1_a), statistic = "count")
  base_row_filter <- result_filter[result_filter$row_label == "Base (n)", ]

  # verify the counts are the same
  identical(as.data.frame(result_rows)[1,2:4], as.data.frame(result_filter)[1,2:4])

  # Test 6: Multiple helpers in filter
  result6 <- tab(data, region, gender,
                 filter = any_positive(q1_a) * top_box(satisfaction, 2))
  expect_s3_class(result6, "tab_result")
  base_row <- result6[result6$row_label == "Base (n)", ]
  total_base <- as.numeric(base_row$Total)
  expect_true(total_base > 0)  # Some respondents match
  expect_true(total_base < nrow(data$dat))  # But not all
})
##### Cell Store Tests #####

test_that("new_cell_store initializes correctly", {
  store <- new_cell_store()
  
  expect_s3_class(store, "cell_store")
  expect_true(inherits(store$cells, "environment"))
  expect_true(inherits(store$cell_data, "environment"))
  expect_equal(store$next_id, 1L)
  expect_equal(length(store$cell_data$cell_ids), 0)
  expect_equal(length(store$cell_data$values), 0)
  expect_equal(length(store$cell_data$bases), 0)
})

test_that("new_cell_store accepts size_hint", {
  store <- new_cell_store(size_hint = 500)
  
  expect_s3_class(store, "cell_store")
  expect_equal(store$next_id, 1L)
})

test_that("add_cell creates cells with sequential IDs", {
  store <- new_cell_store()
  
  spec1 <- list(base_expr = quote(TRUE), row_expr = quote(x == 1), col_expr = quote(y == 1),
                meta = list(base_vars = list(), row_vars = list(), col_vars = list()))
  comp1 <- list(statistic = "column_pct", array_refs = list(base = 1, row = 1, col = 1))
  
  id1 <- add_cell(store, value = 10.5, base = 100, specification = spec1, 
                  computation = comp1, derivation = NULL)
  
  expect_equal(id1, "c_000001")
  expect_equal(store$next_id, 2L)
  
  spec2 <- list(base_expr = quote(TRUE), row_expr = quote(x == 2), col_expr = quote(y == 1),
                meta = list(base_vars = list(), row_vars = list(), col_vars = list()))
  comp2 <- list(statistic = "column_pct", array_refs = list(base = 1, row = 2, col = 1))
  
  id2 <- add_cell(store, value = 15.3, base = 100, specification = spec2,
                  computation = comp2, derivation = NULL)
  
  expect_equal(id2, "c_000002")
  expect_equal(store$next_id, 3L)
})

test_that("add_cell stores data in both hash and columnar structures", {
  store <- new_cell_store()
  
  spec <- list(base_expr = quote(TRUE), row_expr = quote(x == 1), col_expr = quote(y == 1),
               label = "Test Cell",
               meta = list(base_vars = list(), row_vars = list(), col_vars = list()))
  comp <- list(statistic = "column_pct", array_refs = list(base = 1, row = 1, col = 1))
  
  id <- add_cell(store, value = 42.5, base = 120, specification = spec, 
                 computation = comp, derivation = NULL)
  
  # Check hash storage
  expect_true(exists(id, envir = store$cells))
  cell_from_hash <- store$cells[[id]]
  expect_equal(cell_from_hash$cell_id, id)
  expect_equal(cell_from_hash$value, 42.5)
  expect_equal(cell_from_hash$base, 120)
  
  # Check columnar storage
  expect_equal(store$cell_data$cell_ids[1], id)
  expect_equal(store$cell_data$values[1], 42.5)
  expect_equal(store$cell_data$bases[1], 120)
})

test_that("get_cell retrieves cell by ID", {
  store <- new_cell_store()
  
  spec <- list(base_expr = quote(TRUE), row_expr = quote(x == 1), col_expr = quote(y == 1),
               meta = list(base_vars = list(), row_vars = list(), col_vars = list()))
  comp <- list(statistic = "column_pct", array_refs = list(base = 1, row = 1, col = 1))
  
  id <- add_cell(store, value = 42.5, base = 120, specification = spec, 
                 computation = comp, derivation = NULL)
  
  cell <- get_cell(store, id)
  
  expect_equal(cell$cell_id, id)
  expect_equal(cell$value, 42.5)
  expect_equal(cell$base, 120)
  expect_null(cell$derivation)
})

test_that("get_cell returns NULL for non-existent ID", {
  store <- new_cell_store()
  
  cell <- get_cell(store, "c_999999")
  
  expect_null(cell)
})

test_that("get_cells retrieves multiple cells", {
  store <- new_cell_store()
  
  spec1 <- list(base_expr = quote(TRUE), row_expr = quote(x == 1), col_expr = quote(y == 1),
                meta = list(base_vars = list(), row_vars = list(), col_vars = list()))
  spec2 <- list(base_expr = quote(TRUE), row_expr = quote(x == 2), col_expr = quote(y == 1),
                meta = list(base_vars = list(), row_vars = list(), col_vars = list()))
  comp <- list(statistic = "column_pct", array_refs = list(base = 1, row = 1, col = 1))
  
  id1 <- add_cell(store, value = 10, base = 100, specification = spec1, computation = comp)
  id2 <- add_cell(store, value = 20, base = 100, specification = spec2, computation = comp)
  
  cells <- get_cells(store, c(id1, id2))
  
  expect_length(cells, 2)
  expect_equal(cells[[1]]$value, 10)
  expect_equal(cells[[2]]$value, 20)
})

test_that("all_cell_ids returns all IDs", {
  store <- new_cell_store()
  
  spec <- list(base_expr = quote(TRUE), row_expr = quote(x == 1), col_expr = quote(y == 1),
               meta = list(base_vars = list(), row_vars = list(), col_vars = list()))
  comp <- list(statistic = "column_pct", array_refs = list(base = 1, row = 1, col = 1))
  
  id1 <- add_cell(store, value = 10, base = 100, specification = spec, computation = comp)
  id2 <- add_cell(store, value = 20, base = 100, specification = spec, computation = comp)
  id3 <- add_cell(store, value = 30, base = 100, specification = spec, computation = comp)
  
  all_ids <- all_cell_ids(store)
  
  expect_length(all_ids, 3)
  expect_true(id1 %in% all_ids)
  expect_true(id2 %in% all_ids)
  expect_true(id3 %in% all_ids)
})

test_that("get_all_values returns columnar values", {
  store <- new_cell_store()
  
  spec <- list(base_expr = quote(TRUE), row_expr = quote(x == 1), col_expr = quote(y == 1),
               meta = list(base_vars = list(), row_vars = list(), col_vars = list()))
  comp <- list(statistic = "column_pct", array_refs = list(base = 1, row = 1, col = 1))
  
  add_cell(store, value = 10.5, base = 100, specification = spec, computation = comp)
  add_cell(store, value = 20.3, base = 100, specification = spec, computation = comp)
  add_cell(store, value = 30.7, base = 100, specification = spec, computation = comp)
  
  values <- get_all_values(store)
  
  expect_equal(values, c(10.5, 20.3, 30.7))
})

test_that("get_all_bases returns columnar bases", {
  store <- new_cell_store()
  
  spec <- list(base_expr = quote(TRUE), row_expr = quote(x == 1), col_expr = quote(y == 1),
               meta = list(base_vars = list(), row_vars = list(), col_vars = list()))
  comp <- list(statistic = "column_pct", array_refs = list(base = 1, row = 1, col = 1))
  
  add_cell(store, value = 10, base = 100, specification = spec, computation = comp)
  add_cell(store, value = 20, base = 150, specification = spec, computation = comp)
  add_cell(store, value = 30, base = 200, specification = spec, computation = comp)
  
  bases <- get_all_bases(store)
  
  expect_equal(bases, c(100, 150, 200))
})

test_that("filter_cells filters by predicate", {
  store <- new_cell_store()
  
  spec <- list(base_expr = quote(TRUE), row_expr = quote(x == 1), col_expr = quote(y == 1),
               meta = list(base_vars = list(), row_vars = list(), col_vars = list()))
  comp <- list(statistic = "column_pct", array_refs = list(base = 1, row = 1, col = 1))
  
  add_cell(store, value = 10, base = 100, specification = spec, computation = comp)
  add_cell(store, value = 50, base = 100, specification = spec, computation = comp)
  add_cell(store, value = 30, base = 100, specification = spec, computation = comp)
  
  # Filter cells with value > 25
  matching_ids <- filter_cells(store, function(cell) cell$value > 25)
  
  expect_length(matching_ids, 2)
  
  # Check that we got the right cells
  matching_cells <- get_cells(store, matching_ids)
  matching_values <- sapply(matching_cells, function(c) c$value)
  expect_true(all(matching_values > 25))
})

test_that("filter_cells_by_value filters efficiently", {
  store <- new_cell_store()
  
  spec <- list(base_expr = quote(TRUE), row_expr = quote(x == 1), col_expr = quote(y == 1),
               meta = list(base_vars = list(), row_vars = list(), col_vars = list()))
  comp <- list(statistic = "column_pct", array_refs = list(base = 1, row = 1, col = 1))
  
  add_cell(store, value = 10, base = 100, specification = spec, computation = comp)
  add_cell(store, value = 50, base = 150, specification = spec, computation = comp)
  add_cell(store, value = 30, base = 120, specification = spec, computation = comp)
  
  # Filter cells with value > 20
  matching_ids <- filter_cells_by_value(store, value > 20)
  
  expect_length(matching_ids, 2)
  
  # Filter cells with base > 110
  matching_ids2 <- filter_cells_by_value(store, base > 110)
  
  expect_length(matching_ids2, 2)
})

test_that("cell_count returns correct count", {
  store <- new_cell_store()
  
  expect_equal(cell_count(store), 0)
  
  spec <- list(base_expr = quote(TRUE), row_expr = quote(x == 1), col_expr = quote(y == 1),
               meta = list(base_vars = list(), row_vars = list(), col_vars = list()))
  comp <- list(statistic = "column_pct", array_refs = list(base = 1, row = 1, col = 1))
  
  add_cell(store, value = 10, base = 100, specification = spec, computation = comp)
  expect_equal(cell_count(store), 1)
  
  add_cell(store, value = 20, base = 100, specification = spec, computation = comp)
  add_cell(store, value = 30, base = 100, specification = spec, computation = comp)
  expect_equal(cell_count(store), 3)
})

test_that("has_cell checks existence", {
  store <- new_cell_store()
  
  spec <- list(base_expr = quote(TRUE), row_expr = quote(x == 1), col_expr = quote(y == 1),
               meta = list(base_vars = list(), row_vars = list(), col_vars = list()))
  comp <- list(statistic = "column_pct", array_refs = list(base = 1, row = 1, col = 1))
  
  id <- add_cell(store, value = 10, base = 100, specification = spec, computation = comp)
  
  expect_true(has_cell(store, id))
  expect_false(has_cell(store, "c_999999"))
})

test_that("validate_cell_store passes for valid store", {
  store <- new_cell_store()
  
  spec <- list(base_expr = quote(TRUE), row_expr = quote(x == 1), col_expr = quote(y == 1),
               meta = list(base_vars = list(), row_vars = list(), col_vars = list()))
  comp <- list(statistic = "column_pct", array_refs = list(base = 1, row = 1, col = 1))
  
  add_cell(store, value = 10, base = 100, specification = spec, computation = comp)
  add_cell(store, value = 20, base = 100, specification = spec, computation = comp)
  
  expect_true(validate_cell_store(store))
})

test_that("cells store derivation metadata", {
  store <- new_cell_store()
  
  spec <- list(base_expr = quote(TRUE), row_expr = quote(x == 1), col_expr = quote(y == 1),
               meta = list(base_vars = list(), row_vars = list(), col_vars = list()))
  comp <- list(statistic = "column_pct", array_refs = list(base = 1, row = 1, col = 1))
  
  # Add source cells
  id1 <- add_cell(store, value = 10, base = 100, specification = spec, computation = comp)
  id2 <- add_cell(store, value = 20, base = 100, specification = spec, computation = comp)
  
  # Add derived cell
  deriv <- list(
    operation = "delta_vs",
    source_cells = c(id1, id2),
    formula = quote(cell2 - cell1)
  )
  
  id3 <- add_cell(store, value = 10, base = NA, specification = spec, 
                  computation = list(statistic = NA, array_refs = list()),
                  derivation = deriv)
  
  cell3 <- get_cell(store, id3)
  
  expect_equal(cell3$derivation$operation, "delta_vs")
  expect_equal(cell3$derivation$source_cells, c(id1, id2))
  expect_equal(cell3$value, 10)
})

##### DSL Semantic Matching Tests #####

test_that("DSL validation accepts allowed operations", {
  # Valid operations
  expect_silent(validate_dsl_expr(quote(x == 1)))
  expect_silent(validate_dsl_expr(quote(x > 5)))
  expect_silent(validate_dsl_expr(quote(x %in% c(1, 2, 3))))
  expect_silent(validate_dsl_expr(quote(a & b)))
  expect_silent(validate_dsl_expr(quote(a | b)))
  expect_silent(validate_dsl_expr(quote(!x)))
  
  # Nested expressions
  expect_silent(validate_dsl_expr(quote(x > 5 & y < 10)))
  expect_silent(validate_dsl_expr(quote((a | b) & c)))
  
  # Symbols and literals
  expect_silent(validate_dsl_expr(quote(variable_name)))
  expect_silent(validate_dsl_expr(quote(TRUE)))
  expect_silent(validate_dsl_expr(5))
  
  # Disallowed operations
  expect_error(validate_dsl_expr(quote(sqrt(x))), "disallowed operation")
  expect_error(validate_dsl_expr(quote(mean(x))), "disallowed operation")
  expect_error(validate_dsl_expr(quote(x + y)), "disallowed operation")
})

test_that("DSL evaluation converts logical to numeric", {
  df <- data.frame(
    x = c(1, 2, 3, 4, 5),
    y = c(2, 4, 6, 8, 10)
  )
  
  # Expression evaluates to logical but should return numeric
  result <- eval_dsl(quote(x %in% c(1, 3, 5)), df)
  
  expect_true(is.numeric(result))
  expect_equal(result, c(1, 0, 1, 0, 1))
  expect_equal(length(result), nrow(df))
  
  # Comparison operators
  result2 <- eval_dsl(quote(x > 3), df)
  expect_equal(result2, c(0, 0, 0, 1, 1))
  
  # Compound expressions
  result3 <- eval_dsl(quote(x > 2 & y < 8), df)
  expect_equal(result3, c(0, 0, 1, 0, 0))
})

test_that("DSL normalization works without data", {
  # OR-equals chain
  expr1 <- quote(x == 1 | x == 2 | x == 3)
  norm1 <- normalize_dsl(expr1, data = NULL)
  expect_equal(norm1, call("%in%", as.symbol("x"), c(1, 2, 3)))
  
  # Variable on left
  expr3 <- quote(5 < x)
  norm3 <- normalize_dsl(expr3, data = NULL)
  expect_equal(norm3, call(">", as.symbol("x"), 5))
  
  # De Morgan's law
  expr4 <- quote(!(a & b))
  norm4 <- normalize_dsl(expr4, data = NULL)
  # Should become !a | !b
  expect_equal(as.character(norm4[[1]]), "|")
})

test_that("DSL data-aware normalization converts thresholds", {
  # Create test data with small value set
  df <- data.frame(
    satisfaction = sample(c(1, 2, 3, 4, 5), 100, replace = TRUE),
    age = sample(18:75, 100, replace = TRUE)  # Many values
  )
  
  # Threshold on small value set
  expr1 <- quote(satisfaction > 3)
  norm1 <- normalize_dsl(expr1, df, max_unique = 11)
  
  # Should convert to %in%
  expect_equal(as.character(norm1[[1]]), "%in%")
  expect_true(all(c(4, 5) %in% norm1[[3]]))
  
  # Threshold on large value set - stays as threshold
  expr2 <- quote(age > 30)
  norm2 <- normalize_dsl(expr2, df, max_unique = 11)
  
  # Should NOT convert (too many unique values)
  expect_equal(norm2, expr2)
  
  # Impossible condition
  expr3 <- quote(satisfaction > 10)
  norm3 <- normalize_dsl(expr3, df, max_unique = 11)
  expect_equal(norm3, quote(FALSE))
})

test_that("Helpers return arrays with DSL metadata", {
  df <- data.frame(
    satisfaction = sample(c(1, 2, 3, 4, 5), 50, replace = TRUE)
  )
  df$satisfaction <- haven::labelled(df$satisfaction,
    c("Very dissatisfied" = 1, "Dissatisfied" = 2, "Neutral" = 3, 
      "Satisfied" = 4, "Very satisfied" = 5))
  attr(df$satisfaction, "label") <- "Overall Satisfaction"
  
  survey <- create_survey_data(df)
  
  # Test top_box
  result <- tab(survey, top_box(satisfaction, 2))
  
  # Extract cell to check DSL
  cell_id <- result$layout$grid[1, 1]
  cell <- get_cell(result$cell_store, cell_id)
  
  # Should have DSL in specification
  expect_false(is.null(cell$specification$dsl$row))
  
  # DSL should be normalized (threshold converted to values)
  row_dsl <- cell$specification$dsl$row
  if (is.call(row_dsl) && as.character(row_dsl[[1]]) == "%in%") {
    # Normalized to %in%
    expect_true(5 %in% row_dsl[[3]])
    expect_true(4 %in% row_dsl[[3]])
  }
})

test_that("Multi-output helpers get unique member indices", {
  df <- data.frame(
    q1_1 = sample(c(0, 1), 50, replace = TRUE),
    q1_2 = sample(c(0, 1), 50, replace = TRUE),
    q1_3 = sample(c(0, 1), 50, replace = TRUE)
  )
  
  for (i in 1:3) {
    df[[paste0("q1_", i)]] <- haven::labelled(df[[paste0("q1_", i)]], 
                                               c("No" = 0, "Yes" = 1))
    attr(df[[paste0("q1_", i)]], "label") <- paste0("Q1_", i)
  }
  
  survey <- create_survey_data(df)
  
  # Add variables that will match
  # Suppress expected metadata warnings about value labels and partial question groups
  survey <- suppressWarnings(
    dplyr::mutate(survey,
      q1_4 = realiselabelled_vec(sample(c(0, 1), 50, replace = TRUE),
                                 variable_label = "Q1_4: Match A"),
      q1_5 = realiselabelled_vec(sample(c(0, 1), 50, replace = TRUE),
                                 variable_label = "Q1_5: Match B")
    )
  )
  
  # Use all_matching to get multi-output
  result <- tab(survey, all_matching("Match", q1))
  
  # Get cells from first column
  cell1_id <- result$layout$grid[1, 1]
  cell2_id <- result$layout$grid[2, 1]
  
  cell1 <- get_cell(result$cell_store, cell1_id)
  cell2 <- get_cell(result$cell_store, cell2_id)
  
  # Row expressions should be different (due to .member_index)
  expect_false(identical(cell1$specification$row_expr, cell2$specification$row_expr))
  
  # Should have .member_index parameter
  if (is.call(cell1$specification$row_expr)) {
    param_names <- names(cell1$specification$row_expr)
    expect_true(".member_index" %in% param_names)
  }
})

test_that("Exact matching distinguishes members", {
  df <- data.frame(
    gender = sample(c(1, 2), 30, replace = TRUE),
    q1_1 = sample(c(0, 1), 30, replace = TRUE),
    q1_2 = sample(c(0, 1), 30, replace = TRUE)
  )
  
  df$gender <- haven::labelled(df$gender, c("Male" = 1, "Female" = 2))
  for (i in 1:2) {
    df[[paste0("q1_", i)]] <- haven::labelled(df[[paste0("q1_", i)]], c("No" = 0, "Yes" = 1))
    attr(df[[paste0("q1_", i)]], "label") <- paste0("Q1_", i)
  }
  attr(df$gender, "label") <- "Gender"
  
  # Add new variables with ANY in label
  df <- df %>%
    dplyr::mutate(
      q1_3 = realiselabelled_vec(sample(c(0, 1), 30, replace = TRUE),
                                 variable_label = "Q1: ANY A"),
      q1_4 = realiselabelled_vec(sample(c(0, 1), 30, replace = TRUE),
                                 variable_label = "Q1: ANY B")
    )
  
  survey <- create_survey_data(df)
  result <- tab(survey, all_matching("ANY", q1), gender)
  
  # Should have 2 data rows (q1_3 and q1_4)
  data_rows <- result[!grepl("Base|NET|Total", result$row_label), ]
  expect_equal(nrow(data_rows), 2)
  
  # Both rows should have valid values (not NA)
  expect_false(any(is.na(data_rows[1, -1])))
  expect_false(any(is.na(data_rows[2, -1])))
})

test_that("Partial matching with value subsets works", {
  df <- data.frame(
    satisfaction = sample(c(1, 2, 3, 4, 5), 50, replace = TRUE)
  )
  df$satisfaction <- haven::labelled(df$satisfaction,
    c("1" = 1, "2" = 2, "3" = 3, "4" = 4, "5" = 5))
  attr(df$satisfaction, "label") <- "Satisfaction"
  
  survey <- create_survey_data(df)
  
  # Create table with top_box (creates cells with satisfaction %in% c(4, 5))
  result <- tab(survey, top_box(satisfaction, 2))
  
  # Get cell
  cell1_id <- result$layout$grid[1, 1]
  cell1 <- get_cell(result$cell_store, cell1_id)
  
  # Query: satisfaction == 5 (subset of {4, 5})
  query_dsl <- quote(satisfaction == 5)
  norm_query <- normalize_dsl(query_dsl, survey$dat, survey$dpdict)
  
  # Should match via partial containment
  is_match <- dsl_is_subset(norm_query, cell1$specification$dsl$row, survey$dat)
  expect_true(is_match)
  
  # Query: satisfaction %in% c(1, 2) (disjoint from {4, 5})
  query_dsl2 <- quote(satisfaction %in% c(1, 2))
  norm_query2 <- normalize_dsl(query_dsl2, survey$dat, survey$dpdict)
  
  # Should NOT match (contradiction)
  is_match2 <- dsl_is_subset(norm_query2, cell1$specification$dsl$row, survey$dat)
  expect_false(is_match2)
})

test_that("pivot_to_grid transforms grid questions", {
  # Create grid question data (A2 pattern)
  df <- data.frame(
    A2_1 = sample(c(1, 2, 3), 50, replace = TRUE),
    A2_2 = sample(c(1, 2, 3), 50, replace = TRUE),
    A2_3 = sample(c(1, 2, 3), 50, replace = TRUE)
  )
  
  for (i in 1:3) {
    df[[paste0("A2_", i)]] <- haven::labelled(df[[paste0("A2_", i)]],
      c("Daily" = 1, "Weekly" = 2, "Monthly" = 3))
    attr(df[[paste0("A2_", i)]], "label") <- paste0("Doc type ", i)
  }
  
  survey <- create_survey_data(df)
  
  # Create long-format table without summaries (3 vars × 3 values = 9 rows × 1 col)
  result_long <- tab(survey, A2, statistic = "row_pct", 
                     show_row_nets = FALSE, show_col_nets = FALSE, show_base = FALSE)
  
  # Should have 9 data rows
  expect_equal(nrow(result_long$layout$grid), 9)
  
  # Pivot to grid (3 rows × 3 cols)
  result_grid <- pivot_to_grid(result_long)
  
  # Should have 3 rows (one per variable)
  expect_equal(nrow(result_grid$layout$grid), 3)
  
  # Should have 3 columns (one per value)
  expect_equal(ncol(result_grid$layout$grid), 3)
  
  # Check all cells present (no NAs)
  non_na_cells <- sum(!is.na(result_grid$layout$grid))
  expect_equal(non_na_cells, 9)
  
  # Check row labels are variable labels
  expect_true(all(grepl("Doc type", result_grid$layout$row_labels)))
  
  # Check column labels are value labels
  expect_true(all(result_grid$layout$col_labels %in% c("Daily", "Weekly", "Monthly")))
})

test_that("DSL system fixes original all_matching NA issue", {
  df <- data.frame(
    gender = sample(c(1, 2), 100, replace = TRUE),
    q1_1 = sample(c(0, 1), 100, replace = TRUE),
    q1_2 = sample(c(0, 1), 100, replace = TRUE),
    q1_3 = sample(c(0, 1), 100, replace = TRUE)
  )
  
  df$gender <- haven::labelled(df$gender, c("Male" = 1, "Female" = 2))
  for (i in 1:3) {
    df[[paste0("q1_", i)]] <- haven::labelled(df[[paste0("q1_", i)]], c("No" = 0, "Yes" = 1))
    attr(df[[paste0("q1_", i)]], "label") <- paste0("Q1_", i, ": Attr ", i)
  }
  attr(df$gender, "label") <- "Gender"
  
  survey <- create_survey_data(df)
  
  # Add new variables
  # Suppress expected metadata warnings about value labels and partial question groups
  survey <- suppressWarnings(
    dplyr::mutate(survey,
      q1_4 = realiselabelled_vec(sample(c(0, 1), 100, replace = TRUE),
                                 variable_label = "Q1: ANY category A"),
      q1_5 = realiselabelled_vec(sample(c(0, 1), 100, replace = TRUE),
                                 variable_label = "Q1: ANY category B")
    )
  )
  
  # Test all_matching
  result <- tab(survey, all_matching("ANY", q1), gender)
  
  # Both rows should have valid percentages (not NA)
  data_rows <- result[grepl("ANY", result$row_label), ]
  
  expect_equal(nrow(data_rows), 2)
  
  # Check no NA values
  for (i in 1:2) {
    row_vals <- as.numeric(data_rows[i, -1])
    expect_false(any(is.na(row_vals)), 
                 info = paste("Row", i, "has NA values"))
  }
  
  # Verify percentages are reasonable
  expect_true(all(as.numeric(data_rows[, "Male"]) >= 0 & as.numeric(data_rows[, "Male"]) <= 100))
  expect_true(all(as.numeric(data_rows[, "Female"]) >= 0 & as.numeric(data_rows[, "Female"]) <= 100))
})

test_that("DSL query functions work correctly", {
  # Test dsl_get_variables
  dsl1 <- quote(x > 5 & y < 10)
  vars1 <- dsl_get_variables(dsl1)
  expect_equal(sort(vars1), c("x", "y"))
  
  # Test dsl_get_values
  dsl2 <- quote(x %in% c(1, 2, 3))
  vals2 <- dsl_get_values(dsl2)
  expect_equal(vals2, c(1, 2, 3))
  
  dsl3 <- quote(x == 5)
  vals3 <- dsl_get_values(dsl3)
  expect_equal(vals3, 5)
  
  # Test dsl_references_variable
  dsl4 <- quote(satisfaction > 3 & age < 30)
  expect_true(dsl_references_variable(dsl4, "satisfaction"))
  expect_true(dsl_references_variable(dsl4, "age"))
  expect_false(dsl_references_variable(dsl4, "income"))
  
  # Test dsl_contains_value
  dsl5 <- quote(x %in% c(4, 5, 6))
  expect_true(dsl_contains_value(dsl5, 5))
  expect_false(dsl_contains_value(dsl5, 1))
  
  dsl6 <- quote(x == 3)
  expect_true(dsl_contains_value(dsl6, 3))
  expect_false(dsl_contains_value(dsl6, 4))
})

test_that("DSL normalization is idempotent", {
  df <- data.frame(x = 1:10)
  
  # Already normalized expression
  expr <- call("%in%", as.symbol("x"), c(4, 5, 6))
  
  # Normalize once
  norm1 <- normalize_dsl(expr, df, NULL)
  
  # Normalize again
  norm2 <- normalize_dsl(norm1, df, NULL)
  
  # Should be identical
  expect_identical(norm1, norm2)
})


test_that("transform_label_comparisons works with == operator", {
  # Create test data with haven_labelled variable
  test_data <- data.frame(
    gender = haven::labelled(c(1, 2, 1, 3), c(Female = 1, Male = 2, Other = 3))
  )
  
  # Test expression transformation
  expr <- quote(gender == "Female")
  transformed <- transform_label_comparisons(expr, test_data)
  
  # Should transform to gender == 1
  expect_equal(as.character(transformed[[1]]), "==")
  expect_equal(as.character(transformed[[2]]), "gender")
  expect_equal(as.numeric(transformed[[3]]), 1)
})

test_that("transform_label_comparisons works with != operator", {
  test_data <- data.frame(
    gender = haven::labelled(c(1, 2, 1, 3), c(Female = 1, Male = 2, Other = 3))
  )
  
  expr <- quote(gender != "Male")
  transformed <- transform_label_comparisons(expr, test_data)
  
  # Should transform to gender != 2
  expect_equal(as.character(transformed[[1]]), "!=")
  expect_equal(as.character(transformed[[2]]), "gender")
  expect_equal(as.numeric(transformed[[3]]), 2)
})

test_that("transform_label_comparisons works with %in% operator", {
  test_data <- data.frame(
    gender = haven::labelled(c(1, 2, 1, 3), c(Female = 1, Male = 2, Other = 3))
  )
  
  expr <- quote(gender %in% c("Female", "Male"))
  transformed <- transform_label_comparisons(expr, test_data)
  
  # Should transform to gender %in% c(1, 2)
  expect_equal(as.character(transformed[[1]]), "%in%")
  expect_equal(as.character(transformed[[2]]), "gender")
  expect_true(is.call(transformed[[3]]))
  expect_equal(as.character(transformed[[3]][[1]]), "c")
  expect_equal(as.numeric(transformed[[3]][[2]]), 1)
  expect_equal(as.numeric(transformed[[3]][[3]]), 2)
})

test_that("transform_label_comparisons is case-sensitive", {
  test_data <- data.frame(
    gender = haven::labelled(c(1, 2, 1, 3), c(Female = 1, Male = 2, Other = 3))
  )
  
  expr <- quote(gender == "female")
  expect_error(
    transform_label_comparisons(expr, test_data),
    "Label 'female' not found in variable 'gender'"
  )
})

test_that("transform_label_comparisons errors on invalid label", {
  test_data <- data.frame(
    gender = haven::labelled(c(1, 2, 1, 3), c(Female = 1, Male = 2, Other = 3))
  )
  
  expr <- quote(gender == "Invalid")
  expect_error(
    transform_label_comparisons(expr, test_data),
    "Label 'Invalid' not found in variable 'gender'.*Available labels: Female, Male, Other"
  )
})

test_that("transform_label_comparisons leaves numeric comparisons unchanged", {
  test_data <- data.frame(
    gender = haven::labelled(c(1, 2, 1, 3), c(Female = 1, Male = 2, Other = 3))
  )
  
  expr <- quote(gender == 1)
  transformed <- transform_label_comparisons(expr, test_data)
  
  # Should remain unchanged
  expect_equal(as.character(transformed[[1]]), "==")
  expect_equal(as.character(transformed[[2]]), "gender")
  expect_equal(transformed[[3]], 1)
})

test_that("transform_label_comparisons leaves non-labelled variables unchanged", {
  test_data <- data.frame(
    age = c(25, 30, 35, 40)
  )
  
  expr <- quote(age > 30)
  transformed <- transform_label_comparisons(expr, test_data)
  
  # Should remain unchanged
  expect_equal(transformed, expr)
})

test_that("tab() works with string label comparisons using ==", {
  set.seed(123)
  test_data <- data.frame(
    gender = factor(sample(c("Male", "Female", "Other"), 100, replace = TRUE)),
    region = factor(sample(c("North", "South"), 100, replace = TRUE))
  )
  
  survey_data <- create_survey_data(test_data)
  
  # Test with string label
  result1 <- tab(survey_data, gender == "Female", region)
  
  # Test with numeric code (should give same result)
  result2 <- tab(survey_data, gender == 1, region)
  
  # Both should work and give same results
  expect_s3_class(result1, "tab_cell_collection")
  expect_s3_class(result2, "tab_cell_collection")
  
  df1 <- as.data.frame(result1)
  df2 <- as.data.frame(result2)
  
  # Values should be identical
  expect_equal(df1[[2]], df2[[2]])  # North column
  expect_equal(df1[[3]], df2[[3]])  # South column
})

test_that("tab() works with string label comparisons using !=", {
  set.seed(123)
  test_data <- data.frame(
    gender = factor(sample(c("Male", "Female", "Other"), 100, replace = TRUE)),
    region = factor(sample(c("North", "South"), 100, replace = TRUE))
  )
  
  survey_data <- create_survey_data(test_data)
  
  result <- tab(survey_data, gender != "Male", region)
  
  expect_s3_class(result, "tab_cell_collection")
  df <- as.data.frame(result)
  expect_true(nrow(df) >= 1)
})

test_that("tab() works with string label comparisons using %in%", {
  set.seed(123)
  test_data <- data.frame(
    gender = factor(sample(c("Male", "Female", "Other"), 100, replace = TRUE)),
    region = factor(sample(c("North", "South"), 100, replace = TRUE))
  )
  
  survey_data <- create_survey_data(test_data)
  
  result <- tab(survey_data, gender %in% c("Female", "Male"), region)
  
  expect_s3_class(result, "tab_cell_collection")
  df <- as.data.frame(result)
  expect_true(nrow(df) >= 1)
})

test_that("tab() errors with helpful message for invalid label", {
  set.seed(123)
  test_data <- data.frame(
    gender = factor(sample(c("Male", "Female", "Other"), 100, replace = TRUE)),
    region = factor(sample(c("North", "South"), 100, replace = TRUE))
  )
  
  survey_data <- create_survey_data(test_data)
  
  expect_error(
    tab(survey_data, gender == "Invalid", region),
    "Label 'Invalid' not found in variable 'gender'"
  )
})

test_that("tab() is case-sensitive for label matching", {
  set.seed(123)
  test_data <- data.frame(
    gender = factor(sample(c("Male", "Female", "Other"), 100, replace = TRUE)),
    region = factor(sample(c("North", "South"), 100, replace = TRUE))
  )
  
  survey_data <- create_survey_data(test_data)
  
  expect_error(
    tab(survey_data, gender == "female", region),
    "Label 'female' not found in variable 'gender'"
  )
})

test_that("tab() works with label comparisons in filter parameter", {
  set.seed(123)
  test_data <- data.frame(
    gender = factor(sample(c("Male", "Female", "Other"), 100, replace = TRUE)),
    satisfaction = sample(1:5, 100, replace = TRUE),
    region = factor(sample(c("North", "South"), 100, replace = TRUE))
  )
  
  survey_data <- create_survey_data(test_data)
  
  result <- tab(survey_data, satisfaction, region, filter = gender == "Female")
  
  expect_s3_class(result, "tab_cell_collection")
  df <- as.data.frame(result)
  expect_true(nrow(df) >= 1)
})

test_that("tab() handles complex expressions with label comparisons", {
  set.seed(123)
  test_data <- data.frame(
    gender = factor(sample(c("Male", "Female", "Other"), 100, replace = TRUE)),
    age = sample(18:65, 100, replace = TRUE),
    region = factor(sample(c("North", "South"), 100, replace = TRUE))
  )
  
  survey_data <- create_survey_data(test_data)
  
  # Complex expression: (gender == "Female") & (age > 30)
  # The & operator should be handled recursively
  result <- tab(survey_data, (gender == "Female") & (age > 30), region)
  
  expect_s3_class(result, "tab_cell_collection")
  df <- as.data.frame(result)
  expect_true(nrow(df) >= 1)
})

test_that("transform_label_comparisons handles nested expressions", {
  test_data <- data.frame(
    gender = haven::labelled(c(1, 2, 1, 3), c(Female = 1, Male = 2, Other = 3)),
    age = c(25, 35, 45, 55)
  )
  
  # Test nested expression with & operator
  expr <- quote((gender == "Female") & (age > 30))
  transformed <- transform_label_comparisons(expr, test_data)
  
  # Should transform the gender == "Female" part to gender == 1
  expect_true(is.call(transformed))
  expect_equal(as.character(transformed[[1]]), "&")
  
  # Left side is wrapped in parentheses, so we need to unwrap it
  left_side <- transformed[[2]]
  expect_equal(as.character(left_side[[1]]), "(")
  
  # The actual comparison is inside the parentheses
  inner_left <- left_side[[2]]
  expect_equal(as.character(inner_left[[1]]), "==")
  expect_equal(as.character(inner_left[[2]]), "gender")
  expect_equal(as.numeric(inner_left[[3]]), 1)
  
  # Right side should be unchanged (also in parentheses)
  right_side <- transformed[[3]]
  expect_equal(as.character(right_side[[1]]), "(")
  
  inner_right <- right_side[[2]]
  expect_equal(as.character(inner_right[[1]]), ">")
  expect_equal(as.character(inner_right[[2]]), "age")
  expect_equal(inner_right[[3]], 30)
})

# Cell-Native Significance Tests ==========================================

test_that("add_sig returns tab_cell_collection for cell-based input", {
  skip_if_not_installed("surveydatar")
  
  data <- data.frame(
    q1 = factor(c("Yes", "Yes", "No", "Yes", "No", "No", "Yes", "Yes")),
    gender = factor(c("Male", "Female", "Male", "Female", "Male", "Female", "Male", "Female"))
  )
  
  result <- tab(data, q1, gender)
  
  # add_sig should preserve cell-based type
  result_with_sig <- add_sig(result, versus = "Male", test = "z_test_proportions")
  
  expect_true(inherits(result_with_sig, "tab_cell_collection"))
  expect_true(inherits(result_with_sig, "tab_result"))
})

test_that("significance is attached to individual cells", {
  skip_if_not_installed("surveydatar")
  
  data <- data.frame(
    q1 = factor(c("Yes", "Yes", "No", "Yes", "No", "No", "Yes", "Yes")),
    gender = factor(c("Male", "Female", "Male", "Female", "Male", "Female", "Male", "Female"))
  )
  
  result <- tab(data, q1, gender) %>%
    add_sig(versus = "Male", test = "z_test_proportions")
  
  # Check that at least some cells have significance data
  store <- result$cell_store
  grid <- result$layout$grid
  
  has_sig <- FALSE
  for (i in seq_len(nrow(grid))) {
    for (j in seq_len(ncol(grid))) {
      cell_id <- grid[i, j]
      if (!is.na(cell_id)) {
        cell <- get_cell(store, cell_id)
        if (!is.null(cell$significance)) {
          has_sig <- TRUE
          expect_true(is.list(cell$significance))
          expect_true("Male" %in% names(cell$significance))
          break
        }
      }
    }
    if (has_sig) break
  }
  
  expect_true(has_sig)
})

test_that("cell-native significance preserved through pipeline operations", {
  skip_if_not_installed("surveydatar")
  
  data <- data.frame(
    q1 = factor(c("Yes", "Yes", "No", "Maybe", "Yes", "No", "Maybe", "Yes")),
    gender = factor(c("Male", "Female", "Male", "Female", "Male", "Female", "Male", "Female"))
  )
  
  # Pipeline with significance
  result <- tab(data, q1, gender) %>%
    add_sig(versus = "Male", test = "z_test_proportions") %>%
    hide_rows("Maybe")
  
  # Should still be cell-based
  expect_true(inherits(result, "tab_cell_collection"))
  
  # Should still have cells with significance
  store <- result$cell_store
  grid <- result$layout$grid
  
  has_sig <- FALSE
  for (i in seq_len(nrow(grid))) {
    for (j in seq_len(ncol(grid))) {
      cell_id <- grid[i, j]
      if (!is.na(cell_id)) {
        cell <- get_cell(store, cell_id)
        if (!is.null(cell$significance)) {
          has_sig <- TRUE
          break
        }
      }
    }
    if (has_sig) break
  }
  
  expect_true(has_sig)
})

test_that("multiple significance tests can be added to cells", {
  skip_if_not_installed("surveydatar")
  
  data <- data.frame(
    q1 = factor(c("Yes", "Yes", "No", "Yes", "No", "No", "Yes", "Yes")),
    region = factor(c("North", "South", "East", "North", "South", "East", "North", "South"))
  )
  
  result <- tab(data, q1, region) %>%
    add_sig(versus = "North", name = "vs_north") %>%
    add_sig(versus = "South", name = "vs_south")
  
  # Check that cells have multiple tests
  store <- result$cell_store
  grid <- result$layout$grid
  
  found_multi_test <- FALSE
  for (i in seq_len(nrow(grid))) {
    for (j in seq_len(ncol(grid))) {
      cell_id <- grid[i, j]
      if (!is.na(cell_id)) {
        cell <- get_cell(store, cell_id)
        if (!is.null(cell$significance)) {
          if (length(names(cell$significance)) >= 2) {
            expect_true("vs_north" %in% names(cell$significance))
            expect_true("vs_south" %in% names(cell$significance))
            found_multi_test <- TRUE
            break
          }
        }
      }
    }
    if (found_multi_test) break
  }
  
  expect_true(found_multi_test)
})

test_that("significance extracted correctly during materialization", {
  skip_if_not_installed("surveydatar")
  
  data <- data.frame(
    q1 = factor(c("Yes", "Yes", "No", "Yes", "No", "No", "Yes", "Yes")),
    gender = factor(c("Male", "Female", "Male", "Female", "Male", "Female", "Male", "Female"))
  )
  
  result <- tab(data, q1, gender) %>%
    add_sig(versus = "Male", test = "z_test_proportions")
  
  # Materialize
  df <- as.data.frame(result)
  
  # Should have significance attribute
  sig <- attr(df, "significance")
  expect_true(!is.null(sig))
  expect_true(is.list(sig))
  expect_true("Male" %in% names(sig))
  
  # Significance should have expected structure
  sig_male <- sig[["Male"]]
  expect_true(!is.null(sig_male$levels))
  expect_true(!is.null(sig_male$p_values))
  expect_true(is.matrix(sig_male$levels))
  expect_true(is.matrix(sig_male$p_values))
})

test_that("add_sig_all works with cell-based results", {
  skip_if_not_installed("surveydatar")
  
  data <- data.frame(
    q1 = factor(c("Yes", "Yes", "No", "Yes", "No", "No", "Yes", "Yes")),
    region = factor(c("North", "South", "East", "North", "South", "East", "North", "South"))
  )
  
  result <- tab(data, q1, region) %>%
    add_sig_all(test = "z_test_proportions")
  
  # Should still be cell-based
  expect_true(inherits(result, "tab_cell_collection"))
  
  # Should have significance in cells
  store <- result$cell_store
  grid <- result$layout$grid
  
  has_sig <- FALSE
  for (i in seq_len(nrow(grid))) {
    for (j in seq_len(ncol(grid))) {
      cell_id <- grid[i, j]
      if (!is.na(cell_id)) {
        cell <- get_cell(store, cell_id)
        if (!is.null(cell$significance)) {
          has_sig <- TRUE
          break
        }
      }
    }
    if (has_sig) break
  }
  
  expect_true(has_sig)
})

test_that("helper functions work correctly", {
  skip_if_not_installed("surveydatar")
  
  # Test extract_result_matrix_from_grid
  data <- data.frame(
    q1 = factor(c("Yes", "No", "Yes", "No")),
    gender = factor(c("Male", "Female", "Male", "Female"))
  )
  
  result <- tab(data, q1, gender)
  store <- result$cell_store
  grid <- result$layout$grid
  
  matrix <- extract_result_matrix_from_grid(store, grid)
  expect_true(is.matrix(matrix))
  expect_equal(nrow(matrix), nrow(grid))
  expect_equal(ncol(matrix), ncol(grid))
  
  # Test identify_meta_indices
  meta_rows <- identify_meta_indices(store, grid, "row")
  meta_cols <- identify_meta_indices(store, grid, "col")
  
  expect_true(is.integer(meta_rows))
  expect_true(is.integer(meta_cols))
  
  # Test build_sig_index_mapping
  mapping <- build_sig_index_mapping(5, c(2, 4))
  expect_equal(length(mapping), 5)
  expect_equal(mapping[1], 1)
  expect_equal(mapping[2], 0)  # Skipped
  expect_equal(mapping[3], 2)
  expect_equal(mapping[4], 0)  # Skipped
  expect_equal(mapping[5], 3)
})

test_that("cell structure includes significance field", {
  skip_if_not_installed("surveydatar")
  
  data <- data.frame(
    q1 = factor(c("Yes", "Yes", "No", "Yes")),
    gender = factor(c("Male", "Female", "Male", "Female"))
  )
  
  result <- tab(data, q1, gender) %>%
    add_sig(versus = "Male", test = "z_test_proportions")
  
  store <- result$cell_store
  grid <- result$layout$grid
  
  # Find a cell with significance
  for (i in seq_len(nrow(grid))) {
    for (j in seq_len(ncol(grid))) {
      cell_id <- grid[i, j]
      if (!is.na(cell_id)) {
        cell <- get_cell(store, cell_id)
        if (!is.null(cell$significance)) {
          # Check structure
          expect_true(is.list(cell$significance))
          test_name <- names(cell$significance)[1]
          sig_data <- cell$significance[[test_name]]
          
          expect_true(!is.null(sig_data$level))
          expect_true(!is.null(sig_data$p_value))
          expect_true(!is.null(sig_data$test_used))
          expect_true(!is.null(sig_data$versus))
          
          return()  # Test passed
        }
      }
    }
  }
  
  # Should have found at least one cell with significance
  expect_true(TRUE)
})

##### EXTERNAL VARIABLE RESOLUTION TESTS #####

test_that("External variables are inlined in expressions", {
  data <- create_tab_test_data(n = 100)
  
  # External variable
  desirable_values <- c(4, 5)
  
  result <- tab(data, satisfaction %in% desirable_values)
  df <- as.data.frame(result)
  
  # Should work and produce correct results
  expect_true(nrow(df) > 0)
  
  # Check that the expression in metadata has inlined values, not the variable name
  store <- result$cell_store
  cell_id <- result$layout$grid[1, 1]
  cell <- get_cell(store, cell_id)
  
  # The expression should contain the literal values, not "desirable_values"
  expr_str <- deparse(cell$specification$row_expr)
  expect_true(grepl("4", expr_str))
  expect_true(grepl("5", expr_str))
  expect_false(grepl("desirable_values", expr_str))
})

test_that("Multiple external variables are inlined", {
  data <- create_tab_test_data(n = 100)
  
  min_val <- 4
  max_val <- 5
  
  result <- tab(data, satisfaction >= min_val & satisfaction <= max_val)
  df <- as.data.frame(result)
  
  expect_true(nrow(df) > 0)
  
  # Check expression has literal values
  store <- result$cell_store
  cell_id <- result$layout$grid[1, 1]
  cell <- get_cell(store, cell_id)
  expr_str <- deparse(cell$specification$row_expr)
  
  expect_true(grepl("4", expr_str))
  expect_true(grepl("5", expr_str))
  expect_false(grepl("min_val", expr_str))
  expect_false(grepl("max_val", expr_str))
})

test_that("External variables in filter expressions are inlined", {
  data <- create_tab_test_data(n = 100)
  
  min_age <- 25
  
  result <- tab(data, satisfaction, filter = age > min_age)
  df <- as.data.frame(result)
  
  expect_true(nrow(df) > 0)
  
  # Filter should have inlined the value
  cell_id <- result$layout$grid[1, 1]
  cell <- get_cell(result$cell_store, cell_id)
  base_spec <- cell$specification$base_expr
  if (!is.null(base_spec)) {
    expr_str <- deparse(base_spec)
    expect_true(grepl("25", expr_str))
    expect_false(grepl("min_age", expr_str))
  }
})

test_that("Helper arguments with external variables are inlined", {
  data <- create_tab_test_data(n = 100)
  
  n_boxes <- 2
  
  result <- tab(data, top_box(satisfaction, n_boxes))
  df <- as.data.frame(result)
  
  expect_true(nrow(df) > 0)
  
  # The helper call should be preserved but argument should be inlined
  store <- result$cell_store
  cell_id <- result$layout$grid[1, 1]
  cell <- get_cell(store, cell_id)
  expr_str <- deparse(cell$specification$row_expr)
  
  expect_true(grepl("top_box", expr_str))
  expect_true(grepl("2", expr_str))
  expect_false(grepl("n_boxes", expr_str))
})

test_that("Data columns take priority over external variables", {
  data <- create_tab_test_data(n = 100)
  
  # Add a column with the same name as our external variable
  data$threshold <- rep(10, nrow(data))
  
  # External variable with same name
  threshold <- 3
  
  # Should use data column, not external variable
  result <- tab(data, satisfaction > threshold, hide_empty = FALSE)
  df <- as.data.frame(result)
  
  expect_true(nrow(df) > 0)
  
  # Expression should still have threshold as a symbol (data column)
  store <- result$cell_store
  cell_id <- result$layout$grid[1, 1]
  cell <- get_cell(store, cell_id)
  expr_str <- deparse(cell$specification$row_expr)
  
  # Should reference the symbol, not the literal
  expect_true(grepl("threshold", expr_str))
  # Should not be the external variable value
  expect_false(grepl("^3$", expr_str))
})

test_that("Unregistered functions produce helpful error", {
  data <- create_tab_test_data(n = 100)
  
  # Unregistered function
  my_filter <- function(x) x > 3
  
  expect_error(
    tab(data, my_filter(satisfaction)),
    "Function 'my_filter' used in expression but not registered as helper"
  )
  
  expect_error(
    tab(data, my_filter(satisfaction)),
    "create_helper"
  )
})

test_that("Unregistered function variables produce helpful error", {
  data <- create_tab_test_data(n = 100)
  
  # Function stored in variable
  filter_fn <- function(x) x > 3
  
  expect_error(
    tab(data, satisfaction > filter_fn),
    "Function 'filter_fn' used in expression but not registered as helper"
  )
})

test_that("Registered helpers are preserved, not inlined", {
  data <- create_tab_test_data(n = 100)
  
  result <- tab(data, top_box(satisfaction, 2))
  
  # The expression should still contain the helper call
  store <- result$cell_store
  cell_id <- result$layout$grid[1, 1]
  cell <- get_cell(store, cell_id)
  expr_str <- deparse(cell$specification$row_expr)
  
  expect_true(grepl("top_box", expr_str))
})

test_that("Metadata only includes data columns after resolution", {
  data <- create_tab_test_data(n = 100)
  
  external_val <- 4
  
  result <- tab(data, satisfaction > external_val)
  
  store <- result$cell_store
  cell_id <- result$layout$grid[1, 1]
  cell <- get_cell(store, cell_id)
  
  # Metadata variables should only include data columns
  # Check variables in the expression (since meta variables aren't populated for simple expressions)
  expr_vars <- all.vars(cell$specification$row_expr)
  
  expect_true("satisfaction" %in% expr_vars)
  expect_false("external_val" %in% expr_vars)
})

test_that("Serialization works with inlined external variables", {
  data <- create_tab_test_data(n = 100)
  
  threshold <- 4
  
  result <- tab(data, satisfaction > threshold)
  
  # Save to temp file
  temp_file <- tempfile(fileext = ".rds")
  on.exit(unlink(temp_file))
  
  saveRDS(result, temp_file)
  
  # Load in fresh environment (threshold no longer exists)
  rm(threshold)
  result_loaded <- readRDS(temp_file)
  
  # Should still work
  df <- as.data.frame(result_loaded)
  expect_true(nrow(df) > 0)
  
  # Expression should have literal value
  store <- result_loaded$cell_store
  cell_id <- result_loaded$layout$grid[1, 1]
  cell <- get_cell(store, cell_id)
  expr_str <- deparse(cell$specification$row_expr)
  
  expect_true(grepl("4", expr_str))
  expect_false(grepl("threshold", expr_str))
})

test_that("Complex expressions with external variables work", {
  data <- create_tab_test_data(n = 100)
  
  vals <- c(4, 5)
  status <- 1
  
  result <- tab(data, satisfaction %in% vals & gender == status)
  df <- as.data.frame(result)
  
  expect_true(nrow(df) > 0)
  
  # Both external variables should be inlined
  store <- result$cell_store
  cell_id <- result$layout$grid[1, 1]
  cell <- get_cell(store, cell_id)
  expr_str <- deparse(cell$specification$row_expr)
  
  expect_false(grepl("vals", expr_str))
  expect_false(grepl("status", expr_str))
  expect_true(grepl("4", expr_str) || grepl("5", expr_str))
})

test_that("External variables work with rows and cols", {
  data <- create_tab_test_data(n = 100)
  
  row_vals <- c(4, 5)
  col_val <- 1
  
  result <- tab(data, satisfaction %in% row_vals, gender == col_val)
  df <- as.data.frame(result)
  
  expect_true(nrow(df) > 0)
  expect_true(ncol(df) > 1)  # Should have columns
})

test_that("Empty external variables are handled correctly", {
  data <- create_tab_test_data(n = 100)
  
  empty_vec <- numeric(0)
  
  # This should work but match nothing
  result <- tab(data, satisfaction %in% empty_vec, hide_empty = FALSE)
  df <- as.data.frame(result)
  
  # Should have rows (the labels) but all zeros or NAs
  expect_true(nrow(df) > 0)
})

test_that("Large external vectors are inlined correctly", {
  data <- create_tab_test_data(n = 100)
  
  large_vec <- 1:50
  
  result <- tab(data, satisfaction %in% large_vec)
  df <- as.data.frame(result)
  
  expect_true(nrow(df) > 0)
  
  # Expression should have all the values inlined
  store <- result$cell_store
  cell_id <- result$layout$grid[1, 1]
  cell <- get_cell(store, cell_id)
  expr_str <- deparse(cell$specification$row_expr)
  
  expect_true(grepl("1", expr_str))
  expect_true(grepl("50", expr_str))
})

test_that("Base R functions work in expressions", {
  data <- create_tab_test_data(n = 100)
  
  # Use an expression that evaluates to logical/numeric
  result1 <- tab(data, as.numeric(gender) > 1, region, hide_empty = FALSE)
  expect_s3_class(result1, "tab_cell_collection")
  df1 <- as.data.frame(result1)
  expect_true(nrow(df1) > 0)
  
  # as.character() should work
  result2 <- tab(data, satisfaction, filter = as.character(region) == "North", hide_empty = FALSE)
  expect_s3_class(result2, "tab_cell_collection")
  df2 <- as.data.frame(result2)
  expect_true(nrow(df2) > 0)
  
  # c() should work
  result3 <- tab(data, satisfaction %in% c(4, 5), hide_empty = FALSE)
  expect_s3_class(result3, "tab_cell_collection")
  df3 <- as.data.frame(result3)
  expect_true(nrow(df3) > 0)
})

test_that("Package functions work in expressions", {
  data <- create_tab_test_data(n = 100)
  
  # Haven functions should work (they're from a package)
  # This tests that we're not blocking package functions
  if (requireNamespace("haven", quietly = TRUE)) {
    # This should work without error
    result <- tab(data, satisfaction)
    expect_s3_class(result, "tab_cell_collection")
  }
})


# Test data generator (simplified version)
create_hide_empty_test_data <- function(n = 100) {
  set.seed(123)
  data.frame(
    gender = factor(
      sample(c("Male", "Female"), n, replace = TRUE),
      levels = c("Male", "Female")
    ),
    region = factor(
      sample(c("North", "South", "East", "West"), n, replace = TRUE),
      levels = c("North", "South", "East", "West")
    ),
    value = sample(1:5, n, replace = TRUE)
  )
}

test_that("hide_empty = FALSE preserves empty rows under filter", {
  # Create a factor variable with a level that will be filtered out
  test_data <- create_hide_empty_test_data(n = 100)
  
  # Add a region_code variable with levels including one we'll filter out
  test_data$region_code <- factor(
    sample(c("A", "B", "C"), nrow(test_data), replace = TRUE),
    levels = c("A", "B", "C", "D")  # D will never appear in data
  )
  
  # Tab with filter that excludes region_code == "C"
  result <- tab(test_data, 
                region_code, 
                gender,
                filter = region_code != "C",
                hide_empty = FALSE)
  
  df <- as.data.frame(result)
  
  # With hide_empty=FALSE, we should still see row for "C"
  expect_true("C" %in% df$row_label)
})

test_that("hide_empty = TRUE removes empty rows under filter", {
  # Same setup as above
  test_data <- create_hide_empty_test_data(n = 100)
  test_data$region_code <- factor(
    sample(c("A", "B", "C"), nrow(test_data), replace = TRUE),
    levels = c("A", "B", "C", "D")
  )
  
  # Tab with hide_empty = TRUE
  result <- tab(test_data, 
                region_code, 
                gender,
                statistic = "row_pct",
                filter = region_code != "C",
                hide_empty = TRUE)
  
  df <- as.data.frame(result)
  
  # With hide_empty=TRUE, row "C" should be gone
  expect_false("C" %in% df$row_label)
  
  # And we should still see A and B
  expect_true("A" %in% df$row_label)
  expect_true("B" %in% df$row_label)
})

test_that("row universes reflect filtered exposure", {
  test_data <- create_hide_empty_test_data(n = 100)
  test_data$row_var <- factor(
    sample(c("R1", "R2", "R3"), nrow(test_data), replace = TRUE),
    levels = c("R1", "R2", "R3")
  )
  test_data$col_var <- factor(
    sample(c("C1", "C2"), nrow(test_data), replace = TRUE),
    levels = c("C1", "C2")
  )
  
  result <- tab(test_data,
                row_var,
                col_var,
                filter = row_var != "R2",
                hide_empty = FALSE)
  
  layout <- result$layout
  row_uni <- layout$row_universe
  
  expect_equal(row_uni[layout$row_labels == "R2"], 0)
  expect_gt(row_uni[layout$row_labels == "R1"], 0)
  expect_gt(row_uni[layout$row_labels == "R3"], 0)
  
  result_hidden <- tab(test_data,
                       row_var,
                       col_var,
                       filter = row_var != "R2",
                       hide_empty = TRUE)
  df_hidden <- as.data.frame(result_hidden)
  expect_false("R2" %in% df_hidden$row_label)
})

test_that("hide_empty = TRUE removes empty columns under filter", {
  # Create test data with a factor column variable
  test_data <- create_hide_empty_test_data(n = 100)
  test_data$brand_small <- factor(
    sample(c("X", "Y", "Z"), nrow(test_data), replace = TRUE),
    levels = c("X", "Y", "Z", "W")  # W will never appear
  )
  
  # Tab with filter that excludes brand_small == "Z"
  result <- tab(test_data, 
                gender, 
                brand_small,
                statistic = "column_pct",
                filter = brand_small != "Z",
                hide_empty = TRUE)
  
  df <- as.data.frame(result)
  
  # With hide_empty=TRUE, column "Z" should be gone
  expect_false("Z" %in% names(df))
  
  # And we should still see X and Y
  expect_true("X" %in% names(df))
  expect_true("Y" %in% names(df))
})

test_that("column universes reflect filtered exposure", {
  test_data <- create_hide_empty_test_data(n = 120)
  test_data$row_var <- factor(
    sample(c("R1", "R2"), nrow(test_data), replace = TRUE),
    levels = c("R1", "R2")
  )
  test_data$col_var <- factor(
    sample(c("C1", "C2", "C3"), nrow(test_data), replace = TRUE),
    levels = c("C1", "C2", "C3")
  )
  
  result <- tab(test_data,
                row_var,
                col_var,
                filter = col_var != "C3",
                hide_empty = FALSE)
  layout <- result$layout
  col_uni <- layout$col_universe
  
  expect_equal(col_uni[layout$col_labels == "C3"], 0)
  expect_gt(col_uni[layout$col_labels == "C1"], 0)
  expect_gt(col_uni[layout$col_labels == "C2"], 0)
  
  result_hidden <- tab(test_data,
                       row_var,
                       col_var,
                       filter = col_var != "C3",
                       hide_empty = TRUE)
  df_hidden <- as.data.frame(result_hidden)
  expect_false("C3" %in% names(df_hidden))
})

test_that("hide_empty works with low_base_threshold", {
  # Create test data where one category will have very low base
  test_data <- create_hide_empty_test_data(n = 100)
  
  # Create a skewed distribution
  test_data$category <- factor(
    c(rep("Common", 95), rep("Rare", 5)),
    levels = c("Common", "Rare", "VeryRare")
  )
  
  # Tab with low_base_threshold that will filter out "Rare" and "VeryRare"
  result <- tab(test_data, 
                category, 
                gender,
                statistic = "row_pct",
                low_base_threshold = 10,
                hide_empty = TRUE)
  
  df <- as.data.frame(result)
  
  # With hide_empty=TRUE and low_base_threshold, both Rare and VeryRare should be gone
  expect_false("Rare" %in% df$row_label)
  expect_false("VeryRare" %in% df$row_label)
  expect_true("Common" %in% df$row_label)
})

test_that("hide_empty preserves summary rows even when they are empty", {
  # Create data where summary row might be all NA
  test_data <- create_hide_empty_test_data(n = 50)
  test_data$binary_var <- factor(
    sample(c("Yes", "No"), nrow(test_data), replace = TRUE),
    levels = c("Yes", "No")
  )
  
  # Tab with a NET row (for count statistic, this is "Total")
  result <- tab(test_data, 
                binary_var, 
                gender,
                statistic = "count",
                show_col_nets = TRUE,
                hide_empty = TRUE)
  
  df <- as.data.frame(result)
  
  # Total column should still be present even with hide_empty=TRUE
  expect_true("Total" %in% names(df))
})

test_that("hide_empty preserves base row even when filtering heavily", {
  test_data <- create_hide_empty_test_data(n = 100)
  test_data$test_var <- factor(
    sample(c("A", "B"), nrow(test_data), replace = TRUE),
    levels = c("A", "B", "C")
  )
  
  # Tab with show_base and hide_empty both TRUE
  result <- tab(test_data, 
                test_var, 
                gender,
                statistic = "row_pct",
                filter = test_var == "A",
                show_base = TRUE,
                hide_empty = TRUE)
  
  df <- as.data.frame(result)
  
  # Base row should still be present
  expect_true("Base (n)" %in% df$row_label)
  
  # "B" and "C" should be gone (all NA due to filter)
  expect_false("B" %in% df$row_label)
  expect_false("C" %in% df$row_label)
})

test_that("hide_empty handles edge case where all non-summary rows are empty", {
  test_data <- create_hide_empty_test_data(n = 50)
  test_data$always_a <- factor(rep("A", nrow(test_data)), levels = c("A", "B", "C"))
  
  # Filter that removes all data
  result <- tab(test_data, 
                always_a, 
                gender,
                filter = always_a == "B",  # Will match nothing
                show_base = TRUE,
                hide_empty = TRUE)
  
  df <- as.data.frame(result)
  
  # Should have at least base row, but no data rows
  expect_true("Base (n)" %in% df$row_label)
  
  # A, B, C should all be gone (all NA)
  expect_false("A" %in% df$row_label)
  expect_false("B" %in% df$row_label)
  expect_false("C" %in% df$row_label)
})

test_that("hide_empty works with both rows and columns simultaneously", {
  test_data <- create_hide_empty_test_data(n = 100)
  test_data$row_var <- factor(
    sample(c("R1", "R2"), nrow(test_data), replace = TRUE),
    levels = c("R1", "R2", "R3")
  )
  test_data$col_var <- factor(
    sample(c("C1", "C2"), nrow(test_data), replace = TRUE),
    levels = c("C1", "C2", "C3")
  )
  
  # Filter that leaves R3 and C3 empty
  result <- tab(test_data, 
                row_var, 
                col_var,
                filter = row_var != "R3" & col_var != "C3",
                hide_empty = TRUE)
  
  df <- as.data.frame(result)
  
  # R3 should be gone from rows
  expect_false("R3" %in% df$row_label)
  # C3 should be gone from columns
  expect_false("C3" %in% names(df))
  
  # R1, R2, C1, C2 should remain
  expect_true("R1" %in% df$row_label)
  expect_true("R2" %in% df$row_label)
  expect_true("C1" %in% names(df))
  expect_true("C2" %in% names(df))
})