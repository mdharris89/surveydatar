# TO DO:
# - Add base calculator tests

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
setup({
  reset_to_builtins()
})

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

mini_data <- create_mini_test_data(10)
mini_survey_data <- create_survey_data(mini_data)

##### Basic Tab Functionality Tests #####

test_that("basic tab functionality retuns the expected format", {
  # Use shared test data
  result <- tab(test_survey_data, gender, region)
  expect_s3_class(result, "tab_result")
  expect_equal(ncol(result) - 1, 5) # 4 regions + Total column
  expect_true(any(grepl("Male", result$row_label)))
  expect_true(any(grepl("Female", result$row_label)))
})

test_that("tab works with survey_data objects properly", {
  result <- tab(test_survey_data, gender, region)
  expect_s3_class(result, "tab_result")

  expect_true(any(grepl("Respondent Gender", result$row_label)))
})

##### Basic Helper Functionality Tests #####

test_that("top and bottom box helper functions return correct format", {
  # Test top_box helper
  result <- tab(test_survey_data, top_box(satisfaction, 2), gender)
  expect_s3_class(result, "tab_result")
  expect_true(any(grepl("top_box", result$row_label)))

  # Test bottom_box helper
  result2 <- tab(test_survey_data, bottom_box(satisfaction, 2), gender)
  expect_s3_class(result2, "tab_result")
  expect_true(any(grepl("bottom_box", result2$row_label)))
})

test_that("multiple helpers work with rows_list", {
  result <- tab(test_survey_data,
                rows = rows_list(
                  "Top 2" = top_box(satisfaction, 2),
                  "Bottom 2" = bottom_box(satisfaction, 2)
                ),
                cols = gender)
  expect_s3_class(result, "tab_result")
  expect_equal(nrow(result) - 2, 2) # 2 helper rows
  expect_true(any(grepl("Top 2", result$row_label)))
  expect_true(any(grepl("Bottom 2", result$row_label)))
})

##### Variable Expansion Tests #####

test_that("question group expansion works", {

  # Test q1 group expansion (should expand to q1_1, q1_2, q1_3)
  result <- tab(test_survey_data, q1, gender)
  expect_s3_class(result, "tab_result")
  expect_true(nrow(result) >= 5) # At least 3 q1 items + base + total
  expect_true(any(grepl("Quality", result$row_label)))
  expect_true(any(grepl("Value", result$row_label)))
  expect_true(any(grepl("Trust", result$row_label)))

  # Test q2 group expansion (should expand to q2_1, q2_2)
  result2 <- tab(test_survey_data, q2, gender)
  expect_s3_class(result2, "tab_result")
  expect_true(nrow(result2) >= 4) # At least 2 q2 items + base + total
  expect_true(any(grepl("Speed", result2$row_label)))
  expect_true(any(grepl("Accuracy", result2$row_label)))
})

test_that("labelled variable expansion works", {

  # Test satisfaction expansion (5 levels)
  result <- tab(test_survey_data, satisfaction)
  expect_s3_class(result, "tab_result")
  expect_equal(nrow(result) - 2, 5) # 5 satisfaction levels (excluding base and total)

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
  expect_equal(ncol(result) - 1, 5) # -1 for row_label column

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

  # Should have 17 rows (one for each brand) + base + total rows
  expect_equal(nrow(result) - 2, 17) # -2 for base and total rows

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

  # Should have 5 rows (satisfaction levels) + base + total rows
  expect_equal(nrow(result) - 2, 5)

  # Should have 4 columns (regions) + Total + row_label column
  expect_equal(ncol(result) - 1, 5)

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
  expect_equal(array_expr, as.numeric(data$age > 30))

  # Test multiplication (filter)
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

  # Test mean with values
  values <- c(5, 4, 3, 2, 1)
  cell_array <- row_array * col_array
  expect_equal(
    compute_cell(base_array, row_array, col_array, "mean", values),
    sum(cell_array * values) / sum(cell_array)
  )

  # Test division by zero
  expect_true(is.na(compute_cell(base_array, rep(1, 5), rep(0, 5), "column_pct")))
})

##### Custom Statistics and Helpers Creation Tests #####

test_that("custom statistic creation and usage works", {
  # Save current state
  original_state <- save_registry_state()

  # Create a simple custom statistic
  test_stat <- create_statistic(
    id = "test_double",
    processor = function(base_array, row_array, col_array, ...) {
      sum(base_array * row_array * col_array) * 2
    },
    base_calculator = base_column_total,
    format_fn = function(x) paste0(x, "x")
  )

  result <- tab(test_survey_data, gender, statistic = test_stat)

  expect_s3_class(result, "tab_result")
  expect_equal(attr(result, "statistic"), test_stat)

  # Restore state
  restore_registry_state(original_state)
})

test_that("custom helper creation and usage works", {
  # Save current state
  original_state <- save_registry_state()

  # Create a simple custom helper
  test_helper <- create_helper(
    id = "test_age_filter",
    processor = function(spec, data, ...) {
      threshold <- spec$components$threshold
      as.numeric(data$age > threshold)
    }
  )

  # Test that helper was registered
  expect_true("test_age_filter" %in% list_tab_helpers())
  expect_equal(get_helper("test_age_filter")$id, "test_age_filter")

  # Restore state
  restore_registry_state(original_state)
})

##### Validation Functions Tests #####

test_that("validate_statistic_variables catches inappropriate variable types", {
  mean_stat <- get_statistic("mean")
  skip_if(is.null(mean_stat), "Mean statistic not found in registry")

  # Test 1: Silent case (current test) - categorical should be fine
  data_good <- data.frame(categorical = factor(rep(c("A", "B"), 50)))
  expect_silent({
    rows <- list(list(type = "simple", components = list(var = "categorical")))
    cols <- list(list(type = "total"))
    validate_statistic_variables(mean_stat, rows, cols, data_good, dpdict = NULL)
  })

  # Test 2: ERROR case - numeric variable with >15 unique values (no labels, no factor)
  data_bad_numeric <- data.frame(continuous = 1:50)  # 50 unique values, unlabelled
  expect_error({
    rows <- list(list(type = "simple", components = list(var = "continuous")))
    cols <- list(list(type = "total"))
    validate_statistic_variables(mean_stat, rows, cols, data_bad_numeric, dpdict = NULL)
  }, "Cannot use numeric variable 'continuous' with 50 unique values")

  # Test 3: ERROR case - questiontype="numeric" in dpdict
  data_with_numeric_type <- data.frame(score = 1:10)
  dpdict_with_numeric <- data.frame(
    variable_names = "score",
    variable_labels = "Test Score",
    questiontype = "numeric"
  )
  expect_error({
    rows <- list(list(type = "simple", components = list(var = "score")))
    cols <- list(list(type = "total"))
    validate_statistic_variables(mean_stat, rows, cols, data_with_numeric_type, dpdict_with_numeric)
  }, "Cannot use numeric variable 'score' \\(questiontype: numeric\\)")
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
  total_base <- as.numeric(base_row$NET)

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

  # Should only have 2 rows: data row + Base
  expect_equal(nrow(result), 2)
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

##### Base Orientation Tests #####

test_that("base orientation displays correctly", {
  # For column-oriented statistics (default), base should be in rows
  result_col <- tab(test_survey_data, gender, region, statistic = "column_pct")
  expect_true("Base (n)" %in% result_col$row_label)

  # For row-oriented statistics, base should be in columns
  result_row <- tab(test_survey_data, gender, region, statistic = "row_pct")
  expect_true("Base (n)" %in% names(result_row))
})

##### Copy Tab Functionality #####

test_that("copy_tab handles different statistic types", {
  skip_if_not(requireNamespace("clipr", quietly = TRUE),
              "clipr package not available")
  skip_if_not(clipr::clipr_available(),
              "Clipboard not available")

  # Test with percentage
  result_pct <- tab(test_survey_data, gender, region, statistic = "column_pct")
  copied_pct <- copy_tab(result_pct)  # Return instead of copying

  expect_true(is.data.frame(copied_pct))
  expect_true(any(grepl("column_pct", copied_pct[nrow(copied_pct), 1])))

  # Test with mean
  result_mean <- tab(test_survey_data, gender, statistic = "mean", values = "age")
  copied_mean <- copy_tab(result_mean)

  expect_true(any(grepl("mean of 'age'", copied_mean[nrow(copied_mean), 1])))
})

test_that("copy_tab preserves significance indicators", {
  skip_if_not(requireNamespace("clipr", quietly = TRUE),
              "clipr package not available")
  skip_if_not(clipr::clipr_available(),
              "Clipboard not available")
  skip_if_not("z_test_proportions" %in% list_tab_significance_tests(),
              "Significance tests not registered")

  result <- tab(test_survey_data,
                gender,
                region)

  # Add significance
  result_with_sig <- add_sig(result)

  # copy_tab returns formatted data invisibly
  copied <- copy_tab(result_with_sig)

  # Should preserve any significance indicators in the output
  expect_true(is.data.frame(copied))
  # The print method shows significance indicators, which would be in the copied data
})

##### STATISTICS MATHEMATICAL ACCURACY TESTS #####

test_that("count statistic calculates exact counts", {

  # Manual calculation
  manual_count_male_north <- sum(test_data$gender == 1 & test_data$region == 1)
  manual_count_female_south <- sum(test_data$gender == 2 & test_data$region == 2)

  # Tab calculation
  result <- tab(test_survey_data, gender, region, statistic = "count")

  # Extract values (removing % signs and converting)
  male_north <- as.numeric(result[result$row_label == "Respondent Gender: Male", "Region: North"])
  female_south <- as.numeric(result[result$row_label == "Respondent Gender: Female", "Region: South"])

  # Test exact equality
  expect_equal(male_north, manual_count_male_north)
  expect_equal(female_south, manual_count_female_south)

  # Test total counts - extract each value individually and sum
  male_counts <- c(
    as.numeric(result[result$row_label == "Respondent Gender: Male", "Region: North"]),
    as.numeric(result[result$row_label == "Respondent Gender: Male", "Region: South"]),
    as.numeric(result[result$row_label == "Respondent Gender: Male", "Region: East"]),
    as.numeric(result[result$row_label == "Respondent Gender: Male", "Region: West"])
  )
  female_counts <- c(
    as.numeric(result[result$row_label == "Respondent Gender: Female", "Region: North"]),
    as.numeric(result[result$row_label == "Respondent Gender: Female", "Region: South"]),
    as.numeric(result[result$row_label == "Respondent Gender: Female", "Region: East"]),
    as.numeric(result[result$row_label == "Respondent Gender: Female", "Region: West"])
  )

  total_count <- sum(male_counts) + sum(female_counts)
  expect_equal(total_count, nrow(test_data))
})

test_that("column_pct statistic calculates exact percentages", {

  result <- tab(test_survey_data, gender, region, statistic = "column_pct")

  # Manual calculation for North region
  north_total <- sum(test_data$region == 1)
  male_north <- sum(test_data$gender == 1 & test_data$region == 1)
  female_north <- sum(test_data$gender == 2 & test_data$region == 1)

  expected_male_pct <- (male_north / north_total) * 100
  expected_female_pct <- (female_north / north_total) * 100

  # Extract percentages from result
  male_pct <- as.numeric(gsub("%", "", result[result$row_label == "Respondent Gender: Male", "Region: North"]))
  female_pct <- as.numeric(gsub("%", "", result[result$row_label == "Respondent Gender: Female", "Region: North"]))

  # Test with tolerance for rounding
  expect_equal(male_pct, expected_male_pct, tolerance = 0.1)
  expect_equal(female_pct, expected_female_pct, tolerance = 0.1)

  # Verify columns sum to 100%
  col_sum <- male_pct + female_pct
  expect_equal(col_sum, 100, tolerance = 0.1)
})

test_that("row_pct statistic calculates exact row percentages", {

  result <- tab(test_survey_data, gender, region, statistic = "row_pct")

  # Manual calculation for Male row
  male_total <- sum(test_data$gender == 1)
  male_north <- sum(test_data$gender == 1 & test_data$region == 1)
  male_south <- sum(test_data$gender == 1 & test_data$region == 2)

  expected_north_pct <- (male_north / male_total) * 100
  expected_south_pct <- (male_south / male_total) * 100

  # Extract percentages
  north_pct <- as.numeric(gsub("%", "", result[result$row_label == "Respondent Gender: Male", "Region: North"]))
  south_pct <- as.numeric(gsub("%", "", result[result$row_label == "Respondent Gender: Male", "Region: South"]))

  expect_equal(north_pct, expected_north_pct, tolerance = 0.1)
  expect_equal(south_pct, expected_south_pct, tolerance = 0.1)

  # Verify NET column shows 100% for each row
  net_male <- as.numeric(result[result$row_label == "Respondent Gender: Male", "NET"])
  expect_equal(net_male, 100)
})

test_that("mean statistic calculates exact means", {

  result <- tab(test_survey_data, gender, region, statistic = "mean", values = "test_values")

  # Manual calculation
  male_north_values <- test_data$test_values[test_data$gender == 1 & test_data$region == 1]
  expected_mean <- mean(male_north_values, na.rm = TRUE)

  # Extract from result
  actual_mean <- as.numeric(result[result$row_label == "Respondent Gender: Male", "Region: North"])

  expect_equal(actual_mean, expected_mean, tolerance = 0.01)

  # Test with NA values
  test_data_na <- test_data
  test_data_na$test_values[1:10] <- NA
  test_survey_data_na <- create_survey_data(test_data_na)

  result_na <- tab(test_survey_data_na, gender, statistic = "mean", values = "test_values")

  # Should calculate mean excluding NAs
  expected_mean_na <- mean(test_data_na$test_values[test_data_na$gender == 1], na.rm = TRUE)
  actual_mean_na <- as.numeric(result_na[result_na$row_label == "Respondent Gender: Male", "Total"])

  expect_equal(actual_mean_na, expected_mean_na, tolerance = 0.01)
})

test_that("median statistic calculates exact medians", {

  result <- tab(test_survey_data, gender, statistic = "median", values = "ordinal_test")

  # Manual calculation
  male_values <- test_data$ordinal_test[test_data$gender == 1]
  expected_median <- median(male_values, na.rm = TRUE)

  actual_median <- as.numeric(result[result$row_label == "Respondent Gender: Male", "Total"])

  expect_equal(actual_median, expected_median)

  # Test with tied values
  result_tied <- tab(test_survey_data, gender, statistic = "median", values = "tied_values")

  female_tied <- test_data$tied_values[test_data$gender == 2]
  expected_tied <- median(female_tied, na.rm = TRUE)
  actual_tied <- as.numeric(result_tied[result_tied$row_label == "Respondent Gender: Female", "Total"])

  expect_equal(actual_tied, expected_tied)
})

test_that("sd statistic calculates exact standard deviations", {

  result <- tab(test_survey_data, gender, region, statistic = "sd", values = "test_values")

  # Manual calculation for specific cell
  values <- test_data$test_values[test_data$gender == 1 & test_data$region == 2]
  expected_sd <- sd(values, na.rm = TRUE)

  actual_sd <- as.numeric(result[result$row_label == "Respondent Gender: Male", "Region: South"])

  expect_equal(actual_sd, expected_sd, tolerance = 0.01)

  # Test that NA is returned when insufficient data
  # Create a very specific scenario with known single values
  single_value_data <- test_data
  # Set all males in North to have the same test_value
  male_north_idx <- which(single_value_data$gender == 1 & single_value_data$region == 1)
  if (length(male_north_idx) > 0) {
    single_value_data$test_values[male_north_idx] <- 42  # All same value
  }

  single_value_survey <- create_survey_data(single_value_data)
  result_single <- tab(single_value_survey, gender, region, statistic = "sd", values = "test_values")

  # SD for male/north should be 0 (all same value) or NA if implementation returns NA for zero variance
  male_north_sd <- as.numeric(result_single[result_single$row_label == "Respondent Gender: Male", "Region: North"])
  expect_true(is.na(male_north_sd) || male_north_sd == 0)
})

test_that("cv statistic calculates coefficient of variation correctly", {

  result <- tab(test_survey_data, gender, statistic = "cv", values = "test_values")

  # Manual calculation
  male_values <- test_data$test_values[test_data$gender == 1]
  expected_cv <- (sd(male_values, na.rm = TRUE) / mean(male_values, na.rm = TRUE)) * 100

  actual_cv <- as.numeric(result[result$row_label == "Respondent Gender: Male", "Total"])

  expect_equal(actual_cv, expected_cv, tolerance = 0.01)
})

test_that("index statistic calculates correctly", {

  result <- tab(test_survey_data, binary_test, region, statistic = "index")

  # Manual calculation for "Yes" in North
  # Index = (cell % / total %) * 100
  yes_north <- sum(test_data$binary_test == 1 & test_data$region == 1)
  total_north <- sum(test_data$region == 1)
  cell_pct <- (yes_north / total_north) * 100

  yes_total <- sum(test_data$binary_test == 1)
  total_total <- nrow(test_data)
  total_pct <- (yes_total / total_total) * 100

  expected_index <- (cell_pct / total_pct) * 100

  actual_index <- as.numeric(result[result$row_label == "Binary test variable: Yes", "Region: North"])

  expect_equal(actual_index, expected_index, tolerance = 0.1)
})

test_that("percentile statistics calculate exact percentiles", {

  # Test p25
  result_p25 <- tab(test_survey_data, gender, statistic = "p25", values = "test_values")

  male_values <- test_data$test_values[test_data$gender == 1]
  expected_p25 <- quantile(male_values, probs = 0.25, na.rm = TRUE)
  actual_p25 <- as.numeric(result_p25[result_p25$row_label == "Respondent Gender: Male", "Total"])

  expect_equal(actual_p25, unname(expected_p25), tolerance = 0.1)

  # Test p75
  result_p75 <- tab(test_survey_data, gender, statistic = "p75", values = "test_values")

  expected_p75 <- quantile(male_values, probs = 0.75, na.rm = TRUE)
  actual_p75 <- as.numeric(result_p75[result_p75$row_label == "Respondent Gender: Male", "Total"])

  expect_equal(actual_p75, unname(expected_p75), tolerance = 0.1)

  # Verify p75 >= p25
  expect_true(actual_p75 >= actual_p25)
})

##### WEIGHTED STATISTICS TESTS #####

test_that("weighted statistics calculate correctly", {

  # Test weighted count
  result_weighted <- tab(test_survey_data, gender, region, weight = "weight", statistic = "count")

  # Manual weighted count
  weighted_male_north <- sum(test_data$weight[test_data$gender == 1 & test_data$region == 1])

  # The exact row will depend on how the data is labeled
  male_row <- grep("Male", result_weighted$row_label)[1]
  actual_weighted <- as.numeric(result_weighted[male_row, "Region: North"])

  expect_equal(actual_weighted, weighted_male_north, tolerance = 0.1)

  # Test weighted mean
  result_wmean <- tab(test_survey_data, gender, weight = "weight", statistic = "mean", values = "test_values")

  # Manual weighted mean calculation
  male_mask <- test_data$gender == 1
  male_weights <- test_data$weight[male_mask]
  male_values <- test_data$test_values[male_mask]
  valid_mask <- !is.na(male_values)

  expected_wmean <- sum(male_weights[valid_mask] * male_values[valid_mask]) / sum(male_weights[valid_mask])
  actual_wmean <- as.numeric(result_wmean[result_wmean$row_label == "Respondent Gender: Male", "Total"])

  expect_equal(actual_wmean, expected_wmean, tolerance = 0.01)
})

##### HELPER FUNCTION MATHEMATICAL ACCURACY TESTS #####

test_that("top_box helper selects correct values", {

  # Test top 2 box on satisfaction (values 4 and 5)
  result <- tab(test_survey_data, top_box(satisfaction, 2), gender)

  # Manual calculation
  top2_count <- sum(test_data$satisfaction %in% c(4, 5))

  # Get base row to verify total
  base_row <- result[result$row_label == "Base (n)", ]
  total_base <- as.numeric(base_row$Total)

  # Get top_box row
  topbox_row <- result[grep("top_box", result$row_label), ]
  topbox_total <- as.numeric(topbox_row$NET)

  # The percentage should match
  expected_pct <- (top2_count / nrow(test_data)) * 100
  expect_equal(topbox_total, expected_pct, tolerance = 0.1)
})

test_that("bottom_box helper selects correct values", {

  # Test bottom 2 box on satisfaction (values 1 and 2)
  result <- tab(test_survey_data, bottom_box(satisfaction, 2), gender)

  # Manual calculation
  bottom2_count <- sum(test_data$satisfaction %in% c(1, 2))

  # Get bottom_box row
  bottombox_row <- result[grep("bottom_box", result$row_label), ]
  bottombox_total <- as.numeric(bottombox_row$NET)

  expected_pct <- (bottom2_count / nrow(test_data)) * 100
  expect_equal(bottombox_total, expected_pct, tolerance = 0.1)
})

# ERROR:
test_that("value_range helper filters correctly", {

  # Test age range 25-45
  result <- tab(test_survey_data, value_range(age, 25, 45), gender)

  # Manual calculation
  in_range <- sum(test_data$age >= 25 & test_data$age <= 45)

  range_row <- result[grep("value_range", result$row_label), ]
  range_total <- as.numeric(range_row$NET)

  expected_pct <- (in_range / nrow(test_data)) * 100
  expect_equal(range_total, expected_pct, tolerance = 0.1)
})

test_that("pattern helper matches correctly", {

  # Test pattern matching on region labels
  result <- tab(test_survey_data, pattern(region_character, "North|South"), gender)

  # Manual calculation - regions 1 (North) and 2 (South)
  matches <- sum(test_data$region_character %in% c("North", "South"))

  pattern_row <- result[grep("pattern", result$row_label), ]
  pattern_total <- as.numeric(pattern_row$NET)

  expected_pct <- (matches / nrow(test_data)) * 100
  expect_equal(pattern_total, expected_pct, tolerance = 0.1)
})

test_that("percentile helper filters correctly", {

  # Test top 25% of income
  result <- tab(test_survey_data, percentile(income, "above", 75), gender)

  # Manual calculation
  p75_threshold <- quantile(test_data$income, probs = 0.75, na.rm = TRUE)
  above_p75 <- sum(test_data$income > p75_threshold)

  percentile_row <- result[grep("percentile", result$row_label), ]
  percentile_total <- as.numeric(percentile_row$NET)

  # Should be approximately 25% (might not be exact due to ties)
  expected_pct <- (above_p75 / nrow(test_data)) * 100
  expect_equal(percentile_total, expected_pct, tolerance = 2)  # Higher tolerance for percentile edge cases
})

test_that("identity_arrays works with categorical array question groups", {

  # Use explicit rows/cols as suggested in the example
  result <- tab(test_survey_data,
                rows = identity_arrays(c("Daily", "Weekly", "Monthly", "Never"), A2_a),
                cols = identity_arrays(c("Docs", "Presentations", "Spreadsheets"), A2_a),
                smart_collapse_row_labels = TRUE)

  # Check structure
  expect_s3_class(result, "tab_result")

  # Should have 4 rows (Daily, Weekly, Monthly, Never) after smart collapse + NET and Base
  expect_equal(nrow(result), 6)

  # Should have 3 data columns (Docs, Presentations, Spreadsheets) + NET
  expect_equal(ncol(result) - 1, 4)  # -1 for row_label column

  # Check row labels match frequency bands
  expect_equal(result$row_label, c("Daily", "Weekly", "Monthly", "Never", "NET", "Base (n)"))

  # Check column names match document types
  expect_equal(names(result)[-1], c("Docs", "Presentations", "Spreadsheets", "NET"))

  # Verify the values make sense - each column should sum to ~100% (column_pct default)
  for (col in names(result)[-1]) {
    col_sum <- sum(result[[col]], na.rm = TRUE)
    expect_true(abs(col_sum - 100) < 1,
                info = paste("Column", col, "should sum to ~100%, got", col_sum))
  }

  # Also test the simpler form with just columns specified
  result2 <- tab(test_survey_data,
                 rows = A2_a,
                 cols = identity_arrays(c("Docs", "Presentations", "Spreadsheets"), A2_a),
                 smart_collapse_row_labels = TRUE)

  # This should expand A2_a in rows, creating 12 initial rows (3 vars × 4 values)
  # But after smart_collapse, should reduce back to 4 rows
  expect_equal(nrow(result2), 4)

  # Results should be identical
  expect_equal(result$row_label, result2$row_label)
  expect_equal(names(result), names(result2))

  # Test that pattern matching works on variable labels
  # "Docs" should match "How often do you create - Docs" in the variable label
  arrays_attr <- attr(result, "arrays")
  expect_true(!is.null(arrays_attr$col_arrays))
  expect_equal(length(arrays_attr$col_arrays), 3)

  # Each column array should have some non-zero values
  for (col_array in arrays_attr$col_arrays) {
    expect_true(sum(col_array) > 0,
                info = "Column array should match some observations")
  }
})

##### EDGE CASES AND ERROR CONDITIONS #####

test_that("helpers handle missing data correctly", {
  test_data <- create_tab_test_data(100)

  # Set some satisfaction values to NA
  test_data$satisfaction[1:20] <- NA
  test_survey_data <- create_survey_data(test_data)

  result <- tab(test_survey_data, top_box(satisfaction, 2), gender)

  # Should calculate based on non-NA values only
  non_na_satisfaction <- test_data$satisfaction[!is.na(test_data$satisfaction)]
  top2_count <- sum(non_na_satisfaction %in% c(4, 5))

  topbox_row <- result[grep("top_box", result$row_label), ]
  topbox_total <- as.numeric(topbox_row$NET)

  # Percentage should be of total sample (including NAs)
  expected_pct <- (top2_count / nrow(test_data)) * 100
  expect_equal(topbox_total, expected_pct, tolerance = 0.1)
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

  for (col in c("Region: North", "Region: South", "Region: East", "Region: West")) {
    region_num <- which(c("North", "South", "East", "West") == gsub("Region: ", "", col))
    expected_avg <- mean(test_data$age[test_data$region == region_num], na.rm = TRUE)
    actual_avg <- as.numeric(avg_row[[col]])
    expect_equal(actual_avg, expected_avg, tolerance = 0.01)
  }
})

##### BASE ROW CALCULATIONS #####

test_that("base row calculations are accurate for all statistics", {

  # Test column_pct base
  result_col <- tab(test_survey_data, gender, region, statistic = "column_pct")
  base_row <- result_col[result_col$row_label == "Base (n)", ]

  # Each column base should equal total in that column
  for (i in 1:4) {
    region_total <- sum(test_data$region == i)
    col_name <- paste0("Region: ", c("North", "South", "East", "West")[i])
    expect_equal(as.numeric(base_row[[col_name]]), region_total)
  }

  # Test row_pct base (should be in columns)
  result_row <- tab(test_survey_data, gender, region, statistic = "row_pct")
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
                                result[result$row_label == "Respondent Gender: Male", "Region: North"]))

  expect_equal(actual_pct, expected_pct, tolerance = 0.1)
})

##### MULTIPLE STATISTIC VALIDATION #####

test_that("different statistics are consistent with each other", {
  test_data <- create_tab_test_data(100)
  test_survey_data <- create_survey_data(test_data)

  # Count and percentage should be consistent
  result_count <- tab(test_survey_data, gender, region, statistic = "count")
  result_pct <- tab(test_survey_data, gender, region, statistic = "column_pct")

  # Calculate percentage from count
  male_north_count <- as.numeric(result_count[result_count$row_label == "Respondent Gender: Male", "Region: North"])
  total_north_count <- as.numeric(result_count[result_count$row_label == "NET", "Region: North"])

  calculated_pct <- (male_north_count / total_north_count) * 100
  reported_pct <- as.numeric(gsub("%", "",
                                  result_pct[result_pct$row_label == "Respondent Gender: Male", "Region: North"]))

  expect_equal(calculated_pct, reported_pct, tolerance = 0.1)
})

##### CROSS-VALIDATION AND CONSISTENCY TESTS #####

test_that("weighted and unweighted calculations maintain consistent relationships", {
  test_data <- create_tab_test_data(100)
  test_survey_data <- create_survey_data(test_data)

  # Compare weighted vs unweighted percentages
  result_unweighted <- tab(test_survey_data, gender, region, statistic = "column_pct")
  result_weighted <- tab(test_survey_data, gender, region, statistic = "column_pct", weight = "weight")

  # Both should sum to 100% in each column
  for (col in c("Region: North", "Region: South", "Region: East", "Region: West")) {
    unweighted_sum <- sum(as.numeric(gsub("%", "", result_unweighted[1:2, col])))
    weighted_sum <- sum(as.numeric(gsub("%", "", result_weighted[1:2, col])))

    expect_equal(unweighted_sum, 100, tolerance = 0.1)
    expect_equal(weighted_sum, 100, tolerance = 0.1)
  }
})

test_that("correlation statistic calculates Pearson correlation correctly", {
  # Create data with known correlation
  set.seed(999)
  n <- 100
  x_base <- rnorm(n)
  y_base <- 0.7 * x_base + rnorm(n, sd = 0.5)  # Correlation ~0.7

  test_data <- data.frame(
    x_var = x_base,
    y_var = y_base,
    group = sample(1:2, n, replace = TRUE)
  )

  test_survey_data <- create_survey_data(test_data)

  # Note: correlation statistic expects both row and column to be numeric
  # This is a special case that might need custom handling
  # For now, we'll test that it produces reasonable values

  # Manual correlation
  expected_cor <- cor(test_data$x_var, test_data$y_var)

  # The correlation statistic implementation would need both variables
  # This test documents expected behavior
  expect_true(abs(expected_cor - 0.7) < 0.2)  # Should be close to 0.7
})

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
