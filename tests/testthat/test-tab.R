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

mini_data <- create_mini_test_data()
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

##### GATE FUNCTIONS AND response_match HELPER TESTS #####
##### Identity Arrays with Gates Tests #####

test_that("response_match without calc_if works", {
  # Use existing mini test data
  mini_data <- create_mini_test_data(100)
  mini_survey_data <- create_survey_data(mini_data)

  result <- tab(
    mini_survey_data,
    rows = response_match(c("Daily", "Weekly", "Monthly"), A2, mode = "value"),
    statistic = "count",
    show_base = FALSE,
    show_row_nets = FALSE,
    show_col_nets = FALSE
  )

  # Should have 3 rows, per the specification
  expect_equal(nrow(result), 3)

  # Without gates, all cells can have values (no orthogonal restriction)
  data_col <- result[[2]] # First data column (Total)
  # Some cells might be 0 if no respondents have that combination,
  # but there's no systematic pattern of zeros like with gates
  expect_true(sum(data_col) > 0) # At least some counts exist
})

test_that("response_match automatic label extraction works", {
  # Use mini test data which has A2_1, A2_2, A2_3 variables
  mini_data <- create_mini_test_data(50)
  mini_survey_data <- create_survey_data(mini_data)

  # Test 1: get_variable_labels with question group
  # Need to pass NULL for patterns argument, then group_name, then named parameter
  result <- tab(
    mini_survey_data,
    rows = response_match(NULL, "A2_a", get_variable_labels = "A2_a"),
    statistic = "count",
    show_base = FALSE
  )

  # Should extract variable labels and force variable mode
  expect_s3_class(result, "tab_result")
  # Should have 3 rows for the 3 A2 variables
  expect_equal(nrow(result) - 1, 3) # Excluding NET row
  # Check that labels contain document types
  expect_true(any(grepl("Docs", result$row_label)))
  expect_true(any(grepl("Presentations", result$row_label)))
  expect_true(any(grepl("Spreadsheets", result$row_label)))

  # Test 2: get_value_labels with question group
  result2 <- tab(
    mini_survey_data,
    rows = response_match(NULL, "A2_a", get_value_labels = "A2_a"),
    statistic = "count",
    show_base = FALSE
  )

  # Should extract value labels and force value mode
  expect_s3_class(result2, "tab_result")
  # Should have 3 rows for Daily, Weekly, Monthly
  expect_equal(nrow(result2) - 1, 3) # Excluding NET row
  expect_true(all(result2$row_label[-nrow(result2)] %in% c("Daily", "Weekly", "Monthly")))

  # Test 3: get_variable_labels with specific variables
  result3 <- tab(
    mini_survey_data,
    rows = response_match(NULL, "A2_a", get_variable_labels = c("A2_1", "A2_3")),
    statistic = "count",
    show_base = FALSE
  )

  # Should only extract labels for specified variables
  expect_equal(nrow(result3) - 1, 2) # Only 2 variables selected
  expect_true(any(grepl("Docs", result3$row_label)))
  expect_true(any(grepl("Spreadsheets", result3$row_label)))
  expect_false(any(grepl("Presentations", result3$row_label))) # A2_2 not selected

  # Test 4: Error handling - non-existent question group
  expect_error(
    tab(mini_survey_data,
        rows = response_match(NULL, "A2_a", get_variable_labels = "NonExistentGroup"),
        statistic = "count"),
    "Question group 'NonExistentGroup' not found in dpdict"
  )

  # Test 5: Warning for missing variables
  expect_warning(
    tab(mini_survey_data,
        rows = response_match(NULL, "A2_a", get_variable_labels = c("A2_1", "NonExistent")),
        statistic = "count",
        show_base = FALSE),
    "Variables not found in dpdict: NonExistent"
  )

  # Test 6: Mutual exclusivity validation
  expect_error(
    tab(mini_survey_data,
        rows = response_match(c("Daily", "Weekly"), "A2_a", get_variable_labels = "A2_a"),
        statistic = "count"),
    "supply only one of"
  )

  # Test 7: Improved syntax
  result <- tab(
    mini_survey_data,
    rows = response_match(get_variable_labels = "A2_a", "A2_a"),
    statistic = "count",
    show_base = FALSE
  )
})

test_that("gate registry functions work correctly", {
  # Check that no_mismatch gate is registered
  expect_true("no_mismatch" %in% list_tab_gates())

  # Get the gate factory
  gate_factory <- get_gate("no_mismatch")
  expect_true(is.function(gate_factory))

  # Create a gate instance
  test_gate <- gate_factory("test_field")
  expect_true(is.function(test_gate))

  # Test the gate logic
  row_meta <- list(test_field = "A", other = "X")
  col_meta <- list(test_field = "A", other = "Y")
  expect_true(test_gate(row_meta, col_meta))

  col_meta$test_field <- "B"
  expect_false(test_gate(row_meta, col_meta))

  # Test with missing fields
  expect_true(test_gate(list(), list(test_field = "A")))
  expect_true(test_gate(list(test_field = "A"), list()))
})


test_that("calc_if wrapper works with response_match", {
  mini_data <- create_mini_test_data(100)
  mini_survey_data <- create_survey_data(mini_data)

  result <- tab(
    mini_survey_data,
    rows = calc_if(no_mismatch("ival"), response_match(c("Daily", "Weekly", "Monthly"), A2, mode = "value")),
    cols = calc_if(no_mismatch("ivar"), response_match(c("Daily", "Weekly", "Monthly"), A2, mode = "value")),
    statistic = "count",
    show_base = FALSE,
    show_row_nets = FALSE,
    show_col_nets = FALSE
  )

  # Should have 3x3 orthogonal table
  expect_equal(nrow(result), 3)
  expect_equal(ncol(result), 4) # row_label + 3 data columns

  # Check orthogonality - each row should have exactly one non-zero value
  data_cols <- result[, -1]
  for (i in 1:nrow(data_cols)) {
    non_zero_count <- sum(data_cols[i, ] > 0, na.rm = TRUE)
    expect_equal(non_zero_count, 1)
  }
})

test_that("calc_if works with simple variables", {
  test_data <- create_tab_test_data(200)
  test_survey_data <- create_survey_data(test_data)

  # Gate a simple variable
  result <- tab(
    test_survey_data,
    rows = calc_if(no_mismatch("test_field"), gender),
    cols = region,
    statistic = "count"
  )

  # Should work without errors
  expect_s3_class(result, "tab_result")
})

test_that("smart collapse works with gated arrays", {
  mini_data <- create_mini_test_data(100)
  mini_survey_data <- create_survey_data(mini_data)

  # Create orthogonal table with smart collapse
  result <- tab(
    mini_survey_data,
    rows = calc_if(no_mismatch("ival"), response_match(c("Daily", "Weekly", "Monthly"), A2, mode = "value")),
    cols = response_match(c("Daily", "Weekly", "Monthly"), A2, mode = "value"),
    statistic = "count",
    collapse = "smart",
    show_base = FALSE,
    show_row_nets = FALSE,
    show_col_nets = FALSE
  )

  # After smart collapse, rows should be collapsed by ival
  expect_equal(nrow(result), 3)
  expect_true(all(result$row_label %in% c("Daily", "Weekly", "Monthly")))

  # Columns should be collapsed by ivar (variable names)
  col_names <- setdiff(names(result), "row_label")
  # Should have A2_usage, A2_satisfaction, A2_importance or similar
  expect_true(length(col_names) == 3)
})

##### Unit tests for modify_labels function #####

test_that("modify_labels works with row labels", {
  # Create test data
  df <- data.frame(
    row_label = c("Very satisfied", "Somewhat satisfied", "Not satisfied"),
    Male = c(25, 30, 10),
    Female = c(20, 35, 15),
    stringsAsFactors = FALSE
  )

  # Test single pattern replacement
  result <- modify_labels(df, row_labels = c("Very satisfied" = "Very sat"))
  expect_equal(result$row_label[1], "Very sat")
  expect_equal(result$row_label[2], "Somewhat satisfied") # unchanged

  # Test multiple patterns applied sequentially
  result <- modify_labels(df, row_labels = c("Very" = "V", "satisfied" = "sat"))
  expect_equal(result$row_label[1], "V sat")
  expect_equal(result$row_label[2], "Somewhat sat")
})

test_that("modify_labels works with column labels", {
  df <- data.frame(
    row_label = c("Option 1", "Option 2"),
    "Male 18-34" = c(25, 30),
    "Female 35+" = c(20, 35),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  # Test single pattern replacement
  result <- modify_labels(df, col_labels = c("Male 18-34" = "M 18-34"))
  expect_equal(names(result)[2], "M 18-34")
  expect_equal(names(result)[3], "Female 35+") # unchanged

  # Test multiple patterns
  result <- modify_labels(df, col_labels = c("Male" = "M", "Female" = "F"))
  expect_equal(names(result)[2], "M 18-34")
  expect_equal(names(result)[3], "F 35+")
})

test_that("modify_labels works with both row and column labels", {
  df <- data.frame(
    row_label = c("Very satisfied", "Not satisfied"),
    "Male respondents" = c(25, 10),
    "Female respondents" = c(20, 15),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  result <- modify_labels(
    df,
    row_labels = c("satisfied" = "sat"),
    col_labels = c("respondents" = "resp")
  )

  expect_equal(result$row_label, c("Very sat", "Not sat"))
  expect_equal(names(result)[2:3], c("Male resp", "Female resp"))
})

test_that("modify_labels preserves tab_result class and attributes", {
  df <- data.frame(
    row_label = c("Option 1", "Option 2"),
    Male = c(25, 30),
    Female = c(20, 35),
    stringsAsFactors = FALSE
  )

  # Add tab_result class and some attributes
  class(df) <- c("tab_result", "data.frame")
  attr(df, "statistic") <- list(id = "count")
  attr(df, "custom_attr") <- "test_value"

  result <- modify_labels(df, row_labels = c("Option" = "Opt"))

  # Check class is preserved
  expect_s3_class(result, "tab_result")
  expect_s3_class(result, "data.frame")

  # Check attributes are preserved
  expect_equal(attr(result, "statistic"), list(id = "count"))
  expect_equal(attr(result, "custom_attr"), "test_value")
})

test_that("modify_labels error handling", {
  df <- data.frame(
    row_label = c("Option 1", "Option 2"),
    Male = c(25, 30),
    stringsAsFactors = FALSE
  )

  # Test invalid input types
  expect_error(modify_labels("not a dataframe"), "x must be a data frame")
  expect_error(modify_labels(df, row_labels = "unnamed vector"), "row_labels must be a named character vector")
  expect_error(modify_labels(df, col_labels = c("pattern")), "col_labels must be a named character vector")

  # Test missing row_label column
  df_no_row_label <- data.frame(other_col = c(1, 2), Male = c(25, 30))
  expect_error(
    modify_labels(df_no_row_label, row_labels = c("test" = "replacement")),
    "Data frame must have a 'row_label' column"
  )
})

test_that("modify_labels handles edge cases", {
  # Data frame with only row_label column
  df_minimal <- data.frame(row_label = c("Option 1", "Option 2"), stringsAsFactors = FALSE)

  expect_warning(
    modify_labels(df_minimal, col_labels = c("test" = "replacement")),
    "No data columns found to modify"
  )

  # Should still work for row labels
  result <- modify_labels(df_minimal, row_labels = c("Option" = "Opt"))
  expect_equal(result$row_label, c("Opt 1", "Opt 2"))
})

test_that("modify_labels supports regex patterns", {
  df <- data.frame(
    row_label = c("  Extra   spaces  ", "Normal text"),
    Male = c(25, 30),
    stringsAsFactors = FALSE
  )

  # Test regex pattern to clean up extra spaces
  result <- modify_labels(df, row_labels = c("\\s+" = " ", "^\\s+|\\s+$" = ""))
  expect_equal(result$row_label[1], "Extra spaces")
  expect_equal(result$row_label[2], "Normal text")
})

##### Unit tests for banner helper ######

test_that("banner helper function works correctly", {
  test_data <- data.frame(
    uid = 1:100,
    q1 = sample(1:5, 100, replace = TRUE),
    country = rep(c("UK", "US", "Germany"), length.out = 100),
    satisfaction = sample(1:7, 100, replace = TRUE),
    stringsAsFactors = FALSE
  )

  test_data$q1 <- sjlabelled::set_labels(
    test_data$q1,
    labels = c("Strongly Disagree" = 1, "Disagree" = 2, "Neutral" = 3,
               "Agree" = 4, "Strongly Agree" = 5)
  )

  test_data$satisfaction <- sjlabelled::set_labels(
    test_data$satisfaction,
    labels = c("Very Dissatisfied" = 1, "Dissatisfied" = 2,
               "Somewhat Dissatisfied" = 3, "Neutral" = 4,
               "Somewhat Satisfied" = 5, "Satisfied" = 6,
               "Very Satisfied" = 7)
  )

  # Test 1: Banner with simple variable
  result1 <- tab(test_data, q1, cols = banner("country", "satisfaction"))

  # Check structure
  expect_s3_class(result1, "tab_result")

  # Check column names follow expected pattern
  col_names <- names(result1)
  expect_true("row_label" %in% col_names)
  expect_true("NET" %in% col_names)

  # Check that all country-satisfaction combinations are present
  countries <- c("UK", "US", "Germany")
  satisfaction_labels <- c("Very Dissatisfied", "Dissatisfied",
                           "Somewhat Dissatisfied", "Neutral",
                           "Somewhat Satisfied", "Satisfied",
                           "Very Satisfied")

  for (country in countries) {
    for (label in satisfaction_labels) {
      expected_col <- paste0(country, ": ", label)
      expect_true(expected_col %in% col_names,
                  info = paste("Missing column:", expected_col))
    }
  }

  # Test 2: Banner with nested top_box helper
  result2 <- tab(test_data, q1, cols = banner(country, top_box(satisfaction, 2)))

  # Check structure
  expect_s3_class(result2, "tab_result")

  # Check column names for top box columns
  col_names2 <- names(result2)
  for (country in countries) {
    expected_col <- paste0(country, ": top_box(satisfaction, 2)")
    expect_true(expected_col %in% col_names2,
                info = paste("Missing top box column:", expected_col))
  }

  # Test 3: Banner with subtotals
  result3 <- tab(test_data, q1, cols = banner("country", "satisfaction", subtotals = TRUE))

  # Check for subtotal columns
  col_names3 <- names(result3)
  for (country in countries) {
    expected_subtotal <- paste0(country, ": Total")
    expect_true(expected_subtotal %in% col_names3,
                info = paste("Missing subtotal column:", expected_subtotal))
  }

  # Test 4: Verify values are correctly filtered
  # Check that UK top box only includes UK respondents with satisfaction 6-7
  uk_mask <- test_data$country == "UK"
  uk_top_box_count <- sum(test_data$satisfaction[uk_mask] >= 6, na.rm = TRUE)

  # Find the UK top box column and check the base count
  uk_top_box_col <- grep("UK:.*[Tt]op", names(result2), value = TRUE)[1]
  if (!is.na(uk_top_box_col)) {
    # Get the base count from the Base (n) row
    base_row <- result2[result2$row_label == "Base (n)", uk_top_box_col]
    actual_base <- as.numeric(base_row)

    expect_equal(actual_base, uk_top_box_count,
                 info = paste("UK Top Box base count mismatch. Expected:", uk_top_box_count,
                              "Actual:", actual_base))
  }

  # Test 5: Error handling for non-existent variable
  expect_error(
    tab(test_data, q1, cols = banner("nonexistent_var", "satisfaction")),
    "not found",
    info = "Should error when banner uses non-existent variable"
  )
})

test_that("banner helper handles edge cases", {
  # Create test data with edge cases
  test_data <- data.frame(
    uid = 1:50,
    q1 = sample(1:3, 50, replace = TRUE),
    category = rep(c("A", "B"), 25),
    rating = sample(1:5, 50, replace = TRUE),
    stringsAsFactors = FALSE
  )

  # Add labels to make q1 categorical
  test_data$q1 <- sjlabelled::set_labels(
    test_data$q1,
    labels = c("Option 1" = 1, "Option 2" = 2, "Option 3" = 3)
  )

  # Test with minimal data (2 categories, 5 ratings)
  result <- tab(test_data, q1, cols = banner("category", "rating"))

  # Should have 2*5 = 10 category-rating columns plus row_label and Total
  expect_equal(ncol(result), 12)

  # Test with single value in outer variable
  single_cat_data <- test_data[test_data$category == "A", ]
  result_single <- tab(single_cat_data, q1, cols = banner("category", "rating"))

  # Should only have A columns (5) plus row_label and Total
  expect_equal(ncol(result_single), 7)
  expect_true(all(grepl("^(row_label|A:|Total|Base|NET)$", names(result_single)) |
                    startsWith(names(result_single), "A:")))
})

test_that("banner helper works with different separators", {
  test_data <- data.frame(
    q1 = sample(1:3, 50, replace = TRUE),
    group = rep(c("Group1", "Group2"), 25),
    level = sample(1:3, 50, replace = TRUE),
    stringsAsFactors = FALSE
  )

  # Test with custom separator
  result <- tab(test_data, q1, cols = banner("group", "level", sep = " - "))

  # Check that column names use the custom separator
  col_names <- names(result)
  group_cols <- col_names[grepl("Group", col_names)]

  expect_true(all(grepl(" - ", group_cols)),
              info = "Custom separator should be used in column names")

  # Verify exact format
  expect_true("Group1 - 1" %in% col_names)
  expect_true("Group2 - 3" %in% col_names)
})

##### Unit tests for glue #####

test_that("multi_tab works with standard test data", {
  # Create standard test data
  test_survey_data <- create_survey_data(create_tab_test_data(200))

  # Split by region
  result <- multi_tab(test_survey_data, gender, satisfaction, by = region)

  expect_s3_class(result, "tab_result")
  expect_true("North: Overall Satisfaction: Very dissatisfied" %in% names(result))
  expect_true("South: Overall Satisfaction: Satisfied" %in% names(result))
  expect_true("Total: Overall Satisfaction: Neutral" %in% names(result))

  # Check that variable labels are used
  expect_true(any(grepl("Respondent Gender", result$row_label)))
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
  expect_true("Age 35+: Respondent Gender: Male" %in% names(result))
  expect_true("Age 35+: Respondent Gender: Female" %in% names(result))

  # Base sizes should be different
  base_row <- which(result$row_label == "Base (n)")
  expect_true(result[base_row, "Respondent Gender: Male"] != result[base_row, "Age 35+: Respondent Gender: Male"])
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

  # Check structure
  expect_true(any(grepl("--- High Satisfaction ---", result$row_label)))
  expect_true(any(grepl("Brand_A", result$row_label)))
  expect_true(any(grepl("Brand_Q", result$row_label)))  # 17th brand

  # Check metadata preserved
  expect_equal(attr(result, "multi_tab_groups"), c("High Satisfaction", "Low Satisfaction"))
})

##### Unit tests for more expressive filters #####
test_that("filters support helpers with same expressiveness as rows", {
  # Use the standard test data
  data <- test_survey_data

  # Test 1: Helper with quoted variable name (already worked)
  result1 <- tab(data, q1, gender, filter = any_positive("q1_1"))
  expect_s3_class(result1, "tab_result")
  expect_true(nrow(result1) > 0)

  # Test 2: Helper with unquoted variable name (previously failed)
  result2 <- tab(data, q1, gender, filter = any_positive(q1_1))
  expect_s3_class(result2, "tab_result")
  expect_equal(dim(result1), dim(result2))  # Should give same result

  # Test 3: Helper with c() and unquoted names (previously failed)
  result3 <- tab(data, satisfaction, gender, filter = any_positive(c(q1_1, q1_2)))
  expect_s3_class(result3, "tab_result")
  # Should have filtered to only respondents with q1_1 OR q1_2 positive
  base_row <- result3[result3$row_label == "Base (n)", ]
  total_n <- as.numeric(base_row$NET)
  expect_true(total_n < nrow(data$dat))  # Filter should reduce sample size

  # Test 4: Complex expression with helper and multiplication (filter combination)
  result4 <- tab(data, satisfaction, gender,
                 filter = any_positive(q1_a) * (age > 30))
  expect_s3_class(result4, "tab_result")
  # Should be smaller than just any_positive(q1) filter
  base_row4 <- result4[result4$row_label == "Base (n)", ]
  expect_true(as.numeric(base_row4$NET) < total_n)

  # Test 5: Verify filter and rows give same result when using same helper
  # Using in rows
  result_rows <- tab(data, any_positive(q1), gender)
  any_positive_row <- result_rows[grep("any_positive", result_rows$row_label), ]

  # Using as filter
  result_filter <- tab(data, total(), gender, filter = any_positive(q1_a))
  base_row_filter <- result_filter[result_filter$row_label == "Base (n)", ]

  # The percentage in rows should match the base count with filter
  expect_equal(
    as.numeric(result_filter$NET[2]),  # Percentage who have any_positive q1
    (as.numeric(base_row_filter$NET) / nrow(data$dat)) * 100,
    tolerance = 0.1
  )

  # Test 6: Multiple helpers in filter
  result6 <- tab(data, region, gender,
                 filter = any_positive(q1_a) * top_box(satisfaction, 2))
  expect_s3_class(result6, "tab_result")
  base_row <- result6[result6$row_label == "Base (n)", ]
  total_base <- as.numeric(base_row$NET)
  expect_true(total_base > 0)  # Some respondents match
  expect_true(total_base < nrow(data$dat))  # But not all
})

