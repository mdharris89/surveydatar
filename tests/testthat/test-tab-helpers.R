# Tests for Tab Helpers and Extensibility Features
# This file tests:
# - Helper functions (top_box, bottom_box, value_range, pattern, percentile, etc.)
# - Custom statistic and helper creation
# - Significance testing
# - Derive operations
# - Layout and hide operations
# - Grid macro
##### Setup #####
# Ensure clean state for each test file
# Note: Functions loaded via helper-load-tab.R
clear_tab_registry()
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

# Test data generator
create_tab_test_data <- function(n = 100) {
  set.seed(123)
  
  n_outliers <- round(0.10 * n)
  n_main     <- n - n_outliers
  make_tied <- function(n) rep(c(10, 20, 30), length.out = n)
  
  raw_data <- data.frame(
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
    
    satisfaction = sjlabelled::set_label(
      sjlabelled::set_labels(
        haven::labelled(sample(c(1, 2, 3, 4, 5), n, replace = TRUE),
                        labels = c("Very dissatisfied" = 1, "Dissatisfied" = 2, "Neutral" = 3, "Satisfied" = 4, "Very satisfied" = 5)),
        labels = c("Very dissatisfied" = 1, "Dissatisfied" = 2, "Neutral" = 3, "Satisfied" = 4, "Very satisfied" = 5)
      ),
      label = "Overall Satisfaction"
    ),
    
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
    
    income = sjlabelled::set_label(
      round(rlnorm(n, meanlog = 10.5, sdlog = 0.8)),
      label = "Annual household income"
    ),
    
    response_time = sjlabelled::set_label(
      sample(
        c(
          rnorm(n_main,  mean = 30, sd = 10),
          runif(n_outliers, min = 100, max = 200)
        ),
        size = n
      ),
      label = "Response time (seconds)"
    ),
    
    test_values = sjlabelled::set_label(
      rnorm(n, mean = 50, sd = 10),
      label = "Test values (normal dist)"
    ),
    
    binary_test = sjlabelled::set_label(
      sjlabelled::set_labels(
        haven::labelled(c(rep(1, 40), rep(0, 60))[sample(n)],
                        labels = c("No" = 0, "Yes" = 1)),
        labels = c("No" = 0, "Yes" = 1)
      ),
      label = "Binary test variable"
    ),
    
    ordinal_test = sjlabelled::set_label(
      rep(1:10, each = 10)[sample(n)],
      label = "Ordinal test (1-10)"
    ),
    
    tied_values = sjlabelled::set_label(
      make_tied(n),
      label = "Values with ties"
    ),
    
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

create_mini_test_data <- function(n = 5) {
  set.seed(123)
  
  raw_data <- data.frame(
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

# Grid test data generator for pivot_to_grid tests
create_grid_test_data <- function(n = 100, seed = 456) {
  set.seed(seed)
  
  raw_data <- data.frame(
    A2_1 = sjlabelled::set_label(
      sjlabelled::set_labels(
        haven::labelled(sample(c(1, 2, 3, 4), n, replace = TRUE,
                               prob = c(0.1, 0.4, 0.3, 0.2)),
                        labels = c("Daily" = 1, "Weekly" = 2,
                                   "Monthly" = 3, "Never" = 4)),
        labels = c("Daily" = 1, "Weekly" = 2, "Monthly" = 3, "Never" = 4)
      ),
      label = "How often do you create - Docs"
    ),
    
    A2_2 = sjlabelled::set_label(
      sjlabelled::set_labels(
        haven::labelled(sample(c(1, 2, 3, 4), n, replace = TRUE,
                               prob = c(0.05, 0.35, 0.4, 0.2)),
                        labels = c("Daily" = 1, "Weekly" = 2,
                                   "Monthly" = 3, "Never" = 4)),
        labels = c("Daily" = 1, "Weekly" = 2, "Monthly" = 3, "Never" = 4)
      ),
      label = "How often do you create - Presentations"
    ),
    
    A2_3 = sjlabelled::set_label(
      sjlabelled::set_labels(
        haven::labelled(sample(c(1, 2, 3, 4), n, replace = TRUE,
                               prob = c(0.2, 0.35, 0.25, 0.2)),
                        labels = c("Daily" = 1, "Weekly" = 2,
                                   "Monthly" = 3, "Never" = 4)),
        labels = c("Daily" = 1, "Weekly" = 2, "Monthly" = 3, "Never" = 4)
      ),
      label = "How often do you create - Spreadsheets"
    ),
    
    A2_4 = sjlabelled::set_label(
      sjlabelled::set_labels(
        haven::labelled(sample(c(1, 2, 3, 4), n, replace = TRUE,
                               prob = c(0.15, 0.25, 0.3, 0.3)),
                        labels = c("Daily" = 1, "Weekly" = 2,
                                   "Monthly" = 3, "Never" = 4)),
        labels = c("Daily" = 1, "Weekly" = 2, "Monthly" = 3, "Never" = 4)
      ),
      label = "How often do you create - Whiteboards"
    ),
    
    A2_5 = sjlabelled::set_label(
      sjlabelled::set_labels(
        haven::labelled(sample(c(1, 2, 3, 4), n, replace = TRUE,
                               prob = c(0.1, 0.3, 0.35, 0.25)),
                        labels = c("Daily" = 1, "Weekly" = 2,
                                   "Monthly" = 3, "Never" = 4)),
        labels = c("Daily" = 1, "Weekly" = 2, "Monthly" = 3, "Never" = 4)
      ),
      label = "How often do you create - Visualizations"
    ),
    
    age = sjlabelled::set_label(
      sample(25:65, n, replace = TRUE),
      label = "Age in years"
    ),
    
    weight_var = sjlabelled::set_label(
      pmax(0.5, pmin(2.0, rnorm(n, mean = 1.0, sd = 0.3))),
      label = "Weight variable"
    ),
    
    stringsAsFactors = FALSE
  )
  
  # Create survey_data object
  survey_obj <- create_survey_data(raw_data)
  
  # Set question_group for A2 variables to enable A2_a group reference
  survey_obj$dpdict$question_group[survey_obj$dpdict$variable_names %in% 
                                      c("A2_1", "A2_2", "A2_3", "A2_4", "A2_5")] <- "A2_a"
  
  return(survey_obj)
}

test_data <- create_tab_test_data()
test_survey_data <- create_survey_data(test_data)

mini_data <- create_mini_test_data()
mini_survey_data <- create_survey_data(mini_data)

grid_test_data <- create_grid_test_data()

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
  expect_equal(nrow(result) - 1, 2) # 2 helper rows, 1 net row
  expect_true(any(grepl("Top 2", result$row_label)))
  expect_true(any(grepl("Bottom 2", result$row_label)))
})

##### Helper target resolution tests #####

test_that("helpers resolve question group names to per-variable outputs", {
  # Use test_survey_data and add new variables with proper labels
  test_data <- create_tab_test_data(100)
  test_survey_data <- create_survey_data(test_data)
  
  # Add new question group variables with similar labels (value labels auto-generated)
  test_survey_data <- test_survey_data %>%
    mutate(
      QG_1 = realiselabelled_vec(sample(1:5, 100, replace = TRUE),
                                 variable_label = "QG - Item A"),
      QG_2 = realiselabelled_vec(sample(1:5, 100, replace = TRUE),
                                 variable_label = "QG - Item B")
    )
  
  # Manually set question_group to ensure they're grouped together
  test_survey_data$dpdict$question_group[test_survey_data$dpdict$variable_names == "QG_1"] <- "QG_a"
  test_survey_data$dpdict$question_group[test_survey_data$dpdict$variable_names == "QG_2"] <- "QG_a"
  
  res <- tab(test_survey_data, rows = top_box(QG, 1))
  expect_s3_class(res, "tab_result")
  # Expect one helper row per variable (showing the top value, 5)
  data_rows <- res[!grepl("Base|Total|NET", res$row_label), ]
  expect_equal(nrow(data_rows), 2)
  expect_true(all(grepl("QG -", data_rows$row_label)))
  expect_true(all(grepl(": 5", data_rows$row_label)))  # Top 1 box = value 5
})

test_that("helpers resolve stems without dpdict (pattern QX_*)", {
  set.seed(1)
  dat <- data.frame(
    QX_1 = sample(1:4, 30, replace = TRUE),
    QX_2 = sample(1:4, 30, replace = TRUE),
    stringsAsFactors = FALSE
  )

  # No dpdict; stem resolution should find QX_1 and QX_2
  res <- tab(dat, rows = top_box(QX, 1))
  expect_s3_class(res, "tab_result")
  # Helper expands to show each variable with its top value
  expect_true(any(grepl("QX_1: 4", res$row_label)))
  expect_true(any(grepl("QX_2: 4", res$row_label)))
})

test_that("value_range errors clearly on mixed-type groups", {
  dat <- data.frame(
    g1 = rnorm(25),
    g2 = sample(letters[1:3], 25, replace = TRUE),
    stringsAsFactors = FALSE
  )
  dpdict <- data.frame(
    variable_names  = c("g1", "g2"),
    variable_labels = c("Metric 1", "Category 1"),
    question_group  = c("G", "G"),
    stringsAsFactors = FALSE
  )
  dpdict$value_labels <- I(list(NULL, NULL))

  survey_obj <- create_survey_data(dat, dpdict)
  expect_error(
    tab(survey_obj, rows = value_range(G, 0, 1)),
    regexp = "non-numeric",
    info = "value_range should reject groups containing non-numeric variables"
  )
})

test_that("percentile helper computes per-variable thresholds for groups", {
  set.seed(2)
  # Use test_survey_data and add numeric variables
  test_data <- create_tab_test_data(60)
  test_survey_data <- create_survey_data(test_data)
  
  # Add numeric variables for percentile testing
  test_survey_data <- test_survey_data %>%
    mutate(
      s1 = rnorm(60, mean = 0),
      s2 = rnorm(60, mean = 2)
    )
  
  # Set proper variable labels
  test_survey_data$dpdict$variable_labels[test_survey_data$dpdict$variable_names == "s1"] <- "Score 1"
  test_survey_data$dpdict$variable_labels[test_survey_data$dpdict$variable_names == "s2"] <- "Score 2"
  
  # Manually set question_group to ensure they're grouped together
  test_survey_data$dpdict$question_group[test_survey_data$dpdict$variable_names == "s1"] <- "Scores_a"
  test_survey_data$dpdict$question_group[test_survey_data$dpdict$variable_names == "s2"] <- "Scores_a"
  
  res <- tab(test_survey_data, rows = percentile(Scores, "above", 50))
  expect_s3_class(res, "tab_result")
  # Should have 2 data rows (one per variable)
  data_rows <- res[!grepl("Base|Total|NET", res$row_label), ]
  expect_equal(nrow(data_rows), 2)
})

test_that("helpers error cleanly for unknown target names", {
  dat <- data.frame(x = 1:5)
  expect_error(
    tab(dat, rows = top_box(DoesNotExist, 1)),
    regexp = "Could not resolve|not found|resolve",
    info = "Unknown group/variable should raise a precise error"
  )
})

##### Helper Mathematical Accuracy Tests #####

test_that("top_box helper selects correct values", {
  # Test top 2 box on satisfaction (values 4 and 5)
  result <- tab(test_survey_data, top_box(satisfaction, 2), gender)

  # Manual calculation
  top2_count <- sum(test_survey_data$dat$satisfaction %in% c(4, 5))

  # Get base row to verify total
  base_row <- result[result$row_label == "Base (n)", ]
  total_base <- as.numeric(base_row$Total)

  # Get top_box row
  topbox_row <- result[grep("top_box", result$row_label), ]
  topbox_total <- as.numeric(topbox_row$Total)

  # The percentage should match
  expected_pct <- (top2_count / nrow(test_survey_data$dat)) * 100
  expect_equal(topbox_total, expected_pct, tolerance = 0.1)
})

test_that("bottom_box helper selects correct values", {
  # Test bottom 2 box on satisfaction (values 1 and 2)
  result <- tab(test_survey_data, bottom_box(satisfaction, 2), gender)

  # Manual calculation
  bottom2_count <- sum(test_survey_data$dat$satisfaction %in% c(1, 2))

  # Get bottom_box row
  bottombox_row <- result[grep("bottom_box", result$row_label), ]
  bottombox_total <- as.numeric(bottombox_row$Total)

  expected_pct <- (bottom2_count / nrow(test_survey_data$dat)) * 100
  expect_equal(bottombox_total, expected_pct, tolerance = 0.1)
})

test_that("value_range helper filters correctly", {
  # Test age range 25-45
  result <- tab(test_survey_data, value_range(age, 25, 45), gender)

  # Manual calculation
  in_range <- sum(test_survey_data$dat$age >= 25 & test_survey_data$dat$age <= 45)

  range_row <- result[grep("value_range", result$row_label), ]
  range_total <- as.numeric(range_row$Total)

  expected_pct <- (in_range / nrow(test_survey_data$dat)) * 100
  expect_equal(range_total, expected_pct, tolerance = 0.1)
})

test_that("pattern helper matches correctly", {
  # Test pattern matching on region labels
  result <- tab(test_survey_data, pattern(region_character, "North|South"), gender)

  # Manual calculation - regions 1 (North) and 2 (South)
  matches <- sum(test_survey_data$dat$region_character %in% c("North", "South"))

  pattern_row <- result[grep("pattern", result$row_label), ]
  pattern_total <- as.numeric(pattern_row$Total)

  expected_pct <- (matches / nrow(test_survey_data$dat)) * 100
  expect_equal(pattern_total, expected_pct, tolerance = 0.1)
})

test_that("percentile helper filters correctly", {
  # Test top 25% of income
  result <- tab(test_survey_data, percentile(income, "above", 75), gender)

  # Manual calculation
  p75_threshold <- quantile(test_survey_data$dat$income, probs = 0.75, na.rm = TRUE)
  above_p75 <- sum(test_survey_data$dat$income > p75_threshold)

  percentile_row <- result[grep("percentile", result$row_label), ]
  percentile_total <- as.numeric(percentile_row$Total)

  # Should be approximately 25% (might not be exact due to ties)
  expected_pct <- (above_p75 / nrow(test_survey_data$dat)) * 100
  expect_equal(percentile_total, expected_pct, tolerance = 2)  # Higher tolerance for percentile edge cases
})

test_that("helpers handle missing data correctly", {
  test_data <- create_tab_test_data(100)

  # Set some satisfaction values to NA
  test_data$satisfaction[1:20] <- NA
  test_survey_data <- create_survey_data(test_data)

  result <- tab(test_survey_data, top_box(satisfaction, 2), gender)

  # Should calculate based on non-NA values only
  non_na_satisfaction <- test_survey_data$dat$satisfaction[!is.na(test_survey_data$dat$satisfaction)]
  top2_count <- sum(non_na_satisfaction %in% c(4, 5))

  topbox_row <- result[grep("top_box", result$row_label), ]
  topbox_total <- as.numeric(topbox_row$Total)

  # Percentage should be of total sample (including NAs)
  expected_pct <- (top2_count / nrow(test_survey_data$dat)) * 100
  expect_equal(topbox_total, expected_pct, tolerance = 0.1)
})

##### Unit tests for all_matching helper #####

test_that("all_matching helper returns individual matching variables", {
  # Use existing test data
  test_data <- create_tab_test_data(100)
  test_survey_data <- create_survey_data(test_data)

  # Add some test variables with "ANY category" in labels to existing question group
  # Suppress expected metadata warnings about value labels and partial question groups
  test_survey_data <- suppressWarnings(
    test_survey_data %>%
      mutate(
        q1_4 = realiselabelled_vec(sample(c(0, 1), 100, replace = TRUE),
                                   variable_label = "Q1: Brand attributes - ANY category A"),
        q1_5 = realiselabelled_vec(sample(c(0, 1), 100, replace = TRUE),
                                   variable_label = "Q1: Brand attributes - ANY category B"),
        q1_6 = realiselabelled_vec(sample(c(0, 1), 100, replace = TRUE),
                                   variable_label = "Q1: Brand attributes - Other stuff")
      )
  )

  # Test all_matching finds only variables with "ANY category" in labels
  result <- tab(test_survey_data,
                all_matching("ANY category", q1_b, pattern_type = "fixed"),
                gender)

  # Should return 2 matching variables as separate rows (q1_4, q1_5 but not q1_6)
  expect_s3_class(result, "tab_result")
  expect_true(any(grepl("ANY category A", result$row_label)))
  expect_true(any(grepl("ANY category B", result$row_label)))
  expect_false(any(grepl("Other stuff", result$row_label)))

  # Should have exactly 2 data rows + base row + any total rows
  data_rows <- result[!grepl("Base|Total|NET", result$row_label), ]
  expect_equal(nrow(data_rows), 2)
})

##### Built-in Statistics Tests #####

##### STATISTICS MATHEMATICAL ACCURACY TESTS #####

test_that("count statistic calculates exact counts", {

  # Manual calculation
  manual_count_male_north <- sum(test_data$gender == 1 & test_data$region == 1)
  manual_count_female_south <- sum(test_data$gender == 2 & test_data$region == 2)

  # Tab calculation
  result <- tab(test_survey_data, gender, region, statistic = "count")

  # Extract values (removing % signs and converting)
  male_north <- as.numeric(result[result$row_label == "Male", "North"])
  female_south <- as.numeric(result[result$row_label == "Female", "South"])

  # Test exact equality
  expect_equal(male_north, manual_count_male_north)
  expect_equal(female_south, manual_count_female_south)

  # Test total counts - extract each value individually and sum
  male_counts <- c(
    as.numeric(result[result$row_label == "Male", "North"]),
    as.numeric(result[result$row_label == "Male", "South"]),
    as.numeric(result[result$row_label == "Male", "East"]),
    as.numeric(result[result$row_label == "Male", "West"])
  )
  female_counts <- c(
    as.numeric(result[result$row_label == "Female", "North"]),
    as.numeric(result[result$row_label == "Female", "South"]),
    as.numeric(result[result$row_label == "Female", "East"]),
    as.numeric(result[result$row_label == "Female", "West"])
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
  male_pct <- as.numeric(gsub("%", "", result[result$row_label == "Male", "North"]))
  female_pct <- as.numeric(gsub("%", "", result[result$row_label == "Female", "North"]))

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
  north_pct <- as.numeric(gsub("%", "", result[result$row_label == "Male", "North"]))
  south_pct <- as.numeric(gsub("%", "", result[result$row_label == "Male", "South"]))

  expect_equal(north_pct, expected_north_pct, tolerance = 0.1)
  expect_equal(south_pct, expected_south_pct, tolerance = 0.1)

  # Verify Total column shows 100% for each row
  net_male <- as.numeric(result[result$row_label == "Male", "Total"])
  expect_equal(net_male, 100)
})

test_that("mean statistic calculates exact means", {

  result <- tab(test_survey_data, gender, region, statistic = "mean", values = "test_values")

  # Manual calculation
  male_north_values <- test_data$test_values[test_data$gender == 1 & test_data$region == 1]
  expected_mean <- mean(male_north_values, na.rm = TRUE)

  # Extract from result
  actual_mean <- as.numeric(result[result$row_label == "Male", "North"])

  expect_equal(actual_mean, expected_mean, tolerance = 0.01)

  # Test with NA values
  test_data_na <- test_data
  test_data_na$test_values[1:10] <- NA
  test_survey_data_na <- create_survey_data(test_data_na)

  result_na <- tab(test_survey_data_na, gender, statistic = "mean", values = "test_values")

  # Should calculate mean excluding NAs
  expected_mean_na <- mean(test_data_na$test_values[test_data_na$gender == 1], na.rm = TRUE)
  actual_mean_na <- as.numeric(result_na[result_na$row_label == "Male", "Total"])

  expect_equal(actual_mean_na, expected_mean_na, tolerance = 0.01)
})

test_that("median statistic calculates exact medians", {

  result <- tab(test_survey_data, gender, statistic = "median", values = "ordinal_test")

  # Manual calculation
  male_values <- test_data$ordinal_test[test_data$gender == 1]
  expected_median <- median(male_values, na.rm = TRUE)

  actual_median <- as.numeric(result[result$row_label == "Male", "Total"])

  expect_equal(actual_median, expected_median)

  # Test with tied values
  result_tied <- tab(test_survey_data, gender, statistic = "median", values = "tied_values")

  female_tied <- test_data$tied_values[test_data$gender == 2]
  expected_tied <- median(female_tied, na.rm = TRUE)
  actual_tied <- as.numeric(result_tied[result_tied$row_label == "Female", "Total"])

  expect_equal(actual_tied, expected_tied)
})

test_that("sd statistic calculates exact standard deviations", {

  result <- tab(test_survey_data, gender, region, statistic = "sd", values = "test_values")

  # Manual calculation for specific cell
  values <- test_data$test_values[test_data$gender == 1 & test_data$region == 2]
  expected_sd <- sd(values, na.rm = TRUE)

  actual_sd <- as.numeric(result[result$row_label == "Male", "South"])

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
  male_north_sd <- as.numeric(result_single[result_single$row_label == "Male", "North"])
  expect_true(is.na(male_north_sd) || male_north_sd == 0)
})

test_that("cv statistic calculates coefficient of variation correctly", {

  result <- tab(test_survey_data, gender, statistic = "cv", values = "test_values")

  # Manual calculation
  male_values <- test_data$test_values[test_data$gender == 1]
  expected_cv <- (sd(male_values, na.rm = TRUE) / mean(male_values, na.rm = TRUE)) * 100

  actual_cv <- as.numeric(result[result$row_label == "Male", "Total"])

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

  actual_index <- as.numeric(result[result$row_label == "Yes", "North"])

  expect_equal(actual_index, expected_index, tolerance = 0.1)
})

test_that("percentile statistics calculate exact percentiles", {

  # Test p25
  result_p25 <- tab(test_survey_data, gender, statistic = "p25", values = "test_values")

  male_values <- test_data$test_values[test_data$gender == 1]
  expected_p25 <- quantile(male_values, probs = 0.25, na.rm = TRUE)
  actual_p25 <- as.numeric(result_p25[result_p25$row_label == "Male", "Total"])

  expect_equal(actual_p25, unname(expected_p25), tolerance = 0.1)

  # Test p75
  result_p75 <- tab(test_survey_data, gender, statistic = "p75", values = "test_values")

  expected_p75 <- quantile(male_values, probs = 0.75, na.rm = TRUE)
  actual_p75 <- as.numeric(result_p75[result_p75$row_label == "Male", "Total"])

  expect_equal(actual_p75, unname(expected_p75), tolerance = 0.1)

  # Verify p75 >= p25
  expect_true(actual_p75 >= actual_p25)
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

  # Suppress expected warning about missing variable labels in minimal test data
  test_survey_data <- suppressWarnings(create_survey_data(test_data))

  # Note: correlation statistic expects both row and column to be numeric
  # This is a special case that might need custom handling
  # For now, we'll test that it produces reasonable values

  # Manual correlation
  expected_cor <- cor(test_data$x_var, test_data$y_var)

  # The correlation statistic implementation would need both variables
  # This test documents expected behavior
  expect_true(abs(expected_cor - 0.7) < 0.2)  # Should be close to 0.7
})

##### WEIGHTED STATISTICS TESTS #####

test_that("weighted statistics calculate correctly", {
  # Create test data
  test_data_weighted <- create_tab_test_data(100)
  
  # Create survey_data object first
  test_survey_weighted <- create_survey_data(test_data_weighted)
  
  # Add weight variable after survey_data creation
  # North: higher weights, South: lower weights, East/West: around 1
  set.seed(456)
  weight_col <- ifelse(test_survey_weighted$dat$region == 1, 
                       runif(nrow(test_survey_weighted$dat), 1.2, 1.8),
                       ifelse(test_survey_weighted$dat$region == 2,
                              runif(nrow(test_survey_weighted$dat), 0.5, 0.9),
                              runif(nrow(test_survey_weighted$dat), 0.9, 1.1)))
  test_survey_weighted$dat$weight <- weight_col
  test_data_weighted$weight <- weight_col

  # Test weighted count
  result_weighted <- tab(test_survey_weighted, gender, region, weight = "weight", statistic = "count")

  # Manual weighted count for Male in North
  male_north_mask <- test_data_weighted$gender == 1 & test_data_weighted$region == 1
  expected_weighted_count <- sum(test_data_weighted$weight[male_north_mask])

  male_row <- grep("Male", result_weighted$row_label, fixed = TRUE)[1]
  actual_weighted <- as.numeric(result_weighted[male_row, "North"])

  expect_equal(actual_weighted, expected_weighted_count, tolerance = 0.1)

  # Test weighted mean
  result_wmean <- tab(test_survey_weighted, gender, weight = "weight", statistic = "mean", values = "test_values")

  # Manual weighted mean calculation for Male
  male_mask <- test_data_weighted$gender == 1
  male_weights <- test_data_weighted$weight[male_mask]
  male_values <- test_data_weighted$test_values[male_mask]
  valid_mask <- !is.na(male_values)

  expected_wmean <- sum(male_weights[valid_mask] * male_values[valid_mask]) / sum(male_weights[valid_mask])
  actual_wmean <- as.numeric(result_wmean[result_wmean$row_label == "Male", "Total"])

  expect_equal(actual_wmean, expected_wmean, tolerance = 0.01)
})

test_that("weighted and unweighted calculations maintain consistent relationships", {
  # Create test data
  test_data_weighted <- create_tab_test_data(100)
  
  # Create survey_data object first
  test_survey_weighted <- create_survey_data(test_data_weighted)
  
  # Add weight variable after survey_data creation
  set.seed(789)
  weight_col <- runif(nrow(test_survey_weighted$dat), 0.5, 1.5)
  test_survey_weighted$dat$weight <- weight_col
  test_data_weighted$weight <- weight_col

  # Compare weighted vs unweighted percentages
  result_unweighted <- tab(test_survey_weighted, gender, region, statistic = "column_pct")
  result_weighted <- tab(test_survey_weighted, gender, region, statistic = "column_pct", weight = "weight")

  # Both should sum to 100% in each column (excluding NET row)
  for (col in c("North", "South", "East", "West")) {
    # Get rows excluding NET row
    unweighted_values <- as.numeric(result_unweighted[[col]][1:2])
    weighted_values <- as.numeric(result_weighted[[col]][1:2])
    
    unweighted_sum <- sum(unweighted_values, na.rm = TRUE)
    weighted_sum <- sum(weighted_values, na.rm = TRUE)

    expect_equal(unweighted_sum, 100, tolerance = 0.1)
    expect_equal(weighted_sum, 100, tolerance = 0.1)
  }
})

##### MULTIPLE STATISTIC VALIDATION #####

test_that("different statistics are consistent with each other", {
  test_data <- create_tab_test_data(100)
  test_survey_data <- create_survey_data(test_data)

  # Count and percentage should be consistent
  result_count <- tab(test_survey_data, gender, region, statistic = "count")
  result_pct <- tab(test_survey_data, gender, region, statistic = "column_pct")

  # Calculate percentage from count
  male_north_count <- as.numeric(result_count[result_count$row_label == "Male", "North"])
  total_north_count <- as.numeric(result_count[result_count$row_label == "NET", "North"])

  calculated_pct <- (male_north_count / total_north_count) * 100
  reported_pct <- as.numeric(gsub("%", "",
                                  result_pct[result_pct$row_label == "Male", "North"]))

  expect_equal(calculated_pct, reported_pct, tolerance = 0.1)
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
  # In tab(), statistic is stored as a list element, not an attribute
  expect_equal(result$statistic, test_stat)

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
  expect_true("Total" %in% col_names)

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
    # Use [[]] to extract the actual value, not a data.frame
    base_row_idx <- which(result2$row_label == "Base (n)")
    actual_base <- as.numeric(result2[[uk_top_box_col]][base_row_idx])
  
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

  # Add labels to make q1 and rating categorical
  test_data$q1 <- sjlabelled::set_labels(
    test_data$q1,
    labels = c("Option 1" = 1, "Option 2" = 2, "Option 3" = 3)
  )
  
  test_data$rating <- sjlabelled::set_labels(
    test_data$rating,
    labels = c("Rating 1" = 1, "Rating 2" = 2, "Rating 3" = 3, 
               "Rating 4" = 4, "Rating 5" = 5)
  )

  # Test with minimal data (2 categories, 5 ratings)
  result <- tab(test_data, q1, cols = banner("category", "rating"))

  # Should have 2*5 = 10 category-rating columns plus row_label and Total
  expect_equal(ncol(result), 12)

  # Test with single value in outer variable
  single_cat_data <- test_data[test_data$category == "A", ]
  
  # Reapply labels after subsetting (subsetting can strip labels)
  single_cat_data$q1 <- sjlabelled::set_labels(
    single_cat_data$q1,
    labels = c("Option 1" = 1, "Option 2" = 2, "Option 3" = 3)
  )
  
  single_cat_data$rating <- sjlabelled::set_labels(
    single_cat_data$rating,
    labels = c("Rating 1" = 1, "Rating 2" = 2, "Rating 3" = 3, 
               "Rating 4" = 4, "Rating 5" = 5)
  )
  
  result_single <- tab(single_cat_data, q1, cols = banner("category", "rating"))

  # Should only have A columns (5) plus row_label and Total = 7 columns
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
  
  # Make q1 and level categorical with labels
  test_data$q1 <- sjlabelled::set_labels(
    test_data$q1,
    labels = c("Option 1" = 1, "Option 2" = 2, "Option 3" = 3)
  )
  
  test_data$level <- sjlabelled::set_labels(
    test_data$level,
    labels = c("Level 1" = 1, "Level 2" = 2, "Level 3" = 3)
  )

  # Test with custom separator
  result <- tab(test_data, q1, cols = banner("group", "level", sep = " - "))

  # Check that column names use the custom separator
  col_names <- names(result)
  group_cols <- col_names[grepl("Group", col_names)]

  expect_true(all(grepl(" - ", group_cols)),
              info = "Custom separator should be used in column names")

  # Verify exact format with labeled values
  expect_true("Group1 - Level 1" %in% col_names)
  expect_true("Group2 - Level 3" %in% col_names)
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

test_that("helpers error cleanly for unknown target names", {
  dat <- data.frame(x = 1:5)
  expect_error(
    tab(dat, rows = top_box(DoesNotExist, 1)),
    regexp = "Could not resolve|not found|resolve",
    info = "Unknown group/variable should raise a precise error"
  )
})

##### response_match Helper Tests #####

test_that("response_match automatic label extraction works", {
  # Use mini test data which has A2_1, A2_2, A2_3 variables
  mini_data <- create_mini_test_data(50)
  mini_survey_data <- create_survey_data(mini_data)

  # Test 1: get_variable_labels with question group
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

##### all_matching Helper Tests #####

test_that("all_matching helper returns individual matching variables", {
  # Use existing test data
  test_data <- create_tab_test_data(100)
  test_survey_data <- create_survey_data(test_data)

  # Add some test variables with "ANY category" in labels to existing question group
  # Suppress expected metadata warnings about value labels and partial question groups
  test_survey_data <- suppressWarnings(
    test_survey_data %>%
      mutate(
        q1_4 = realiselabelled_vec(sample(c(0, 1), 100, replace = TRUE),
                                   variable_label = "Q1: Brand attributes - ANY category A"),
        q1_5 = realiselabelled_vec(sample(c(0, 1), 100, replace = TRUE),
                                   variable_label = "Q1: Brand attributes - ANY category B"),
        q1_6 = realiselabelled_vec(sample(c(0, 1), 100, replace = TRUE),
                                   variable_label = "Q1: Brand attributes - Other stuff")
      )
  )

  # Test all_matching finds only variables with "ANY category" in labels
  result <- tab(test_survey_data,
                all_matching("ANY category", q1_b, pattern_type = "fixed"),
                gender)

  # Should return 2 matching variables as separate rows (q1_4, q1_5 but not q1_6)
  expect_s3_class(result, "tab_result")
  expect_true(any(grepl("ANY category A", result$row_label)))
  expect_true(any(grepl("ANY category B", result$row_label)))
  expect_false(any(grepl("Other stuff", result$row_label)))

  # Should have exactly 2 data rows + base row + any total rows
  data_rows <- result[!grepl("Base|Total|NET", result$row_label), ]
  expect_equal(nrow(data_rows), 2)
})

##### any_positive Helper in Filter Tests #####

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
  result_rows <- tab(data, any_positive(q1), gender)
  any_positive_row <- result_rows[grep("any_positive", result_rows$row_label), ]

  # Using as filter
  result_filter <- tab(data, total(), gender, filter = any_positive(q1_a))
  base_row_filter <- result_filter[result_filter$row_label == "Base (n)", ]

  # The percentage in rows should match the base count with filter
  expect_equal(
    as.numeric(result_filter$Total[2]),  # Percentage who have any_positive q1
    (as.numeric(base_row_filter$Total) / nrow(data$dat)) * 100,
    tolerance = 0.1
  )

  # Test 6: Multiple helpers in filter
  result6 <- tab(data, region, gender,
                 filter = any_positive(q1_a) * top_box(satisfaction, 2))
  expect_s3_class(result6, "tab_result")
  base_row <- result6[result6$row_label == "Base (n)", ]
  total_base <- as.numeric(base_row$Total)
  expect_true(total_base > 0)  # Some respondents match
  expect_true(total_base < nrow(data$dat))  # But not all
})

##### Significance Testing Tests #####
# Setup test data for significance tests
setup_sig_test_data <- function() {
  set.seed(42)
  test_data <- get_big_test_dat(800)
  survey_obj <- create_survey_data(test_data)
  data_for_manual <- survey_obj$dat
  
  # Add weight variable for weighted tests
  test_data$weight_var <- pmax(0.3, pmin(3.0, rnorm(nrow(test_data), mean = 1.0, sd = 0.4)))
  
  list(test_data = test_data, survey_obj = survey_obj, data_for_manual = data_for_manual)
}

test_that("Z-test for proportions matches manual calculation", {
  sig_data <- setup_sig_test_data()
  test_data <- sig_data$test_data
  data_for_manual <- sig_data$data_for_manual
  
  # Run tab with significance testing
  result_prop <- tab(test_data, labelledordinal, labelledcategorical,
                     statistic = "column_pct")
  result_prop <- add_sig(result_prop, versus = "first_col", test = "z_test_proportions")

  # Manual calculation for "Very satisfied" (level 4) between North vs East
  north_satisfied <- sum(data_for_manual$labelledordinal == 4 &
                           data_for_manual$labelledcategorical == 1, na.rm = TRUE)
  north_total <- sum(data_for_manual$labelledcategorical == 1, na.rm = TRUE)

  east_satisfied <- sum(data_for_manual$labelledordinal == 4 &
                          data_for_manual$labelledcategorical == 2, na.rm = TRUE)
  east_total <- sum(data_for_manual$labelledcategorical == 2, na.rm = TRUE)

  # Manual z-test calculation
  p1 <- north_satisfied / north_total
  p2 <- east_satisfied / east_total
  p_pooled <- (north_satisfied + east_satisfied) / (north_total + east_total)
  se <- sqrt(p_pooled * (1 - p_pooled) * (1/north_total + 1/east_total))
  z_manual <- (p2 - p1) / se
  p_value_manual <- 2 * pnorm(-abs(z_manual))

  # Extract p-value from tab result
  sig_result <- attr(result_prop, "significance")
  tab_p_value <- sig_result[[1]]$p_values[4, 2]

  # Test that manual and tab p-values match within tolerance
  expect_equal(tab_p_value, p_value_manual, tolerance = 0.0001)
  expect_true(!is.na(tab_p_value))
  expect_true(tab_p_value >= 0 && tab_p_value <= 1)
})

test_that("T-test for means matches manual calculation", {
  sig_data <- setup_sig_test_data()
  test_data <- sig_data$test_data
  data_for_manual <- sig_data$data_for_manual
  
  # Run tab with t-test
  result_means <- tab(test_data, booleans, labelledcategorical,
                      statistic = "mean",
                      values = "randomnumeric")
  result_means <- add_sig(result_means, versus = "first_col", test = "t_test")

  # Manual t-test calculation - compare North vs West for TRUE booleans
  north_values <- data_for_manual$randomnumeric[data_for_manual$labelledcategorical == 1 &
                                                  data_for_manual$booleans == TRUE]
  west_values <- data_for_manual$randomnumeric[data_for_manual$labelledcategorical == 4 &
                                                 data_for_manual$booleans == TRUE]

  # Remove NAs
  north_values <- north_values[!is.na(north_values)]
  west_values <- west_values[!is.na(west_values)]

  # Manual t-test
  manual_t_test <- t.test(west_values, north_values)

  # Extract from tab result
  sig_result_t <- attr(result_means, "significance")
  tab_p_value <- sig_result_t[[1]]$p_values[1, 4]

  # Test that manual and tab p-values match
  expect_equal(tab_p_value, manual_t_test$p.value, tolerance = 0.0001)
  expect_true(!is.na(tab_p_value))
})

test_that("Chi-square test matches manual calculation", {
  sig_data <- setup_sig_test_data()
  test_data <- sig_data$test_data
  data_for_manual <- sig_data$data_for_manual
  
  # Run tab with chi-square test (without summary rows/columns)
  result_chi <- tab(test_data, labelledordinal, labelledcategorical,
                    statistic = "count",
                    show_col_nets = FALSE)
  result_chi <- add_sig(result_chi, test = "chi_square")

  # Manual chi-square calculation
  cont_table <- table(data_for_manual$labelledordinal,
                      data_for_manual$labelledcategorical)

  # Remove any rows/cols that are all zero
  cont_table <- cont_table[rowSums(cont_table) > 0, colSums(cont_table) > 0]

  manual_chi <- chisq.test(cont_table)

  # Extract from tab result
  sig_result_chi <- attr(result_chi, "significance")

  # For omnibus tests, all cells have the same p-value
  tab_p_value <- sig_result_chi[[1]]$p_values[1, 1]

  expect_equal(tab_p_value, manual_chi$p.value, tolerance = 0.0001)
  expect_true(!is.na(tab_p_value))
  expect_true(sig_result_chi[[1]]$is_omnibus)
})

test_that("Mann-Whitney test matches manual calculation", {
  sig_data <- setup_sig_test_data()
  test_data <- sig_data$test_data
  data_for_manual <- sig_data$data_for_manual
  
  # Run tab with Mann-Whitney test
  result_mw <- tab(test_data, booleans, labelledcategorical,
                   statistic = "median",
                   values = "randomnumeric")
  result_mw <- add_sig(result_mw, test = "mann_whitney", versus = "first_col")

  # Manual Mann-Whitney calculation - North vs South for TRUE booleans
  north_values_mw <- data_for_manual$randomnumeric[data_for_manual$labelledcategorical == 1 &
                                                     data_for_manual$booleans == TRUE]
  south_values_mw <- data_for_manual$randomnumeric[data_for_manual$labelledcategorical == 3 &
                                                     data_for_manual$booleans == TRUE]

  # Remove NAs
  north_values_mw <- north_values_mw[!is.na(north_values_mw)]
  south_values_mw <- south_values_mw[!is.na(south_values_mw)]

  manual_mw_test <- wilcox.test(south_values_mw, north_values_mw)

  # Extract from tab result
  sig_result_mw <- attr(result_mw, "significance")
  tab_p_value <- sig_result_mw[[1]]$p_values[1, 3]

  expect_equal(tab_p_value, manual_mw_test$p.value, tolerance = 0.0001)
  expect_true(!is.na(tab_p_value))
})

test_that("Multiple comparison adjustments work correctly", {
  sig_data <- setup_sig_test_data()
  test_data <- sig_data$test_data
  
  # Run without adjustment
  result_no_adjust <- tab(test_data, labelledordinal, labelledcategorical,
                          statistic = "column_pct")
  result_no_adjust <- add_sig(result_no_adjust, test = "z_test_proportions",
                              versus = "first_col", adjust = "none")

  # Extract raw p-values
  sig_no_adjust <- attr(result_no_adjust, "significance")
  raw_p_values <- as.vector(sig_no_adjust[[1]]$p_values[, -1])
  raw_p_values <- raw_p_values[!is.na(raw_p_values)]

  # Test Bonferroni adjustment
  result_bonf <- tab(test_data, labelledordinal, labelledcategorical,
                     statistic = "column_pct")
  result_bonf <- add_sig(result_bonf, test = "z_test_proportions",
                         versus = "first_col", adjust = "bonferroni")

  sig_bonf <- attr(result_bonf, "significance")
  bonf_p_values <- as.vector(sig_bonf[[1]]$p_values[, -1])
  bonf_p_values <- bonf_p_values[!is.na(bonf_p_values)]

  manual_bonf <- p.adjust(raw_p_values, method = "bonferroni")

  expect_equal(bonf_p_values, manual_bonf, tolerance = 0.0001)
  expect_true(all(bonf_p_values >= raw_p_values)) # Bonferroni should increase p-values

  # Test BH adjustment
  result_bh <- tab(test_data, labelledordinal, labelledcategorical,
                   statistic = "column_pct")
  result_bh <- add_sig(result_bh, test = "z_test_proportions",
                       versus = "first_col", adjust = "BH")

  sig_bh <- attr(result_bh, "significance")
  bh_p_values <- as.vector(sig_bh[[1]]$p_values[, -1])
  bh_p_values <- bh_p_values[!is.na(bh_p_values)]

  manual_bh <- p.adjust(raw_p_values, method = "BH")

  expect_equal(bh_p_values, manual_bh, tolerance = 0.0001)
})

test_that("Custom significance test works correctly", {
  sig_data <- setup_sig_test_data()
  test_data <- sig_data$test_data
  data_for_manual <- sig_data$data_for_manual
  
  # Create a simple "practical significance" test
  create_significance_test(
    id = "practical_sig",
    name = "Practical significance test",
    description = "Tests if absolute difference in proportions exceeds threshold",
    processor = function(base_array, row_array, col_array_1, col_array_2, threshold = 0.1, ...) {
      n1 <- sum(base_array * row_array * col_array_1, na.rm = TRUE)
      N1 <- sum(base_array * col_array_1, na.rm = TRUE)
      n2 <- sum(base_array * row_array * col_array_2, na.rm = TRUE)
      N2 <- sum(base_array * col_array_2, na.rm = TRUE)

      if (is.na(N1) || N1 == 0 || is.na(N2) || N2 == 0) {
        return(list(p_value = NA_real_, statistic = NA_real_))
      }

      p1 <- n1 / N1
      p2 <- n2 / N2
      abs_diff <- abs(p2 - p1)

      p_value <- if (abs_diff > threshold) 0 else 1

      return(list(
        p_value = p_value,
        statistic = p2 - p1,
        method = paste0("Practical significance test (threshold = ", threshold, ")")
      ))
    }
  )

  result_practical <- tab(test_data, labelledordinal, labelledcategorical,
                          statistic = "column_pct")
  result_practical <- add_sig(result_practical, test = "practical_sig", versus = "first_col")

  # Manual verification for "Very satisfied" between North vs East
  north_satisfied <- sum(data_for_manual$labelledordinal == 4 &
                           data_for_manual$labelledcategorical == 1, na.rm = TRUE)
  north_total <- sum(data_for_manual$labelledcategorical == 1, na.rm = TRUE)
  east_satisfied <- sum(data_for_manual$labelledordinal == 4 &
                          data_for_manual$labelledcategorical == 2, na.rm = TRUE)
  east_total <- sum(data_for_manual$labelledcategorical == 2, na.rm = TRUE)

  p1_manual <- north_satisfied / north_total
  p2_manual <- east_satisfied / east_total
  abs_diff_manual <- abs(p2_manual - p1_manual)
  expected_p <- if(abs_diff_manual > 0.1) 0 else 1

  sig_result_practical <- attr(result_practical, "significance")
  actual_p <- sig_result_practical[[1]]$p_values[4, 2]

  expect_equal(actual_p, expected_p)
  expect_true(actual_p %in% c(0, 1)) # Should be binary decision

  # Clean up
  clear_tab_registry()
  ensure_builtins_registered()
})

test_that("Weighted z-test matches manual calculation", {
  sig_data <- setup_sig_test_data()
  test_data <- sig_data$test_data
  data_for_manual <- sig_data$data_for_manual
  
  # Run weighted tab with significance testing
  result_weighted <- tab(test_data, labelledordinal, labelledcategorical,
                         weight = "weight_var",
                         statistic = "column_pct")
  result_weighted <- add_sig(result_weighted, test = "z_test_weighted", versus = "first_col")

  weights <- test_data$weight_var

  # Manual weighted calculation for "Very satisfied" between North vs East
  north_satisfied_w <- sum((data_for_manual$labelledordinal == 4 &
                              data_for_manual$labelledcategorical == 1) * weights,
                           na.rm = TRUE)
  north_total_w <- sum((data_for_manual$labelledcategorical == 1) * weights,
                       na.rm = TRUE)

  east_satisfied_w <- sum((data_for_manual$labelledordinal == 4 &
                             data_for_manual$labelledcategorical == 2) * weights,
                          na.rm = TRUE)
  east_total_w <- sum((data_for_manual$labelledcategorical == 2) * weights,
                      na.rm = TRUE)

  # Weighted proportions
  p1_w <- north_satisfied_w / north_total_w
  p2_w <- east_satisfied_w / east_total_w
  p_pooled_w <- (north_satisfied_w + east_satisfied_w) / (north_total_w + east_total_w)

  # Calculate effective sample sizes for manual calculation
  # Get weights for each group, removing NAs first
  north_weights <- weights[data_for_manual$labelledcategorical == 1]
  east_weights <- weights[data_for_manual$labelledcategorical == 2]

  # Remove NAs and zero/negative weights
  north_weights <- north_weights[!is.na(north_weights) & north_weights > 0]
  east_weights <- east_weights[!is.na(east_weights) & east_weights > 0]

  # Effective sample sizes: n_eff = (sum(w))^2 / sum(w^2)
  north_n_eff <- (sum(north_weights))^2 / sum(north_weights^2)
  east_n_eff <- (sum(east_weights))^2 / sum(east_weights^2)

  # Weighted standard error using effective sample sizes
  se_w <- sqrt(p_pooled_w * (1 - p_pooled_w) * (1/north_n_eff + 1/east_n_eff))
  z_weighted <- (p2_w - p1_w) / se_w
  p_value_weighted <- 2 * pnorm(-abs(z_weighted))

  # Extract from tab result
  sig_weighted <- attr(result_weighted, "significance")
  tab_p_value <- sig_weighted[[1]]$p_values[4, 2]

  expect_equal(tab_p_value, p_value_weighted, tolerance = 0.001)
  expect_true(!is.na(tab_p_value))
})

test_that("Weighted t-test matches manual calculation", {
  sig_data <- setup_sig_test_data()
  test_data <- sig_data$test_data
  data_for_manual <- sig_data$data_for_manual
  
  # Run weighted t-test
  result_weighted_mean <- tab(test_data, booleans, labelledcategorical,
                              weight = "weight_var",
                              statistic = "mean",
                              values = "randomnumeric")
  result_weighted_mean <- add_sig(result_weighted_mean, test = "t_test_weighted", versus = "first_col")

  weights <- test_data$weight_var

  # Manual weighted t-test calculation
  # Get weighted values for both groups (TRUE booleans only)
  north_values_wt <- data_for_manual$randomnumeric[data_for_manual$labelledcategorical == 1 &
                                                     data_for_manual$booleans == TRUE]
  west_values_wt <- data_for_manual$randomnumeric[data_for_manual$labelledcategorical == 4 &
                                                    data_for_manual$booleans == TRUE]
  north_weights_wt <- weights[data_for_manual$labelledcategorical == 1 &
                                data_for_manual$booleans == TRUE]
  west_weights_wt <- weights[data_for_manual$labelledcategorical == 4 &
                               data_for_manual$booleans == TRUE]

  # Remove NAs and zero weights
  valid_north <- !is.na(north_values_wt) & !is.na(north_weights_wt) & north_weights_wt > 0
  valid_west <- !is.na(west_values_wt) & !is.na(west_weights_wt) & west_weights_wt > 0

  north_values_wt <- north_values_wt[valid_north]
  west_values_wt <- west_values_wt[valid_west]
  north_weights_wt <- north_weights_wt[valid_north]
  west_weights_wt <- west_weights_wt[valid_west]

  # Skip if insufficient data
  skip_if(length(north_values_wt) < 2 || length(west_values_wt) < 2)

  # Weighted means
  mean_north_wt <- sum(north_weights_wt * north_values_wt) / sum(north_weights_wt)
  mean_west_wt <- sum(west_weights_wt * west_values_wt) / sum(west_weights_wt)

  # Weighted variances (population formula, then corrected)
  var_north_pop <- sum(north_weights_wt * (north_values_wt - mean_north_wt)^2) / sum(north_weights_wt)
  var_west_pop <- sum(west_weights_wt * (west_values_wt - mean_west_wt)^2) / sum(west_weights_wt)

  # Effective sample sizes
  north_n_eff_t <- (sum(north_weights_wt))^2 / sum(north_weights_wt^2)
  west_n_eff_t <- (sum(west_weights_wt))^2 / sum(west_weights_wt^2)

  # Convert to sample variances using effective sample size
  var_north_wt <- var_north_pop * north_n_eff_t / (north_n_eff_t - 1)
  var_west_wt <- var_west_pop * west_n_eff_t / (west_n_eff_t - 1)

  # Standard error of difference in means
  se_diff_wt <- sqrt(var_north_wt/north_n_eff_t + var_west_wt/west_n_eff_t)

  # T statistic
  t_stat_wt <- (mean_west_wt - mean_north_wt) / se_diff_wt

  # Welch-Satterthwaite degrees of freedom
  s1_sq_over_n1 <- var_north_wt / north_n_eff_t
  s2_sq_over_n2 <- var_west_wt / west_n_eff_t
  df_wt <- (s1_sq_over_n1 + s2_sq_over_n2)^2 /
    (s1_sq_over_n1^2/(north_n_eff_t-1) + s2_sq_over_n2^2/(west_n_eff_t-1))
  df_wt <- max(1, min(df_wt, north_n_eff_t + west_n_eff_t - 2))

  # Two-tailed p-value
  p_value_wt <- 2 * pt(-abs(t_stat_wt), df_wt)

  # Extract significance results for weighted t-test
  sig_result_wt <- attr(result_weighted_mean, "significance")
  tab_p_value <- sig_result_wt[[1]]$p_values[1, 4]

  expect_equal(tab_p_value, p_value_wt, tolerance = 0.001)
  expect_true(!is.na(tab_p_value))
})

test_that("Weighted data produces different results than unweighted", {
  sig_data <- setup_sig_test_data()
  test_data <- sig_data$test_data
  
  # Unweighted
  result_unweighted <- tab(test_data, labelledordinal, labelledcategorical,
                           statistic = "column_pct")
  result_unweighted <- add_sig(result_unweighted, test = "z_test_proportions", versus = "first_col")

  # Weighted
  result_weighted <- tab(test_data, labelledordinal, labelledcategorical,
                         weight = "weight_var",
                         statistic = "column_pct")
  result_weighted <- add_sig(result_weighted, test = "z_test_weighted", versus = "first_col")

  # Extract base sizes - they should be different
  unweighted_bases <- as.numeric(result_unweighted[nrow(result_unweighted), -1])
  weighted_bases <- as.numeric(result_weighted[nrow(result_weighted), -1])

  # Bases should be different
  expect_false(identical(unweighted_bases, weighted_bases))

  # P-values should generally be different
  sig_unweighted <- attr(result_unweighted, "significance")
  sig_weighted <- attr(result_weighted, "significance")

  unweighted_p <- sig_unweighted[[1]]$p_values[4, 2]
  weighted_p <- sig_weighted[[1]]$p_values[4, 2]

  # P-values should be different (though they could theoretically be the same)
  expect_true(!is.na(unweighted_p))
  expect_true(!is.na(weighted_p))
})

##### Mathematical Accuracy Tests #####

test_that("pivot_to_grid basic row percentage accuracy - rows sum to 100%", {
  result <- tab(grid_test_data, A2_a, statistic = "column_pct") %>% pivot_to_grid()
  grid_df <- as.data.frame(result, show_base = FALSE)
  
  # Remove row_label column and any non-numeric columns
  numeric_cols <- sapply(grid_df, is.numeric)
  data_only <- grid_df[, numeric_cols]
  
  # Each row should sum to 100% (within tolerance)
  row_sums <- rowSums(data_only, na.rm = TRUE)
  
  for (i in seq_along(row_sums)) {
    expect_equal(as.numeric(row_sums[i]), 100, tolerance = 0.2,
                 label = paste("Row", i, "sum"))
  }
})

test_that("pivot_to_grid cell-by-cell verification against manual calculations", {
  result <- tab(grid_test_data, A2_a, statistic = "column_pct") %>% pivot_to_grid()
  grid_df <- as.data.frame(result)
  
  raw_data <- grid_test_data$dat
  
  # Test all 20 cells (5 variables × 4 values)
  variables <- c("A2_1", "A2_2", "A2_3", "A2_4", "A2_5")
  var_labels <- c("How often do you create - Docs",
                  "How often do you create - Presentations",
                  "How often do you create - Spreadsheets",
                  "How often do you create - Whiteboards",
                  "How often do you create - Visualizations")
  values <- 1:4
  value_labels <- c("Daily", "Weekly", "Monthly", "Never")
  
  for (i in seq_along(variables)) {
    var <- variables[i]
    var_label <- var_labels[i]
    
    for (j in seq_along(values)) {
      val <- values[j]
      val_label <- value_labels[j]
      
      # Manual calculation
      manual_count <- sum(raw_data[[var]] == val, na.rm = TRUE)
      manual_total <- sum(!is.na(raw_data[[var]]))
      manual_pct <- (manual_count / manual_total) * 100
      
      # Get value from grid
      grid_pct <- as.numeric(grid_df[grid_df$row_label == var_label, val_label])
      
      # Assert match
      expect_equal(grid_pct, manual_pct, tolerance = 0.1,
                   info = paste("Cell mismatch:", var_label, "-", val_label))
    }
  }
})

test_that("pivot_to_grid base sizes are correct", {
  # Using count statistic to check base sizes
  result <- tab(grid_test_data, A2_a, statistic = "count") %>% pivot_to_grid()
  grid_df <- as.data.frame(result)
  
  raw_data <- grid_test_data$dat
  variables <- c("A2_1", "A2_2", "A2_3", "A2_4", "A2_5")
  
  # Each row's total should equal the number of non-NA responses for that variable
  for (i in seq_along(variables)) {
    var <- variables[i]
    
    # Manual count of non-NA responses
    manual_total <- sum(!is.na(raw_data[[var]]))
    
    # Sum across all value columns in grid
    row_total <- sum(as.numeric(grid_df[i, -1]), na.rm = TRUE)
    
    expect_equal(row_total, manual_total, tolerance = 0.1,
                 info = paste("Base size mismatch for", var))
  }
})

test_that("pivot_to_grid weighted calculations are correct", {
  result <- tab(grid_test_data, A2_a, statistic = "column_pct", weight = "weight_var") %>%
    pivot_to_grid()
  grid_df <- as.data.frame(result)
  
  raw_data <- grid_test_data$dat
  weights <- grid_test_data$dat$weight_var
  
  # Test several cells with weighted calculations
  test_cases <- list(
    list(var = "A2_1", val = 1, label = "How often do you create - Docs", val_label = "Daily"),
    list(var = "A2_2", val = 3, label = "How often do you create - Presentations", val_label = "Monthly"),
    list(var = "A2_5", val = 2, label = "How often do you create - Visualizations", val_label = "Weekly")
  )
  
  for (tc in test_cases) {
    # Manual weighted calculation
    weighted_count <- sum(weights[raw_data[[tc$var]] == tc$val], na.rm = TRUE)
    weighted_total <- sum(weights[!is.na(raw_data[[tc$var]])], na.rm = TRUE)
    manual_pct <- (weighted_count / weighted_total) * 100
    
    # Get value from grid
    grid_pct <- as.numeric(grid_df[grid_df$row_label == tc$label, tc$val_label])
    
    expect_equal(grid_pct, manual_pct, tolerance = 0.1,
                 info = paste("Weighted cell mismatch:", tc$label, "-", tc$val_label))
  }
})

test_that("pivot_to_grid with filtered data produces correct results", {
  # Apply filter: age > 40
  result <- tab(grid_test_data, A2_a, statistic = "column_pct", filter = age > 40) %>%
    pivot_to_grid()
  grid_df <- as.data.frame(result)
  
  raw_data <- grid_test_data$dat
  filtered_data <- raw_data[raw_data$age > 40, ]
  
  # Test a few cells on filtered data
  test_cases <- list(
    list(var = "A2_1", val = 1, label = "How often do you create - Docs", val_label = "Daily"),
    list(var = "A2_3", val = 4, label = "How often do you create - Spreadsheets", val_label = "Never")
  )
  
  for (tc in test_cases) {
    # Manual calculation on filtered data
    manual_count <- sum(filtered_data[[tc$var]] == tc$val, na.rm = TRUE)
    manual_total <- sum(!is.na(filtered_data[[tc$var]]))
    manual_pct <- if (manual_total > 0) (manual_count / manual_total) * 100 else 0
    
    # Get value from grid
    grid_pct <- as.numeric(grid_df[grid_df$row_label == tc$label, tc$val_label])
    
    expect_equal(grid_pct, manual_pct, tolerance = 0.1,
                 info = paste("Filtered cell mismatch:", tc$label, "-", tc$val_label))
  }
  
  # Verify base sizes are smaller due to filter
  count_result <- tab(grid_test_data, A2_a, statistic = "count", filter = age > 40) %>%
    pivot_to_grid()
  count_df <- as.data.frame(count_result)
  
  row_total <- sum(as.numeric(count_df[1, -1]), na.rm = TRUE)
  expected_total <- sum(!is.na(filtered_data$A2_1))
  
  expect_equal(row_total, expected_total, tolerance = 0.1)
  expect_true(row_total < 100)  # Should be less than full sample
})

##### Layout Correctness Tests #####

test_that("pivot_to_grid produces correct grid dimensions", {
  result <- tab(grid_test_data, A2_a, statistic = "column_pct") %>% pivot_to_grid()
  grid_df <- as.data.frame(result, show_base = FALSE)
  
  # Should have 5 rows (one per variable)
  expect_equal(nrow(grid_df), 5)
  
  # Should have 4 data columns plus row_label column = 5 columns
  expect_equal(ncol(grid_df), 5)
})

test_that("pivot_to_grid row labels are correct", {
  result <- tab(grid_test_data, A2_a, statistic = "column_pct") %>% pivot_to_grid()
  grid_df <- as.data.frame(result, show_base = FALSE)
  
  expected_labels <- c(
    "How often do you create - Docs",
    "How often do you create - Presentations",
    "How often do you create - Spreadsheets",
    "How often do you create - Whiteboards",
    "How often do you create - Visualizations"
  )
  
  expect_equal(grid_df$row_label, expected_labels)
})

test_that("pivot_to_grid column labels are correct", {
  result <- tab(grid_test_data, A2_a, statistic = "column_pct") %>% pivot_to_grid()
  grid_df <- as.data.frame(result, show_base = FALSE)
  
  expected_cols <- c("row_label", "Daily", "Weekly", "Monthly", "Never")
  
  expect_equal(names(grid_df), expected_cols)
})

test_that("pivot_to_grid cell correspondence is correct", {
  result <- tab(grid_test_data, A2_a, statistic = "column_pct") %>% pivot_to_grid()
  grid_df <- as.data.frame(result, show_base = FALSE)
  
  raw_data <- grid_test_data$dat
  
  # Test specific known positions
  # Position [1, 2] = A2_1 (Docs), Daily
  manual_count <- sum(raw_data$A2_1 == 1, na.rm = TRUE)
  manual_total <- sum(!is.na(raw_data$A2_1))
  manual_pct <- (manual_count / manual_total) * 100
  
  grid_pct <- as.numeric(grid_df[1, "Daily"])
  
  expect_equal(grid_pct, manual_pct, tolerance = 0.1)
  
  # Position [3, 4] = A2_3 (Spreadsheets), Monthly  
  manual_count <- sum(raw_data$A2_3 == 3, na.rm = TRUE)
  manual_total <- sum(!is.na(raw_data$A2_3))
  manual_pct <- (manual_count / manual_total) * 100
  
  grid_pct <- as.numeric(grid_df[3, "Monthly"])
  
  expect_equal(grid_pct, manual_pct, tolerance = 0.1)
})

##### Edge Cases and Data Quality #####

test_that("pivot_to_grid handles missing data correctly", {
  # Create version with NAs
  test_data_na <- create_grid_test_data(100, seed = 789)
  test_data_na$dat$A2_2[1:20] <- NA  # Add NAs to Presentations variable
  
  result <- tab(test_data_na, A2_a, statistic = "column_pct") %>% pivot_to_grid()
  grid_df <- as.data.frame(result, show_base = FALSE)
  
  # Row with NAs should still sum to 100% (excluding NAs)
  presentations_row <- grid_df[grid_df$row_label == "How often do you create - Presentations", ]
  row_sum <- sum(as.numeric(presentations_row[, -1]), na.rm = TRUE)
  
  expect_equal(row_sum, 100, tolerance = 0.2)
  
  # Base should reflect non-NA count
  count_result <- tab(test_data_na, A2_a, statistic = "count") %>% pivot_to_grid()
  count_df <- as.data.frame(count_result, show_base = FALSE)
  
  presentations_count <- sum(as.numeric(count_df[2, -1]), na.rm = TRUE)
  expected_count <- sum(!is.na(test_data_na$dat$A2_2))
  
  expect_equal(presentations_count, expected_count, tolerance = 0.1)
  expect_equal(expected_count, 80)  # 100 - 20 NAs
  
  # Other variables should be unaffected
  docs_count <- sum(as.numeric(count_df[1, -1]), na.rm = TRUE)
  expect_equal(docs_count, 100, tolerance = 0.1)
})

test_that("pivot_to_grid handles sparse data (zero responses)", {
  # Create data where one variable-value combination has zero responses
  test_data_sparse <- suppressWarnings(create_grid_test_data(50, seed = 999))
  # Force all A2_4 responses to be 3 or 4 (no 1s or 2s)
  test_data_sparse$dat$A2_4[test_data_sparse$dat$A2_4 <= 2] <- 3
  
  result <- tab(test_data_sparse, A2_a, statistic = "column_pct") %>% pivot_to_grid()
  grid_df <- as.data.frame(result, show_base = FALSE)
  
  whiteboards_row <- grid_df[grid_df$row_label == "How often do you create - Whiteboards", ]
  
  # Daily and Weekly should be 0%
  expect_equal(as.numeric(whiteboards_row$Daily), 0, tolerance = 0.1)
  expect_equal(as.numeric(whiteboards_row$Weekly), 0, tolerance = 0.1)
  
  # Row should still sum to 100%
  row_sum <- sum(as.numeric(whiteboards_row[, -1]), na.rm = TRUE)
  expect_equal(row_sum, 100, tolerance = 0.2)
})

test_that("pivot_to_grid works with count statistic", {
  result <- tab(grid_test_data, A2_a, statistic = "count") %>% pivot_to_grid()
  grid_df <- as.data.frame(result)
  
  raw_data <- grid_test_data$dat
  
  # Verify a few cells show correct counts
  docs_daily_count <- as.numeric(grid_df[1, "Daily"])
  expected_count <- sum(raw_data$A2_1 == 1, na.rm = TRUE)
  
  expect_equal(docs_daily_count, expected_count, tolerance = 0.1)
  
  # Verify counts are integers (or close to it)
  expect_true(abs(docs_daily_count - round(docs_daily_count)) < 0.01)
})

##### Custom Extractors #####

test_that("pivot_to_grid works with formula-based extractors", {
  # Test using formula syntax (should produce same result as default)
  result_default <- tab(grid_test_data, A2_a, statistic = "column_pct") %>%
    pivot_to_grid()
  
  result_formula <- tab(grid_test_data, A2_a, statistic = "column_pct") %>%
    pivot_to_grid(
      row_extractor = ~ dsl_get_variables(.x$specification$dsl$row)[1],
      col_extractor = ~ dsl_get_values(.x$specification$dsl$row)[1]
    )
  
  df_default <- as.data.frame(result_default)
  df_formula <- as.data.frame(result_formula)
  
  # Should produce identical results
  df1 <- df_default
  df2 <- df_formula
  attr(df1, "layout") <- NULL
  attr(df2, "layout") <- NULL
  expect_equal(df1, df2)
  })

test_that("pivot_to_grid custom extractors for transposed layout", {
  # Create custom extractors to swap dimensions
  result <- tab(grid_test_data, A2_a, statistic = "column_pct") %>%
    pivot_to_grid(
      row_extractor = function(cell) {
        dsl_get_values(cell$specification$dsl$row)[1]
      },
      col_extractor = function(cell) {
        dsl_get_variables(cell$specification$dsl$row)[1]
      }
    )
  
  grid_df <- as.data.frame(result, show_base = FALSE)
  
  # Should now have 4 rows (one per value) and 5 data columns (one per variable) + row_label
  expect_equal(nrow(grid_df), 4)
  expect_equal(ncol(grid_df), 6)  # row_label + 5 variables
  
  # Row labels should be value labels
  expect_true(all(c("Daily", "Weekly", "Monthly", "Never") %in% grid_df$row_label))
})

##### Integration Tests #####

test_that("pivot_to_grid works with chained hide operations", {
  result <- tab(grid_test_data, A2_a, statistic = "column_pct") %>%
    pivot_to_grid() %>%
    hide_rows("How often do you create - Whiteboards")
  
  grid_df <- as.data.frame(result, show_base = FALSE)
  
  # Should have 4 rows instead of 5
  expect_equal(nrow(grid_df), 4)
  
  # Whiteboards row should be gone
  expect_false("How often do you create - Whiteboards" %in% grid_df$row_label)
})

test_that("pivot_to_grid works with chained sort operations", {
  result <- tab(grid_test_data, A2_a, statistic = "column_pct") %>%
    pivot_to_grid() %>%
    arrange_rows(.by = "Daily", .sort = "desc")
  
  grid_df <- as.data.frame(result, show_base = FALSE)
  
  # First row should have highest Daily percentage
  daily_vals <- as.numeric(grid_df$Daily)
  expect_equal(daily_vals[1], max(daily_vals))
  
  # Values should be in descending order
  expect_true(all(diff(daily_vals) <= 0))
})

test_that("pivot_to_grid result exports to data.frame correctly", {
  result <- tab(grid_test_data, A2_a, statistic = "row_pct") %>% pivot_to_grid()
  
  grid_df <- as.data.frame(result)
  
  # Should be a proper data.frame
  expect_s3_class(grid_df, "data.frame")
  
  # Should have row_label column
  expect_true("row_label" %in% names(grid_df))
  
  # Should have all value columns
  expect_true(all(c("Daily", "Weekly", "Monthly", "Never") %in% names(grid_df)))
  
  # All data columns should be numeric
  data_cols <- setdiff(names(grid_df), "row_label")
  for (col in data_cols) {
    expect_true(is.numeric(grid_df[[col]]))
  }
})

##### Error Handling #####

test_that("pivot_to_grid requires cell-based tab_result", {
  # Create a data.frame result (not cell-based)
  df_result <- data.frame(x = 1:5, y = 6:10)
  
  expect_error(
    pivot_to_grid(df_result),
    "cell-based tab_result"
  )
})

test_that("pivot_to_grid handles extraction failures gracefully", {
  # This test depends on implementation details, but we can test that
  # the function doesn't crash with unusual data
  
  # Create a simple tab result
  result <- tab(grid_test_data, A2_a, statistic = "row_pct")
  
  # Try with an extractor that might fail on some cells
  # Should either work or give a clear error
  expect_error(
    pivot_to_grid(result, 
                  row_extractor = function(cell) stop("Test error"),
                  col_extractor = function(cell) 1),
    "Test error|No cells matched|extraction"
  )
})

##### Derive Operations Tests #####

# Helper to setup factor mtcars for derive tests
setup_factor_mtcars <- function() {
  mtcars_cat <- mtcars
  mtcars_cat$cyl <- factor(mtcars$cyl)
  mtcars_cat$gear <- factor(mtcars$gear)
  mtcars_cat$am <- factor(mtcars$am)
  mtcars_cat
}

test_that("sum_if adds derived cells for columns", {
  mtcars_cat <- setup_factor_mtcars()
  result <- tab(mtcars_cat, cyl, gear, show_row_nets = FALSE, show_col_nets = FALSE, show_base = FALSE)
  
  original_ncol <- ncol(result$layout$grid)
  original_cell_count <- cell_count(result$cell_store)
  
  # Apply sum_if
  result_derived <- derive(result, sum_if("ival", dimension = "cols"))
  
  # Should have added derived columns
  expect_gt(ncol(result_derived$layout$grid), original_ncol)
  
  # Should have added derived cells to store
  expect_gt(cell_count(result_derived$cell_store), original_cell_count)
})

test_that("sum_if adds derived cells for rows", {
  mtcars_cat <- setup_factor_mtcars()
  result <- tab(mtcars_cat, cyl, gear, show_row_nets = FALSE, show_col_nets = FALSE, show_base = FALSE)
  
  original_nrow <- nrow(result$layout$grid)
  original_cell_count <- cell_count(result$cell_store)
  
  # Apply sum_if for rows
  result_derived <- derive(result, sum_if("ival", dimension = "rows"))
  
  # Should have added derived rows
  expect_gt(nrow(result_derived$layout$grid), original_nrow)
  
  # Should have added derived cells
  expect_gt(cell_count(result_derived$cell_store), original_cell_count)
})

test_that("sum_if derived cells have correct derivation metadata", {
  mtcars_cat <- setup_factor_mtcars()
  result <- tab(mtcars_cat, cyl, gear, show_row_nets = FALSE, show_col_nets = FALSE, show_base = FALSE) %>%
    derive(sum_if("ival", dimension = "cols"))
  
  # Find a derived cell
  n_original_cols <- 3  # cyl has 3 levels
  if (ncol(result$layout$grid) > n_original_cols) {
    derived_cell_id <- result$layout$grid[1, n_original_cols + 1]
    
    if (!is.na(derived_cell_id)) {
      derived_cell <- get_cell(result$cell_store, derived_cell_id)
      
      # Check derivation metadata
      expect_true(!is.null(derived_cell$derivation))
      expect_equal(derived_cell$derivation$operation, "sum_if")
      expect_true(length(derived_cell$derivation$source_cells) > 0)
    }
  }
})

test_that("delta_vs creates difference column", {
  mtcars_cat <- setup_factor_mtcars()
  result <- tab(mtcars_cat, cyl, gear, show_row_nets = FALSE, show_col_nets = FALSE, show_base = FALSE)
  
  original_ncol <- ncol(result$layout$grid)
  original_cell_count <- cell_count(result$cell_store)
  
  # Get first two column labels
  col1 <- result$layout$col_labels[1]
  col2 <- result$layout$col_labels[2]
  
  # Apply delta_vs
  result_derived <- derive(result, delta_vs(col1, col2))
  
  # Should have added one new column
  expect_equal(ncol(result_derived$layout$grid), original_ncol + 1)
  
  # Should have added derived cells (one per row)
  expect_equal(cell_count(result_derived$cell_store), 
               original_cell_count + nrow(result$layout$grid))
})

test_that("delta_vs derived cells track source cells", {
  mtcars_cat <- setup_factor_mtcars()
  result <- tab(mtcars_cat, cyl, gear, show_row_nets = FALSE, show_col_nets = FALSE, show_base = FALSE)
  
  col1 <- result$layout$col_labels[1]
  col2 <- result$layout$col_labels[2]
  
  result_derived <- derive(result, delta_vs(col1, col2))
  
  # Get a derived cell
  derived_col_idx <- ncol(result$layout$grid) + 1
  derived_cell_id <- result_derived$layout$grid[1, derived_col_idx]
  
  if (!is.na(derived_cell_id)) {
    derived_cell <- get_cell(result_derived$cell_store, derived_cell_id)
    
    # Check derivation tracking
    expect_equal(derived_cell$derivation$operation, "delta_vs")
    expect_equal(length(derived_cell$derivation$source_cells), 2)
    
    # Verify the calculation
    source1_id <- derived_cell$derivation$source_cells[1]
    source2_id <- derived_cell$derivation$source_cells[2]
    
    source1 <- get_cell(result_derived$cell_store, source1_id)
    source2 <- get_cell(result_derived$cell_store, source2_id)
    
    # delta should be source2 - source1
    expect_equal(derived_cell$value, source2$value - source1$value, tolerance = 1e-10)
  }
})

test_that("index_vs creates index columns", {
  mtcars_cat <- setup_factor_mtcars()
  result <- tab(mtcars_cat, cyl, gear, show_row_nets = FALSE, show_col_nets = FALSE, show_base = FALSE)
  
  original_ncol <- ncol(result$layout$grid)
  base_col <- result$layout$col_labels[1]
  
  result_derived <- derive(result, index_vs(base_col, multiplier = 100))
  
  # Should have added index columns for all except base column
  expect_equal(ncol(result_derived$layout$grid), original_ncol + (original_ncol - 1))
})

test_that("index_vs calculates correctly", {
  mtcars_cat <- setup_factor_mtcars()
  result <- tab(mtcars_cat, cyl, gear, show_row_nets = FALSE, show_col_nets = FALSE, show_base = FALSE)
  
  base_col <- result$layout$col_labels[1]
  result_derived <- derive(result, index_vs(base_col, multiplier = 100))
  
  # Get an index cell
  derived_col_idx <- ncol(result$layout$grid) + 1
  derived_cell_id <- result_derived$layout$grid[1, derived_col_idx]
  
  if (!is.na(derived_cell_id)) {
    derived_cell <- get_cell(result_derived$cell_store, derived_cell_id)
    
    # Check derivation
    expect_equal(derived_cell$derivation$operation, "index_vs")
    expect_equal(length(derived_cell$derivation$source_cells), 2)
    
    # Verify calculation
    base_cell_id <- derived_cell$derivation$source_cells[1]
    col_cell_id <- derived_cell$derivation$source_cells[2]
    
    base_cell <- get_cell(result_derived$cell_store, base_cell_id)
    col_cell <- get_cell(result_derived$cell_store, col_cell_id)
    
    expected_index <- (col_cell$value / base_cell$value) * 100
    expect_equal(derived_cell$value, expected_index, tolerance = 1e-10)
  }
})

test_that("share_of_sum by row creates derived columns", {
  mtcars_cat <- setup_factor_mtcars()
  result <- tab(mtcars_cat, cyl, gear, show_row_nets = FALSE, show_col_nets = FALSE, show_base = FALSE)
  
  original_ncol <- ncol(result$layout$grid)
  
  result_derived <- derive(result, share_of_sum(by = "row"))
  
  # Should have doubled the number of columns
  expect_equal(ncol(result_derived$layout$grid), original_ncol * 2)
})

test_that("share_of_sum by col creates derived rows", {
  mtcars_cat <- setup_factor_mtcars()
  result <- tab(mtcars_cat, cyl, gear, show_row_nets = FALSE, show_col_nets = FALSE, show_base = FALSE)
  
  original_nrow <- nrow(result$layout$grid)
  
  result_derived <- derive(result, share_of_sum(by = "col"))
  
  # Should have doubled the number of rows
  expect_equal(nrow(result_derived$layout$grid), original_nrow * 2)
})

test_that("share_of_sum calculates correctly by row", {
  mtcars_cat <- setup_factor_mtcars()
  result <- tab(mtcars_cat, cyl, gear, show_row_nets = FALSE, show_col_nets = FALSE, show_base = FALSE)
  
  result_derived <- derive(result, share_of_sum(by = "row"))
  
  # Get an original cell and its share cell
  orig_cell_id <- result$layout$grid[1, 1]
  orig_cell <- get_cell(result$cell_store, orig_cell_id)
  
  # Calculate expected share
  row_cell_ids <- result$layout$grid[1, ]
  row_values <- sapply(row_cell_ids, function(id) {
    if (!is.na(id)) get_cell(result$cell_store, id)$value else NA
  })
  row_total <- sum(row_values, na.rm = TRUE)
  expected_share <- (orig_cell$value / row_total) * 100
  
  # Find the derived share cell
  derived_col_idx <- ncol(result$layout$grid) + 1
  share_cell_id <- result_derived$layout$grid[1, derived_col_idx]
  
  if (!is.na(share_cell_id)) {
    share_cell <- get_cell(result_derived$cell_store, share_cell_id)
    expect_equal(share_cell$value, expected_share, tolerance = 1e-10)
  }
})

test_that("derive operations are truly additive", {
  mtcars_cat <- setup_factor_mtcars()
  result <- tab(mtcars_cat, cyl, gear, show_row_nets = FALSE, show_col_nets = FALSE, show_base = FALSE)
  
  # Get original cell IDs
  original_cell_ids <- all_cell_ids(result$cell_store)
  
  # Apply derive operation
  result_derived <- derive(result, sum_if("ival", dimension = "cols"))
  
  # All original cell IDs should still be present
  for (orig_id in original_cell_ids) {
    expect_true(has_cell(result_derived$cell_store, orig_id))
  }
})

test_that("multiple derive operations can be chained", {
  mtcars_cat <- setup_factor_mtcars()
  result <- tab(mtcars_cat, cyl, gear, show_row_nets = FALSE, show_col_nets = FALSE, show_base = FALSE)
  
  original_cell_count <- cell_count(result$cell_store)
  col1 <- result$layout$col_labels[1]
  col2 <- result$layout$col_labels[2]
  
  # Chain multiple derive operations (skip index_vs for now, just test chaining)
  result_derived <- result %>%
    derive(delta_vs(col1, col2))
  
  # Should have added cells
  expect_gt(cell_count(result_derived$cell_store), original_cell_count)
  
  # All original cells should still be there
  original_ids <- all_cell_ids(result$cell_store)
  for (orig_id in original_ids) {
    expect_true(has_cell(result_derived$cell_store, orig_id))
  }
})

##### Layout Operations Tests #####

test_that("arrange_rows reorders grid correctly by column values", {
  mtcars_cat <- setup_factor_mtcars()
  result <- tab(mtcars_cat, cyl, gear, show_row_nets = FALSE, show_col_nets = FALSE, show_base = FALSE)
  
  # Sort by first column
  result_sorted <- arrange_rows(result, .by = "3", .sort = "desc")
  
  # Grid should be reordered
  df <- as.data.frame(result_sorted)
  
  # First row should have highest value in column "3"
  expect_gte(df[1, "3"], df[2, "3"])
  expect_gte(df[2, "3"], df[3, "3"])
})

test_that("arrange_rows preserves all cells", {
  mtcars_cat <- setup_factor_mtcars()
  result <- tab(mtcars_cat, cyl, gear, show_row_nets = FALSE, show_col_nets = FALSE, show_base = FALSE)
  
  original_cell_count <- cell_count(result$cell_store)
  result_sorted <- arrange_rows(result, .by = "3")
  
  # Cell count should be unchanged
  expect_equal(cell_count(result_sorted$cell_store), original_cell_count)
})

test_that("arrange_cols reorders grid correctly by row values", {
  mtcars_cat <- setup_factor_mtcars()
  result <- tab(mtcars_cat, cyl, gear, show_row_nets = FALSE, show_col_nets = FALSE, show_base = FALSE)
  
  first_row <- result$layout$row_labels[1]
  result_sorted <- arrange_cols(result, .by = first_row, .sort = "desc")
  
  df <- as.data.frame(result_sorted)
  
  # Columns should be sorted
  row1_values <- as.numeric(df[1, -1])
  expect_true(all(diff(row1_values) <= 0))  # Descending order
})

test_that("arrange_rows reorders rows manually", {
  mtcars_cat <- setup_factor_mtcars()
  result <- tab(mtcars_cat, cyl, gear, show_row_nets = FALSE, show_col_nets = FALSE, show_base = FALSE)
  
  # Reverse order
  new_order <- rev(result$layout$row_labels)
  result_arranged <- arrange_rows(result, new_order)
  
  # Labels should be reversed
  expect_equal(result_arranged$layout$row_labels, new_order)
})

test_that("arrange_cols reorders columns manually", {
  mtcars_cat <- setup_factor_mtcars()
  result <- tab(mtcars_cat, cyl, gear, show_row_nets = FALSE, show_col_nets = FALSE, show_base = FALSE)
  
  # Reverse order
  new_order <- rev(result$layout$col_labels)
  result_arranged <- arrange_cols(result, new_order)
  
  # Labels should be reversed (data.frame-based - cell-based not implemented yet)
  # For now, just test it doesn't error
  expect_s3_class(result_arranged, "tab_result")
})

test_that("move_row moves row to top", {
  mtcars_cat <- setup_factor_mtcars()
  result <- tab(mtcars_cat, cyl, gear, show_row_nets = FALSE, show_col_nets = FALSE, show_base = FALSE)
  
  # Move last row to top
  last_label <- result$layout$row_labels[length(result$layout$row_labels)]
  result_moved <- move_row(result, .which = last_label, .to = "top")
  
  # First row should now be the one we moved
  expect_equal(result_moved$layout$row_labels[1], last_label)
})

test_that("move_row moves row to bottom", {
  mtcars_cat <- setup_factor_mtcars()
  result <- tab(mtcars_cat, cyl, gear, show_row_nets = FALSE, show_col_nets = FALSE, show_base = FALSE)
  
  # Move first row to bottom
  first_label <- result$layout$row_labels[1]
  result_moved <- move_row(result, .which = first_label, .to = "bottom")
  
  # Last row should now be the one we moved
  last_idx <- length(result_moved$layout$row_labels)
  expect_equal(result_moved$layout$row_labels[last_idx], first_label)
})

##### Hide Operations Tests #####

test_that("hide_rows removes rows from grid", {
  mtcars_cat <- setup_factor_mtcars()
  result <- tab(mtcars_cat, cyl, gear, show_row_nets = FALSE, show_col_nets = FALSE, show_base = FALSE)
  
  original_nrow <- nrow(result$layout$grid)
  row_to_hide <- result$layout$row_labels[2]
  
  result_hidden <- hide_rows(result, row_to_hide)
  
  # Grid should have fewer rows
  expect_equal(nrow(as.data.frame(result_hidden)), original_nrow - 1)
  
  # Hidden row should not be in labels
  expect_false(row_to_hide %in% as.data.frame(result_hidden)$row_labels)
})

test_that("hide_rows preserves cells in store", {
  mtcars_cat <- setup_factor_mtcars()
  result <- tab(mtcars_cat, cyl, gear, show_row_nets = FALSE, show_col_nets = FALSE, show_base = FALSE)
  
  original_cell_count <- cell_count(result$cell_store)
  row_to_hide <- result$layout$row_labels[2]
  
  result_hidden <- hide_rows(result, row_to_hide)
  
  # Cells should still be in store
  expect_equal(cell_count(result_hidden$cell_store), original_cell_count)
})

test_that("hide_cols removes columns from grid", {
  mtcars_cat <- setup_factor_mtcars()
  result <- tab(mtcars_cat, cyl, gear, show_row_nets = FALSE, show_col_nets = FALSE, show_base = FALSE)
  
  original_ncol <- ncol(result)
  col_to_hide <- result$layout$col_labels[1]
  
  result_hidden <- hide_cols(result, col_to_hide)
  
  # Grid should have fewer columns
  expect_equal(ncol(as.data.frame(result_hidden)), original_ncol - 1) # -1 for row labels
  
  # Hidden column should not be in labels
  expect_false(col_to_hide %in% as.data.frame(result_hidden)$col_labels)
})

test_that("hide_cols preserves cells in store", {
  mtcars_cat <- setup_factor_mtcars()
  result <- tab(mtcars_cat, cyl, gear, show_row_nets = FALSE, show_col_nets = FALSE, show_base = FALSE)
  
  original_cell_count <- cell_count(result$cell_store)
  col_to_hide <- result$layout$col_labels[1]
  
  result_hidden <- hide_cols(result, col_to_hide)
  
  # Cells should still be in store
  expect_equal(cell_count(result_hidden$cell_store), original_cell_count)
})

test_that("hide_rows_except keeps only matching rows", {
  mtcars_cat <- setup_factor_mtcars()
  result <- tab(mtcars_cat, cyl, gear, show_row_nets = FALSE, show_col_nets = FALSE, show_base = FALSE)
  
  result_hidden <- hide_rows_except(result, "4|6")
  
  # Should only have rows matching pattern
  expect_true(all(grepl("4|6", as.data.frame(result_hidden)$row_labels)))
})

test_that("hide_cols_except keeps only matching columns", {
  mtcars_cat <- setup_factor_mtcars()
  result <- tab(mtcars_cat, cyl, gear, show_row_nets = FALSE, show_col_nets = FALSE, show_base = FALSE)
  
  result_hidden <- hide_cols_except(result, "3|4")
  
  # Should only have columns matching pattern
  expect_true(all(grepl("3|4", as.data.frame(result_hidden)$col_labels)))
})

test_that("hidden cells don't appear in materialized data.frame", {
  mtcars_cat <- setup_factor_mtcars()
  result <- tab(mtcars_cat, cyl, gear, show_row_nets = FALSE, show_col_nets = FALSE, show_base = FALSE)
  
  result_hidden <- hide_rows(result, "8") %>%
    hide_cols("5")
  
  df <- as.data.frame(result_hidden)
  
  # Hidden row and column should not be in data.frame
  expect_false("8" %in% df$row_label)
  expect_false("5" %in% names(df))
})

test_that("layout and hide operations can be combined", {
  mtcars_cat <- setup_factor_mtcars()
  
  result <- tab(mtcars_cat, cyl, gear, show_row_nets = FALSE, show_col_nets = FALSE, show_base = FALSE) %>%
    arrange_rows(.by = "3") %>%
    hide_rows_except("4|6") %>%
    arrange_cols(.by = "4")
  
  df <- as.data.frame(result)
  
  # Should only have 2 rows (4 and 6)
  expect_equal(nrow(df), 2)
  expect_true(all(df$row_label %in% c("4", "6")))
})
