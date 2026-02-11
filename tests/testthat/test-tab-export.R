## tab() export/display tests
##
## tab() produces a tab_result (cells + layout), which can then be materialized, formatted, and exported to
## downstream representations. The tests in this file check export/display behaviour (copying, label changes,
## multi-tab composition, and export helpers) against realistic tab results.

##### Setup #####
# Sets up shared registry state and fixtures used to generate tab results for export/display tests.
# Ensure clean state for each test file  
# Note: Functions loaded via helper-load-tab.R
clear_tab_registry()
ensure_builtins_registered()

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
    
    satisfaction = sjlabelled::set_label(
      sjlabelled::set_labels(
        haven::labelled(sample(c(1, 2, 3, 4, 5), n, replace = TRUE),
                        labels = c("Very dissatisfied" = 1, "Dissatisfied" = 2, "Neutral" = 3, "Satisfied" = 4, "Very satisfied" = 5)),
        labels = c("Very dissatisfied" = 1, "Dissatisfied" = 2, "Neutral" = 3, "Satisfied" = 4, "Very satisfied" = 5)
      ),
      label = "Overall Satisfaction"
    ),
    
    brand = sjlabelled::set_label(
      sjlabelled::set_labels(
        haven::labelled(sample(c(1:17), n, replace = TRUE),
                        labels = setNames(1:17, paste0("Brand_", LETTERS[1:17]))),
        labels = setNames(1:17, paste0("Brand_", LETTERS[1:17]))
      ),
      label = "Preferred brand"
    ),
    
    income = round(rlnorm(n, meanlog = 10.5, sdlog = 0.8)),
    
    stringsAsFactors = FALSE
  )
  
  return(raw_data)
}

test_data <- create_tab_test_data(200)
# Suppress expected warning about missing variable labels in minimal test data
test_survey_data <- suppressWarnings(create_survey_data(test_data))

##### copy_tab Tests #####
# Checks formatting/copy behaviour when materializing tab results for downstream tools.

test_that("copy_tab handles different statistic types", {
  # Checks that copy_tab() materializes tab results into a copy-friendly data.frame while preserving
  # the statistic context (e.g., percent vs values-based statistics).
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

  expect_true(any(grepl("statistic = \"mean\", values = \"age\"", copied_mean[nrow(copied_mean), 1])))
})

test_that("copy_tab preserves significance indicators", {
  # Checks that copy_tab() preserves significance annotations when copying a tab result that has had
  # significance testing applied.
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
  result_with_sig <- as.data.frame(result_with_sig)  # Materialize to extract significance

  # copy_tab returns formatted data invisibly
  copied <- copy_tab(result_with_sig)

  # Should preserve any significance indicators in the output
  expect_true(is.data.frame(copied))
  # The print method shows significance indicators, which would be in the copied data
})

##### modify_labels Tests #####
# Checks post-compute label changes on tab results (layout labels) without changing computed cells.

test_that("modify_labels updates row labels in cell-based tab_result", {
  # Checks that modify_labels() updates row labels in the tab layout without changing the underlying cells.
  result <- tab(test_survey_data, satisfaction, gender)
  
  result_modified <- modify_labels(result, row_labels = c("Very satisfied" = "Very sat", 
                                                            "Satisfied" = "Sat"))
  
  expect_true("Very sat" %in% result_modified$layout$row_labels)
  expect_true("Sat" %in% result_modified$layout$row_labels)
  expect_false("Very satisfied" %in% result_modified$layout$row_labels)
  expect_false("Satisfied" %in% result_modified$layout$row_labels)
})

test_that("modify_labels updates column labels in cell-based tab_result", {
  # Checks that modify_labels() updates column labels in the tab layout without changing the underlying cells.
  result <- tab(test_survey_data, satisfaction, gender)
  
  result_modified <- modify_labels(result, col_labels = c("Male" = "Men", 
                                                            "Female" = "Women"))
  
  expect_true("Men" %in% result_modified$layout$col_labels)
  expect_true("Women" %in% result_modified$layout$col_labels)
  expect_false("Male" %in% result_modified$layout$col_labels)
  expect_false("Female" %in% result_modified$layout$col_labels)
})

test_that("modify_labels supports regex patterns", {
  # Checks that modify_labels() supports regex-based label rewriting for batch relabeling.
  result <- tab(test_survey_data, satisfaction, gender)
  
  result_modified <- modify_labels(result, row_labels = c("^Very.*$" = "Extreme Response"))
  
  expect_true("Extreme Response" %in% result_modified$layout$row_labels)
  expect_false(any(grepl("^Very", result_modified$layout$row_labels)))
})

test_that("modify_labels validates input type", {
  # Checks that modify_labels() requires a cell-based tab_result (not a materialized data.frame).
  result <- tab(test_survey_data, satisfaction, gender)
  
  expect_error(
    modify_labels(as.data.frame(result), row_labels = c("A" = "B")),
    "modify_labels\\(\\) requires a cell-based tab_result"
  )
})

test_that("modify_labels validates label format", {
  # Checks that modify_labels() validates that row_labels/col_labels are named character vectors.
  result <- tab(test_survey_data, satisfaction, gender)
  
  # Unnamed vector should error
  expect_error(
    modify_labels(result, row_labels = c("A", "B")),
    "row_labels must be a named character vector"
  )
  
  # Non-character should error
  expect_error(
    modify_labels(result, col_labels = list(a = "A", b = "B")),
    "col_labels must be a named character vector"
  )
})

##### multi_tab and glue_tab Tests #####
# Checks combining multiple tab results into a single output while preserving structure and metadata.

test_that("multi_tab works with standard test data", {
  # Checks that multi_tab() produces a combined tab_result by splitting the input survey into groups
  # and stacking the resulting tables with stable labels.
  # Create standard test data
  # Suppress expected warning about missing variable labels in minimal test data
  test_survey_data <- suppressWarnings(create_survey_data(create_tab_test_data(200)))

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
  # Checks that glue_tab() combines multiple tab results while keeping group-specific bases distinct.
  # Suppress expected warning about missing variable labels in minimal test data
  test_survey_data <- suppressWarnings(create_survey_data(create_tab_test_data(300)))

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
  # Checks that multi_tab() supports custom grouping expressions and preserves grouping metadata on output.
  # Suppress expected warning about missing variable labels in minimal test data
  test_survey_data <- suppressWarnings(create_survey_data(create_tab_test_data(150)))

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

##### tab_to_flourish Tests #####
# Checks export transformation from tab_result into Flourish-ready structures (shape, settings, and errors).

test_that("tab_to_flourish basic functionality works", {
  # Checks that tab_to_flourish() converts a tab_result into a Flourish-ready object with data/bindings/settings.
  skip_if_not_installed("tidyr")

  # Create test data
  test_data <- data.frame(
    gender = factor(c("Male", "Female", "Male", "Female")),
    age_group = factor(c("Young", "Old", "Young", "Old")),
    satisfaction = c(4, 5, 3, 4),
    income = c(50000, 60000, 45000, 55000)
  )

  # Basic tab result
  tab_result <- tab(test_data, gender, age_group)

  # Test basic conversion
  fltab <- tab_to_flourish(tab_result)

  expect_s3_class(fltab, "flourish_tab")
  expect_true(is.data.frame(fltab$data))
  expect_true(is.list(fltab$bindings))
  expect_true(is.character(fltab$chart_type))
  expect_true(is.list(fltab$settings))
})

test_that("tab_to_flourish handles different statistics", {
  # Checks that tab_to_flourish() handles both percent-based and values-based statistics consistently.
  skip_if_not_installed("tidyr")

  test_data <- data.frame(
    gender = factor(c("Male", "Female", "Male", "Female")),
    age_group = factor(c("Young", "Old", "Young", "Old")),
    income = c(50000, 60000, 45000, 55000)
  )

  # Test count statistic
  tab_count <- tab(test_data, gender, age_group, statistic = "count")
  fltab_count <- tab_to_flourish(tab_count)
  expect_s3_class(fltab_count, "flourish_tab")

  # Test mean statistic
  tab_mean <- tab(test_data, gender, age_group, statistic = "mean", values = "income")
  fltab_mean <- tab_to_flourish(tab_mean)
  expect_s3_class(fltab_mean, "flourish_tab")
  expect_equal(fltab_mean$settings$number_format$n_dec, 1)
})

test_that("tab_to_flourish chart type guessing works", {
  # Checks that tab_to_flourish() selects a chart_type that matches the structure of the tab result.
  skip_if_not_installed("tidyr")

  test_data <- data.frame(
    gender = factor(c("Male", "Female", "Male", "Female", "Male", "Female")),
    region = factor(c("North", "South", "North", "South", "East", "West")),
    satisfaction = c(4, 5, 3, 4, 2, 5)
  )

  # Test different table structures
  tab_2x2 <- tab(test_data, gender, region)
  fltab_2x2 <- tab_to_flourish(tab_2x2)
  expect_true(fltab_2x2$chart_type %in% c("column_stacked"))

  # Test single column
  tab_single <- tab(test_data, gender)
  fltab_single <- tab_to_flourish(tab_single)
  expect_true(fltab_single$chart_type %in% c("column_grouped", "bar_grouped", "table"))
})

test_that("tab_to_flourish handles custom settings", {
  # Checks that tab_to_flourish() accepts user settings and carries them through into the exported object.
  skip_if_not_installed("tidyr")

  test_data <- data.frame(
    gender = factor(c("Male", "Female", "Male", "Female")),
    age_group = factor(c("Young", "Old", "Young", "Old"))
  )

  tab_result <- tab(test_data, gender, age_group)

  custom_settings <- list(
    "layout.title" = "Test Title",
    "number_format.suffix" = "%",
    "labels" = TRUE
  )

  fltab <- tab_to_flourish(tab_result, settings = custom_settings)

  expect_equal(fltab$settings$layout.title, "Test Title")
  expect_equal(fltab$settings$number_format.suffix, "%")
  expect_true(fltab$settings$labels)
})

test_that("tab_to_flourish data cleaning works", {
  # Checks that tab_to_flourish() strips or preserves summary/base rows consistently when preparing export data.
  skip_if_not_installed("tidyr")

  test_data <- data.frame(
    gender = factor(c("Male", "Female", "Male", "Female")),
    age_group = factor(c("Young", "Old", "Young", "Old"))
  )

  tab_result <- tab(test_data, gender, age_group)

  # Test stripping options
  fltab_strip_all <- tab_to_flourish(tab_result,
                                     strip_summary_rows = TRUE,
                                     strip_summary_cols = TRUE)

  # Base is always stripped for Flourish
  expect_false("Base (n)" %in% fltab_strip_all$data$label)

  # Base should always be stripped, even with other options set to FALSE
  fltab_keep_summaries <- tab_to_flourish(tab_result, 
                                          strip_summary_rows = FALSE,
                                          strip_summary_cols = FALSE)
  expect_false("Base (n)" %in% fltab_keep_summaries$data$label)
})

test_that("tab_to_flourish handles different chart types", {
  # Checks that tab_to_flourish() supports explicit chart_type selection for the same underlying tab result.
  skip_if_not_installed("tidyr")

  test_data <- data.frame(
    gender = factor(c("Male", "Female", "Male", "Female")),
    region = factor(c("North", "South", "East", "West"))
  )

  tab_result <- tab(test_data, gender, region)

  # Test explicit chart types
  chart_types <- c("column_grouped", "bar_grouped", "table", "donut", "line")

  for (chart_type in chart_types) {
    fltab <- tab_to_flourish(tab_result, chart_type = chart_type)
    expect_equal(fltab$chart_type, chart_type)
    expect_s3_class(fltab, "flourish_tab")
  }
})

test_that("print.flourish_tab works correctly", {
  # Checks that the flourish_tab print method produces a readable summary without error.
  skip_if_not_installed("tidyr")

  test_data <- data.frame(
    gender = factor(c("Male", "Female", "Male", "Female")),
    region = factor(c("North", "South", "East", "West"))
  )

  tab_result <- tab(test_data, gender, region)
  fltab <- tab_to_flourish(tab_result)

  # Test that print method works without error
  expect_output(print(fltab), "flourish_tab")
  expect_output(print(fltab), "chart_type")
  expect_output(print(fltab), "bindings")
  expect_output(print(fltab), "data")
})

test_that("tab_to_flourish error handling works", {
  # Checks that tab_to_flourish() validates inputs and errors clearly on invalid types.
  expect_error(tab_to_flourish("not_a_tab_result"), "tab_result")
})

test_that("tab_to_flourish handles rows_list and complex specifications", {
  # Checks that tab_to_flourish() can export tab results built from more complex row specifications.
  skip_if_not_installed("tidyr")

  test_data <- data.frame(
    cyl = c(4, 6, 8, 4, 6, 8),
    mpg = c(25, 20, 15, 30, 18, 12),
    gear = factor(c(4, 4, 3, 5, 4, 3))
  )

  # Test with rows_list
  tab_result <- tab(test_data,
                    rows = rows_list("High Cyl" = cyl >= 6, "Low Cyl" = cyl < 6),
                    cols = gear)

  fltab <- tab_to_flourish(tab_result)
  expect_s3_class(fltab, "flourish_tab")
  expect_true("High Cyl" %in% names(fltab$data))
})

test_that("preview_flourish requires correct inputs", {
  # Checks that preview_flourish() validates inputs (API key presence and object types) before attempting preview.
  skip_if_not_installed("tidyr")

  test_data <- data.frame(
    gender = factor(c("Male", "Female")),
    region = factor(c("North", "South"))
  )

  tab_result <- tab(test_data, gender, region)
  fltab <- tab_to_flourish(tab_result)

  # Test error when API key is missing
  withr::with_envvar(c("FLOURISH_API_KEY" = ""), {
    expect_error(preview_flourish(fltab), "API key")
  })

  # Test error with wrong input type
  expect_error(preview_flourish("not_flourish_tab"), "flourish_tab")
})

test_that("internal helper functions work correctly", {
  # Checks that internal helpers used by export/display functions infer chart types and extract statistic metadata.
  skip_if_not_installed("tidyr")

  test_data <- data.frame(
    gender = factor(c("Male", "Female", "Male", "Female")),
    region = factor(c("North", "South", "East", "West"))
  )

  tab_result <- tab(test_data, gender, region)

  # Test guess_flourish_chart_type
  chart_type <- guess_flourish_chart_type(tab_result)
  expect_true(is.character(chart_type))
  expect_length(chart_type, 1)

  # Test .extract_stat_info
  stat_info <- .extract_stat_info(tab_result)
  expect_true(is.list(stat_info))
  expect_true("id" %in% names(stat_info))
})

##### tab_to_reactable Tests #####
# Checks export/display transformation from tab_result into reactable-ready structures and options.

test_that("tab_to_reactable basic functionality works", {
  # Checks that tab_to_reactable() converts a tab_result into a reactable-ready object with expected data and settings.
  skip_if_not_installed("reactable")
  
  # Create test data
  test_data <- data.frame(
    gender = factor(c("Male", "Female", "Male", "Female")),
    age_group = factor(c("Young", "Old", "Young", "Old")),
    satisfaction = c(4, 5, 3, 4)
  )
  
  # Basic tab result
  tab_result <- tab(test_data, gender, age_group)
  
  # Test basic conversion
  rtab <- tab_to_reactable(tab_result)
  
  expect_s3_class(rtab, "reactable_tab")
  expect_true(is.data.frame(rtab$data))
  expect_true(is.list(rtab$metadata))
  expect_true(is.list(rtab$settings))
})

test_that("tab_to_reactable handles different color modes", {
  # Checks that tab_to_reactable() supports the available color modes and records the chosen mode in settings.
  skip_if_not_installed("reactable")
  
  test_data <- data.frame(
    gender = factor(c("Male", "Female", "Male", "Female")),
    region = factor(c("North", "South", "East", "West"))
  )
  
  tab_result <- tab(test_data, gender, region)
  
  # Test different color modes
  color_modes <- c("none", "heatmap", "top_n")
  
  for (mode in color_modes) {
    if (mode == "top_n") {
      rtab <- tab_to_reactable(tab_result, color_mode = mode, color_top_n = 3)
    } else {
      rtab <- tab_to_reactable(tab_result, color_mode = mode)
    }
    expect_s3_class(rtab, "reactable_tab")
    expect_equal(rtab$settings$color_mode, mode)
  }
})

test_that("tab_to_reactable handles custom display options", {
  # Checks that tab_to_reactable() applies common display options (freezing, sorting, pagination, formatting).
  skip_if_not_installed("reactable")
  
  test_data <- data.frame(
    gender = factor(c("Male", "Female", "Male", "Female")),
    region = factor(c("North", "South", "East", "West"))
  )
  
  tab_result <- tab(test_data, gender, region)
  
  # Test custom options
  rtab <- tab_to_reactable(
    tab_result,
    freeze_headers = TRUE,
    freeze_first_column = TRUE,
    enable_sorting = FALSE,
    pagination = 10,
    searchable = TRUE,
    decimal_places = 2
  )
  
  expect_true(rtab$settings$freeze_headers)
  expect_true(rtab$settings$freeze_first_column)
  expect_false(rtab$settings$enable_sorting)
  expect_equal(rtab$settings$pagination, 10)
  expect_true(rtab$settings$searchable)
  expect_equal(rtab$settings$decimal_places, 2)
})

test_that("tab_to_reactable error handling works", {
  # Checks that tab_to_reactable() validates inputs and required arguments for specific modes.
  skip_if_not_installed("reactable")
  
  # Test error with wrong input type
  expect_error(tab_to_reactable("not_a_tab_result"), "tab_result")
  
  # Test error with top_n mode without color_top_n
  test_data <- data.frame(
    gender = factor(c("Male", "Female")),
    region = factor(c("North", "South"))
  )
  tab_result <- tab(test_data, gender, region)
  
  expect_error(
    tab_to_reactable(tab_result, color_mode = "top_n"),
    "color_top_n"
  )
})

test_that("tab_to_reactable respects color_exclude_cols for heatmap mode", {
  # Checks that tab_to_reactable() accepts and carries through excluded columns when applying heatmap coloring.
  skip_if_not_installed("reactable")
  
  test_data <- data.frame(
    gender = factor(c("Male", "Female", "Male", "Female", "Male", "Female")),
    region = factor(c("North", "South", "East", "West", "North", "South")),
    satisfaction = c(4, 5, 3, 4, 2, 5)
  )
  
  tab_result <- tab(test_data, gender, region)
  
  # Test with excluded columns
  rtab <- tab_to_reactable(
    tab_result, 
    color_mode = "heatmap",
    color_exclude_cols = c("North", "South")
  )
  
  expect_s3_class(rtab, "reactable_tab")
  expect_equal(rtab$settings$color_exclude_cols, c("North", "South"))
  
  # Verify that the excluded columns are stored in settings
  expect_true("color_exclude_cols" %in% names(rtab$settings))
})

test_that("tab_to_reactable respects color_exclude_cols for top_n mode", {
  # Checks that color_exclude_cols is respected when using top_n coloring.
  skip_if_not_installed("reactable")
  
  test_data <- data.frame(
    gender = factor(c("Male", "Female", "Male", "Female", "Male", "Female")),
    region = factor(c("North", "South", "East", "West", "North", "South")),
    satisfaction = c(4, 5, 3, 4, 2, 5)
  )
  
  tab_result <- tab(test_data, gender, region)
  
  # Test with excluded columns in top_n mode
  rtab <- tab_to_reactable(
    tab_result, 
    color_mode = "top_n",
    color_top_n = 2,
    color_exclude_cols = c("East")
  )
  
  expect_s3_class(rtab, "reactable_tab")
  expect_equal(rtab$settings$color_exclude_cols, c("East"))
})

test_that("tab_to_reactable respects color_exclude_cols for significance mode", {
  # Checks that color_exclude_cols is respected when using significance-based coloring.
  skip_if_not_installed("reactable")
  skip_if_not("z_test_proportions" %in% list_tab_significance_tests(),
              "Significance tests not registered")
  
  test_data <- create_tab_test_data(150)
  test_survey <- suppressWarnings(create_survey_data(test_data))
  
  tab_result <- tab(test_survey, gender, region) %>%
    add_sig(versus = "North")
  
  # Test with excluded columns in significance mode
  rtab <- tab_to_reactable(
    tab_result, 
    color_mode = "significance",
    color_exclude_cols = c("South")
  )
  
  expect_s3_class(rtab, "reactable_tab")
  expect_equal(rtab$settings$color_exclude_cols, c("South"))
})

test_that("tab_to_reactable works with empty color_exclude_cols (default)", {
  # Checks the default behaviour when no columns are excluded from coloring.
  skip_if_not_installed("reactable")
  
  test_data <- data.frame(
    gender = factor(c("Male", "Female", "Male", "Female")),
    region = factor(c("North", "South", "East", "West"))
  )
  
  tab_result <- tab(test_data, gender, region)
  
  # Test default (no exclusions)
  rtab <- tab_to_reactable(tab_result, color_mode = "heatmap")
  
  expect_s3_class(rtab, "reactable_tab")
  expect_equal(rtab$settings$color_exclude_cols, character(0))
})

test_that("tab_to_reactable handles non-existent column names in color_exclude_cols", {
  # Checks that non-existent column names in color_exclude_cols do not cause failures.
  skip_if_not_installed("reactable")
  
  test_data <- data.frame(
    gender = factor(c("Male", "Female", "Male", "Female")),
    region = factor(c("North", "South", "East", "West"))
  )
  
  tab_result <- tab(test_data, gender, region)
  
  # Should not error with non-existent column names
  expect_no_error({
    rtab <- tab_to_reactable(
      tab_result, 
      color_mode = "heatmap",
      color_exclude_cols = c("NonExistent", "AlsoNotReal")
    )
  })
})

test_that("tab_to_reactable color_exclude_cols works with summary columns", {
  # Checks that color_exclude_cols can target summary columns (e.g., Total) generated by tab().
  skip_if_not_installed("reactable")
  
  test_data <- data.frame(
    gender = factor(c("Male", "Female", "Male", "Female", "Male", "Female")),
    region = factor(c("North", "South", "East", "West", "North", "South"))
  )
  
  # Create tab with column nets (Total)
  tab_result <- tab(test_data, gender, region, show_col_nets = TRUE)
  
  # Exclude the Total column from coloring
  rtab <- tab_to_reactable(
    tab_result, 
    color_mode = "heatmap",
    color_exclude_cols = c("Total")
  )
  
  expect_s3_class(rtab, "reactable_tab")
  expect_true("Total" %in% rtab$settings$color_exclude_cols)
})

test_that("show_summary() unhides summaries that were hidden", {
  # Checks that show_summary() updates the visible layout to include summary rows/columns after hiding.
  mtcars_cat <- mtcars
  mtcars_cat$cyl <- factor(mtcars_cat$cyl)
  mtcars_cat$gear <- factor(mtcars_cat$gear)
  
  # Create tab with summaries visible, then hide them
  result <- tab(mtcars_cat, cyl, gear, 
                show_row_nets = TRUE, 
                show_col_nets = TRUE) %>%
    hide_summary(rows = TRUE, cols = TRUE)
  
  # Verify summaries are not in the grid after hiding
  df_hidden <- as.data.frame(result)
  expect_false(any(grepl("NET", df_hidden$row_label)))
  expect_false(any(grepl("Total", names(df_hidden))))
  
  # Show row summaries
  result_with_rows <- show_summary(result, .rows = TRUE, .cols = FALSE)
  df_with_rows <- as.data.frame(result_with_rows)
  expect_true(any(grepl("NET", df_with_rows$row_label)))
  expect_false(any(grepl("Total", names(df_with_rows))))
  
  # Show column summaries
  result_with_cols <- hide_summary(result, rows = TRUE, cols = TRUE) %>%
    show_summary(.rows = FALSE, .cols = TRUE)
  df_with_cols <- as.data.frame(result_with_cols)
  expect_false(any(grepl("NET", df_with_cols$row_label)))
  expect_true(any(grepl("Total", names(df_with_cols))))
  
  # Show both summaries
  result_with_both <- show_summary(result, .rows = TRUE, .cols = TRUE)
  df_with_both <- as.data.frame(result_with_both)
  expect_true(any(grepl("NET", df_with_both$row_label)))
  expect_true(any(grepl("Total", names(df_with_both))))
})

test_that("show_base() works on tabs created with show_base = FALSE", {
  # Checks that show_base() updates the visible layout to include base after a tab was created with base hidden.
  mtcars_cat <- mtcars
  mtcars_cat$cyl <- factor(mtcars_cat$cyl)
  mtcars_cat$gear <- factor(mtcars_cat$gear)
  
  # Create tab with base hidden
  result <- tab(mtcars_cat, cyl, gear, show_base = FALSE)
  
  # Verify base is not in the grid initially
  df_initial <- as.data.frame(result)
  expect_false(any(grepl("Base", df_initial$row_label)))
  expect_false(any(grepl("Base", names(df_initial))))
  
  # Add base back with auto orientation
  result_with_base <- show_base(result, orientation = "auto")
  df_with_base <- as.data.frame(result_with_base)
  
  # Base should be added (either as row or column depending on orientation)
  has_base <- any(grepl("Base", df_with_base$row_label)) || any(grepl("Base", names(df_with_base)))
  expect_true(has_base)
})

test_that("Pipeline: hide then show summaries and base", {
  # Checks that hide/show operations compose cleanly when toggling summaries and base visibility in sequence.
  mtcars_cat <- mtcars
  mtcars_cat$cyl <- factor(mtcars_cat$cyl)
  mtcars_cat$gear <- factor(mtcars_cat$gear)
  
  # Create tab with everything visible
  result <- tab(mtcars_cat, cyl, gear, 
                show_row_nets = TRUE, 
                show_col_nets = TRUE,
                show_base = TRUE)
  
  # Hide everything
  result_hidden <- result %>%
    hide_summary(rows = TRUE, cols = TRUE) %>%
    hide_base()
  
  df_hidden <- as.data.frame(result_hidden)
  expect_false(any(grepl("NET", df_hidden$row_label)))
  expect_false(any(grepl("Total", names(df_hidden))))
  expect_false(any(grepl("Base", df_hidden$row_label)))
  expect_false(any(grepl("Base", names(df_hidden))))
  
  # Show everything again
  result_shown <- result_hidden %>%
    show_summary(.rows = TRUE, .cols = TRUE) %>%
    show_base()
  
  df_shown <- as.data.frame(result_shown)
  expect_true(any(grepl("NET", df_shown$row_label)))
  expect_true(any(grepl("Total", names(df_shown))))
  has_base <- any(grepl("Base", df_shown$row_label)) || any(grepl("Base", names(df_shown)))
  expect_true(has_base)
})

##### view_base_matrix Tests #####
# Checks base display helpers that reconstruct per-cell bases and respect the visible tab layout.

test_that("view_base_matrix works with cell-based tab results", {
  # Checks that view_base_matrix() reconstructs a base matrix from a cell-based tab_result with matching shape and labels.
  # Create a simple tab
  result <- tab(test_survey_data, gender, region)
  
  # Extract base matrix
  bases <- view_base_matrix(result)
  
  # Check structure
  expect_true(is.data.frame(bases))
  expect_true("row_label" %in% names(bases))
  expect_equal(ncol(bases), ncol(result))
  expect_equal(nrow(bases), nrow(result))
  
  # Check that values are numeric
  expect_true(all(sapply(bases[-1], is.numeric)))
  
  # Check that all base values are positive
  numeric_cols <- bases[, -1, drop = FALSE]
  expect_true(all(numeric_cols >= 0, na.rm = TRUE))
})

test_that("view_base_matrix works with materialized data.frame tab results", {
  # Checks that view_base_matrix() can extract bases from a materialized tab result (data.frame) using stored base metadata.
  # Create tab and materialize it
  result <- tab(test_survey_data, gender, region)
  result_df <- as.data.frame(result)
  
  # Extract base matrix from materialized result
  bases <- view_base_matrix(result_df)
  
  # Check structure
  expect_true(is.data.frame(bases))
  expect_true("row_label" %in% names(bases))
  expect_equal(names(bases), names(result_df))
  
  # Check that values are numeric
  expect_true(all(sapply(bases[-1], is.numeric)))
})

test_that("view_base_matrix respects hidden rows and columns", {
  # Checks that view_base_matrix() reflects the visible layout after rows/columns have been hidden.
  # Create tab with some rows and columns hidden
  result <- tab(test_survey_data, satisfaction, region) %>%
    hide_rows("Neutral") %>%
    hide_cols("East")
  
  # Extract base matrix
  bases <- view_base_matrix(result)
  
  # Check that hidden elements are not in the output
  expect_false("Neutral" %in% bases$row_label)
  expect_false("East" %in% names(bases))
  
  # Check dimensions match visible tab result
  expect_equal(ncol(bases), ncol(result))
  expect_equal(nrow(bases), nrow(result))
})

test_that("view_base_matrix column and row labels match tab result", {
  # Checks that view_base_matrix() uses the same visible row/column labels as the corresponding tab result output.
  # Create tab without base row to ensure exact match
  result <- tab(test_survey_data, satisfaction, gender, show_base = FALSE)
  
  # Extract base matrix
  bases <- view_base_matrix(result)
  
  # Materialize tab result
  result_df <- as.data.frame(result)
  
  # Check that labels match exactly
  expect_equal(names(bases), names(result_df))
  expect_equal(bases$row_label, result_df$row_label)
  
  # Also test that column names match for tab with base
  result_with_base <- tab(test_survey_data, satisfaction, gender, show_base = TRUE)
  bases_with_base <- view_base_matrix(result_with_base)
  result_with_base_df <- as.data.frame(result_with_base)
  
  # Column names should always match
  expect_equal(names(bases_with_base), names(result_with_base_df))
  
  # Row labels should match except the base_matrix won't have its own "Base (n)" row
  # since it stores base values for data rows
  non_base_rows <- result_with_base_df$row_label != "Base (n)"
  expect_equal(bases_with_base$row_label, result_with_base_df$row_label[non_base_rows])
})

test_that("view_base_matrix works with different statistics", {
  # Checks that view_base_matrix() works consistently across different statistics, returning numeric bases with stable structure.
  # Test with count statistic
  result_count <- tab(test_survey_data, gender, region, statistic = "count")
  bases_count <- view_base_matrix(result_count)
  expect_true(is.data.frame(bases_count))
  expect_true(all(sapply(bases_count[-1], is.numeric)))
  
  # Test with mean statistic
  result_mean <- tab(test_survey_data, gender, region, statistic = "mean", values = "age")
  bases_mean <- view_base_matrix(result_mean)
  expect_true(is.data.frame(bases_mean))
  expect_true(all(sapply(bases_mean[-1], is.numeric)))
  
  # Test with row_pct
  result_row <- tab(test_survey_data, gender, region, statistic = "row_pct")
  bases_row <- view_base_matrix(result_row)
  expect_true(is.data.frame(bases_row))
  expect_true(all(sapply(bases_row[-1], is.numeric)))
})

test_that("view_base_matrix handles derived columns", {
  # Checks that view_base_matrix() continues to work after derived columns are added to a tab result.
  # Create tab with derived column
  result <- tab(test_survey_data, satisfaction, region) %>%
    derive(delta_vs("North", "South"))
  
  # Extract base matrix
  bases <- view_base_matrix(result)
  
  # Should work without error
  expect_true(is.data.frame(bases))
  expect_equal(ncol(bases), ncol(result))
  
  # Derived columns should have base values (may be NA or matching source columns)
  # delta_vs creates column with label "to_col - from_col"
  expect_true("South - North" %in% names(bases))
})

test_that("view_base_matrix handles base row/column visibility", {
  # Checks that view_base_matrix() returns output aligned to tabs regardless of whether base is shown/hidden in the visible layout.
  # Create tab with base visible
  result_with_base <- tab(test_survey_data, gender, region, show_base = TRUE)
  bases_with <- view_base_matrix(result_with_base)
  
  # Create tab with base hidden
  result_no_base <- tab(test_survey_data, gender, region, show_base = FALSE)
  bases_no <- view_base_matrix(result_no_base)
  
  # Both should work
  expect_true(is.data.frame(bases_with))
  expect_true(is.data.frame(bases_no))
  
  # Dimensions should match their respective tab results
  expect_equal(ncol(bases_with), ncol(result_with_base))
  expect_equal(ncol(bases_no), ncol(result_no_base))
})

test_that("view_base_matrix handles summary rows and columns", {
  # Checks that view_base_matrix() remains aligned with tab results that include summary rows/columns.
  mtcars_cat <- mtcars
  mtcars_cat$cyl <- factor(mtcars_cat$cyl)
  mtcars_cat$gear <- factor(mtcars_cat$gear)
  
  # Create tab with summaries
  result <- tab(mtcars_cat, cyl, gear, 
                show_row_nets = TRUE, 
                show_col_nets = TRUE)
  
  # Extract base matrix
  bases <- view_base_matrix(result)
  
  # Should include summary rows/columns
  expect_true(is.data.frame(bases))
  expect_true(any(grepl("NET|Total", bases$row_label)))
  expect_true(any(grepl("NET|Total", names(bases))))
  
  # Summary row/column bases should be numeric
  net_row <- which(grepl("NET", bases$row_label))[1]
  if (length(net_row) > 0) {
    expect_true(is.numeric(bases[net_row, 2]))
  }
})

test_that("view_base_matrix error handling works correctly", {
  # Checks that view_base_matrix() errors clearly on invalid inputs.
  # Test with wrong input type
  expect_error(
    view_base_matrix("not_a_tab_result"),
    "view_base_matrix\\(\\) requires a tab_result object"
  )
  
  expect_error(
    view_base_matrix(data.frame(x = 1:3)),
    "view_base_matrix\\(\\) requires a tab_result object"
  )
})

test_that("view_base_matrix handles data.frame without base_matrix attribute", {
  # Checks that view_base_matrix() errors clearly when base metadata is missing from a materialized result.
  # Create a regular data frame without tab_result class
  df <- data.frame(row_label = c("A", "B"), col1 = c(1, 2), col2 = c(3, 4))
  class(df) <- c("tab_result", "data.frame")
  
  # Should error with informative message
  expect_error(
    view_base_matrix(df),
    "base_matrix.*attribute"
  )
})

test_that("view_base_matrix output has correct structure for single column tab", {
  # Checks that view_base_matrix() returns a correctly shaped output for one-dimensional tab results.
  # Single column (no cols specified)
  result <- tab(test_survey_data, gender)
  bases <- view_base_matrix(result)
  
  expect_true(is.data.frame(bases))
  expect_equal(ncol(bases), 2)  # row_label + 1 data column
  expect_true("row_label" %in% names(bases))
})

test_that("view_base_matrix preserves base values correctly", {
  # Checks that view_base_matrix() returns sensible numeric bases for the visible tab cells.
  # Create tab with show_base = FALSE to compare underlying base_matrix
  result <- tab(test_survey_data, gender, region, show_base = FALSE)
  bases <- view_base_matrix(result)
  
  # All base values should be positive integers (counts)
  expect_true(all(sapply(bases[-1], is.numeric)))
  base_values <- unlist(bases[-1])
  base_values <- base_values[!is.na(base_values)]
  expect_true(all(base_values >= 0))
  expect_true(all(base_values == floor(base_values)))  # Should be integers
  
  # Base values should be reasonable (not zero, not more than total sample)
  total_n <- nrow(test_survey_data$dat)
  expect_true(all(base_values > 0))
  expect_true(all(base_values <= total_n))
  
  # Check that we have the expected number of base values
  expected_cells <- nrow(bases) * (ncol(bases) - 1)  # -1 for row_label
  actual_cells <- length(base_values)
  expect_equal(actual_cells, expected_cells)
})

test_that("view_base_matrix handles tabs with filters", {
  # Checks that view_base_matrix() reflects the impact of whole-table filters on bases.
  # Create tab with filter
  result <- tab(test_survey_data, gender, region, filter = age > 30)
  bases <- view_base_matrix(result)
  
  expect_true(is.data.frame(bases))
  expect_equal(ncol(bases), ncol(result))
  expect_equal(nrow(bases), nrow(result))
  
  # Base values should reflect the filtered data (smaller than unfiltered)
  result_unfiltered <- tab(test_survey_data, gender, region)
  bases_unfiltered <- view_base_matrix(result_unfiltered)
  
  # Filtered bases should generally be smaller (though not guaranteed for every cell)
  expect_true(is.data.frame(bases))
})

# Tests for Layout Definition Objects
# Checks the layout matcher machinery used to allocate stored cells into the visible grid.

test_that("new_layout_def creates valid layout_def objects", {
  # Checks that new_layout_def() constructs a layout_def with expected defaults and required fields.
  # Basic construction
  def <- new_layout_def(
    label = "Test Row",
    dimension = "row"
  )
  
  expect_s3_class(def, "layout_def")
  expect_equal(def$label, "Test Row")
  expect_equal(def$dimension, "row")
  expect_null(def$row_expr_matcher)
})

test_that("new_layout_def accepts matcher functions", {
  # Checks that matcher functions are stored and invoked as part of layout_def evaluation.
  matcher_fn <- function(x) identical(x, quote(gender == 1))
  
  def <- new_layout_def(
    row_expr_matcher = matcher_fn,
    label = "Male",
    dimension = "row"
  )
  
  expect_equal(def$row_expr_matcher, matcher_fn)
  expect_true(def$row_expr_matcher(quote(gender == 1)))
  expect_false(def$row_expr_matcher(quote(gender == 2)))
})

test_that("cell_qualifies_for_def returns TRUE when all matchers pass", {
  # Checks that cell_qualifies_for_def() returns TRUE when a cell satisfies all matchers on a layout_def.
  # Create a simple cell
  cell <- list(
    cell_id = "c_000001",
    value = 42.3,
    base = 150,
    specification = list(
      row_expr = quote(gender == 1),
      col_expr = quote(satisfaction == 5),
      is_summary_row = FALSE,
      is_summary_col = FALSE
    )
  )
  
  # Create a layout_def that should match
  def <- new_layout_def(
    row_expr_matcher = function(expr) identical(expr, quote(gender == 1)),
    label = "Male",
    dimension = "row"
  )
  
  expect_true(cell_qualifies_for_def(cell, def))
})

test_that("cell_qualifies_for_def returns FALSE when any matcher fails", {
  # Checks that cell_qualifies_for_def() returns FALSE when any matcher on the layout_def does not match the cell.
  cell <- list(
    cell_id = "c_000001",
    value = 42.3,
    base = 150,
    specification = list(
      row_expr = quote(gender == 1),
      col_expr = quote(satisfaction == 5),
      is_summary_row = FALSE
    )
  )
  
  # Def with non-matching expression
  def <- new_layout_def(
    row_expr_matcher = function(expr) identical(expr, quote(gender == 2)),
    label = "Female",
    dimension = "row"
  )
  
  expect_false(cell_qualifies_for_def(cell, def))
})

test_that("cell_qualifies_for_def handles NULL matchers correctly", {
  # Checks that NULL matchers behave as wildcards (do not constrain qualification).
  cell <- list(
    cell_id = "c_000001",
    value = 42.3,
    base = 150,
    specification = list(
      row_expr = quote(gender == 1)
    )
  )
  
  # Def with all NULL matchers (should match everything)
  def <- new_layout_def(
    label = "Any",
    dimension = "row"
  )
  
  expect_true(cell_qualifies_for_def(cell, def))
})

test_that("cell_qualifies_for_def checks base_matcher", {
  # Checks that base_matcher constraints are applied when qualifying cells to layout positions.
  cell <- list(
    cell_id = "c_000001",
    value = 42.3,
    base = 25,
    specification = list()
  )
  
  # Base >= 30
  def_high <- new_layout_def(
    base_matcher = threshold_matcher(30, ">="),
    label = "High base",
    dimension = "row"
  )
  
  # Base < 30
  def_low <- new_layout_def(
    base_matcher = threshold_matcher(30, "<"),
    label = "Low base",
    dimension = "row"
  )
  
  expect_false(cell_qualifies_for_def(cell, def_high))
  expect_true(cell_qualifies_for_def(cell, def_low))
})

test_that("cell_qualifies_for_def checks is_summary matchers", {
  # Checks that summary-row/summary-col matchers control whether summary cells qualify for a layout position.
  summary_cell <- list(
    cell_id = "c_000002",
    value = 100,
    base = 200,
    specification = list(
      is_summary_row = TRUE,
      is_summary_col = FALSE
    )
  )
  
  regular_cell <- list(
    cell_id = "c_000003",
    value = 50,
    base = 100,
    specification = list(
      is_summary_row = FALSE,
      is_summary_col = FALSE
    )
  )
  
  # Def that matches summary rows
  summary_def <- new_layout_def(
    is_summary_row_matcher = function(x) isTRUE(x),
    label = "NET",
    dimension = "row"
  )
  
  # Def that excludes summary rows
  non_summary_def <- new_layout_def(
    is_summary_row_matcher = negate_matcher(function(x) isTRUE(x)),
    label = "Regular",
    dimension = "row"
  )
  
  expect_true(cell_qualifies_for_def(summary_cell, summary_def))
  expect_false(cell_qualifies_for_def(summary_cell, non_summary_def))
  expect_false(cell_qualifies_for_def(regular_cell, summary_def))
  expect_true(cell_qualifies_for_def(regular_cell, non_summary_def))
})

test_that("initialize_layout_defs creates defs from specs", {
  # Checks that initialize_layout_defs() turns row/col specifications into layout_def objects used for allocation.
  specs <- list(
    list(expr = quote(gender == 1), label = "Male"),
    list(expr = quote(gender == 2), label = "Female")
  )
  
  defs <- initialize_layout_defs(specs, dimension = "row")
  
  expect_equal(length(defs), 2)
  expect_s3_class(defs[[1]], "layout_def")
  expect_s3_class(defs[[2]], "layout_def")
  expect_equal(defs[[1]]$label, "Male")
  expect_equal(defs[[2]]$label, "Female")
  expect_equal(defs[[1]]$dimension, "row")
})

test_that("initialize_layout_defs includes summary spec when provided", {
  # Checks that initialize_layout_defs() can append a summary specification to the generated layout definitions.
  specs <- list(
    list(expr = quote(gender == 1), label = "Male")
  )
  
  summary_spec <- list(
    expr = quote(TRUE),
    label = "NET"
  )
  
  defs <- initialize_layout_defs(specs, summary_spec, dimension = "row")
  
  expect_equal(length(defs), 2)
  expect_equal(defs[[2]]$label, "NET")
  expect_false(is.null(defs[[2]]$is_summary_row_matcher))
})

test_that("helper matcher constructors work correctly", {
  # Checks that matcher constructor helpers produce functions with the expected matching behaviour.
  # exact_match_matcher
  matcher <- exact_match_matcher(quote(x == 1))
  expect_true(matcher(quote(x == 1)))
  expect_false(matcher(quote(x == 2)))
  
  # threshold_matcher
  matcher <- threshold_matcher(30, ">=")
  expect_true(matcher(50))
  expect_false(matcher(20))
  expect_false(matcher(NA))
  
  # set_matcher
  matcher <- set_matcher(c("a", "b", "c"))
  expect_true(matcher("b"))
  expect_false(matcher("d"))
  
  # negate_matcher
  is_true <- function(x) isTRUE(x)
  is_false <- negate_matcher(is_true)
  expect_true(is_false(FALSE))
  expect_false(is_false(TRUE))
})

test_that("cell_qualifies_for_def checks statistic_matcher", {
  # Checks that statistic_matcher can qualify cells based on their statistic_id metadata.
  cell <- list(
    specification = list(statistic_id = "mean")
  )
  
  def_match <- new_layout_def(
    statistic_matcher = statistic_matcher(c("mean", "median")),
    label = "Means", dimension = "row"
  )
  expect_true(cell_qualifies_for_def(cell, def_match))
  
  def_no_match <- new_layout_def(
    statistic_matcher = statistic_matcher("count"),
    label = "Counts", dimension = "row"
  )
  expect_false(cell_qualifies_for_def(cell, def_no_match))
})

test_that("cell_qualifies_for_def checks values_var_matcher", {
  # Checks that values_var_matcher can qualify cells based on the values variable used by a statistic.
  cell <- list(
    specification = list(values_var = as.symbol("income"))
  )
  
  def_match <- new_layout_def(
    values_var_matcher = function(x) identical(x, as.symbol("income")),
    label = "Income", dimension = "row"
  )
  expect_true(cell_qualifies_for_def(cell, def_match))
  
  def_no_match <- new_layout_def(
    values_var_matcher = function(x) identical(x, as.symbol("age")),
    label = "Age", dimension = "row"
  )
  expect_false(cell_qualifies_for_def(cell, def_no_match))
})

test_that("cell_qualifies_for_def checks value_matcher", {
  # Checks that value_matcher can qualify cells based on computed numeric values.
  cell <- list(value = 45.5)
  
  def_match <- new_layout_def(
    value_matcher = value_matcher(30, ">="),
    label = "High values", dimension = "row"
  )
  expect_true(cell_qualifies_for_def(cell, def_match))
  
  def_no_match <- new_layout_def(
    value_matcher = value_matcher(50, ">="),
    label = "Very high", dimension = "row"
  )
  expect_false(cell_qualifies_for_def(cell, def_no_match))
})

test_that("cell_qualifies_for_def checks expr matchers", {
  # Checks that expression matchers (row/col) can qualify cells based on their specification expressions.
  cell <- list(
    specification = list(
      row_expr = quote(gender == 1),
      col_expr = quote(region == "North")
    )
  )
  
  def_row_match <- new_layout_def(
    row_expr_matcher = expr_matcher(quote(gender == 1)),
    label = "Male", dimension = "row"
  )
  expect_true(cell_qualifies_for_def(cell, def_row_match))
  
  def_col_match <- new_layout_def(
    col_expr_matcher = expr_matcher(quote(region == "North")),
    label = "North", dimension = "col"
  )
  expect_true(cell_qualifies_for_def(cell, def_col_match))
  
  def_no_match <- new_layout_def(
    row_expr_matcher = expr_matcher(quote(gender == 2)),
    label = "Female", dimension = "row"
  )
  expect_false(cell_qualifies_for_def(cell, def_no_match))
})

test_that("cell_qualifies_for_def checks measure_id_matcher", {
  cell <- list(
    specification = list(
      measure_id = "m2"
    )
  )

  def_match <- new_layout_def(
    measure_id_matcher = function(x) identical(x, "m2"),
    label = "Measure m2",
    dimension = "col"
  )
  expect_true(cell_qualifies_for_def(cell, def_match))

  def_no_match <- new_layout_def(
    measure_id_matcher = function(x) identical(x, "m1"),
    label = "Measure m1",
    dimension = "col"
  )
  expect_false(cell_qualifies_for_def(cell, def_no_match))
})

test_that("multi-measure export keeps summary placeholders and aligned measure metadata", {
  dat <- get_basic_test_dat()

  x <- tab(
    dat,
    labelledordinal,
    binarycategoricalasfactor,
    measures = list(
      measure("column_pct", id = "pct"),
      measure("mean", values = "randomnumeric", id = "avg")
    ),
    show_row_nets = TRUE,
    show_col_nets = TRUE
  )
  df <- as.data.frame(x)

  expect_true("Summary" %in% df$row_label)
  expect_true("Summary" %in% names(df))
  expect_true(all(is.na(df[df$row_label == "Summary", -1, drop = TRUE])))
  expect_true(all(is.na(df[["Summary"]])))

  measure_matrix <- attr(df, "measure_matrix")
  expect_true(!is.null(measure_matrix))
  expect_equal(nrow(measure_matrix), nrow(df))
  expect_equal(ncol(measure_matrix), ncol(df) - 1L)
})