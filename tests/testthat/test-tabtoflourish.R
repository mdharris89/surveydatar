test_that("tab_to_flourish basic functionality works", {
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
  skip_if_not_installed("tidyr")

  test_data <- data.frame(
    gender = factor(c("Male", "Female", "Male", "Female")),
    age_group = factor(c("Young", "Old", "Young", "Old"))
  )

  tab_result <- tab(test_data, gender, age_group)

  # Test stripping options
  fltab_strip_all <- tab_to_flourish(tab_result,
                                     strip_base = TRUE,
                                     strip_summary_rows = TRUE,
                                     strip_summary_cols = TRUE)

  expect_false("Base (n)" %in% fltab_strip_all$data$row_label)

  fltab_keep_base <- tab_to_flourish(tab_result, strip_base = FALSE)
  expect_true("Base (n)" %in% fltab_keep_base$data$row_label)
})

test_that("tab_to_flourish handles different chart types", {
  skip_if_not_installed("tidyr")

  test_data <- data.frame(
    gender = factor(c("Male", "Female", "Male", "Female")),
    region = factor(c("North", "South", "East", "West"))
  )

  tab_result <- tab(test_data, gender, region)

  # Test explicit chart types
  chart_types <- c("column_grouped", "bar_grouped", "table", "pie", "line")

  for (chart_type in chart_types) {
    fltab <- tab_to_flourish(tab_result, chart_type = chart_type)
    expect_equal(fltab$chart_type, chart_type)
    expect_s3_class(fltab, "flourish_tab")
  }
})

test_that("tab_to_flourish percent scaling works", {
  skip_if_not_installed("tidyr")

  test_data <- data.frame(
    gender = factor(c("Male", "Female", "Male", "Female")),
    region = factor(c("North", "South", "East", "West"))
  )

  tab_result <- tab(test_data, gender, region, statistic = "column_pct")

  # Test auto scaling
  fltab_auto <- tab_to_flourish(tab_result, percent_scale = "auto")
  expect_s3_class(fltab_auto, "flourish_tab")

  # Test 0-1 scaling
  fltab_01 <- tab_to_flourish(tab_result, percent_scale = "0-1")
  expect_s3_class(fltab_01, "flourish_tab")

  # Test 0-100 scaling
  fltab_100 <- tab_to_flourish(tab_result, percent_scale = "0-100")
  expect_s3_class(fltab_100, "flourish_tab")
})

test_that("print.flourish_tab works correctly", {
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
  expect_error(tab_to_flourish("not_a_tab_result"), "tab_result")

  # Test with invalid percent_scale
  test_data <- data.frame(
    gender = factor(c("Male", "Female")),
    region = factor(c("North", "South"))
  )

  tab_result <- tab(test_data, gender, region)
  expect_error(tab_to_flourish(tab_result, percent_scale = "invalid"), "arg")
})

test_that("tab_to_flourish handles rows_list and complex specifications", {
  skip_if_not_installed("tidyr")

  test_data <- data.frame(
    cyl = c(4, 6, 8, 4, 6, 8),
    mpg = c(25, 20, 15, 30, 18, 12),
    gear = c(4, 4, 3, 5, 4, 3)
  )

  # Test with rows_list
  tab_result <- tab(test_data,
                    rows = rows_list("High Cyl" = cyl >= 6, "Low Cyl" = cyl < 6),
                    cols = gear)

  fltab <- tab_to_flourish(tab_result)
  expect_s3_class(fltab, "flourish_tab")
  expect_true("High Cyl" %in% fltab$data$row_label ||
                any(grepl("High Cyl", fltab$data$row_label)))
})

test_that("preview_flourish requires correct inputs", {
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
  stat_info <- surveydatar:::.extract_stat_info(tab_result)
  expect_true(is.list(stat_info))
  expect_true("id" %in% names(stat_info))
})
