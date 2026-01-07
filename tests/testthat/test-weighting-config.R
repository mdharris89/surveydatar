## Weighting configuration tests
##
## Weighting configuration defines how downstream weighting functions interpret raw data columns and
## how constraints are identified/expanded. The tests in this file check defaults, custom overrides,
## input validation, and a small end-to-end configuration usage path.

test_that("create_weighting_config works with defaults", {
  config <- create_weighting_config()
  
  expect_s3_class(config, "weighting_config")
  expect_equal(config$cols$strata, "Country")
  expect_equal(config$cols$id, "respondent_id")
  expect_true("QCOUNTRY" %in% names(config$legacy_mappings))
  expect_equal(config$legacy_mappings$QCOUNTRY, "Country")
  
  # Check patterns
  expect_true("sum" %in% names(config$patterns))
  expect_true("occasion" %in% names(config$patterns))
  expect_true("demo" %in% names(config$patterns))
})

test_that("create_weighting_config accepts custom strata column", {
  config <- create_weighting_config(strata_col = "market")
  
  expect_equal(config$cols$strata, "market")
  expect_equal(config$cols$id, "respondent_id")  # Default unchanged
})

test_that("create_weighting_config accepts custom id column", {
  config <- create_weighting_config(id_col = "resp_id")
  
  expect_equal(config$cols$id, "resp_id")
  expect_equal(config$cols$strata, "Country")  # Default unchanged
})

test_that("create_weighting_config accepts demo_var_mapping", {
  mapping <- list(Gender = "SC1", Age = "SC2merged")
  config <- create_weighting_config(demo_var_mapping = mapping)
  
  expect_equal(config$demo_var_mapping, mapping)
})

test_that("create_weighting_config accepts custom legacy_mappings", {
  legacy <- list(QMARKET = "market", QREGION = "region")
  config <- create_weighting_config(legacy_mappings = legacy)
  
  expect_equal(config$legacy_mappings, legacy)
})

test_that("create_weighting_config can disable legacy_mappings", {
  config <- create_weighting_config(legacy_mappings = NULL)
  
  expect_null(config$legacy_mappings)
})

test_that("create_weighting_config accepts custom constraint_patterns", {
  patterns <- list(
    sum = "^Total:",
    occasion = "^Usage:",
    demo = "^Demographic:"
  )
  config <- create_weighting_config(constraint_patterns = patterns)
  
  expect_equal(config$patterns, patterns)
})

test_that("create_weighting_config validates inputs", {
  expect_error(
    create_weighting_config(strata_col = c("Country", "Market")),
    "strata_col must be a single character string"
  )
  
  expect_error(
    create_weighting_config(id_col = 123),
    "id_col must be a single character string"
  )
  
  expect_error(
    create_weighting_config(demo_var_mapping = "not a list"),
    "demo_var_mapping must be a named list or NULL"
  )
  
  expect_error(
    create_weighting_config(legacy_mappings = "not a list"),
    "legacy_mappings must be a named list or NULL"
  )
  
  expect_error(
    create_weighting_config(constraint_patterns = "not a list"),
    "constraint_patterns must be a named list"
  )
})

test_that("print.weighting_config works", {
  config <- create_weighting_config(
    strata_col = "market",
    demo_var_mapping = list(Gender = "SC1")
  )
  
  output <- capture.output(print(config))
  
  expect_true(any(grepl("Weighting Configuration", output)))
  expect_true(any(grepl("Strata: market", output)))
  expect_true(any(grepl("Gender -> SC1", output)))
})

test_that("default_weighting_config returns valid config", {
  config <- default_weighting_config()
  
  expect_s3_class(config, "weighting_config")
  expect_equal(config$cols$strata, "Country")
})

# Integration tests with run_unified_weighting

test_that("run_unified_weighting works with custom strata column", {
  skip_if_not_installed("CVXR")
  
  set.seed(123)
  n <- 200
  test_data <- data.frame(
    respondent_id = 1:n,
    market = sample(c("US", "UK"), n, replace = TRUE),
    gender = sample(c("Male", "Female"), n, replace = TRUE)
  )
  
  # Create config with custom strata
  config <- create_weighting_config(strata_col = "market")
  
  result <- run_unified_weighting(
    raw_data = test_data,
    stages = list(
      constraints = list(
        gender ~ c(Male = 0.48, Female = 0.52)
      )
    ),
    config = config,
    verbose = FALSE
  )
  
  expect_s3_class(result, "unified_weighting_result")
  expect_equal(nrow(result$weights), n)
})

test_that("run_unified_weighting works with demo_var_mapping", {
  skip_if_not_installed("CVXR")
  
  set.seed(123)
  n <- 200
  test_data <- data.frame(
    respondent_id = 1:n,
    SC1 = sample(c(1, 2), n, replace = TRUE),
    SC2 = sample(c(1, 2, 3), n, replace = TRUE)
  )
  
  # Add labels
  attr(test_data$SC1, "labels") <- c("Male" = 1, "Female" = 2)
  attr(test_data$SC2, "labels") <- c("Young" = 1, "Middle" = 2, "Old" = 3)
  
  # Create config with mapping
  config <- create_weighting_config(
    demo_var_mapping = list(
      Gender = "SC1",
      AgeGroup = "SC2"
    )
  )
  
  result <- run_unified_weighting(
    raw_data = test_data,
    stages = list(
      constraints = list(
        Gender ~ c(Male = 0.48, Female = 0.52)
      )
    ),
    config = config,
    verbose = FALSE
  )
  
  expect_s3_class(result, "unified_weighting_result")
  
  # Check that alias column was created
  final_data <- if (inherits(result$data, "survey_data")) {
    result$data$dat
  } else {
    result$data
  }
  expect_true("Gender" %in% names(final_data))
})

test_that("run_unified_weighting applies legacy mappings", {
  skip_if_not_installed("CVXR")
  
  set.seed(123)
  n <- 200
  test_data <- data.frame(
    respondent_id = 1:n,
    QCOUNTRY = sample(c(1, 2), n, replace = TRUE),
    gender = sample(c("Male", "Female"), n, replace = TRUE)
  )
  
  # Add labels to QCOUNTRY
  attr(test_data$QCOUNTRY, "labels") <- c("US" = 1, "UK" = 2)
  
  result <- run_unified_weighting(
    raw_data = test_data,
    stages = list(
      constraints = list(
        gender ~ c(Male = 0.48, Female = 0.52)
      )
    ),
    config = NULL,  # uses default config, which includes QCOUNTRY -> Country
    verbose = FALSE
  )
  
  expect_s3_class(result, "unified_weighting_result")
  
  # Check that Country column was created
  final_data <- if (inherits(result$data, "survey_data")) {
    result$data$dat
  } else {
    result$data
  }
  expect_true("Country" %in% names(final_data))
})

test_that("run_unified_weighting works without config", {
  skip_if_not_installed("CVXR")
  
  set.seed(123)
  n <- 200
  test_data <- data.frame(
    respondent_id = 1:n,
    Country = sample(c("US", "UK"), n, replace = TRUE),
    gender = sample(c("Male", "Female"), n, replace = TRUE)
  )
  
  # No config provided - should use defaults
  result <- run_unified_weighting(
    raw_data = test_data,
    stages = list(
      constraints = list(
        gender ~ c(Male = 0.48, Female = 0.52)
      )
    ),
    verbose = FALSE
  )
  
  expect_s3_class(result, "unified_weighting_result")
  expect_equal(nrow(result$weights), n)
})

test_that("config with custom id column works", {
  skip_if_not_installed("CVXR")
  
  set.seed(123)
  n <- 200
  test_data <- data.frame(
    resp_id = 1:n,
    gender = sample(c("Male", "Female"), n, replace = TRUE)
  )
  
  config <- create_weighting_config(id_col = "resp_id")
  
  result <- run_unified_weighting(
    raw_data = test_data,
    stages = list(
      constraints = list(
        gender ~ c(Male = 0.48, Female = 0.52)
      )
    ),
    config = config,
    verbose = FALSE
  )
  
  expect_s3_class(result, "unified_weighting_result")
  
  # Check that resp_id exists in final data
  final_data <- if (inherits(result$data, "survey_data")) {
    result$data$dat
  } else {
    result$data
  }
  expect_true("resp_id" %in% names(final_data))
})

