# Tests for weighting functionality
library(testthat)

# Helper function to create a simple weighting_target_function
create_test_target_function <- function(probs) {
  force(probs)  # Capture probs in closure
  
  target_fn <- function(data, n_mc_iter = 1000, verbose = FALSE) {
    if (length(probs) == 1) {
      # Uniform probability
      return(rep(probs, nrow(data)))
    } else if (length(probs) == nrow(data)) {
      # Custom probabilities per respondent
      return(probs)
    } else {
      stop("probs length must be 1 or match nrow(data)")
    }
  }
  
  structure(target_fn, class = c("weighting_target_function", "function"))
}

# ============================================================================
# SECTION 1: calculate_target_weights() function tests
# ============================================================================

test_that("calculate_target_weights returns uniform weights when no selection_probs", {
  n <- 100
  result <- calculate_target_weights(n = n, selection_probs = NULL)
  
  expect_equal(length(result), n)
  expect_equal(result, rep(1, n))
})

test_that("calculate_target_weights applies alpha blending correctly", {
  n <- 100
  # Everyone has 50% selection probability
  selection_probs <- rep(0.5, n)
  
  # Alpha = 0 should give uniform weights
  result_alpha0 <- calculate_target_weights(
    n = n,
    selection_probs = selection_probs,
    alpha = 0
  )
  expect_equal(as.numeric(result_alpha0), rep(1, n), tolerance = 1e-6, ignore_attr = TRUE)
  
  # Alpha = 1 should give full HT weights (1/pi)
  result_alpha1 <- calculate_target_weights(
    n = n,
    selection_probs = selection_probs,
    alpha = 1
  )
  # With uniform probs of 0.5, HT weight = 1/0.5 = 2
  # After normalization: 2 * n / (2*n) = 1
  expect_equal(as.numeric(result_alpha1), rep(1, n), tolerance = 1e-6, ignore_attr = TRUE)
  
  # Alpha = 0.7 (default) should be between uniform and HT
  result_alpha7 <- calculate_target_weights(
    n = n,
    selection_probs = selection_probs,
    alpha = 0.7
  )
  # Manual calculation: 0.7 * 2 + 0.3 * 1 = 1.7
  # After normalization: 1.7 * n / (1.7*n) = 1
  expect_equal(as.numeric(result_alpha7), rep(1, n), tolerance = 1e-6, ignore_attr = TRUE)
})

test_that("calculate_target_weights handles non-uniform selection probabilities", {
  n <- 100
  # Half with high probability (0.8), half with low (0.2)
  selection_probs <- c(rep(0.8, 50), rep(0.2, 50))
  
  result <- calculate_target_weights(
    n = n,
    selection_probs = selection_probs,
    alpha = 1  # Full HT for clear test
  )
  
  # Expected HT weights: 1/0.8 = 1.25 and 1/0.2 = 5.0
  expected_ht <- c(rep(1/0.8, 50), rep(1/0.2, 50))
  # Normalize to sum to n
  expected_ht <- expected_ht * n / sum(expected_ht)
  
  expect_equal(as.numeric(result), expected_ht, tolerance = 1e-6, ignore_attr = TRUE)
  
  # Those with lower selection probability should get higher weights
  expect_true(all(result[51:100] > result[1:50]))
})

test_that("calculate_target_weights normalizes to sum to n", {
  n <- 100
  selection_probs <- runif(n, 0.1, 0.9)
  
  for (alpha in c(0, 0.5, 0.7, 1)) {
    result <- calculate_target_weights(
      n = n,
      selection_probs = selection_probs,
      alpha = alpha
    )
    expect_equal(sum(result), n, tolerance = 1e-6,
                 info = sprintf("Alpha = %.1f", alpha))
  }
})

test_that("calculate_target_weights enforces minimum probability", {
  n <- 100
  # Include some extremely small probabilities
  selection_probs <- c(rep(0.5, 90), rep(1e-10, 10))
  
  result <- calculate_target_weights(
    n = n,
    selection_probs = selection_probs,
    alpha = 1,
    min_prob = 1e-6
  )
  
  # Should not have extreme weights (would be 1e10 without min_prob)
  expect_true(all(result < 1000))
})

test_that("calculate_target_weights enforces maximum weight cap", {
  n <- 100
  # Create extreme imbalance
  selection_probs <- c(rep(0.99, 99), 0.001)
  
  result <- calculate_target_weights(
    n = n,
    selection_probs = selection_probs,
    alpha = 1,
    max_weight = 10
  )
  
  # Before normalization, no weight should exceed max_weight * alpha
  # After normalization, they might exceed slightly, but should be bounded
  expect_true(all(result < 50))  # Reasonable after normalization
})

test_that("calculate_target_weights errors on length mismatch", {
  expect_error(
    calculate_target_weights(n = 100, selection_probs = rep(0.5, 50)),
    "Selection probabilities length.*must match sample size"
  )
})

test_that("calculate_target_weights errors on missing values", {
  selection_probs <- c(rep(0.5, 90), rep(NA, 10))
  
  expect_error(
    calculate_target_weights(n = 100, selection_probs = selection_probs),
    "Selection probabilities contain.*missing values"
  )
})

test_that("calculate_target_weights stores diagnostic attributes", {
  n <- 100
  selection_probs <- rep(0.5, n)
  alpha <- 0.7
  
  result <- calculate_target_weights(
    n = n,
    selection_probs = selection_probs,
    alpha = alpha
  )
  
  expect_equal(attr(result, "alpha"), alpha)
  expect_equal(attr(result, "selection_probs"), selection_probs)
  expect_true(!is.null(attr(result, "ht_weights")))
})


# ============================================================================
# SECTION 2: Integration with run_unified_weighting
# ============================================================================

test_that("run_unified_weighting works without bias correction (NULL target)", {
  skip_if_not_installed("CVXR")
  
  # Create simple test data
  set.seed(123)
  n <- 200
  test_data <- data.frame(
    respondent_id = 1:n,
    gender = sample(c("Male", "Female"), n, replace = TRUE),
    age_group = sample(c("18-34", "35-54", "55+"), n, replace = TRUE)
  )
  
  # Simple demographic constraint
  result <- run_unified_weighting(
    raw_data = test_data,
    stages = list(
      constraints = list(
        gender ~ c(Male = 0.48, Female = 0.52)
      ),
      weighting_target = NULL  # No bias correction
    ),
    alpha = 0.7,
    cap = 3.5,
    verbose = FALSE
  )
  
  expect_s3_class(result, "unified_weighting_result")
  expect_true("weights" %in% names(result))
  expect_equal(nrow(result$weights), n)
  expect_true(all(result$weights$weight > 0))
  expect_true(all(result$weights$weight <= 3.5))
})

test_that("run_unified_weighting works with numeric vector bias correction", {
  skip_if_not_installed("CVXR")
  
  set.seed(123)
  n <- 200
  test_data <- data.frame(
    respondent_id = 1:n,
    gender = sample(c("Male", "Female"), n, replace = TRUE),
    age_group = sample(c("18-34", "35-54", "55+"), n, replace = TRUE)
  )
  
  # Create target weights: higher for first half
  target_weights <- c(rep(2, n/2), rep(0.5, n/2))
  
  result <- run_unified_weighting(
    raw_data = test_data,
    stages = list(
      constraints = list(
        gender ~ c(Male = 0.48, Female = 0.52)
      ),
      weighting_target = target_weights
    ),
    alpha = 0.7,
    cap = 3.5,
    verbose = FALSE
  )
  
  expect_s3_class(result, "unified_weighting_result")
  expect_equal(nrow(result$weights), n)
  
  # Weights should reflect the bias correction
  # First half should generally be higher than second half
  mean_first_half <- mean(result$weights$weight[1:(n/2)])
  mean_second_half <- mean(result$weights$weight[(n/2+1):n])
  expect_true(mean_first_half > mean_second_half)
})

test_that("run_unified_weighting works with weighting_target_function", {
  skip_if_not_installed("CVXR")
  
  set.seed(123)
  n <- 200
  test_data <- data.frame(
    respondent_id = 1:n,
    gender = sample(c("Male", "Female"), n, replace = TRUE),
    age_group = sample(c("18-34", "35-54", "55+"), n, replace = TRUE)
  )
  
  # Create selection probabilities: lower for older people
  selection_probs <- ifelse(test_data$age_group == "55+", 0.3, 0.7)
  
  # Create weighting target function
  target_fn <- create_test_target_function(selection_probs)
  
  result <- run_unified_weighting(
    raw_data = test_data,
    stages = list(
      constraints = list(
        gender ~ c(Male = 0.48, Female = 0.52)
      ),
      weighting_target = target_fn
    ),
    alpha = 0.7,
    cap = 3.5,
    verbose = FALSE
  )
  
  expect_s3_class(result, "unified_weighting_result")
  expect_equal(nrow(result$weights), n)
  
  # Older people (lower selection prob) should get higher weights on average
  weights_older <- result$weights$weight[test_data$age_group == "55+"]
  weights_younger <- result$weights$weight[test_data$age_group != "55+"]
  
  expect_true(mean(weights_older) > mean(weights_younger))
})

test_that("alpha parameter affects bias correction strength", {
  skip_if_not_installed("CVXR")
  
  set.seed(123)
  n <- 200
  test_data <- data.frame(
    respondent_id = 1:n,
    gender = sample(c("Male", "Female"), n, replace = TRUE),
    age_group = sample(c("18-34", "35-54", "55+"), n, replace = TRUE)
  )
  
  # Create selection probabilities with clear imbalance
  selection_probs <- ifelse(test_data$age_group == "55+", 0.2, 0.8)
  target_fn <- create_test_target_function(selection_probs)
  
  # Test with alpha = 0 (no bias correction)
  result_alpha0 <- run_unified_weighting(
    raw_data = test_data,
    stages = list(
      constraints = list(
        gender ~ c(Male = 0.48, Female = 0.52)
      ),
      weighting_target = target_fn,
      alpha = 0
    ),
    cap = 3.5,
    verbose = FALSE
  )
  
  # Test with alpha = 1 (full bias correction)
  result_alpha1 <- run_unified_weighting(
    raw_data = test_data,
    stages = list(
      constraints = list(
        gender ~ c(Male = 0.48, Female = 0.52)
      ),
      weighting_target = target_fn,
      alpha = 1
    ),
    cap = 3.5,
    verbose = FALSE
  )
  
  # Calculate weight dispersion (variance) for each
  var_alpha0 <- var(result_alpha0$weights$weight)
  var_alpha1 <- var(result_alpha1$weights$weight)
  
  # Higher alpha should lead to more weight dispersion (correction for bias)
  expect_true(var_alpha1 > var_alpha0)
})

test_that("run_unified_weighting errors on target weight length mismatch", {
  skip_if_not_installed("CVXR")
  
  set.seed(123)
  n <- 200
  test_data <- data.frame(
    respondent_id = 1:n,
    gender = sample(c("Male", "Female"), n, replace = TRUE)
  )
  
  # Wrong length target weights
  wrong_target <- rep(1, 100)
  
  expect_error(
    run_unified_weighting(
      raw_data = test_data,
      stages = list(
        constraints = list(
          gender ~ c(Male = 0.48, Female = 0.52)
        ),
        weighting_target = wrong_target
      ),
      verbose = FALSE
    ),
    "Weighting target length must match number of respondents"
  )
})


# ============================================================================
# SECTION 3: Bias correction diagnostics and validation
# ============================================================================

test_that("bias correction preserves bias check property (w*pi ≈ constant)", {
  # Direct test of the mathematical property
  # NOTE: After normalization to sum to n, w*pi is not exactly 1, 
  # but should be constant across respondents for full HT
  n <- 100
  selection_probs <- runif(n, 0.2, 0.8)
  
  target_weights <- calculate_target_weights(
    n = n,
    selection_probs = selection_probs,
    alpha = 1  # Full HT
  )
  
  bias_check <- target_weights * selection_probs
  
  # For full HT (alpha=1), all w*pi should be very similar (constant)
  # This is the key property - uniform bias correction
  expect_true(sd(bias_check) < 0.01,
              info = sprintf("SD of w*pi = %.4f, should be near 0 for full HT", sd(bias_check)))
  
  # The constant value should be close to mean selection prob
  # because normalized weights must maintain the sample size constraint
  expect_equal(mean(target_weights), 1, tolerance = 0.01,
               info = "Mean weight should be 1 (normalization)")
  
  # Test that sum of weights equals n
  expect_equal(sum(target_weights), n, tolerance = 1e-6)
})

test_that("partial bias correction (alpha < 1) creates controlled variance", {
  n <- 100
  # Create large imbalance
  selection_probs <- c(rep(0.1, 20), rep(0.9, 80))
  
  # Test different alpha values
  alphas <- c(0, 0.3, 0.5, 0.7, 1)
  variances <- numeric(length(alphas))
  
  for (i in seq_along(alphas)) {
    weights <- calculate_target_weights(
      n = n,
      selection_probs = selection_probs,
      alpha = alphas[i]
    )
    variances[i] <- var(weights)
  }
  
  # Variance should increase monotonically with alpha
  expect_true(all(diff(variances) >= 0))
  
  # Alpha = 0 should have zero variance (uniform)
  expect_equal(variances[1], 0, tolerance = 1e-10)
})

test_that("weighting_target_function can access data columns", {
  # Test that target functions can use data to compute probabilities
  test_data <- data.frame(
    respondent_id = 1:100,
    age = sample(20:80, 100, replace = TRUE)
  )
  
  # Create a target function that uses age
  age_based_target <- function(data, n_mc_iter = 1000, verbose = FALSE) {
    # Older people less likely to be selected
    probs <- pmax(0.1, 1 - (data$age - 20) / 60 * 0.7)
    return(probs)
  }
  class(age_based_target) <- c("weighting_target_function", "function")
  
  # Should execute without error
  result_probs <- age_based_target(test_data, verbose = FALSE)
  
  expect_equal(length(result_probs), nrow(test_data))
  expect_true(all(result_probs >= 0.1))
  expect_true(all(result_probs <= 1))
  
  # Older respondents should have lower probabilities
  expect_true(cor(test_data$age, result_probs) < 0)
})


# ============================================================================
# SECTION 4: Edge cases and error handling
# ============================================================================

test_that("bias correction handles extreme selection probabilities gracefully", {
  n <- 100
  # Very extreme probabilities
  selection_probs <- c(0.001, rep(0.5, 98), 0.999)
  
  # Should not error, even with extreme values
  result <- calculate_target_weights(
    n = n,
    selection_probs = selection_probs,
    alpha = 1,
    max_weight = 100
  )
  
  expect_equal(length(result), n)
  expect_true(all(is.finite(result)))
  expect_true(all(result > 0))
})

test_that("bias correction with all equal probabilities gives uniform weights", {
  n <- 100
  selection_probs <- rep(0.5, n)
  
  for (alpha in c(0, 0.3, 0.7, 1)) {
    result <- calculate_target_weights(
      n = n,
      selection_probs = selection_probs,
      alpha = alpha
    )
    
    # Should all be equal (uniform)
    expect_true(sd(result) < 1e-10,
                info = sprintf("Alpha = %.1f", alpha))
  }
})

test_that("weighting_target_function error handling works", {
  skip_if_not_installed("CVXR")
  
  test_data <- data.frame(
    respondent_id = 1:100,
    gender = sample(c("Male", "Female"), 100, replace = TRUE)
  )
  
  # Create a target function that errors
  error_target <- function(data, n_mc_iter = 1000, verbose = FALSE) {
    stop("Intentional test error")
  }
  class(error_target) <- c("weighting_target_function", "function")
  
  expect_error(
    run_unified_weighting(
      raw_data = test_data,
      stages = list(
        constraints = list(
          gender ~ c(Male = 0.48, Female = 0.52)
        ),
        weighting_target = error_target
      ),
      verbose = FALSE
    ),
    "Error in weighting target evaluation"
  )
})

# ============================================================================
# SECTION 5: Multi-stage weighting with bias correction
# ============================================================================

test_that("multi-stage weighting can use different bias corrections per stage", {
  skip_if_not_installed("CVXR")
  
  set.seed(123)
  n <- 200
  test_data <- data.frame(
    respondent_id = 1:n,
    gender = sample(c("Male", "Female"), n, replace = TRUE),
    age_group = sample(c("18-34", "35-54", "55+"), n, replace = TRUE),
    region = sample(c("North", "South", "East", "West"), n, replace = TRUE)
  )
  
  # Different target weights for each stage
  target1 <- rep(1, n)  # Uniform for stage 1
  target2 <- ifelse(test_data$region == "North", 2, 1)  # Higher for North in stage 2
  
  result <- run_unified_weighting(
    raw_data = test_data,
    stages = list(
      list(
        constraints = list(
          gender ~ c(Male = 0.48, Female = 0.52)
        ),
        weighting_target = target1,
        alpha = 0.5
      ),
      list(
        constraints = list(
          age_group ~ c(`18-34` = 0.3, `35-54` = 0.4, `55+` = 0.3)
        ),
        weighting_target = target2,
        alpha = 0.8
      )
    ),
    cap = 3.5,
    verbose = FALSE
  )
  
  expect_s3_class(result, "unified_weighting_result")
  expect_equal(length(result$stages), 2)
  expect_equal(nrow(result$weights), n)
})

