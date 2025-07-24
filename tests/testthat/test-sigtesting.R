# Setup test data
set.seed(42)
test_data <- get_big_test_dat(400)
survey_obj <- create_survey_data(test_data)
data_for_manual <- survey_obj$dat

# Add weight variable for weighted tests
test_data$weight_var <- pmax(0.3, pmin(3.0, rnorm(nrow(test_data), mean = 1.0, sd = 0.4)))

test_that("Z-test for proportions matches manual calculation", {
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
  tab_p_value <- sig_result$`labelledcategorical: North`$p_values[4, 2]

  # Test that manual and tab p-values match within tolerance
  expect_equal(tab_p_value, p_value_manual, tolerance = 0.0001)
  expect_true(!is.na(tab_p_value))
  expect_true(tab_p_value >= 0 && tab_p_value <= 1)
})

test_that("T-test for means matches manual calculation", {
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
  tab_p_value <- sig_result_t$`labelledcategorical: North`$p_values[1, 4]

  # Test that manual and tab p-values match
  expect_equal(tab_p_value, manual_t_test$p.value, tolerance = 0.0001)
  expect_true(!is.na(tab_p_value))
})

test_that("Chi-square test matches manual calculation", {
  # Run tab with chi-square test (without summary rows/columns)
  result_chi <- tab(test_data, labelledordinal, labelledcategorical,
                    statistic = "count",
                    show_column_net = FALSE,
                    show_column_total = FALSE)
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
  tab_p_value <- sig_result_mw$`labelledcategorical: North`$p_values[1, 3]

  expect_equal(tab_p_value, manual_mw_test$p.value, tolerance = 0.0001)
  expect_true(!is.na(tab_p_value))
})

test_that("Multiple comparison adjustments work correctly", {
  # Run without adjustment
  result_no_adjust <- tab(test_data, labelledordinal, labelledcategorical,
                          statistic = "column_pct")
  result_no_adjust <- add_sig(result_no_adjust, test = "z_test_proportions",
                              versus = "first_col", adjust = "none")

  # Extract raw p-values
  sig_no_adjust <- attr(result_no_adjust, "significance")
  raw_p_values <- as.vector(sig_no_adjust$`labelledcategorical: North`$p_values[, -1])
  raw_p_values <- raw_p_values[!is.na(raw_p_values)]

  # Test Bonferroni adjustment
  result_bonf <- tab(test_data, labelledordinal, labelledcategorical,
                     statistic = "column_pct")
  result_bonf <- add_sig(result_bonf, test = "z_test_proportions",
                         versus = "first_col", adjust = "bonferroni")

  sig_bonf <- attr(result_bonf, "significance")
  bonf_p_values <- as.vector(sig_bonf$`labelledcategorical: North`$p_values[, -1])
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
  bh_p_values <- as.vector(sig_bh$`labelledcategorical: North`$p_values[, -1])
  bh_p_values <- bh_p_values[!is.na(bh_p_values)]

  manual_bh <- p.adjust(raw_p_values, method = "BH")

  expect_equal(bh_p_values, manual_bh, tolerance = 0.0001)
})

test_that("Custom significance test works correctly", {
  # Create a simple "practical significance" test
  create_significance_test(
    id = "practical_sig",
    name = "Practical significance test",
    description = "Tests if absolute difference in proportions exceeds threshold",
    processor = function(base_array, row_array, col_array_1, col_array_2, threshold = 0.1, ...) {
      n1 <- sum(base_array * row_array * col_array_1)
      N1 <- sum(base_array * col_array_1)
      n2 <- sum(base_array * row_array * col_array_2)
      N2 <- sum(base_array * col_array_2)

      if (N1 == 0 || N2 == 0) {
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
  actual_p <- sig_result_practical$`labelledcategorical: North`$p_values[4, 2]

  expect_equal(actual_p, expected_p)
  expect_true(actual_p %in% c(0, 1)) # Should be binary decision

  # Clean up
  clear_tab_registry()
  ensure_builtins_registered()
})

test_that("Weighted z-test matches manual calculation", {
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
  tab_p_value <- sig_weighted$`labelledcategorical: North`$p_values[4, 2]

  expect_equal(tab_p_value, p_value_weighted, tolerance = 0.001)
  expect_true(!is.na(tab_p_value))
})

test_that("Weighted t-test matches manual calculation", {
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
  tab_p_value <- sig_result_wt$`labelledcategorical: North`$p_values[1, 4]

  expect_equal(tab_p_value, p_value_wt, tolerance = 0.001)
  expect_true(!is.na(tab_p_value))
})

test_that("Weighted data produces different results than unweighted", {
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

  unweighted_p <- sig_unweighted$`labelledcategorical: North`$p_values[4, 2]
  weighted_p <- sig_weighted$`labelledcategorical: North`$p_values[4, 2]

  # P-values should be different (though they could theoretically be the same)
  expect_true(!is.na(unweighted_p))
  expect_true(!is.na(weighted_p))
})
