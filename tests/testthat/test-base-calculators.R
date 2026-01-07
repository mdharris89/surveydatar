# tab() computes cell values/bases from base_array + row/col arrays, then stores cells in a cell_store.
# Each statistic chooses a base_calculator (e.g. column totals for column_pct; valid-value bases for mean).
# The tests in this file cover base calculator semantics directly and via small integration checks against tab().

test_that("base calculator functions compute correct weighted bases on simple inputs", {
  # Checks the base calculator semantics used by tab() statistics on small, explicit arrays.
  # Bases are weighted: base is the sum of base_array contributions (not unweighted n).
  base_array <- c(1, 2, 3, 4, 5)  # weights
  row_raw  <- c(1, 0, 1, NA, 1)
  col_raw  <- c(1, 1, 0, 1, NA)
  
  # Convert raw arrays to (m,u)
  row_u <- as.numeric(!is.na(row_raw))
  col_u <- as.numeric(!is.na(col_raw))
  row_m <- row_raw; row_m[is.na(row_m)] <- 0
  col_m <- col_raw; col_m[is.na(col_m)] <- 0

  # Cell base: intersection of row & col; NA in row/col arrays means “excluded”.
  expect_equal(
    base_cell_count(base_array, row_m, row_u, col_m, col_u),
    1
  )

  # Column base: denominator for column_pct (sum of weights where col_array == 1).
  expect_equal(
    base_column_total(base_array, row_m, row_u, col_m, col_u),
    1 + 2
  )

  # Row base: denominator for row_pct (sum of weights where row_array == 1).
  expect_equal(
    base_row_total(base_array, row_m, row_u, col_m, col_u),
    1 + 3
  )

  # Grand base: table-wide base (sum of weights).
  expect_equal(
    base_grand_total(base_array, row_m, row_u, col_m, col_u),
    sum(base_array)
  )
})

test_that("valid-value base calculators exclude NA values", {
  # Checks the valid-value base semantics used by values-based statistics (mean/median/sd/cv/pXX):
  # bases exclude respondents with NA in values_array.
  base_array   <- c(1, 2, 3, 4, 5)
  row_m    <- c(1, 1, 1, 1, 1)
  col_m    <- c(1, 1, 1, 1, 1)
  row_u    <- c(1, 1, 1, 1, 1)
  col_u    <- c(1, 1, 1, 1, 1)
  values_array <- c(10, NA, 20, NA, 30)

  expect_equal(
    base_cell_count_valid(base_array, row_m, row_u, col_m, col_u, values_array = values_array),
    1 + 3 + 5
  )

  expect_equal(
    base_column_total_valid(base_array, row_m, row_u, col_m, col_u, values_array = values_array),
    1 + 3 + 5
  )

  expect_equal(
    base_row_total_valid(base_array, row_m, row_u, col_m, col_u, values_array = values_array),
    1 + 3 + 5
  )
})

test_that("tab() cell bases align with exported base calculators", {
  # Checks that the bases stored on computed cells are consistent with recomputing bases from the arrays
  # produced by tab(), using the appropriate exported base calculator for the statistic.
  dat <- get_big_test_dat(n = 80, random_seed = 123)
  survey <- create_survey_data(dat)

  get_cell <- getFromNamespace("get_cell", "surveydatar")

  # column_pct: base should equal the column total base calculator.
  res <- tab(
    survey,
    rows = labelledordinal,
    cols = labelledcategorical,
    statistic = "column_pct"
  )

  base_array <- res$arrays$base_array
  row_m      <- res$arrays$row_arrays[[1]]
  row_u      <- res$arrays$row_u_arrays[[1]]
  col_m      <- res$arrays$col_arrays[[1]]
  col_u      <- res$arrays$col_u_arrays[[1]]

  cell_id <- res$layout$grid[1, 1]
  cell <- get_cell(res$cell_store, cell_id)

  expect_equal(
    cell$base,
    base_column_total(base_array, row_m, row_u, col_m, col_u)
  )

  # row_pct: base should equal the row total base calculator.
  res_row <- tab(
    survey,
    rows = labelledordinal,
    cols = labelledcategorical,
    statistic = "row_pct"
  )

  base_array_r <- res_row$arrays$base_array
  row_m_r      <- res_row$arrays$row_arrays[[1]]
  row_u_r      <- res_row$arrays$row_u_arrays[[1]]
  col_m_r      <- res_row$arrays$col_arrays[[1]]
  col_u_r      <- res_row$arrays$col_u_arrays[[1]]

  cell_id_r <- res_row$layout$grid[1, 1]
  cell_r <- get_cell(res_row$cell_store, cell_id_r)

  expect_equal(
    cell_r$base,
    base_row_total(base_array_r, row_m_r, row_u_r, col_m_r, col_u_r)
  )

  # mean (values stat): base should reflect valid (non-NA) values.
  res_mean <- tab(
    survey,
    rows = labelledordinal,
    cols = labelledcategorical,
    statistic = "mean",
    values = "randomnumeric"
  )
  base_array2 <- res_mean$arrays$base_array
  row_m2     <- res_mean$arrays$row_arrays[[1]]
  row_u2     <- res_mean$arrays$row_u_arrays[[1]]
  col_m2     <- res_mean$arrays$col_arrays[[1]]
  col_u2     <- res_mean$arrays$col_u_arrays[[1]]
  values_arr <- res_mean$arrays$values_array

  cell_id2 <- res_mean$layout$grid[1, 1]
  cell2 <- get_cell(res_mean$cell_store, cell_id2)

  expect_equal(
    cell2$base,
    base_cell_count_valid(base_array2, row_m2, row_u2, col_m2, col_u2, values_array = values_arr)
  )
})


