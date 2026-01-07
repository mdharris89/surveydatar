# tab() computes cell values/bases from base_array + row/col arrays, then stores cells in a cell_store.
# Layout functions then allocate those stored cells to the visible grid (layout$grid).
# The tests in this file check core invariants across these stages, plus focused tests of internal helpers
# that build bases, synchronize base matrices, and construct per-cell specifications.

test_that("tab() computation produces cells consistent with statistic processor and base calculator", {
  # Checks that a cell pulled from layout$grid is consistent with recomputing that cell directly from
  # res$arrays using the statistic's processor and base_calculator.
  dat <- get_big_test_dat(n = 80, random_seed = 123)
  survey <- create_survey_data(dat)

  get_cell <- getFromNamespace("get_cell", "surveydatar")
  validate_cell_store <- getFromNamespace("validate_cell_store", "surveydatar")

  res <- tab(
    survey,
    rows = labelledordinal,
    cols = labelledcategorical,
    statistic = "column_pct"
  )

  expect_true(validate_cell_store(res$cell_store))

  i <- 1
  j <- 1
  cell_id <- res$layout$grid[i, j]
  cell <- get_cell(res$cell_store, cell_id)

  expect_type(cell$value, "double")
  expect_type(cell$base, "double")

  expected_value <- res$statistic$processor(
    base_array = res$arrays$base_array,
    row_m = res$arrays$row_arrays[[i]],
    row_u = res$arrays$row_u_arrays[[i]],
    col_m = res$arrays$col_arrays[[j]],
    col_u = res$arrays$col_u_arrays[[j]],
    values = res$arrays$values_array
  )
  expected_base <- res$statistic$base_calculator(
    base_array = res$arrays$base_array,
    row_m = res$arrays$row_arrays[[i]],
    row_u = res$arrays$row_u_arrays[[i]],
    col_m = res$arrays$col_arrays[[j]],
    col_u = res$arrays$col_u_arrays[[j]],
    values_array = res$arrays$values_array
  )

  expect_equal(cell$value, expected_value)
  expect_equal(cell$base, expected_base)
})

test_that("as.data.frame base row can be NA when bases differ by cell, but view_base_matrix retains full bases", {
  dat <- get_routing_test_dat(n = 120, random_seed = 123)
  survey <- create_survey_data(dat)

  # Checks how the per-cell bases computed during tab() are represented when materializing output.
  # For values-based statistics, bases can differ by cell (valid values), so as.data.frame() may show NA
  # in the Base row, while view_base_matrix() still exposes the underlying per-cell base matrix.
  res <- tab(
    survey,
    rows = screen1,
    cols = total(),
    statistic = "mean",
    values = "screen2",
    show_base = TRUE
  )

  df <- as.data.frame(res)
  base_label <- res$statistic$base_label

  expect_true(base_label %in% df$row_label)

  base_row <- df[df$row_label == base_label, , drop = FALSE]
  # Confirm the Base row contains NA (cannot be displayed consistently in a single row).
  expect_true(any(is.na(base_row[1, -1])))

  bases <- view_base_matrix(res)
  expect_true(nrow(bases) > 0)
  expect_true(ncol(bases) > 1)
  # Confirm the underlying per-cell base matrix contains real numeric bases.
  expect_true(any(!is.na(as.matrix(bases[, -1, drop = FALSE]))))
})

test_that("calculate_base_array applies filter and weights with expected NA/error behavior", {
  # Checks the internal base_array construction used by tab(): weights and whole-table filters are
  # converted into a numeric base_array that defines the effective population for all computations.
  calculate_base_array <- getFromNamespace("calculate_base_array", "surveydatar")

  dat <- data.frame(
    x = 1:5,
    w = c(1, 2, NA, 4, 5)
  )

  # Internal building block: filter/weight -> numeric base_array used by statistics.
  # Filter only: returns 0/1 indicator with NA treated as FALSE.
  thr <- 3
  base_f <- calculate_base_array(dat, filter_expr = quote(x > thr), weight_var = NULL, parent_env = environment())
  expect_equal(base_f, c(0, 0, 0, 1, 1))

  # Weight only: returns weights, with NA weights treated as 0.
  base_w <- calculate_base_array(dat, filter_expr = NULL, weight_var = "w", parent_env = environment())
  expect_equal(base_w, c(1, 2, 0, 4, 5))

  # Filter + weight: elementwise product of filter indicator and weights.
  base_fw <- calculate_base_array(dat, filter_expr = quote(x > thr), weight_var = "w", parent_env = environment())
  expect_equal(base_fw, c(0, 0, 0, 4, 5))

  # Error contract: filter must yield a logical vector of length nrow(data).
  expect_error(
    calculate_base_array(dat, filter_expr = quote(c(TRUE, FALSE)), weight_var = NULL, parent_env = environment()),
    "logical vector of length nrow\\(data\\)"
  )

  # Error contract: weight must exist, be numeric, and be non-negative.
  expect_error(
    calculate_base_array(dat, filter_expr = NULL, weight_var = "missing", parent_env = environment()),
    "not found"
  )
  expect_error(
    calculate_base_array(transform(dat, w = as.character(w)), filter_expr = NULL, weight_var = "w", parent_env = environment()),
    "must be numeric"
  )
  expect_error(
    calculate_base_array(transform(dat, w = c(1, -1, 1, 1, 1)), filter_expr = NULL, weight_var = "w", parent_env = environment()),
    "non-negative"
  )
})

test_that("sync_base_matrix computes base matrix matching statistic base_calculator", {
  # Checks that base matrices built from row/col arrays are consistent with applying the statistic's
  # base_calculator to each (row, col) pair.
  sync_base_matrix <- getFromNamespace("sync_base_matrix", "surveydatar")
  ensure_builtins_registered()

  stat <- getFromNamespace("get_statistic", "surveydatar")("column_pct")

  # Should equal applying stat$base_calculator at every (row, col) intersection.
  base_array <- c(1, 2, 3, 4, 5)
  row_arrays <- list(
    c(1, 0, 1, 0, 1),
    c(0, 1, 0, 1, 0)
  )
  col_arrays <- list(
    c(1, 1, 0, 0, 0),
    c(0, 0, 1, 1, 0),
    c(0, 0, 0, 0, 1)
  )

  bm <- sync_base_matrix(
    row_arrays = row_arrays,
    col_arrays = col_arrays,
    row_u_arrays = list(rep(1, length(base_array)), rep(1, length(base_array))),
    col_u_arrays = list(rep(1, length(base_array)), rep(1, length(base_array)), rep(1, length(base_array))),
    base_array = base_array,
    values_array = NULL,
    statistic = stat
  )

  expect_equal(dim(bm), c(length(row_arrays), length(col_arrays)))

  expected <- matrix(NA_real_, nrow = 2, ncol = 3)
  for (i in seq_along(row_arrays)) {
    for (j in seq_along(col_arrays)) {
      expected[i, j] <- stat$base_calculator(
        base_array = base_array,
        row_m = row_arrays[[i]],
        row_u = rep(1, length(base_array)),
        col_m = col_arrays[[j]],
        col_u = rep(1, length(base_array)),
        values_array = NULL
      )
    }
  }
  expect_equal(bm, expected)
})

test_that(".is_orthogonal detects overlapping memberships by row/col", {
  # Checks the internal orthogonality helper used to validate that a membership matrix does not assign
  # a respondent to multiple categories within a row/column.
  is_orthogonal <- getFromNamespace(".is_orthogonal", "surveydatar")

  # Orthogonality check:
  # - by='row': each row has <=1 non-zero per respondent
  # - by='col': each col has <=1 non-zero per respondent
  mat_ok <- matrix(
    c(1, 0,
      0, 1,
      0, 0),
    nrow = 3,
    byrow = TRUE
  )
  expect_true(is_orthogonal(mat_ok, by = "row"))
  expect_true(is_orthogonal(mat_ok, by = "col"))

  mat_bad_row <- matrix(
    c(1, 1,
      0, 0),
    nrow = 2,
    byrow = TRUE
  )
  expect_false(is_orthogonal(mat_bad_row, by = "row"))

  mat_bad_col <- matrix(
    c(1, 0,
      1, 0),
    nrow = 2,
    byrow = TRUE
  )
  expect_false(is_orthogonal(mat_bad_col, by = "col"))
})

test_that("build_cell_specification returns expected structure and normalized DSL fields", {
  # Checks the internal cell specification builder used during tab(): it should produce a stable
  # per-cell specification structure containing expressions plus normalized DSL metadata.
  build_cell_specification <- getFromNamespace("build_cell_specification", "surveydatar")

  dat <- get_big_test_dat(n = 20, random_seed = 123)
  dpdict <- create_dict_with_metadata(dat)

  # build_cell_specification() defines the semantic identity stored on each cell.
  # Minimal specs: enough structure for normalize_dsl() to run.
  row_spec <- list(expr = quote(labelledordinal == 1), dsl = quote(labelledordinal == 1))
  col_spec <- list(expr = quote(labelledcategorical == 1), dsl = quote(labelledcategorical == 1))

  # Arrays with meta (as created by parsing/expansion); need meta$dsl + variables for normalization.
  row_array <- rep(0, nrow(dat))
  col_array <- rep(0, nrow(dat))
  attr(row_array, "meta") <- list(dsl = quote(labelledordinal == 1), variables = list("labelledordinal"), tags = NULL, label = "Row")
  attr(col_array, "meta") <- list(dsl = quote(labelledcategorical == 1), variables = list("labelledcategorical"), tags = NULL, label = "Col")

  base_filter_expr <- quote(TRUE)
  base_filter_spec <- NULL

  spec <- build_cell_specification(
    row_spec = row_spec,
    col_spec = col_spec,
    row_array = row_array,
    col_array = col_array,
    base_filter_spec = base_filter_spec,
    base_filter_expr = base_filter_expr,
    data = dat,
    dpdict = dpdict,
    statistic_id = "count",
    values_var = "randomnumeric"
  )

  expect_true(is.list(spec))
  expect_true(all(c("base_expr", "row_expr", "col_expr", "dsl", "meta", "statistic_id", "values_var") %in% names(spec)))
  expect_true(is.list(spec$dsl))
  expect_true(all(c("row", "col", "base") %in% names(spec$dsl)))
  expect_true(isTRUE(is.symbol(spec$values_var)))
  expect_equal(as.character(spec$values_var), "randomnumeric")
  expect_equal(spec$statistic_id, "count")
})


