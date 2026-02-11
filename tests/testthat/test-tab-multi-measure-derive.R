testthat::test_that("derive operations require measure scope on multi-measure tabs", {
  dat <- get_basic_test_dat()
  x <- tab(
    dat,
    labelledordinal,
    binarycategoricalasfactor,
    measures = list(
      measure("column_pct", id = "pct"),
      measure("mean", values = "randomnumeric", id = "avg")
    )
  )

  testthat::expect_error(
    derive(x, delta_vs("pct | No", "pct | Yes")),
    "ambiguous on multi-measure tables"
  )
})

testthat::test_that("delta_vs with measure scope preserves measure/stat metadata", {
  dat <- get_basic_test_dat()
  x <- tab(
    dat,
    labelledordinal,
    binarycategoricalasfactor,
    measures = list(
      measure("column_pct", id = "pct"),
      measure("mean", values = "randomnumeric", id = "avg")
    )
  )

  x2 <- derive(x, delta_vs("pct | No", "pct | Yes", measure = "pct"))
  new_col <- tail(x2$layout$col_labels, 1)
  testthat::expect_match(new_col, "^pct \\| Yes - pct \\| No$")

  col_idx <- ncol(x2$layout$grid)
  cell_id <- x2$layout$grid[1, col_idx]
  cell <- get_cell(x2$cell_store, cell_id)

  testthat::expect_equal(cell$specification$measure_id, "pct")
  testthat::expect_equal(cell$specification$statistic_id, "column_pct")
  testthat::expect_equal(cell$computation$statistic, "column_pct")
})

testthat::test_that("share_of_sum uses scoped measure blocks", {
  dat <- get_basic_test_dat()
  x <- tab(
    dat,
    labelledordinal,
    binarycategoricalasfactor,
    measures = list(
      measure("column_pct", id = "pct"),
      measure("mean", values = "randomnumeric", id = "avg")
    )
  )

  x2 <- derive(x, share_of_sum(by = "row", measure = "avg"))
  # Derived labels should be present and tied to avg scope.
  testthat::expect_true(any(grepl("% of Row", x2$layout$col_labels)))

  new_idx <- tail(seq_along(x2$layout$col_labels), 1)
  cell_id <- x2$layout$grid[1, new_idx]
  cell <- get_cell(x2$cell_store, cell_id)
  testthat::expect_equal(cell$specification$measure_id, "avg")
})
