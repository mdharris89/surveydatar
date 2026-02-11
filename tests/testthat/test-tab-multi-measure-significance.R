testthat::test_that("add_sig partitions multi-measure blocks and resolves tests per block", {
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
  x_sig <- add_sig(x, versus = "first_col")

  # First data row, first pct column should use proportion test.
  pct_cell_id <- x_sig$layout$grid[1, which(x_sig$layout$measure_col_map == "pct")[1]]
  pct_cell <- get_cell(x_sig$cell_store, pct_cell_id)
  testthat::expect_equal(
    pct_cell$significance[[names(pct_cell$significance)[1]]]$test_used,
    "z_test_proportions"
  )

  # First data row, first avg column should use t-test.
  avg_cell_id <- x_sig$layout$grid[1, which(x_sig$layout$measure_col_map == "avg")[1]]
  avg_cell <- get_cell(x_sig$cell_store, avg_cell_id)
  testthat::expect_equal(
    avg_cell$significance[[names(avg_cell$significance)[1]]]$test_used,
    "t_test"
  )
})

testthat::test_that("add_sig_all works on multi-measure tabs", {
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

  x_sig <- add_sig_all(x)
  df <- as.data.frame(x_sig)
  sig <- attr(df, "significance")

  testthat::expect_true(is.list(sig))
  testthat::expect_true(length(sig) > 0)
})
