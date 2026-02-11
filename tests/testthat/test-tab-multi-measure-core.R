testthat::test_that("tab() enforces explicit measure() syntax and legacy incompatibility", {
  dat <- get_basic_test_dat()

  testthat::expect_error(
    tab(
      dat,
      labelledordinal,
      binarycategoricalasfactor,
      measures = list("column_pct")
    ),
    "must be created with measure\\(\\)"
  )

  testthat::expect_error(
    tab(
      dat,
      labelledordinal,
      binarycategoricalasfactor,
      statistic = "column_pct",
      measures = list(measure("column_pct"))
    ),
    "do not also supply legacy"
  )
})

testthat::test_that("multi-measure tab materializes with measure metadata and D1 placeholders", {
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
  meas_mat <- attr(df, "measure_matrix")

  testthat::expect_true(inherits(x, "tab_cell_collection"))
  testthat::expect_true(!is.null(meas_mat))
  testthat::expect_equal(nrow(meas_mat), nrow(df))
  testthat::expect_equal(ncol(meas_mat), ncol(df) - 1L)
  testthat::expect_true(any(grepl("^pct \\| ", names(df))))
  testthat::expect_true(any(grepl("^avg \\| ", names(df))))

  # D1: summary placeholders should exist (if requested) and be NA.
  testthat::expect_true("Summary" %in% df$row_label)
  summary_row <- which(df$row_label == "Summary")[1]
  testthat::expect_true(all(is.na(df[summary_row, -1, drop = TRUE])))
  testthat::expect_true("Summary" %in% names(df))
  testthat::expect_true(all(is.na(df[["Summary"]])))
})

testthat::test_that("multi-measure D2 base display uses cross-measure consistency", {
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
  df <- as.data.frame(x)
  base_row <- df[df$row_label == "Base (n)", , drop = FALSE]

  testthat::expect_equal(nrow(base_row), 1L)
  # If a logical column has disagreement across measures, all paired presentations are NA.
  # The first pair is No.
  testthat::expect_true(is.na(base_row[["pct | No"]]))
  testthat::expect_true(is.na(base_row[["avg | No"]]))
})

testthat::test_that("measure_axis rows creates row measure blocks", {
  dat <- get_basic_test_dat()

  x <- tab(
    dat,
    labelledordinal,
    binarycategoricalasfactor,
    measures = list(
      measure("column_pct", id = "pct"),
      measure("mean", values = "randomnumeric", id = "avg")
    ),
    measure_axis = "rows"
  )

  testthat::expect_true(!is.null(x$layout$measure_row_map))
  testthat::expect_true(is.null(x$layout$measure_col_map))
  testthat::expect_true(any(grepl("^pct \\| ", x$layout$row_labels)))
  testthat::expect_true(any(grepl("^avg \\| ", x$layout$row_labels)))
})
