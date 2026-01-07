## among() helper tests
##
## among(x, universe) attaches an explicit eligibility mask (u) to a spec.
## These tests verify:
## - among() produces the same (m,u) behaviour as manual NA-gating
## - tab() results (cell values + bases) are identical under both approaches
## - clear errors for length/type mismatches

ensure_builtins_registered()

test_that("among() matches manual NA-gating for arrays, cell values, and bases", {
  set.seed(123)
  n <- 250

  dat <- data.frame(
    uid = seq_len(n),
    region = factor(sample(c("North", "East", "South"), n, replace = TRUE)),
    aware_a = sample(c(0, 1), n, replace = TRUE, prob = c(0.4, 0.6)),
    consider_a_all = sample(c(0L, 1L, NA_integer_), n, replace = TRUE, prob = c(0.45, 0.45, 0.10))
  )

  res_among <- tab(
    dat,
    rows = among(consider_a_all == 1, aware_a == 1),
    cols = region,
    statistic = "column_pct",
    show_base = TRUE
  )

  res_manual <- tab(
    dat,
    rows = ifelse(aware_a == 1, consider_a_all == 1, NA),
    cols = region,
    statistic = "column_pct",
    show_base = TRUE
  )

  # 1) Arrays: among() should produce identical (m,u) as NA-gating
  expect_equal(
    res_among$arrays$row_arrays[[1]],
    res_manual$arrays$row_arrays[[1]],
    ignore_attr = TRUE
  )
  expect_equal(
    res_among$arrays$row_u_arrays[[1]],
    res_manual$arrays$row_u_arrays[[1]]
  )

  # 2) Cell values + bases: compare the single data row across all columns
  get_cell <- getFromNamespace("get_cell", "surveydatar")

  base_label <- res_among$statistic$base_label

  data_row_label_among <- setdiff(res_among$layout$row_labels, base_label)
  data_row_label_manual <- setdiff(res_manual$layout$row_labels, base_label)

  expect_equal(length(data_row_label_among), 1)
  expect_equal(length(data_row_label_manual), 1)

  i_among <- which(res_among$layout$row_labels == data_row_label_among)
  i_manual <- which(res_manual$layout$row_labels == data_row_label_manual)

  expect_setequal(res_among$layout$col_labels, res_manual$layout$col_labels)

  for (col_lab in res_among$layout$col_labels) {
    j_among <- which(res_among$layout$col_labels == col_lab)
    j_manual <- which(res_manual$layout$col_labels == col_lab)

    cell_id_among <- res_among$layout$grid[i_among, j_among]
    cell_id_manual <- res_manual$layout$grid[i_manual, j_manual]

    expect_false(is.na(cell_id_among))
    expect_false(is.na(cell_id_manual))

    cell_among <- get_cell(res_among$cell_store, cell_id_among)
    cell_manual <- get_cell(res_manual$cell_store, cell_id_manual)

    expect_equal(cell_among$value, cell_manual$value)
    expect_equal(cell_among$base, cell_manual$base)
  }

  # 3) Displayed base row matches too (user-facing)
  df_among <- as.data.frame(res_among)
  df_manual <- as.data.frame(res_manual)

  base_row_among <- df_among[df_among$row_label == base_label, , drop = FALSE]
  base_row_manual <- df_manual[df_manual$row_label == base_label, , drop = FALSE]

  expect_equal(
    as.numeric(base_row_among[1, -1]),
    as.numeric(base_row_manual[1, -1])
  )
})

test_that("among() errors clearly for scalar/invalid arguments", {
  dat <- data.frame(
    uid = 1:10,
    aware_a = c(1, 0, 1, 1, 0, 1, 0, 0, 1, 1),
    consider_a_all = c(1, 0, 1, 0, 1, NA, 0, 1, 0, 1)
  )

  expect_error(
    tab(dat, rows = among(consider_a_all == 1, 1), cols = total()),
    regexp = "universe must have length nrow\\(data\\)"
  )

  expect_error(
    tab(dat, rows = among(1, aware_a == 1), cols = total()),
    regexp = "x must have length nrow\\(data\\)"
  )

  expect_error(
    tab(dat, rows = among(consider_a_all == 1, "bad"), cols = total()),
    regexp = "universe must evaluate to logical or numeric"
  )
})


