# Tests for the smart sampling layer

test_that(".sample_for_filter mixes kept and dropped rows", {
  df <- data.frame(x = 1:100)
  mask <- df$x > 40   # 60 kept, 40 dropped
  s <- animatedplyr:::.sample_for_filter(df, mask, n = 5, seed = 1)

  expect_length(s$rows_idx, 5)
  expect_true(any(s$flags))    # at least one keep
  expect_true(any(!s$flags))   # at least one drop
  expect_equal(s$flags, mask[s$rows_idx])
  expect_equal(s$rows_idx, sort(s$rows_idx))  # original order preserved
})

test_that(".sample_for_filter is reproducible with a seed", {
  df <- data.frame(x = 1:100)
  mask <- df$x > 40
  a <- animatedplyr:::.sample_for_filter(df, mask, n = 5, seed = 42)
  b <- animatedplyr:::.sample_for_filter(df, mask, n = 5, seed = 42)
  expect_identical(a, b)
})

test_that(".sample_for_filter handles tiny buckets", {
  df <- data.frame(x = 1:10)
  mask <- df$x > 9          # only 1 keep
  s <- animatedplyr:::.sample_for_filter(df, mask, n = 5, seed = 1)
  expect_length(s$rows_idx, 5)
  expect_equal(sum(s$flags), 1)   # the single keep is included
})

test_that(".sample_for_filter handles n > nrow", {
  df <- data.frame(x = 1:3)
  mask <- c(TRUE, FALSE, TRUE)
  s <- animatedplyr:::.sample_for_filter(df, mask, n = 5, seed = 1)
  expect_length(s$rows_idx, 3)
})

test_that(".sample_for_select always includes selected columns", {
  df <- as.data.frame(matrix(1:50, nrow = 5))
  colnames(df) <- paste0("c", 1:10)
  s <- animatedplyr:::.sample_for_select(df, c("c3", "c7"), seed = 1)
  expect_true(all(c("c3", "c7") %in% s$show_cols))
  expect_lte(length(s$show_cols), 4)
})

test_that(".sample_for_mutate always includes source columns", {
  df <- as.data.frame(matrix(1:50, nrow = 5))
  colnames(df) <- paste0("c", 1:10)
  s <- animatedplyr:::.sample_for_mutate(df, "c9", seed = 1)
  expect_true("c9" %in% s$show_cols)
  expect_lte(length(s$show_cols), 3)
})

test_that("edge cases produce callouts", {
  expect_match(
    animatedplyr:::.detect_edge_cases("filter", n_kept = 10, n_total = 10),
    "All rows"
  )
  expect_match(
    animatedplyr:::.detect_edge_cases("filter", n_kept = 0, n_total = 10),
    "No rows"
  )
  expect_null(
    animatedplyr:::.detect_edge_cases("filter", n_kept = 4, n_total = 10)
  )
  expect_match(
    animatedplyr:::.detect_edge_cases("select", n_cols_after = 0),
    "No columns"
  )
})

test_that(".with_seed restores RNG state", {
  set.seed(123)
  before <- .Random.seed
  animatedplyr:::.with_seed(99, runif(10))
  expect_identical(.Random.seed, before)
})
