# =============================================================================
# LAYER 3 — Sampling and reproducibility behavior
#
# Q: Does the package consistently and intentionally choose what to display?
# This is the package's core pedagogical promise: the shown example must be
# stable (same seed -> same sample), bounded (<= 5 rows x 4 cols), and signal-
# preserving (filter shows keeps AND drops; select/mutate keep the columns
# that matter).
# =============================================================================

# ---- reproducibility at the public API -------------------------------------

test_that("same seed -> identical sampled data for every verb", {
  expect_identical(
    payload_of(animate_filter(mtcars, mpg > 19.3, seed = 7))$before,
    payload_of(animate_filter(mtcars, mpg > 19.3, seed = 7))$before
  )
  expect_identical(
    payload_of(animate_select(mtcars, mpg, cyl, hp, seed = 7))$before,
    payload_of(animate_select(mtcars, mpg, cyl, hp, seed = 7))$before
  )
  expect_identical(
    payload_of(animate_mutate(mtcars, k = wt * 2, seed = 7))$before,
    payload_of(animate_mutate(mtcars, k = wt * 2, seed = 7))$before
  )
})

test_that("a seeded render never disturbs the caller's RNG stream", {
  # When a seed is supplied the sampler must set AND restore global RNG state,
  # so a reproducible animation can't perturb the surrounding analysis.
  # (The no-seed path deliberately consumes randomness for a fresh sample.)
  set.seed(123)
  before <- .Random.seed
  invisible(animate_filter(mtcars, mpg > 19.3, seed = 99))
  expect_identical(.Random.seed, before)
})

# ---- display caps ----------------------------------------------------------

test_that("display is capped at n_rows rows and max_cols columns", {
  out <- animate_filter(mtcars, mpg > 19.3, seed = 1)   # defaults 5 x 4
  p   <- payload_of(out)
  expect_lte(length(p$before$rows), 5)
  expect_lte(length(p$before$cols), 4)
})

test_that("max_cols in the config tightens the column cap", {
  out <- animate_select(mtcars, mpg, cyl, hp, seed = 1,
                        config = animate_config(max_cols = 3))
  expect_lte(length(payload_of(out)$before$cols), 3)
})

test_that("n_rows controls how many rows are shown", {
  out <- animate_filter(mtcars, mpg > 15, seed = 1, n_rows = 3)
  expect_lte(length(payload_of(out)$before$rows), 3)
})

# ---- signal preservation: filter shows both buckets ------------------------

test_that("filter sample mixes kept and dropped rows when both exist", {
  out <- animate_filter(mtcars, mpg > 19.3, seed = 1)
  flags <- payload_of(out)$row_flags
  expect_true(any(flags))    # at least one keep is shown
  expect_true(any(!flags))   # at least one drop is shown
})

# ---- internal sampler: .sample_for_filter ----------------------------------

test_that(".sample_for_filter mixes buckets and preserves original order", {
  df <- data.frame(x = 1:100)
  mask <- df$x > 40                       # 60 keep / 40 drop
  s <- animatedplyr:::.sample_for_filter(df, mask, n = 5, seed = 1)

  expect_length(s$rows_idx, 5)
  expect_true(any(s$flags) && any(!s$flags))
  expect_equal(s$flags, mask[s$rows_idx])
  expect_equal(s$rows_idx, sort(s$rows_idx))
})

test_that(".sample_for_filter is reproducible and handles tiny/short buckets", {
  df <- data.frame(x = 1:100); mask <- df$x > 40
  expect_identical(
    animatedplyr:::.sample_for_filter(df, mask, n = 5, seed = 42),
    animatedplyr:::.sample_for_filter(df, mask, n = 5, seed = 42)
  )
  # only one row qualifies as a keep -> it must still appear
  tiny <- data.frame(x = 1:10)
  s <- animatedplyr:::.sample_for_filter(tiny, tiny$x > 9, n = 5, seed = 1)
  expect_length(s$rows_idx, 5)
  expect_equal(sum(s$flags), 1)
  # n larger than the frame -> clamp to nrow
  three <- data.frame(x = 1:3)
  s3 <- animatedplyr:::.sample_for_filter(three, c(TRUE, FALSE, TRUE),
                                          n = 5, seed = 1)
  expect_length(s3$rows_idx, 3)
})

# ---- internal samplers: select / mutate keep required columns --------------

test_that(".sample_for_select always keeps the selected columns", {
  df <- wide_df()
  s <- animatedplyr:::.sample_for_select(df, c("c3", "c7"), seed = 1)
  expect_true(all(c("c3", "c7") %in% s$show_cols))
  expect_lte(length(s$show_cols), 4)
})

test_that(".sample_for_mutate always keeps the source column", {
  df <- wide_df()
  s <- animatedplyr:::.sample_for_mutate(df, "c9", seed = 1)
  expect_true("c9" %in% s$show_cols)
  expect_lte(length(s$show_cols), 3)
})

test_that(".sample_cols returns every column when the frame is narrow", {
  df <- toy_df()
  expect_equal(animatedplyr:::.sample_cols(df, required = "score", max_cols = 4),
               colnames(df))
})

# ---- edge-case detector (pure function) ------------------------------------

test_that(".detect_edge_cases classifies filter/select boundaries", {
  expect_match(animatedplyr:::.detect_edge_cases("filter", 10, 10), "All rows")
  expect_match(animatedplyr:::.detect_edge_cases("filter", 0, 10), "No rows")
  expect_null(animatedplyr:::.detect_edge_cases("filter", 4, 10))
  expect_match(animatedplyr:::.detect_edge_cases("select", n_cols_after = 0),
               "No columns")
})

test_that(".with_seed restores RNG state after running its code", {
  set.seed(123)
  before <- .Random.seed
  animatedplyr:::.with_seed(99, runif(10))
  expect_identical(.Random.seed, before)
})
