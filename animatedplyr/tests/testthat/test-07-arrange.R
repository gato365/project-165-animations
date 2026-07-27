# =============================================================================
# LAYER 7 — animate_arrange()
#
# Q: Does the arrange animation show a genuine reshuffle, order rows exactly
# as dplyr::arrange() would (including desc()), and concede gracefully when
# the sample is already in order?
# =============================================================================

# ---- internal sampler: .sample_for_arrange ---------------------------------

test_that(".sample_for_arrange avoids an already-sorted sample when it can", {
  set.seed(99)
  df <- data.frame(x = sample(1:100))
  sorted_pos <- match(seq_len(nrow(df)), order(df$x))
  s <- animatedplyr:::.sample_for_arrange(df, sorted_pos, n = 5, seed = 1)

  expect_length(s$rows_idx, 5)
  expect_equal(s$rows_idx, sort(s$rows_idx))        # original order preserved
  expect_false(s$already_sorted)
  expect_true(is.unsorted(sorted_pos[s$rows_idx]))  # a visible reshuffle
})

test_that(".sample_for_arrange is reproducible and clamps n to nrow", {
  set.seed(7)
  df <- data.frame(x = sample(1:50))
  sorted_pos <- match(seq_len(nrow(df)), order(df$x))
  expect_identical(
    animatedplyr:::.sample_for_arrange(df, sorted_pos, n = 5, seed = 42),
    animatedplyr:::.sample_for_arrange(df, sorted_pos, n = 5, seed = 42)
  )
  tiny <- data.frame(x = c(3, 1, 2))
  s <- animatedplyr:::.sample_for_arrange(tiny, match(1:3, order(tiny$x)),
                                          n = 5, seed = 1)
  expect_length(s$rows_idx, 3)
})

test_that(".sample_for_arrange concedes when the data is genuinely sorted", {
  df <- data.frame(x = 1:20)                    # sorted: every sample sorted
  sorted_pos <- seq_len(20)
  s <- animatedplyr:::.sample_for_arrange(df, sorted_pos, n = 5, seed = 1)
  expect_true(s$already_sorted)
  expect_length(s$rows_idx, 5)                  # still returns a sample
})

# ---- public API ------------------------------------------------------------

test_that("animate_arrange returns a well-formed animate_html object", {
  out <- animate_arrange(mtcars, desc(mpg), seed = 1)

  expect_s3_class(out, "animate_html")
  expect_true(inherits(out, "html"))

  p <- payload_of(out)
  expect_equal(p$verb, "arrange")
  expect_match(p$title, "arrange(mtcars, desc(mpg))", fixed = TRUE)
  expect_named(p, c("verb", "title", "expression", "sort_cols", "directions",
                    "before", "after", "colors", "disclosure", "callout"))
  expect_equal(unlist(p$sort_cols), "mpg")
  expect_equal(unlist(p$directions), "desc")
  expect_true("mpg" %in% unlist(p$before$cols))   # sort column always shown
})

test_that("the sorted payload matches dplyr::arrange on the shown rows", {
  out <- animate_arrange(mtcars, desc(mpg), seed = 1)
  p   <- payload_of(out)

  expect_equal(payload_num(p$after, "mpg"),
               sort(payload_num(p$before, "mpg"), decreasing = TRUE))
  # same rows, same dims -- arrange changes ORDER, never CONTENT
  expect_setequal(payload_row_keys(p$after), payload_row_keys(p$before))
  expect_length(p$after$rows, length(p$before$rows))
  expect_equal(p$after$cols, p$before$cols)
  # and there is a visible reshuffle to animate
  expect_false(identical(payload_row_keys(p$before),
                         payload_row_keys(p$after)))
})

test_that("multi-column sorts incl. desc() match dplyr::arrange exactly", {
  df <- data.frame(g = c("b", "a", "b", "a", "c", "a"),
                   x = c(2, 5, 1, 3, 9, 4), stringsAsFactors = FALSE)
  out <- animate_arrange(df, g, desc(x), n_rows = 6, seed = 1)
  p   <- payload_of(out)

  shown <- data.frame(g = payload_col(p$before, "g"),
                      x = payload_num(p$before, "x"),
                      stringsAsFactors = FALSE)
  expected <- dplyr::arrange(shown, g, dplyr::desc(x))
  expect_equal(payload_col(p$after, "g"), expected$g)
  expect_equal(payload_num(p$after, "x"), expected$x)
  expect_equal(unlist(p$sort_cols), c("g", "x"))
  expect_equal(unlist(p$directions), c("asc", "desc"))
})

test_that("ties are ordered stably, exactly as dplyr::arrange orders them", {
  df <- data.frame(k = c(2, 1, 2, 1, 2, 1), id = 1:6)
  out <- animate_arrange(df, k, n_rows = 6, seed = 1)
  p   <- payload_of(out)

  shown <- data.frame(k  = payload_num(p$before, "k"),
                      id = payload_num(p$before, "id"))
  expected <- dplyr::arrange(shown, k)
  expect_equal(payload_num(p$after, "id"), expected$id)
  expect_equal(payload_num(p$after, "k"),  expected$k)
})

test_that("dplyr::desc() qualified calls are recognized", {
  out <- animate_arrange(mtcars, dplyr::desc(mpg), seed = 1)
  p   <- payload_of(out)
  expect_equal(unlist(p$directions), "desc")
  expect_equal(unlist(p$sort_cols), "mpg")
})

test_that("same seed -> identical arrange payload", {
  expect_identical(
    payload_of(animate_arrange(mtcars, desc(mpg), seed = 7)),
    payload_of(animate_arrange(mtcars, desc(mpg), seed = 7))
  )
})

test_that("the arrange payload survives a JSON round trip intact", {
  p    <- payload_of(animate_arrange(mtcars, cyl, desc(mpg), seed = 1))
  json <- jsonlite::toJSON(p, auto_unbox = TRUE, null = "null")
  expect_true(jsonlite::validate(json))
  back <- jsonlite::fromJSON(json, simplifyVector = FALSE)
  expect_equal(back$verb, "arrange")
  expect_true(is.list(back$sort_cols))     # stays an array, never a scalar
  expect_length(back$sort_cols, 2)
})

# ---- edge cases ------------------------------------------------------------

test_that("an already-sorted frame yields a callout, not an animation", {
  out <- animate_arrange(data.frame(x = 1:8), x, seed = 1)
  p   <- payload_of(out)
  expect_match(p$callout, "already in this order")
  expect_length(p$before$rows, 0)
})

test_that("a constant sort column yields the already-sorted callout", {
  out <- animate_arrange(data.frame(x = rep(1, 8), y = 1:8), x, seed = 1)
  expect_match(payload_of(out)$callout, "already in this order")
})

test_that("an empty frame yields a no-rows callout", {
  out <- animate_arrange(mtcars[0, ], mpg, seed = 1)
  expect_match(payload_of(out)$callout, "no rows", ignore.case = TRUE)
})

test_that("animate_arrange requires at least one sort column", {
  expect_error(animate_arrange(mtcars), "at least one sort column")
})

# ---- with_animation routing ------------------------------------------------

test_that("with_animation routes arrange() to animate_arrange", {
  out <- with_animation(mtcars, arrange(desc(mpg)), seed = 1)
  p   <- payload_of(out)
  expect_equal(p$verb, "arrange")
  expect_equal(unlist(p$directions), "desc")
  expect_identical(p, payload_of(animate_arrange(mtcars, desc(mpg), seed = 1)))
})

# ---- edge-case detector ----------------------------------------------------

test_that(".detect_edge_cases classifies arrange boundaries", {
  expect_match(
    animatedplyr:::.detect_edge_cases("arrange", already_sorted = TRUE),
    "already in this order"
  )
  expect_match(
    animatedplyr:::.detect_edge_cases("arrange", n_total = 0L),
    "no rows", ignore.case = TRUE
  )
  expect_null(
    animatedplyr:::.detect_edge_cases("arrange", n_total = 10L,
                                      already_sorted = FALSE)
  )
})
