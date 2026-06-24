# =============================================================================
# LAYER 2 — Edge-case and failure behavior
#
# Q: Does the package respond correctly when the situation is weird or wrong?
# Scope: empty data, degenerate filters, no/unknown columns, invalid config,
#        bad user input. The package should break GRACEFULLY (clear callout or
#        clear error), never randomly.
# =============================================================================

# ---- degenerate filters: handled with a friendly callout, not a crash ------

test_that("filter on an empty data frame yields a 'no rows' callout", {
  out <- animate_filter(mtcars[0, ], mpg > 19.3, seed = 1)
  expect_match(payload_of(out)$callout, "no rows", ignore.case = TRUE)
})

test_that("filter where every row matches yields an 'all rows' callout", {
  out <- animate_filter(mtcars, mpg > 0, seed = 1)
  expect_match(payload_of(out)$callout, "All rows")
})

test_that("filter where no row matches yields a 'no rows match' callout", {
  out <- animate_filter(mtcars, mpg > 1000, seed = 1)
  expect_match(payload_of(out)$callout, "No rows match")
})

test_that("a callout animation carries empty before/after grids", {
  out <- animate_filter(mtcars, mpg > 1000, seed = 1)
  p   <- payload_of(out)
  expect_length(p$before$rows, 0)
  expect_length(p$row_flags, 0)
})

# ---- select / mutate degenerate inputs -------------------------------------

test_that("selecting zero columns yields a 'no columns' callout", {
  out <- animate_select(mtcars)   # dplyr::select(df) -> 0 columns
  expect_match(payload_of(out)$callout, "No columns")
})

test_that("a constant mutate (no source column) still works", {
  out <- animate_mutate(mtcars, flag = 1, seed = 1)
  p   <- payload_of(out)
  expect_true("flag" %in% unlist(p$after$cols))
  expect_null(p$source_col)         # nothing to highlight, and that's fine
})

# ---- wrong input: should error clearly -------------------------------------

test_that("selecting an unknown column errors", {
  expect_error(animate_select(mtcars, not_a_column, seed = 1))
})

test_that("a non-logical filter condition errors clearly", {
  expect_error(animate_filter(mtcars, mpg, seed = 1),
               "logical vector")
})

test_that("animate_mutate requires a *named* expression", {
  expect_error(animate_mutate(mtcars, wt * 2), "named expression")
})

test_that("animate_mutate warns and uses the first of several columns", {
  expect_warning(
    out <- animate_mutate(mtcars, a = wt * 2, b = hp * 2, seed = 1),
    "one new column at a time"
  )
  expect_true("a" %in% unlist(payload_of(out)$after$cols))
  expect_false("b" %in% unlist(payload_of(out)$after$cols))
})

test_that("with_animation rejects unsupported verbs", {
  expect_error(with_animation(mtcars, arrange(mpg)), "Unsupported verb")
})

test_that("with_animation rejects a non-call expression", {
  expect_error(with_animation(mtcars, mpg), "dplyr verb call")
})

test_that("with_animation rejects multiple filter conditions", {
  expect_error(with_animation(mtcars, filter(mpg > 19, cyl == 6)),
               "single filter condition")
})

# ---- invalid configuration --------------------------------------------------

test_that("animate_config rejects out-of-range numeric knobs", {
  expect_error(animate_config(box_size = 5),       "box_size")     # too small
  expect_error(animate_config(duration = 10),      "duration")     # too fast
  expect_error(animate_config(cell_opacity = 2),   "cell_opacity") # > 1
  expect_error(animate_config(max_cols = 0),       "max_cols")     # < 1
  expect_error(animate_config(border_width = -1),  "border_width") # negative
})

test_that("animate_config rejects malformed colors and flags", {
  expect_error(animate_config(colors = c("#FF0000")),    "named")  # unnamed
  expect_error(animate_config(show_disclosure = "yes"),  "logical")
  expect_error(animate_config(font_family = c("a", "b")), "single string")
})

test_that(".merge_config warns on unknown options but keeps defaults", {
  expect_warning(animatedplyr:::.merge_config(list(nonsense = 1)),
                 "unknown config")
  merged <- animatedplyr:::.merge_config(list(box_size = 120))
  expect_equal(merged$box_size, 120)
  expect_equal(merged$font_size, 13)   # untouched default survives
})

test_that(".merge_config rejects a non-list config", {
  expect_error(animatedplyr:::.merge_config(config = 42), "must be a list")
})
