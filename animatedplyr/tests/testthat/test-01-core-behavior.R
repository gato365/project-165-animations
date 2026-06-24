# =============================================================================
# LAYER 1 — Core function behavior  (the "happy path")
#
# Q: Does each function do what it is supposed to do when used correctly?
# Scope: normal inputs, expected class/structure of the output object.
# =============================================================================

test_that("animate_filter returns a well-formed animate_html object", {
  out <- animate_filter(mtcars, mpg > 19.3, seed = 1)

  expect_s3_class(out, "animate_html")
  expect_true(inherits(out, "html"))         # so knitr embeds it as raw HTML
  expect_type(as.character(out), "character")

  p <- payload_of(out)
  expect_equal(p$verb, "filter")
  expect_match(p$title, "filter(mtcars, mpg > 19.3)", fixed = TRUE)
  expect_named(p, c("verb", "title", "expression", "before", "after",
                    "row_flags", "colors", "disclosure", "callout"))
})

test_that("animate_select keeps the selected columns and drops the rest", {
  out <- animate_select(mtcars, mpg, cyl, hp, seed = 1)
  p   <- payload_of(out)

  expect_equal(p$verb, "select")
  expect_true(all(c("mpg", "cyl", "hp") %in% unlist(p$after$cols)))
  # 'after' never contains a column that wasn't selected
  expect_true(all(unlist(p$after$cols) %in% c("mpg", "cyl", "hp")))
})

test_that("animate_mutate adds the new column and records its source", {
  out <- animate_mutate(mtcars, wt_kg = wt * 453.6, seed = 1)
  p   <- payload_of(out)

  expect_equal(p$verb, "mutate")
  expect_true("wt_kg" %in% unlist(p$after$cols))
  expect_false("wt_kg" %in% unlist(p$before$cols))  # genuinely new
  expect_equal(p$source_col, "wt")
})

test_that("with_animation routes each verb to the right animator", {
  f <- with_animation(mtcars, filter(mpg > 19.3), seed = 1)
  s <- with_animation(mtcars, select(mpg, cyl),    seed = 1)
  m <- with_animation(mtcars, mutate(wt_kg = wt * 453.6), seed = 1)

  expect_equal(payload_of(f)$verb, "filter")
  expect_equal(payload_of(s)$verb, "select")
  expect_equal(payload_of(m)$verb, "mutate")
})

test_that("with_animation tolerates dplyr:: qualified verbs", {
  f <- with_animation(mtcars, dplyr::filter(mpg > 19.3), seed = 1)
  expect_equal(payload_of(f)$verb, "filter")
})

test_that("animate_config returns validated defaults", {
  cfg <- animate_config()
  expect_s3_class(cfg, "animate_config")
  expect_equal(cfg$box_size, 95)
  expect_equal(cfg$max_cols, 4L)
  expect_true(cfg$show_disclosure)
})

test_that("a custom config is carried on the returned object", {
  cfg <- animate_config(box_size = 120, font_family = "Menlo")
  out <- animate_filter(mtcars, mpg > 19.3, seed = 1, config = cfg)
  expect_equal(config_of(out)$box_size, 120)
  expect_equal(config_of(out)$font_family, "Menlo")
})

test_that("the three verbs work on a hand-built frame too", {
  df <- toy_df()
  expect_s3_class(animate_filter(df, score > 25, seed = 1), "animate_html")
  expect_s3_class(animate_select(df, id, score, seed = 1),  "animate_html")
  expect_s3_class(animate_mutate(df, dbl = score * 2, seed = 1), "animate_html")
})
