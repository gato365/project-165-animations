# Tests for config system

test_that("animate_config returns validated defaults", {
  cfg <- animate_config()
  expect_s3_class(cfg, "animate_config")
  expect_equal(cfg$box_size, 95)
  expect_true(cfg$show_disclosure)
})

test_that("animate_config validates inputs", {
  expect_error(animate_config(box_size = 5))            # too small
  expect_error(animate_config(duration = 10))           # too fast
  expect_error(animate_config(colors = c("#FF0000")))   # unnamed
})

test_that(".merge_config merges over defaults and warns on unknowns", {
  merged <- animatedplyr:::.merge_config(list(box_size = 120))
  expect_equal(merged$box_size, 120)
  expect_equal(merged$font_size, 13)   # default survives
  expect_warning(animatedplyr:::.merge_config(list(nonsense = 1)))
})


# Tests for verb functions

test_that("animate_filter returns a classed HTML object", {
  out <- animate_filter(mtcars, mpg > 19.3, seed = 1)
  expect_s3_class(out, "animate_html")
  expect_true(inherits(out, "html"))
  expect_match(as.character(out), "filter\\(mtcars, mpg > 19.3\\)")
})

test_that("animate_filter payload survives a JSON round trip", {
  out <- animate_filter(mtcars, mpg > 19.3, seed = 1)
  p <- attr(out, "animate_payload")
  json <- jsonlite::toJSON(p, auto_unbox = TRUE, null = "null")
  back <- jsonlite::fromJSON(json, simplifyVector = FALSE)
  expect_equal(back$verb, "filter")
  expect_length(back$row_flags, 5)
})

test_that("animate_filter is reproducible with a seed", {
  a <- animate_filter(mtcars, mpg > 19.3, seed = 7)
  b <- animate_filter(mtcars, mpg > 19.3, seed = 7)
  expect_identical(attr(a, "animate_payload")$before,
                   attr(b, "animate_payload")$before)
})

test_that("animate_filter all-keep produces a callout, not a grid", {
  out <- animate_filter(mtcars, mpg > 0, seed = 1)
  p <- attr(out, "animate_payload")
  expect_match(p$callout, "All rows")
})

test_that("animate_filter all-drop produces a callout", {
  out <- animate_filter(mtcars, mpg > 1000, seed = 1)
  p <- attr(out, "animate_payload")
  expect_match(p$callout, "No rows")
})

test_that("animate_select keeps selected cols and reports disclosure", {
  out <- animate_select(mtcars, mpg, cyl, hp, seed = 1)
  p <- attr(out, "animate_payload")
  expect_true(all(c("mpg", "cyl", "hp") %in% unlist(p$after$cols)))
  expect_gte(p$disclosure$hidden_rows, 1)
})

test_that("animate_mutate computes the new column on shown rows", {
  out <- animate_mutate(mtcars, wt_kg = wt * 453.6, seed = 1)
  p <- attr(out, "animate_payload")
  expect_true("wt_kg" %in% unlist(p$after$cols))
  expect_equal(p$source_col, "wt")
})

test_that("animate_mutate rejects unnamed expressions", {
  expect_error(animate_mutate(mtcars, wt * 2), "named expression")
})

test_that("with_animation routes all three verbs", {
  f <- with_animation(mtcars, filter(mpg > 19.3), seed = 1)
  s <- with_animation(mtcars, select(mpg, cyl), seed = 1)
  m <- with_animation(mtcars, mutate(wt_kg = wt * 453.6), seed = 1)
  expect_equal(attr(f, "animate_payload")$verb, "filter")
  expect_equal(attr(s, "animate_payload")$verb, "select")
  expect_equal(attr(m, "animate_payload")$verb, "mutate")
})

test_that("with_animation rejects unsupported verbs", {
  expect_error(with_animation(mtcars, arrange(mpg)), "Unsupported verb")
})

test_that("custom config flows into the HTML", {
  cfg <- animate_config(box_size = 120, font_family = "Menlo")
  out <- animate_filter(mtcars, mpg > 19.3, seed = 1, config = cfg)
  html <- as.character(out)
  expect_match(html, "120px")
  expect_match(html, "Menlo")
})

test_that("extended appearance config flows into the HTML", {
  cfg <- animate_config(
    canvas_bg = "#1e1e1e", keep_color = "#00ff00",
    border_radius = 12, cell_opacity = 0.5, transition = 250
  )
  out  <- animate_filter(mtcars, mpg > 19.3, seed = 1, config = cfg)
  html <- as.character(out)
  expect_match(html, "--canvas-bg: #1e1e1e", fixed = TRUE)
  expect_match(html, "--keep-color: #00ff00", fixed = TRUE)
  expect_match(html, "--border-radius: 12px", fixed = TRUE)
  expect_match(html, "--cell-opacity: 0.5", fixed = TRUE)
  expect_match(html, "--transition: 250ms", fixed = TRUE)
  expect_false(grepl("__[A-Z_]+__", html))   # no unreplaced tokens
})

test_that("animate_config validates the new knobs", {
  expect_error(animate_config(cell_opacity = 2), "cell_opacity")
  expect_error(animate_config(max_cols = 0), "max_cols")
  expect_error(animate_config(border_width = -1), "border_width")
})

test_that("max_cols caps the number of columns shown", {
  out <- animate_select(mtcars, mpg, cyl, hp, seed = 1,
                        config = animate_config(max_cols = 3))
  p <- attr(out, "animate_payload")
  expect_lte(length(p$before$cols), 3)
})

test_that("animate_save_gif errors helpfully on bad input", {
  expect_error(animate_save_gif("not an animation", "x.gif"),
               "animate_\\*\\(\\)")
  out <- animate_filter(mtcars, mpg > 19.3, seed = 1)
  expect_error(animate_save_gif(out, "x.png"), "\\.gif")
})
