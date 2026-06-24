# =============================================================================
# LAYER 5 — Printing, Quarto, and viewer behavior
#
# Q: Does the object display correctly in common teaching environments?
# Full RStudio/Positron Viewer behavior depends on the user's machine and can't
# be asserted in CI, so this is deliberately a LIGHT layer: we test the print
# method's contract and the html-ness that lets knitr/Quarto embed the object.
# Real IDE viewer behavior is covered by manual smoke checks (see README).
# =============================================================================

test_that("printing non-interactively emits the HTML and returns invisibly", {
  out <- animate_filter(mtcars, mpg > 19.3, seed = 1)
  # Tests run with interactive() == FALSE, so print() cats the HTML.
  expect_output(print(out), "<div id=\"animdplyr-")
  expect_invisible(print(out))
})

test_that("print returns its input unchanged (for piping)", {
  out <- animate_filter(mtcars, mpg > 19.3, seed = 1)
  returned <- withVisible(print(out))$value
  expect_identical(returned, out)
})

test_that("the object is an html string knitr/Quarto can embed inline", {
  out <- animate_select(mtcars, mpg, cyl, seed = 1)
  # htmltools/knitr treat anything inheriting 'html' as raw HTML output.
  expect_true(inherits(out, "html"))
  expect_type(as.character(out), "character")
  expect_gt(nchar(as.character(out)), 100)
})

test_that("knitr::knit_print produces embeddable HTML output", {
  skip_if_not_installed("knitr")
  out <- animate_filter(mtcars, mpg > 19.3, seed = 1)
  kp  <- knitr::knit_print(out)
  expect_match(as.character(kp), "<div id=\"animdplyr-")
})

test_that("htmltools can save the animation to a standalone page", {
  # Mirrors what the Viewer pane does: write a self-contained HTML file.
  out <- animate_filter(mtcars, mpg > 19.3, seed = 1)
  tmp <- tempfile(fileext = ".html")
  on.exit(unlink(tmp), add = TRUE)
  htmltools::save_html(htmltools::HTML(as.character(out)), tmp)
  expect_true(file.exists(tmp))
  expect_gt(file.size(tmp), 0)
})
