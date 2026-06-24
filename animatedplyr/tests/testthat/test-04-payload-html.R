# =============================================================================
# LAYER 4 — Payload and HTML structure
#
# Q: Does the R output give the browser the correct materials?
# We do NOT check that the animation *looks* right here — only that the browser
# receives a valid, complete structure that COULD render correctly: a valid
# JSON payload with the expected fields, a container div, embedded CSS/JS, and
# config values threaded into the stylesheet. Hostile data must not break it.
# =============================================================================

# ---- payload completeness + JSON validity ----------------------------------

test_that("the filter payload survives a JSON round trip intact", {
  out  <- animate_filter(mtcars, mpg > 19.3, seed = 1)
  p    <- payload_of(out)
  json <- jsonlite::toJSON(p, auto_unbox = TRUE, null = "null")

  expect_true(jsonlite::validate(json))
  back <- jsonlite::fromJSON(json, simplifyVector = FALSE)
  expect_equal(back$verb, "filter")
  expect_equal(length(back$row_flags), length(p$before$rows))
})

test_that("every payload exposes the fields the JS engine reads", {
  for (out in list(
    animate_filter(mtcars, mpg > 19.3, seed = 1),
    animate_select(mtcars, mpg, cyl, hp, seed = 1),
    animate_mutate(mtcars, k = wt * 2, seed = 1)
  )) {
    p <- payload_of(out)
    expect_true(all(c("verb", "title", "before", "after",
                      "colors", "disclosure") %in% names(p)))
    # before/after both carry the {cols, rows} shape the engine expects
    expect_true(all(c("cols", "rows") %in% names(p$before)))
    expect_true(all(c("cols", "rows") %in% names(p$after)))
    expect_true(all(c("hidden_rows", "hidden_cols") %in% names(p$disclosure)))
  }
})

test_that("disclosure reports the hidden rows/cols honestly", {
  out <- animate_select(mtcars, mpg, cyl, hp, seed = 1)
  d   <- payload_of(out)$disclosure
  expect_gte(d$hidden_rows, 1)   # mtcars has 32 rows, we show <= 5
  expect_gte(d$hidden_cols, 1)   # mtcars has 11 cols, we show <= 4
})

# ---- HTML container + embedded assets --------------------------------------

test_that("the HTML carries a uniquely-id'd container, CSS and JS", {
  html <- as.character(animate_filter(mtcars, mpg > 19.3, seed = 1))
  expect_match(html, "<style>")
  expect_match(html, "<script>")
  expect_match(html, "<div id=\"animdplyr-")
  expect_match(html, "var PAYLOAD =")
})

test_that("every template token is substituted (no __TOKEN__ leaks through)", {
  html <- as.character(animate_mutate(mtcars, k = wt * 2, seed = 1))
  expect_false(grepl("__[A-Z_]+__", html))
})

test_that("two animations on one page get different container ids", {
  id1 <- sub(".*<div id=\"(animdplyr-[^\"]+)\".*", "\\1",
             as.character(animate_filter(mtcars, mpg > 19.3, seed = 1)))
  id2 <- sub(".*<div id=\"(animdplyr-[^\"]+)\".*", "\\1",
             as.character(animate_filter(mtcars, mpg > 19.3, seed = 1)))
  expect_false(identical(id1, id2))
})

# ---- config values are threaded into the stylesheet ------------------------

test_that("basic config values appear in the rendered CSS", {
  cfg  <- animate_config(box_size = 120, font_family = "Menlo")
  html <- as.character(animate_filter(mtcars, mpg > 19.3, seed = 1, config = cfg))
  expect_match(html, "120px")
  expect_match(html, "Menlo")
})

test_that("the full appearance config is threaded into the CSS variables", {
  cfg <- animate_config(
    canvas_bg = "#1e1e1e", keep_color = "#00ff00", new_color = "#abcdef",
    border_radius = 12, cell_opacity = 0.5, transition = 250, cell_gap = 9
  )
  html <- as.character(animate_filter(mtcars, mpg > 19.3, seed = 1, config = cfg))
  expect_match(html, "--canvas-bg: #1e1e1e",  fixed = TRUE)
  expect_match(html, "--keep-color: #00ff00", fixed = TRUE)
  expect_match(html, "--new-color: #abcdef",  fixed = TRUE)
  expect_match(html, "--border-radius: 12px", fixed = TRUE)
  expect_match(html, "--cell-opacity: 0.5",   fixed = TRUE)
  expect_match(html, "--transition: 250ms",   fixed = TRUE)
  expect_match(html, "--gap: 9px",            fixed = TRUE)
})

# ---- hostile data must not break the structure -----------------------------

test_that("quotes, ampersands, NA, unicode and long text round-trip safely", {
  df  <- nasty_df()
  out <- animate_filter(df, val > 0, seed = 1)
  p   <- payload_of(out)
  json <- jsonlite::toJSON(p, auto_unbox = TRUE, null = "null")
  expect_true(jsonlite::validate(json))     # still valid JSON
  # NA renders as the literal string "NA", not a broken token
  flat <- unlist(p$before$rows)
  expect_true(any(flat == "NA"))
})

test_that("a data value containing </script> cannot close the script block", {
  # Regression guard: the payload is embedded inside <script>...</script>.
  # The only legitimate closing tag is the template's own; a data value of
  # "</script>" must be escaped (as "<\/script>") so it can't break the page.
  # Partial filter (keeps some, drops some) so the grid renders and the
  # hostile value lands in the payload rather than short-circuiting to a callout.
  df   <- data.frame(x = c("</script>", "ok", "z"), y = c(1, 2, 3),
                     stringsAsFactors = FALSE)
  html <- as.character(animate_filter(df, y > 1, seed = 1))
  expect_equal(lengths(regmatches(html, gregexpr("</script>", html, fixed = TRUE))),
               1L)                          # exactly one real closing tag
  expect_match(html, "<\\/script>", fixed = TRUE)  # data value was escaped
})

test_that("the static-frame renderer (used for GIFs) hides the controls", {
  out  <- animate_filter(mtcars, mpg > 19.3, seed = 1)
  p    <- payload_of(out); cfg <- config_of(out)
  html <- animatedplyr:::.html_template(p, cfg, static_step = 2L)
  expect_match(html, "STATIC_STEP = 2", fixed = TRUE)
  expect_false(grepl("__[A-Z_]+__", html))
})
