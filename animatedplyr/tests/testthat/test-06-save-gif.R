# =============================================================================
# LAYER 6 — GIF saving behavior
#
# Q: Does optional GIF export work when the setup is right, and fail clearly
#    when it is not?
# GIF export is OPT-IN: it depends on the suggested packages webshot2 + magick
# AND a headless Chrome. The deterministic input-validation checks always run;
# the real end-to-end render is skipped unless the full pipeline is available,
# so a plain install never fails these tests.
# =============================================================================

# ---- input validation: always runs, no heavy deps needed -------------------

test_that("animate_save_gif rejects objects it didn't create", {
  expect_error(animate_save_gif("not an animation", "x.gif"),
               "animate_\\*\\(\\)")
})

test_that("animate_save_gif insists on a .gif path", {
  out <- animate_filter(mtcars, mpg > 19.3, seed = 1)
  expect_error(animate_save_gif(out, "x.png"), "\\.gif")
  expect_error(animate_save_gif(out, "noextension"), "\\.gif")
})

test_that("animate_save_gif reports missing suggested packages clearly", {
  # Only meaningful when the deps are genuinely absent; otherwise skip.
  if (requireNamespace("webshot2", quietly = TRUE) &&
      requireNamespace("magick", quietly = TRUE)) {
    skip("webshot2 + magick are installed; missing-dep path not exercised")
  }
  out <- animate_filter(mtcars, mpg > 19.3, seed = 1)
  expect_error(animate_save_gif(out, tempfile(fileext = ".gif")),
               "install.packages")
})

# ---- end-to-end render: only when the whole pipeline is available ----------

test_that("animate_save_gif writes a real GIF file end to end", {
  skip_on_cran()
  if (!gif_pipeline_available()) {
    skip("GIF pipeline unavailable (needs webshot2 + magick + Chrome)")
  }

  out <- animate_filter(mtcars, mpg > 19.3, seed = 1)
  path <- tempfile(fileext = ".gif")
  on.exit(unlink(path), add = TRUE)

  expect_message(ret <- animate_save_gif(out, path), "Saved")
  expect_identical(ret, path)               # returns the path (invisibly)
  expect_true(file.exists(path))
  expect_gt(file.size(path), 0)

  # It really is a GIF, and a callout animation collapses to a single frame.
  info <- magick::image_info(magick::image_read(path))
  expect_true(all(info$format == "GIF"))
  expect_equal(nrow(info), 4L)              # four animation steps -> 4 frames
})

test_that("a callout animation exports as a single-frame GIF", {
  skip_on_cran()
  if (!gif_pipeline_available()) {
    skip("GIF pipeline unavailable (needs webshot2 + magick + Chrome)")
  }
  out  <- animate_filter(mtcars, mpg > 1000, seed = 1)   # no rows match
  path <- tempfile(fileext = ".gif")
  on.exit(unlink(path), add = TRUE)
  animate_save_gif(out, path)
  info <- magick::image_info(magick::image_read(path))
  expect_equal(nrow(info), 1L)
})
