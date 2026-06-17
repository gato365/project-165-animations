# =============================================================================
# R/save_gif.R — opt-in GIF export (suggested dependencies only)
# =============================================================================

#' Export an animation as an animated GIF file
#'
#' Renders each step of an animation to a PNG frame using a headless browser,
#' then stitches the frames into a GIF. The resulting file works anywhere an
#' image works: PDF output, Word, slides, or plain markdown via
#' `![](file.gif)`.
#'
#' Requires the suggested packages `webshot2` and `magick`, plus a local
#' Chrome/Chromium installation (webshot2 will find or prompt for one).
#' These are intentionally NOT required by the main package, so a plain
#' install stays lightweight.
#'
#' @param animation An object returned by [animate_filter()],
#'   [animate_select()], [animate_mutate()], or [with_animation()].
#' @param path Output file path ending in `.gif`.
#' @param fps Frames per second of the resulting GIF. Default 0.8
#'   (slow enough to read each step).
#' @param width,height Pixel dimensions of each captured frame.
#'   Defaults 700 x 450.
#'
#' @return Invisibly, the output path.
#' @export
#'
#' @examples
#' \dontrun{
#' anim <- animate_filter(mtcars, mpg > 19.3, seed = 42)
#' animate_save_gif(anim, "filter_demo.gif")
#' }
animate_save_gif <- function(animation, path, fps = 0.8,
                             width = 700, height = 450) {

  if (!inherits(animation, "animate_html")) {
    stop("`animation` must be created by an animate_*() function.",
         call. = FALSE)
  }
  if (!grepl("\\.gif$", path, ignore.case = TRUE)) {
    stop("`path` must end in .gif", call. = FALSE)
  }

  missing_pkgs <- c(
    if (!requireNamespace("webshot2", quietly = TRUE)) "webshot2",
    if (!requireNamespace("magick",   quietly = TRUE)) "magick"
  )
  if (length(missing_pkgs)) {
    stop(
      "To export GIFs, install the suggested packages first:\n",
      "  install.packages(c(",
      paste0('"', missing_pkgs, '"', collapse = ", "),
      "))\n",
      "webshot2 also needs Chrome/Chromium available on your system.",
      call. = FALSE
    )
  }

  payload <- attr(animation, "animate_payload")
  config  <- attr(animation, "animate_config")
  if (is.null(payload) || is.null(config)) {
    stop("This animation object is missing its payload; ",
         "re-create it with the current package version.", call. = FALSE)
  }

  # Callout animations have a single frame.
  n_steps <- if (!is.null(payload$callout)) 1L else 4L

  tmpdir <- tempfile("animgif")
  dir.create(tmpdir)
  on.exit(unlink(tmpdir, recursive = TRUE), add = TRUE)

  png_paths <- character(n_steps)
  for (i in seq_len(n_steps)) {
    html_i <- .html_template(payload, config, static_step = i - 1L)
    html_file <- file.path(tmpdir, paste0("frame", i, ".html"))
    writeLines(paste0(
      "<!DOCTYPE html><html><head><meta charset='utf-8'></head><body>",
      html_i,
      "</body></html>"
    ), html_file)

    png_paths[i] <- file.path(tmpdir, paste0("frame", i, ".png"))
    webshot2::webshot(
      url = paste0("file://", normalizePath(html_file)),
      file = png_paths[i],
      vwidth = width, vheight = height,
      delay = 1  # let CSS transitions settle
    )
  }

  frames <- magick::image_read(png_paths)
  gif <- magick::image_animate(frames, fps = fps)
  magick::image_write(gif, path)

  message("Saved ", n_steps, "-frame GIF to ", normalizePath(path))
  invisible(path)
}
