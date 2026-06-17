# =============================================================================
# R/config.R — configuration system
# =============================================================================

#' Configure the appearance and behavior of animations
#'
#' Returns a validated configuration list that can be passed to any
#' `animate_*()` function via its `config` argument. All knobs have sensible
#' defaults, so most users never call this.
#'
#' @param box_size Cell width in pixels. Default 95.
#' @param font_size Base font size in pixels. Default 13.
#' @param font_family CSS font family. Default `"Courier New"`.
#' @param duration Milliseconds between animation steps when playing.
#'   Default 1300.
#' @param colors Optional named character vector mapping column names to hex
#'   colors, e.g. `c(mpg = "#1A237E")`. Unnamed columns fall back to the
#'   built-in palette.
#' @param show_disclosure Show the gray "+N rows / M cols" pill under the grid
#'   when data was hidden by sampling? Default TRUE.
#'
#' @return A list of class `"animate_config"`.
#' @export
#'
#' @examples
#' cfg <- animate_config(box_size = 110, duration = 1500)
#' animate_filter(mtcars, mpg > 25, config = cfg)
animate_config <- function(box_size = 95,
                           font_size = 13,
                           font_family = "Courier New",
                           duration = 1300,
                           colors = NULL,
                           show_disclosure = TRUE) {

  stopifnot(
    is.numeric(box_size),  length(box_size) == 1L,  box_size  >= 40,
    is.numeric(font_size), length(font_size) == 1L, font_size >= 8,
    is.character(font_family), length(font_family) == 1L,
    is.numeric(duration),  length(duration) == 1L,  duration  >= 200,
    is.logical(show_disclosure), length(show_disclosure) == 1L
  )
  if (!is.null(colors)) {
    if (!is.character(colors) || is.null(names(colors)) || any(names(colors) == "")) {
      stop("`colors` must be a *named* character vector, e.g. c(mpg = '#1A237E').",
           call. = FALSE)
    }
  }

  structure(
    list(
      box_size        = as.numeric(box_size),
      font_size       = as.numeric(font_size),
      font_family     = font_family,
      duration        = as.numeric(duration),
      colors          = colors,
      show_disclosure = show_disclosure
    ),
    class = "animate_config"
  )
}

# Internal: the canonical defaults.
.default_config <- function() animate_config()

# Internal: merge a user config (or NULL, or plain list) over the defaults.
.merge_config <- function(config = NULL) {
  base <- .default_config()
  if (is.null(config)) return(base)
  if (!is.list(config)) {
    stop("`config` must be a list, ideally built with animate_config().",
         call. = FALSE)
  }
  known <- names(base)
  unknown <- setdiff(names(config), known)
  if (length(unknown)) {
    warning("Ignoring unknown config option(s): ",
            paste(unknown, collapse = ", "), call. = FALSE)
  }
  for (nm in intersect(names(config), known)) {
    base[[nm]] <- config[[nm]]
  }
  base
}
