# =============================================================================
# R/config.R — configuration system
# =============================================================================

#' Configure the appearance and behavior of animations
#'
#' Returns a validated configuration list that can be passed to any
#' `animate_*()` function via its `config` argument. All knobs have sensible
#' defaults, so most users never call this directly — but every styling aspect
#' of the animation is exposed here, which is what powers [animate_studio()].
#'
#' Color arguments accept any CSS color string: hex (`"#1A237E"`), rgb/rgba
#' (`"rgba(0,0,0,0.18)"`), or named (`"steelblue"`).
#'
#' @param box_size Cell width in pixels. Default 95.
#' @param font_size Base font size in pixels. Default 13.
#' @param font_family CSS font family. Default `"Courier New"`.
#' @param font_weight CSS font weight for cell text: a number like `600` or a
#'   keyword like `"bold"`. Default `600`.
#' @param duration Milliseconds between animation steps when playing.
#'   Default 1300.
#' @param transition Milliseconds for the CSS transitions (fades, recenters,
#'   color changes). Default 600.
#' @param colors Optional named character vector mapping column names to colors,
#'   e.g. `c(mpg = "#1A237E")`. Unnamed columns fall back to the built-in
#'   palette.
#' @param show_disclosure Show the gray "+N rows / M cols" pill under the grid
#'   when data was hidden by sampling? Default TRUE.
#' @param max_cols Maximum number of columns to display. Default 4.
#' @param canvas_bg Background color of the whole animation card.
#'   Default `"#fafafa"`.
#' @param canvas_padding Padding (px) around the animation card. Default 24.
#' @param title_bg,title_color Background and text color of the title chip.
#'   Defaults `"#111111"` / `"#ffffff"`.
#' @param cell_bg,cell_color Background and text color of data cells.
#'   Defaults `"#f0f0f0"` / `"#222222"`.
#' @param header_color Text color of the header (column-name) cells. The header
#'   *background* comes from the per-column `colors`. Default `"#ffffff"`.
#' @param border_color Cell border color. Default `"rgba(0,0,0,0.18)"`.
#' @param border_width Cell border thickness in pixels. Default 1.5.
#' @param border_radius Cell corner radius in pixels (rounded corners).
#'   Default 5.
#' @param cell_gap Gap in pixels between cells (and between rows). Default 8.
#' @param cell_opacity Opacity of cells, 0 (transparent) to 1 (opaque).
#'   Default 1.
#' @param keep_color,drop_color,new_color Highlight colors for filter-kept
#'   rows, filter-dropped rows, and the mutate-created column. Defaults
#'   `"#43A047"` (green) / `"#E63946"` (red) / `"#2196F3"` (blue).
#'
#' @return A list of class `"animate_config"`.
#' @export
#'
#' @examples
#' cfg <- animate_config(box_size = 110, duration = 1500)
#' animate_filter(mtcars, mpg > 25, config = cfg)
#'
#' # A dark theme
#' dark <- animate_config(
#'   canvas_bg = "#1e1e1e", cell_bg = "#2d2d2d", cell_color = "#eee",
#'   title_bg = "#000", border_color = "#444"
#' )
#' animate_select(mtcars, mpg, cyl, hp, config = dark)
animate_config <- function(box_size = 95,
                           font_size = 13,
                           font_family = "Courier New",
                           font_weight = 600,
                           duration = 1300,
                           transition = 600,
                           colors = NULL,
                           show_disclosure = TRUE,
                           max_cols = 4L,
                           canvas_bg = "#fafafa",
                           canvas_padding = 24,
                           title_bg = "#111111",
                           title_color = "#ffffff",
                           cell_bg = "#f0f0f0",
                           cell_color = "#222222",
                           header_color = "#ffffff",
                           border_color = "rgba(0,0,0,0.18)",
                           border_width = 1.5,
                           border_radius = 5,
                           cell_gap = 8,
                           cell_opacity = 1,
                           keep_color = "#43A047",
                           drop_color = "#E63946",
                           new_color = "#2196F3") {

  # --- numeric knobs: scalar + lower bound ---------------------------------
  .pos_num <- function(x, name, min) {
    if (!is.numeric(x) || length(x) != 1L || is.na(x) || x < min) {
      stop("`", name, "` must be a single number >= ", min, ".", call. = FALSE)
    }
    as.numeric(x)
  }
  box_size       <- .pos_num(box_size,       "box_size",       40)
  font_size      <- .pos_num(font_size,      "font_size",       8)
  duration       <- .pos_num(duration,       "duration",      200)
  transition     <- .pos_num(transition,     "transition",      0)
  max_cols       <- as.integer(.pos_num(max_cols, "max_cols",   1))
  canvas_padding <- .pos_num(canvas_padding, "canvas_padding",  0)
  border_width   <- .pos_num(border_width,   "border_width",    0)
  border_radius  <- .pos_num(border_radius,  "border_radius",   0)
  cell_gap       <- .pos_num(cell_gap,       "cell_gap",        0)

  if (!is.numeric(cell_opacity) || length(cell_opacity) != 1L ||
      is.na(cell_opacity) || cell_opacity < 0 || cell_opacity > 1) {
    stop("`cell_opacity` must be a single number between 0 and 1.", call. = FALSE)
  }

  # --- string knobs: scalar character --------------------------------------
  .str <- function(x, name) {
    if (!is.character(x) || length(x) != 1L || is.na(x)) {
      stop("`", name, "` must be a single string.", call. = FALSE)
    }
    x
  }
  font_family  <- .str(font_family,  "font_family")
  font_weight  <- .str(as.character(font_weight), "font_weight")
  canvas_bg    <- .str(canvas_bg,    "canvas_bg")
  title_bg     <- .str(title_bg,     "title_bg")
  title_color  <- .str(title_color,  "title_color")
  cell_bg      <- .str(cell_bg,      "cell_bg")
  cell_color   <- .str(cell_color,   "cell_color")
  header_color <- .str(header_color, "header_color")
  border_color <- .str(border_color, "border_color")
  keep_color   <- .str(keep_color,   "keep_color")
  drop_color   <- .str(drop_color,   "drop_color")
  new_color    <- .str(new_color,    "new_color")

  if (!is.logical(show_disclosure) || length(show_disclosure) != 1L) {
    stop("`show_disclosure` must be a single logical.", call. = FALSE)
  }
  if (!is.null(colors)) {
    if (!is.character(colors) || is.null(names(colors)) || any(names(colors) == "")) {
      stop("`colors` must be a *named* character vector, e.g. c(mpg = '#1A237E').",
           call. = FALSE)
    }
  }

  structure(
    list(
      box_size        = box_size,
      font_size       = font_size,
      font_family     = font_family,
      font_weight     = font_weight,
      duration        = duration,
      transition      = transition,
      colors          = colors,
      show_disclosure = show_disclosure,
      max_cols        = max_cols,
      canvas_bg       = canvas_bg,
      canvas_padding  = canvas_padding,
      title_bg        = title_bg,
      title_color     = title_color,
      cell_bg         = cell_bg,
      cell_color      = cell_color,
      header_color    = header_color,
      border_color    = border_color,
      border_width    = border_width,
      border_radius   = border_radius,
      cell_gap        = cell_gap,
      cell_opacity    = cell_opacity,
      keep_color      = keep_color,
      drop_color      = drop_color,
      new_color       = new_color
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
