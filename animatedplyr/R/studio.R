# =============================================================================
# R/studio.R — launcher for the interactive Shiny "studio"
# =============================================================================

#' Launch the interactive animation studio
#'
#' Opens a Shiny app for designing animations without writing code. You can
#' pick a dataset (built-ins or your own upload), choose a `dplyr` verb and
#' operation, and tune every visual aspect — colors, fonts, box size and
#' appearance, spacing, transparency, animation speed — with a **live preview**
#' that updates as you drag. The app also shows the exact
#' [animate_config()] + `animate_*()` code that reproduces what you see, so a
#' design you like can be copied straight into a script, vignette, or
#' [animate_save_gif()] call.
#'
#' Requires the suggested packages `shiny` and `DT`. The dataset menu also
#' offers `palmerpenguins::penguins` when that package is installed.
#'
#' @param ... Passed on to [shiny::runApp()] (e.g. `launch.browser`, `port`).
#'
#' @return Called for its side effect; runs until the app is closed.
#' @export
#'
#' @examples
#' \dontrun{
#' animate_studio()
#' }
animate_studio <- function(...) {
  missing_pkgs <- c(
    if (!requireNamespace("shiny", quietly = TRUE)) "shiny",
    if (!requireNamespace("DT",    quietly = TRUE)) "DT"
  )
  if (length(missing_pkgs)) {
    stop(
      "animate_studio() needs the suggested package(s): ",
      paste(missing_pkgs, collapse = ", "), ".\n",
      "  install.packages(c(",
      paste0('"', missing_pkgs, '"', collapse = ", "), "))",
      call. = FALSE
    )
  }

  app_dir <- system.file("shiny", "studio", package = "animatedplyr")
  if (!nzchar(app_dir) || !dir.exists(app_dir)) {
    stop("Could not locate the studio app. Try reinstalling animatedplyr.",
         call. = FALSE)
  }

  shiny::runApp(app_dir, ...)
}
