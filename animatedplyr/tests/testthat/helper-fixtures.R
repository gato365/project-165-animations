# =============================================================================
# Shared fixtures + helpers for the test suite.
#
# Files named helper-*.R are sourced by testthat before any test-*.R file, so
# everything defined here is available to every layer.
# =============================================================================

# A tiny, fully-controlled data frame. Using a hand-built frame (rather than
# mtcars) for many tests keeps expectations exact and independent of dataset
# quirks.
toy_df <- function() {
  data.frame(
    id    = 1:6,
    score = c(10, 20, 30, 40, 50, 60),
    grade = c("A", "B", "A", "C", "B", "A"),
    stringsAsFactors = FALSE
  )
}

# A wide frame (more columns than fit) to exercise column sampling/caps.
wide_df <- function(ncol = 10, nrow = 8) {
  m <- as.data.frame(matrix(seq_len(ncol * nrow), nrow = nrow))
  colnames(m) <- paste0("c", seq_len(ncol))
  m
}

# A frame full of values that are hostile to naive HTML/JSON embedding.
nasty_df <- function() {
  data.frame(
    txt = c("quote\"here", "</script><b>x</b>", "a & b < c > d",
            "emoji ✅ unicode", NA,
            paste(rep("very-long", 20), collapse = "-")),
    val = c(1, 2, 3, NA, 5, 6),
    stringsAsFactors = FALSE
  )
}

# Convenience: pull the payload attached to an animate_html object.
payload_of <- function(x) attr(x, "animate_payload")
config_of  <- function(x) attr(x, "animate_config")

# Extract one column of a payload {cols, rows} table as a character vector.
payload_col <- function(tbl, col) {
  vapply(tbl$rows, function(r) r[[col]], character(1))
}

# Same, coerced to numeric ("NA" strings become NA quietly).
payload_num <- function(tbl, col) {
  suppressWarnings(as.numeric(payload_col(tbl, col)))
}

# One string per payload row, so whole rows can be compared/set-compared.
payload_row_keys <- function(tbl) {
  vapply(tbl$rows, function(r) paste(unlist(r), collapse = ""),
         character(1))
}

# A frame with 4 groups of known, uneven sizes (10/8/6/4) and exact halves,
# so summary values reconstructed from the payload strings are exact.
grouped_df <- function() {
  data.frame(
    sp = rep(c("A", "B", "C", "D"), times = c(10, 8, 6, 4)),
    m  = seq_len(28) / 2,
    w  = rep(1:7, times = 4),
    stringsAsFactors = FALSE
  )
}

# Is the GIF pipeline actually runnable in this environment? Needs both
# suggested packages AND a headless Chrome that webshot2 can drive.
gif_pipeline_available <- function() {
  if (!requireNamespace("webshot2", quietly = TRUE)) return(FALSE)
  if (!requireNamespace("magick",   quietly = TRUE)) return(FALSE)
  chrome <- tryCatch(
    nzchar(Sys.getenv("CHROMOTE_CHROME")) ||
      nzchar(Sys.which("google-chrome")) ||
      nzchar(Sys.which("chromium")) ||
      nzchar(Sys.which("chromium-browser")) ||
      file.exists("/Applications/Google Chrome.app/Contents/MacOS/Google Chrome"),
    error = function(e) FALSE
  )
  isTRUE(chrome)
}
