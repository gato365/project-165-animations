# =============================================================================
# R/utils.R — shared internal helpers
# =============================================================================

# Run `code` with a temporary RNG seed, restoring global RNG state afterward.
# seed = NULL means "do not touch the RNG" (fresh randomness each call).
.with_seed <- function(seed, code) {
  if (!is.null(seed)) {
    has_old <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
    old <- if (has_old) get(".Random.seed", envir = globalenv()) else NULL
    on.exit({
      if (has_old) {
        assign(".Random.seed", old, envir = globalenv())
      } else if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
        rm(".Random.seed", envir = globalenv())
      }
    }, add = TRUE)
    set.seed(seed)
  }
  code
}

# Safe sampling without replacement. base::sample() has a notorious gotcha:
# when `x` is a single number >= 1 it samples from seq_len(x) instead of
# returning x itself. Indexing by position sidesteps that entirely, so this is
# correct even when `x` has length 1.
.resample <- function(x, size) x[sample.int(length(x), size)]

# Unique container id so multiple animations on one page never clash.
# Uses a package-local counter (not the RNG) so user set.seed() calls can
# never cause two animations to share an id.
.id_env <- new.env(parent = emptyenv())
.id_env$counter <- 0L

.new_id <- function() {
  .id_env$counter <- .id_env$counter + 1L
  paste0(
    "animdplyr-",
    format(Sys.time(), "%H%M%S"),
    "-",
    Sys.getpid(), "-",
    .id_env$counter
  )
}

# Format a single cell value for display.
.fmt_cell <- function(x) {
  if (is.numeric(x)) {
    out <- format(x, trim = TRUE, digits = 4, scientific = FALSE)
  } else {
    out <- as.character(x)
  }
  out[is.na(x)] <- "NA"
  out
}

# Convert a (small, already-sampled) data frame to the {cols, rows} shape
# the JS engine expects. Values are stringified so JS never formats numbers.
.df_to_payload <- function(df) {
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  cols <- colnames(df)
  if (nrow(df) == 0L || length(cols) == 0L) {
    return(list(cols = as.list(cols), rows = list()))
  }
  rows <- lapply(seq_len(nrow(df)), function(i) {
    vals <- lapply(cols, function(cn) .fmt_cell(df[[cn]][i]))
    stats::setNames(vals, cols)
  })
  list(cols = as.list(cols), rows = rows)
}

# Default qualitative palette for column headers, recycled as needed.
# `overrides` (named character vector) wins over the palette.
.default_colors <- function(cols, overrides = NULL, new_col = NULL) {
  palette <- c("#E63946", "#2196F3", "#43A047", "#FF6F00",
               "#7B1FA2", "#00897B", "#F4511E", "#3949AB")
  idx <- ((seq_along(cols) - 1L) %% length(palette)) + 1L
  out <- stats::setNames(palette[idx], cols)
  if (!is.null(new_col) && new_col %in% cols) out[new_col] <- "#7B1FA2"
  if (!is.null(overrides) && length(overrides)) {
    shared <- intersect(names(overrides), cols)
    out[shared] <- unname(overrides[shared])
  }
  as.list(out)
}

# Choose which columns to display: all `required` columns (in original df
# order), plus randomly sampled extras up to `max_cols` total.
.sample_cols <- function(df, required, max_cols = 4L) {
  all_cols <- colnames(df)
  required <- intersect(all_cols, required)   # keep original order, drop unknowns
  if (length(all_cols) <= max_cols) {
    return(all_cols)
  }
  n_extra <- max(0L, max_cols - length(required))
  pool <- setdiff(all_cols, required)
  extras <- if (n_extra > 0L && length(pool) > 0L) {
    .resample(pool, min(n_extra, length(pool)))
  } else {
    character(0)
  }
  shown <- union(required, extras)
  all_cols[all_cols %in% shown]               # restore original order
}

# Build the disclosure info given what's shown vs the full df.
.disclosure <- function(df, shown_rows, shown_cols) {
  list(
    hidden_rows = max(0L, nrow(df) - shown_rows),
    hidden_cols = max(0L, ncol(df) - length(shown_cols))
  )
}
