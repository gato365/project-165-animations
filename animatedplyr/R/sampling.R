# =============================================================================
# R/sampling.R — smart sampling (the intellectual core of the package)
#
# Each sampler returns a small slice of the data that preserves the
# pedagogical signal of the verb:
#   filter : both kept AND dropped rows visible
#   select : selected columns always visible (+ optionally a dropped one)
#   mutate : source column(s) and new column always visible
# =============================================================================

# ---- filter ----------------------------------------------------------------
# Returns list(rows_idx = integer(), flags = logical()) where flags aligns
# with rows_idx and says whether each shown row passes the condition.
# Targets ~60% keeps / ~40% drops in the sample.
.sample_for_filter <- function(df, mask, n = 5L, seed = NULL) {
  stopifnot(is.logical(mask), length(mask) == nrow(df))
  mask[is.na(mask)] <- FALSE

  kept_idx <- which(mask)
  drop_idx <- which(!mask)

  .with_seed(seed, {
    n <- min(n, nrow(df))
    target_keep <- ceiling(n * 0.6)
    target_drop <- n - target_keep

    # Rebalance when one bucket is too small.
    n_keep <- min(target_keep, length(kept_idx))
    n_drop <- min(target_drop, length(drop_idx))
    shortfall <- n - (n_keep + n_drop)
    if (shortfall > 0L) {
      extra_keep <- min(shortfall, length(kept_idx) - n_keep)
      n_keep <- n_keep + extra_keep
      shortfall <- shortfall - extra_keep
      n_drop <- n_drop + min(shortfall, length(drop_idx) - n_drop)
    }

    pick_keep <- if (n_keep > 0L) sample(kept_idx, n_keep) else integer(0)
    pick_drop <- if (n_drop > 0L) sample(drop_idx, n_drop) else integer(0)

    rows_idx <- sort(c(pick_keep, pick_drop))   # preserve original order
    list(rows_idx = rows_idx, flags = mask[rows_idx])
  })
}

# ---- select ----------------------------------------------------------------
# Returns list(show_cols = character(), rows_idx = integer()).
# show_cols always contains every selected column; if room remains under
# max_cols, up to n_extra non-selected columns are sampled in so students can
# see something get dropped.
.sample_for_select <- function(df, selected_cols, n = 5L, n_extra = 1L,
                               max_cols = 4L, seed = NULL) {
  .with_seed(seed, {
    budget <- max(length(selected_cols),
                  min(max_cols, length(selected_cols) + n_extra))
    show_cols <- .sample_cols(df, required = selected_cols, max_cols = budget)
    rows_idx <- sort(sample(seq_len(nrow(df)), min(n, nrow(df))))
    list(show_cols = show_cols, rows_idx = rows_idx)
  })
}

# ---- mutate ----------------------------------------------------------------
# Returns list(show_cols = character(), rows_idx = integer()).
# show_cols always includes every source column the expression depends on.
.sample_for_mutate <- function(df, source_cols, n = 5L, max_cols = 3L,
                               seed = NULL) {
  .with_seed(seed, {
    show_cols <- .sample_cols(df, required = source_cols, max_cols = max_cols)
    rows_idx <- sort(sample(seq_len(nrow(df)), min(n, nrow(df))))
    list(show_cols = show_cols, rows_idx = rows_idx)
  })
}

# ---- edge cases ------------------------------------------------------------
# Returns a callout string, or NULL when the animation should run normally.
.detect_edge_cases <- function(verb, n_kept = NULL, n_total = NULL,
                               n_cols_after = NULL) {
  if (verb == "filter") {
    if (n_total == 0L) {
      return("The data frame has no rows.")
    }
    if (n_kept == n_total) {
      return("All rows satisfy this condition \u2014 nothing is filtered out.")
    }
    if (n_kept == 0L) {
      return("No rows match this condition \u2014 try a less strict condition.")
    }
  }
  if (verb == "select" && !is.null(n_cols_after) && n_cols_after == 0L) {
    return("No columns remain after this select().")
  }
  NULL
}
