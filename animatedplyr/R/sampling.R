# =============================================================================
# R/sampling.R \u2014 smart sampling (the intellectual core of the package)
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

    pick_keep <- if (n_keep > 0L) .resample(kept_idx, n_keep) else integer(0)
    pick_drop <- if (n_drop > 0L) .resample(drop_idx, n_drop) else integer(0)

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

# ---- arrange ---------------------------------------------------------------
# Returns list(rows_idx = integer(), already_sorted = logical(1)).
# `sorted_pos` maps each original row index to its position in the fully
# arranged data (ties resolved exactly as dplyr::arrange resolves them).
# A sample only teaches something if the shown rows are NOT already in sorted
# relative order, so we resample up to `tries` times before conceding; a
# concession (already_sorted = TRUE) means the data itself is sorted/constant
# and the caller should fall through to an edge-case callout.
.sample_for_arrange <- function(df, sorted_pos, n = 5L, tries = 5L,
                                seed = NULL) {
  stopifnot(is.numeric(sorted_pos), length(sorted_pos) == nrow(df))
  .with_seed(seed, {
    n <- min(n, nrow(df))
    best <- integer(0)
    for (i in seq_len(max(1L, tries))) {
      rows_idx <- sort(.resample(seq_len(nrow(df)), n))
      if (is.unsorted(sorted_pos[rows_idx])) {
        best <- rows_idx
        break
      }
      if (!length(best)) best <- rows_idx
    }
    list(rows_idx = best,
         already_sorted = !is.unsorted(sorted_pos[best]))
  })
}

# ---- group_summarize -------------------------------------------------------
# Returns list(rows_idx, show_cols, groups, n_groups_total, n_complete).
# `groups` is ordered most-frequent-first; each element is
# list(value = <label>, rows_pos = <1-based positions within rows_idx>).
# Shows up to 3 groups with a DELIBERATELY UNEVEN allocation (3/2/1 at the
# default n = 6) so students see that groups differ in size. Rows with NA in
# any displayed column are skipped so every shown value participates in the
# summary.
.sample_for_group_summarize <- function(df, group_col, needed_cols, n = 6L,
                                        max_cols = 4L, seed = NULL) {
  na_cols <- intersect(union(group_col, needed_cols), colnames(df))
  complete <- stats::complete.cases(df[, na_cols, drop = FALSE])
  pool <- which(complete)
  gvals <- as.character(df[[group_col]])[pool]
  counts <- sort(table(gvals), decreasing = TRUE)

  if (length(pool) == 0L || length(counts) == 0L) {
    return(list(rows_idx = integer(0), show_cols = character(0),
                groups = list(), n_groups_total = 0L, n_complete = 0L))
  }

  .with_seed(seed, {
    k <- min(3L, length(counts), n)
    top <- names(counts)[seq_len(k)]
    avail <- as.integer(counts[seq_len(k)])

    # Uneven 3:2:1 weights, rounded to n, then reconciled with what exists.
    weights <- c(3, 2, 1)[seq_len(k)]
    alloc <- pmax(1L, as.integer(round(n * weights / sum(weights))))
    alloc <- pmin(alloc, avail)
    i <- k
    while (sum(alloc) > n && i >= 1L) {
      alloc[i] <- alloc[i] - min(alloc[i] - 1L, sum(alloc) - n)
      i <- i - 1L
    }
    for (i in seq_len(k)) {
      alloc[i] <- alloc[i] + min(avail[i] - alloc[i], n - sum(alloc))
    }

    picks <- lapply(seq_len(k), function(i) {
      .resample(pool[gvals == top[i]], alloc[i])
    })
    rows_idx <- sort(unlist(picks))          # step 1 shows original order
    groups <- lapply(seq_len(k), function(i) {
      list(value = top[i],
           rows_pos = match(sort(picks[[i]]), rows_idx))
    })
    show_cols <- .sample_cols(df, required = na_cols, max_cols = max_cols)
    list(rows_idx = rows_idx, show_cols = show_cols, groups = groups,
         n_groups_total = length(counts), n_complete = length(pool))
  })
}

# ---- edge cases ------------------------------------------------------------
# Returns a callout string, or NULL when the animation should run normally.
.detect_edge_cases <- function(verb, n_kept = NULL, n_total = NULL,
                               n_cols_after = NULL, already_sorted = NULL,
                               n_groups = NULL, n_complete = NULL) {
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
  if (verb == "arrange") {
    if (!is.null(n_total) && n_total == 0L) {
      return("The data frame has no rows.")
    }
    if (isTRUE(already_sorted)) {
      return(paste0("These rows are already in this order \u2014 arrange() ",
                    "would not change what you see. Try the opposite ",
                    "direction or another column."))
    }
  }
  if (verb == "group_summarize") {
    if (!is.null(n_complete) && n_complete == 0L) {
      return(paste0("No complete rows to display \u2014 every row has a ",
                    "missing value in the columns used."))
    }
    if (!is.null(n_groups) && n_groups == 1L) {
      return(paste0("Only one group is present \u2014 group_by() adds ",
                    "nothing here, so summarize() collapses everything ",
                    "into a single grand-summary row."))
    }
  }
  NULL
}
