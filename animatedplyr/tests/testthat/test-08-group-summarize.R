# =============================================================================
# LAYER 8 — animate_group_summarize()
#
# Q: Does the combined group_by + summarize animation pick few, uneven groups,
# compute every requested summary correctly on the shown rows, and keep the
# badge counts honest?
# =============================================================================

# ---- internal sampler: .sample_for_group_summarize -------------------------

test_that(".sample_for_group_summarize shows at most 3 groups, unevenly", {
  s <- animatedplyr:::.sample_for_group_summarize(grouped_df(), "sp", "m",
                                                  n = 6, seed = 1)
  expect_length(s$groups, 3)                     # 4 groups exist, 3 shown
  expect_equal(s$n_groups_total, 4L)
  sizes <- vapply(s$groups, function(g) length(g$rows_pos), integer(1))
  expect_equal(sizes, c(3L, 2L, 1L))             # deliberately uneven
  expect_length(s$rows_idx, 6)
  expect_equal(s$rows_idx, sort(s$rows_idx))     # original order preserved
  # groups are the most frequent ones (A=10, B=8, C=6 beat D=4)
  labels <- vapply(s$groups, function(g) g$value, character(1))
  expect_setequal(labels, c("A", "B", "C"))
})

test_that(".sample_for_group_summarize rows_pos indexes rows_idx correctly", {
  df <- grouped_df()
  s <- animatedplyr:::.sample_for_group_summarize(df, "sp", "m",
                                                  n = 6, seed = 1)
  for (g in s$groups) {
    expect_equal(unique(df$sp[s$rows_idx[g$rows_pos]]), g$value)
  }
  # every shown row belongs to exactly one group box
  all_pos <- sort(unlist(lapply(s$groups, `[[`, "rows_pos")))
  expect_equal(all_pos, seq_along(s$rows_idx))
})

test_that(".sample_for_group_summarize skips rows with NA in shown columns", {
  df <- grouped_df()
  df$m[df$sp == "A"] <- NA                       # group A becomes unusable
  s <- animatedplyr:::.sample_for_group_summarize(df, "sp", "m",
                                                  n = 6, seed = 1)
  labels <- vapply(s$groups, function(g) g$value, character(1))
  expect_false("A" %in% labels)
  expect_false(anyNA(df$m[s$rows_idx]))
  expect_equal(s$n_complete, sum(!is.na(df$m)))
})

test_that(".sample_for_group_summarize is reproducible", {
  expect_identical(
    animatedplyr:::.sample_for_group_summarize(grouped_df(), "sp", "m",
                                               n = 6, seed = 42),
    animatedplyr:::.sample_for_group_summarize(grouped_df(), "sp", "m",
                                               n = 6, seed = 42)
  )
})

test_that(".sample_for_group_summarize handles an all-NA frame", {
  df <- grouped_df()
  df$m <- NA_real_
  s <- animatedplyr:::.sample_for_group_summarize(df, "sp", "m",
                                                  n = 6, seed = 1)
  expect_length(s$rows_idx, 0)
  expect_equal(s$n_complete, 0L)
})

# ---- public API ------------------------------------------------------------

test_that("animate_group_summarize returns a well-formed animate_html object", {
  out <- animate_group_summarize(grouped_df(), sp, n = n(), seed = 1)

  expect_s3_class(out, "animate_html")
  expect_true(inherits(out, "html"))

  p <- payload_of(out)
  expect_equal(p$verb, "group_summarize")
  expect_match(p$title, "group_by(grouped_df(), sp)", fixed = TRUE)
  expect_named(p, c("verb", "title", "expression", "verb_label", "group_col",
                    "groups", "summary_cols", "before", "after", "colors",
                    "disclosure", "note", "callout"))
  expect_equal(p$group_col, "sp")
  expect_equal(unlist(p$summary_cols), "n")
})

test_that("arbitrary summary functions compute correct values on shown rows", {
  out <- animate_group_summarize(grouped_df(), sp,
                                 n = dplyr::n(), avg = mean(m),
                                 s = sd(m), med = median(m), seed = 1)
  p <- payload_of(out)

  shown <- data.frame(sp = payload_col(p$before, "sp"),
                      m  = payload_num(p$before, "m"),
                      stringsAsFactors = FALSE)
  expected <- dplyr::summarise(dplyr::group_by(shown, sp),
                               n = dplyr::n(), avg = mean(m),
                               s = sd(m), med = median(m), .groups = "drop")
  expected <- expected[match(payload_col(p$after, "sp"), expected$sp), ]

  expect_equal(payload_num(p$after, "n"),   as.numeric(expected$n))
  expect_equal(payload_num(p$after, "avg"), expected$avg, tolerance = 1e-3)
  expect_equal(payload_num(p$after, "s"),   expected$s,   tolerance = 1e-3)
  expect_equal(payload_num(p$after, "med"), expected$med, tolerance = 1e-3)
})

test_that("at most 3 groups are shown, with deliberately uneven sizes", {
  out <- animate_group_summarize(grouped_df(), sp, n = dplyr::n(), seed = 1)
  p   <- payload_of(out)

  expect_lte(length(p$groups), 3)
  expect_equal(vapply(p$groups, function(g) g$count, integer(1)),
               c(3L, 2L, 1L))
  expect_equal(p$disclosure$hidden_groups, 1L)   # 4 groups exist, 3 shown
})

test_that("badge counts equal the computed n() values, box by box", {
  out <- animate_group_summarize(grouped_df(), sp, n = n(), seed = 1)
  p   <- payload_of(out)

  labels <- vapply(p$groups, function(g) g$label, character(1))
  badges <- vapply(p$groups, function(g) g$count, integer(1))
  expect_equal(payload_col(p$after, "sp"), labels)  # rows align with boxes
  expect_equal(payload_num(p$after, "n"), as.numeric(badges))
})

test_that("group row_indices are 0-based and partition the shown rows", {
  out <- animate_group_summarize(grouped_df(), sp, n = dplyr::n(), seed = 1)
  p   <- payload_of(out)
  idx <- sort(unlist(lapply(p$groups, `[[`, "row_indices")))
  expect_equal(as.integer(idx), seq_along(p$before$rows) - 1L)
})

test_that("each shown group carries its own color", {
  out <- animate_group_summarize(grouped_df(), sp, n = dplyr::n(), seed = 1)
  cols <- vapply(payload_of(out)$groups, function(g) g$color, character(1))
  expect_equal(length(unique(cols)), length(cols))
})

test_that("same seed -> identical group_summarize payload", {
  expect_identical(
    payload_of(animate_group_summarize(grouped_df(), sp, avg = mean(m),
                                       seed = 7)),
    payload_of(animate_group_summarize(grouped_df(), sp, avg = mean(m),
                                       seed = 7))
  )
})

test_that("animate_group_summarise is an exact alias (spelling aside)", {
  a <- payload_of(animate_group_summarize(grouped_df(), sp, n = dplyr::n(),
                                          seed = 3))
  b <- payload_of(animate_group_summarise(grouped_df(), sp, n = dplyr::n(),
                                          seed = 3))
  expect_match(b$title, "summarise(", fixed = TRUE)
  a$title <- b$title <- NULL
  a$verb_label <- b$verb_label <- NULL
  expect_identical(a, b)
})

test_that("rows with NA in displayed columns are never shown", {
  df <- grouped_df()
  df$m[c(1, 11, 19)] <- NA
  out <- animate_group_summarize(df, sp, avg = mean(m), seed = 1)
  expect_false(any(unlist(payload_of(out)$before$rows) == "NA"))
})

test_that("the group_summarize payload survives a JSON round trip intact", {
  out <- animate_group_summarize(grouped_df(), sp, n = dplyr::n(),
                                 avg = mean(m), seed = 1)
  p    <- payload_of(out)
  json <- jsonlite::toJSON(p, auto_unbox = TRUE, null = "null")
  expect_true(jsonlite::validate(json))

  back <- jsonlite::fromJSON(json, simplifyVector = FALSE)
  expect_equal(back$verb, "group_summarize")
  expect_length(back$groups, length(p$groups))
  # even a 1-row group keeps row_indices as an array, never a bare scalar
  expect_true(all(vapply(back$groups,
                         function(g) is.list(g$row_indices), logical(1))))
  expect_length(back$after$rows, length(p$groups))
})

# ---- edge cases and errors -------------------------------------------------

test_that("a single-group frame renders with a grand-summary note", {
  df <- data.frame(g = rep("only", 5), v = 1:5)
  out <- animate_group_summarize(df, g, avg = mean(v), seed = 1)
  p   <- payload_of(out)

  expect_null(p$callout)                # the animation still renders
  expect_match(p$note, "one group")
  expect_length(p$groups, 1)
  expect_length(p$after$rows, 1)
})

test_that("a missing group column errors clearly", {
  expect_error(animate_group_summarize(grouped_df(), nope, n = dplyr::n()),
               "not found")
})

test_that("unnamed summary expressions error with naming advice", {
  expect_error(animate_group_summarize(grouped_df(), sp, mean(m)),
               "must be named")
})

test_that("at least one summary expression is required", {
  expect_error(animate_group_summarize(grouped_df(), sp),
               "at least one named summary")
})

# ---- edge-case detector ----------------------------------------------------

test_that(".detect_edge_cases classifies group_summarize boundaries", {
  expect_match(
    animatedplyr:::.detect_edge_cases("group_summarize", n_groups = 1L),
    "one group"
  )
  expect_match(
    animatedplyr:::.detect_edge_cases("group_summarize", n_complete = 0L),
    "No complete rows"
  )
  expect_null(
    animatedplyr:::.detect_edge_cases("group_summarize", n_groups = 3L,
                                      n_complete = 10L)
  )
})
