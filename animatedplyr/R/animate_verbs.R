# =============================================================================
# R/animate_verbs.R — the three exported animators
# =============================================================================

#' Animate a filter() operation
#'
#' Shows how `dplyr::filter()` evaluates a condition row by row, highlighting
#' kept rows (green) and dropped rows (red), then collapsing the dropped rows
#' away. The display is a smart sample: at most `n_rows` rows mixing kept and
#' dropped, and at most 4 columns (always including columns used in the
#' condition).
#'
#' @param data A data frame.
#' @param condition An unquoted condition, exactly as you would pass to
#'   `dplyr::filter()`.
#' @param n_rows Maximum rows to display. Default 5.
#' @param seed Optional integer for reproducible sampling. Default NULL
#'   (fresh sample each render).
#' @param config Optional configuration list from [animate_config()].
#'
#' @return An object of class `"animate_html"`. Prints inline below a
#'   Quarto/R Markdown chunk; opens in the Viewer pane at the console.
#' @export
#'
#' @examples
#' animate_filter(mtcars, mpg > 19.3)
#' animate_filter(mtcars, mpg > 19.3, seed = 42)
animate_filter <- function(data, condition, n_rows = 5L, seed = NULL,
                           config = NULL) {
  cfg <- .merge_config(config)
  cond_quo  <- rlang::enquo(condition)
  cond_text <- rlang::as_label(cond_quo)
  data_name <- rlang::as_label(rlang::enquo(data))

  df <- as.data.frame(data, stringsAsFactors = FALSE)
  mask <- rlang::eval_tidy(cond_quo, data = df)
  if (!is.logical(mask)) {
    stop("The condition must evaluate to a logical vector.", call. = FALSE)
  }
  mask[is.na(mask)] <- FALSE

  title <- paste0("filter(", data_name, ", ", cond_text, ")")
  callout <- .detect_edge_cases("filter",
                                n_kept = sum(mask), n_total = nrow(df))

  cond_cols <- intersect(all.vars(rlang::quo_get_expr(cond_quo)), colnames(df))

  if (!is.null(callout)) {
    payload <- list(
      verb = "filter", title = title, expression = cond_text,
      before = .df_to_payload(df[0, , drop = FALSE]),
      after  = .df_to_payload(df[0, , drop = FALSE]),
      row_flags = logical(0), colors = list(),
      disclosure = list(hidden_rows = 0L, hidden_cols = 0L),
      callout = callout
    )
    return(.as_animate_html(.html_template(payload, cfg), payload, cfg))
  }

  s <- .with_seed(seed, {
    rows <- .sample_for_filter(df, mask, n = n_rows)
    cols <- .sample_cols(df, required = cond_cols, max_cols = 4L)
    list(rows = rows, cols = cols)
  })

  shown   <- df[s$rows$rows_idx, s$cols, drop = FALSE]
  kept_df <- shown[s$rows$flags, , drop = FALSE]

  payload <- list(
    verb       = "filter",
    title      = title,
    expression = cond_text,
    before     = .df_to_payload(shown),
    after      = .df_to_payload(kept_df),
    row_flags  = as.logical(s$rows$flags),
    colors     = .default_colors(s$cols, overrides = cfg$colors),
    disclosure = .disclosure(df, length(s$rows$rows_idx), s$cols),
    callout    = NULL
  )
  .as_animate_html(.html_template(payload, cfg), payload, cfg)
}


#' Animate a select() operation
#'
#' Shows how `dplyr::select()` keeps the named columns and drops the rest:
#' dropped columns fade out, then the survivors recenter. When the data frame
#' has more columns than fit on screen, one non-selected column is sampled in
#' so students can watch something get dropped.
#'
#' @param data A data frame.
#' @param ... Columns to select, exactly as you would pass to
#'   `dplyr::select()`.
#' @param n_rows Maximum rows to display. Default 5.
#' @param seed Optional integer for reproducible sampling.
#' @param config Optional configuration list from [animate_config()].
#'
#' @return An object of class `"animate_html"`.
#' @export
#'
#' @examples
#' animate_select(mtcars, mpg, cyl, hp)
animate_select <- function(data, ..., n_rows = 5L, seed = NULL,
                           config = NULL) {
  cfg <- .merge_config(config)
  data_name <- rlang::as_label(rlang::enquo(data))

  df <- as.data.frame(data, stringsAsFactors = FALSE)
  selected_df  <- dplyr::select(df, ...)
  selected_cols <- colnames(selected_df)
  sel_text <- paste(selected_cols, collapse = ", ")
  title <- paste0("select(", data_name, ", ", sel_text, ")")

  callout <- .detect_edge_cases("select", n_cols_after = length(selected_cols))
  if (!is.null(callout)) {
    payload <- list(
      verb = "select", title = title, expression = sel_text,
      before = .df_to_payload(df[0, , drop = FALSE]),
      after  = .df_to_payload(df[0, , drop = FALSE]),
      colors = list(),
      disclosure = list(hidden_rows = 0L, hidden_cols = 0L),
      callout = callout
    )
    return(.as_animate_html(.html_template(payload, cfg), payload, cfg))
  }

  s <- .sample_for_select(df, selected_cols, n = n_rows, seed = seed)

  shown_before <- df[s$rows_idx, s$show_cols, drop = FALSE]
  shown_after  <- shown_before[, intersect(s$show_cols, selected_cols),
                               drop = FALSE]

  payload <- list(
    verb       = "select",
    title      = title,
    expression = sel_text,
    before     = .df_to_payload(shown_before),
    after      = .df_to_payload(shown_after),
    colors     = .default_colors(s$show_cols, overrides = cfg$colors),
    disclosure = .disclosure(df, length(s$rows_idx), s$show_cols),
    callout    = NULL
  )
  .as_animate_html(.html_template(payload, cfg), payload, cfg)
}


#' Animate a mutate() operation
#'
#' Shows how `dplyr::mutate()` computes a new column: the source column(s)
#' the expression depends on are highlighted, then the new column pops into
#' view with its computed values.
#'
#' @param data A data frame.
#' @param ... A single name-value pair, exactly as you would pass to
#'   `dplyr::mutate()`, e.g. `wt_kg = wt * 453.6`. (One new column per
#'   animation keeps the story readable; chain calls for more.)
#' @param n_rows Maximum rows to display. Default 5.
#' @param seed Optional integer for reproducible sampling.
#' @param config Optional configuration list from [animate_config()].
#'
#' @return An object of class `"animate_html"`.
#' @export
#'
#' @examples
#' animate_mutate(mtcars, wt_kg = wt * 453.6)
animate_mutate <- function(data, ..., n_rows = 5L, seed = NULL,
                           config = NULL) {
  cfg <- .merge_config(config)
  data_name <- rlang::as_label(rlang::enquo(data))

  dots <- rlang::enquos(...)
  if (length(dots) == 0L || is.null(names(dots)) || names(dots)[1] == "") {
    stop("animate_mutate() needs a named expression, e.g. ",
         "animate_mutate(df, wt_kg = wt * 453.6).", call. = FALSE)
  }
  if (length(dots) > 1L) {
    warning("animate_mutate() animates one new column at a time; ",
            "using the first and ignoring the rest.", call. = FALSE)
    dots <- dots[1]
  }

  df <- as.data.frame(data, stringsAsFactors = FALSE)
  new_col   <- names(dots)[1]
  expr_text <- paste0(new_col, " = ", rlang::as_label(dots[[1]]))
  title     <- paste0("mutate(", data_name, ", ", expr_text, ")")

  source_cols <- intersect(all.vars(rlang::quo_get_expr(dots[[1]])),
                           colnames(df))
  if (length(source_cols) == 0L) {
    # constant mutate like mutate(df, flag = 1) — legal; nothing to highlight
    source_cols <- character(0)
  }

  s <- .sample_for_mutate(df, source_cols, n = n_rows, seed = seed)

  shown_before <- df[s$rows_idx, s$show_cols, drop = FALSE]
  shown_after  <- dplyr::mutate(shown_before, !!!dots)

  payload <- list(
    verb       = "mutate",
    title      = title,
    expression = expr_text,
    source_col = if (length(source_cols)) source_cols[1] else NULL,
    before     = .df_to_payload(shown_before),
    after      = .df_to_payload(shown_after),
    colors     = .default_colors(colnames(shown_after),
                                 overrides = cfg$colors, new_col = new_col),
    disclosure = .disclosure(df, length(s$rows_idx), s$show_cols),
    callout    = NULL
  )
  .as_animate_html(.html_template(payload, cfg), payload, cfg)
}
