# =============================================================================
# R/animate_verbs.R -- the three exported animators
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
  
  ## --------------
  ## A) Create the configuration, capture the condition, and get the data name
  ## --------------
  cfg <- .merge_config(config)
  cond_quo  <- rlang::enquo(condition)
  cond_text <- rlang::as_label(cond_quo)
  data_name <- rlang::as_label(rlang::enquo(data))


  ## --------------
  ## B) Evaluate the condition and handle missing values
  ## --------------
  df <- as.data.frame(data, stringsAsFactors = FALSE)
  mask <- rlang::eval_tidy(cond_quo, data = df)
  if (!is.logical(mask)) {
    stop("The condition must evaluate to a logical vector.", call. = FALSE)
  }
  mask[is.na(mask)] <- FALSE


  ## --------------
  ## C) Create the title and detect edge cases
  ## --------------
  title <- paste0("filter(", data_name, ", ", cond_text, ")")
  callout <- .detect_edge_cases("filter",
                                n_kept = sum(mask), n_total = nrow(df))

  ## --------------
  ## D) Identify columns used in the condition
  ## --------------
  cond_cols <- intersect(all.vars(rlang::quo_get_expr(cond_quo)), colnames(df))


  ## --------------
  ## E) Handle edge cases with a callout message
  ## --------------
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

  ## --------------
  ## F) Sample rows and columns to show in the animation
  ## --------------
  s <- .with_seed(seed, {
    rows <- .sample_for_filter(df, mask, n = n_rows)
    cols <- .sample_cols(df, required = cond_cols, max_cols = cfg$max_cols)
    list(rows = rows, cols = cols)
  })

  ## --------------
  ## G) Create the "before" and "after" data frames for the animation
  ## --------------
  shown   <- df[s$rows$rows_idx, s$cols, drop = FALSE]
  kept_df <- shown[s$rows$flags, , drop = FALSE]


  ## --------------
  ## H) Create the payload for the animation
  ## --------------
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
  ## --------------
  ## I) Render the animation
  ## --------------
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

  ## --------------
  ## A) Create the configuration and determine the data name
  ## --------------                          
  cfg <- .merge_config(config)
  data_name <- rlang::as_label(rlang::enquo(data))


  ## --------------
  ## B) Create the title and other strings for the animation
  ## --------------
  df <- as.data.frame(data, stringsAsFactors = FALSE)
  selected_df  <- dplyr::select(df, ...)
  selected_cols <- colnames(selected_df)
  sel_text <- paste(selected_cols, collapse = ", ")
  title <- paste0("select(", data_name, ", ", sel_text, ")")

  ## --------------
  ## C) Handle edge cases with a callout message
  ## --------------
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

  ## --------------
  ## D) Sample rows and columns to show in the animation 
  ## --------------
  s <- .sample_for_select(df, selected_cols, n = n_rows, seed = seed,
                          max_cols = cfg$max_cols)

  ## --------------
  ## E) Create the "before" and "after" data frames for the animation
  ## --------------
  shown_before <- df[s$rows_idx, s$show_cols, drop = FALSE]
  shown_after  <- shown_before[, intersect(s$show_cols, selected_cols),
                               drop = FALSE]


  ## --------------
  ## F) Create the payload for the animation
  ## --------------
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
  ## --------------
  ## G) Render the animation
  ## --------------
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

  ## --------------
  ## A) Create the configuration and determine the data name
  ## --------------
  cfg <- .merge_config(config)
  data_name <- rlang::as_label(rlang::enquo(data))

  ## --------------
  ## B) Capture the expression and check for errors 
  ## --------------
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
  ## --------------
  ## C) Create the data frame and determine the new column
  ## --------------
  df <- as.data.frame(data, stringsAsFactors = FALSE)
  new_col   <- names(dots)[1]
  expr_text <- paste0(new_col, " = ", rlang::as_label(dots[[1]]))
  title     <- paste0("mutate(", data_name, ", ", expr_text, ")")


  ## --------------
  ## D) Identify source columns used in the expression 
  ## --------------
  source_cols <- intersect(all.vars(rlang::quo_get_expr(dots[[1]])),
                           colnames(df))
  if (length(source_cols) == 0L) {
    # constant mutate like mutate(df, flag = 1) -- legal; nothing to highlight
    source_cols <- character(0)
  }

  ## --------------
  ## E) Sample rows and columns to show in the animation
  ## --------------
  # Reserve one column slot for the new column mutate() adds.
  s <- .sample_for_mutate(df, source_cols, n = n_rows, seed = seed,
                          max_cols = max(1L, cfg$max_cols - 1L))


  ## --------------
  ## F) Create the "before" and "after" data frames for the animation
  ## --------------
  shown_before <- df[s$rows_idx, s$show_cols, drop = FALSE]
  shown_after  <- dplyr::mutate(shown_before, !!!dots)


  ## --------------
  ## G) Create the payload for the animation
  ## --------------
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
  ## --------------
  ## H) Render the animation
  ## --------------
  .as_animate_html(.html_template(payload, cfg), payload, cfg)
}


#' Animate an arrange() operation
#'
#' Shows how `dplyr::arrange()` reorders rows: the sort column(s) light up,
#' then the rows reshuffle into sorted order. The closing frame stresses the
#' key pedagogical point: `arrange()` changes the ORDER of rows, never their
#' CONTENT -- same rows in, same rows out. The sampler deliberately avoids
#' picking rows that already happen to be in sorted order, so there is always
#' a visible reshuffle; when the data itself is already sorted (or the sort
#' column is constant), a callout explains that instead.
#'
#' @param data A data frame.
#' @param ... Sort columns, exactly as you would pass to `dplyr::arrange()`,
#'   including `desc()`, e.g. `animate_arrange(mtcars, desc(mpg))` or
#'   `animate_arrange(penguins, species, desc(body_mass_g))`.
#' @param n_rows Maximum rows to display. Default 5.
#' @param seed Optional integer for reproducible sampling.
#' @param config Optional configuration list from [animate_config()].
#'
#' @return An object of class `"animate_html"`.
#' @export
#'
#' @examples
#' animate_arrange(mtcars, desc(mpg))
#' animate_arrange(mtcars, cyl, desc(mpg), seed = 42)
animate_arrange <- function(data, ..., n_rows = 5L, seed = NULL,
                            config = NULL) {

  ## --------------
  ## A) Create the configuration, capture the sort spec, get the data name
  ## --------------
  cfg <- .merge_config(config)
  dots <- rlang::enquos(...)
  if (length(dots) == 0L) {
    stop("animate_arrange() needs at least one sort column, e.g. ",
         "animate_arrange(df, desc(mpg)).", call. = FALSE)
  }
  data_name <- rlang::as_label(rlang::enquo(data))
  df <- as.data.frame(data, stringsAsFactors = FALSE)

  ## --------------
  ## B) Parse each sort term into a column label and a direction
  ## --------------
  sort_cols  <- character(length(dots))
  directions <- character(length(dots))
  for (i in seq_along(dots)) {
    ex <- rlang::quo_get_expr(dots[[i]])
    if (rlang::is_call(ex, "desc")) {
      directions[i] <- "desc"
      sort_cols[i]  <- rlang::as_label(ex[[2]])
    } else {
      directions[i] <- "asc"
      sort_cols[i]  <- rlang::as_label(ex)
    }
  }
  expr_text <- paste(vapply(dots, rlang::as_label, character(1)),
                     collapse = ", ")
  title <- paste0("arrange(", data_name, ", ", expr_text, ")")

  ## --------------
  ## C) Compute the full-data sort order and sample a reshuffle-able slice
  ## --------------
  callout <- .detect_edge_cases("arrange", n_total = nrow(df))
  s <- NULL
  if (is.null(callout)) {
    ord_df <- df
    ord_df[["..animatedplyr_row.."]] <- seq_len(nrow(df))
    ord <- dplyr::arrange(ord_df, !!!dots)[["..animatedplyr_row.."]]
    sorted_pos <- match(seq_len(nrow(df)), ord)

    req_cols <- intersect(
      unique(unlist(lapply(dots,
                           function(q) all.vars(rlang::quo_get_expr(q))))),
      colnames(df))

    s <- .with_seed(seed, {
      rows <- .sample_for_arrange(df, sorted_pos, n = n_rows)
      cols <- .sample_cols(df, required = req_cols, max_cols = cfg$max_cols)
      list(rows = rows, cols = cols)
    })
    callout <- .detect_edge_cases("arrange", n_total = nrow(df),
                                  already_sorted = s$rows$already_sorted)
  }

  ## --------------
  ## D) Handle edge cases with a callout message
  ## --------------
  if (!is.null(callout)) {
    payload <- list(
      verb = "arrange", title = title, expression = expr_text,
      sort_cols = as.list(sort_cols), directions = as.list(directions),
      before = .df_to_payload(df[0, , drop = FALSE]),
      after  = .df_to_payload(df[0, , drop = FALSE]),
      colors = list(),
      disclosure = list(hidden_rows = 0L, hidden_cols = 0L),
      callout = callout
    )
    return(.as_animate_html(.html_template(payload, cfg), payload, cfg))
  }

  ## --------------
  ## E) Create the "before" (sampled order) and "after" (sorted) frames
  ## --------------
  shown        <- df[s$rows$rows_idx, s$cols, drop = FALSE]
  shown_sorted <- dplyr::arrange(shown, !!!dots)

  ## --------------
  ## F) Create the payload for the animation
  ## --------------
  payload <- list(
    verb       = "arrange",
    title      = title,
    expression = expr_text,
    sort_cols  = as.list(sort_cols),
    directions = as.list(directions),
    before     = .df_to_payload(shown),
    after      = .df_to_payload(shown_sorted),
    colors     = .dark_colors(s$cols, overrides = cfg$colors),
    disclosure = .disclosure(df, length(s$rows$rows_idx), s$cols),
    callout    = NULL
  )
  ## --------------
  ## G) Render the animation
  ## --------------
  .as_animate_html(.html_template(payload, cfg), payload, cfg)
}


#' Animate group_by() + summarize() together
#'
#' One animation for the whole aggregation story: rows first gather into
#' dashed, color-coded group boxes (`group_by()` tags rows -- the data is
#' unchanged), then each group collapses to a single summary row with the
#' requested values computed by `dplyr::summarise()` on the shown rows.
#' Up to 3 groups (the most frequent) are displayed with deliberately uneven
#' sizes so students see that groups differ, and each group's color follows
#' it from box to badge to key cell to summary row.
#'
#' `animate_group_summarise()` is an exact alias.
#'
#' @param data A data frame.
#' @param group_var One unquoted column to group by, e.g. `species`.
#' @param ... Named summary expressions, exactly as you would pass to
#'   `dplyr::summarise()`, e.g. `n = n(), avg_mass = mean(body_mass_g)`.
#'   Any summary function works; every expression must be named.
#' @param n_rows Maximum rows to display. Default 6.
#' @param seed Optional integer for reproducible sampling.
#' @param config Optional configuration list from [animate_config()].
#'
#' @return An object of class `"animate_html"`.
#' @export
#'
#' @examples
#' animate_group_summarize(mtcars, cyl, n = dplyr::n(), avg_mpg = mean(mpg))
animate_group_summarize <- function(data, group_var, ..., n_rows = 6L,
                                    seed = NULL, config = NULL) {
  # capture the label BEFORE anything forces the `data` promise
  data_name <- rlang::as_label(rlang::enquo(data))
  .animate_group_summarize(data, rlang::enquo(group_var), rlang::enquos(...),
                           data_name = data_name, verb_word = "summarize",
                           n_rows = n_rows, seed = seed, config = config)
}

#' @rdname animate_group_summarize
#' @export
animate_group_summarise <- function(data, group_var, ..., n_rows = 6L,
                                    seed = NULL, config = NULL) {
  data_name <- rlang::as_label(rlang::enquo(data))
  .animate_group_summarize(data, rlang::enquo(group_var), rlang::enquos(...),
                           data_name = data_name, verb_word = "summarise",
                           n_rows = n_rows, seed = seed, config = config)
}

# Shared implementation behind animate_group_summarize()/-ise(). `group_quo`
# and `dots` arrive pre-captured so both spellings report the user's own
# calls in errors and titles.
.animate_group_summarize <- function(data, group_quo, dots, data_name,
                                     verb_word, n_rows, seed, config) {

  ## --------------
  ## A) Create the configuration and validate the inputs
  ## --------------
  cfg <- .merge_config(config)
  df <- as.data.frame(data, stringsAsFactors = FALSE)

  group_col <- rlang::as_label(group_quo)
  if (!group_col %in% colnames(df)) {
    stop("Column '", group_col, "' not found in the data. `group_var` must ",
         "be one unquoted column name, e.g. animate_group_summarize(",
         "penguins, species, n = n()).", call. = FALSE)
  }
  if (length(dots) == 0L) {
    stop("animate_group_summarize() needs at least one named summary ",
         "expression, e.g. avg_mass = mean(body_mass_g).", call. = FALSE)
  }
  nm <- names(dots)
  if (is.null(nm) || any(!nzchar(nm))) {
    stop("Every summary expression must be named (name = expression), e.g. ",
         "animate_group_summarize(df, species, avg = mean(body_mass_g)).",
         call. = FALSE)
  }

  ## --------------
  ## B) Create the title and identify the columns the summaries need
  ## --------------
  expr_text <- paste(paste0(nm, " = ",
                            vapply(dots, rlang::as_label, character(1))),
                     collapse = ", ")
  title <- paste0("group_by(", data_name, ", ", group_col, ") |> ",
                  verb_word, "(", expr_text, ")")
  needed_cols <- intersect(
    unique(unlist(lapply(dots,
                         function(q) all.vars(rlang::quo_get_expr(q))))),
    colnames(df))

  ## --------------
  ## C) Sample up to 3 uneven groups of NA-free rows
  ## --------------
  s <- .sample_for_group_summarize(df, group_col, needed_cols, n = n_rows,
                                   max_cols = cfg$max_cols, seed = seed)

  ## --------------
  ## D) Handle edge cases with a callout message
  ## --------------
  callout <- .detect_edge_cases("group_summarize", n_complete = s$n_complete)
  if (!is.null(callout)) {
    payload <- list(
      verb = "group_summarize", title = title, expression = expr_text,
      verb_label = verb_word, group_col = group_col, groups = list(),
      summary_cols = as.list(nm),
      before = .df_to_payload(df[0, , drop = FALSE]),
      after  = .df_to_payload(df[0, , drop = FALSE]),
      colors = list(),
      disclosure = list(hidden_rows = 0L, hidden_cols = 0L,
                        hidden_groups = 0L),
      note = NULL, callout = callout
    )
    return(.as_animate_html(.html_template(payload, cfg), payload, cfg))
  }

  ## --------------
  ## E) Compute the summaries with real dplyr::summarise on the shown rows
  ## --------------
  # Bare n() must work even when dplyr isn't attached (fresh classroom
  # session): if a quosure calls n() and its environment can't find one,
  # graft on a binding to dplyr::n, which reads the group context
  # dynamically inside summarise().
  dots <- lapply(dots, function(q) {
    if (!"n" %in% all.names(rlang::quo_get_expr(q))) return(q)
    env <- rlang::quo_get_env(q)
    if (rlang::env_has(env, "n", inherit = TRUE)) return(q)
    e <- new.env(parent = env)
    e$n <- dplyr::n
    rlang::quo_set_env(q, e)
  })

  shown <- df[s$rows_idx, s$show_cols, drop = FALSE]
  summary_df <- dplyr::summarise(dplyr::group_by(shown, !!group_quo),
                                 !!!dots, .groups = "drop")
  # reorder to match the group boxes (most frequent group first)
  glabels <- vapply(s$groups, `[[`, character(1), "value")
  summary_df <- summary_df[
    match(glabels, as.character(summary_df[[group_col]])), , drop = FALSE]

  ## --------------
  ## F) Assign each shown group its color (box, badge, key cell, outline)
  ## --------------
  gcolors <- .group_palette(length(s$groups))
  groups_payload <- lapply(seq_along(s$groups), function(i) {
    g <- s$groups[[i]]
    list(label = g$value,
         count = length(g$rows_pos),
         color = gcolors[[i]],
         row_indices = I(as.integer(g$rows_pos) - 1L))   # 0-based for JS
  })

  ## --------------
  ## G) Create the payload for the animation
  ## --------------
  disclosure <- .disclosure(df, length(s$rows_idx), s$show_cols)
  disclosure$hidden_groups <- s$n_groups_total - length(s$groups)

  payload <- list(
    verb         = "group_summarize",
    title        = title,
    expression   = expr_text,
    verb_label   = verb_word,
    group_col    = group_col,
    groups       = groups_payload,
    summary_cols = as.list(nm),
    before       = .df_to_payload(shown),
    after        = .df_to_payload(summary_df),
    colors       = .dark_colors(union(s$show_cols, colnames(summary_df)),
                                overrides = cfg$colors),
    disclosure   = disclosure,
    note         = .detect_edge_cases("group_summarize",
                                      n_groups = s$n_groups_total),
    callout      = NULL
  )
  ## --------------
  ## H) Render the animation
  ## --------------
  .as_animate_html(.html_template(payload, cfg), payload, cfg)
}
