# =============================================================================
# R/with_animation.R — one-call wrapper
# =============================================================================

#' Animate any supported dplyr verb in one call
#'
#' Captures a dplyr verb expression and routes it to the matching animator.
#' This is the recommended classroom syntax.
#'
#' @param data A data frame.
#' @param expr A dplyr verb call: `filter(...)`, `select(...)`, or
#'   `mutate(...)`.
#' @param n_rows Maximum rows to display. Default 5.
#' @param seed Optional integer for reproducible sampling.
#' @param config Optional configuration list from [animate_config()].
#'
#' @return An object of class `"animate_html"`.
#' @export
#'
#' @examples
#' with_animation(mtcars, filter(mpg > 19.3))
#' with_animation(mtcars, select(mpg, cyl, hp))
#' with_animation(mtcars, mutate(wt_kg = wt * 453.6))
with_animation <- function(data, expr, n_rows = 5L, seed = NULL,
                           config = NULL) {
  expr_quo <- rlang::enquo(expr)
  call <- rlang::quo_get_expr(expr_quo)

  if (!rlang::is_call(call)) {
    stop("`expr` must be a dplyr verb call like filter(...), select(...), ",
         "or mutate(...).", call. = FALSE)
  }

  verb <- as.character(call[[1]])
  # tolerate dplyr::filter style
  if (length(verb) > 1L) verb <- verb[length(verb)]
  args <- as.list(call)[-1]

  env <- rlang::quo_get_env(expr_quo)

  if (verb == "filter") {
    if (length(args) != 1L) {
      stop("with_animation() supports a single filter condition.", call. = FALSE)
    }
    fn_call <- rlang::call2(animate_filter, rlang::enexpr(data), args[[1]],
                            n_rows = n_rows, seed = seed, config = config)
    return(rlang::eval_tidy(fn_call, env = env))
  }
  if (verb == "select") {
    fn_call <- rlang::call2(animate_select, rlang::enexpr(data), !!!args,
                            n_rows = n_rows, seed = seed, config = config)
    return(rlang::eval_tidy(fn_call, env = env))
  }
  if (verb == "mutate") {
    fn_call <- rlang::call2(animate_mutate, rlang::enexpr(data), !!!args,
                            n_rows = n_rows, seed = seed, config = config)
    return(rlang::eval_tidy(fn_call, env = env))
  }

  stop("Unsupported verb: '", verb,
       "'. Supported verbs are filter, select, mutate.", call. = FALSE)
}


# =============================================================================
# print method — makes console use open the Viewer pane
# =============================================================================

#' Print an animation
#'
#' At an interactive console, opens the animation in the Viewer pane
#' (RStudio/Positron) or the default browser. Inside knitr/Quarto rendering,
#' the underlying `html` class is picked up automatically and embedded inline,
#' so this method is bypassed.
#'
#' @param x An object of class `"animate_html"`.
#' @param ... Ignored.
#' @export
print.animate_html <- function(x, ...) {
  if (interactive()) {
    htmltools::html_print(htmltools::HTML(as.character(x)))
  } else {
    cat(as.character(x), "\n")
  }
  invisible(x)
}
