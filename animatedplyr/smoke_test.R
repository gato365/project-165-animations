# ============================================================
# smoke_test.R — run this once after installing to verify v0.1.0
#   Rscript smoke_test.R       (from the package root, after install)
# or interactively:
#   devtools::load_all(); source("smoke_test.R")
# ============================================================

library(animatedplyr)

ok <- function(label, expr) {
  res <- tryCatch({ force(expr); TRUE }, error = function(e) {
    message("FAIL: ", label, " — ", conditionMessage(e)); FALSE
  })
  if (res) message("ok:   ", label)
  res
}

results <- c(
  ok("filter returns animate_html",
     stopifnot(inherits(animate_filter(mtcars, mpg > 19.3, seed = 1), "animate_html"))),
  ok("select returns animate_html",
     stopifnot(inherits(animate_select(mtcars, mpg, cyl, seed = 1), "animate_html"))),
  ok("mutate returns animate_html",
     stopifnot(inherits(animate_mutate(mtcars, kg = wt * 453.6, seed = 1), "animate_html"))),
  ok("with_animation routes filter",
     stopifnot(attr(with_animation(mtcars, filter(mpg > 19.3), seed = 1),
                    "animate_payload")$verb == "filter")),
  ok("seed reproducibility",
     stopifnot(identical(
       attr(animate_filter(mtcars, mpg > 19.3, seed = 7), "animate_payload")$before,
       attr(animate_filter(mtcars, mpg > 19.3, seed = 7), "animate_payload")$before))),
  ok("all-keep edge case callout",
     stopifnot(grepl("All rows",
       attr(animate_filter(mtcars, mpg > 0), "animate_payload")$callout))),
  ok("config flows into HTML",
     stopifnot(grepl("120px", as.character(
       animate_filter(mtcars, mpg > 19.3, seed = 1,
                      config = animate_config(box_size = 120)))))),
  ok("payload JSON parses",
     stopifnot(!is.null(jsonlite::fromJSON(jsonlite::toJSON(
       attr(animate_filter(mtcars, mpg > 19.3, seed = 1), "animate_payload"),
       auto_unbox = TRUE, null = "null")))))
)

if (all(results)) {
  message("\nAll ", length(results), " smoke tests passed.")
  message("Now open the animation in the Viewer:")
  message("  animate_filter(mtcars, mpg > 19.3, seed = 42)")
} else {
  stop(sum(!results), " smoke test(s) failed.")
}
