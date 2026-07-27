# animatedplyr

Animated, interactive visualizations of dplyr verbs for statistics education.

```r
library(animatedplyr)
animate_filter(mtcars, mpg > 19.3)
```

Renders an interactive step-through animation **inline below a Quarto chunk**,
or **in the Viewer pane** when called at the console: rows are flagged
keep/drop, failing rows collapse away, and a gray pill discloses what the
sampler hid.

## Install

```r
# install.packages("devtools")
devtools::install_github("YOUR_GITHUB/animatedplyr")
```

## Quick start

```r
animate_filter(mtcars, mpg > 19.3)               # row-driven
animate_select(mtcars, mpg, cyl, hp)             # column-driven
animate_mutate(mtcars, wt_kg = wt * 453.6)       # new-column
animate_arrange(mtcars, desc(mpg))               # reorder rows
animate_group_summarize(mtcars, cyl,             # group + aggregate, one call
                        n = n(), avg_mpg = mean(mpg))

# one-call wrapper
with_animation(mtcars, filter(mpg > 19.3))
with_animation(mtcars, arrange(desc(mpg)))

# reproducible sampling for books / grading
animate_filter(mtcars, mpg > 19.3, seed = 42)

# theming
cfg <- animate_config(box_size = 120, duration = 1800)
animate_filter(mtcars, mpg > 19.3, config = cfg)
```

## Smart sampling

Any data frame is reduced to **≤ 5 rows × ≤ 4 columns** while preserving the
pedagogical signal:

- `filter()` samples a mix of kept **and** dropped rows (~60/40)
- `select()` always shows the selected columns, plus one dropped column when room allows
- `mutate()` always shows the source column(s) and the new column
- `arrange()` picks rows that are **not** already in sorted order, so there is always a visible reshuffle
- `animate_group_summarize()` shows up to 3 groups (the most frequent) with deliberately **uneven** sizes (3/2/1), so students see that groups differ before they collapse to one summary row each
- a gray pill reports what was hidden ("+27 rows · 7 cols · +2 groups not shown")
- degenerate cases (all rows pass / none pass / rows already sorted) render a callout instead of an empty animation

## Classroom examples (penguins + built-in NBA game logs)

```r
library(palmerpenguins)
animate_arrange(penguins, species, desc(body_mass_g), seed = 42)
animate_group_summarize(penguins, species,
                        n = n(), avg_mass = mean(body_mass_g), seed = 42)

# built-in game logs: kb_df (Kobe), lb_df (LeBron), mj_df (Jordan)
animate_arrange(kb_df, desc(PTS), seed = 42)
animate_group_summarize(lb_df, Game_Location,
                        n = n(), avg_pts = mean(PTS), seed = 42)
animate_group_summarize(mj_df, Game_Outcome,
                        avg_margin = mean(Point_Margin), seed = 42)
```

## GIF export (opt-in)

```r
install.packages(c("webshot2", "magick"))   # suggested, not required
anim <- animate_filter(mtcars, mpg > 19.3, seed = 42)
animate_save_gif(anim, "filter_demo.gif")
# then anywhere: ![](filter_demo.gif)
```

## Verify your install

```r
source(system.file("..", "smoke_test.R", package = "animatedplyr"))
# or from a git clone: Rscript smoke_test.R
```

## Demo document

A full demo lives at `inst/examples/demo.qmd` — render it with Quarto to see
every feature on one page.
