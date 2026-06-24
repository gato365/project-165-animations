# animatedplyr test suite

These tests are organized into **six layers**, ordered from the most
R-based / deterministic logic to the hardest environment-dependent behavior.
The numbering of the files reflects that order, so reading or running them
top-to-bottom moves from "pure logic" toward "needs a browser."

| File | Layer | Question it answers |
|------|-------|---------------------|
| `test-01-core-behavior.R`            | 1. Core behavior            | Does each function do the right thing on normal input? |
| `test-02-edge-cases.R`               | 2. Edge cases & failure     | Does it break *gracefully* on weird/wrong input? |
| `test-03-sampling-reproducibility.R` | 3. Sampling & reproducibility | Does it choose what to display consistently and intentionally? |
| `test-04-payload-html.R`             | 4. Payload & HTML structure | Does the R output hand the browser a valid structure? |
| `test-05-print-render.R`             | 5. Print / Quarto / Viewer  | Does the object display in teaching environments? |
| `test-06-save-gif.R`                 | 6. GIF export               | Does optional GIF export work, or fail clearly? |

`helper-fixtures.R` holds shared test data (`toy_df()`, `wide_df()`,
`nasty_df()`) and helpers (`payload_of()`, `config_of()`,
`gif_pipeline_available()`). testthat sources `helper-*.R` automatically
before any test file.

## What each layer is (and isn't)

1. **Core behavior** — the happy path. Normal filter/select/mutate examples;
   correct class (`animate_html` / `html`), structure, and payload.
2. **Edge cases & failure** — empty frames, no/all rows matching, zero columns
   selected, unknown columns, non-logical conditions, invalid config, unnamed
   mutate. The contract here is a clear **callout** or a clear **error** —
   never a random crash.
3. **Sampling & reproducibility** — the package's core promise. Same `seed`
   ⇒ same sample; display capped at ≤ 5 rows × ≤ 4 cols; filter shows kept
   *and* dropped rows; select/mutate always keep the columns that matter; the
   global RNG stream is left untouched.
4. **Payload & HTML structure** — *not* whether the animation looks perfect,
   only whether the browser receives valid materials: a JSON payload with the
   expected fields, a uniquely-id'd container div, embedded CSS/JS, config
   threaded into CSS variables, and **hostile data** (quotes, `&`, `NA`,
   unicode, long text, `</script>`) that can't break the page.
5. **Print / Quarto / Viewer** — deliberately **light**, because real
   RStudio/Positron Viewer behavior depends on the user's machine. We test the
   `print` method's contract and the html-ness that lets knitr/Quarto embed
   the object inline. Full IDE behavior is a manual smoke check (below).
6. **GIF export** — **opt-in**. Input validation always runs; the real
   end-to-end render is skipped unless `webshot2` + `magick` + a headless
   Chrome are all present, so a plain install never fails here.

## How to run the tests

From the package directory (`animatedplyr/`):

```r
# Everything (the normal command):
devtools::test()

# A single layer:
testthat::test_file("tests/testthat/test-03-sampling-reproducibility.R")

# Filter by name across all files:
devtools::test(filter = "reproducib")     # matches test-03 by file stem

# Full check (what CRAN/CI runs):
devtools::check()        # or:  R CMD check .
```

From a shell, without opening R:

```sh
Rscript -e 'devtools::test()'
Rscript -e 'devtools::check()'
```

> Tip: if you changed code in `R/`, reinstall or `devtools::load_all()` first —
> `devtools::test()` does the `load_all()` for you, but a bare
> `testthat::test_file()` against an installed package will test the *installed*
> version, not your working tree.

## How to read / evaluate the results

testthat prints one line per file with tallies:

```
✔ | F W S  OK | Context
✔ |        12 | 01-core-behavior
✔ |     2   9 | 06-save-gif        # 2 skipped (no Chrome) — expected
```

- **OK** — passing assertions. This is what you want.
- **F (Fail)** — a real failure. The output names the file, the
  `test_that()` description, the line, and the expected-vs-actual values.
  Start at the first failure; later ones are often downstream of it.
- **W (Warn)** — a warning fired during a test that didn't expect one. Treat
  as a yellow flag: usually a deprecation or an unguarded edge.
- **S (Skip)** — a test that opted out (e.g. GIF render with no Chrome,
  `knit_print` with knitr absent). Skips are **expected** for the optional
  layers and are not failures — but a skip where you expected a run means the
  dependency you wanted to exercise wasn't there.

A green run means: the verbs behave on normal input (L1), degrade gracefully
(L2), sample reproducibly within the caps (L3), and emit a valid embeddable
structure (L4/L5). The GIF layer (L6) is "green or skipped" depending on the
machine.

### Evaluating coverage (optional)

```r
covr::report(covr::package_coverage())
```

Aim to keep `R/sampling.R`, `R/config.R`, and `R/render.R` — the logic core —
at high coverage; `R/save_gif.R` and `R/studio.R` will read lower because
their heavy paths need a browser / a live Shiny session.

## What is intentionally NOT unit-tested (manual smoke checks)

Some behavior depends on a live IDE or browser and is verified by hand:

- **Viewer pane** — at an interactive console, `animate_filter(mtcars, mpg > 20)`
  should open in the RStudio **and** Positron Viewer.
- **Quarto / R Markdown** — knit a chunk containing an `animate_*()` call and
  confirm the animation appears inline and the Play/Prev/Next controls work.
- **The Shiny studio** — `animate_studio()`: live preview updates as you drag,
  verb-specific controls show/hide, and the "R code" tab is copy-pasteable.
- **GIF visual quality** — open an exported `.gif` and confirm each of the four
  steps is legible and the timing is readable.
