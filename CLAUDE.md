# CLAUDE.md

Guidance for Claude Code when working in this repository.

## Rules

* Use `package::function()` for every call to another package, including in
  tests and vignettes. Internal nightowl calls are unqualified.
* The package is called `nightowl`. Survival analysis lives in the companion
  package `nightwatch`, which depends on nightowl; do not add survival code
  here.
* GitHub issues go on the project board "nightowl" (columns: Backlog, Ready,
  In progress, In review, Done). Add every issue you create to the board in
  the right column.
* Read `CODE_STYLE.md` before writing code. Explicit formals, validation with
  `cli::cli_abort()`, no `:::`, no `eval(parse())`, no globals, `TRUE`/`FALSE`.
* Every export needs roxygen with `@return` and a runnable `@examples`.
  Every behaviour needs a test with an assertion.

## Layout

* `R/plot.R`, `R/nightowl-plots.R`, `R/svg.R`: `Plot`, the `NightowlPlots`
  vector, SVG rendering.
* `R/declarative-plot.R`, `R/layers.R`, `R/layer-registry.R`,
  `R/plot-finishers.R`, `R/styles.R`, `inst/styles/*.yaml`: declarative plots.
* `R/summary.R`, `R/summarise.R`, `R/group-test.R`, `R/inline-plots.R`:
  summary tables and sparklines.
* `R/tables.R`, `R/utils-html.R`, `inst/assets/`: table rendering, CSS and JS.
* `R/colours.R`, `R/theme.R`, `R/options.R`: palettes, theme, options.
* `R/snapshot-svg.R`: `expect_snapshot_svg()`.

## Commands

```r
devtools::document()
devtools::test()
rcmdcheck::rcmdcheck(args = c("--as-cran", "--no-manual"), error_on = "note")
pkgdown::build_site()
```

Snapshot review after intentional figure changes: `testthat::snapshot_review()`.
