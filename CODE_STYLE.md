# Code style

This describes how nightowl is actually written, so that new code matches.

## Shape of the code

* **Fully qualified calls to other packages** (`ggplot2::ggplot()`), never
  `library()`. Internal calls are unqualified.
* **R6 for objects with a lifecycle** (`Plot`, `DeclarativePlot`, `Summary`):
  explicit constructor formals, one fixed build sequence in `initialize()`,
  renderers as thin methods over a shared `raw()`/`svg()`. No `...` field
  injection: an unknown argument must be an error.
* **Plain functions with a uniform contract** where a family is meant to be
  composed: layer verbs are `function(g, mapping = list(), ...)` and return
  `g + layer`; summary calculations are `function(x, ...)` returning a scalar,
  a one-row data frame, or a `NightowlPlots`.
* **Declarations over code.** Plots are specifications; specifications are
  validated (`validate_spec()`) before evaluation. Functions named in a
  specification are resolved by `resolve_function()`; there is no
  `eval(parse())`.
* **Plots are values.** Anything that draws returns a `Plot` or a
  `NightowlPlots`, never a side effect.
* **Provenance is data.** Captions such as the summary method or an axis zoom
  are derived from the specification, not collected as side effects.

## Conventions

* Names: `snake_case` functions, `UpperCamel` R6 classes, verbs first
  (`render_`, `add_inline_`, `layer_`, `calc_`, `format_`). Options and
  palettes carry the `nightowl_` prefix. British spelling in the API
  (`colour`), American accepted where ggplot2 does.
* `TRUE`/`FALSE`, never `T`/`F`. Native pipe `|>` internally.
* Errors through `cli::cli_abort()` naming the offending value and the
  allowed set. Validation helpers live in `R/utils.R`.
* Every export has a title, description, `@param`, `@return` and a runnable
  example. Internal helpers use `@noRd`.
* Defaults come from `nightowl_option()`, never from a global variable.
* Styling of HTML output lives in `inst/assets/css/nightowl.css`, not in
  inline `style=` strings.

## Tests

* testthat 3e, base and `survival` datasets only, `set.seed()` where
  randomness is involved.
* Every behaviour has an assertion; a test without `expect_*()` is not a
  test.
* Numeric results are checked against an independent computation
  (`t.test()`, `tapply()`, `table()`).
* Figures: `vdiffr::expect_doppelganger()` on ggplot objects,
  `expect_snapshot_svg()` on rendered output. Review changes with
  `testthat::snapshot_review()`.

## Things that were removed on purpose

Private helper packages, `:::`, `eval(parse())`, `assign()` into other
frames or the global environment, exports that mask base or dplyr names,
commented-out code, `browser()` calls, and REPL transcripts in `tests/`.
