# Contributing

Thanks for helping with nightowl.

## Setup

```r
pak::pak(c("devtools", "rcmdcheck", "vdiffr", "xml2"))
devtools::load_all()
devtools::test()
```

## Workflow

1. Open an issue describing the change; it is added to the nightowl project board.
2. Branch from `main`, make the change with tests and roxygen.
3. Run `devtools::document()`, `devtools::test()` and
   `rcmdcheck::rcmdcheck(args = c("--as-cran", "--no-manual"))`. The check must
   be clean: no errors, warnings or notes.
4. Update `NEWS.md` under the development version.
5. Open a pull request. CI runs the check matrix, coverage and the pkgdown build.

## Figures

If a change alters a figure on purpose, run the tests, review the snapshots
with `testthat::snapshot_review()`, accept them, and commit the updated
files under `tests/testthat/_snaps/`.

See `CODE_STYLE.md` for how the code is written.
