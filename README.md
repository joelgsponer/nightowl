# nightowl

<!-- badges: start -->
[![R-CMD-check](https://github.com/joelgsponer/nightowl/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/joelgsponer/nightowl/actions/workflows/R-CMD-check.yaml)
[![Codecov test coverage](https://codecov.io/gh/joelgsponer/nightowl/graph/badge.svg)](https://app.codecov.io/gh/joelgsponer/nightowl)
<!-- badges: end -->

Declarative plots and summary tables with inline graphics, built on ggplot2.

nightowl treats a plot as a value. A `Plot` renders itself to SVG once; a
`NightowlPlots` vector holds many, so a data frame column can be a column of
sparklines that render inside `reactable` and `kableExtra` tables. Plots are
declared as layer stacks, in R or in YAML style files, and validated before
they are drawn.

Survival analysis (Cox models, Kaplan-Meier curves) lives in the companion
package [nightwatch](https://github.com/joelgsponer/nightwatch), which builds
on nightowl.

## Installation

```r
# install.packages("pak")
pak::pak("joelgsponer/nightowl")
```

## A plot from a specification

```r
library(nightowl)

p <- DeclarativePlot$new(
  data = ChickWeight,
  mapping = list(x = "Time", y = "weight", colour = "Diet", id = "Chick"),
  layers = list(
    list(type = "traces", alpha = 0.15),
    list(type = "summary", fun.data = "mean_se", geom = "line", linewidth = 1)
  )
)
p$plot      # the ggplot
p$html()    # SVG in an HTML block, rendered once and memoised
```

The same specification can be a YAML style:

```r
list_styles()
styled_plot(ChickWeight, "Boxplot-SummaryMean", x = "Time", y = "weight", fill = "Diet")
```

## A table with inline plots

```r
s <- Summary$new(mtcars, "mpg", group_by = "cyl", method = summarise_numeric_pointrange)
s$raw()        # a tibble; the Pointrange column is a NightowlPlots vector
s$reactable()  # interactive table with the sparklines inline
s$html()       # static HTML table
```

Any named list of functions is a summary template:

```r
calc_summary(
  dplyr::group_by(iris, Species), "Sepal.Length",
  calculations = list(N = length, Mean = format_mean_ci, Hist = add_inline_histogram),
  parameters = list(Hist = list(xlim = range(iris$Sepal.Length)))
)
```

## Testing figures

```r
test_that("the trend plot is stable", {
  expect_snapshot_svg(p, "trend")          # normalised SVG, reviewed side by side
  vdiffr::expect_doppelganger("trend", p$plot)
})
```

## Learn more

* `vignette("nightowl")` walks through the workflow.
* [Reference](https://joelgsponer.github.io/nightowl/reference/) lists every function by pillar.
