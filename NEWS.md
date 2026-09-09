# nightowl 0.1.0

First release of the rebuilt package.

* Four pillars, one idea: `Plot` objects render to SVG once and can live in a
  `NightowlPlots` column; `DeclarativePlot` and YAML styles declare plots as
  data; `Summary` and `calc_summary()` build summary tables with inline
  plots; `render_kable()`, `render_html()` and `render_reactable()` show them.
* Plot specifications are validated (`validate_spec()`); unknown fields,
  mapping keys and layer types are errors instead of silent no-ops.
* Layer verbs `layer_*()` form the vocabulary of style files and can be used
  directly on a ggplot.
* Built-in colour-blind-safe palettes (`nightowl_palettes()`), semantic
  colour roles, `theme_nightowl()` and a shared stylesheet
  (`nightowl_dependency()`).
* Package options live in `options()` under the `nightowl.` prefix
  (`nightowl_options()`).
* `expect_snapshot_svg()` for SVG snapshot tests with side-by-side review.
* Survival analysis (Cox models, Kaplan-Meier) moves to the nightwatch
  package. Meta-analysis, donut plots, correlation matrices, grouped
  chi-square tables, stacked percentage plots and all private-package
  dependencies were removed.
