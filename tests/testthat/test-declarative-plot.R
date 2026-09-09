test_that("DeclarativePlot builds the declared layers", {
  d <- test_long()
  p <- DeclarativePlot$new(
    d, list(x = "t", y = "v", colour = "g", id = "id"),
    layers = list(
      list(type = "traces", alpha = 0.2),
      list(type = "summary", fun.data = "mean_se", geom = "line"),
      list(type = "points")
    ),
    svg = list(width = 5, height = 3)
  )
  expect_s3_class(p, "Plot")
  expect_s3_class(p$plot, "ggplot")
  expect_length(p$plot$layers, 3)
  expect_s3_class(p$plot$layers[[3]]$geom, "GeomPoint")
  expect_equal(p$captions, "Summary: mean_se")
  expect_equal(p$svg_options$width, 5)
  expect_equal(p$type, "DeclarativePlot")
  expect_named(p$data, c("t", "v", "g", "id"))
})

test_that("unknown arguments, keys and types error", {
  d <- test_long()
  expect_error(DeclarativePlot$new(d, list(x = "t", y = "v"), facetting = list()), "unused argument")
  expect_error(DeclarativePlot$new(d, list(x = "t", y = "v", zz = "g")), "Unknown mapping key")
  expect_error(DeclarativePlot$new(d, list(x = "t", y = "nope")), "not present")
  expect_error(DeclarativePlot$new(d, list(x = "t", y = "v"), layers = list(list(type = "boxplt"))), "Unknown layer type")
  expect_error(DeclarativePlot$new(d, list(x = "t", y = "v"), layers = list(list(geom = "x"))), "no .*type")
  expect_error(DeclarativePlot$new(d, list(x = "t", y = "v"), svg = list(colour = 1)), "Unknown svg option")
  expect_error(DeclarativePlot$new(d, list(x = "t", y = "v"), transform = list(x = "not_a_function")), "Cannot resolve")
  expect_error(DeclarativePlot$new(d[0, ], list(x = "t", y = "v")), "no rows")
})

test_that("transform, axis, colours and annotation are applied", {
  d <- test_long()
  d$t <- as.integer(d$t)
  p <- DeclarativePlot$new(
    d, list(x = "t", y = "v", fill = "g"),
    layers = list(list(type = "boxplot")),
    transform = list(x = "factor"),
    axis = list(units_y = "mm", ylim = c(-2, 2)),
    annotation = list(title = "Custom title", subtitle = "sub", legend_position = "bottom")
  )
  expect_true(is.factor(p$data$t))
  labs <- plot_labels(p$plot)
  expect_equal(labs$y, "v (mm)")
  expect_equal(labs$title, "Custom title")
  expect_match(labs$caption, "Zoom on y axis")
  built <- ggplot2::ggplot_build(p$plot)
  expect_equal(sort(unique(built$data[[1]]$fill)), sort(nightowl_palette("owl", 3)))
})

test_that("default title, facets from mapping, and title suppression", {
  d <- test_long()
  p <- DeclarativePlot$new(d, list(x = "t", y = "v", facet_col = "g"), layers = list(list(type = "violin")))
  expect_equal(plot_labels(p$plot)$title, "v vs. t")
  expect_s3_class(p$plot$facet, "FacetGrid")
  expect_equal(p$facets$column, "g")
  q <- DeclarativePlot$new(d, list(x = "t", y = "v"), layers = list(list(type = "points")),
                           annotation = list(title = FALSE),
                           facets = list(type = "wrap", row = "g"))
  expect_null(plot_labels(q$plot)$title)
  expect_s3_class(q$plot$facet, "FacetWrap")
})

test_that("binned layers group a numeric x", {
  d <- test_long()
  d$x <- as.numeric(d$t) + stats::runif(nrow(d))
  p <- DeclarativePlot$new(d, list(x = "x", y = "v", fill = "g"), layers = list(list(type = "boxplot", cut_args = list(n = 3))))
  built <- ggplot2::ggplot_build(p$plot)
  expect_equal(nrow(built$data[[1]]), 9)
})

test_that("layer verbs work standalone", {
  g <- ggplot2::ggplot(test_long(), ggplot2::aes(t, v, colour = g))
  out <- g |>
    layer_traces(id = "id", alpha = 0.2) |>
    layer_summary(fun.data = "mean_se", geom = "line") |>
    layer_smooth(method = "lm", formula = y ~ x, mapping = list(group = "g")) |>
    layer_geom("ggplot2::geom_rug", sides = "l")
  expect_length(out$layers, 4)
  expect_error(layer_traces(g), "id")
  expect_error(layer_summary(g, fun = mean, fun.data = ggplot2::mean_se), "not both")
  expect_error(layer_geom(g, "nopkg::geom_x"), "nopkg")
  expect_error(layer_geom(g, "geom_that_does_not_exist"), "Cannot resolve")
})

test_that("vdiffr goldens for the main styles", {
  skip_if_not_installed("vdiffr")
  d <- test_long()
  for (style in c("Boxplot", "Dotplot-SummaryMean", "Traces-SummaryMean")) {
    set.seed(42)
    p <- styled_plot(d, style, x = "t", y = "v", fill = "g", colour = "g", id = "id")
    vdiffr::expect_doppelganger(style, p$plot)
  }
})
