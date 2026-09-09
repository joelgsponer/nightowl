test_that("every shipped style loads, validates and renders", {
  d <- test_long()
  for (style in list_styles()) {
    spec <- load_style(style)
    expect_type(spec, "list")
    expect_true(is.character(spec$name) && nzchar(spec$name), info = style)
    expect_true(is.character(spec$description) && nzchar(spec$description), info = style)
    if (startsWith(style, "Inline")) {
      if (style == "Inline-Halfeye") skip_if_not_installed("ggdist")
      out <- add_inline_plot(d$v, style = style)
      expect_true(is_NightowlPlots(out), info = style)
    } else {
      if (style == "Raincloud") skip_if_not_installed("ggdist")
      p <- styled_plot(d, style, x = "t", y = "v", fill = "g", colour = "g", id = "id")
      expect_s3_class(p, "DeclarativePlot")
      expect_match(as.character(p$svg(download_button = FALSE)), "^<svg", info = style)
    }
  }
})

test_that("names and descriptions are distinct", {
  specs <- lapply(list_styles(), load_style)
  expect_false(any(duplicated(vapply(specs, `[[`, "", "name"))))
  expect_false(any(duplicated(vapply(specs, `[[`, "", "description"))))
})

test_that("load_style accepts a path and rejects unknown names", {
  path <- withr::local_tempfile(fileext = ".yaml")
  writeLines(c("name: T", "description: t", "layers:", "- type: points"), path)
  expect_equal(load_style(path)$layers[[1]]$type, "points")
  expect_error(load_style("Nope"), "not found")
  expect_error(load_style(1), "single string")
})

test_that("validate_spec rejects malformed specifications", {
  expect_error(validate_spec(list(bad_key = 1)), "Unknown style field")
  expect_error(validate_spec(list(layers = list(list(geom = "x")))), "no .*type")
  expect_error(validate_spec(list(layers = list(list(type = "points", mapping = list(zz = "a"))))), "Unknown layer 1 mapping key")
  expect_error(validate_spec(list(scales = list(list(x = 1)))), "no .*scale")
  expect_error(validate_spec(list(svg = list(colour = 1))), "Unknown svg option")
  expect_error(validate_spec(list(transform = list(zz = "factor"))), "Unknown transform key")
  expect_error(validate_spec(list(layers = list(list(type = "points", mapping = list(x = 1))))), "column names")
  expect_error(validate_spec("x"), "list")
  expect_invisible(validate_spec(load_style("Boxplot")))
})

test_that("styled_plot applies overrides and the YAML 'y' key survives", {
  d <- test_long()
  p <- styled_plot(d, "Boxplot", x = "t", y = "v", override = list(svg = list(width = 3), annotation = list(title = "T")))
  expect_equal(p$svg_options$width, 3)
  expect_equal(plot_labels(p$plot)$title, "T")
  h <- load_style("Histogram-simple")
  expect_true("y" %in% names(h$layers[[1]]$mapping))
  expect_null(h$layers[[1]]$mapping$y)
})

test_that("resolve_function handles functions, namespaced and bare names", {
  expect_identical(resolve_function(mean), mean)
  expect_identical(resolve_function("ggplot2::geom_point"), ggplot2::geom_point)
  expect_identical(resolve_function("geom_point"), ggplot2::geom_point)
  expect_identical(resolve_function("factor"), factor)
  expect_error(resolve_function("ggplot2::not_a_fn"), "not found")
  expect_error(resolve_function(1), "function")
})
