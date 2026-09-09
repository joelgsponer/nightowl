test_that("Plot validates input and merges svg options", {
  expect_error(Plot$new(1), "ggplot")
  expect_error(Plot$new(test_gg(), svg = 1), "list")
  p <- Plot$new(test_gg(), svg = list(width = 5))
  expect_equal(p$svg_options$width, 5)
  expect_equal(p$svg_options$height, nightowl_option("svg")$height)
  expect_true(is_Plot(p))
  expect_false(is_Plot(test_gg()))
})

test_that("Plot sizes come from the svg options", {
  p <- test_plot(width = 4, height = 2)
  expect_equal(p$width(), 288)
  expect_equal(p$height(), 144)
  expect_equal(svg_viewbox(p$svg())[3:4], c(288, 144))
})

test_that("Plot renders once per option set", {
  calls <- 0
  local_mocked_bindings(render_svg = function(plot, ...) {
    calls <<- calls + 1
    htmltools::HTML("<svg viewBox='0 0 1 1'></svg>")
  })
  p <- test_plot()
  p$svg()
  p$svg()
  p$html()
  expect_equal(calls, 1)
  p$svg(width = 9)
  expect_equal(calls, 2)
})

test_that("Plot html carries the dependency and honours resize", {
  p <- test_plot(width = 4, height = 2)
  h <- p$html()
  expect_s3_class(h, "shiny.tag")
  expect_equal(htmltools::htmlDependencies(h)[[1]]$name, "nightowl")
  expect_match(h$attribs$class, "nightowl-plot")
  fixed <- p$html(resize = FALSE)
  expect_match(fixed$attribs$style, "width:288px;height:144px;")
  expect_match(p$as.character(), "^<button|^<svg")
  expect_equal(p$format(), "<Plot>")
})
