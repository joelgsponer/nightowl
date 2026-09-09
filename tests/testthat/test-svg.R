test_that("render_svg returns well-formed SVG with the requested viewBox", {
  skip_if_not_installed("xml2")
  svg <- render_svg(test_gg(), width = 4, height = 3, download_button = FALSE)
  expect_s3_class(svg, "html")
  doc <- xml2::read_xml(as.character(svg))
  expect_equal(xml2::xml_name(doc), "svg")
  expect_equal(xml2::xml_attr(doc, "width"), "100%")
  expect_equal(xml2::xml_attr(doc, "height"), "100%")
  expect_equal(svg_viewbox(svg), c(0, 0, 288, 216))
})

test_that("render_svg writes the font stack and the download button", {
  svg <- as.character(render_svg(test_gg(), width = 2, height = 2, font_family = "Foo, 'Bar Baz', sans-serif", download_button = TRUE))
  expect_match(svg, "font-family: Foo, \"Bar Baz\", sans-serif;", fixed = TRUE)
  expect_match(svg, "^<button class=\"nightowl-download\"")
  plain <- as.character(render_svg(test_gg(), width = 2, height = 2, download_button = FALSE))
  expect_match(plain, "^<svg")
})

test_that("render_svg is deterministic and closes its device", {
  n <- length(grDevices::dev.list())
  a <- as.character(render_svg(test_gg(), width = 2, height = 2, download_button = FALSE))
  b <- as.character(render_svg(test_gg(), width = 2, height = 2, download_button = FALSE))
  expect_identical(a, b)
  broken <- ggplot2::ggplot(mtcars, ggplot2::aes(.data$nope, mpg)) + ggplot2::geom_point()
  expect_error(render_svg(broken, width = 2, height = 2))
  expect_error(render_svg(1), "ggplot")
  expect_equal(length(grDevices::dev.list()), n)
})

test_that("svg_normalise strips volatile parts and is idempotent", {
  svg <- render_svg(test_gg(), width = 2, height = 2, download_button = TRUE)
  norm <- svg_normalise(svg)
  expect_length(norm, 1)
  expect_false(grepl("nightowl-download", norm))
  expect_false(grepl("textLength", norm))
  expect_identical(svg_normalise(norm), norm)
  expect_equal(svg_normalise(test_plot()), svg_normalise(test_gg(), width = 3, height = 2))
  expect_error(svg_normalise(1), "Cannot extract SVG")
})

test_that("expect_snapshot_svg stores an svg snapshot", {
  skip_if(!identical(Sys.getenv("NIGHTOWL_SVG_SNAPSHOTS"), "true") && !identical(Sys.info()[["sysname"]], "Linux"),
          "SVG snapshots run on Linux or with NIGHTOWL_SVG_SNAPSHOTS=true")
  expect_snapshot_svg(test_plot(), "scatter")
})
