test_that("options have defaults after load", {
  expect_equal(nightowl_option("palette"), "owl")
  expect_type(nightowl_option("svg"), "list")
  expect_named(nightowl_options(), c("palette", "missing_colour", "header_width", "font_family",
                                     "download_button", "web_fonts", "svg"))
})

test_that("options can be set and restored", {
  old <- nightowl_options(palette = "dusk", header_width = 5)
  expect_equal(nightowl_option("palette"), "dusk")
  expect_equal(nightowl_options("palette", "header_width"), list(palette = "dusk", header_width = 5))
  options(old)
  expect_equal(nightowl_option("palette"), "owl")
})

test_that("unknown options error", {
  expect_error(nightowl_option("nope"), "Unknown nightowl option")
  expect_error(nightowl_options(nope = 1), "Unknown nightowl option")
})
