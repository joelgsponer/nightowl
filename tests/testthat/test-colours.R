test_that("palettes are valid colours of documented length", {
  pals <- nightowl_palettes()
  expect_named(pals, c("owl", "dusk", "muted"))
  expect_equal(lengths(pals), c(owl = 7, dusk = 6, muted = 9))
  for (p in pals) expect_true(all(grepl("^#[0-9A-F]{6}$", p)))
})

test_that("nightowl_palette returns n colours and reserves the missing colour", {
  expect_length(nightowl_palette("owl", 3), 3)
  withr::local_options(nightowl.missing_colour = "#ABCDEF")
  m <- nightowl_palette("owl", 4, missing = TRUE)
  expect_length(m, 4)
  expect_equal(m[4], "#ABCDEF")
  expect_equal(nightowl_palette("owl", 1, missing = TRUE), "#ABCDEF")
  expect_message(long <- nightowl_palette("dusk", 10), "interpolating")
  expect_length(long, 10)
})

test_that("bad palette arguments error", {
  expect_error(nightowl_palette("nope"), "Unknown palette")
  expect_error(nightowl_palette("owl", 0), "positive integer")
  expect_error(nightowl_colours("nope"), "Unknown colour role")
})

test_that("level_colours binds (Missing) by name", {
  cols <- level_colours(c("x", "(Missing)", "y"))
  expect_named(cols, c("x", "(Missing)", "y"))
  expect_equal(unname(cols["(Missing)"]), nightowl_missing_colour())
})

test_that("scales attach to a plot and colour missing values grey", {
  d <- data.frame(x = 1:4, y = 1:4, g = factor(c("a", "b", NA, "a")))
  gg <- ggplot2::ggplot(d, ggplot2::aes(x, y, colour = g, fill = g)) +
    ggplot2::geom_point(shape = 21) +
    scale_colour_nightowl() +
    scale_fill_nightowl("muted")
  built <- ggplot2::ggplot_build(gg)
  cols <- built$data[[1]]$colour
  expect_equal(cols[3], nightowl_missing_colour())
  expect_equal(sort(unique(cols[-3])), sort(nightowl_palette("owl", 2)))
})

test_that("is_dark is vectorised", {
  expect_equal(is_dark(c("#000000", "#FFFFFF", "#0072B2")), c(TRUE, FALSE, TRUE))
})
