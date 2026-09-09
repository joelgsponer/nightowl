test_that("NightowlPlots is a well-behaved vctrs vector", {
  x <- new_NightowlPlots(test_plot(), test_plot(1, 1))
  expect_true(is_NightowlPlots(x))
  expect_length(x, 2)
  expect_length(c(x, x), 4)
  expect_s3_class(x[1], "NightowlPlots")
  expect_true(is_Plot(x[[1]]))
  expect_equal(vctrs::vec_ptype_abbr(x), "NghtwlPl")
  expect_equal(format(x), c("<Plot>", "<Plot>"))
  expect_equal(new_NightowlPlots(list(test_plot())), new_NightowlPlots(test_plot()))
  expect_identical(new_NightowlPlots(x), x)
  expect_error(new_NightowlPlots(1), "Plot")
})

test_that("NightowlPlots works as a tibble column", {
  x <- new_NightowlPlots(test_plot(), test_plot(1, 1))
  tb <- tibble::tibble(g = c("a", "b"), p = x)
  expect_snapshot(print(tb))
  out <- tb |>
    dplyr::mutate(n = 1:2) |>
    dplyr::filter(g == "b")
  expect_true(is_NightowlPlots(out$p))
  expect_length(out$p, 1)
  bound <- dplyr::bind_rows(tb, tb)
  expect_length(bound$p, 4)
  expect_true(is_NightowlPlots(bound$p))
})

test_that("conversions render every element", {
  x <- new_NightowlPlots(test_plot(), test_plot(1, 1))
  chr <- as.character(x)
  expect_length(chr, 2)
  expect_true(all(grepl("<svg", chr)))
  expect_length(as_ggplot(x), 2)
  expect_s3_class(as_ggplot(x)[[1]], "ggplot")
  expect_s3_class(as_html(x)[[1]], "shiny.tag")
  expect_equal(plots_width(x), 216)
  expect_output(print(x), "NightowlPlots\\[2\\]")
})
