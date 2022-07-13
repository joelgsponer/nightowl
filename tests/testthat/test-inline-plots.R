test_that("inline plots work", {
  a <- nightowl::add_violin(runif(100), height = 4)
  a

  nightowl::make_scale(a)

  nightowl::add_inline_plot(rnorm(1000, 0, 1),
    mapping = list(x = "x", y = NULL),
    style = "Inline-Density"
  )

  nightowl::add_inline_histogram(rnorm(1000, 0, 1))

  nightowl::add_inline_pointrange(tibble::tibble(y = 0, ymin = -1, ymax = 1)) %>%
    nightowl::make_scale()



  a$css <- list(style = list(background = "red"))
  a$html(resize = FALSE)
  htmltools::browsable()
})
