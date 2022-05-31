test_that("multiplication works", {
  nightowl::forestplot(0, -1, 1) %>%
    htmltools::browsable()

  nightowl::forestplot(0, -1, 1, height = 0.35, theme = ggplot2::theme_bw) %>%
    htmltools::browsable()

  nightowl::forestplot(0, -1, 1, height = 3, theme = ggplot2::theme_bw) %>%
    htmltools::browsable()
})
