test_that("multiplication works", {
  a <- nightowl::forestplot(0, -1, 1)
  print(a)
  print(a, .browser = F)
  html(a) %>% class()
  ggplot(a)
  as.character(a)



  data.frame(a)
  tibble::tibble(Plot = a)
  vctrs::vec_ptype_abbr

  format(a)
  str(a)

  nightowl::forestplot(0, -1, 1, height = 0.35, theme = ggplot2::theme_bw) %>%
    htmltools::browsable()

  nightowl::forestplot(0, -1, 1, height = 3, theme = ggplot2::theme_bw) %>%
    htmltools::browsable()

  .p <- tibble::tibble(a = c(3, 1, 2, 4), b = c(3, 1, 2, 4), c = c("A", "B", "C", "D")) %>%
    ggplot2::ggplot(mapping = ggplot2::aes(x = a, y = b, color = c, data_id = c, tooltip = c, class = c)) +
    ggiraph::geom_point_interactive(size = 10, extra_interactive_params = "test") +
    ggplot2::theme_void()

  nightowl::render_svg(.p)
  nightowl::render_svg(.p) %>% print(browse = F)

  a <- nightowl::ggplot_to_girafe(.p)

  tmp <- tempfile()
  ggiraph::dsvg(tmp, height = 8, width = 8, pointsize = 45)
  .p
  dev.off()
  shiny::div(
    htmltools::HTML(ggiraph:::read_file(tmp))
  ) %>%
    htmltools::browsable()
})
