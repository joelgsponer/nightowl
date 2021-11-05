test_that("ggally works", {
  testdata <- mtcars[, c(1, 3, 4, 5, 6, 7)]
  GGally::ggcorr(testdata)
  GGally::ggpairs(testdata)

  GGally::ggcorr(testdata, label = T, method = c("pairwise", "spearman"), size = 6)

  lowerFn <- function(data, mapping, method = "lm", ...) {
    ggplot(data = data, mapping = mapping) +
      ggplot2::geom_point() +
      ggplot2::geom_smooth(method = method, color = picasso::roche_color("blue"), ...) +
      picasso::theme_picasso()
  }
  upperFn <- function(data, mapping, method = "lm", ...) {
    res <- dplyr::select(data, !!!mapping) %>% cor()
    res <- res[1, 2]
    if (res == 0) col <- picasso::roche_color("black", alpha = 0.1)
    if (res < 0) col <- picasso::roche_color("red", alpha = 1)
    if (res > 0) col <- picasso::roche_color("green", alpha = 1)
    p <- ggplot(data = data, mapping = mapping) +
      ggplot2::geom_smooth(
        method = method,
        color = "black",
        size = 0.1,
        fill = col,
        alpha = abs(res) * 0.8,
        ...
      ) +
      ggpmisc::stat_correlation(color = picasso::roche_color("black")) +
      ggplot2::theme_void()
    p
  }
  diagFn <- function(data, mapping, method = "lm", ...) {
    p <- ggplot(data = data, mapping = mapping) +
      ggplot2::geom_density(fill = picasso::roche_color("blue"), alpha = 0.4) +
      picasso::theme_picasso()
    p
  }
  GGally::ggpairs(
    data = testdata,
    lower = list(continuous = GGally::wrap(lowerFn, method = "lm")),
    diag = list(continuous = GGally::wrap(diagFn)),
    upper = list(continuous = GGally::wrap(upperFn))
  ) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 20, face = "bold", hjust = 0, vjust = 2),
      plot.subtitle = ggplot2::element_text(size = 14, hjust = 0, vjust = 2),
      panel.background = ggplot2::element_rect(fill = "white"),
      plot.background = ggplot2::element_rect(fill = "white"),
      panel.border = ggplot2::element_rect(color = "#231F20FF", fill = NA),
      strip.background = ggplot2::element_rect(fill = "white"),
      legend.position = "bottom",
      legend.background = ggplot2::element_rect(fill = "white", colour = "white"),
      legend.key = ggplot2::element_rect(fill = "white"),
      axis.text = ggplot2::element_text(colour = "#231F20FF"),
      strip.text.x = ggplot2::element_text(color = "#231F20FF"),
      strip.text.y = ggplot2::element_text(color = "#231F20FF"),
    )
})
