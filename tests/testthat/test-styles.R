test_that("loading styles works", {
  nightowl::load_style("Boxplot")

  nightowl::styled_plot(mtcars, "Boxplot", x = "mpg", y = "wt")

  nightowl::styled_plot(mtcars, "Boxplot", x = "mpg", y = "wt", fill = "mpg")

  nightowl::styled_plot(mtcars, "Boxplot-SummaryMean", x = "cyl", y = "wt", fill = "cyl")


  nightowl::styled_plot(ChickWeight,
    "Boxplot-SummaryMean",
    x = "Time",
    y = "weight",
    group = "Diet",
    color = "Diet",
    fill = "Diet"
  ) %>%
    nightowl::as_html() %>%
    shiny::div(style = "width: 50%; height: 50%;") %>%
    htmltools::browsable()


  ggplot(mpg, ggplot2::aes(cyl, hwy)) +
    geom_point() +
    stat_chull(fill = NA, colour = "black")
})
