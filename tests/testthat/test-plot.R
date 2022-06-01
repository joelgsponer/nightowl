test_that("multiplication works", {
  require(magrittr)
  testdata <- ChickWeight %>%
    dplyr::filter(Time < 10)


  g <- ggplot2::ggplot(testdata, ggplot2::aes(x = Time, y = weight)) +
    ggplot2::geom_point()



  nightowl::plot(testdata,
    transform = list(x = "waRRior::fct_lexicographic"),
    mapping = list(
      x = "Time",
      y = "weight",
      color = "Diet",
      group = "Diet",
      id = "Chick",
      lty = "Diet"
    ),
    layers = list(
      list(type = "generic", geom = "ggplot2::geom_jitter", alpha = 0.3, width = 0.1),
      list(type = "traces", geom = "line", alpha = 0.3),
      list(type = "summary", mapping = list(color = NULL), geom = "line", dodge = 0, size = 1.5)
    ),
    annotation = list(test = "lm"),
    svg = list()
  )

  .p <- nightowl::plot(testdata,
    transform = list(x = "waRRior::fct_lexicographic"),
    mapping = list(
      x = "Time",
      y = "Chick",
      fill = "weight",
      facet_row = "Diet"
    ),
    layers = list(
      list(type = "generic", geom = "ggplot2::geom_tile", color = "black")
    )
  )
  .p
  plotly::ggplotly(.p)

  nightowl::plot(testdata,
    transform = list(x = "waRRior::fct_lexicographic"),
    mapping = list(
      x = "Diet",
      y = "weight",
      group = "Diet",
      fill = "Diet",
      facet_rows = "Time"
    ),
    layers = list(
      list(type = "boxplot"),
      list(
        type = "generic",
        geom = "ggpubr::stat_compare_means",
        comparisons = list(c("1", "2"))
      ),
      list(
        type = "generic",
        geom = "ggpubr::stat_compare_means",
        method = "anova"
      )
    )
  )


  big <- data.frame(
    time = c(runif(1000, 0, 10)),
    group = sample(c("A", "B"), 1000, T)
  ) %>%
    dplyr::mutate(value = runif(1000, 0, time))

  nightowl::plot(big,
    transform = list(
      color = nightowl::fct_natural
    ),
    mapping = list(
      x = "time",
      y = "value",
      color = "group",
      fill = "group",
      group = "group"
    ),
    layers = list(
      points = list(),
      smooth = list(),
      summary = list(
        mapping = list(color = NULL),
        binwidth = 0,
        geom = "pointrange"
      )
    )
  )

  nightowl::plot(testdata,
    transform = list(),
    mapping = list(
      x = "Time",
      y = "weight",
      fill = "Diet"
    ),
    layers = list(
      violin = list(
        cut_f = ggplot2::cut_width,
        cut_args = list(
          width = 1
        )
      ),
      summary = list(
        mapping = list(color = NULL),
        binwidth = 0.9,
        geom = "pointrange"
      )
    )
  )


  nightowl::plot(testdata,
    mapping = list(
      x = "Time",
      y = "weight",
      color = "Diet",
      fill = "Diet",
      id = "Chick"
    ),
    layers = list(
      traces = list(mapping = list(group = "Chick"), geom = "point", dodge = 0),
      traces = list(mapping = list(group = NULL), geom = "line", dodge = 0)
    ),
    annotation = list(title = "TEST"),
    axis = list(log_y = TRUE, xlim = c(2, 4), units_x = "days", units_y = "g"),
    svg = list(),
    dodge = 0.5
  ) %>%
    htmltools::browsable()

  nightowl::plot(mtcars,
    transform = list(
      x = factor
    ),
    mapping = list(
      x = "mpg",
      y = "disp",
      fill = "cyl",
      facet_row = c("gear")
    ),
    layers = list(
      points = list(),
      boxplot = list()
    ),
    annotation = list(title = "TEST"),
    axis = list(log_y = TRUE, units_x = "days", units_y = "g"),
    facet = list(),
    svg = list(),
    dodge = 0.5
  ) %>%
    htmltools::browsable()

  nightowl::plot(testdata,
    mapping = list(
      x = "Time",
      y = "weight",
      id = "Chick",
      color = "Diet"
    ),
    layers = list(
      points = list(),
      traces = list()
    ),
    dodge = 0.5
  )
})
