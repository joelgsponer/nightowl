test_that("multiplication works", {
  require(magrittr)
  testdata <- ChickWeight %>%
    dplyr::filter(Time < 10)

  nightowl::plot(testdata,
    transform = list(x = "fct_natural"),
    mapping = list(
      x = "Time",
      y = "weight",
      color = "Diet",
      group = "Diet",
      id = "Chick",
      lty = "Diet"
    ),
    layers = list(
      list(type = "traces", geom = "line", alpha = 0.3),
      list(type = "summary", mapping = list(color = NULL), geom = "line", dodge = 0, size = 1.5)
    )
  )

  nightowl::plot(testdata,
    mapping = list(
      x = "Time",
      y = "weight",
      color = "Diet",
      fill = "Diet",
      group = "Diet"
    ),
    transform = list(x = "factor"),
    layers = list(
      mapping = list(group = NULL),
      list(type = "boxplot")
    )
  )

  nightowl::plot(ChickWeight,
    transform = list(x = fct_natural),
    mapping = list(
      x = "Time",
      y = "Chick",
      fill = "weight",
      facet_row = "Diet"
    ),
    layers = list(
      generic = list(geom = "ggplot2::geom_tile", color = "black")
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
