test_that("summary works", {
  testdata <- tibble::tibble(
    "foo" = c(rep("A", 50), rep("B", 50), rep("C", 50), rep("D", 50)),
    "bar" = c(rnorm(50, 0, 1), rnorm(50, 1, 2), rnorm(50, 2, 3), rnorm(50, 3, 4)),
    "baz" = runif(200),
    "qux" = sample(c("Apple", "Pears", "Banana", "This  is something very long ...."), 200, T),
    "s1" = sample(c("Morning", "Midday", "Evening"), 200, T)
  ) %>%
    dplyr::mutate(qux = dplyr::case_when(
      foo == "A" & qux == "Apple" ~ "Pears",
      foo == "B" & qux == "Banana" ~ NA_character_,
      TRUE ~ qux
    )) %>%
    dplyr::mutate(bar = dplyr::case_when(
      foo == "A" & bar < 0 ~ NA_real_,
      TRUE ~ bar
    )) %>%
    dplyr::mutate(qux = factor(qux))
  testdata

  nightowl::Summary$new(testdata, "qux", "s1", debug = F)
  nightowl::Summary$new(testdata, "qux", "s1")$raw()
  nightowl::Summary$new(testdata, "qux", "s1")$reactable()

  nightowl::Summary$new(testdata, "qux", "foo", method = nightowl::summarise_categorical_barplot)$data

  nightowl::Summary$new(testdata, "qux", "foo", method = nightowl::summarise_categorical_barplot)$kable()
  nightowl::Summary$new(testdata %>% dplyr::filter(foo == "A"), "qux", "foo", method = nightowl::summarise_categorical_barplot)$kable()

  NightowlOptions$set_colors(picasso::roche_colors() %>% rev())
  NightowlOptions$set_header_width(10)
  nightowl::Summary$new(testdata, "qux", "foo", method = nightowl::summarise_categorical_barplot)$kable()

  nightowl::Summary$new(testdata, "baz", "s1", method = nightowl::summarise_numeric_violin)$raw()
  nightowl::Summary$new(testdata, "baz", "s1", method = nightowl::summarise_numeric_violin)$kable()
  nightowl::Summary$new(testdata, "baz", "s1", method = nightowl::summarise_numeric_histogram)$kable()
  nightowl::Summary$new(testdata, "baz", "s1", method = nightowl::summarise_numeric_violin)$reactable()
  nightowl::Summary$new(testdata, "baz", "s1", method = nightowl::summarise_numeric_pointrange)$reactable()

  a <- nightowl::Summary$new(testdata, "baz", "s1", method = nightowl::summarise_numeric_pointrange)$raw()

  purrr::walk(a$Pointrange, function(.x) {
    .x$options_svg[["width"]] <- 3
    .x$plot <- .x$plot + ggplot2::xlim(c(0.25, 0.75))
  })

  purrr::walk2(c("red", "blue", "green"), a$Pointrange, function(.x, .y) {
    .y$plot <- .y$plot + ggplot2::scale_color_manual(values = .x)
  })

  nightowl::render_kable(a)

  nightowl::Summary$new(testdata,
    "baz",
    "s1",
    method = nightowl::summarise_numeric_violin
  )$reactable()

  # Pointrange
  palmerpenguins::penguins %>%
    dplyr::group_by(species) %>%
    dplyr::summarise(mean = ggplot2::mean_cl_boot(bill_length_mm), ) %>%
    render_kable()

  # Testing memoisation
  flights <- nightowl::Summary$new(nycflights13::flights, "month", "day")
  flights$raw()


  # Data summary
  nightowl::data_summary(testdata, "bar", "foo", output = "kable", labels = c(bar = "Bar", foo = "Foo"), keep_y = TRUE)
  nightowl::data_summary(testdata, "bar", "foo", output = "reactable", labels = c(bar = "Bar", foo = "Foo"))
  nightowl::data_summary(testdata, "qux", "foo", output = "kable")
  nightowl::data_summary(testdata, "qux", "foo", output = "reactable")
  nightowl::data_summary_with_plot(testdata, "bar", "foo", output = "kable", labels = c(bar = "Bar", foo = "Foo"))
  nightowl::data_summary_with_plot(testdata, "bar", "foo", output = "reactable", labels = c(bar = "Bar", foo = "Foo"))
  nightowl::data_summary_with_plot(testdata, "qux", "foo", output = "kable")

  # Summarise categorical
  nightowl::summarise(testdata, "qux")
  nightowl::summarise_categorical(testdata, "qux") %>% nightowl::render_kable()
  nightowl::summarise_categorical_barplot(testdata, "qux") %>% nightowl::render_kable()

  # Summarise numeric
  nightowl::summarise_numeric(testdata, "bar") %>% nightowl::render_kable()
  nightowl::summarise_numeric_forestplot(testdata, "bar") %>% nightowl::render_kable()

  # Adding scales
  a <- nightowl::summarise_numeric_forestplot(testdata, "bar")
  a %>%
    nightowl::add_scale(height = 0.8, scaling = 3) %>%
    nightowl::render_kable()

  nightowl::summarise_categorical_barplot(testdata, "qux") %>%
    nightowl::add_scale() %>%
    nightowl::render_kable()

  # Groupings
  testdata %>%
    dplyr::group_by(foo) %>%
    purrr::map_df(c("bar", "baz"), function(col, .data) {
      nightowl::summarise_numeric_violin(.data, col)
    }, .data = .) %>%
    nightowl::render_kable()

  testdata %>%
    dplyr::group_by("foo") %>%
    nightowl::summarise_numeric_violin("bar") %>%
    nightowl::render_kable()

  # Some reactables
  nightowl::summary(testdata, "qux", c("foo", "s1"), output = "kable")
  nightowl::render_reactable()

  nightowl::summary(testdata, "bar", c("foo", "s1"), output = "raw", calc_p = F) %>%
    nightowl::render_reactable() %>%
    as.character()

  nightowl::reactable_summary(testdata,
    "s1",
    c("bar", "qux"),
    "foo",
    labels = c(bar = "Bar", foo = "Foo", qux = "This variable"),
    plan = "sequential"
  )

  nightowl::mean(runif(10))

  nightowl::reactable_summary(testdata,
    split = NULL,
    c("bar", "qux"),
    "foo",
    labels = c(bar = "Bar", foo = "Foo", qux = "This variable")
  )

  nightowl::summary(testdata, "bar", "foo", output = "kable")
  nightowl::summary(testdata, "baz", "foo", output = "kable")
  nightowl::summary(testdata, "qux", "foo", output = "kable")

  nightowl::forestplot(1, 0, 2)

  testdata %>%
    dplyr::group_by(foo) %>%
    attributes()

  nightowl::calc_summary_numeric(testdata, "bar")
  nightowl::calc_summary_numeric(testdata, "bar") %>% nightowl::render_kable()
  nightowl::calc_summary_numeric(dplyr::group_by(testdata, foo), "bar") %>% nightowl::render_kable()
  nightowl::calc_summary_numeric(dplyr::group_by(testdata, foo), "bar") %>% nightowl::render_reactable()

  nightowl::summarise(
    data = dplyr::group_by(testdata, foo),
    column = "bar",
    calculations = list(
      `N.` = length,
      Median = function(x) median(x, na.rm = T),
      Mean = nightowl::formated_mean,
      Violin = nightowl::add_violin
    ),
    parameters = rlang::expr(list(
      Violin = list(
        theme = picasso::theme_void,
        height = 1.5,
        ylim = range(data[[column]], na.rm = T)
      )
    ))
  ) %>% nightowl::render_kable()

  nightowl::summarise(
    data = dplyr::group_by(testdata, foo),
    column = "bar",
    calculations = list(
      `N.` = length,
      Median = function(x) median(x, na.rm = T),
      Mean = nightowl::formated_mean,
      Violin = function(x) nightowl::styled_plot(x, )
    ),
    parameters = rlang::expr(list(
      Violin = list(
        theme = picasso::theme_void,
        height = 1.5,
        ylim = range(data[[column]], na.rm = T)
      )
    ))
  ) %>% nightowl::render_kable()

  nightowl::calc_summary_numeric(
    data = dplyr::group_by(testdata, foo),
    column = "bar",
    calculations = list(
      `N.` = length,
      Median = function(x) median(x, na.rm = T),
      Mean = nightowl::formated_mean,
      Density = nightowl::add_density
    ),
    parameters = rlang::expr(list(
      Density = list(
        theme = ggplot2::theme_void,
        height = 1.5,
        ylim = range(data[[column]], na.rm = T)
      )
    ))
  ) %>% nightowl::render_kable()

  nightowl::calc_summary_numeric(dplyr::group_by(mtcars, cyl), "mpg") %>%
    nightowl::add_scale("Forestplot")

  nightowl::calc_summary(dplyr::group_by(testdata, foo), "bar")

  nightowl::calc_summary(dplyr::group_by(testdata, foo), "bar", calculations = list(Min = min))

  nightowl::calc_summary_numeric(dplyr::group_by(mtcars, cyl), "mpg") %>%
    nightowl::add_scale()



  nightowl::frequencies(testdata$qux)
  nightowl::frequencies(testdata$qux, "count")
  nightowl::frequencies(testdata$qux, "percent")
  nightowl::frequencies(testdata$qux, "print")

  nightowl::frequencies(testdata$qux, "barplot") %>%
    nightowl::render_kable()

  nightowl::calc_summary(dplyr::group_by(testdata, foo), "qux", calculations = list(N = nightowl::n, Freq = nightowl::frequencies))

  nightowl::calc_summary(dplyr::group_by(
    testdata,
    foo
  ),
  column = "qux",
  calculations = list(
    N = nightowl::n,
    Freq = nightowl::frequencies,
    Bar = function(x) {
      nightowl::frequencies(x,
        output = "barplot"
      )
    }
  ),
  names_sep = NULL
  ) %>%
    nightowl::render_kable()

  nightowl::frequencies(sample(letters, 100, T))


  nightowl::calc_summary(testdata, "qux")
  nightowl::calc_summary(testdata %>% dplyr::group_by(s1), "qux")
  nightowl::calc_summary_categorical(testdata, "qux")
  nightowl::calc_summary_categorical(testdata, "qux") %>% nightowl::render_kable()
  nightowl::calc_summary_categorical(dplyr::group_by(testdata, foo), "qux") %>% nightowl::render_reactable()

  nightowl::calc_summary(testdata,
    "bar",
    calculations = list(Test = function(x, param) {
      param
    }),
    parameters = list(Test = list(param = "test"))
  )


  s <- nightowl::Summary$new(testdata %>% dplyr::group_by(s1), "qux")
  s
  s$raw()
  s$keep_y <- FALSE
  s$raw()
  s$reactable()

  s$options_test$correct <- TRUE
  s$calc_test()$test$footnote

  s <- nightowl::Summary$new(testdata, "bar", "foo", labels = c(bar = "Bar", foo = "Foo"), keep_y = TRUE)
  s
  s$kable()
  s$html()
  s$reactable()

  s <- nightowl::Summary$new(testdata, "bar", "foo", method = nightowl::summarise_numeric_forestplot, labels = c(bar = "Bar", foo = "Foo"), keep_y = TRUE)


  s$reactable(fullWidth = TRUE)

  s$kable()

  s$hash
  s$is_dirty()
  s$data <- mtcars
  s$is_dirty()
})
