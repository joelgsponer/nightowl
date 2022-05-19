test_that("summary works", {
  testdata <- tibble::tibble(
    "foo" = sample(LETTERS[1:5], 200, T),
    "bar" = runif(200),
    "baz" = runif(200),
    "qux" = sample(c("Apple", "Pears", "Banana"), 200, T)
  )

  nightowl::summary(testdata, "bar", "foo", output = "kable")

  nightowl::summary(testdata, "bar", "foo", output = "kable")
  nightowl::summary(testdata, "qux", "foo", output = "kable")

  nightowl::forestplot(1, 0, 2)

  testdata %>%
    dplyr::group_by(foo) %>%
    attributes()

  nightowl::calc_summary_numeric(testdata, "bar")
  nightowl::calc_summary_numeric(dplyr::group_by(testdata, foo), "bar")

  nightowl::calc_summary_numeric(dplyr::group_by(mtcars, cyl), "mpg")

  nightowl::calc_summary(dplyr::group_by(testdata, foo), "bar")

  nightowl::calc_summary(dplyr::group_by(testdata, foo), "bar", calculations = list(Min = min))

  testdata %>%
    dplyr::summarise(mean = ggplot2::mean_cl_boot(bar)) %>%
    tidyr::unnest(mean) %>%
    nightowl::add_forestplot("y", "ymin", "ymax") %>%
    nightowl::render_kable()

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

  nightowl::calc_summary_categorical(testdata, "qux")
})
