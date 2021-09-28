test_that("line plot works", {
  testdata <- tibble::tibble(
    Y = runif(10),
    X = rep(seq(1, 5), 2),
    ID = c(rep("A", 5), rep("B", 5)),
    GROUP = c(rep("A", 5), rep("B", 5))
  )
  testdata
  nightowl::line_plot(testdata,
    x = "X",
    y = "Y",
    fill = "GROUP",
    color = "GROUP",
    id = "ID",
    facet_row = "GROUP"
  )
  nightowl::line_plot(testdata,
    x = "X",
    y = "Y",
    id = "ID",
    facet_row = "GROUP"
  )

  nightowl::line_plot(testdata,
    x = "X",
    y = "Y",
    fill = "GROUP",
    color = "GROUP",
    id = "ID",
    facet_col = "GROUP"
  )
  testdataA <- tibble::tibble(
    Y = runif(1000),
    X = sample(seq(1, 5), 1000, T),
    ID = sample(letters[1:10], 1000, T),
    GROUP = "A"
  )
  testdataB <- tibble::tibble(
    Y = runif(1000),
    X = sample(seq(1, 5), 1000, T),
    ID = sample(letters[11:20], 1000, T),
    GROUP = "B"
  )
  testdata <- dplyr::bind_rows(testdataA, testdataB)

  nightowl::line_plot(testdata,
    x = "X",
    y = "Y",
    fill = "GROUP",
    color = "GROUP",
    id = "ID"
  )

  nightowl::line_plot(testdata,
    x = "X",
    y = "Y",
    fill = "GROUP",
    color = "GROUP",
    id = "ID",
    color_values = NULL,
    summarise_y = median
  )

  nightowl::line_plot(testdata,
    x = "X",
    y = "Y",
    fill = "GROUP",
    color = "GROUP",
    id = "ID",
    color_values = NULL,
    summarise_y = mean
  )

  nightowl::line_plot(testdata,
    x = "X",
    y = "Y",
    fill = "GROUP",
    color = "GROUP",
    id = "ID",
    color_values = NULL,
    summarise_y = min
  )

  nightowl::line_plot(testdata,
    x = "X",
    y = "Y",
    fill = "GROUP",
    color = "GROUP",
    id = "ID",
    summarise_y = median,
    method_smooth = "median"
  )
})
