test_that("svg works", {
  testdata <- palmerpenguins::penguins_raw
  nightowl::boxplot(testdata,
    x = "Species",
    y = "Culmen Depth (mm)",
    add_violin = T,
    add_boxplot = F,
    points_size = 3,
    facet_col = "Island"
  ) %>%
    nightowl::render_svg(height = 50, width = 40, scaling = 3) %>%
    htmltools::browsable()
})
