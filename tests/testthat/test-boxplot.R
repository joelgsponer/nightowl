test_that("boxplot works", {
  testdata <- palmerpenguins::penguins_raw
  nightowl::boxplot(testdata,
    x = "Species",
    y = "Culmen Depth (mm)",
    add_violin = T,
    add_points = F,
    add_boxplot = F,
    points_size = 0.5,
    points_alpha = 1,
    points_color = "black",
    facet_col = "Island",
    add_lines = T,
    add_smooth = "loess",
    unused = "unused"
  ) %>%
    waRRior::expect_ggplot()
  nightowl::boxplot(testdata,
    x = "Sex",
    y = "Culmen Depth (mm)",
    add_violin = T,
    add_boxplot = F,
    points_size = 3,
    facet_col = c("Sex", "Region"),
    remove_missing = F,
    plot_height = T # This is an additonal parameter that is not needed
  ) %>%
    waRRior::expect_ggplot()
  nightowl::boxplot(testdata,
    x = "Sex",
    y = "Culmen Depth (mm)",
    add_violin = T,
    add_boxplot = F,
    points_size = 3,
    facet_row = c("Sex", "Region"),
    remove_missing = F,
    plot_height = T # This is an additonal parameter that is not needed
  ) %>%
    waRRior::expect_ggplot()
  nightowl::boxplot(testdata,
    x = "Sex",
    y = "Culmen Depth (mm)",
    add_violin = T,
    add_boxplot = F,
    points_size = 3,
    facet_col = c("Sex", "Region"),
    remove_missing = F,
    plot_height = T # This is an additonal parameter that is not needed
  ) %>%
    waRRior::expect_ggplot()
  nightowl::boxplot(testdata,
    x = "Species",
    y = "Culmen Depth (mm)",
    add_violin = T,
    add_boxplot = F,
    points_size = 3,
    facet_row = c("Sex", "Island"),
    remove_missing = F
  ) %>%
    waRRior::expect_ggplot()
  nightowl::boxplot(testdata,
    x = "Region",
    y = "Culmen Depth (mm)",
  ) %>%
    waRRior::expect_ggplot()
  nightowl::boxplot(testdata,
    x = "Island",
    y = "Culmen Depth (mm)",
    theme = picasso::theme_dark
  ) %>%
    waRRior::expect_ggplot()
  nightowl::boxplot(testdata,
    x = "Island",
    y = "Culmen Depth (mm)",
    add_violin = F,
    add_points = F,
    facet_col = "Region"
  ) %>%
    waRRior::expect_ggplot()
  nightowl::boxplot(testdata,
    x = "Species",
    y = "Culmen Depth (mm)",
    add_violin = F,
    add_points = F,
    facet_row = c("Region", "Sex")
  ) %>%
    waRRior::expect_ggplot()
  nightowl::boxplot(testdata,
    x = "Species",
    y = "Culmen Depth (mm)",
    add_violin = F,
    add_boxplot = F,
    add_points = T,
    facet_row = "Region",
    facet_col = "Sex"
  ) %>%
    waRRior::expect_ggplot()
})
