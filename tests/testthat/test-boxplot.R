test_that("boxplot works", {
  testdata <- palmerpenguins::penguins_raw
  nightowl::boxplot(testdata,
    x = "Species",
    y = "Culmen Depth (mm)",
    add_violin = T,
    add_boxplot = F,
    points_size = 3,
    facet_col = "Island"
  )

  nightowl::summary_table(testdata,
    x = "Species",
    y = "Culmen Depth (mm)",
    facet_col = "Island"
  )

  nightowl::boxplot(testdata,
    x = "Sex",
    y = "Culmen Depth (mm)",
    add_violin = T,
    add_boxplot = F,
    points_size = 3,
    facet_col = "Region",
    remove_missing = F,
    plot_height = T # This is an additonal parameter that is not needed
  )

  nightowl::boxplot(testdata,
    x = "Species",
    y = "Culmen Depth (mm)",
    add_violin = T,
    add_boxplot = F,
    points_size = 3,
    facet_row = "Region",
    remove_missing = F
  )

  nightowl::boxplot(testdata,
    x = "Region",
    y = "Culmen Depth (mm)",
  )
  nightowl::boxplot(testdata,
    x = "Island",
    y = "Culmen Depth (mm)",
    theme = picasso::theme_dark
  )
  nightowl::boxplot(testdata,
    x = "Island",
    y = "Culmen Depth (mm)",
    add_violin = F,
    add_points = F,
    facet_col = "Region"
  )
  nightowl::boxplot(testdata,
    x = "Species",
    y = "Culmen Depth (mm)",
    add_violin = F,
    add_points = F,
    facet_row = c("Region", "Sex")
  )
  nightowl::boxplot(testdata,
    x = "Species",
    y = "Culmen Depth (mm)",
    add_violin = F,
    add_boxplot = F,
    add_points = T,
    facet_row = "Region",
    facet_col = "Sex"
  )
})
