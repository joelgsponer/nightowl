test_that("boxplot works", {
  species <- iris$Species %>%
    unique() %>%
    as.character()
  testdata <- tibble::tibble(
    Y = runif(100),
    X = rep(seq(1, 5), 20),
    ID = sample(species, 100, replace = T),
    GROUP = c(rep("A", 50), rep("B", 50)),
    `GROUP (X)` = c(rep("A", 50), rep("B", 50))
  )
  testdata

  nightowl::boxplot(testdata,
    x = "GROUP",
    y = "Y",
    fill = "GROUP"
  )

  nightowl::boxplot(testdata,
    x = "GROUP (X)",
    y = "Y"
  )

  nightowl::boxplot(testdata,
    x = "GROUP (X)",
    y = "Y",
    theme = picasso::theme_dark
  )

  nightowl::boxplot(testdata,
    x = "GROUP (X)",
    y = "Y",
    add_violin = F,
    add_points = F,
    facet_col = "GROUP"
  )

  nightowl::boxplot(testdata,
    x = "GROUP (X)",
    y = "Y",
    add_violin = F,
    add_points = F,
    facet_row = c("GROUP", "ID")
  )

  nightowl::boxplot(testdata,
    x = "GROUP (X)",
    y = "Y",
    add_violin = F,
    add_boxplot = F,
    add_points = T,
    facet_row = "GROUP",
    facet_col = "ID"
  )
})
