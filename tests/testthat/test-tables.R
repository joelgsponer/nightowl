test_that("tables works", {
  library(magrittr)
  testdata <- palmerpenguins::penguins_raw
  nightowl::summary_table(testdata,
    x = "Species",
    y = "Culmen Depth (mm)",
  )

  nightowl::summary_table(testdata,
    x = "Species",
    y = c("Island", "Culmen Depth (mm)"),
  )
  nightowl::summary_table(testdata,
    x = c("Species"),
    facet_col = c("Island"),
    y = c("Culmen Depth (mm)"),
  )
  nightowl::summary_table(testdata,
    x = "Species",
    group = "Sex",
    y = c("Culmen Depth (mm)"),
    facet_row = c("Island")
  )
  nightowl::summary_table(testdata,
    x = "Species",
    group = "Sex",
    group_split = "col",
    y = c("Culmen Depth (mm)"),
    facet_row = c("Island")
  )
  nightowl::summary_table(testdata,
    x = "Species",
    y = c("Sex"),
    facet_row = c("Island")
  )
  nightowl::summary_table(testdata,
    x = "Species",
    y = c("Sex"),
    facet_row = c("Island"),
    denom = "N_row"
  )
  nightowl::summary_table(testdata,
    x = "Species",
    y = c("Sex"),
    facet_row = c("Island"),
    denom = "N_col"
  )
  nightowl::summary_table(testdata,
    x = "Species",
    y = c("Sex"),
    facet_row = c("Island"),
    denom = "N_col",
    notused = "lorem"
  )
  testthat::expect_true(TRUE)
})
