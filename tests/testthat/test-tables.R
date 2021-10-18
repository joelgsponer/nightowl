test_that("tables works", {
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
    facet_cols = c("Island"),
    y = c("Culmen Depth (mm)"),
  )
  nightowl::summary_table(testdata,
    x = "Species",
    group = "Sex",
    y = c("Culmen Depth (mm)"),
    facet_rows = c("Island")
  )
  nightowl::summary_table(testdata,
    x = "Species",
    group = "Sex",
    group_split = "cols",
    y = c("Culmen Depth (mm)"),
    facet_rows = c("Island")
  )
  nightowl::summary_table(testdata,
    x = "Species",
    y = c("Sex"),
    facet_rows = c("Island")
  )
  nightowl::summary_table(testdata,
    x = "Species",
    y = c("Sex"),
    facet_rows = c("Island"),
    denom = "N_row"
  )
  nightowl::summary_table(testdata,
    x = "Species",
    y = c("Sex"),
    facet_rows = c("Island"),
    denom = "N_col"
  )
})
