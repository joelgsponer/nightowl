test_that("tables works", {

  library(magrittr)
  testdata <- palmerpenguins::penguins_raw
  nightowl::table(testdata,
    mapping = list(
      x = "Species",
      y = c("Region", "Culmen Depth (mm)")
    )
  )

  random.cdisc.data::cadae$AESER

  nightowl::table(random.cdisc.data::cadae,
    mapping = list(
      x = "ARM",
      id = "USUBJID",
      split_rows_by = "SEX",
      events = c("AESER", "AESDTH")
    ),
    layers = list(
      list(
        type = "tern::summarize_patients_events_in_cols",
        filters_list = list(
          serious = c(AESER = "Y"),
          fatal = c(AESDTH = "Y")
        )
      )
    )
  )

  nightowl::table(random.cdisc.data::cadae,
    mapping = list(
      x = "ARM",
      id = "USUBJID",
      vars = "AEBODSYS",
      split_rows_by = "SEX"
    ),
    layers = list(
      list(
        type = "rtables::add_overall_col",
        label = "All Patients"
      ),
      list(
        type = "tern::summarize_num_patients",
        var = "USUBJID"
      ),
      list(
        type = "rtables::split_rows_by",
        var = "AEBODSYS",
        child_labels = "visible",
        nested = FALSE,
        indent_mod = -1L,
        split_fun = drop_split_levels,
        label_pos = "topleft"
        #split_label = obj_label(adae$AEBODSYS)
      ),
      list(
        type = "tern::summarize_num_patients",
        var = "USUBJID"
      )
    )
  )

  summarize_num_patients(
    var = "USUBJID",
    .stats = c("unique", "nonunique"),
    .labels = c(
      unique = "Total number of patients with at least one adverse event",
      nonunique = "Overall total number of events"
    )


  nightowl::summary_table(testdata,
    x = "Species",
    y = "Island"
  )

  nightowl::summary_table(testdata,
    x = "Species",
    y = c("Sex", "Culmen Depth (mm)"),
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
