test_that("donut_plot works", {
  nightowl::make_donut_plot_categorical(
    data = palmerpenguins::penguins,
    column = "species",
    split = "island",
    label = "Species"
  ) %>%
    nightowl::render_donut_plot(header = levels(palmerpenguins::penguins$island))

  prop.table(table(palmerpenguins::penguins$island, palmerpenguins::penguins$species), 1)

  nightowl::make_donut_plot_numeric(
    data = palmerpenguins::penguins,
    column = "bill_length_mm",
    split = "island",
    label = "Bill length (mm)"
  ) %>%
    nightowl::render_donut_plot(header = levels(palmerpenguins::penguins$island))

  nightowl::donut_plot(
    data = palmerpenguins::penguins,
    columns = c("bill_length_mm", "species"),
    split = "island"
  )

  nightowl::donut_plot(
    data = palmerpenguins::penguins,
    columns = c("bill_length_mm", "species"),
    split = "island",
    labels = c("Bill length (mm)", "Species"),
    options = list(brewer_pal = "YlGnBu")
  )

  nightowl::donut_plot(
    data = palmerpenguins::penguins,
    columns = c("sex", "bill_length_mm", "species"),
    split = "island",
    labels = c("Bill length (mm)", "Species"),
    options = list(numeric = list(colors = picasso::colors_ibm()), categorical = list(brewer_pal = "YlGnBu"))
  )

  nightowl::donut_plot(
    data = palmerpenguins::penguins,
    columns = c("sex", "bill_length_mm", "species"),
    split = "island"
  )
})
