test_that("tests works", {
  testthat::expect_error(nightowl::calc_test(tibble::as_tibble(iris), "Sepal.Length", gracefully = F))

  nightowl::calc_test(tibble::as_tibble(iris) %>% dplyr::group_by(Species), "Sepal.Length")

  nightowl::calc_test(palmerpenguins::penguins %>% dplyr::group_by(island), "species")

  nightowl::Test$new(palmerpenguins::penguins %>% dplyr::group_by(island), "species")
  nightowl::Test$new(palmerpenguins::penguins %>% dplyr::group_by(island), "bill_length_mm")

  t$table
})
