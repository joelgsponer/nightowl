test_that("grouped chisq works", {
  input <- tibble::tibble(
    my_split = c(sample(LETTERS[1:4], 100, T), "bad"),
    group = sample(letters[1:2], 101, T),
    values = sample(c("Apples", "Pears"), 101, T)
  )
  input

  test <- nightowl::grouped_chisq(input, group_by = "my_split", x = "group", y = "values")
  nightowl::extract_results_grouped_chisq(test)
  nightowl::report_results_grouped_chisq(test)

  nightowl::extract_errors_grouped_chisq(test)
  nightowl::report_errors_grouped_chisq(test)

  nightowl::plot_grouped_chisq(input, group_by = "my_split", x = "group", y = "values", flex_direction = "column")
  nightowl::reactable_grouped_chisq(input, group_by = "my_split", x = "group", y = "values")
})
