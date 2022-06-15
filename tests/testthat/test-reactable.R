test_that("multiplication works", {
  data <- mtcars
  reactable::reactable(data, groupBy = "vs")

  nightowl::render_reactable(data)
})
