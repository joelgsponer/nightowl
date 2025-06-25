test_that("reactable tables render with proper formatting and grouping", {
  data <- mtcars
  reactable::reactable(data, groupBy = "vs")

  nightowl::render_reactable(data)
})
