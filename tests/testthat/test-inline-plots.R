test_that("inline plots work", {
  a <- nightowl::add_violin(runif(100))
  a

  nightowl::make_scale(a)


  a$css <- list(style = list(background = "red"))
  a$html(resize = FALSE)
  htmltools::browsable()
})
