test_that("wraping works", {

  varname <- "the brown fox jumped over the lazy dog"
  varname2 <- "lorem ipsum dolor sit amet"

  library(magrittr)
  testdata <- ChickWeight %>%
    dplyr::filter(Time < 10) %>%
    dplyr::mutate(!!rlang::sym(varname) := Diet) %>%
    dplyr::mutate(!!rlang::sym(varname2) := weight)

  nightowl::plot(testdata,
    transform = list(x = "factor"),
    mapping = list(
      y = varname2,
      x = varname, 
      color = varname,
      fill = varname,
      lty = varname
    ),
    layers = list(
      list(type = "boxplot")
    ),
    annotation = list(wrap_y = 30, wrap_guides = 10)
  )

})
