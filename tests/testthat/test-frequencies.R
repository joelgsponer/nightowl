test_that("frequencies work", {

  x = c(
    rep("A", 25),
    rep("B", 50),
    rep(NA, 25)
  )

  nightowl::frequencies(x) %>% nightowl::render_kable()


})
