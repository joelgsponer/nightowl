test_that("multiplication works", {
  require(magrittr)
  x <- runif(sample(seq(10, 100), 1))
  alpha <- runif(1)
  nightowl::ci_t(x, alpha)
  nightowl::ci_t(x, alpha) %>%
    expect_equal(t.test(x, conf.level = 1 - alpha)$conf.int)
})
