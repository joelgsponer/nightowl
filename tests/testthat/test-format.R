test_that("format_p_value formats consistently", {
  p <- c(0.00001, 0.004, 0.032, 0.21, NA)
  expect_equal(format_p_value(p, html = FALSE), c("<0.001", "0.004", "0.032", "0.210", NA))
  expect_equal(format_p_value(p)[1], "&lt;0.001")
  expect_equal(format_p_value(0.004, stars = TRUE, html = FALSE), "0.004 **")
  expect_equal(format_p_value(0.5, digits = 2, html = FALSE), "0.50")
  expect_error(format_p_value("a"), "numeric")
})

test_that("mean_ci matches t.test and handles small samples", {
  x <- mtcars$mpg
  ci <- mean_ci(x)
  tt <- stats::t.test(x)
  expect_equal(ci$y, mean(x))
  expect_equal(c(ci$ymin, ci$ymax), as.numeric(tt$conf.int))
  expect_equal(mean_ci(numeric(0))$y, NA_real_)
  expect_true(is.na(mean_ci(5)$ymin))
  b1 <- mean_ci(x, method = "boot", seed = 1)
  b2 <- mean_ci(x, method = "boot", seed = 1)
  expect_equal(b1, b2)
  expect_true(b1$ymin < b1$y && b1$y < b1$ymax)
})

test_that("format_mean_ci returns a one-row tibble", {
  out <- format_mean_ci(mtcars$mpg)
  expect_s3_class(out, "tbl_df")
  expect_named(out, c("Mean", "CI"))
  expect_equal(out$Mean, round(mean(mtcars$mpg), 2))
  expect_match(as.character(out$CI), "^\\[[0-9.]+, [0-9.]+\\]$")
})

test_that("count_extreme_values is sign-safe", {
  x <- c(stats::rnorm(200, sd = 0.1), 50, -50)
  expect_equal(count_extreme_values(x), 2)
  y <- stats::rnorm(500)
  expect_equal(count_extreme_values(y - 100), count_extreme_values(y))
  expect_equal(count_extreme_values(-y), count_extreme_values(y))
  expect_equal(count_extreme_values(c(rep(-100, 50), -1000)), 1)
  expect_equal(count_extreme_values(c(1, 2, NA)), 0)
})

test_that("format_frequencies spreads levels into columns", {
  x <- factor(c("a", "b", "a", NA))
  out <- format_frequencies(x)
  expect_named(out, c("a", "b", "(Missing)"))
  expect_equal(out$a, "50.0% (2)")
  expect_equal(format_frequencies(x, output = "counts")$a, 2L)
  expect_equal(format_frequencies(x, output = "percent")$b, 25)
  expect_named(format_frequencies(factor(c("a", "b"))), c("a", "b"))
})
