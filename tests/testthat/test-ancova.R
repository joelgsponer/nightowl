test_that("ANCOVA", {
  testdata <- data.frame(
    y = runif(100),
    x = sample(c("A", "B", "C"), 100, TRUE)
  )
  m <- lm(y ~ x, data = testdata)
  aov(y ~ x, data = testdata)
  a <- anova(m)
  car::leveneTest(m)
  car::Anova(m)
  car::Anova(m, type = "III")
})
