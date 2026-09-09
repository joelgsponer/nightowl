# Shared fixtures. Deterministic, no external data.
test_gg <- function() {
  ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) + ggplot2::geom_point()
}

test_plot <- function(width = 3, height = 2) {
  Plot$new(test_gg(), svg = list(width = width, height = height))
}

test_long <- function(n_per = 40, seed = 1) {
  set.seed(seed)
  data.frame(
    g = factor(rep(c("a", "b", "c"), each = n_per)),
    t = factor(rep(1:4, length.out = 3 * n_per)),
    v = stats::rnorm(3 * n_per),
    id = rep(seq_len(n_per * 3 / 4), 4)
  )
}
