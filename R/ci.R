# ===============================================================================
# Creating confidence intervals
# ===============================================================================

#' T-Distribution
#' @export
ci_t <- function(x, alpha) {
  .mean <- mean(x)
  .sd <- sd(x)
  n <- length(x)
  df <- n - 1
  t <- qt(p = alpha / 2, df, lower.tail = F)
  .error <- (t * (.sd / sqrt(n)))
  lower <- .mean - .error
  upper <- .mean + .error
  res <- c(lower, upper)
  attr(res, "conf.level") <- 1 - alpha
  return(res)
}
