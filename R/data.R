# =================================================
#' @title Detect Statistical Outliers
#' @description Detects outliers in a numeric vector using boxplot statistics with a fold threshold.
#' @param x A numeric vector
#' @param fold Numeric threshold multiplier for outlier detection (default: 15)
#' @return Logical vector indicating outlier positions
#' @export
detect_outliers <- function(x, fold = 15) {
  tmp <- graphics::boxplot(x, plot = FALSE)
  to_keep <- dplyr::if_else(x > fold * tmp$stats[5] | x < (1 / fold) * tmp$stats[1],
    TRUE, FALSE
  )
  to_keep[is.na(to_keep)] <- FALSE
  return(to_keep)
}
# =================================================
#' @title Count Statistical Outliers
#' @description Counts the number of outliers in a numeric vector using boxplot statistics.
#' @param x A numeric vector
#' @param fold Numeric threshold multiplier for outlier detection (default: 15)
#' @return Integer count of outliers
#' @export
count_outliers <- function(x, fold = 15) {
  tmp <- graphics::boxplot(x, plot = FALSE)
  to_keep <- dplyr::if_else(x > fold * tmp$stats[5] | x < (1 / fold) * tmp$stats[1],
    TRUE, FALSE
  )
  sum(to_keep, na.rm = T)
}
# =================================================
