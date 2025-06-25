# =================================================
#' @title Detect Outliers in Numeric Vector
#' @description Identifies outliers in a numeric vector using boxplot statistics and a fold threshold
#' @param x Numeric vector to check for outliers
#' @param fold Numeric value for outlier detection threshold (default: 15). Values above fold * upper whisker or below (1/fold) * lower whisker are marked as outliers
#' @return Logical vector indicating which values are outliers (TRUE) or not (FALSE)
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
#' @title Count Outliers in Numeric Vector
#' @description Counts the number of outliers in a numeric vector using boxplot statistics and a fold threshold
#' @param x Numeric vector to check for outliers
#' @param fold Numeric value for outlier detection threshold (default: 15). Values above fold * upper whisker or below (1/fold) * lower whisker are counted as outliers
#' @return Integer count of outliers found in the vector
#' @export
count_outliers <- function(x, fold = 15) {
  tmp <- graphics::boxplot(x, plot = FALSE)
  to_keep <- dplyr::if_else(x > fold * tmp$stats[5] | x < (1 / fold) * tmp$stats[1],
    TRUE, FALSE
  )
  sum(to_keep, na.rm = T)
}
# =================================================
