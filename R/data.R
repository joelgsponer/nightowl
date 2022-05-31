# =================================================
#' @title
#' MISSING_TITLE
#' @description
#' @detail
#' @param
#' @return
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
