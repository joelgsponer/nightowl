# =================================================
#' @title
#' MISSING_TITLE
#' @description
#' @detail
#' @param
#' @return
#' @export
percentage <- function(data, mapping) {
  n <- data %>%
    dplyr::group_by_all() %>%
    dplyr::tally()
  N <- data %>%
    dplyr::group_by_at(c(mapping$x, mapping$facet_row, mapping$facet_col)) %>%
    dplyr::tally() %>%
    dplyr::rename(N = n)
  join <- dplyr::inner_join(n, N)
  mapping$y <- "percentage"
  join[[mapping$y]] <- join$n / join$N * 100
  return(list(data = join, mapping = mapping))
}
# =================================================
#' @title
#' MISSING_TITLE
#' @description
#' @detail
#' @param
#' @return
#' @export
frequency <- function(data, mapping) {
  n <- data %>%
    dplyr::group_by_all() %>%
    dplyr::tally()
  N <- data %>%
    dplyr::group_by_at(mapping$x) %>%
    dplyr::tally() %>%
    dplyr::rename(N = n)
  join <- dplyr::inner_join(n, N)
  mapping$y <- "frequency"
  join[[mapping$y]] <- join$n / join$N
  return(list(data = join, mapping = mapping))
}
# =================================================
# =================================================
#' @title
#' MISSING_TITLE
#' @description
#' @detail
#' @param
#' @return
#' @export
count <- function(data, mapping) {
  n <- data %>%
    dplyr::group_by_all() %>%
    dplyr::tally()
  mapping$y <- "n"
  join[[mapping$y]] <- join$n / join$N * 100
  return(list(data = join, mapping = mapping))
}
# =================================================
