# =================================================
#' @title Calculate Percentage Transformation
#' @description Transforms count data to percentages within groups
#' @param data Data frame containing the data to transform
#' @param mapping List specifying variable mappings including x, facet_row, and facet_col
#' @return List containing transformed data with percentage column and updated mapping
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
#' @title Calculate Frequency Transformation
#' @description Transforms count data to frequencies (proportions) within groups
#' @param data Data frame containing the data to transform
#' @param mapping List specifying variable mappings including x variable
#' @return List containing transformed data with frequency column and updated mapping
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
#' @title Calculate Count Transformation
#' @description Counts occurrences of combinations in grouped data
#' @param data Data frame containing the data to count
#' @param mapping List specifying variable mappings
#' @return List containing counted data with n column and updated mapping
#' @export
count <- function(data, mapping) {
  n <- data %>%
    dplyr::group_by_all() %>%
    dplyr::tally()
  mapping$y <- "n"
  return(list(data = n, mapping = mapping))
}
# =================================================
