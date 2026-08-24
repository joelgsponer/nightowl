# =================================================
#' @title Calculate Percentage Transformation
#' @description 
#' Transform count data to percentage values within groups. Useful for creating
#' percentage-based visualizations and summary statistics.
#' @param data A data frame containing the variables to transform
#' @param mapping A list containing variable mappings (x, facet_row, facet_col)
#' @return A list containing transformed data and updated mapping
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
#' @description 
#' Transform count data to frequency (proportion) values within groups. Creates
#' relative frequency values between 0 and 1.
#' @param data A data frame containing the variables to transform
#' @param mapping A list containing variable mappings, particularly x for grouping
#' @return A list containing transformed data and updated mapping
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
#' @description 
#' Transform data to count values by calculating frequencies within groups.
#' Provides raw count data for visualization and analysis.
#' @param data A data frame containing the variables to transform
#' @param mapping A list containing variable mappings for grouping
#' @return A list containing transformed data and updated mapping
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
