# =================================================
#' @title Calculate Percentage Transformation
#' @description Transforms count data to percentages within groups
#' @param data Data frame containing the data to transform
#' @param mapping List specifying variable mappings including x, facet_row, and facet_col
#' @return List containing transformed data with percentage column and updated mapping
#' @export
percentage <- function(data, mapping) {
  # Input validation
  if (!is.data.frame(data)) {
    stop("data must be a data frame")
  }
  if (!is.list(mapping) || is.null(mapping$x)) {
    stop("mapping must be a list with at least an 'x' element")
  }
  
  # Count occurrences by all variables
  n <- data %>%
    dplyr::group_by(dplyr::across(dplyr::everything())) %>%
    dplyr::tally()
  
  # Count totals by grouping variables (removing NULL values)
  group_vars <- c(mapping$x, mapping$facet_row, mapping$facet_col)
  group_vars <- group_vars[!sapply(group_vars, is.null)]
  
  N <- data %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) %>%
    dplyr::tally() %>%
    dplyr::rename(N = n)
  
  # Join with explicit key specification
  join_cols <- intersect(names(n), names(N))
  join <- dplyr::inner_join(n, N, by = join_cols)
  
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
  # Input validation
  if (!is.data.frame(data)) {
    stop("data must be a data frame")
  }
  if (!is.list(mapping) || is.null(mapping$x)) {
    stop("mapping must be a list with at least an 'x' element")
  }
  
  # Count occurrences by all variables
  n <- data %>%
    dplyr::group_by(dplyr::across(dplyr::everything())) %>%
    dplyr::tally()
  
  # Count totals by x variable
  N <- data %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(mapping$x))) %>%
    dplyr::tally() %>%
    dplyr::rename(N = n)
  
  # Join with explicit key specification
  join_cols <- intersect(names(n), names(N))
  join <- dplyr::inner_join(n, N, by = join_cols)
  
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
  # Input validation
  if (!is.data.frame(data)) {
    stop("data must be a data frame")
  }
  if (!is.list(mapping)) {
    stop("mapping must be a list")
  }
  
  # Count occurrences by all variables
  n <- data %>%
    dplyr::group_by(dplyr::across(dplyr::everything())) %>%
    dplyr::tally()
  
  mapping$y <- "n"
  return(list(data = n, mapping = mapping))
}
# =================================================
