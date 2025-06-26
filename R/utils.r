#' expand mapping
expand_mapping <- function(mapping,
                           envir = parent.frame(),
                           as_symbol = T) {
  force(envir)
  purrr::iwalk(mapping, function(.value, .name) {
    if (!is.null(.value)) {
      assign(.name, rlang::sym(.value), envir = envir)
    }
  })
}
# =================================================
#' wrapping labeleer
#' @export
label_with_wraping <- function(x, width = 15) {
  purrr::map(ggplot2::label_both(x), ~ stringr::str_wrap(.x, width))
} # ===============================================================================
#' Spread data
#' @export
spread_data <- function(DATA, key, value = NULL) {
  if (is.null(value)) {
    return(DATA)
  } else {
    tryCatch({
      DATA %>%
        dplyr::ungroup() %>%
        tidyr::pivot_wider(names_from = key, values_from = value)
    })
  }
}
# ===============================================================================
#' Wrap text label at a certain width
#' @param width
#' @export
text_wraping <- function(DATA, cols = NULL, width = 30, ...) {
  if (is.null(cols)) cols <- names(DATA)[purrr::map_lgl(DATA, ~ is.factor(.x))]
  purrr::reduce(cols, function(.out, .col) {
    .x <- .out[[.col]]
    levels(.x) <- stringr::str_wrap(levels(.x), width = width)
    .out[[.col]] <- .x
    .out
  }, .init = DATA)
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
format_p_value <- function(p) {
  try(p <- round(p, digits = 4))
  pval <- base::format.pval(p,
    digits = 4, eps = 0.0001, nsmall = 4,
    scientific = FALSE
  )
  if (is.na(p) || is.null(p) || length(p) == 0) {
    return("Failed")
  }
  if (p < 0.001) {
    return(paste(pval, "(***)"))
  } else if (p < 0.01) {
    return(paste(pval, "(**)"))
  } else if (p < 0.05) {
    return(paste(p, "(*)"))
  } else {
    return(paste(p, "(n.s.)"))
  }
}
# =================================================s
`[.R6` <- function(x, ...) x$`[`(...)
`[<-.R6` <- function(x, ...) x$`[<-`(...)
length.R6 <- function(x) x$length()
format.R6 <- function(x) x$format()
as.data.frame.R6 <- function(x, ...) x$as.data.frame()
as.character.R6 <- function(x, ...) x$as.character()

# =================================================
# waRRior Replacement Functions
# =================================================

#' Split data at specific groups (compatible with waRRior::named_group_split_at)
#'
#' @param data Data frame to split
#' @param group_var Grouping variable name
#' @param at Group values to split at
#' @return Named list of data frames
#' @export
nightowl_group_split_at <- function(data, group_var, at) {
  # Group the data and split
  grouped_data <- dplyr::group_by(data, !!dplyr::sym(group_var))
  split_data <- dplyr::group_split(grouped_data, .keep = TRUE)
  
  # Get group keys to name the list
  group_keys <- dplyr::group_keys(grouped_data)
  names(split_data) <- group_keys[[group_var]]
  
  # Filter to only the requested groups if 'at' is specified
  if (!missing(at)) {
    split_data <- split_data[names(split_data) %in% at]
  }
  
  return(split_data)
}

#' Get groups from grouped data (compatible with waRRior::get_groups)
#'
#' @param data Grouped data frame
#' @return Vector of group names
#' @export
nightowl_get_groups <- function(data) {
  if (dplyr::is_grouped_df(data)) {
    group_vars <- dplyr::group_vars(data)
    return(group_vars)
  } else {
    return(character(0))
  }
}

#' Remove and return last element from list (compatible with waRRior::pop)
#'
#' @param x List or vector
#' @return Last element of the list/vector
#' @export
nightowl_pop <- function(x) {
  if (length(x) == 0) {
    return(NULL)
  }
  return(x[[length(x)]])
}

#' Get function by name (compatible with waRRior::getfun)
#'
#' @param name Function name as string
#' @param package Package name (optional)
#' @return Function object
#' @export
nightowl_getfun <- function(name, package = NULL) {
  if (!is.null(package)) {
    # Try to get function from specific package
    full_name <- paste0(package, "::", name)
    return(eval(parse(text = full_name)))
  } else {
    # Search in available environments
    return(get(name, mode = "function"))
  }
}
