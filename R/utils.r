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
format_p_value <- function(p, htmlsafe = T) {
  try(p <- round(p, digits = 4))
  pval <- base::format.pval(p,
    digits = 4, eps = 0.0001, nsmall = 4,
    scientific = FALSE
  )
  if (htmlsafe) pval <- stringr::str_replace_all(pval, "<", "&#60;")
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
# ===============================================================================
# from ggplot2 utilities.r
"%||%" <- function(a, b) {
  if (!is.null(a)) a else b
}
# =================================================s
`[.R6` <- function(x, ...) x$`[`(...)
`[<-.R6` <- function(x, ...) x$`[<-`(...)
length.R6 <- function(x) x$length()
format.R6 <- function(x) x$format()
as.data.frame.R6 <- function(x, ...) x$as.data.frame()
as.character.R6 <- function(x, ...) x$as.character()
# =================================================
#' Get group variables from data frame
nightowl_get_groups <- function(data) {
  dplyr::group_vars(data)
}
