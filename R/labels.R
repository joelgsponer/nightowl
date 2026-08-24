# =================================================
#' @title Extract Variable Labels
#' @description 
#' Extract descriptive labels for variables from a labels list, falling back to 
#' variable names if labels are not available.
#' @param x A character vector of variable names
#' @param labels A named list or vector containing variable labels
#' @return A character vector of labels corresponding to the input variables
#' @export
get_labels <- function(x, labels) {
  if (is.null(x)) {
    return(NULL)
  }
  purrr::map_chr(x, function(.var) {
    .label <- tryCatch(labels[[.var]], error = function(e) {
      return(.var)
    })
    if (!is.null(.label)) {
      .label
    } else {
      .var
    }
  })
}
# =================================================
