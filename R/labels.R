# =================================================
#' @title
#' MISSING_TITLE
#' @description
#' @detail
#' @param
#' @return
#' @export
get_labels <- function(x, labels) {
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
