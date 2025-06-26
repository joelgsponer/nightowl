# =================================================
#' @title Get Human-Readable Labels for Variables
#' @description Maps variable names to their human-readable labels using a provided labels list
#' @param x Character vector of variable names to get labels for
#' @param labels Named list where names are variable names and values are their corresponding labels
#' @return Character vector of labels corresponding to the input variable names. Returns the original variable name if no label is found
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
