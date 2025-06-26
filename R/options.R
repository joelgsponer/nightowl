#' R6 Class
.NightowlOptions <- R6::R6Class("NightowlOptions",
  public = list(
    # Initialize the class ---------------------------------------------------
    initialize = function(colors = NULL,
                          missingColor = NULL) {
      if (is.null(colors)) colors <- nightowl_colors()
      if (is.null(missingColor)) missingColor <- nightowl_colors()["grey"]
      nightowl_progress_step("Setting up Nightowl")
      # Setting values
      self$set_missing_color(missingColor)
      self$set_colors(colors)
    },
    # Color handling -------------------------------------------------------
    set_colors = function(colors) {
      private$colors <- colors
    },
    get_colors = function(n = NULL, missing = TRUE) {
      colors <- private$colors
      if (!is.null(n)) {
        colors <- private$colors[1:n]
        if (missing) {
          colors <- colors[1:n - 1]
          colors <- c(colors, private$missingColor)
        }
      }
      unname(colors)
    },
    set_missing_color = function(color) {
      private$missingColor <- color
    },
    get_missing_color = function() {
      return(private$missingColor)
    },
    # Table settings -------------------------------------------------------
    set_header_width = function(width) {
      private$headerWidth <- width
    },
    get_header_width = function() {
      return(private$headerWidth)
    }
  ),
  private = list(
    colors = NULL,
    missingColor = NULL,
    headerWidth = 20
  )
)
# ====

#' Get Nightowl Package Options
#' 
#' @description Access the global nightowl options instance from package namespace
#' @return NightowlOptions R6 object instance
#' @export
get_nightowl_options <- function() {
  if (is.null(.pkg_env$nightowl_options)) {
    .pkg_env$nightowl_options <- .NightowlOptions$new()
  }
  return(.pkg_env$nightowl_options)
}

#' Set Nightowl Package Options  
#' 
#' @description Replace the global nightowl options instance
#' @param options NightowlOptions R6 object instance
#' @export
set_nightowl_options <- function(options) {
  if (!inherits(options, "NightowlOptions")) {
    stop("options must be a NightowlOptions object")
  }
  .pkg_env$nightowl_options <- options
  invisible(options)
}
