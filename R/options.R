#' R6 Class
.NightowlOptions <- R6::R6Class("NightowlOptions",
  public = list(
    # Initialize the class ---------------------------------------------------
    initialize = function(colors = picasso::roche_colors(),
                          missingColor = picasso::roche_colors()["grey"]) {
      cli::cli_progress_step("Setting up Nightowl")
      # Setting values
      self$set_missing_color(missingColor)
      self$set_colors(colors)
    },
    # Color handling -------------------------------------------------------
    set_colors = function(colors) {
      private$colors <- waRRior::pop(colors, private$missingColor)
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
