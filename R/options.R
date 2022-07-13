# #' R6 Class 
# #' @description
# #' @detail
# #' 
NightowlOptions <- R6::R6Class('NightowlOptions',
  public = list(
     initialize = function(
      colors = picasso::roche_colors()
    ){
      cli::cli_progress_step('Setting up Nightowl')
      self$colors <- colors
    },
     colors = NULL 
  ),
  private = list()
)
