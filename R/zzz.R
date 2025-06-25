# Package environment for internal state management
.pkg_env <- new.env(parent = emptyenv())

# On Package load - initialize internal package state
.onLoad <- function(libname, pkgname) {
  # Initialize options in package environment instead of global environment
  .pkg_env$nightowl_options <- .NightowlOptions$new()
}
