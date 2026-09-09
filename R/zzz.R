.onLoad <- function(libname, pkgname) {
  op <- options()
  defaults <- nightowl_default_options()
  unset <- !(names(defaults) %in% names(op))
  if (any(unset)) options(defaults[unset])
  invisible()
}
