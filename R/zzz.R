.onLoad <- function(libname, pkgname) {
  assign("NightowlOptions",
    nightowl:::.NightowlOptions$new(),
    env = globalenv()
  )
}
