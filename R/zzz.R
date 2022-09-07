# On Package load it assigns som 
.onLoad <- function(libname, pkgname) {
  assign("NightowlOptions",
    nightowl:::.NightowlOptions$new(),
    env = globalenv()
  )
}
