# Helper functions to replace non-CRAN dependencies for CRAN compliance

#' Pop elements from a vector/list (replacement for waRRior::pop)
#' @param x vector/list to pop from  
#' @param to_remove elements to remove
#' @return vector/list with elements removed
pop <- function(x, to_remove) {
  x[!x %in% to_remove]
}

#' Get number of unique values (replacement for waRRior::length_unique)  
#' @param x vector
#' @return integer count of unique values
length_unique <- function(x) {
  length(unique(x))
}

#' Get function from character string (replacement for waRRior::getfun)
#' @param x character string representing function name
#' @return function object
getfun <- function(x) {
  if (is.character(x)) {
    # Handle namespace-qualified function names
    if (grepl("::", x)) {
      parts <- strsplit(x, "::")[[1]]
      pkg <- parts[1]
      fun_name <- parts[2]
      return(get(fun_name, envir = asNamespace(pkg)))
    } else {
      return(get(x))
    }
  }
  return(x)
}