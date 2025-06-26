#' Custom Error Classes for nightowl
#'
#' Standardized error handling with custom error classes
#' @keywords internal

#' Base nightowl error class
#'
#' @param message Error message
#' @param call The call that generated the error
#' @return An error object of class 'nightowl_error'
#' @keywords internal
nightowl_error <- function(message, call = NULL) {
  structure(
    list(message = message, call = call),
    class = c("nightowl_error", "error", "condition")
  )
}

#' Input validation error
#'
#' @param message Error message
#' @param parameter Parameter name that failed validation
#' @param value The invalid value
#' @param call The call that generated the error
#' @return An error object of class 'validation_error'
#' @keywords internal
validation_error <- function(message, parameter = NULL, value = NULL, call = NULL) {
  structure(
    list(
      message = message,
      parameter = parameter,
      value = value,
      call = call
    ),
    class = c("validation_error", "nightowl_error", "error", "condition")
  )
}

#' Security error for unsafe operations
#'
#' @param message Error message
#' @param operation The operation that was attempted
#' @param call The call that generated the error
#' @return An error object of class 'security_error'
#' @keywords internal
security_error <- function(message, operation = NULL, call = NULL) {
  structure(
    list(
      message = message,
      operation = operation,
      call = call
    ),
    class = c("security_error", "nightowl_error", "error", "condition")
  )
}

#' Data error for data-related issues
#'
#' @param message Error message
#' @param data_info Information about the problematic data
#' @param call The call that generated the error
#' @return An error object of class 'data_error'
#' @keywords internal
data_error <- function(message, data_info = NULL, call = NULL) {
  structure(
    list(
      message = message,
      data_info = data_info,
      call = call
    ),
    class = c("data_error", "nightowl_error", "error", "condition")
  )
}

#' Throw a validation error
#'
#' @param message Error message
#' @param parameter Parameter name that failed validation
#' @param value The invalid value
#' @param call The call that generated the error (defaults to parent call)
#' @keywords internal
throw_validation_error <- function(message, parameter = NULL, value = NULL, call = sys.call(-1)) {
  err <- validation_error(message, parameter, value, call)
  stop(err)
}

#' Throw a security error
#'
#' @param message Error message
#' @param operation The operation that was attempted
#' @param call The call that generated the error (defaults to parent call)
#' @keywords internal
throw_security_error <- function(message, operation = NULL, call = sys.call(-1)) {
  err <- security_error(message, operation, call)
  stop(err)
}

#' Throw a data error
#'
#' @param message Error message
#' @param data_info Information about the problematic data
#' @param call The call that generated the error (defaults to parent call)
#' @keywords internal
throw_data_error <- function(message, data_info = NULL, call = sys.call(-1)) {
  err <- data_error(message, data_info, call)
  stop(err)
}

#' Print method for nightowl errors
#'
#' @param x A nightowl error object
#' @param ... Additional arguments (ignored)
#' @export
print.nightowl_error <- function(x, ...) {
  cat("nightowl error: ", x$message, "\n", sep = "")
  if (!is.null(x$call)) {
    cat("Call: ", deparse(x$call), "\n", sep = "")
  }
}

#' Print method for validation errors
#'
#' @param x A validation error object
#' @param ... Additional arguments (ignored)
#' @export
print.validation_error <- function(x, ...) {
  cat("Validation error: ", x$message, "\n", sep = "")
  if (!is.null(x$parameter)) {
    cat("Parameter: ", x$parameter, "\n", sep = "")
  }
  if (!is.null(x$value)) {
    cat("Invalid value: ", as.character(x$value), "\n", sep = "")
  }
  if (!is.null(x$call)) {
    cat("Call: ", deparse(x$call), "\n", sep = "")
  }
}

#' Print method for security errors
#'
#' @param x A security error object
#' @param ... Additional arguments (ignored)
#' @export
print.security_error <- function(x, ...) {
  cat("Security error: ", x$message, "\n", sep = "")
  if (!is.null(x$operation)) {
    cat("Operation: ", x$operation, "\n", sep = "")
  }
  if (!is.null(x$call)) {
    cat("Call: ", deparse(x$call), "\n", sep = "")
  }
}