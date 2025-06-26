#' Input Validation Framework
#'
#' Centralized validation utilities for secure input handling
#' @keywords internal

#' Validate file path for security
#'
#' @param path Character string path to validate
#' @param allow_absolute Logical, whether to allow absolute paths
#' @return Logical, TRUE if path is safe
#' @keywords internal
is_safe_path <- function(path, allow_absolute = FALSE) {
  if (!is.character(path) || length(path) != 1 || is.na(path)) {
    return(FALSE)
  }
  
  # Check for path traversal attempts
  if (grepl("\\.\\.", path) || grepl("~", path)) {
    return(FALSE)
  }
  
  # Check for absolute paths if not allowed
  if (!allow_absolute && (startsWith(path, "/") || grepl("^[A-Za-z]:", path))) {
    return(FALSE)
  }
  
  # Check for null bytes and other dangerous characters
  if (grepl("\\x00", path) || grepl("[<>:\"|?*]", path)) {
    return(FALSE)
  }
  
  TRUE
}

#' Validate numeric input
#'
#' @param x Input to validate
#' @param min_value Minimum allowed value
#' @param max_value Maximum allowed value
#' @param allow_na Whether NA values are allowed
#' @param param_name Parameter name for error messages
#' @return The validated input
#' @keywords internal
validate_numeric <- function(x, min_value = -Inf, max_value = Inf, 
                           allow_na = FALSE, param_name = "x") {
  if (!is.numeric(x)) {
    stop(paste0("Parameter '", param_name, "' must be numeric, not ", class(x)[1]))
  }
  
  if (!allow_na && any(is.na(x))) {
    stop(paste0("Parameter '", param_name, "' cannot contain NA values"))
  }
  
  if (any(x[!is.na(x)] < min_value, na.rm = TRUE)) {
    stop(paste0("Parameter '", param_name, "' must be >= ", min_value))
  }
  
  if (any(x[!is.na(x)] > max_value, na.rm = TRUE)) {
    stop(paste0("Parameter '", param_name, "' must be <= ", max_value))
  }
  
  x
}

#' Validate data frame input
#'
#' @param data Input to validate
#' @param required_cols Required column names
#' @param param_name Parameter name for error messages
#' @return The validated data frame
#' @keywords internal
validate_data_frame <- function(data, required_cols = NULL, param_name = "data") {
  if (!is.data.frame(data)) {
    stop(paste0("Parameter '", param_name, "' must be a data frame, not ", class(data)[1]))
  }
  
  if (nrow(data) == 0) {
    stop(paste0("Parameter '", param_name, "' cannot be empty"))
  }
  
  if (!is.null(required_cols)) {
    missing_cols <- setdiff(required_cols, names(data))
    if (length(missing_cols) > 0) {
      stop(paste0("Parameter '", param_name, "' must contain columns: ", 
                  paste(missing_cols, collapse = ", ")))
    }
  }
  
  data
}

#' Validate column names
#'
#' @param names Character vector of column names to validate
#' @param param_name Parameter name for error messages
#' @return The validated column names
#' @keywords internal
validate_column_names <- function(names, param_name = "column names") {
  if (!is.character(names)) {
    stop(paste0("Parameter '", param_name, "' must be character, not ", class(names)[1]))
  }
  
  # Check for valid R variable names (prevent code injection)
  valid_pattern <- "^[a-zA-Z][a-zA-Z0-9._]*$"
  invalid_names <- names[!grepl(valid_pattern, names)]
  
  if (length(invalid_names) > 0) {
    stop(paste0("Invalid ", param_name, ": ", paste(invalid_names, collapse = ", "), 
                ". Names must start with a letter and contain only letters, numbers, dots, and underscores."))
  }
  
  names
}

#' Validate character input
#'
#' @param x Input to validate
#' @param allowed_values Vector of allowed values (NULL for any)
#' @param min_length Minimum string length
#' @param max_length Maximum string length
#' @param param_name Parameter name for error messages
#' @return The validated input
#' @keywords internal
validate_character <- function(x, allowed_values = NULL, min_length = 0, 
                              max_length = Inf, param_name = "x") {
  if (!is.character(x)) {
    stop(paste0("Parameter '", param_name, "' must be character, not ", class(x)[1]))
  }
  
  if (any(nchar(x) < min_length)) {
    stop(paste0("Parameter '", param_name, "' must be at least ", min_length, " characters"))
  }
  
  if (any(nchar(x) > max_length)) {
    stop(paste0("Parameter '", param_name, "' must be at most ", max_length, " characters"))
  }
  
  if (!is.null(allowed_values)) {
    invalid_values <- x[!x %in% allowed_values]
    if (length(invalid_values) > 0) {
      stop(paste0("Invalid values for '", param_name, "': ", 
                  paste(invalid_values, collapse = ", "), 
                  ". Allowed values are: ", paste(allowed_values, collapse = ", ")))
    }
  }
  
  x
}

#' Validate file path securely
#'
#' @param path File path to validate
#' @param must_exist Whether file must exist
#' @param allowed_extensions Vector of allowed file extensions
#' @param param_name Parameter name for error messages
#' @return The validated file path
#' @keywords internal
validate_file_path <- function(path, must_exist = FALSE, allowed_extensions = NULL, 
                              param_name = "path") {
  if (!is.character(path) || length(path) != 1) {
    stop(paste0("Parameter '", param_name, "' must be a single character string"))
  }
  
  if (!is_safe_path(path)) {
    stop(paste0("Parameter '", param_name, "' contains unsafe path characters or path traversal attempts"))
  }
  
  if (must_exist && !file.exists(path)) {
    stop(paste0("File specified in '", param_name, "' does not exist: ", path))
  }
  
  if (!is.null(allowed_extensions)) {
    file_ext <- tools::file_ext(path)
    if (!file_ext %in% allowed_extensions) {
      stop(paste0("Parameter '", param_name, "' must have one of these extensions: ", 
                  paste(allowed_extensions, collapse = ", ")))
    }
  }
  
  path
}

#' Validate logical input
#'
#' @param x Input to validate
#' @param param_name Parameter name for error messages
#' @return The validated input
#' @keywords internal
validate_logical <- function(x, param_name = "x") {
  if (!is.logical(x)) {
    stop(paste0("Parameter '", param_name, "' must be logical (TRUE/FALSE), not ", class(x)[1]))
  }
  
  if (any(is.na(x))) {
    stop(paste0("Parameter '", param_name, "' cannot contain NA values"))
  }
  
  x
}