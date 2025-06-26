#' expand mapping
expand_mapping <- function(mapping,
                           envir = parent.frame(),
                           as_symbol = T) {
  force(envir)
  purrr::iwalk(mapping, function(.value, .name) {
    if (!is.null(.value)) {
      assign(.name, rlang::sym(.value), envir = envir)
    }
  })
}
# =================================================
#' wrapping labeleer
#' @export
label_with_wraping <- function(x, width = 15) {
  purrr::map(ggplot2::label_both(x), ~ stringr::str_wrap(.x, width))
} # ===============================================================================
#' Spread data
#' @export
spread_data <- function(DATA, key, value = NULL) {
  if (is.null(value)) {
    return(DATA)
  } else {
    tryCatch({
      DATA %>%
        dplyr::ungroup() %>%
        tidyr::pivot_wider(names_from = key, values_from = value)
    })
  }
}
# ===============================================================================
#' Wrap text label at a certain width
#' @param width
#' @export
text_wraping <- function(DATA, cols = NULL, width = 30, ...) {
  if (is.null(cols)) cols <- names(DATA)[purrr::map_lgl(DATA, ~ is.factor(.x))]
  purrr::reduce(cols, function(.out, .col) {
    .x <- .out[[.col]]
    levels(.x) <- stringr::str_wrap(levels(.x), width = width)
    .out[[.col]] <- .x
    .out
  }, .init = DATA)
}
# =================================================
# =================================================
#' @title
#' MISSING_TITLE
#' @description
#' @detail
#' @param
#' @return
#' @export
format_p_value <- function(p) {
  try(p <- round(p, digits = 4))
  pval <- base::format.pval(p,
    digits = 4, eps = 0.0001, nsmall = 4,
    scientific = FALSE
  )
  if (is.na(p) || is.null(p) || length(p) == 0) {
    return("Failed")
  }
  if (p < 0.001) {
    return(paste(pval, "(***)"))
  } else if (p < 0.01) {
    return(paste(pval, "(**)"))
  } else if (p < 0.05) {
    return(paste(p, "(*)"))
  } else {
    return(paste(p, "(n.s.)"))
  }
}
# =================================================s
`[.R6` <- function(x, ...) x$`[`(...)
`[<-.R6` <- function(x, ...) x$`[<-`(...)
length.R6 <- function(x) x$length()
format.R6 <- function(x) x$format()
as.data.frame.R6 <- function(x, ...) x$as.data.frame()
as.character.R6 <- function(x, ...) x$as.character()

# =================================================
# waRRior Replacement Functions
# =================================================

#' Split data at specific groups (compatible with waRRior::named_group_split_at)
#'
#' @param data Data frame to split
#' @param group_var Grouping variable name
#' @param at Group values to split at
#' @return Named list of data frames
#' @export
nightowl_group_split_at <- function(data, group_var, at) {
  # Group the data and split
  grouped_data <- dplyr::group_by(data, !!dplyr::sym(group_var))
  split_data <- dplyr::group_split(grouped_data, .keep = TRUE)
  
  # Get group keys to name the list
  group_keys <- dplyr::group_keys(grouped_data)
  names(split_data) <- group_keys[[group_var]]
  
  # Filter to only the requested groups if 'at' is specified
  if (!missing(at)) {
    split_data <- split_data[names(split_data) %in% at]
  }
  
  return(split_data)
}

#' Get groups from grouped data (compatible with waRRior::get_groups)
#'
#' @param data Grouped data frame
#' @return Vector of group names
#' @export
nightowl_get_groups <- function(data) {
  if (dplyr::is_grouped_df(data)) {
    group_vars <- dplyr::group_vars(data)
    return(group_vars)
  } else {
    return(character(0))
  }
}

#' Remove specified elements from vector (compatible with waRRior::pop)
#'
#' @param x Vector or list
#' @param remove Elements to remove from x
#' @return Vector x with elements in remove excluded
#' @export
nightowl_pop <- function(x, remove) {
  # Remove specified elements from x
  x[!x %in% remove]
}

#' Get function by name (compatible with waRRior::getfun)
#'
#' @param name Function name as string
#' @param package Package name (optional)
#' @return Function object
#' @export
nightowl_getfun <- function(name, package = NULL) {
  if (!is.null(package)) {
    # Try to get function from specific package
    full_name <- paste0(package, "::", name)
    return(eval(parse(text = full_name)))
  } else {
    # Search in available environments
    return(get(name, mode = "function"))
  }
}

#' Tally/count values at specified columns (compatible with waRRior::tally_at)
#'
#' @param data Data frame
#' @param cols Column names to tally
#' @return Data frame with counts
#' @export
nightowl_tally_at <- function(data, cols) {
  if (length(cols) == 1) {
    # Single column - simple count
    data %>%
      dplyr::count(!!rlang::sym(cols), name = "n")
  } else {
    # Multiple columns - count combinations
    data %>%
      dplyr::count(!!!rlang::syms(cols), name = "n")
  }
}

#' Split data into named groups (compatible with waRRior::named_group_split)
#'
#' @param data Data frame to split
#' @param ... Grouping variables
#' @return Named list of data frames
#' @export
nightowl_named_group_split <- function(data, ...) {
  # Group the data
  grouped_data <- dplyr::group_by(data, ...)
  
  # Split the data
  split_data <- dplyr::group_split(grouped_data, .keep = TRUE)
  
  # Get group keys to name the list
  group_keys <- dplyr::group_keys(grouped_data)
  
  # Create names for the split data
  if (ncol(group_keys) == 1) {
    names(split_data) <- as.character(group_keys[[1]])
  } else {
    # Multiple grouping variables - combine names
    names(split_data) <- apply(group_keys, 1, function(x) paste(x, collapse = " / "))
  }
  
  return(split_data)
}

#' Split data into named groups at specific values (compatible with waRRior::named_group_split_at)
#'
#' @param data Data frame to split
#' @param group_vars Grouping variable names
#' @param at Group values to split at (optional)
#' @param keep Logical, keep grouping variables in output
#' @param verbose Logical, print progress messages
#' @return Named list of data frames
#' @export
nightowl_named_group_split_at <- function(data, group_vars, at = NULL, keep = FALSE, verbose = FALSE) {
  # Group the data
  grouped_data <- dplyr::group_by_at(data, group_vars)
  
  # Split the data
  split_data <- dplyr::group_split(grouped_data, .keep = keep)
  
  # Get group keys to name the list
  group_keys <- dplyr::group_keys(grouped_data)
  
  # Create names for the split data
  if (ncol(group_keys) == 1) {
    names(split_data) <- as.character(group_keys[[1]])
  } else {
    # Multiple grouping variables - combine names
    names(split_data) <- apply(group_keys, 1, function(x) paste(x, collapse = " / "))
  }
  
  # Filter to only the requested groups if 'at' is specified
  if (!is.null(at)) {
    split_data <- split_data[names(split_data) %in% at]
  }
  
  if (verbose) {
    message("Split data into ", length(split_data), " groups: ", paste(names(split_data), collapse = ", "))
  }
  
  return(split_data)
}

#' Drop specified columns from data frame (compatible with waRRior::drop_columns)
#'
#' @param data Data frame
#' @param cols Column names to drop
#' @return Data frame with specified columns removed
#' @export
nightowl_drop_columns <- function(data, cols) {
  # Remove columns that exist in the data
  cols_to_drop <- cols[cols %in% names(data)]
  
  if (length(cols_to_drop) > 0) {
    data <- dplyr::select(data, -dplyr::all_of(cols_to_drop))
  }
  
  return(data)
}

#' Drop empty columns from data frame (compatible with waRRior::drop_empty_columns)
#'
#' @param data Data frame
#' @return Data frame with empty columns removed
#' @export
nightowl_drop_empty_columns <- function(data) {
  # Identify columns that are entirely NA or empty strings
  empty_cols <- sapply(data, function(x) {
    all(is.na(x) | x == "" | x == " ")
  })
  
  # Remove empty columns
  data[!empty_cols]
}

#' Count unique values in a vector (compatible with waRRior::length_unique)
#'
#' @param x Vector
#' @return Number of unique values
#' @export
nightowl_length_unique <- function(x) {
  length(unique(x))
}

#' Replace element parameter in string using regex (compatible with waRRior::regex_replace_element_parameter)
#'
#' @param string Input string
#' @param parameter Parameter name to replace
#' @param value New value for the parameter
#' @return Modified string
#' @export
nightowl_regex_replace_element_parameter <- function(string, parameter, value) {
  # Create pattern to match parameter="old_value" or parameter='old_value'
  pattern <- paste0(parameter, '\\s*=\\s*["\'][^"\']*["\']')
  replacement <- paste0(parameter, '="', value, '"')
  
  # Replace the parameter value
  stringr::str_replace(string, pattern, replacement)
}

#' Collapse top-level list elements (compatible with waRRior::collapse_top_level)
#'
#' @param x List with nested structure
#' @return Flattened list
#' @export
nightowl_collapse_top_level <- function(x) {
  # Flatten one level of list nesting
  do.call(c, x)
}

#' Factor levels in lexicographic order (compatible with waRRior::fct_lexicographic)
#'
#' @param x Factor or character vector
#' @return Factor with levels in lexicographic (alphabetical) order
#' @export
nightowl_fct_lexicographic <- function(x) {
  if (is.factor(x)) {
    # Reorder existing factor levels alphabetically
    factor(x, levels = sort(levels(x)))
  } else {
    # Convert to factor with alphabetically sorted levels
    factor(x, levels = sort(unique(x)))
  }
}
