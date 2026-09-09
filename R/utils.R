# Internal helpers shared across the package. Nothing here is exported.

#' Drop values from a vector
#' @noRd
pop <- function(x, remove) {
  x[!x %in% remove]
}

#' Resolve a function given as a function or a string
#'
#' Accepts a function object, a `"pkg::fun"` string, or a bare name looked up
#' in ggplot2 and then base R. Never evaluates arbitrary code.
#' @noRd
resolve_function <- function(x, arg = rlang::caller_arg(x), call = rlang::caller_env()) {
  if (is.function(x)) {
    return(x)
  }
  if (!rlang::is_string(x)) {
    cli::cli_abort("{.arg {arg}} must be a function or a string naming one, not {.obj_type_friendly {x}}.", call = call)
  }
  if (grepl("::", x, fixed = TRUE)) {
    parts <- strsplit(x, ":::?", fixed = FALSE)[[1]]
    pkg <- parts[[1]]
    fun <- parts[[2]]
    rlang::check_installed(pkg, reason = glue::glue("to use `{x}`"), call = call)
    ns <- asNamespace(pkg)
    if (!exists(fun, envir = ns, inherits = FALSE)) {
      cli::cli_abort("Function {.fn {x}} not found in package {.pkg {pkg}}.", call = call)
    }
    return(get(fun, envir = ns, inherits = FALSE))
  }
  for (ns in list(asNamespace("ggplot2"), baseenv(), asNamespace("stats"))) {
    if (exists(x, envir = ns, inherits = FALSE, mode = "function")) {
      return(get(x, envir = ns, inherits = FALSE, mode = "function"))
    }
  }
  cli::cli_abort(c(
    "Cannot resolve {.val {x}} to a function.",
    i = "Use a namespaced name such as {.val ggplot2::geom_point}."
  ), call = call)
}

#' Split a data frame by grouping variables into a named list
#'
#' Names are `var==level` joined by `sep`. The group keys are kept as the
#' `"keys"` attribute so callers do not need to parse the names.
#' @noRd
named_group_split <- function(data, vars = dplyr::group_vars(data), keep = TRUE, sep = " / ") {
  data <- dplyr::ungroup(data)
  if (length(vars) == 0) {
    res <- list(Overall = data)
    attr(res, "keys") <- tibble::tibble(.rows = 1)
    return(res)
  }
  check_columns(data, vars)
  grouped <- dplyr::group_by(data, dplyr::across(dplyr::all_of(vars)))
  keys <- dplyr::group_keys(grouped)
  pieces <- dplyr::group_split(grouped, .keep = keep)
  nms <- purrr::pmap_chr(keys, function(...) {
    vals <- list(...)
    paste(paste0(names(vals), "==", vapply(vals, as.character, character(1))), collapse = sep)
  })
  res <- setNames(as.list(pieces), nms)
  attr(res, "keys") <- keys
  res
}

#' Convert to factor with an explicit level for missing values
#' @noRd
explicit_missing <- function(x, level = "(Missing)") {
  if (!is.factor(x)) x <- factor(x)
  if (!anyNA(x)) {
    return(x)
  }
  forcats::fct_na_value_to_level(x, level = level)
}

#' Wrap text and replace newlines
#' @noRd
wrap_text <- function(x, width = 30) {
  x <- stringr::str_replace_all(x, "\n", " ")
  stringr::str_wrap(x, width = width)
}

#' Wrap factor levels of every factor column (or `cols`)
#' @noRd
wrap_levels <- function(data, cols = NULL, width = 30) {
  if (is.null(cols)) cols <- names(data)[vapply(data, is.factor, logical(1))]
  for (col in cols) {
    levels(data[[col]]) <- stringr::str_wrap(levels(data[[col]]), width = width)
  }
  data
}

#' ggplot2 labeller that shows `variable: value` wrapped
#' @noRd
label_both_wrapped <- function(width = 15) {
  function(labels) {
    purrr::map(ggplot2::label_both(labels), ~ stringr::str_wrap(.x, width))
  }
}

capitalize <- function(x) {
  paste0(toupper(substring(x, 1, 1)), substring(x, 2))
}

#' Replace variable names by labels where a label exists
#' @noRd
apply_labels <- function(x, labels) {
  if (is.null(x) || is.null(labels)) {
    return(x)
  }
  labels <- unlist(labels)
  hit <- x %in% names(labels)
  x[hit] <- unname(labels[x[hit]])
  x
}

#' Abort unless all `cols` are columns of `data`
#' @noRd
check_columns <- function(data, cols, arg = "data", call = rlang::caller_env()) {
  cols <- unique(unlist(cols))
  missing <- setdiff(cols, names(data))
  if (length(missing) > 0) {
    cli::cli_abort("{cli::qty(length(missing))}Column{?s} {.val {missing}} not present in {.arg {arg}}.", call = call)
  }
  invisible(TRUE)
}

check_data_frame <- function(x, arg = rlang::caller_arg(x), call = rlang::caller_env()) {
  if (!is.data.frame(x)) {
    cli::cli_abort("{.arg {arg}} must be a data frame, not {.obj_type_friendly {x}}.", call = call)
  }
  invisible(TRUE)
}

check_string <- function(x, arg = rlang::caller_arg(x), allow_null = FALSE, call = rlang::caller_env()) {
  if (is.null(x) && allow_null) {
    return(invisible(TRUE))
  }
  if (!rlang::is_string(x)) {
    cli::cli_abort("{.arg {arg}} must be a single string, not {.obj_type_friendly {x}}.", call = call)
  }
  invisible(TRUE)
}
