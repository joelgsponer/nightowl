#' Plot styles
#'
#' @description
#' A style is a plot specification stored as YAML (see [DeclarativePlot] for
#' the fields). nightowl ships a set of styles in its `styles` directory;
#' `list_styles()` names them, `load_style()` reads one (by name or file
#' path) and validates it, and `styled_plot()` applies a style to data with a
#' mapping given as `...`.
#'
#' @param style A style name from `list_styles()` or the path of a YAML file.
#' @param data A data frame.
#' @param ... For `styled_plot()`, the mapping as `aesthetic = "column"` pairs,
#'   e.g. `x = "Time", y = "weight"`.
#' @param override A list merged over the style, e.g.
#'   `list(svg = list(width = 5))`.
#' @return `list_styles()` returns a character vector. `load_style()` returns
#'   the validated specification as a list. `styled_plot()` returns a
#'   [DeclarativePlot].
#' @examples
#' list_styles()
#' load_style("Boxplot")$layers[[1]]$type
#' p <- styled_plot(ChickWeight, "Boxplot", x = "Time", y = "weight", fill = "Diet")
#' class(p)
#' @name styles
NULL

#' @rdname styles
#' @export
list_styles <- function() {
  files <- list.files(system.file("styles", package = "nightowl"), pattern = "\\.ya?ml$")
  sort(tools::file_path_sans_ext(files))
}

#' @rdname styles
#' @export
load_style <- function(style) {
  check_string(style)
  path <- if (file.exists(style)) {
    style
  } else {
    system.file("styles", paste0(style, ".yaml"), package = "nightowl")
  }
  if (!nzchar(path) || !file.exists(path)) {
    cli::cli_abort(c(
      "Style {.val {style}} not found.",
      i = "Built-in styles: {.val {list_styles()}}. Or pass the path of a YAML file."
    ))
  }
  spec <- yaml::read_yaml(path, handlers = yaml_handlers())
  validate_spec(spec)
  spec
}

#' @rdname styles
#' @export
styled_plot <- function(data, style, ..., override = list()) {
  mapping <- list(...)
  spec <- load_style(style)
  if (length(override) > 0) spec <- modifyList(spec, override)
  spec$colours <- spec$colours %||% spec$colors
  spec$colors <- NULL
  rlang::exec(DeclarativePlot$new, data = data, mapping = mapping, !!!spec)
}

# YAML 1.1 treats y/n/yes/no/on/off as booleans; keep the one-letter forms as
# strings so `y:` can name an aesthetic.
yaml_handlers <- function() {
  keep <- function(x) if (tolower(x) %in% c("y", "n")) x else as.logical(x %in% c("yes", "true", "on", "TRUE", "True"))
  list("bool#yes" = keep, "bool#no" = keep)
}
