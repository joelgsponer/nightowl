# The layer registry maps the `type:` vocabulary of style files to layer verbs,
# and validates plot specifications before anything is evaluated.

layer_registry <- function() {
  list(
    geom = layer_geom,
    generic = layer_geom,
    points = layer_points,
    boxplot = layer_boxplot,
    violin = layer_violin,
    dotplot = layer_dotplot,
    summary = layer_summary,
    smooth = layer_smooth,
    traces = layer_traces
  )
}

#' Available layer types
#'
#' The names accepted in the `type` field of a layer specification or style
#' file, each mapping to a `layer_*()` function.
#'
#' @return A character vector.
#' @examples
#' nightowl_layers()
#' @export
nightowl_layers <- function() {
  names(layer_registry())
}

#' Resolve a layer type to a function
#' @noRd
resolve_layer <- function(type, call = rlang::caller_env()) {
  if (is.function(type)) {
    return(type)
  }
  registry <- layer_registry()
  if (!rlang::is_string(type) || !type %in% names(registry)) {
    cli::cli_abort(c(
      "Unknown layer type {.val {type}}.",
      i = "Available types: {.val {names(registry)}}."
    ), call = call)
  }
  registry[[type]]
}

spec_fields <- function() {
  c("name", "description", "transform", "layers", "scales", "facets", "axis",
    "colours", "colors", "theming", "annotation", "dodge", "svg", "resize", "class")
}

mapping_fields <- function() {
  c("x", "y", "group", "fill", "colour", "color", "size", "shape", "linetype", "lty",
    "alpha", "label", "id", "xmin", "xmax", "ymin", "ymax", "lower", "middle", "upper",
    "weight", "facet_row", "facet_col")
}

svg_fields <- function() {
  c("width", "height", "scaling", "bg", "font_family", "web_fonts", "download_button",
    "element_width", "element_height", "filename")
}

#' Validate a plot specification
#'
#' Checks the structure of a plot specification (the parsed content of a style
#' file, or the arguments of [DeclarativePlot]) before it is used: unknown
#' fields, unknown mapping keys, layers without a resolvable `type`, scales
#' without a `scale`, and unknown `svg` options all raise an error naming the
#' offending key.
#'
#' @param spec A named list.
#' @param mapping Optional plot-level mapping to validate alongside.
#' @return `spec`, invisibly.
#' @examples
#' validate_spec(load_style("Boxplot"))
#' try(validate_spec(list(layers = list(list(type = "boxplt")))))
#' @export
validate_spec <- function(spec, mapping = NULL) {
  call <- rlang::current_env()
  if (!is.list(spec)) {
    cli::cli_abort("A plot specification must be a list, not {.obj_type_friendly {spec}}.")
  }
  check_keys(names(spec), spec_fields(), what = "style field", call = call)
  if (!is.null(mapping)) check_mapping(mapping, where = "mapping", call = call)
  if (!is.null(spec$transform)) {
    if (!is.list(spec$transform)) cli::cli_abort("{.field transform} must be a named list.", call = call)
    check_keys(names(spec$transform), mapping_fields(), what = "transform key", call = call)
  }
  layers <- spec$layers %||% list()
  if (!is.list(layers)) cli::cli_abort("{.field layers} must be a list of layer specifications.", call = call)
  for (i in seq_along(layers)) {
    layer <- layers[[i]]
    if (!is.list(layer) || is.null(layer$type)) {
      cli::cli_abort("Layer {i} has no {.field type}.", call = call)
    }
    resolve_layer(layer$type, call = call)
    if (!is.null(layer$mapping)) check_mapping(layer$mapping, where = glue::glue("layer {i} mapping"), call = call)
  }
  scales <- spec$scales %||% list()
  for (i in seq_along(scales)) {
    if (!is.list(scales[[i]]) || is.null(scales[[i]]$scale)) {
      cli::cli_abort("Scale {i} has no {.field scale}.", call = call)
    }
  }
  if (!is.null(spec$svg)) {
    if (!is.list(spec$svg)) cli::cli_abort("{.field svg} must be a list.", call = call)
    check_keys(names(spec$svg), svg_fields(), what = "svg option", call = call)
  }
  if (!is.null(spec$facets) && !is.list(spec$facets)) {
    cli::cli_abort("{.field facets} must be a list.", call = call)
  }
  invisible(spec)
}

check_mapping <- function(mapping, where = "mapping", call = rlang::caller_env()) {
  if (!is.list(mapping)) {
    cli::cli_abort("{.field {where}} must be a named list.", call = call)
  }
  check_keys(names(mapping), mapping_fields(), what = glue::glue("{where} key"), call = call)
  bad <- names(mapping)[!vapply(mapping, function(v) is.null(v) || rlang::is_string(v), logical(1))]
  if (length(bad) > 0) {
    cli::cli_abort("{.field {where}}: {cli::qty(length(bad))}entr{?y/ies} {.val {bad}} must be column names (single strings) or NULL.", call = call)
  }
  invisible(mapping)
}

check_keys <- function(keys, allowed, what, call = rlang::caller_env()) {
  if (length(keys) > 0 && (is.null(keys) || any(!nzchar(keys)))) {
    cli::cli_abort("Every {what} must be named.", call = call)
  }
  bad <- setdiff(keys, allowed)
  if (length(bad) > 0) {
    cli::cli_abort(c(
      "{cli::qty(length(bad))}Unknown {what}{?s}: {.val {bad}}.",
      i = "Allowed: {.val {allowed}}."
    ), call = call)
  }
  invisible(keys)
}

#' Build a ggplot2 mapping from a named list of column names
#'
#' Entries that are `NULL` become `aes(key = NULL)`, which un-inherits the
#' plot-level aesthetic; nightowl-only keys (`id`, facets) are dropped.
#' @noRd
aes_from_list <- function(mapping, keep_null = TRUE) {
  mapping <- mapping[setdiff(names(mapping), c("id", "facet_row", "facet_col"))]
  names(mapping)[names(mapping) == "color"] <- "colour"
  names(mapping)[names(mapping) == "lty"] <- "linetype"
  if (!keep_null) mapping <- purrr::compact(mapping)
  args <- lapply(mapping, function(v) if (is.null(v)) NULL else rlang::sym(v))
  args <- lapply(names(args), function(k) if (is.null(args[[k]])) rlang::expr(NULL) else args[[k]])
  names(args) <- names(mapping)
  rlang::inject(ggplot2::aes(!!!args))
}
