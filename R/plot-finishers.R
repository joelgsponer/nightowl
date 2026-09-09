# Internal steps applied by DeclarativePlot after the layers: scales, facets,
# axis, colours, theme and annotation. Each returns the modified ggplot.

apply_scales <- function(g, scales) {
  for (spec in scales %||% list()) {
    fn <- resolve_function(spec$scale)
    args <- purrr::compact(spec[setdiff(names(spec), "scale")])
    g <- g + rlang::exec(fn, !!!args)
  }
  g
}

apply_facets <- function(g, facets) {
  if (is.null(facets) || (is.null(facets$row) && is.null(facets$column))) {
    return(g)
  }
  type <- facets$type %||% "grid"
  scales <- facets$scales %||% "free"
  labeller <- label_both_wrapped(facets$label_width %||% 15)
  extra <- facets[setdiff(names(facets), c("type", "row", "column", "scales", "label_width"))]
  if (type == "wrap") {
    vars <- c(facets$row, facets$column)
    fn <- ggplot2::facet_wrap
    return(g + rlang::exec(fn, facets = ggplot2::vars(!!!rlang::syms(vars)), scales = scales,
                           labeller = labeller, !!!extra))
  }
  if (type != "grid") {
    cli::cli_abort("Facet {.field type} must be {.val grid} or {.val wrap}, not {.val {type}}.")
  }
  rows <- if (is.null(facets$row)) NULL else ggplot2::vars(!!!rlang::syms(facets$row))
  cols <- if (is.null(facets$column)) NULL else ggplot2::vars(!!!rlang::syms(facets$column))
  g + rlang::exec(ggplot2::facet_grid, rows = rows, cols = cols, scales = scales, labeller = labeller, !!!extra)
}

apply_axis <- function(g, axis, mapping = list()) {
  axis <- axis %||% list()
  captions <- character(0)
  xlab <- mapping$x
  ylab <- mapping$y
  if (!is.null(axis$units_x) && !is.null(xlab)) xlab <- glue::glue("{xlab} ({axis$units_x})")
  if (!is.null(axis$units_y) && !is.null(ylab)) ylab <- glue::glue("{ylab} ({axis$units_y})")
  if (isTRUE(axis$log_x)) {
    g <- g + ggplot2::scale_x_log10()
    if (!is.null(xlab)) xlab <- glue::glue("log10({xlab})")
  }
  if (isTRUE(axis$log_y)) {
    g <- g + ggplot2::scale_y_log10()
    if (!is.null(ylab)) ylab <- glue::glue("log10({ylab})")
  }
  if (!is.null(xlab)) g <- g + ggplot2::xlab(as.character(xlab))
  if (!is.null(ylab)) g <- g + ggplot2::ylab(as.character(ylab))
  if (!is.null(axis$xlim) || !is.null(axis$ylim)) {
    g <- g + ggplot2::coord_cartesian(xlim = axis$xlim, ylim = axis$ylim)
    if (!is.null(axis$xlim)) captions <- c(captions, glue::glue("Zoom on x axis: {paste(axis$xlim, collapse = ' to ')}"))
    if (!is.null(axis$ylim)) captions <- c(captions, glue::glue("Zoom on y axis: {paste(axis$ylim, collapse = ' to ')}"))
  }
  attr(g, "nightowl_captions") <- captions
  g
}

apply_colours <- function(g, data, mapping, colours) {
  colours <- colours %||% list()
  palette <- colours$palette %||% nightowl_option("palette")
  max_levels <- colours$max_levels %||% 12
  captions <- character(0)
  for (aesthetic in c("fill", "colour")) {
    var <- mapping[[aesthetic]] %||% if (aesthetic == "colour") mapping$color else NULL
    if (is.null(var) || !var %in% names(data)) next
    values <- data[[var]]
    if (is.numeric(values)) next
    n <- length(unique(values))
    if (n <= max_levels) {
      scale <- if (aesthetic == "fill") scale_fill_nightowl(palette) else scale_colour_nightowl(palette)
      g <- g + scale
    } else {
      g <- g + ggplot2::guides(!!aesthetic := "none")
      captions <- c(captions, glue::glue("Legend for {var} not shown ({n} levels)"))
    }
  }
  attr(g, "nightowl_captions") <- captions
  g
}

apply_theme <- function(g, theming) {
  theming <- theming %||% list()
  theme_fn <- resolve_function(theming$theme %||% theme_nightowl)
  g <- g + theme_fn()
  elements <- theming[setdiff(names(theming), "theme")]
  if (length(elements) > 0) {
    built <- purrr::imap(elements, function(spec, name) {
      if (!is.list(spec)) {
        return(spec)
      }
      element <- resolve_function(spec$element %||% cli::cli_abort("Theme entry {.field {name}} needs an {.field element}."))
      rlang::exec(element, !!!spec[setdiff(names(spec), "element")])
    })
    g <- g + rlang::exec(ggplot2::theme, !!!built)
  }
  g
}

apply_annotation <- function(g, mapping, annotation, captions = character(0)) {
  a <- annotation %||% list()
  wrap_x <- a$wrap_x %||% 40
  wrap_y <- a$wrap_y %||% 40
  wrap_title <- a$wrap_title %||% 70
  wrap_legend <- a$wrap_legend %||% 30
  if (!is.null(a$axis_text_x_angle)) {
    g <- g + ggplot2::theme(axis.text.x = ggplot2::element_text(
      angle = a$axis_text_x_angle, hjust = a$axis_text_x_hjust %||% 1, vjust = a$axis_text_x_vjust %||% 1
    ))
  }
  if (!is.null(a$legend_position)) g <- g + ggplot2::theme(legend.position = a$legend_position)
  current <- plot_labels(g)
  labs <- list()
  for (key in setdiff(names(current), c("title", "subtitle", "caption", "x", "y"))) {
    if (is.character(current[[key]])) labs[[key]] <- wrap_text(current[[key]], wrap_legend)
  }
  x_label <- a$xlab %||% current$x %||% mapping$x
  y_label <- a$ylab %||% current$y %||% mapping$y
  if (is.character(x_label)) labs$x <- wrap_text(x_label, wrap_x)
  if (is.character(y_label)) labs$y <- wrap_text(y_label, wrap_y)
  title <- a$title
  if (is.null(title) && !is.null(mapping$x) && !is.null(mapping$y)) {
    title <- glue::glue("{mapping$y} vs. {mapping$x}")
  }
  if (!isFALSE(title) && !is.null(title)) labs$title <- wrap_text(title, wrap_title)
  if (!is.null(a$subtitle)) labs$subtitle <- a$subtitle
  captions <- unique(c(a$caption, captions))
  if (length(captions) > 0) labs$caption <- paste(captions, collapse = "\n")
  g + rlang::exec(ggplot2::labs, !!!labs)
}

take_captions <- function(g) {
  cap <- attr(g, "nightowl_captions") %||% character(0)
  attr(g, "nightowl_captions") <- NULL
  list(plot = g, captions = cap)
}

#' Current labels of a ggplot as a plain named list (works across ggplot2 3.x and 4.x)
#' @noRd
plot_labels <- function(g) {
  labs <- tryCatch(g$labels, error = function(e) NULL)
  labs <- as.list(labs %||% list())
  labs[vapply(labs, function(v) is.character(v) || is.null(v), logical(1))]
}
