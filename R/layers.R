# Layer verbs. Every verb has the signature `function(g, mapping = list(), ...)`
# and returns `g + <layer>`. They are the vocabulary of the `type:` field in
# style files (see `nightowl_layers()`), and can be called directly.

#' Layer verbs
#'
#' @description
#' Functions that add one layer to a ggplot. They share a contract: the first
#' argument is the plot, `mapping` is a named list of column names, and the
#' remaining arguments are passed to the underlying ggplot2 layer. Because the
#' contract is uniform, layers can be listed in a style file and applied by
#' [DeclarativePlot] in order.
#'
#' * `layer_geom()` adds any layer function given by name, e.g.
#'   `"ggplot2::geom_jitter"` or `"ggdist::stat_halfeye"`.
#' * `layer_points()`, `layer_smooth()` wrap the obvious geoms.
#' * `layer_boxplot()`, `layer_violin()`, `layer_dotplot()` bin a numeric `x`
#'   into intervals first, so distributions can be drawn along a continuous
#'   axis.
#' * `layer_summary()` adds a [ggplot2::stat_summary()] (binned when `x` is
#'   numeric).
#' * `layer_traces()` connects observations sharing an `id` (spaghetti plot).
#'
#' @param g A ggplot object.
#' @param mapping Named list of column names for aesthetics of this layer.
#'   A `NULL` value un-inherits the plot-level aesthetic.
#' @param geom For `layer_geom()`, a layer function or its name; for
#'   `layer_summary()` and `layer_traces()`, the geom name passed to
#'   [ggplot2::stat_summary()] or [ggplot2::geom_line()].
#' @param dodge Dodge width; `NULL` means no dodging.
#' @param cut_f,cut_args Binning function and its arguments used when `x` is
#'   numeric.
#' @param fun,fun.data Summary functions, as in [ggplot2::stat_summary()].
#'   Strings are resolved, so `"mean_se"` or `"nightowl::mean_ci"` work.
#' @param id Column identifying a trace. Filled in from the plot-level
#'   mapping by [DeclarativePlot].
#' @param method Smoothing method, see [ggplot2::geom_smooth()].
#' @param binaxis,stackdir See [ggplot2::geom_dotplot()].
#' @param ... Passed to the ggplot2 layer.
#' @return A ggplot object.
#' @examples
#' g <- ggplot2::ggplot(ChickWeight, ggplot2::aes(Time, weight, colour = Diet))
#' g |>
#'   layer_traces(id = "Chick", alpha = 0.2) |>
#'   layer_summary(fun.data = "mean_se", geom = "line", linewidth = 1)
#' @name layers
NULL

#' @rdname layers
#' @export
layer_geom <- function(g, geom, mapping = list(), ...) {
  fn <- resolve_function(geom)
  g + fn(mapping = aes_from_list(mapping), ...)
}

#' @rdname layers
#' @export
layer_points <- function(g, mapping = list(), ...) {
  g + ggplot2::geom_point(mapping = aes_from_list(mapping), ...)
}

#' @rdname layers
#' @export
layer_boxplot <- function(g, mapping = list(), dodge = 0.75, cut_f = "cut_interval", cut_args = list(n = 5), ...) {
  layer_binned(ggplot2::geom_boxplot, g, mapping, dodge, cut_f, cut_args, ...)
}

#' @rdname layers
#' @export
layer_violin <- function(g, mapping = list(), dodge = 0.75, cut_f = "cut_interval", cut_args = list(n = 5), ...) {
  layer_binned(ggplot2::geom_violin, g, mapping, dodge, cut_f, cut_args, ...)
}

#' @rdname layers
#' @export
layer_dotplot <- function(g, mapping = list(), dodge = 0.75, binaxis = "y", stackdir = "center",
                          cut_f = "cut_interval", cut_args = list(n = 5), ...) {
  layer_binned(ggplot2::geom_dotplot, g, mapping, dodge, cut_f, cut_args,
               binaxis = binaxis, stackdir = stackdir, ...)
}

layer_binned <- function(geom, g, mapping, dodge, cut_f, cut_args, ...) {
  aes <- aes_from_list(mapping)
  x_var <- plot_variable(g, "x")
  layer_data <- NULL
  if (!is.null(x_var) && is.numeric(g$data[[x_var]])) {
    cut_f <- resolve_function(cut_f)
    layer_data <- g$data
    layer_data$.bin <- rlang::exec(cut_f, layer_data[[x_var]], !!!cut_args)
    fill_var <- plot_variable(g, "fill")
    group_aes <- if (!is.null(fill_var)) {
      ggplot2::aes(group = interaction(.data[[fill_var]], .data$.bin))
    } else {
      ggplot2::aes(group = .data$.bin)
    }
    aes[names(group_aes)] <- group_aes
  }
  if (!is.null(layer_data)) {
    position <- if (is.null(dodge)) "identity" else ggplot2::position_dodge2(preserve = "single")
    return(g + geom(data = layer_data, mapping = aes, position = position, orientation = "x", ...))
  }
  position <- if (is.null(dodge)) "identity" else ggplot2::position_dodge(dodge, preserve = "total")
  g + geom(data = layer_data, mapping = aes, position = position, ...)
}

#' @rdname layers
#' @export
layer_summary <- function(g, mapping = list(), fun = NULL, fun.data = NULL, geom = "pointrange",
                          dodge = 0.75, ...) {
  if (!is.null(fun) && !is.null(fun.data)) {
    cli::cli_abort("Specify either {.arg fun} or {.arg fun.data}, not both.")
  }
  if (is.null(fun) && is.null(fun.data)) fun.data <- "mean_se"
  if (!is.null(fun.data)) fun.data <- resolve_function(fun.data)
  if (!is.null(fun)) fun <- resolve_function(fun)
  x_var <- plot_variable(g, "x")
  numeric_x <- !is.null(x_var) && is.numeric(g$data[[x_var]])
  stat <- if (numeric_x) ggplot2::stat_summary_bin else ggplot2::stat_summary
  # dodging only makes sense on a discrete x axis
  position <- if (is.null(dodge) || numeric_x) "identity" else ggplot2::position_dodge(dodge, preserve = "total")
  aes <- aes_from_list(mapping)
  if (geom %in% c("line", "path", "step") && is.null(aes$group)) {
    group_var <- group_variable(g, mapping)
    aes$group <- if (is.null(group_var)) rlang::quo(1) else rlang::quo(.data[[group_var]])
  }
  g + stat(mapping = aes, geom = geom, fun = fun, fun.data = fun.data, position = position, ...)
}

#' @rdname layers
#' @export
layer_smooth <- function(g, mapping = list(), method = "lm", ...) {
  g + ggplot2::geom_smooth(mapping = aes_from_list(mapping), method = method, ...)
}

#' @rdname layers
#' @export
layer_traces <- function(g, mapping = list(), id = NULL, geom = "line", ...) {
  if (is.null(id)) {
    cli::cli_abort("{.fn layer_traces} needs an {.arg id} column, either directly or through the plot mapping.")
  }
  aes <- aes_from_list(mapping)
  aes$group <- rlang::quo(.data[[id]])
  fn <- resolve_function(paste0("geom_", geom))
  g + fn(mapping = aes, ...)
}

#' Column name behind a plot-level aesthetic, or NULL
#' @noRd
plot_variable <- function(g, aesthetic) {
  q <- g$mapping[[aesthetic]]
  if (is.null(q)) {
    return(NULL)
  }
  nm <- rlang::as_label(q)
  if (nm %in% names(g$data)) nm else NULL
}

#' Variable that should group connected geoms: the layer's own colour, fill or
#' group, else the plot's, respecting explicit un-inheritance (`colour = NULL`).
#' @noRd
group_variable <- function(g, mapping) {
  for (key in c("group", "colour", "color", "fill")) {
    if (key %in% names(mapping)) {
      if (!is.null(mapping[[key]])) return(mapping[[key]])
      next
    }
    aesthetic <- if (key == "color") "colour" else key
    var <- plot_variable(g, aesthetic)
    if (!is.null(var)) return(var)
  }
  NULL
}
