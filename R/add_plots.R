#' generic
#' @export
generic <- function(geom,
                    g,
                    mapping = list(),
                    ...) {
  .aes <- do.call(ggplot2::aes, mapping)
  .f <- eval(parse(text = geom))
  g + .f(mapping = .aes, ...)
}
#' Add generic
#' @export
add_geom <- function(geom,
                     g,
                     mapping = list(
                       lty = NULL,
                       color = NULL,
                       group = NULL
                     ),
                     dodge = 0.5,
                     color = "black",
                     cut_f = ggplot2::cut_interval,
                     cut_args = list(n = 5),
                     ...) {
  .data <- g$data
  nightowl:::expand_mapping(mapping)
  if (is.numeric(dplyr::pull(g$data, !!g$mapping$x))) {
    if (is.character(cut_f)) cut_f <- eval(parse(text = cut_f))
    if (!is.null(g$mapping$fill)) {
      peacock::log("cutting")
      .group <- do.call(cut_f, c(
        list(x = g$data[[rlang::as_label(g$mapping$x)]]),
        cut_args
      ))
      .data <- cbind(g$data, .group)
      .aes <- ggplot2::aes(group = interaction(!!g$mapping$fill, .group))
    } else {
      .aes <- ggplot2::aes(group = cut_f(!!g$mapping$x, cut_n))
    }
    # .aes <- do.call(ggplot2::aes, mapping)
  } else {
    .aes <- do.call(ggplot2::aes, mapping)
  }

  g + geom(
    data = .data,
    mapping = .aes,
    position = ggplot2::position_dodge(dodge, preserve = "total"),
    ...
  )
}
# ===============================================================================
#' Add Violin
#' @export
violin <- function(...) {
  nightowl::add_geom(ggplot2::geom_violin, ...)
}
# ===============================================================================
#' Add Boxplot
#' @export
boxplot <- function(...) {
  nightowl::add_geom(ggplot2::geom_boxplot, ...)
}
# ===============================================================================
#' Add dotplot
#' @export
dotplot <- function(binaxis = "y",
                    stackdir = "center",
                    ...) {
  nightowl::add_geom(
    ggplot2::geom_dotplot,
    binaxis = binaxis,
    stackdir = stackdir,
    ...
  )
}
# ===============================================================================
#' Add points
#' @export
points <- function(g, mapping = list(), ...) {
  .aes <- do.call(ggplot2::aes, mapping)
  g + ggplot2::geom_point(mapping = .aes, ...)
}
# ===============================================================================
#' Add summary
#' @export
summary <- function(g,
                    mapping = list(),
                    fun.data = NULL,
                    fun = NULL,
                    dodge = 1,
                    shape = 21,
                    ...) {
  if (!is.null(fun.data) && !is.null(fun)) {
    rlang::abort("Error in summary: Please specify either fun or fun.data, not both")
  }
  if (is.null(fun.data) && is.null(fun)) {
    method <- "mean_se"
  }
  if (!is.null(fun.data)) method <- fun.data
  if (!is.null(fun)) method <- fun
  attributes(g)$caption <- c(
    attributes(g)$caption, glue::glue("Method for summary: '{method}'")
  )
  if (is.numeric(dplyr::pull(g$data, !!g$mapping$x))) {
    .f <- ggplot2::stat_summary_bin
  } else {
    .f <- ggplot2::stat_summary
  }
  .aes <- do.call(ggplot2::aes, mapping)
  g + .f(mapping = .aes, position = ggplot2::position_dodge2(dodge, preserve = "total"), shape = shape, fun = fun, fun.data = fun.data, ...)
}
# ===============================================================================
#' Add smooth
#' @export
smooth <- function(g,
                   mapping = list(),
                   dodge = 0,
                   method = "lm",
                   color = "black",
                   ...) {
  attributes(g)$caption <- c(
    attributes(g)$caption,
    glue::glue("Method for summary: '{method}'")
  )
  .aes <- do.call(ggplot2::aes, mapping)
  g + ggplot2::geom_smooth(
    mapping = .aes,
    position = ggplot2::position_dodge(dodge, preserve = "total"),
    ...
  )
}
# ===============================================================================
#' Add facet_grid
#' @export
facets <- function(g,
                   type = "grid",
                   column = NULL,
                   row = NULL,
                   scales = "free",
                   labeller = "label_both",
                   ...) {
  if (!is.null(column) | !is.null(row)) {
    if (is.null(column)) column <- "."
    if (is.null(row)) row <- "."

    .f <- switch(type,
      grid = ggplot2::facet_grid,
      wrap = ggplot2::facet_wrap,
      NULL
    )
    if (is.null(.f)) {
      rlang::abort(
        glue::glue(
          "Error in facetting: type {type} is not valid. Use 'grid' or 'wrap'"
        )
      )
    }

    g + .f(
      as.formula(
        paste(
          paste0("`", row, "`", collapse = "+"),
          "~",
          paste0("`", column, "`", collapse = "+")
        )
      ),
      scales = scales,
      labeller = match.fun(labeller),
      ...
    )
  } else {
    return(g)
  }
}
# ===============================================================================
#' Add traces
#' @export
traces <- function(g,
                   mapping = list(),
                   dodge = 0,
                   fun.data = NULL,
                   fun = NULL,
                   shape = 21,
                   ...) {
  if (!is.null(fun.data) && !is.null(fun)) {
    rlang::abort("Error in summary: Please specify either fun or fun.data, not both")
  }
  if (is.null(fun.data) && is.null(fun)) {
    method <- "mean_se"
  }
  if (!is.null(fun.data)) method <- fun.data
  if (!is.null(fun)) method <- fun
  attributes(g)$caption <- c(
    attributes(g)$caption, glue::glue("Method for summary: '{method}'")
  )
  .aes <- nightowl::aes(mapping)
  g + ggplot2::stat_summary(
    mapping = .aes,
    position = ggplot2::position_dodge(dodge, preserve = "total"),
    shape = shape,
    ...
  )
}
# ===============================================================================
