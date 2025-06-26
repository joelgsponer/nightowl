#' generic
#' @export
generic <- function(geom,
                    g,
                    mapping = list(),
                    ...) {
  .aes <- do.call(ggplot2::aes, mapping)
  .f <- nightowl_getfun(geom)
  g + .f(mapping = .aes, ...)
}
#' generic
#' @export
scales <- function(scale,
                   g,
                   ...) {
  .f <- nightowl_getfun(scale)
  g + do.call(.f, list(...))
}
# ===============================================================================
#' Add generic
#' @export
add_geom <- function(geom,
                     g,
                     mapping = list(),
                     dodge = 0.5,
                     color = "black",
                     cut_f = ggplot2::cut_interval,
                     cut_args = list(n = 5),
                     ...) {
  .data <- g$data
  nightowl:::expand_mapping(mapping)
  if (is.numeric(dplyr::pull(g$data, !!g$mapping$x))) {
    if (is.character(cut_f)) cut_f <- eval(parse(text = cut_f))
    .group <- do.call(cut_f, c(
      list(x = g$data[[rlang::as_label(g$mapping$x)]]),
      cut_args
    ))
    .data <- cbind(g$data, .group)
    if (!is.null(g$mapping$fill)) {
      .aes <- ggplot2::aes(group = interaction(!!g$mapping$fill, .group))
    } else {
      .aes <- ggplot2::aes(group = .group)
    }
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
#' @title Add Boxplot Geometry
#' @description Adds boxplot geometry to nightowl plots with position dodging
#' @param ... Additional arguments passed to add_geom and geom_boxplot
#' @return Modified ggplot object with boxplot layer
#' @export
nw_boxplot <- function(...) {
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
#' @title Add Points Geometry
#' @description Adds point geometry to nightowl plots with custom aesthetic mapping
#' @param g ggplot object to add points to
#' @param mapping Aesthetic mapping list for points
#' @param ... Additional arguments passed to geom_point
#' @return Modified ggplot object with points layer
#' @export
nw_points <- function(g, mapping = list(), ...) {
  .aes <- do.call(ggplot2::aes, mapping)
  g + ggplot2::geom_point(mapping = .aes, ...)
}
# ===============================================================================
#' @title Add Summary Statistics Layer
#' @description Adds statistical summary layer to nightowl plots with customizable functions
#' @param g ggplot object to add summary to
#' @param mapping Aesthetic mapping list for summary statistics
#' @param fun.data Function for computing summary data
#' @param fun Function for computing single summary statistic
#' @param dodge Position dodge width
#' @param shape Point shape for summary statistics
#' @param ... Additional arguments passed to stat_summary
#' @return Modified ggplot object with summary statistics layer
#' @export
nw_summary <- function(g,
                    mapping = list(),
                    fun.data = NULL,
                    fun = NULL,
                    dodge = 1,
                    shape = 21,
                    ...) {
  nightowl_progress_step("summary.gg")
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
  g + .f(mapping = .aes, position = ggplot2::position_dodge(dodge, preserve = "total"), shape = shape, fun = fun, fun.data = fun.data, ...)
}
# ===============================================================================
#' @title Add Smooth Trend Line
#' @description Adds smooth trend line to nightowl plots with customizable methods
#' @param g ggplot object to add smooth line to
#' @param mapping Aesthetic mapping list for smooth line
#' @param dodge Position dodge width
#' @param method Smoothing method (default: "lm")
#' @param color Line color
#' @param ... Additional arguments passed to geom_smooth
#' @return Modified ggplot object with smooth trend line
#' @export
nw_smooth <- function(g,
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
                   labeller = "label_with_wraping",
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
  .aes <- ggplot2::aes(group = !!g$mapping$id, lty = NULL)
  g + ggplot2::stat_summary(
    mapping = .aes,
    position = ggplot2::position_dodge(0, preserve = "total"),
    shape = shape,
    ...
  )
}
# ===============================================================================
#' @title Apply Axis Transformations
#' @description Applies axis transformations including log scaling and limits to nightowl plots
#' @param g ggplot object to modify axes for
#' @param log_x Logical indicating whether to apply log10 transformation to x-axis
#' @param log_y Logical indicating whether to apply log10 transformation to y-axis
#' @param xlim Numeric vector of x-axis limits
#' @param ylim Numeric vector of y-axis limits
#' @param units_x Units label for x-axis
#' @param units_y Units label for y-axis
#' @param ... Additional arguments
#' @return Modified ggplot object with axis transformations
#' @export
nw_axis <- function(g,
                 log_x = F,
                 log_y = F,
                 xlim = NULL,
                 ylim = NULL,
                 units_x = NULL,
                 units_y = NULL,
                 ...) {
  if (!is.null(units_x)) {
    g$labels$x <- paste0(g$labels$x, "(", units_x, ")")
  }
  if (!is.null(units_y)) {
    g$labels$y <- paste0(g$labels$y, "(", units_y, ")")
  }
  if (log_x) {
    g <- g + ggplot2::scale_x_log10()
    g$labels$x <- glue::glue("log10({g$labels$x})")
  }
  if (log_y) {
    g <- g + ggplot2::scale_y_log10()
    g$labels$y <- glue::glue("log10({g$labels$y})")
  }
  if (!is.null(xlim)) {
    g <- g + ggplot2::coord_cartesian(xlim = xlim)
    attributes(g)$caption <- c(
      attributes(g)$caption,
      glue::glue("Zoom on X Axis: {paste0(xlim, units_x, collapse = ' - ')}")
    )
  }
  if (!is.null(ylim)) {
    g <- g + ggplot2::coord_cartesian(ylim = ylim)
    attributes(g)$caption <- c(
      attributes(g)$caption,
      glue::glue("Zoom on Y Axis: {paste0(ylim, units_y, collapse = ' - ')}")
    )
  }
  return(g)
}
# ===============================================================================
#' Apply annotation
#' @export
annotation <- function(g,
                       x,
                       y,
                       title = NULL,
                       xlab = NULL,
                       ylab = NULL,
                       axis_text_x_angle = 45,
                       axis_text_x_hjust = 1,
                       axis_text_x_vjust = 1,
                       legend_position = "bottom",
                       test = NULL,
                       test_args = NULL,
                       wrap_title = 60,
                       wrap_x = 30,
                       wrap_y = 30,
                       wrap_guides = 30,
                       ...) {
  g <- g + ggplot2::theme(
    axis.text.x = ggplot2::element_text(
      angle = axis_text_x_angle,
      hjust = axis_text_x_hjust,
      vjust = axis_text_x_vjust,
    ),
    legend.position = legend_position
  )
  ## Guides
  g$labels <- purrr::imap(g$labels, function(x, y) {
    if (!y %in% c("title", "caption")) {
      stringr::str_replace_all(x, "\n", " ") %>%
        stringr::str_wrap(wrap_guides)
    } else {
      return(x)
    }
  })
  ## X label
  if (!is.null(xlab)) {
    g <- g + ggplot2::xlab(xlab)
  } else {
    xlab <- stringr::str_replace_all(g$labels$x, "\n", " ")
    xlab <- stringr::str_wrap(xlab, wrap_x)
    g <- g + ggplot2::xlab(xlab)
  }
  ## Y label
  if (!is.null(ylab)) {
    g <- g + ggplot2::ylab(ylab)
  } else {
    ylab <- stringr::str_replace_all(g$labels$y, "\n", " ")
    ylab <- stringr::str_wrap(ylab, wrap_y)
    g <- g + ggplot2::ylab(ylab)
  }
  ## Title
  if (!is.null(title)) {
    g <- g + ggplot2::ggtitle(title)
  } else {
    auto_title <- stringr::str_wrap(glue::glue("{x} vs. {y}"), width = wrap_title)
    g <- g + ggplot2::ggtitle(auto_title)
  }
  if (!is.null(attributes(g)$caption)) {
    g <- g + ggplot2::labs(
      caption = paste(unique(attributes(g)$caption), collapse = "\n")
    )
  }
  if (!is.null(test)) {
    .formula.txt <- glue::glue("`{y}` ~ `{x}`")
    .formula <- as.formula(.formula.txt)
    # .model <- waRRior::getfun(test)
    res <- do.call(test, c(list(formula = .formula, data = g$data), test_args)) %>%
      broom::tidy() %>%
      dplyr::mutate(p.value = nightowl::format_p_value(p.value)) %>%
      dplyr::mutate_if(is.numeric, function(x) round(x, 2))
    xmin <- min(as.numeric(g$data[[as.character(g$mapping$x)[2]]]), na.rm = T)
    ymax <- max(as.numeric(g$data[[as.character(g$mapping$y)[2]]]), na.rm = T)

    g <- g + ggplot2::annotate(
      geom = "table",
      y = ymax,
      x = xmin,
      label = list(res)
    )
  }
  return(g)
}
# =================================================
#' Create aes from list
#' Setup ggplot
#' This was difficult, fist store parameters in list,
#' Convert to symbols
#' drop the onses which are null, call aes_ function (CAVE: ecex)
#' also think of other places where params is used, e.g. params$id
#' @export
aes <- function(aes) {
  aes <- aes %>%
    purrr::compact() %>%
    purrr::map(~ rlang::sym(.x))
  f <- ggplot2::aes_
  rlang::exec(.fn = "f", !!!aes)
}
# ===============================================================================
#' @title Define Color Scales
#' @description Defines color and fill scales for nightowl plots with automatic legend handling
#' @param g ggplot object to apply color scales to
#' @param DATA Data frame containing the variables
#' @param mapping Aesthetic mapping list containing color/fill variables
#' @param ... Additional arguments
#' @return Modified ggplot object with color scales applied
#' @export
nw_colors <- function(g, DATA, mapping, ...) {
  fill <- mapping$fill
  color <- mapping$color
  if (!is.null(fill) && is.factor(DATA[[fill]])) {
    if (length(unique(DATA[[fill]])) <= 10) {
      g <- g + ggplot2::discrete_scale("fill", "roche", picasso::roche_palette_discrete(1))
      g <- g + ggplot2::guides(fill = ggplot2::guide_legend(nrow = 2))
    } else {
      g <- g + ggplot2::guides(fill = "none")
      attributes(g)$caption <- c(
        attributes(g)$caption,
        glue::glue("Legend for {fill} not shown (to many values)")
      )
    }
  }
  if (!is.null(color) && is.factor(DATA[[color]])) {
    if (length(unique(DATA[[color]])) <= 10) {
      g <- g + ggplot2::discrete_scale("color", "roche", picasso::roche_palette_discrete(1))
      g <- g + ggplot2::guides(colour = ggplot2::guide_legend(nrow = 2, override.aes = list(
        size = 2, color = picasso::roche_palette_discrete()(length(unique(DATA[[color]])))
      )))
    } else {
      g <- g + ggplot2::guides(color = "none")
      attributes(g)$caption <- c(
        attributes(g)$caption,
        glue::glue("Legend for {color} not shown (to many values)")
      )
    }
  }
  return(g)
}
# ===============================================================================
#' Apply theme
#' @export
theme <- function(g, theme = "ggplot2::theme_bw", ...) {
  # Add Theme
  if (is.character(theme)) {
    .f <- nightowl_getfun(theme)
    g <- g + .f()
  } else if (is.function(theme)) {
    g <- g + theme()
  } else {
    rlang::abort("theme has to be either a function name or a function itself")
  }
  # Add other elements to theme
  args <- list(...)
  elements <- purrr::imap(args, function(.x, .y) {
    if (is.character(.x$element)) {
      .x$element <- nightowl_getfun(.x$element)
    }
    params <- .x[nightowl_pop(names(.x), "element")]
    do.call(.x$element, params)
  })
  g <- g + do.call(ggplot2::theme, elements)
  return(g)
}
# ===============================================================================
