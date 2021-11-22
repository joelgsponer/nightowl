# ===============================================================================
#' Add Violin
#' @export
violin <- function(g,
                       mapping,
                       dodge = 0.5,
                       width = 0.3,
                       alpha = 0.5,
                       ...) {
  g + ggplot2::geom_violin(
    mapping = ggplot2::aes(x = forcats::as_factor(.data[[mapping$x]])),
    position = ggplot2::position_dodge(dodge, preserve = "total"),
    ...
  )
}
# ===============================================================================
#' Add Boxplot
#' @export
boxplot <- function(g,
                        mapping,
                        dodge = 0.5,
                        color = "black",
                        breaks = 10,
                        ...) {
  data <- g$data
  nightowl:::expand_mapping(mapping)
  if(is.numeric(data[[mapping$x]])){
    data <- data %>%
      dplyr::mutate(group = cut(!!x, breaks)) %>%
      dplyr::mutate(lower = as.numeric( sub("\\((.+),.*", "\\1", group))) %>%
      dplyr::mutate(upper = as.numeric( sub("[^,]*,([^]]*)\\]", "\\1", group))) %>%
      dplyr::mutate(width = upper - lower) %>%
      dplyr::mutate()
      dplyr::group_by(group) %>%
      dplyr::group_split() %>%
      purrr::map_df(function(.data){
        .data %>%
          dplyr::mutate(midpoint = mean(!!x))
      })
      browser()
    .aes <- ggplot2::aes(x = data$midpoint, group = data$group)

  } else {
    .aes <- ggplot2::aes()
  }
  g + ggplot2::geom_boxplot(
    mapping = .aes,
    position = ggplot2::position_dodge(dodge, preserve = "total"),
    color = color,
    ...
  )

}
# ===============================================================================
#' Add dotplot
#' @export
dotplot <- function(g,
                        mapping,
                        dodge = 0.5,
                        binaxis = "y",
                        stackdir = "center",
                        dotsize = 0.3,
                        ...) {
  g + ggplot2::geom_dotplot(
    mapping = ggplot2::aes(x = forcats::as_factor(.data[[mapping$x]]), group = NULL),
    position = ggplot2::position_dodge(dodge, preserve = "total"),
    binaxis = binaxis,
    stackdir = stackdir,
    dotsize = dotsize,
    ...
  )
}
# ===============================================================================
#' Add points
#' @export
points <- function(g,
                       mapping,
                       dodge = 0.5,
                       binaxis = "y",
                       stackdir = "center",
                       size = 0.3,
                       ...) {
  g + ggplot2::geom_point(
    ...
  )
}
# ===============================================================================
#' Add summary
#' @export
summary <- function(g,
                        mapping,
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
  g + ggplot2::stat_summary(
    mapping = ggplot2::aes(
      x = as.numeric(forcats::as_factor(.data[[mapping$x]]))
    ),
    position = ggplot2::position_dodge(dodge, preserve = "total"),
    shape = shape,
    ...
  )
}
# ===============================================================================
#' Add smooth
#' @export
smooth <- function(g,
                       mapping,
                       dodge = 0,
                       method = "lm",
                       color = "black",
                       ...) {
  attributes(g)$caption <- c(
    attributes(g)$caption,
    glue::glue("Method for summary: '{method}'")
  )
  g + ggplot2::geom_smooth(
    mapping = ggplot2::aes(x = as.numeric(forcats::as_factor(.data[[mapping$x]]))),
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
  }
}
# ===============================================================================
#' Add traces
#' @export
traces <- function(g,
                       mapping,
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
  g + ggplot2::stat_summary(
    mapping = ggplot2::aes(
      x = as.numeric(forcats::as_factor(.data[[mapping$x]])),
      group = .data[[mapping$id]]
    ),
    position = ggplot2::position_dodge(dodge, preserve = "total"),
    shape = shape,
    ...
  )
}
# ===============================================================================
