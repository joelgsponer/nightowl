# =================================================
#' @title
#' MISSING_TITLE
#' @description
#' @detail
#' @param
#' @return
#' @export
add_barplot <- function(x) {
  x <- factor(x) %>%
    forcats::fct_explicit_na()
  counts <- base::table(x) / length(x) * 100
  .p <- tibble::tibble(fill = names(counts), y = counts) %>%
    dplyr::mutate(fill = forcats::fct_inorder(fill)) %>%
    dplyr::mutate(fill = forcats::fct_rev(fill)) %>%
    ggplot2::ggplot(ggplot2::aes(y = 1, x = y, fill = fill)) +
    ggplot2::geom_col(orientation = "y") +
    MetBrewer::scale_fill_met_d("Demuth", drop = F) +
    ggplot2:::scale_y_discrete(expand = ggplot2::expansion(0)) +
    ggplot2::scale_x_continuous(limits = c(0, 100)) +
    ggplot2::theme_void() +
    ggplot2::theme(
      legend.position = "none",
      plot.margin = ggplot2::margin(0, 0, 0, 0, "cm")
    )
  nightowl::render_svg(.p, height = 0.8, add_download_button = FALSE)
}
# =================================================
#' @title
#' MISSING_TITLE
#' @description
#' @detail
#' @param
#' @return
#' @export
add_forestplot <- function(x,
                           fun_data = ggplot2::mean_cl_boot,
                           xintercept = NULL,
                           xlim = NULL) {
  vals <- fun_data(x)
  nightowl::forestplot(
    x = vals[[1]],
    xmin = vals[[2]],
    xmax = vals[[3]],
    xintercept = xintercept,
    xlim = xlim
  )
}
# =================================================
#' @title
#' MISSING_TITLE
#' @description
#' @detail
#' @param
#' @return
#' @export
add_inline_plot <- function(x,
                            y = rep(0, length(x)),
                            mapping = list(x = "x", y = "y"),
                            style = NULL,
                            xlim = NULL,
                            ylim = NULL,
                            height = 0.8,
                            width = 8,
                            scaling = 1,
                            theme = ggplot2::theme_void,
                            coord_flip = FALSE,
                            hide_title = T,
                            hide_legend = T,
                            hide_x_axis = T,
                            hide_y_axis = T,
                            fill = picasso::roche_colors("lightblue"),
                            expansion_y = 0,
                            add_download_button = FALSE,
                            fun.data = ggplot2::mean_cl_boot,
                            ...) {
  # Prepare paramters
  ## DATA
  if (!inherits(x, "data.frame")) {
    DATA <- tibble::tibble(x = x, y = y)
  } else {
    DATA <- x
  }
  ## Paramters added via ...
  if (!is.null(style)) {
    .style <- nightowl::load_style(style)
  } else {
    .ellipsis <- list(...)
    if (!is.null(.ellipsis$svg)) {
      cli::cli_alter_warning("For inline plots svg paramters should be passed directeley. Ignoring additional svg paramters in plot call.")
      .ellipsis$svg <- NULL
    }
    .style <- .ellipsis
  }
  ## Putting them together
  .args <- c(
    list(DATA = DATA),
    list(mapping = mapping),
    .style
  )
  .p <- do.call(nightowl::plot, .args)

  # Coord flip
  if (coord_flip) {
    .p <- .p + ggplot2::coord_flip()
  }

  # Axis limits
  if (!is.null(xlim) && is.numeric(.p$data[[mapping$x]])) {
    .p <- .p + ggplot2::scale_x_continuous(limits = xlim)
  }
  if (!is.null(ylim) && is.numeric(.p$data[[mapping$y]])) {
    .p <- .p + ggplot2::scale_y_continuous(limits = ylim)
  }
  .p <- .p + theme()

  # Hiding stuff
  if (hide_title) {
    .p <- .p + picasso::hide_title()
  }
  if (hide_legend) {
    .p <- .p + picasso::hide_legend()
  }
  if (hide_x_axis) {
    .p <- .p + picasso::hide_x_axis()
  }
  if (hide_y_axis) {
    .p <- .p + picasso::hide_y_axis()
  }

  # Create SVG
  nightowl::render_svg(.p,
    height = height,
    width = width,
    scaling = scaling,
    add_download_button = add_download_button
  )
}
# =================================================
#' @title
#' MISSING_TITLE
#' @description
#' @detail
#' @param
#' @return
#' @export
add_violin <- function(x,
                       ylim = NULL,
                       height = 0.8,
                       width = 8,
                       scaling = 1,
                       theme = ggplot2::theme_void,
                       hide_legend = T,
                       hide_x_axis = T,
                       hide_y_axis = T,
                       fill = picasso::roche_colors("lightblue"),
                       expansion_y = 0,
                       add_download_button = FALSE,
                       fun.data = ggplot2::mean_cl_boot) {
  .data <- tibble::tibble(x = x)
  .p <- ggplot2::ggplot(.data,
    mapping = ggplot2::aes(y = x, x = 0)
  ) +
    ggplot2::geom_violin(fill = fill) +
    ggplot2::stat_summary(fun.data = fun.data, size = 2) +
    ggplot2::coord_flip() +
    ggplot2:::scale_y_continuous(expand = ggplot2::expansion(expansion_y))
  if (!is.null(ylim)) {
    .p <- .p + ggplot2::ylim(ylim)
  }
  .p <- .p + theme()
  if (hide_legend) {
    .p <- .p + picasso::hide_legend()
  }
  if (hide_x_axis) {
    .p <- .p + picasso::hide_x_axis()
  }
  if (hide_y_axis) {
    .p <- .p + picasso::hide_y_axis()
  }
  nightowl::render_svg(.p,
    height = height,
    width = width,
    scaling = scaling,
    add_download_button = add_download_button
  )
}
# ===============================================================================
#' @title
#' MISSING_TITLE
#' @description
#' @detail
#' @param
#' @return
#' @export
add_density <- function(x,
                        ylim = NULL,
                        height = 0.8,
                        width = 8,
                        scaling = 1,
                        theme = ggplot2::theme_void,
                        hide_x_axis = T,
                        hide_y_axis = T,
                        fill = picasso_colors("lightblue"),
                        expansion_y = 0,
                        add_download_button = FALSE) {
  .data <- tibble::tibble(x = x)
  .p <- ggplot2::ggplot(.data,
    mapping = ggplot2::aes(y = x, x = 0)
  ) +
    ggplot2::geom_violin(fill = fill) +
    ggplot2::stat_summary(size = 2) +
    ggplot2::coord_flip() +
    ggplot2:::scale_y_continuous(expand = ggplot2::expansion(expansion_y))
  if (!is.null(ylim)) {
    .p <- .p + ggplot2::ylim(ylim)
  }
  .p <- .p + theme()
  if (!add_scale) {
    .p <- .p + picasso::hide_y_axis()
    .p <- .p + picasso::hide_x_axis()
  }
  nightowl::render_svg(.p,
    height = height,
    width = width,
    scaling = scaling,
    add_download_button = add_download_button
  )
}
# =================================================
#' @title
#' MISSING_TITLE
#' @description
#' @detail
#' @param
#' @return
#' @export
add_histogram <- function(x,
                          height = 0.8,
                          binwidth = NULL,
                          xlim = NULL,
                          add_scale = F) {
  .data <- tibble::tibble(x = x)
  .p <- ggplot2::ggplot(.data,
    mapping = ggplot2::aes(x = x)
  ) +
    ggplot2::geom_histogram(fill = picasso::roche_colors("lightblue"), binwidth = binwidth, color = "black") +
    ggplot2::theme_void() +
    ggplot2:::scale_y_continuous(expand = ggplot2::expansion(0))
  if (add_scale) {
    .p <- .p + picasso::add_x_axis()
  }
  nightowl::render_svg(.p, height = height, add_download_button = FALSE)
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
add_scale <- function(obj,
                      height = 1,
                      scaling = 3.5,
                      legend_position = "none") {
  obj <- dplyr::ungroup(obj)
  columns <- purrr::imap(obj, ~ if (nightowl::is_NightowlPlots(.x)) .y else NULL) %>%
    purrr::compact()


  ggs <- purrr::map(columns, function(.column) {
    .gg <- nightowl::as_ggplot(obj[[.column]])[[1]]
    .gg$layers <- NULL
    .gg +
      ggplot2::theme_classic() +
      ggplot2::theme(
        legend.position = legend_position,
        axis.line.x = ggplot2::element_line(colour = "black", size = ggplot2::rel(1)),
        axis.line.y = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_blank(),
        axis.text.x = ggplot2::element_text(size = ggplot2::rel(1)),
        axis.ticks.y = ggplot2::element_blank(),
        # plot.margin = ggplot2::margin(0, 0, 0, 0, "cm"),
        axis.title.y = ggplot2::element_blank(),
        axis.title.x = ggplot2::element_blank()
      )
  }) %>%
    purrr::set_names(columns) %>%
    purrr::map(~ nightowl::InlinePlot$new(
      ggplot = .x,
      type = "NightowlScale",
      svg = list(height = height, scaling = scaling, add_download_button = FALSE)
    ))
  .scales <- purrr::map(ggs, ~ nightowl::new_NightowlPlots(.x))
  dplyr::bind_rows(obj, .scales) %>%
    dplyr::mutate_if(is.factor, as.character) %>%
    dplyr::mutate_if(is.character, function(x) tidyr::replace_na(x, ""))
}
#-------------------------------------------------------------------------------
#' R6 Class
#' @description
#' @detail
#' @export
InlinePlot <- R6::R6Class("InlinePlot",
  public = list(
    initialize = function(...) {
      args <- list(...)
      purrr::imap(list(...), function(.x, .y) {
        if (.y %in% names(self)) {
          self[[.y]] <- .x
        }
      })
      self$set_html()
    },
    ggplot = NULL,
    # SVG/HTML ---------------------------------------------------------------------------
    svg = NULL,
    html = NULL,
    set_html = function() {
      self$html <- memoise::memoise(function() {
        do.call(nightowl::render_svg, c(list(g = self$ggplot), self$svg))
      })
    },
    # print --------------------------------------------------------------------
    print = function() {
      print(self$html())
    },
    # format ----
    type = "NighowlInlinePlots",
    format = function(...) {
      return(glue::glue("<{self$type}>"))
    },
    as.character = function() {
      return(as.character(self$html()))
    },
    get_width = function() {
      viewBox <- self$as.character() %>%
        stringr::str_extract("viewBox='([^']+)") %>%
        stringr::str_replace("viewBox='", "") %>%
        stringr::str_split(" ")
      viewBox[[1]][3] %>%
        as.numeric()
    },
    get_height = function() {
      viewBox <- self$as.character() %>%
        stringr::str_extract("viewBox='([^']+)") %>%
        stringr::str_replace("viewBox='", "") %>%
        stringr::str_split(" ")
      viewBox[[1]][4] %>%
        as.numeric()
    }
  ),
  private = list()
)
#-------------------------------------------------------------------------------
