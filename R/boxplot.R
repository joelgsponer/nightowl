# ===============================================================================
#' Boxplot
#' @export
boxplot <-
  function(DATA,
           x,
           y,
           fill = NULL,
           color = NULL,
           title = NULL,
           xlab = NULL,
           axis.text.x.angle = 45,
           ylab = NULL,
           log_y = F,
           remove_missing = T,
           dodge = 1,
           add_violin = T,
           add_boxplot = T,
           add_points = T,
           points_color = picasso::roche_color("black"),
           points_size = 1,
           points_stroke = 1,
           points_alpha = 0.8,
           add_title = T,
           facet_row = NULL,
           facet_col = NULL,
           facet_label_width = 20,
           theme = picasso::theme_picasso,
           color_values = unname(picasso::roche_colors()),
           scales = "free_x",
           ...) {
    #*******************************************************************************
    # Parameters
    if (is.null(fill) || fill == "") fill <- x
    if (!is.null(fill) && is.null(color)) color <- fill
    #*******************************************************************************
    # Data preparation
    # Add text wraping for facets
    DATA <- DATA %>%
      dplyr::mutate_at(
        c(facet_row, facet_col),
        function(x) stringr::str_wrap(x, width = facet_label_width)
      )
    # Drop missing values
    if (remove_missing) {
      DATA <- DATA %>%
        dplyr::filter_at(c(x, y, fill, color, facet_row, facet_col), function(x) !is.na(x)) %>%
        dplyr::mutate_if(is.character, factor) %>%
        droplevels()
    } else {
      # Make missing factors explicit
      # Convert Characters to factors
      DATA <- DATA %>%
        dplyr::mutate_if(is.character, factor) %>%
        dplyr::mutate_if(is.factor, forcats::fct_explicit_na)
    }
    #*******************************************************************************
    # Setup ggplot
    # This was difficult, fist store parameters in list,
    # Convert to symbols
    # drop the onses which are null, call aes_ function (CAVE: ecex)
    # also think of other places where params is used, e.g. params$id
    params <- list(
      x = x,
      y = y,
      fill = fill,
      color = color,
      group = fill
    ) %>%
      purrr::compact() %>%
      purrr::map(~ rlang::sym(.x))
    f <- ggplot2::aes_
    .aes <- rlang::exec(.fn = "f", !!!params)
    g <- DATA %>%
      ggplot2::ggplot(.aes)
    #*******************************************************************************
    # Add Violin
    if (add_violin) {
      g <- g + ggplot2::geom_violin(
        position = ggplot2::position_dodge(dodge, preserve = "total"),
        width = 1, alpha = 0.5
      )
    }
    #*******************************************************************************
    # Add Boxplot
    if (add_boxplot) {
      if (add_violin) {
        g <- g + ggplot2::geom_boxplot(
          position = ggplot2::position_dodge(dodge, preserve = "total"),
          outlier.shape = NA,
          coef = 0,
          alpha = 1,
          width = 0.3,
          color = "black"
        )
      } else {
        g <- g + ggplot2::geom_boxplot(
          outlier.shape = NA,
          position = ggplot2::position_dodge(dodge, preserve = "total"),
          width = 0.3,
          color = "black"
        )
      }
    }
    #*******************************************************************************
    # Add points
    if (add_points) {
      g <- g + ggplot2::geom_jitter(
        pch = 21,
        color = points_color,
        alpha = points_alpha,
        stroke = points_stroke,
        size = points_size,
        position = ggplot2::position_jitterdodge(dodge.width = dodge)
      )
    }
    #*******************************************************************************
    # Faceting
    if (!is.null(facet_col) | !is.null(facet_row)) {
      if (is.null(facet_col)) facet_col <- "."
      if (is.null(facet_row)) facet_row <- "."
      g <- g + ggplot2::facet_grid(
        as.formula(
          paste(
            paste0("`", facet_row, "`", collapse = "+"),
            "~",
            paste0("`", facet_col, "`", collapse = "+")
          )
        ),
        scales = scales,
        labeller = ggplot2::label_both
      )
    }
    #*******************************************************************************
    # Colors and theming
    if (is.factor(DATA[[fill]])) {
      g <- g + ggplot2::scale_fill_manual(values = color_values)
      g <- g + ggplot2::scale_color_manual(values = color_values)
    }
    # Add Theme
    if (is.character(theme)) {
      g <- g + eval(parse(text = paste0(theme, "()")))
    } else if (is.function(theme)) {
      g <- g + theme()
    } else {
      rlang::abort("theme has to be either a function name as a string or a function iself")
    }
    # Adjust margins etc,
    g <- g + ggplot2::theme(
      legend.key.width = ggplot2::unit(2, "cm"),
      plot.margin = ggplot2::margin(1, 1, 1, 1, "cm")
    )
    #*******************************************************************************
    # Annotation
    ## Labels
    g <- g + ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = axis.text.x.angle),
      legend.position = "bottom"
    )
    ## X label
    if (!is.null(xlab)) {
      g <- g + ggplot2::xlab(xlab)
    }
    ## Y label
    if (!is.null(ylab)) {
      g <- g + ggplot2::ylab(ylab)
    }
    ## Title
    if (!is.null(title)) {
      if (title == "auto") {
        auto_title <- glue::glue("{x} vs. {y}")
        g <- g + ggplot2::ggtitle(auto_title)
      } else {
        g <- g + ggplot2::ggtitle(title)
      }
    }
    #*******************************************************************************
    # Axis
    if (log_y) g <- g + ggplot2::scale_y_log10()
    #*******************************************************************************
    # Finishing
    return(g)
  }
# ===============================================================================
