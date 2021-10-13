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
           axis.text.x.hjust = 1,
           axis.text.x.vjust = 1,
           legend.position = "bottom",
           x_label_width = 20,
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
           label_width = 20,
           theme = picasso::theme_picasso,
           palette_discrete = picasso::roche_palette_discrete(1),
           scales = "free_x",
           ...) {
    #*******************************************************************************
    # Parameters
    if (is.null(fill) || fill == "") fill <- x
    if (!is.null(fill) && is.null(color)) color <- fill
    # Drop columns that are not needed
    DATA <- DATA %>%
      dplyr::select_at(c(x, y, color, fill, facet_row, facet_col))
    #*******************************************************************************
    # Drop missing values
    DATA <- nightowl::prepare_data_for_plotting(DATA, remove_missing = remove_missing)
    # Data preparation
    DATA <- nightowl::add_text_wraping(DATA, width = label_width)
    #*******************************************************************************
    # Setup Plot
    .aes <- list(
      x = x,
      y = y,
      fill = fill,
      color = color,
      group = fill
    )
    g <- nightowl::ggplot(DATA, .aes)
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
      g <- g + ggplot2::discrete_scale("fill", "roche", palette_discrete, ...)
      g <- g + ggplot2::discrete_scale("color", "roche", palette_discrete, ...)
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
      axis.text.x = ggplot2::element_text(
        angle = axis.text.x.angle,
        hjust = axis.text.x.hjust,
        vjust = axis.text.x.vjust,
      ),
      legend.position = legend.position,
      ...
    ) +
      ggplot2::scale_y_continuous(n.breaks = 20)

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
