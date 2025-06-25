# Make some more simple barcharts
# =================================================
#' @title Create donut plots for categorical variables
#' @description
#' Creates interactive donut plot visualizations for categorical variables split by groups,
#' with automatic color coding and percentage labels.
#' @param data A data frame containing the categorical variable and grouping variable
#' @param column String. Name of the categorical column to visualize
#' @param split String. Name of the grouping variable to split plots by
#' @param threshold_levels Integer. Minimum levels to apply fraction threshold. Default 3
#' @param threshold_fractions Numeric. Minimum fraction for label display. Default 0.089
#' @param colors Named vector of colors for categories. If NULL, uses brewer palette
#' @param brewer_pal String. RColorBrewer palette name. Default "OrRd"
#' @param color_missing String. Color for missing values. Default "#BEBEBE"
#' @param label String. Display label for the row header. Default column name
#' @return An HTML div containing the donut plots with legend
#' @export
make_donut_plot_categorical <- function(data,
                                        column,
                                        split,
                                        threshold_levels = 3,
                                        threshold_fractions = 0.089,
                                        colors = NULL,
                                        brewer_pal = "OrRd",
                                        color_missing = "#BEBEBE",
                                        label = column) {
  # Make split factor if it is not yet
  if (!is.factor(data[[split]])) {
    data[[split]] <- factor(data[[split]]) %>%
      forcats::fct_explicit_na()
  }
  # Make column a factor
  data[[column]] <- factor(data[[column]]) %>%
    forcats::fct_explicit_na()
  # Number of levels
  nl <- nlevels(data[[column]])
  # Prepare colors if not provided
  if (is.null(colors)) {
    colors <- RColorBrewer::brewer.pal(nl, brewer_pal)
    colors <- colors[1:nl]
    colors <- purrr::set_names(colors, levels(data[[column]]))
    try(colors[["(Missing)"]] <- color_missing)
  }
  if (is.null(names(colors))) {
    names(colors) <- levels(data[[column]])
    try(colors[["(Missing)"]] <- color_missing)
  }
  # Split Data
  p <- waRRior::named_group_split(data, !!rlang::sym(split)) %>%
    purrr::imap(function(.x, .y) {
      # Calculate frequencies and prepare labels
      freq <- waRRior::tally_at(.x, column) %>%
        dplyr::arrange(!!rlang::sym(column))
      freq$fraction <- freq$n / sum(freq$n)
      freq$ymax <- cumsum(freq$fraction)
      freq$ymin <- c(0, head(freq$ymax, n = -1)) # Compute label position
      freq$labelPosition <- (freq$ymax + freq$ymin) / 2
      freq[[split]] <- .y
      freq$label <- paste0(round(freq$fraction * 100), "%")
      # No label if a lot of levels and low fraction (see threshold)
      if (nl > threshold_levels) {
        freq$label[freq$fraction <= threshold_fractions] <- ""
      }
      # Prepare plot
      .p <- ggplot2::ggplot(freq, mapping = ggplot2::aes(ymax = ymax, ymin = ymin, xmax = 19, xmin = 6, fill = !!rlang::sym(column))) +
        ggplot2::geom_rect(color = "black", lwd = 0.1) +
        ggplot2::geom_text(x = 13, mapping = ggplot2::aes(y = labelPosition, label = label), color = "black", size = 4, fontface = "bold") +
        ggplot2::coord_polar(theta = "y") +
        ggplot2::scale_fill_manual(values = colors) +
        ggplot2::xlim(c(-1, 19)) +
        ggplot2::theme_void() +
        ggplot2::theme(
          legend.position = "none",
          plot.margin = grid::unit(c(0, 0, 0, 0), "cm")
        ) +
        ggplot2::guides(fill = ggplot2::guide_legend(nrow = 1, byrow = TRUE))
      # Pack into div
      shiny::div(
        class = "plot",
        nightowl::render_svg(.p, height = 5, scaling = 6, add_download_button = FALSE)
      )
    })
  # Build a legend
  legend <- purrr::map2(colors[1:nl], levels(data[[column]]), function(.color, .level) {
    shiny::div(class = "legend-item", style = glue::glue("background-color: {.color};"), .level)
  }) %>%
    shiny::div(class = "legend")
  # Put everything together
  shiny::div(
    class = "row",
    shiny::div(
      class = "rowheader",
      label,
      legend
    ),
    p
  ) %>%
    htmltools::browsable()
}
# =================================================
# =================================================
#' @title Create violin-box plots for numeric variables
#' @description
#' Creates violin-box plot combinations for numeric variables split by groups,
#' showing distribution and median reference lines.
#' @param data A data frame containing the numeric variable and grouping variable
#' @param column String. Name of the numeric column to visualize
#' @param split String. Name of the grouping variable to split plots by
#' @param colors Named vector of colors for groups. If NULL, uses brewer palette
#' @param brewer_pal String. RColorBrewer palette name. Default "OrRd"
#' @param color_missing String. Color for missing values. Default "grey"
#' @param label String. Display label for the row header. Default column name
#' @param .range Numeric vector of length 2. Y-axis limits. If NULL, uses data range
#' @return An HTML div containing the violin-box plots with median annotations
#' @export
make_donut_plot_numeric <- function(data,
                                    column,
                                    split,
                                    colors = NULL,
                                    brewer_pal = "OrRd",
                                    color_missing = "grey",
                                    label = column,
                                    .range = NULL) {
  # Calculate range overall
  if (is.null(.range)) {
    .range <- range(data[[column]], na.rm = T)
  }
  # Calculate Median overall
  .median <- median(data[[column]], na.rm = T)
  # Make split factor if it is not yet
  if (!is.factor(data[[split]])) {
    data[[split]] <- factor(data[[split]]) %>%
      forcats::fct_explicit_na()
  }
  l <- levels(data[[split]])
  nl <- nlevels(data[[split]])
  # if no colors are provided, use a diverging palette
  if (is.null(colors)) {
    colors <- RColorBrewer::brewer.pal(nl, brewer_pal) %>%
      purrr::set_names(l)
  }
  if (is.null(names(colors))) {
    names(colors) <- levels(data[[split]])
  }
  try(colors[["(Missing)"]] <- color_missing)
  # Split and plot
  p <- waRRior::named_group_split(data, !!rlang::sym(split)) %>%
    purrr::imap(function(.x, .y) {
      if (picasso::is_dark(colors[[.y]])) {
        .contrast <- "white"
      } else {
        .contrast <- "black"
      }
      .p <- ggplot2::ggplot(.x,
        mapping = ggplot2::aes(x = !!rlang::sym(split), y = !!rlang::sym(column))
      ) +
        ggplot2::geom_hline(yintercept = .median, color = "black", lwd = 0.5, lty = 3) +
        ggplot2::geom_violin(
          fill = colors[[.y]],
          trim = FALSE
        ) +
        ggplot2::geom_boxplot(
          fill = colors[[.y]],
          outlier.shape = NA,
          # position = ggplot2::position_nudge(-0.2),
          width = 0.1,
          color = .contrast
        ) +
        ggplot2::theme_bw() +
        ggplot2::scale_y_continuous(limits = .range) +
        ggplot2::scale_x_discrete(expand = ggplot2::expansion(0.8)) +
        ggplot2::xlab("") +
        ggplot2::ylab("") +
        ggplot2::theme_classic() +
        ggplot2::theme(
          legend.position = "left",
          axis.line.x.bottom = ggplot2::element_blank(),
          plot.margin = grid::unit(c(0.1, 1, 0, 0), "cm")
        ) +
        picasso::hide_x_axis() +
        ggplot2::guides(fill = ggplot2::guide_legend(nrow = 1, byrow = TRUE))
      shiny::div(
        class = "plot",
        nightowl::render_svg(.p, height = 6, scaling = 6, add_download_button = FALSE),
        shiny::div(class = "median", glue::glue("Median: {round(median(.x[[column]], na.rm = TRUE), 1)}"))
      )
    })
  shiny::div(
    class = "row",
    shiny::div(class = "rowheader", label),
    p
  ) %>%
    htmltools::browsable()
}
# =================================================
#' @title Create multi-column donut plot dashboard
#' @description
#' Creates a comprehensive dashboard of donut plots for multiple variables,
#' automatically choosing appropriate plot types based on variable types.
#' @param data A data frame containing variables to visualize
#' @param columns Character vector. Names of columns to create plots for
#' @param split String. Name of the grouping variable to split plots by
#' @param labels Character vector. Display labels for each column. If NULL, uses column names
#' @param options List. Plotting options for each column or by variable type
#' @return An HTML dashboard with headers and multiple donut plots
#' @export
donut_plot <- function(data,
                       columns,
                       split,
                       labels = NULL,
                       options = NULL) {
  # Make split factor if it is not yet
  if (!is.factor(data[[split]])) {
    data[[split]] <- factor(data[[split]]) %>%
      forcats::fct_explicit_na()
  }
  columns <- as.list(columns)
  if (is.null(labels)) {
    labels <- as.list(columns)
  }
  if (is.null(options)) {
    options <- purrr::map(columns, ~NULL)
  }
  if (length(options) == 1) {
    options <- purrr::map(columns, ~options)
  }
  if (length(options) == 2 && all(c("numeric", "categorical") %in% names(options))) {
    options <- purrr::map(columns, function(.column) {
      if (is.numeric(data[[.column]])) {
        options[["numeric"]]
      } else {
        options[["categorical"]]
      }
    })
  }

  params <- list(
    data = purrr::map(columns, ~ dplyr::select(data, !!rlang::sym(.x), !!rlang::sym(split))),
    columns,
    labels,
    options
  ) %>%
    purrr::set_names(c("data", "column", "label", "options"))

  p <- purrr::pmap(params, function(data, column, label, options) {
    if (is.numeric(data[[column]])) {
      do.call(nightowl::make_donut_plot_numeric, c(list(data = data, column = column, split = split, label = label), options))
    } else {
      do.call(nightowl::make_donut_plot_categorical, c(list(data = data, column = column, split = split, label = label), options))
    }
  })
  header <- levels(data[[split]])
  do.call(nightowl::render_donut_plot, c(list(header = header), p))
}
# =================================================
#' @title Render donut plot dashboard with headers
#' @description
#' Renders a complete donut plot dashboard with column headers and multiple plot rows,
#' applying consistent styling and layout.
#' @param header Character vector. Column headers for the dashboard
#' @param ... Plot objects to include in the dashboard
#' @return An HTML div containing the complete dashboard with styling
#' @export
render_donut_plot <- function(header, ...) {
  plots <- list(...)
  header <- purrr::map(c("", header), function(.x) {
    shiny::div(class = "colheader", .x)
  }) %>%
    shiny::div(class = "row")
  shiny::div(
    nightowl::css_donut_plot(),
    # Add additional border if only one row
    if (length(plots) == 1) {
      shiny::HTML("
        <style>
          .container > .row:last-child > .plot{
            border-top: 2px solid black;
          }
        </style>
      ")
    },
    shiny::div(
      class = "container",
      header,
      plots
    )
  ) %>%
    shiny::div() %>%
    htmltools::browsable()
}
# =================================================
#' @title Generate CSS styling for donut plot dashboards
#' @description
#' Generates comprehensive CSS styling for donut plot dashboards,
#' including layout, borders, fonts, and responsive design elements.
#' @return An HTML style tag containing CSS for donut plot styling
#' @export
css_donut_plot <- function() {
  shiny::HTML("<style>
    body {
      font-family: 'Open Sans', sans-serif;
    }
    .row{
     display:flex;
     flex-direction:row;
     align-items:center;
     justify-content:center;
    }
    .median {
        font-size: 0.6em;
      bottom: 13px;
      font-weight: bold;
      text-align: right;
      position: relative;
      margin: 5px;
    }
    .colheader{
      width: 300px;
      text-align: center;
      font-weight: bold;
      font-size: 2em;
      vertical-align: middle;
    }
    .colheader:first-child{
      width: 400px;
      font-weight: bold;
      text-align: right;
      vertical-align: middle;
      padding: 5px;
    }
    .legend{
      display: flex;
      flex-direction: row;
      flex-wrap: wrap;
      align-items: center;
      justify-content: flex-end;
      font-size: 1.5em;
    }
    .legend-item {
      font-size: 0.6em;
      padding: 2px;
      border-radius: 5px;
      margin: 1px;
      font-weight: 500;
    }
    .rowheader {
      width: 400px;
      max-width: 400px;
      font-weight: 700;
      text-align: right;
      vertical-align: middle;
      padding: 5px;
      font-size: 1.5em;
    }
    .plot{
      width: 300px;
      border-left: 2px solid black;
    }
    .plot:last-child{
      border-right: 2px solid black;
    }
    .row:nth-child(2) > .plot{
      border-top: 2px solid black!important;
    }
    .row:last-child > .plot{
      border-bottom: 2px solid black;
    }
  </style>")
}
# =================================================
