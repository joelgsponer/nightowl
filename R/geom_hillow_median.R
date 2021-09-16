#===============================================================================
#' Geom Hillow Median
#' @export
GeomHillowMedian <- ggplot2::ggproto(
  "GeomHillowMedian",
  ggplot2::Geom,
  setup_params = function(data, params) {
    params
  },
  setup_data = function(data, params) {
    ggplot2::GeomLine$setup_data(data, params)
  },

  draw_group = function(data,
                        panel_params,
                        coord,
                        add_ribbon = TRUE,
                        add_whiskers = FALSE,
                        whiskers_width = 0.2,
                        ribbon_alpha = 0.5,
                        add_points = F
  ) {
    ribbon <- data %>%
      dplyr::mutate(colour = NA, alpha = ribbon_alpha)

    whiskers <- data %>%
      dplyr::mutate(
        xmin = x - whiskers_width/2,
        xmax = x +  whiskers_width/2,
        linetype = "solid"
      )

    path <- transform(data, alpha = NA)

    points <- data %>%
      dplyr::mutate(alpha = 1, size = 3, stroke = 1, shape = 21)

    has_ribbon <- add_ribbon && !is.null(data$ymax) && !is.null(data$ymin)
    has_error_bars <- add_whiskers && !is.null(data$ymax) && !is.null(data$ymin)

    grid::gList(
      if (has_ribbon) ggplot2::GeomRibbon$draw_panel(ribbon, panel_params, coord),
      if (has_error_bars) ggplot2::GeomErrorbar$draw_panel(whiskers, panel_params, coord),
      ggplot2::GeomLine$draw_panel(path, panel_params, coord),
      if (add_points) ggplot2::GeomPoint$draw_panel(points, panel_params, coord)
    )
  },
  draw_key = ggplot2::draw_key_smooth,
  required_aes = c("x", "y"),
  optional_aes = c("ymin", "ymax"),
  default_aes = ggplot2::aes(colour = "#3366FF", fill = "grey60", size = 1,
                    linetype = 1, weight = 1, alpha = 0.4)
)

#===============================================================================
#' Stat Hillow Median
#' @export
StatHillowMedian <- ggplot2::ggproto(
  "StatHillowMedian",
  ggplot2::Stat,
  compute_group = function(data, scales) {
    data %>%
      dplyr::group_by(x) %>%
      dplyr::group_split(keep = T) %>%
      purrr::map_df(function(this_group){
        current_x <- dplyr::select(this_group, x) %>% unique()
        this_y <- dplyr::select(this_group, y)
        current_PANEL <- dplyr::select(this_group, PANEL) %>% unique()
        current_group <- dplyr::select(this_group, group) %>% unique()
        cbind(current_x,ggplot2::median_hilow(this_y), current_PANEL, current_group)
      }) %>%
      dplyr::mutate(PANEL = as.factor(PANEL))
  },
  required_aes = c("x", "y")
)


#===============================================================================
#' Geom hillow median
#' @export
geom_hillow_median <- function(
  mapping = NULL,
  data = NULL,
  position = "identity",
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE,
  add_ribbon = T,
  add_whiskers = F,
  add_points = F,
  whiskers_width = 0.1,
  ribbon_alpha = 0.5,
  ...) {
  ggplot2::layer(
    stat = nightowl::StatHillowMedian, data = data, mapping = mapping, geom = nightowl::GeomHillowMedian,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      add_ribbon = add_ribbon,
      add_whiskers = add_whiskers,
      whiskers_width = whiskers_width,
      ribbon_alpha = ribbon_alpha,
      add_points = add_points,
      ...
    )
  )
}
#===============================================================================
