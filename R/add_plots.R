#===============================================================================
#' Add Violin
#' @export
add_violin <- function(DATA, g, x, dodge = 0.2, width = 0.3, alpha = 0.5, ...){
      g + ggplot2::geom_violin(
      mapping = ggplot2::aes(x = forcats::as_factor(DATA[[x]])),
      position = ggplot2::position_dodge(dodge, preserve = "total"),
      ...
    )
}
#===============================================================================
#' Add Boxplot
#' @export
add_boxplot <- function(DATA,
                        g,
                        x,
                        dodge = 0.2,
                        outlier.shape = NA,
                        coef = 0,
                        alpha = 1,
                        width = 0.2,
                        color = "black", ... ){
  g + ggplot2::geom_boxplot(
        mapping = ggplot2::aes(x = forcats::as_factor(DATA[[x]])),
        position = ggplot2::position_dodge(dodge, preserve = "total"),
        outlier.shape = outlier.shape,
        coef = coef,
        alpha = alpha,
        width = width,
        color = color,
        ...
      )
}
#===============================================================================


