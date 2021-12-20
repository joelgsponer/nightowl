# ===============================================================================
#' Boxplot
#' @importFrom ggplot2 aes mean_cl_boot mean_cl_normal mean_se mean_sdl label_both
#' @export
plot <- function(DATA,
                 mapping = list(
                   x = NULL,
                   y = NULL,
                   group = NULL,
                   fill = NULL,
                   color = NULL,
                   size = NULL,
                   shape = NULL,
                   lty = NULL,
                   id = NULL
                 ),
                 processing = NULL,
                 transform = NULL,
                 layers = list(),
                 annotation = NULL,
                 axis = NULL,
                 theming = NULL,
                 facets = NULL,
                 svg = NULL,
                 dodge = 1,
                 ...) {
  #*******************************************************************************
  # Drop columns that are not needed
  cols <- c(
    unlist(unname(mapping)),
    purrr::map(layers, ~ unlist(unname(.x$mapping))) %>% unlist()
  ) %>%
    unique()
  DATA <- DATA %>%
    tibble::as_tibble() %>%
    dplyr::select_at(cols)
  if (any(dim(DATA) == 0)) rlang::abort("No data, check mapping")
  #*******************************************************************************
  # Transformations
  if (!is.null(transform)) {
    purrr::iwalk(transform, function(.f, .var) {
      .f <- waRRior::getfun(.f)
      .var <- mapping[[.var]]
      if (!is.null(.var)) {
        DATA <<- DATA %>%
          dplyr::mutate(!!rlang::sym(.var) := .f(!!rlang::sym(.var)))
      }
    })
  }
  #*******************************************************************************
  # Prepare facets (if any)
  if (is.null(facets) &&
    (!is.null(mapping$facet_row) ||
      !is.null(mapping$facet_col))
  ) {
    facets <- list()
  }
  if (!is.null(mapping$facet_col)) facets$column <- mapping$facet_col
  if (!is.null(mapping$facet_row)) facets$row <- mapping$facet_row
  mapping <- mapping[waRRior::pop(names(mapping), c("facet_row", "facet_col"))]
  #*******************************************************************************
  # Setup Plot
  .aes <- nightowl:::aes(mapping)
  g <- ggplot2::ggplot(DATA, .aes)
  #*******************************************************************************
  # Add type to layers
  # layers <- purrr::imap(layers, function(.x, .y) {
  #   c(list(type = .y), .x)
  # })
  #*******************************************************************************
  # Add layers
  g <- purrr::reduce(layers, function(.x, .y) {
    .y$g <- .x
    if (is.null(.y$dodge)) .y$dodge <- dodge
    thiscall <- glue::glue("do.call(nightowl::{.y$type}, .y)")
    .y$type <- NULL
    eval(parse(text = thiscall))
  }, .init = g)
  #*******************************************************************************
  # Facets
  g <- do.call(nightowl::facets, c(list(g = g), facets))
  #*******************************************************************************
  # Axis
  g <- do.call(nightowl::axis, c(list(g = g), axis))
  # #*******************************************************************************
  # Colors and theming
  g <- do.call(nightowl::colors, c(list(g = g, DATA = DATA, mapping = mapping)))
  # # Add Theme
  g <- do.call(nightowl::theme, c(list(g = g), theming))
  # #*******************************************************************************
  # # Annotation
  g <- do.call(nightowl::annotation, c(list(g = g), mapping, annotation, axis))
  #*******************************************************************************
  # Create SVG
  if (!is.null(svg)) {
    g <- do.call(nightowl::render_svg, c(list(g = g), svg))
  }
  # Finishing up
  return(g)
}
# ===============================================================================
