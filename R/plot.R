# =================================================
#' @title
#' MISSING_TITLE
#' @description
#' @detail
#' @param
#' @return
#' @export
is_Plot <- function(x) {
  all(
    inherits(x, "Plot"),
    inherits(x, "R6")
  )
}
# ===============================================================================
#' R6 Class
#'
#' @description
#'
#' @detail
#' @export
Plot <- R6::R6Class("Plot",
  public = list(
    type = NULL,
    initialize = function(plot = NULL,
                          options_svg = list(width = 8, height = 8, scaling = 1),
                          type = "NightowlPlot",
                          resize = TRUE,
                          css = NULL) {
      self$options_svg <- options_svg
      self$type <- type
      self$css <- css
      self$resize <- resize
      private$set_plot(plot)
      private$set_render_svg()
      return(self)
    },
    #' @field ggplot object, potentially other plot types possible (base does not work)
    plot = NULL,
    # SVG/HTML ---------------------------------------------------------------------------
    options_svg = NULL,
    svg = function(...) {
      args <- list(...)
      args <- c(self$options_svg[!names(self$options_svg) %in% names(args)], args)
      do.call(private$render_svg, c(list(g = self$plot), args))
    },
    # HTML --------------------------------------------------------------------
    css = list(
      class = "NightowlPlot",
      style = NULL
    ),
    parse_style = function(style = self$css$style) {
      purrr::imap(style, ~ glue::glue("{stringr::str_replace(.y, '_', '-')}:{.x};"))
    },
    resize = NULL,
    html = function(resize = self$resize) {
      if (!resize) {
        style <- self$parse_style(
          c(
            self$css$style,
            list(
              width = paste0(self$get_width(), "px"),
              height = paste0(self$get_height(), "px")
            )
          )
        )
      } else {
        style <- self$parse_style(self$css$style)
      }
      shiny::div(
        class = paste(self$css$class),
        style = style,
        self$svg()
      )
    },
    # print --------------------------------------------------------------------
    print = function(browser = TRUE) {
      if (browser) {
        self$html() %>%
          htmltools::browsable() %>%
          print()
      }
    },
    format = function(...) {
      return(self$type)
    },
    as.character = function() {
      return(as.character(self$html()))
    },
    get_width = function() {
      viewBox <- self$svg() %>%
        as.character() %>%
        stringr::str_extract("viewBox='([^']+)") %>%
        stringr::str_replace("viewBox='", "") %>%
        stringr::str_split(" ")
      viewBox[[1]][3] %>%
        as.numeric()
    },
    get_height = function() {
      viewBox <- self$svg() %>%
        as.character() %>%
        stringr::str_extract("viewBox='([^']+)") %>%
        stringr::str_replace("viewBox='", "") %>%
        stringr::str_split(" ")
      viewBox[[1]][4] %>%
        as.numeric()
    }
  ),
  private = list(
    set_render_svg = function() {
      private$render_svg <- memoise::memoise(render_svg)
    },
    render_svg = NULL,
    set_plot = function(plot) {
      self$plot <- plot
    }
  )
)
# ===============================================================================
#' R6 Class
#'
#' @description
#'
#' @detail
#' @export
DeclarativePlot <- R6::R6Class("DeclarativePlot",
  inherit = nightowl::Plot,
  public = list(
    data = NULL,
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
    dodge = 1,
    processing = NULL,
    transform = NULL,
    layers = list(),
    scales = list(),
    annotation = NULL,
    axis = NULL,
    colors = NULL,
    theming = NULL,
    facets = NULL,
    # Intitalize ---------------------------------------------------------------
    initialize = function(...) {
      super$initialize()
      
      # Initialize memoized plot function once during object creation
      private$memoized_plot_fn <- memoise::memoise(private$generate_plot)
      
      args <- list(...)
      purrr::imap(list(...), function(.x, .y) {
        if (.y %in% names(self)) {
          if (.y == "svg") .y <- "options_svg"
          self[[.y]] <- .x
        }
      })
      self$select_data()
      self$transform_data()
      self$prepare_facets()
      self$set_plot()
      return(self)
    },
    # select data
    select_data = function() {
      cols <- c(
        unlist(unname(self$mapping)),
        purrr::map(self$layers, ~ unlist(unname(.x$mapping))) %>% unlist()
      ) %>%
        unique()
      if (inherits(self$data, "data.frame")) {
        .data <- tibble::as_tibble(self$data)
      }
      self$data <- .data %>%
        dplyr::select_at(cols)
      if (any(dim(self$data) == 0)) rlang::abort("No data, check mapping")
    },
    # tranform data (this also potentionally updates the mapping if for example frequencies are calculated)
    transform_data = function() {
      transform <- self$transform
      mapping <- self$mapping
      .data <- self$data
      if (!is.null(transform)) {
        transform_data <- transform$data
        transform <- transform[waRRior::pop(names(transform), "data")]
        .data <- purrr::reduce(names(transform), function(.data, .var_name) {
          .f <- transform[[.var_name]]
          if (is.character(.f)) .f <- waRRior::getfun(.f)
          .var <- mapping[[.var_name]]
          if (!is.null(.var)) {
            .data <- .data %>%
              dplyr::mutate(!!rlang::sym(.var) := .f(!!rlang::sym(.var)))
          }
          return(.data)
        }, .init = .data)
        if (!is.null(transform_data)) {
          if (is.character(transform_data)) .f <- waRRior::getfun(transform_data)
          res_tranform_data <- .f(.data, mapping)
          self$data <- res_tranform_data$data
          self$mapping <- res_tranform_data$mapping
        } else {
          # Update self$data with transformed data when no transform_data function
          self$data <- .data
        }
      }
    },
    # prepare facets
    prepare_facets = function() {
      facets <- self$facets
      mapping <- self$mapping
      # Prepare facets (if any)
      if (is.null(facets) &&
        (!is.null(mapping$facet_row) ||
          !is.null(mapping$facet_col))
      ) {
        facets <- list()
      }
      if (!is.null(mapping$facet_col)) facets$column <- mapping$facet_col
      if (!is.null(mapping$facet_row)) facets$row <- mapping$facet_row
      self$mapping <- mapping[waRRior::pop(names(mapping), c("facet_row", "facet_col"))]
      self$facets <- facets
    },
    # ggplot ---
    plot = NULL,
    set_plot = function() {
      # Create serialized cache key for better consistency
      cache_key <- list(
        data = self$data,
        mapping = self$mapping,
        layers = self$layers,
        scales = self$scales,
        facets = self$facets,
        axis = self$axis,
        colors = self$colors,
        theming = self$theming,
        annotation = self$annotation,
        dodge = self$dodge
      )
      
      # Track if this is a cache hit by comparing counters before/after
      misses_before <- private$cache_misses
      
      # Generate plot using pre-initialized memoized function
      self$plot <- private$memoized_plot_fn(cache_key)
      
      # If misses didn't increase, it was a cache hit
      if (private$cache_misses == misses_before) {
        private$cache_hits <- private$cache_hits + 1
      }
    },
    # Cache invalidation methods
    invalidate_cache = function() {
      # Clear the memoised cache and recreate the function
      if (!is.null(private$memoized_plot_fn)) {
        memoise::forget(private$memoized_plot_fn)
        private$memoized_plot_fn <- memoise::memoise(private$generate_plot)
      }
      invisible(self)
    },
    
    # Cache statistics methods
    get_cache_stats = function() {
      list(
        hits = private$cache_hits,
        misses = private$cache_misses,
        hit_rate = if ((private$cache_hits + private$cache_misses) > 0) {
          private$cache_hits / (private$cache_hits + private$cache_misses)
        } else {
          0
        }
      )
    },
    
    reset_cache_stats = function() {
      private$cache_hits <- 0
      private$cache_misses <- 0
      invisible(self)
    },
    
    # Override setters to invalidate cache when properties change
    set_data = function(value) {
      self$data <- value
      self$invalidate_cache()
      invisible(self)
    },
    
    set_mapping = function(value) {
      self$mapping <- value
      self$invalidate_cache()
      invisible(self)
    },
    
    set_layers = function(value) {
      self$layers <- value
      self$invalidate_cache()
      invisible(self)
    },
    
    set_scales = function(value) {
      self$scales <- value
      self$invalidate_cache()
      invisible(self)
    },
    
    # Format ---
    type = "<NightowlDeclarativePlot>",
    format = function(...) {
      return(self$type)
    }
  ),
  private = list(
    memoized_plot_fn = NULL,
    cache_hits = 0,
    cache_misses = 0,
    generate_plot = function(cache_key) {
      # Increment cache miss counter (only called on actual generation)
      private$cache_misses <- private$cache_misses + 1
      
      # Extract parameters from cache_key
      data <- cache_key$data
      mapping <- cache_key$mapping
      layers <- cache_key$layers
      scales <- cache_key$scales
      facets <- cache_key$facets
      axis <- cache_key$axis
      colors <- cache_key$colors
      theming <- cache_key$theming
      annotation <- cache_key$annotation
      dodge <- cache_key$dodge
      
      # Setup Plot
      .aes <- nightowl:::aes(mapping)
      g <- ggplot2::ggplot(data, .aes)
      # Add layers
      g <- purrr::reduce(layers, function(.x, .y) {
        .y$g <- .x
        .y <- rev(.y)
        if (is.null(.y$dodge)) .y$dodge <- dodge
        type <- .y$type
        .y$type <- NULL
        .y <- purrr::compact(.y)
        thiscall <- glue::glue("do.call(nightowl::{type}, .y)")
        eval(parse(text = thiscall))
      }, .init = g)
      # ************************************************************************
      # Add scales
      g <- purrr::reduce(scales, function(.x, .y) {
        .y$g <- .x
        .y <- rev(.y)
        .y <- purrr::compact(.y)
        thiscall <- glue::glue("do.call(nightowl::scales, .y)")
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
      g <- do.call(nightowl::colors, c(list(g = g, DATA = data, mapping = mapping), colors))
      # # Add Theme
      g <- do.call(nightowl::theme, c(list(g = g), theming))
      # #*******************************************************************************
      # # Annotation
      g <- do.call(nightowl::annotation, c(list(g = g), mapping, annotation, axis))
      #*******************************************************************************
      # Finishing up
      return(g)
    }
  )
)
# ===============================================================================
