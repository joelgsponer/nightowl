# =================================================
#' @title Render Interactive SVG Plot with ggiraph
#' @description
#' Creates an interactive SVG plot using ggiraph with customizable options for width,
#' height, and styling. Supports both code evaluation and direct ggplot object rendering.
#' @param code R code to evaluate for plot generation (alternative to ggobj)
#' @param ggobj A ggplot2 object to render (alternative to code)
#' @param pointsize Numeric value for text size in points (default: 12)
#' @param width_svg Width of the SVG in inches (default: 6)
#' @param height_svg Height of the SVG in inches (default: 5) 
#' @param options List of additional options passed to ggiraph
#' @param fix_rect Logical indicating whether to fix rect elements in SVG (default: TRUE)
#' @param ... Additional arguments passed to ggiraph::dsvg()
#' @return HTML object containing the rendered interactive SVG plot
#' @export
render_girafe <- function(code, ggobj = NULL, pointsize = 12,
                          width_svg = NULL, height_svg = NULL,
                          options = list(),
                          fix_rect = TRUE,
                          ...) {
  path <- tempfile()

  if (is.null(width_svg)) {
    width_svg <- 6
  }
  if (is.null(height_svg)) {
    height_svg <- 5
  }


  args <- list(...)
  args$canvas_id <- args$canvas_id %||% paste("svg", gsub("-", "_", uuid::UUIDgenerate()), sep = "_")
  args$file <- path
  args$width <- width_svg
  args$height <- height_svg
  args$pointsize <- pointsize
  args$standalone <- FALSE
  args$setdims <- FALSE
  # we need a surface with pointer events
  if (identical(args$bg, "transparent")) {
    args$bg <- "#fffffffd"
  }

  devlength <- length(dev.list())
  do.call(ggiraph::dsvg, args)
  tryCatch(
    {
      if (!is.null(ggobj)) {
        if (!inherits(ggobj, "ggplot")) {
          abort("`ggobj` must be a ggplot2 plot", call = NULL)
        }
        print(ggobj)
      } else {
        code
      }
    },
    finally = {
      if (length(dev.list()) > devlength) {
        dev.off()
      }
    }
  )
  svg <- waRRior::read_plain_textfile(args$file)
  if (fix_rect) svg <- stringr::str_replace(svg, stringr::fixed("/>"), "></rect>")
  svg <- htmltools::HTML(svg)
  svg <- htmltools::browsable(svg)
  return(svg)
}
# ===============================================================================
#' Create SVG string
#' @param g ggplot object
#' @export
render_svg <- function(g,
                       code = NULL,
                       height = 8,
                       width = 8,
                       element_width = "100%",
                       element_height = "100%",
                       scaling = 1,
                       add_download_button = T,
                       standalone = F,
                       bg = "transparent",
                       font_family = "Lato, sans-serif",
                       fix_rect = T,
                       ...) {
  fix_font <- function(html, param = "font-family", value = font_family, old_value = "[^;]*") {
    str <- as.character(html)
    pattern <- glue::glue("{param}: {old_value}")
    replace <- glue::glue("{param}: {value}")
    new_str <- stringr::str_replace_all(str, pattern, replace)
    return(htmltools::HTML(new_str))
  }
  n_dev <- length(dev.list())
  tryCatch(
    {
      cli::cli_progress_step("Opening SVG device")
      svg <- svglite::svgstring(
        height = height, width = width, scaling = scaling,
        web_fonts = "https://fonts.googleapis.com/css2?family=Lato:wght@100;300;400;700;900&display=swap",
        standalone = standalone,
        bg = bg,
        ...
      )
      if (inherits(g, "call")) {
        rlang::eval_tidy(g)
      } else {
        print(g)
      }
      svg <- waRRior::regex_replace_element_parameter(svg(), "width", element_width) %>%
        waRRior::regex_replace_element_parameter("height", element_height) %>%
        fix_font()
      if (add_download_button) {
        svg <- nightowl::add_download_button(svg)
      }
      svg <- as.character(svg)
      if (fix_rect) svg <- stringr::str_replace(svg, stringr::fixed("/>"), "></rect>")
      svg <- htmltools::HTML(svg)
      svg <- htmltools::browsable(svg)
      return(svg)
    },
    error = function(e) {
      stop(e)
    },
    finally = {
      while (length(dev.list()) > n_dev) {
        cli::cli_progress_step("Closing device")
        dev.off()
      }
    }
  )
}
# =================================================
#' @title Add Download Button to SVG Plot
#' @description
#' Adds a clickable download button to an SVG plot that allows users to save
#' the plot as an SVG file. The button includes JavaScript functionality for
#' client-side file download.
#' @param x HTML object containing an SVG plot
#' @return HTML div element containing the original plot with added download functionality
#' @export
add_download_button <- function(x) {
  shiny::div(
    AceOfSpades::useAceOfSpadesJS(),
    htmltools::HTML('
      <script>
        function nightowl_download_svg(e) {
          var svg = e.parentElement.querySelector("svg");

          var svgXml = (new XMLSerializer).serializeToString(svg);
          var blob = new Blob([svgXml], {type: "image/svg+xml"});
          var url = URL.createObjectURL(blob);

          var link = document.createElement("a");
          link.href = url;
          link.download = "image.svg";
          link.style.display = "none";
          document.body.appendChild(link);

          // your code goes here
          link.click();
          document.body.removeChild(link);
        };
      </script>
    '),
    htmltools::HTML("
      <div
       class = 'nightowl-svg-download-button'
       onclick='nightowl_download_svg(this)'
       style = 'text-align:right; font-weight:900; font-family:sans-serif;cursor:pointer;'
      >&#10515; Save Plot</div>
    "),
    x
  )
}
# =================================================
#' @title Create New NightowlPlots Vector
#' @description
#' Constructor function for creating a new NightowlPlots vector class that can
#' hold multiple plot objects with specialized printing and formatting methods.
#' @param ... Plot objects to include in the NightowlPlots vector
#' @return A NightowlPlots vector containing the provided plot objects
#' @export
new_NightowlPlots <- function(...) {
  x <- list(...)
  x <- unclass(x)
  vctrs::new_vctr(x, class = "NightowlPlots")
}
# =================================================
#' @title Check if Object is NightowlPlots
#' @description
#' Tests whether an object inherits from the NightowlPlots or NghtwlPl class.
#' @param x Object to test
#' @return Logical value indicating whether the object is a NightowlPlots instance
#' @export
is_NightowlPlots <- function(x) {
  inherits(x, "NightowlPlots") || inherits(x, "NghtwlPl")
}
# =================================================
#' @title Format Method for NightowlPlots
#' @description
#' Applies the format method to each plot object within a NightowlPlots vector,
#' returning a list of formatted representations.
#' @param x A NightowlPlots object
#' @return List containing formatted representations of each plot
#' @export
format.NightowlPlots <- function(x) {
  purrr::map(x, ~ .x$format())
}
# =================================================
#' @title Convert Object to HTML
#' @description
#' Generic function for converting objects to HTML representation.
#' Methods are defined for specific classes like NightowlPlots.
#' @param x Object to convert to HTML
#' @return HTML representation of the object
#' @export
as_html <- function(x) {
  UseMethod("as_html")
}
# =================================================
#' @title Convert NightowlPlots to HTML
#' @description
#' Converts each plot in a NightowlPlots vector to its HTML representation
#' by calling the html() method on each plot object.
#' @param x A NightowlPlots object
#' @return List containing HTML representations of each plot
#' @export
as_html.NightowlPlots <- function(x) {
  purrr::map(x, ~ .x$html())
}
# =================================================
#' @title Convert NightowlPlots to JSON
#' @description
#' Converts each plot in a NightowlPlots vector to JSON format by first
#' converting to HTML and then serializing with jsonlite.
#' @param x A NightowlPlots object
#' @return List containing JSON representations of each plot
#' @export
asJSON.NightowlPlots <- function(x) {
  purrr::map(x, ~ jsonlite:::asJSON(.x$html()))
}
# =================================================
#' @title Extract ggplot Objects
#' @description
#' Generic function for extracting ggplot2 objects from specialized plot classes.
#' Methods are defined for specific classes like NightowlPlots.
#' @param x Object containing ggplot objects
#' @return ggplot2 object(s) extracted from the input
#' @export
as_ggplot <- function(x) {
  UseMethod("as_ggplot")
}
# =================================================
#' @title Extract ggplot Objects from NightowlPlots
#' @description
#' Extracts the underlying ggplot2 objects from each plot in a NightowlPlots vector
#' by accessing the plot field of each object.
#' @param x A NightowlPlots object
#' @return List containing ggplot2 objects from each plot
#' @export
as_ggplot.NightowlPlots <- function(x) {
  purrr::map(x, ~ .x$plot)
}
# =================================================
#' @title Print Method for NightowlPlots
#' @description
#' Prints each plot in a NightowlPlots vector by calling the print method
#' on each individual plot object.
#' @param x A NightowlPlots object
#' @param browser Logical indicating whether to display in browser (default: TRUE)
#' @return Invisibly returns the input object after printing
#' @export
print.NightowlPlots <- function(x, browser = T) {
  purrr::map(x, ~ .x$print())
}
# =================================================
#' @title Vector Type Abbreviation for NightowlPlots
#' @description
#' Provides the abbreviated type name for NightowlPlots vectors when displayed
#' in tibbles or other vctrs-aware contexts.
#' @param x A NightowlPlots object
#' @return Character string "NghtwlPl" as the abbreviated type name
#' @export
vec_ptype_abbr.NightowlPlots <- function(x) {
  "NghtwlPl"
}
# =================================================
#' @title Convert NightowlPlots to Character
#' @description
#' Converts each plot in a NightowlPlots vector to character representation
#' by calling the as.character method on each plot object.
#' @param x A NightowlPlots object
#' @return Character vector containing string representations of each plot
#' @export
as.character.NightowlPlots <- function(x) {
  purrr::map_chr(x, ~ .x$as.character())
}
# =================================================
#' @title Vector Prototype for NightowlPlots Combination
#' @description
#' Defines the common type when combining two NightowlPlots vectors.
#' Part of the vctrs framework for type-stable vector operations.
#' @param x First NightowlPlots object
#' @param y Second NightowlPlots object
#' @param ... Additional arguments (unused)
#' @return The prototype NightowlPlots object
#' @export
vec_ptype2.NightowlPlots.NightowlPlots <- function(x, y, ...) {
  x
}
# =================================================
#' @title Vector Cast for NightowlPlots
#' @description
#' Defines how to cast between NightowlPlots vectors.
#' Part of the vctrs framework for type-stable vector operations.
#' @param x NightowlPlots object to cast
#' @param to Target NightowlPlots type
#' @param ... Additional arguments (unused)
#' @return The cast NightowlPlots object
#' @export
vec_cast.NightowlPlots.NightowlPlots <- function(x, to, ...) {
  x
}
# =================================================
#' @title Convert to R6 Object
#' @description
#' Generic function for converting objects to their R6 class representation.
#' Methods are defined for specific classes like NightowlPlots.
#' @param x Object to convert to R6
#' @return R6 object representation
#' @export
as_R6 <- function(x) {
  UseMethod("as_R6")
}
# =================================================
#' @title Convert NightowlPlots to R6 Objects
#' @description
#' Returns the R6 objects contained within a NightowlPlots vector.
#' Each plot object is already an R6 instance.
#' @param x A NightowlPlots object
#' @return List containing the R6 plot objects
#' @export
as_R6.NightowlPlots <- function(x) {
  purrr::map(x, ~.x)
}
# =================================================
#' @title Get Object Width
#' @description
#' Generic function for retrieving the width of plot objects.
#' Methods are defined for specific classes like NightowlPlots.
#' @param x Object to get width from
#' @return Numeric value representing the width
#' @export
width <- function(x) {
  UseMethod("width")
}
# =================================================
#' @title Get Maximum Width from NightowlPlots
#' @description
#' Calculates the maximum width across all plots in a NightowlPlots vector
#' by calling get_width() on each plot and returning the maximum value.
#' @param x A NightowlPlots object
#' @return Numeric value representing the maximum width across all plots
#' @export
width.NightowlPlots <- function(x) {
  purrr::map_dbl(x, ~ .x$get_width()) %>%
    max(na.rm = T)
}
# =================================================
#' @title Get Object Height
#' @description
#' Generic function for retrieving the height of plot objects.
#' Methods are defined for specific classes like NightowlPlots.
#' @param x Object to get height from
#' @return Numeric value representing the height
#' @export
height <- function(x) {
  UseMethod("height")
}
# =================================================
#' @title Get Maximum Height from NightowlPlots
#' @description
#' Calculates the maximum height across all plots in a NightowlPlots vector
#' by calling get_height() on each plot and returning the maximum value.
#' @param x A NightowlPlots object
#' @return Numeric value representing the maximum height across all plots
#' @export
height.NightowlPlots <- function(x) {
  purrr::map_dbl(x, ~ .x$get_height()) %>%
    max(na.rm = T)
}
# =================================================
as.character.NightowlPlots <- function(x) {
  res <- purrr::map_chr(x, function(.x) {
    .x$html() %>%
      as.character()
  })
  return(res)
}
# =================================================
