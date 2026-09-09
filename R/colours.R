#' Colour palettes
#'
#' Three colour-blind-safe discrete palettes ship with nightowl:
#'
#' * `"owl"`: the Okabe-Ito palette (7 colours), the default.
#' * `"dusk"`: Paul Tol's bright scheme (6 colours).
#' * `"muted"`: Paul Tol's muted scheme (9 colours).
#'
#' `nightowl_palette()` returns `n` colours from a palette, interpolating when
#' more are requested than the palette holds. With `missing = TRUE` the last
#' colour is the package-wide missing colour, so `"(Missing)"` levels always
#' look the same.
#'
#' @param name Palette name; defaults to the `palette` option.
#' @param n Number of colours. `NULL` returns the whole palette.
#' @param missing Reserve the last of the `n` colours for missing values.
#' @return `nightowl_palettes()` returns a named list of character vectors.
#'   `nightowl_palette()` returns a character vector of hex colours.
#' @examples
#' names(nightowl_palettes())
#' nightowl_palette("owl", 3)
#' nightowl_palette("dusk", 4, missing = TRUE)
#' @export
nightowl_palettes <- function() {
  list(
    owl = c("#0072B2", "#D55E00", "#009E73", "#E69F00", "#CC79A7", "#56B4E9", "#F0E442"),
    dusk = c("#4477AA", "#EE6677", "#228833", "#CCBB44", "#66CCEE", "#AA3377"),
    muted = c("#332288", "#88CCEE", "#44AA99", "#117733", "#999933", "#DDCC77", "#CC6677", "#882255", "#AA4499")
  )
}

#' @rdname nightowl_palettes
#' @export
nightowl_palette <- function(name = nightowl_option("palette"), n = NULL, missing = FALSE) {
  palettes <- nightowl_palettes()
  if (!rlang::is_string(name) || !name %in% names(palettes)) {
    cli::cli_abort(c(
      "Unknown palette {.val {name}}.",
      i = "Available palettes: {.val {names(palettes)}}."
    ))
  }
  pal <- palettes[[name]]
  if (is.null(n)) {
    return(pal)
  }
  n <- as.integer(n)
  if (is.na(n) || n < 1) {
    cli::cli_abort("{.arg n} must be a positive integer.")
  }
  n_pal <- if (missing) n - 1L else n
  cols <- if (n_pal <= length(pal)) {
    pal[seq_len(n_pal)]
  } else {
    cli::cli_inform("Palette {.val {name}} has {length(pal)} colours; interpolating to {n_pal}.")
    colorRampPalette(pal)(n_pal)
  }
  if (missing) cols <- c(cols, nightowl_missing_colour())
  unname(cols)
}

#' Semantic colours
#'
#' Fixed colour roles used across nightowl outputs, so that a reference line,
#' an estimate, or a fill look the same in every figure and table.
#'
#' @param role One or more of `"accent"`, `"reference"`, `"fill"`, `"ink"`,
#'   `"muted"`, `"missing"`. `NULL` returns all.
#' @return A named character vector of hex colours.
#' @examples
#' nightowl_colours()
#' nightowl_colours("reference")
#' @export
nightowl_colours <- function(role = NULL) {
  cols <- c(
    accent = "#0072B2",
    reference = "#D55E00",
    fill = "#B7D4EA",
    ink = "#1F1F1F",
    muted = "#6B6B6B",
    missing = nightowl_missing_colour()
  )
  if (is.null(role)) {
    return(cols)
  }
  bad <- setdiff(role, names(cols))
  if (length(bad) > 0) {
    cli::cli_abort(c("Unknown colour role{?s}: {.val {bad}}.", i = "Roles: {.val {names(cols)}}."))
  }
  cols[role]
}

#' @rdname nightowl_colours
#' @export
nightowl_missing_colour <- function() {
  nightowl_option("missing_colour")
}

#' Discrete colour and fill scales
#'
#' ggplot2 scales that use a nightowl palette and colour missing values with
#' the package-wide missing colour.
#'
#' @param name Palette name, see [nightowl_palettes()].
#' @param ... Passed on to [ggplot2::discrete_scale()].
#' @return A ggplot2 scale.
#' @examples
#' library(ggplot2)
#' ggplot(mtcars, aes(wt, mpg, colour = factor(cyl))) +
#'   geom_point() +
#'   scale_colour_nightowl()
#' @export
scale_colour_nightowl <- function(name = nightowl_option("palette"), ...) {
  ggplot2::discrete_scale(
    aesthetics = "colour",
    palette = function(n) nightowl_palette(name, n),
    na.value = nightowl_missing_colour(),
    ...
  )
}

#' @rdname scale_colour_nightowl
#' @export
scale_color_nightowl <- scale_colour_nightowl

#' @rdname scale_colour_nightowl
#' @export
scale_fill_nightowl <- function(name = nightowl_option("palette"), ...) {
  ggplot2::discrete_scale(
    aesthetics = "fill",
    palette = function(n) nightowl_palette(name, n),
    na.value = nightowl_missing_colour(),
    ...
  )
}

#' Colours for the levels of a factor, `(Missing)` bound by name
#' @noRd
level_colours <- function(levels, name = nightowl_option("palette"), missing_level = "(Missing)") {
  has_missing <- missing_level %in% levels
  cols <- nightowl_palette(name, n = length(levels), missing = has_missing)
  if (has_missing) {
    ordinary <- setdiff(levels, missing_level)
    cols <- setNames(cols, c(ordinary, missing_level))[levels]
  } else {
    cols <- setNames(cols, levels)
  }
  cols
}

#' Is a colour dark? Used to pick readable text on coloured cells.
#' @noRd
is_dark <- function(colour) {
  rgb <- col2rgb(colour) / 255
  lin <- ifelse(rgb <= 0.03928, rgb / 12.92, ((rgb + 0.055) / 1.055)^2.4)
  luminance <- 0.2126 * lin[1, ] + 0.7152 * lin[2, ] + 0.0722 * lin[3, ]
  unname(luminance < 0.5)
}
