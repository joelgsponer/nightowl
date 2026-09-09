#' Snapshot-test SVG output
#'
#' `expect_snapshot_svg()` is a testthat expectation that stores the rendered
#' SVG of a plot as a snapshot file, so that changes in a figure show up as a
#' reviewable diff in [testthat::snapshot_review()], which renders `.svg`
#' snapshots side by side. Text metrics depend on the platform's fonts, so
#' snapshots are kept per operating system (`variant`) and the expectation
#' skips on CRAN.
#'
#' `svg_normalise()` strips the parts of svglite output that vary between
#' renderings without a visible change: generated ids, text length hints and
#' the download button.
#'
#' @param x A [Plot], a [NightowlPlots] vector, a ggplot object, or SVG
#'   markup (character or [htmltools::HTML()]).
#' @param name Snapshot file name without extension.
#' @param variant Snapshot variant; defaults to the operating system name.
#' @param ... For ggplot input, rendering options passed to [render_svg()].
#' @return `expect_snapshot_svg()` is called for its side effect and returns
#'   the normalised SVG invisibly. `svg_normalise()` returns a character
#'   vector, one element per SVG.
#' @examples
#' gg <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) + ggplot2::geom_point()
#' svg <- svg_normalise(render_svg(gg, width = 3, height = 2))
#' nchar(svg) > 0
#' @export
expect_snapshot_svg <- function(x, name, variant = Sys.info()[["sysname"]], ...) {
  rlang::check_installed("testthat", reason = "to use `expect_snapshot_svg()`")
  testthat::skip_on_cran()
  svg <- svg_normalise(x, ...)
  path <- file.path(tempdir(), paste0(name, ".svg"))
  writeLines(paste(svg, collapse = "\n"), path)
  testthat::expect_snapshot_file(
    path,
    name = paste0(name, ".svg"),
    variant = variant,
    compare = testthat::compare_file_text
  )
  invisible(svg)
}

#' @rdname expect_snapshot_svg
#' @export
svg_normalise <- function(x, ...) {
  svgs <- svg_strings(x, ...)
  vapply(svgs, function(s) {
    s <- stringr::str_replace_all(s, "<button class='nightowl-download'.*?</button>", "")
    s <- stringr::str_replace_all(s, "<button class=\"nightowl-download\".*?</button>", "")
    s <- stringr::str_replace_all(s, "(id|class)='svglite[^']*'", "\\1='svglite'")
    s <- stringr::str_replace_all(s, "\\.svglite[-_A-Za-z0-9]*", ".svglite")
    s <- stringr::str_replace_all(s, " textLength='[^']*'", "")
    s <- stringr::str_replace_all(s, " lengthAdjust='[^']*'", "")
    s <- stringr::str_replace_all(s, "id='cp[A-Za-z0-9=+/]*'", "id='cp'")
    s <- stringr::str_replace_all(s, "url\\(#cp[A-Za-z0-9=+/]*\\)", "url(#cp)")
    s <- stringr::str_replace_all(s, ">\\s*<", ">\n<")
    trimws(s)
  }, character(1), USE.NAMES = FALSE)
}

svg_strings <- function(x, ...) {
  if (is_Plot(x)) {
    return(as.character(x$svg(download_button = FALSE, ...)))
  }
  if (is_NightowlPlots(x)) {
    return(vapply(vctrs::vec_data(x), function(p) as.character(p$svg(download_button = FALSE)), character(1)))
  }
  if (inherits(x, "ggplot")) {
    return(as.character(render_svg(x, download_button = FALSE, ...)))
  }
  if (is.character(x)) {
    return(as.character(x))
  }
  cli::cli_abort("Cannot extract SVG from {.obj_type_friendly {x}}.")
}
