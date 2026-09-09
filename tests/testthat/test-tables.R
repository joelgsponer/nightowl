summary_table <- function() {
  Summary$new(mtcars, "mpg", group_by = "cyl", method = summarise_numeric_pointrange)$raw()
}

test_that("render_kable renders plots inline and wraps headers", {
  tbl <- summary_table()
  k <- render_kable(tbl, caption = "Cap", footnote = "Note", header_width = 3)
  expect_s3_class(k, "kableExtra")
  expect_match(k, "class='nightowl-table'|class=\"nightowl-table")
  expect_match(k, "<svg")
  expect_match(k, "Cap")
  expect_match(k, "Note")
  expect_equal(stringr::str_count(k, "<svg"), 4)
  plain <- render_kable(head(mtcars, 2), scale_row = FALSE, header_width = NULL)
  expect_false(grepl("<svg", plain))
  expect_error(render_kable(1), "data frame")
})

test_that("render_html wraps kable in the card with the dependency", {
  h <- render_html(head(mtcars, 2), title = "T", subtitle = "S", footnote = "F")
  expect_s3_class(h, "shiny.tag")
  expect_equal(htmltools::htmlDependencies(h)[[1]]$name, "nightowl")
  html <- as.character(h)
  expect_match(html, "nightowl-title")
  expect_match(html, "nightowl-footnote")
})

test_that("render_reactable configures plot and group columns", {
  tbl <- summary_table()
  w <- render_reactable(tbl)
  expect_s3_class(w, "reactable")
  cols <- w$x$tag$attribs$columns
  by_id <- setNames(cols, vapply(cols, function(c) c$id, ""))
  expect_true(by_id$Pointrange$html)
  expect_equal(by_id$Pointrange$minWidth, 216)
  expect_match(by_id$Pointrange$footer, "<svg")
  expect_equal(by_id$cyl$sticky, "left")
  expect_true(any(vapply(htmltools::htmlDependencies(w), function(d) d$name == "nightowl", logical(1))))
  custom <- render_reactable(tbl, columns = list(N = reactable::colDef(name = "Count")), scale_row = FALSE)
  ccols <- custom$x$tag$attribs$columns
  expect_true(any(vapply(ccols, function(c) identical(c$name, "Count"), logical(1))))
})

test_that("add_inline_scale appends exactly one axis row", {
  tbl <- summary_table()
  out <- add_inline_scale(tbl)
  expect_equal(nrow(out), nrow(tbl) + 1)
  expect_true(is_NightowlPlots(out$Pointrange))
  expect_equal(out$Variable[nrow(out)], "")
  expect_equal(out$Pointrange[[nrow(out)]]$type, "Scale")
  expect_identical(add_inline_scale(head(mtcars)), dplyr::ungroup(head(mtcars)))
  expect_error(add_inline_scale(tbl, columns = "N"), "not")
})

test_that("nightowl_reactable_theme is a theme", {
  expect_s3_class(nightowl_reactable_theme(), "reactableTheme")
})
