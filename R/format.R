#' Format p-values for tables
#'
#' Formats p-values consistently: three significant decimals, values below
#' `eps` shown as `"<0.001"`, optional significance stars.
#'
#' @param p Numeric vector of p-values. `NA` stays `NA`.
#' @param digits Number of decimals.
#' @param eps Smallest value printed; smaller values print as `"<eps"`.
#' @param stars Append significance markers (`***`, `**`, `*`).
#' @param html Escape `<` for HTML output.
#' @return A character vector the same length as `p`.
#' @examples
#' format_p_value(c(0.00001, 0.004, 0.032, 0.21, NA))
#' format_p_value(0.004, stars = TRUE)
#' @export
format_p_value <- function(p, digits = 3, eps = 10^-digits, stars = FALSE, html = TRUE) {
  if (!is.numeric(p)) {
    cli::cli_abort("{.arg p} must be numeric.")
  }
  out <- ifelse(
    is.na(p), NA_character_,
    ifelse(p < eps, paste0("<", formatC(eps, format = "f", digits = digits)),
      formatC(round(p, digits), format = "f", digits = digits)
    )
  )
  if (stars) {
    star <- ifelse(is.na(p), "", ifelse(p < 0.001, " ***", ifelse(p < 0.01, " **", ifelse(p < 0.05, " *", ""))))
    out <- paste0(out, star)
    out[is.na(p)] <- NA_character_
  }
  if (html) out <- stringr::str_replace_all(out, "<", "&lt;")
  out
}

#' Mean with a confidence interval
#'
#' A dependency-free replacement for `Hmisc::smean.cl.*`. Returns the layout
#' ggplot2 expects from a `fun.data`, so it can be used in
#' [ggplot2::stat_summary()] as well as in summary templates.
#'
#' @param x Numeric vector. Missing values are dropped.
#' @param conf Confidence level.
#' @param method `"t"` for a t-interval, `"boot"` for a percentile bootstrap.
#' @param B Bootstrap replicates when `method = "boot"`.
#' @param seed Optional seed used only inside the bootstrap.
#' @return A one-row data frame with columns `y`, `ymin`, `ymax`.
#' @examples
#' mean_ci(mtcars$mpg)
#' mean_ci(mtcars$mpg, method = "boot", seed = 1)
#' @export
mean_ci <- function(x, conf = 0.95, method = c("t", "boot"), B = 1000, seed = NULL) {
  method <- rlang::arg_match(method)
  x <- x[!is.na(x)]
  n <- length(x)
  m <- if (n > 0) mean(x) else NA_real_
  if (n < 2) {
    return(data.frame(y = m, ymin = NA_real_, ymax = NA_real_))
  }
  if (method == "t") {
    half <- stats::qt((1 + conf) / 2, df = n - 1) * sd(x) / sqrt(n)
    return(data.frame(y = m, ymin = m - half, ymax = m + half))
  }
  if (!is.null(seed)) {
    old <- if (exists(".Random.seed", globalenv())) get(".Random.seed", globalenv()) else NULL
    on.exit(if (!is.null(old)) assign(".Random.seed", old, globalenv()), add = TRUE)
    set.seed(seed)
  }
  boots <- vapply(seq_len(B), function(i) mean(sample(x, n, replace = TRUE)), numeric(1))
  q <- stats::quantile(boots, c((1 - conf) / 2, (1 + conf) / 2), names = FALSE)
  data.frame(y = m, ymin = q[1], ymax = q[2])
}

#' Format a mean with its confidence interval
#'
#' @inheritParams mean_ci
#' @param digits Decimals shown.
#' @return A one-row tibble with columns `Mean` and `CI`.
#' @examples
#' format_mean_ci(mtcars$mpg)
#' @export
format_mean_ci <- function(x, digits = 2, conf = 0.95, method = "t") {
  ci <- mean_ci(x, conf = conf, method = method)
  fmt <- function(v) formatC(round(v, digits), format = "f", digits = digits)
  tibble::tibble(
    Mean = round(ci$y, digits),
    CI = if (is.na(ci$ymin)) NA_character_ else glue::glue("[{fmt(ci$ymin)}, {fmt(ci$ymax)}]")
  )
}

#' Count extreme values
#'
#' Counts values beyond Tukey's far-out fences: below `Q1 - fold * IQR` or
#' above `Q3 + fold * IQR`. Sign-safe, so negative data behave correctly.
#'
#' @param x Numeric vector. Missing values are ignored.
#' @param fold Multiplier of the interquartile range; 1.5 gives the usual
#'   boxplot whiskers, 3 (the default) Tukey's far-out fences.
#' @return An integer count.
#' @examples
#' count_extreme_values(c(rnorm(100), 50, -50))
#' @export
count_extreme_values <- function(x, fold = 3) {
  x <- x[!is.na(x)]
  if (length(x) < 4) {
    return(0L)
  }
  q <- stats::quantile(x, c(0.25, 0.75), names = FALSE)
  iqr <- q[2] - q[1]
  sum(x < q[1] - fold * iqr | x > q[2] + fold * iqr)
}

#' Format frequencies of a categorical vector
#'
#' Returns one column per level with `"percent% (count)"` strings, ready to be
#' spread across a summary table. Missing values become a `"(Missing)"` level.
#'
#' @param x A factor or character vector.
#' @param digits Decimals for percentages.
#' @param output `"print"` for formatted strings, `"counts"` or `"percent"`
#'   for the raw numbers.
#' @param header_width Width at which level names are wrapped for headers.
#' @return A one-row tibble with one column per level.
#' @examples
#' format_frequencies(factor(c("a", "b", "a", NA)))
#' @export
format_frequencies <- function(x, digits = 1, output = c("print", "counts", "percent"),
                               header_width = nightowl_option("header_width")) {
  output <- rlang::arg_match(output)
  x <- explicit_missing(x)
  counts <- table(x)
  percent <- round(as.numeric(counts) / length(x) * 100, digits)
  nms <- names(counts)
  if (output == "counts") {
    return(tibble::as_tibble(setNames(as.list(as.integer(counts)), nms)))
  }
  if (output == "percent") {
    return(tibble::as_tibble(setNames(as.list(percent), nms)))
  }
  cells <- glue::glue("{formatC(percent, format = 'f', digits = digits)}% ({as.integer(counts)})")
  headers <- stringr::str_replace_all(stringr::str_wrap(nms, header_width), "\n", "<br>")
  tibble::as_tibble(setNames(as.list(as.character(cells)), headers))
}
