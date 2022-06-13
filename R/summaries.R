# =================================================
#' @title
#' MISSING_TITLE
#' @description
#' @detail
#' @param
#' @return
#' @export
reactable_summary <- function(data, split = NULL, columns, group_by = NULL, plan = "sequential", ...) {
  future::plan(plan)
  if (is.null(split)) {
    data <- dplyr::mutate(data, split = "All")
    split <- "split"
  }
  .ranges <- purrr::map(columns, ~ if (is.numeric(data[[.x]])) range(data[.x], na.rm = T) else NULL) %>%
    purrr::set_names(columns)
  data %>%
    waRRior::named_group_split_at(split) %>%
    furrr::future_imap(function(.x, .y) {
      cli::cli_h1("{.y}")
      purrr::map(columns, function(.column) {
        res <- nightowl::summary(.x,
          .column,
          group_by,
          add_caption = FALSE,
          output = "html",
          show_p = FALSE,
          .range = .ranges[[.column]],
          ...
        )
        tibble::tibble(
          Split = .y,
          Variable = attributes(res)$column,
          Method = attributes(res)$method,
          p = nightowl::format_p_value(attributes(res)$p.value),
          Output = shiny::HTML(res)
        )
      }) %>%
        dplyr::bind_rows()
    },
    .options = furrr::furrr_options(seed = TRUE)
    ) %>%
    dplyr::bind_rows() %>%
    nightowl::render_reactable(defaultPageSize = length(columns), minWidth_html = 500, full_width = T)
}
# =================================================
#=================================================
#' @title
#' MISSING_TITLE
#' @description
#' @detail
#' @param
#' @return
#' @export
data_summary_with_plot <- function(...) {
  nightowl::data_summary(
    .summarise_numeric = nightowl::summarise_numeric_forestplot,
    .summarise_categorical = nightowl::summarise_categorical_barplot,
    ...
  )
}
#=================================================
#' @title
#' MISSING_TITLE
#' @description
#' @detail
#' @param
#' @return
#' @export
data_summary <- function(data,
                         column,
                         group_by = NULL,
                         .mean = ggplot2::mean_cl_boot,
                         .summarise_numeric = nightowl::summarise_numeric,
                         .summarise_categorical = nightowl::summarise_categorical,
                         calc_p = TRUE,
                         show_p = TRUE,
                         add_caption = TRUE,
                         wrap_header = TRUE,
                         output = "raw",
                         labels = NULL,
                         .range = NULL,
                         arrange_by = NULL,
                         ...) {
  cli::cli_h2("Calculating summary for {column}")
  .data <- data %>%
    dplyr::select_at(c(column, group_by)) %>%
    dplyr::mutate_if(is.character, factor) %>%
    dplyr::mutate_if(is.factor, forcats::fct_explicit_na)

  if (!is.null(labels)) {
    names(.data) <- nightowl::get_labels(names(.data), labels)
    column <- nightowl::get_labels(column, labels)
    group_by <- nightowl::get_labels(group_by, labels)
  }

  if (!is.null(group_by)) {
    .data <- dplyr::group_by_at(.data, group_by)
  }

  if (is.numeric(.data[[column]])) {
    res <- .summarise_numeric(data = .data, column = column, ...)
    if (calc_p && !is.null(group_by)) {
      test <- .data %>%
        dplyr::group_split() %>%
        purrr::imap(~ .x[[column]]) %>%
        kruskal.test()
      test_method <- "Kruskal-Wallis"
      test_p_value <- test$p.value
    } else {
      test_method <- "None"
      test_p_value <- NA
    }
  } else {
    res <- .summarise_categorical(data = .data, column = column, ...)
    if (calc_p && !is.null(group_by)) {
      cont_table <- .data %>%
        dplyr::select_at(c(column, group_by)) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(group = paste(!!!rlang::syms(group_by))) %>%
        waRRior::drop_columns(group_by) %>%
        droplevels() %>%
        base::table()
      if (min(dim(cont_table)) > 1) {
        test <- chisq.test(cont_table, correct = F)
        test_method <- "Chi-squared test"
        test_p_value <- test$p.value
      } else {
        test_method <- "None"
        test_p_value <- NA
      }
    }
  }

  if (add_caption) {
    .caption <- glue::glue("Summary for {column}")
  } else {
    .caption <- NULL
  }
  if (wrap_header) {
    names(res) <- stringr::str_wrap(names(res), width = 20)
  }
  if (!is.null(arrange_by)) res <- dplyr::arrange(res, !!rlang::sym(arrange_by))
  if (output == "raw") {
    res$column <- column
    res <- dplyr::select(res, column, tidyselect::everything())
  } else {
    res <- nightowl::render_kable(res, caption = .caption, ...)
    if (show_p) {
      res <- res %>%
        kableExtra::add_footnote(glue::glue("{test_method}: {nightowl::format_p_value(test_p_value)}"))
    }
  }
  if (output == "html") {
    res <- res %>% shiny::HTML()
  }
  attributes(res)$column <- column
  attributes(res)$group_by <- group_by
  if (calc_p && !is.null(group_by)) {
    attributes(res)$method <- test_method
    attributes(res)$p.value <- test_p_value
  }
  return(res)
}
# =================================================
#' @title
#' MISSING_TITLE
#' @description
#' @detail
#' @param
#' @return
#' @export
summarise <- function(data,
                      column,
                      calculations = list(`N.` = length),
                      parameters = list(),
                      unnest = TRUE,
                      name_for_column = "Variable",
                      names_sep = ".") {
  stopifnot(is.list(calculations))
  if (rlang::is_expression(parameters)) {
    cli::cli_progress_step("Evaluating parameters")
    parameters <- eval(parameters)
  }

  .calculations <- purrr::imap(calculations, ~ list(
    column = .y,
    calculation = .x,
    params = tryCatch(parameters[[.y]], error = function(e) NULL)
  ))

  .group <- waRRior::get_groups(data)

  res <- purrr::reduce(.calculations, function(.x, .y) {
    cli::cli_progress_step("Calculating {.y$column}")
    .x <- .x %>%
      dplyr::group_split() %>%
      purrr::map(
        function(.thisgroup) {
          res <- dplyr::mutate(.thisgroup, !!rlang::sym(.y$column) := do.call(.y$calculation, c(list(x = .thisgroup[[column]]), .y$params)))
          res
        }
      )
    .x <- dplyr::bind_rows(.x) %>%
      dplyr::group_by_at(.group)
    if (unnest) {
      if (any(class(.x[[.y$column]]) %in% c("tibble", "data.frame", "list"))) {
        cli::cli_progress_step("Unesting {.y$column}")
        .x <- tidyr::unnest(.x, !!rlang::sym(.y$column), names_sep = names_sep, names_repair = "minimal")
      }
    }
    return(.x)
  }, .init = data)

  .new_columns <- names(res)[!names(res) %in% names(data)]

  res <- res %>%
    dplyr::mutate(!!rlang::sym(name_for_column) := column) %>%
    dplyr::select_at(c(.group, name_for_column, .new_columns)) %>%
    dplyr::distinct()
  attributes(res)$parameters <- parameters
  return(res)
}
# =================================================
#' @title
#' MISSING_TITLE
#' @description
#' @detail
#' @param
#' @return
#' @export
summarise_categorical <- function(data,
                                  column,
                                  calculations = list(
                                    `N.` = length,
                                    Freq = nightowl::frequencies
                                  ),
                                  parameters = list(
                                    Freq = list(
                                      add_colors = FALSE)
                                  ),
                                  unnest = TRUE, names_sep = NULL) {
  do.call(nightowl::summarise, as.list(environment()))
}
# =================================================
#' @title
#' MISSING_TITLE
#' @description
#' @detail
#' @param
#' @return
#' @export
summarise_categorical_barplot <- function(data,
                                          column,
                                          calculations = list(
                                            `N.` = length,
                                            Freq = nightowl::frequencies,
                                            Barplot = nightowl::add_barplot
                                          ),
                                          unnest = TRUE, names_sep = NULL) {
  do.call(nightowl::summarise, as.list(environment()))
}
# =================================================
#' @title
#' MISSING_TITLE
#' @description
#' @detail
#' @param
#' @return
#' @export
summarise_numeric <- function(data,
                              column,
                              calculations = list(
                                `N.` = length,
                                `Missing` = function(x) {
                                  return(sum(is.na(x)))
                                },
                                `Outliers` = nightowl::count_outliers,
                                Median = function(x) median(x, na.rm = T),
                                Min = function(x) min(x, na.rm = T),
                                Max = function(x) max(x, na.rm = T),
                                Mean = nightowl::formated_mean
                              ),
                              parameters = list(),
                              unnest = TRUE) {
  do.call(nightowl::summarise, as.list(environment()))
}
#' @title
#' MISSING_TITLE
#' @description
#' @detail
#' @param
#' @return
#' @export
summarise_numeric_forestplot <- function(data,
                                         column,
                                         calculations = list(
                                           `N.` = length,
                                           `Missing` = function(x) {
                                             return(sum(is.na(x)))
                                           },
                                           `Outliers` = nightowl::count_outliers,
                                           Median = function(x) median(x, na.rm = T),
                                           Min = function(x) min(x, na.rm = T),
                                           Max = function(x) max(x, na.rm = T),
                                           Mean = nightowl::formated_mean,
                                           Forestplot = nightowl::add_forestplot
                                         ),
                                         parameters = list(
                                           Forestplot = list(
                                             xintercept = mean(data[[column]], na.rm = T),
                                             xlim = c(
                                               mean(data[[column]], na.rm = T) - IQR(data[[column]], na.rm = T),
                                               mean(data[[column]], na.rm = T) + IQR(data[[column]], na.rm = T)
                                             )
                                           )
                                         ),
                                         unnest = TRUE) {
  do.call(nightowl::summarise, as.list(environment()))
}
# =================================================
#' @title
#' MISSING_TITLE
#' @description
#' @detail
#' @param
#' @return
#' @export
summarise_numeric_violin <- function(data,
                                     column,
                                     calculations = list(
                                       `N.` = length,
                                       Median = function(x) median(x, na.rm = T),
                                       Mean = nightowl::formated_mean,
                                       Violin = nightowl::add_violin
                                     ),
                                     parameters = list(
                                       Violin = list(
                                         theme = picasso::theme_void,
                                         height = 1.5,
                                         ylim = range(data[[column]], na.rm = T)
                                       )
                                     ),
                                     unnest = TRUE) {
  if (rlang::is_expression(parameters)) {
    parameters <- eval(parameters)
  }
  do.call(nightowl::summarise, as.list(environment()))
}
# ===============================================================================
#' @title
#' MISSING_TITLE
#' @description
#' @detail
#' @param
#' @return
#' @export
summarise_numeric_histogram <- function(data,
                                        column,
                                        calculations = list(
                                          `N.` = length,
                                          Median = function(x) median(x, na.rm = T),
                                          Mean = nightowl::formated_mean,
                                          Histogram = nightowl::add_histogram
                                        ),
                                        parameters = list(),
                                        unnest = TRUE) {
  if (rlang::is_expression(parameters)) {
    parameters <- eval(parameters)
  }
  do.call(nightowl::summarise, as.list(environment()))
}

# =================================================
#' @title
#' MISSING_TITLE
#' @description
#' @detail
#' @param
#' @return
#' @export
frequencies <- function(x, output = "print", digits = 1, str_width = 20, add_colors = T) {
  x <- forcats::fct_explicit_na(x)
  counts <- base::table(x)
  if (output == "counts") {
    counts %>%
      as.list() %>%
      tibble::as_tibble() %>%
      return()
  }
  N <- length(x)
  percent <- counts / N * 100
  percent <- round(percent, digits)
  if (output == "percent") {
    percent %>%
      as.list() %>%
      tibble::as_tibble() %>%
      return()
  }
  if (output == "print") {
    print <- as.character(glue::glue("{percent}%({counts})"))
    names(print) <- stringr::str_wrap(names(counts), str_width) %>%
      stringr::str_replace_all("\n", "<br>")
    if(add_colors){
      colors <- MetBrewer::met.brewer("Demuth", length(counts)) %>%
        rev()
    } else {
      colors <- rep("white", length(counts))
    }
    print <- purrr::map2(print, colors, ~ nightowl::style_cell(.x,
      border_color = .y,
      border_style = "solid",
      border_width = "3px",
      text_align = "center",
      padding = "0px",
      margin = "0 0 0 0 px"
    ) %>% unlist())
    print %>%
      as.list() %>%
      tibble::as_tibble() %>%
      return()
  }
}
# =================================================
#' @title
#' MISSING_TITLE
#' @description
#' @detail
#' @param
#' @return
#' @export
n <- function(...) {
  dplyr::n()
}
# =================================================
#' @title
#' MISSING_TITLE
#' @description
#' @detail
#' @param
#' @return
#' @export
formated_mean <- function(x, fun = Hmisc::smean.cl.boot, digits = 1) {
  val <- fun(x)
  val <- round(val, digits)
  glue::glue("{val[1]}({val[2]}-{val[3]})")
}
# =================================================
