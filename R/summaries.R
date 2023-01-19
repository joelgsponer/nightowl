# =================================================
#' @title
#' MISSING_TITLE
#' @export
summarise <- function(data,
                      column,
                      template = NULL,
                      calculations = template$calculations,
                      parameters = template$parameters,
                      unnest = TRUE,
                      name_for_column = "Variable",
                      names_sep = ".") {
  if(is.null(template) &&
     is.null(calculations) &&
     is.null(parameters)) {
    cli::cli_alert_warning("No template or calculations provided, defaulting to just counting.")
    calculations = list(`N.` = length)
    parameters = list()
  }
  stopifnot(is.list(calculations))
  if (rlang::is_expression(parameters)) {
    parameters <- eval(parameters)
  }

  .calculations <- purrr::imap(calculations, ~ list(
    column = .y,
    calculation = .x,
    params = tryCatch(parameters[[.y]], error = function(e) NULL)
  ))

  .group <- waRRior::get_groups(data)

  res <- purrr::reduce(.calculations, function(.x, .y) {
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
      if (any(class(.x[[.y$column]]) %in% c("tibble", "data.frame", "list")) &&
        !inherits(.x[[.y$column]], "NightowlPlots")) {
        .x <- tidyr::unnest(.x, !!rlang::sym(.y$column), names_sep = names_sep, names_repair = "minimal")
        names(.x) <- stringr::str_replace_all(names(.x), glue::glue("{.y$column}."), "")
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
#' @export
summarise_categorical <- function(self,
                                  calculations = list(
                                    `N.` = length,
                                    Freq = nightowl::format_frequencies
                                  ),
                                  parameters = list(
                                    Freq = list(
                                      add_colors = FALSE
                                    )
                                  )) {
  return(list(calculations = calculations, parameters = parameters))
}
# =================================================
#' @title
#' MISSING_TITLE
#' @export
summarise_categorical_barplot <- function(self,
                                          calculations = list(
                                            `N.` = length,
                                            Freq = nightowl::format_frequencies,
                                            Barplot = nightowl::add_barplot
                                          ),
                                          parameters = list(
                                            Freq = list(
                                              add_legend = TRUE,
                                              add_colors = TRUE
                                            )
                                          ),
                                          unnest = TRUE, names_sep = NULL) {
  return(list(calculations = calculations, parameters = parameters))
}
# =================================================
#' @title
#' MISSING_TITLE
#' @export
summarise_numeric <- function(data,
                              column,
                              calculations = list(
                                `N.` = length,
                                `Missing` = function(x) {
                                  return(sum(is.na(x)))
                                },
                                `Extreme Values` = nightowl::count_outliers,
                                Median = function(x) median(x, na.rm = T),
                                Min = function(x) min(x, na.rm = T),
                                Max = function(x) max(x, na.rm = T),
                                Mean = nightowl::formated_mean
                              ),
                              parameters = list(),
                              unnest = TRUE) {
  return(list(calculations = calculations, parameters = parameters))
}
# ===============================================================================
#' @title
#' MISSING_TITLE
#' @export
summarise_numeric_forestplot <- function(data,
                                         column,
                                         calculations = list(
                                           `N.` = length,
                                           `Missing` = function(x) {
                                             return(sum(is.na(x)))
                                           },
                                           `Extreme Values` = nightowl::count_outliers,
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
  return(list(calculations = calculations, parameters = parameters))
}
# ===============================================================================
#' @title
#' MISSING_TITLE
#' @export
summarise_numeric_pointrange <- function(data,
                                         column,
                                         calculations = list(
                                           `N.` = length,
                                           Median = function(x) median(x, na.rm = T),
                                           Mean = nightowl::formated_mean,
                                           Pointrange = nightowl::add_inline_pointrange
                                         ),
                                         parameters = list(
                                           Pointrange = list(
                                             fun_data = ggplot2::mean_cl_boot,
                                             xintercept = mean(data[[column]], na.rm = T),
                                             xlim = c(
                                               min(data[[column]], na.rm = T),
                                               max(data[[column]], na.rm = T)
                                             )
                                           )
                                         ),
                                         unnest = TRUE) {
  return(list(calculations = calculations, parameters = parameters))
}
# =================================================
#' @title
#' MISSING_TITLE
#' @export
summarise_numeric_violin <- function(self,
                                     calculations = list(
                                       `N.` = length,
                                       Median = function(x) median(x, na.rm = T),
                                       Mean = nightowl::formated_mean,
                                       Violin = nightowl::add_violin
                                     ),
                                     parameters = list(
                                       Violin = list(
                                         theme = picasso::theme_void,
                                         ylim = range(self$data[[self$column]], na.rm = T)
                                       )
                                     )) {
  return(list(calculations = calculations, parameters = parameters))
}

# ===============================================================================
#' @title
#' MISSING_TITLE
#' @export
summarise_numeric_histogram <- function(self,
                                        data,
                                        column,
                                        calculations = list(
                                          `N.` = function(x) sum(!is.na(x)),
                                          Median = function(x) median(x, na.rm = T),
                                          Mean = nightowl::formated_mean,
                                          Histogram = nightowl::add_inline_histogram
                                        ),
                                        parameters = list(
                                          Histogram = list(
                                            xlim = range(self$data[[self$column]], na.rm = T) + c(-0.1, 0.1)
                                          )
                                        ),
                                        unnest = TRUE) {
  return(list(calculations = calculations, parameters = parameters))
}
# =================================================
#' @title
#' MISSING_TITLE
#' @export
calc_percentage <- function(x, N = length(x), digits = 1) {
  x <- forcats::fct_explicit_na(x)
  counts <- base::table(x)
  percent <- counts / N * 100
  percent <- round(percent, digits)
  return(percent)
}
# =================================================
#' @expo'
#' @title
#' MISSING_TITLE
#' @export
format_frequencies <- function(x,
                               N = length(x),
                               output = "print",
                               digits = 1,
                               str_width = NightowlOptions$get_header_width(),
                               add_legend = FALSE,
                               add_colors = T, 
                               colors = NightowlOptions$get_colors) {
  x <- forcats::fct_explicit_na(x)
  counts <- base::table(x)
  if (output == "counts") {
    counts %>%
      as.list() %>%
      tibble::as_tibble() %>%
      return()
  }
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
    colors <- colors(n = length(counts), missing = "(Missing)" %in% names(counts))
    legend <- purrr::map(colors, function(x) {
      if(add_legend){
        shiny::div(
          "",
          style = glue::glue("background-color:{x}; width:10px; height:10px; border-radius:50%;")
        )
      } else {
        ""
      }
    })
    print <- purrr::map2(print, colors, ~ nightowl::style_cell(.x,
      # background_color = ifelse(add_colors, .y, "white"),
      # color = ifelse(picasso::is_dark(.y) && add_colors, "white", "black"),
      # border_style = "solid",
      # border_width = "3px" ,
      # font_weight = ifelse(add_colors, "bold", "normal"),
      text_align = "center",
      padding_top = "0px",
      padding_bottom = "0px",
      padding_left = ifelse(add_colors, "3px", "0px"),
      padding_right = ifelse(add_colors, "3px", "0px"),
      border_radius = "5px",
      margin = "0 0 0 0 px"
    ) %>% unlist())
    print <- purrr::map2(legend, print, function(x, y) {
      shiny::div(
        style = glue::glue("
          width:max-content;
          display: flex;
          flex-direction: row;
          justify-content: flex-start;
          align-content: center;
          align-items: center;
          "),
        x,
        shiny::HTML(y)
      ) %>%
        as.character()
    }) %>%
      purrr::set_names(names(print))

    print %>%
      as.list() %>%
      tibble::as_tibble() %>%
      return()
  }
}


# =================================================
#' @title
#' MISSING_TITLE
#' @export
n <- function(...) {
  dplyr::n()
}
# =================================================
#' @title
#' MISSING_TITLE
#' @export
formated_mean <- function(x, fun = Hmisc::smean.cl.boot, digits = 2) {
  val <- fun(x)
  val <- round(val, digits)
  tibble::tibble(Mean = val[1], CL = glue::glue("[{val[2]}, {val[3]}]"))
}
