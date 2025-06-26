# =================================================
#' @title Apply calculations to grouped data columns
#' @description
#' Applies a list of calculations to a specific column across grouped data,
#' returning summary statistics with proper grouping structure preserved.
#' @param data A data frame or tibble, possibly grouped
#' @param column String. The column name to apply calculations to
#' @param calculations Named list of functions to apply. Default is `list("N." = length)`
#' @param parameters Named list of parameters for each calculation function
#' @param unnest Logical. Whether to unnest nested results. Default TRUE
#' @param name_for_column String. Name for the variable column. Default "Variable"
#' @param names_sep String. Separator for unnested column names. Default "."
#' @return A tibble with applied calculations, maintaining group structure
#' @export
summarise <- function(data,
                      column,
                      calculations = list(`N.` = length),
                      parameters = list(),
                      unnest = TRUE,
                      name_for_column = "Variable",
                      names_sep = ".") {
  stopifnot(is.list(calculations))
  # Remove unsafe eval - parameters should already be a list
  if (!is.list(parameters)) {
    stop("parameters must be a list, not an expression")
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
#' @title Configure calculations for categorical variable summaries
#' @description
#' Provides default calculations and parameters for summarizing categorical variables,
#' including counts and formatted frequencies.
#' @param calculations Named list of calculation functions for categorical data
#' @param parameters Named list of parameters for each calculation function
#' @return A list containing calculations and parameters for categorical summaries
#' @export
summarise_categorical <- function(calculations = list(
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
#' @title Configure categorical summaries with barplot visualization
#' @description
#' Provides calculations and parameters for categorical variable summaries that include
#' barplot visualizations alongside counts and frequencies.
#' @param self The calling object context
#' @param calculations Named list of calculation functions including barplot generation
#' @param parameters Named list of parameters for each calculation, especially formatting
#' @param unnest Logical. Whether to unnest nested results. Default TRUE
#' @param names_sep String. Separator for column names when unnesting. Default NULL
#' @return A list containing calculations and parameters for categorical summaries with barplots
#' @export
summarise_categorical_barplot <- function(self,
                                          calculations = list(
                                            `N.` = length,
                                            Freq = nightowl::format_frequencies,
                                            Barplot = nightowl::add_barplot
                                          ),
                                          parameters = list(
                                            Freq = list(
                                              add_colors = TRUE
                                            )
                                          ),
                                          unnest = TRUE, names_sep = NULL) {
  return(list(calculations = calculations, parameters = parameters))
}
# =================================================
#' @title Configure calculations for numeric variable summaries
#' @description
#' Provides default calculations for summarizing numeric variables including
#' counts, missing values, outliers, and descriptive statistics.
#' @param data A data frame containing the numeric variable
#' @param column String. The name of the numeric column to summarize
#' @param calculations Named list of statistical functions to apply
#' @param parameters Named list of parameters for each calculation function
#' @param unnest Logical. Whether to unnest nested results. Default TRUE
#' @return A list containing calculations and parameters for numeric summaries
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
#' @title Configure numeric summaries with forest plot visualization
#' @description
#' Provides calculations for numeric variables that include forest plot visualizations
#' alongside standard descriptive statistics.
#' @param data A data frame containing the numeric variable
#' @param column String. The name of the numeric column to summarize
#' @param calculations Named list of functions including forest plot generation
#' @param parameters Named list including forest plot parameters (xlim, xintercept)
#' @param unnest Logical. Whether to unnest nested results. Default TRUE
#' @return A list containing calculations and parameters for numeric summaries with forest plots
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
#' @title Configure numeric summaries with point range visualization
#' @description
#' Provides calculations for numeric variables that include point range plots
#' showing confidence intervals alongside descriptive statistics.
#' @param data A data frame containing the numeric variable
#' @param column String. The name of the numeric column to summarize
#' @param calculations Named list including point range plot generation functions
#' @param parameters Named list including point range parameters (xlim, xintercept, fun_data)
#' @param unnest Logical. Whether to unnest nested results. Default TRUE
#' @return A list containing calculations and parameters for numeric summaries with point ranges
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
#' @title Configure numeric summaries with violin plot visualization
#' @description
#' Provides calculations for numeric variables that include violin plots
#' showing distribution density alongside descriptive statistics.
#' @param self The calling object context containing data and column information
#' @param calculations Named list including violin plot generation functions
#' @param parameters Named list including violin plot parameters (theme, ylim)
#' @return A list containing calculations and parameters for numeric summaries with violin plots
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
                                         theme = ggplot2::theme_void,
                                         ylim = range(self$data[[self$column]], na.rm = T)
                                       )
                                     )) {
  return(list(calculations = calculations, parameters = parameters))
}

# ===============================================================================
#' @title Configure numeric summaries with histogram visualization
#' @description
#' Provides calculations for numeric variables that include histogram visualizations
#' showing distribution patterns alongside descriptive statistics.
#' @param self The calling object context containing data and column information
#' @param data A data frame containing the numeric variable
#' @param column String. The name of the numeric column to summarize
#' @param calculations Named list including histogram generation functions
#' @param parameters Named list including histogram parameters (xlim)
#' @param unnest Logical. Whether to unnest nested results. Default TRUE
#' @return A list containing calculations and parameters for numeric summaries with histograms
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
#' @title Calculate percentages for factor levels
#' @description
#' Calculates the percentage distribution of factor levels in a vector,
#' handling missing values explicitly.
#' @param x A vector, typically factor or character
#' @param N Integer. Total count for percentage calculation. Default is length(x)
#' @param digits Integer. Number of decimal places for rounding. Default 1
#' @return A named numeric vector of percentages for each factor level
#' @export
calc_percentage <- function(x, N = length(x), digits = 1) {
  x <- forcats::fct_na_value_to_level(x, level = "(Missing)")
  counts <- base::table(x)
  percent <- counts / N * 100
  percent <- round(percent, digits)
  return(percent)
}
# =================================================
#' @title Format frequency distributions with optional styling
#' @description
#' Formats frequency distributions as counts, percentages, or styled HTML output
#' with optional color coding for visualization.
#' @param x A vector to calculate frequencies for
#' @param N Integer. Total count for percentage calculation. Default is length(x)
#' @param output String. Output format: "print", "counts", or "percent". Default "print"
#' @param digits Integer. Decimal places for percentages. Default 1
#' @param str_width Integer. Maximum width for text wrapping
#' @param add_colors Logical. Whether to add color styling. Default TRUE
#' @param colors Function. Color palette function for styling
#' @return A tibble with formatted frequency information
#' @export
format_frequencies <- function(x,
                               N = length(x),
                               output = "print",
                               digits = 1,
                               str_width = get_nightowl_options()$get_header_width(),
                               add_colors = T, colors = get_nightowl_options()$get_colors) {
  x <- forcats::fct_na_value_to_level(x, level = "(Missing)")
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
      shiny::div(
        "",
        style = glue::glue("background-color:{x}; width:10px; height:10px; border-radius:50%;")
      )
    })
    print <- purrr::map2(print, colors, ~ nightowl::style_cell(.x,
      # background_color = ifelse(add_colors, .y, "white"),
      # color = ifelse(nightowl::is_dark(.y) && add_colors, "white", "black"),
      # border_style = "solid",
      # border_width = "3px",
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
#' @title Count rows in current group
#' @description
#' Wrapper function for dplyr::n() to count the number of rows in the current group.
#' @param ... Additional arguments (currently unused)
#' @return Integer count of rows in current group
#' @export
n <- function(...) {
  dplyr::n()
}
# =================================================
#' @title Calculate formatted mean with confidence limits
#' @description
#' Calculates the mean with confidence limits using bootstrap methods,
#' returning a formatted tibble with mean and confidence interval.
#' @param x Numeric vector to calculate mean and confidence limits for
#' @param fun Function for calculating mean and confidence limits. Default Hmisc::smean.cl.boot
#' @param digits Integer. Number of decimal places for rounding. Default 2
#' @return A tibble with Mean and CL (confidence limits) columns
#' @export
formated_mean <- function(x, fun = Hmisc::smean.cl.boot, digits = 2) {
  val <- fun(x)
  val <- round(val, digits)
  tibble::tibble(Mean = val[1], CL = glue::glue("[{val[2]}, {val[3]}]"))
}
