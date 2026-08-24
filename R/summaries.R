# =================================================
#' @title Flexible Data Summarization Function
#' @description
#' A flexible function for computing statistical summaries on grouped data using
#' customizable calculations and parameters. Supports template-based configuration
#' for standardized statistical analyses.
#' @param data A data frame containing the data to summarize
#' @param column Character string specifying the column name to analyze
#' @param template Optional list containing predefined calculations and parameters
#' @param calculations Named list of functions to apply for statistical calculations
#' @param parameters Named list of parameters to pass to calculation functions
#' @param unnest Logical indicating whether to unnest nested results (default: TRUE)
#' @param name_for_column Character string for the variable name column (default: "Variable")
#' @param names_sep Character string separator for nested column names (default: ".")
#' @return A tibble with statistical summaries, preserving grouping structure
#' @export
#' @examples
#' # Basic numeric summary
#' summarise(mtcars, "mpg", calculations = list(Mean = mean, SD = sd))
#' 
#' # Using a predefined template
#' template <- list(calculations = list(N = length, Mean = mean))
#' summarise(iris, "Sepal.Length", template = template)
summarise <- function(data,
                      column,
                      template = NULL,
                      calculations = template$calculations,
                      parameters = template$parameters,
                      unnest = TRUE,
                      name_for_column = "Variable",
                      names_sep = ".") {
  if (is.null(template) &&
    is.null(calculations) &&
    is.null(parameters)) {
    cli::cli_alert_warning("No template or calculations provided, defaulting to just counting.")
    calculations <- list(`N.` = length)
    parameters <- list()
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
#' @title Categorical Variable Summary Template
#' @description
#' Provides a standardized template for summarizing categorical variables,
#' including frequency counts and formatted frequency displays.
#' @param self Not used in this template function
#' @param calculations Named list of calculation functions (default: length and format_frequencies)
#' @param parameters Named list of parameters for calculations (default: no color formatting)
#' @return A list containing calculations and parameters for categorical summary
#' @export
#' @examples
#' # Get categorical summary template
#' template <- summarise_categorical()
#' # Apply to data
#' # summarise(mtcars, "cyl", template = template)
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
#' @title Categorical Variable Summary with Bar Plot Template
#' @description
#' Provides a template for summarizing categorical variables with an integrated
#' bar plot visualization, including frequency counts and colored formatting.
#' @param self Not used in this template function
#' @param calculations Named list of calculation functions (default: length, format_frequencies, add_barplot)
#' @param parameters Named list of parameters for calculations (default: with legend and colors)
#' @param unnest Logical indicating whether to unnest results (default: TRUE)
#' @param names_sep Character separator for column names (default: NULL)
#' @return A list containing calculations and parameters for categorical summary with bar plot
#' @export
#' @examples
#' # Get categorical summary with barplot template
#' template <- summarise_categorical_barplot()
#' # Apply to data
#' # summarise(mtcars, "cyl", template = template)
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
#' @title Numeric Variable Summary Template
#' @description
#' Provides a comprehensive template for summarizing numeric variables,
#' including sample size, missing values, outliers, and descriptive statistics.
#' @param data A data frame (used for context but not directly in calculations)
#' @param column Character string specifying the column name (used for context)
#' @param calculations Named list of statistical functions (default: comprehensive numeric summary)
#' @param parameters Named list of parameters for calculations (default: empty list)
#' @param unnest Logical indicating whether to unnest results (default: TRUE)
#' @return A list containing calculations and parameters for numeric summary
#' @export
#' @examples
#' # Get numeric summary template
#' template <- summarise_numeric(mtcars, "mpg")
#' # Apply to data
#' # summarise(mtcars, "mpg", template = template)
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
#' @title Numeric Variable Summary with Forest Plot Template
#' @description
#' Provides a template for summarizing numeric variables with an integrated
#' forest plot visualization, including descriptive statistics and visual representation.
#' @param data A data frame containing the data for context and plot parameters
#' @param column Character string specifying the column name for analysis
#' @param calculations Named list of statistical functions (default: comprehensive stats plus forest plot)
#' @param parameters Named list of parameters for calculations (default: forest plot with mean reference)
#' @param unnest Logical indicating whether to unnest results (default: TRUE)
#' @return A list containing calculations and parameters for numeric summary with forest plot
#' @export
#' @examples
#' # Get numeric summary with forest plot template
#' template <- summarise_numeric_forestplot(mtcars, "mpg")
#' # Apply to data
#' # summarise(mtcars, "mpg", template = template)
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
#' @title Numeric Variable Summary with Point Range Plot Template
#' @description
#' Provides a template for summarizing numeric variables with an integrated
#' point range plot, showing confidence intervals and central tendencies.
#' @param data A data frame containing the data for context and plot parameters
#' @param column Character string specifying the column name for analysis
#' @param calculations Named list of statistical functions (default: basic stats plus point range plot)
#' @param parameters Named list of parameters for calculations (default: bootstrap confidence intervals)
#' @param unnest Logical indicating whether to unnest results (default: TRUE)
#' @return A list containing calculations and parameters for numeric summary with point range plot
#' @export
#' @examples
#' # Get numeric summary with point range plot template
#' template <- summarise_numeric_pointrange(mtcars, "mpg")
#' # Apply to data
#' # summarise(mtcars, "mpg", template = template)
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
#' @title Numeric Variable Summary with Violin Plot Template
#' @description
#' Provides a template for summarizing numeric variables with an integrated
#' violin plot visualization, showing distribution density and summary statistics.
#' @param self Object containing data and column information for plot parameters
#' @param calculations Named list of statistical functions (default: basic stats plus violin plot)
#' @param parameters Named list of parameters for calculations (default: violin plot with void theme)
#' @return A list containing calculations and parameters for numeric summary with violin plot
#' @export
#' @examples
#' # Get numeric summary with violin plot template
#' # template <- summarise_numeric_violin(summary_obj)
#' # Apply to data
#' # summarise(mtcars, "mpg", template = template)
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
#' @title Numeric Variable Summary with Histogram Template
#' @description
#' Provides a template for summarizing numeric variables with an integrated
#' histogram visualization, showing distribution shape and summary statistics.
#' @param self Object containing data and column information for plot parameters
#' @param data A data frame (parameter maintained for consistency)
#' @param column Character string specifying column name (parameter maintained for consistency)
#' @param calculations Named list of statistical functions (default: comprehensive stats plus histogram)
#' @param parameters Named list of parameters for calculations (default: histogram with data range)
#' @param unnest Logical indicating whether to unnest results (default: TRUE)
#' @return A list containing calculations and parameters for numeric summary with histogram
#' @export
#' @examples
#' # Get numeric summary with histogram template
#' # template <- summarise_numeric_histogram(summary_obj, mtcars, "mpg")
#' # Apply to data
#' # summarise(mtcars, "mpg", template = template)
summarise_numeric_histogram <- function(self,
                                        data,
                                        column,
                                        calculations = list(
                                          `N.` = function(x) sum(!is.na(x)),
                                          Median = function(x) median(x, na.rm = T),
                                          Mean = nightowl::formated_mean,
                                          Min = function(x) min(x, na.rm = T),
                                          Max = function(x) max(x, na.rm = T),
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
#' @title Calculate Percentages for Categorical Data
#' @description
#' Calculates percentages for each level of a categorical variable,
#' handling missing values explicitly as a separate category.
#' @param x A vector of categorical data (factor or character)
#' @param N Total sample size for percentage calculation (default: length of x)
#' @param digits Number of decimal places for rounding (default: 1)
#' @return A named numeric vector of percentages for each category
#' @export
#' @examples
#' # Calculate percentages for a categorical variable
#' calc_percentage(c("A", "B", "A", "C", NA))
#' 
#' # With custom sample size
#' calc_percentage(c("A", "B", "A"), N = 100)
calc_percentage <- function(x, N = length(x), digits = 1) {
  x <- forcats::fct_explicit_na(x)
  counts <- base::table(x)
  percent <- counts / N * 100
  percent <- round(percent, digits)
  return(percent)
}
# =================================================
#' @title Format Frequency Tables with Multiple Output Options
#' @description
#' Formats frequency tables for categorical variables with multiple output formats,
#' including counts, percentages, and formatted print output with optional styling.
#' @param x A vector of categorical data (factor or character)
#' @param N Total sample size for percentage calculation (default: length of x)
#' @param output Output format: "print", "counts", or "percent" (default: "print")
#' @param digits Number of decimal places for percentages (default: 1)
#' @param str_width Maximum width for category names (default: from NightowlOptions)
#' @param add_legend Logical indicating whether to add color legend (default: FALSE)
#' @param add_colors Logical indicating whether to add color formatting (default: TRUE)
#' @param colors Function to generate colors (default: from NightowlOptions)
#' @return A tibble with formatted frequencies in the specified output format
#' @export
#' @examples
#' # Basic frequency formatting
#' format_frequencies(c("A", "B", "A", "C"))
#' 
#' # Get raw counts
#' format_frequencies(c("A", "B", "A"), output = "counts")
#' 
#' # Get percentages only
#' format_frequencies(c("A", "B", "A"), output = "percent")
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
      if (add_legend) {
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
#' @title Count Observations Helper Function
#' @description
#' A simple wrapper around dplyr::n() for counting observations in groups.
#' Useful in summarization contexts where group-wise counts are needed.
#' @param ... Additional arguments (not used, maintained for compatibility)
#' @return Integer count of observations in the current group
#' @export
#' @examples
#' # Count observations in grouped data
#' # mtcars %>% group_by(cyl) %>% summarise(count = n())
n <- function(...) {
  dplyr::n()
}
# =================================================
#' @title Formatted Mean with Confidence Limits
#' @description
#' Calculates the mean with confidence limits and formats the result as a tibble
#' with separate columns for the mean estimate and confidence interval.
#' @param x A numeric vector
#' @param fun Function to calculate mean and confidence limits (default: Hmisc::smean.cl.boot)
#' @param digits Number of decimal places for rounding (default: 2)
#' @return A tibble with columns 'Mean' (numeric) and 'CL' (formatted confidence interval string)
#' @export
#' @examples
#' # Calculate formatted mean with bootstrap confidence limits
#' formated_mean(rnorm(100))
#' 
#' # Using different confidence limit function
#' formated_mean(rnorm(50), fun = Hmisc::smean.cl.normal)
formated_mean <- function(x, fun = Hmisc::smean.cl.boot, digits = 2) {
  val <- fun(x)
  val <- round(val, digits)
  tibble::tibble(Mean = val[1], CL = glue::glue("[{val[2]}, {val[3]}]"))
}
