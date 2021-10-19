# ===============================================================================
#' Summry table
#' Table to display data that is being used in plots. All character columns
#' are converted into factors.
#' @param DATA
#' @param x Split columns by
#' @param y Variable to summarise
#' @param group Grouping variable
#' @param group_split Split Groups be split into "cols" or "rows"
#' @param facet_cols Split columns by, additionally  to x.
#' @param facet_rows Split rows by
#' @param remove_missing If TRUE any missing values in character or factor
#'   columns are remove. If FALSE they are recoded as (Missing).
#' @param label_width Width of labels for text wraping
#' @param denom Denominator to be used for frequency calculations.
#'   Possible values are "n", "N_col", "N_row".
#'   See also \link{[https://docs.roche.com/doc/tern/v0.7.5/reference/summarize_variables.html]{tern::summarize_vars()}}
#' @export
summary_table <- function(DATA,
                          x,
                          y,
                          group = NULL,
                          group_split = "rows",
                          facet_cols = NULL,
                          facet_rows = NULL,
                          remove_missing = FALSE,
                          label_width = 20,
                          denom = "n",
                          ...) {
  #*******************************************************************************
  # Drop columns that are not needed
  DATA <- DATA %>%
    dplyr::select_at(c(x, y, group, facet_cols, facet_rows))
  #*******************************************************************************
  # Drop missing values
  DATA <- nightowl::prepare_data_for_plotting(DATA, remove_missing = remove_missing)
  # Data preparation
  DATA <- nightowl::add_text_wraping(DATA, width = label_width)
  #*******************************************************************************
  # Build table
  .lty <- rtables::basic_table()
  .lty <- purrr::reduce(facet_cols, ~ rtables::split_cols_by(.x, .y), .init = .lty)
  .lty <- purrr::reduce(facet_rows, ~ rtables::split_rows_by(.x, .y), .init = .lty)
  .lty <- rtables::split_cols_by(.lty, x)
  if (!is.null(group) && group_split == "rows") .lty <- rtables::split_rows_by(.lty, group)
  if (!is.null(group) && group_split == "cols") .lty <- rtables::split_cols_by(.lty, group)
  .lty <- tern::summarize_vars(.lty, y, denom = denom)
  rtables::build_table(.lty, DATA)
}
