#===============================================================================
#' Summry table
#' Table to display data that is being used in plots
#' @export
summary_table <- function(DATA,
                          x,
                          y,
                          group = NULL,
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
  .lty <- purrr::reduce(facet_cols, ~rtables::split_cols_by(.x, .y), .init = .lty)
  .lty <- purrr::reduce(facet_rows, ~rtables::split_rows_by(.x, .y), .init = .lty)
  .lty <- rtables::split_cols_by(.lty, x)
  if(!is.null(group)) .lty <- rtables::split_rows_by(.lty, group)
  .lty <- tern::summarize_vars(.lty, y, denom = denom)
  rtables::build_table(.lty, DATA)
}
