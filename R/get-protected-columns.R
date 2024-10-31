#' @title Get protected columns from data
#'
#' @description Helper function to get the names of all columns in a data frame
#' that are protected columns.
#'
#' @inheritParams as_forecast_doc_template
#'
#' @return
#' A character vector with the names of protected columns in the data.
#' If data is `NULL` (default) then it returns a list of all columns that are
#' protected in scoringutils.
#'
#' @keywords internal
get_protected_columns <- function(data = NULL) {

  protected_columns <- c(
    "predicted", "observed", "sample_id", "quantile_level", "upper", "lower",
    "pit_value", "interval_range", "boundary", "predicted_label",
    "interval_coverage", "interval_coverage_deviation",
    "quantile_coverage", "quantile_coverage_deviation",
    grep("_relative_skill$", names(data), value = TRUE),
    grep("coverage_", names(data), fixed = TRUE, value = TRUE)
  )

  if (is.null(data)) {
    return(protected_columns)
  }

  # only return protected columns that are present
  datacols <- colnames(data)
  protected_columns <- intersect(
    datacols,
    protected_columns
  )

  return(protected_columns)
}
