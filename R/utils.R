#' @title Available metrics in scoringutils
#'
#' @return A vector with the name of all available metrics
#' @export
#' @keywords info

available_metrics <- function() {
  return(unique(scoringutils::metrics$Name))
}


#' Safely delete Columns From a Data.table
#'
#' @description take a vector of column names and delete the columns if they
#' are present in the data.table
#' @param df A data.table or data.frame from which columns shall be deleted
#' @param cols_to_delete character vector with names of columns to be deleted
#' @param make_unique whether to make the data set unique after removing columns
#' @importFrom data.table as.data.table
#' @return A data.table
#'
#' @keywords internal
#'
delete_columns <- function(df, cols_to_delete, make_unique = FALSE) {
  df <- data.table::as.data.table(df)
  delete_columns <- names(df)[names(df) %in% cols_to_delete]
  if (length(delete_columns) > 0) {
    if (make_unique) {
      df <- unique(df[, eval(delete_columns) := NULL])
    } else {
      df <- df[, eval(delete_columns) := NULL]
    }
  }
  return(df)
}



