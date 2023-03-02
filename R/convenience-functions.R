#' @title Add transformations
#'
#' @description Add transformations of the forecasts and observations for
#' later scoring. For more information on why this might be desirable, check
#' out the linked reference.
#'
#' @inheritParams score
#' @return A data.table that includes the original data as well as a
#' transformation of the original data. There will be one additional column,
#' 'scale', present which will be set to "natural" for the untransformed
#' forecasts.
#'
#' @importFrom data.table ':=' is.data.table copy
#' @author Nikos Bosse \email{nikosbosse@@gmail.com}
#' @export
#' @references Transformation of forecasts for evaluating predictive
#' performance in an epidemiological context
#' Nikos I. Bosse, Sam Abbott, Anne Cori, Edwin van Leeuwen, Johannes Bracher,
#' Sebastian Funk
#' medRxiv 2023.01.23.23284722
#' \doi{https://doi.org/10.1101/2023.01.23.23284722}
#' <https://www.medrxiv.org/content/10.1101/2023.01.23.23284722v1> # nolint

#' @keywords check-forecasts
#' @examples
#' add_transformation(example_quantile)

add_transformation <- function(data,
                               fun = function(x) {log(x + 1)},
                               label = "log",
                               ...) {

  data <- as.data.table(data)
  transformed_data <- data.table::copy(data)

  data[, scale := "natural"]
  transformed_data[, scale := label]

  transformed_data[, prediction := fun(prediction, ...)]
  transformed_data[, true_value := fun(true_value, ...)]

  out <- rbind(data, transformed_data)

  return(out)
}




#' @title Set unit of a single forecast manually
#'
#' @description Helper function to set the unit of a single forecast manually.
#' This simple function keeps the columns specified in `forecast_unit` (plus
#' additional protected columns, e.g. for true values, predictions or quantile
#' levels) and removes duplicate rows.
#'
#' @inheritParams score
#' @return A data.table with only those columns kept that are relevant to
#' scoring or denote the unit of a single forecast as specified by the user.
#'
#' @importFrom data.table ':=' is.data.table copy
#' @author Nikos Bosse \email{nikosbosse@@gmail.com}
#' @export
#' @references Transformation of forecasts for evaluating predictive
#' performance in an epidemiological context
#' Nikos I. Bosse, Sam Abbott, Anne Cori, Edwin van Leeuwen, Johannes Bracher,
#' Sebastian Funk
#' medRxiv 2023.01.23.23284722
#' \doi{https://doi.org/10.1101/2023.01.23.23284722}
#' <https://www.medrxiv.org/content/10.1101/2023.01.23.23284722v1> # nolint

#' @keywords data-handling
#' @examples
#'

set_forecast_unit <- function(data,
                              forecast_unit = NULL) {

  datacols <- colnames(data)
  missing <- forecast_unit[!(forecast_unit %in% datacols)]

  if (length(missing) > 0) {
    warning(
      paste0(
        "Column(s) '",
        missing,
        "' are not columns of the data"
      )
    )
  }

  keep_cols <- intersect(
    datacols,
    get_protected_columns(data)
  )
  keep_cols <- c(keep_cols, forecast_unit)
  out <- unique(data[, .SD, .SDcols = keep_cols])[]
  return(out)
}





