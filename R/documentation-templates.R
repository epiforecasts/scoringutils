#' @title General information on creating a `forecast` object
#'
#' @description
#' Process and validate a data.frame (or similar) or similar with forecasts
#' and observations. If the input passes all input checks, those functions will
#' be converted to a `forecast` object. A forecast object is a `data.table` with
#' a class `forecast` and an additional class that depends on the forecast type.
#'
#' The arguments `observed`, `predicted`, etc. make it possible to rename
#' existing columns of the input data to match the required columns for a
#' forecast object. Using the argument `forecast_unit`, you can specify
#' the columns that uniquely identify a single forecast (and thereby removing
#' other, unneeded columns. See section "Forecast Unit" below for details).
#'
#' @param data A data.frame (or similar) with predicted and observed values.
#'   See the details section of for additional information
#'   on the required input format.
#' @param forecast_unit (optional) Name of the columns in `data` (after
#'   any renaming of columns) that denote the unit of a
#'   single forecast. See [get_forecast_unit()] for details.
#'   If `NULL` (the default), all columns that are not required columns are
#'   assumed to form the unit of a single forecast. If specified, all columns
#'   that are not part of the forecast unit (or required columns) will be removed.
#' @param observed (optional) Name of the column in `data` that contains the
#'   observed values. This column will be renamed to "observed".
#' @param predicted (optional) Name of the column in `data` that contains the
#'   predicted values. This column will be renamed to "predicted".
#' @inheritSection forecast_types Forecast unit
#' @keywords as_forecast
#' @name as_forecast_doc_template
NULL


#' @title Documentation template for forecast types
#'
#' @details # Forecast unit
#'
#' In order to score forecasts, `scoringutils` needs to know which of the rows
#' of the data belong together and jointly form a single forecasts. This is
#' easy e.g. for point forecast, where there is one row per forecast. For
#' quantile or sample-based forecasts, however, there are multiple rows that
#' belong to a single forecast.
#'
#' The *forecast unit* or *unit of a single forecast* is then described by the
#' combination of columns that uniquely identify a single forecast.
#' For example, we could have forecasts made by different models in various
#' locations at different time points, each for several weeks into the future.
#' The forecast unit could then be described as
#' `forecast_unit = c("model", "location", "forecast_date", "forecast_horizon")`.
#' `scoringutils` automatically tries to determine the unit of a single
#' forecast. It uses all existing columns for this, which means that no columns
#' must be present that are unrelated to the forecast unit. As a very simplistic
#' example, if you had an additional row, "even", that is one if the row number
#' is even and zero otherwise, then this would mess up scoring as `scoringutils`
#' then thinks that this column was relevant in defining the forecast unit.
#'
#' In order to avoid issues, we recommend setting the forecast unit explicitly,
#' using the `forecast_unit` argument. This will simply drop unneeded columns,
#' while making sure that all necessary, 'protected columns' like "predicted"
#' or "observed" are retained.
#'
#' @name forecast_types
#' @keywords internal
NULL

#' Documentation template for check functions
#' @param data A data.frame or similar to be checked
#' @param observed Input to be checked. Should be a numeric vector with the
#'   observed values of size n.
#' @param columns A character vector of column names to check
#' @returns Returns TRUE if the check was successful and a string with an
#'   error message otherwise.
#' @name document_check_functions
#' @keywords internal
NULL

#' Documentation template for assert functions
#' @param observed Input to be checked. Should be a numeric vector with the
#'   observed values of size n.
#' @returns Returns NULL invisibly if the assertion was successful and throws an
#'   error otherwise.
#' @name document_assert_functions
#' @keywords internal
NULL

#' Documentation template for test functions
#' @returns Returns TRUE if the check was successful and FALSE otherwise
#' @name document_test_functions
#' @keywords internal
NULL

#' Illustration of required inputs for sample-based forecasts
#' @details # Input format
#' \if{html}{
#'   \out{<div style="text-align: left">}
#'   \figure{metrics-sample.png}{options: style="width:750px;max-width:100\%;"}
#'   \out{</div><p>}
#'   Overview of required input format for sample-based forecasts
#' }
#' \if{latex}{
#'   \figure{metrics-sample.png}
#' }
#' @name illustration-input-metric-sample
#' @keywords internal
NULL

#' Illustration of required inputs for binary and point forecasts
#' @details # Input format
#' \if{html}{
#'   \out{<div style="text-align: left">}
#'   \figure{metrics-binary-point.png}{options: style="width:750px;max-width:100\%;"}
#'   \out{</div><p>}
#'   Overview of required input format for binary and point forecasts
#' }
#' \if{latex}{
#'   \figure{metrics-binary-point.png}
#' }
#' @name illustration-input-metric-binary-point
#' @keywords internal
NULL

#' Illustration of required inputs for quantile-based forecasts
#' @details # Input format
#' \if{html}{
#'   \out{<div style="text-align: left">}
#'   \figure{metrics-quantile.png}{options: style="width:750px;max-width:100\%;"}
#'   \out{</div><p>}
#'   Overview of required input format for quantile-based forecasts
#' }
#' \if{latex}{
#'   \figure{metrics-quantile.png}
#' }
#' @name illustration-input-metric-quantile
#' @keywords internal
NULL

#' Illustration of required inputs for nominal forecasts
#' @details # Input format
#' \if{html}{
#'   \out{<div style="text-align: left">}
#'   \figure{metrics-nominal.png}{options: style="width:750px;max-width:100\%;"}
#'   \out{</div><p>}
#'   Overview of required input format for nominal forecasts
#' }
#' \if{latex}{
#'   \figure{metrics-nominal.png}
#' }
#' @name illustration-input-metric-nominal
#' @keywords internal
NULL
