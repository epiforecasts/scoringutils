#' @title Documentation template for forecast types
#'
#' @details # Forecast types and input formats
#'
#' Various different forecast types / forecast formats are supported. At the
#' moment, those are:
#' - point forecasts
#' - binary forecasts ("soft binary classification")
#' - Probabilistic forecasts in a quantile-based format (a forecast is
#' represented as a set of predictive quantiles)
#' - Probabilistic forecasts in a sample-based format (a forecast is represented
#' as a set of predictive samples)
#'
#' Forecast types are determined based on the columns present in the input data.
#' Here is an overview of the required format for each forecast type:
#' \if{html}{
#'   \out{<div style="text-align: left">}
#'   \figure{required-inputs.png}{options: style="width:750px;max-width:100\%;"}
#'   \out{</div>}
#' }
#' \if{latex}{
#'   \figure{required-inputs.png}
#' }
#'
#' *All forecast types* require a data.frame or similar with columns `observed`
#' `predicted`, and `model`.
#'
#' *Point forecasts* require a column `observed` of type numeric and a column
#' `predicted` of type numeric.
#'
#' *Binary forecasts* require a column `observed` of type factor with exactly
#' two levels and a column `predicted` of type numeric with probabilities,
#' corresponding to the probability that `observed` is equal to the second
#' factor level. See details [here][brier_score()] for more information.
#'
#' *Quantile-based forecasts* require a column `observed` of type numeric,
#' a column `predicted` of type numeric, and a column `quantile_level` of type
#' numeric with quantile-levels (between 0 and 1).
#'
#' *Sample-based forecasts* require a column `observed` of type numeric,
#' a column `predicted` of type numeric, and a column `sample_id` of type
#' numeric with sample indices.
#'
#' For more information see the vignettes and the example data
#' ([example_quantile], [example_sample_continuous], [example_integer],
#' [example_point()], and [example_binary]).
#'
#' @details # Forecast unit
#'
#' In order to score forecasts, `scoringutils` needs to know which of the rows
#' of the data belong together and jointly form a single forecasts. This is
#' easy e.g. for point forecast, where there is one row per forecast. For
#' quantile or sample-based forecasts, however, there are multiple rows that
#' belong to single forecast.
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
#' usually through the `forecast_unit` argument in [as_forecast()]. This will
#' drop unneeded columns, while making sure that all
#' necessary, 'protected columns' like "predicted" or "observed" are retained.
#'
#' @name forecast_types
#' @keywords internal
NULL

#' Documentation template for check functions
#' @param data A data.frame or similar to be checked
#' @param observed Input to be checked. Should be a numeric vector with the
#'   observed values of size n.
#' @param columns A character vector of column names to check
#' @return Returns TRUE if the check was successful and a string with an
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
