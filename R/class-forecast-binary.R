#' @title Create a `forecast` object for binary forecasts
#' @inherit as_forecast_doc_template params description
#' @details
#' # Required input
#'
#' The input needs to be a data.frame or similar with the following columns:
#' - `observed`: `factor` with exactly two levels representing the observed
#'   values. The highest factor level is assumed to be the reference level.
#'   This means that corresponding value in `predicted` represent the
#'   probability that the observed value is equal to the highest factor level.
#' - `predicted`: `numeric` with predicted probabilities, representing
#'   the probability that the corresponding value in `observed` is equal to
#'   the highest available factor level.
#'
#' For convenience, we recommend an additional column `model` holding the name
#' of the forecaster or model that produced a prediction, but this is not
#' strictly necessary.
#'
#' See the [example_binary] data set for an example.
#' @inheritSection forecast_types Forecast unit
#' @param ... Unused
#' @returns A `forecast` object of class `forecast_binary`
#' @family functions to create forecast objects
#' @export
#' @keywords as_forecast transform
#' @examples
#' as_forecast_binary(
#'   example_binary,
#'   predicted = "predicted",
#'   forecast_unit = c("model", "target_type", "target_end_date",
#'                     "horizon", "location")
#' )
as_forecast_binary <- function(data, ...) {
  UseMethod("as_forecast_binary")
}

#' @rdname as_forecast_binary
#' @export
#' @method as_forecast_binary default
#' @importFrom cli cli_warn
as_forecast_binary.default <- function(data,
                                       forecast_unit = NULL,
                                       observed = NULL,
                                       predicted = NULL,
                                       ...) {
  data <- as_forecast_generic(
    data,
    forecast_unit,
    observed = observed,
    predicted = predicted
  )
  data <- new_forecast(data, "forecast_binary")
  assert_forecast(data)
  return(data)
}

#' @export
#' @rdname assert_forecast
#' @importFrom cli cli_abort
#' @keywords validate-forecast-object
assert_forecast.forecast_binary <- function(
  forecast, forecast_type = NULL, verbose = TRUE, ...
) {
  forecast <- assert_forecast_generic(forecast, verbose)
  assert_forecast_type(forecast, actual = "binary", desired = forecast_type)

  columns_correct <- test_columns_not_present(
    forecast, c("sample_id", "quantile_level")
  )
  if (!columns_correct) {
    #nolint start: keyword_quote_linter
    cli_abort(
      c(
        "!" = "Checking `forecast`: Input looks like a binary forecast, but an
         additional column called `sample_id` or `quantile` was found.",
        "i" = "Please remove the column."
      )
    )
  }
  input_check <- check_input_binary(forecast$observed, forecast$predicted)
  if (!isTRUE(input_check)) {
    cli_abort(
      c(
        "!" = "Checking `forecast`: Input looks like a binary forecast, but
             found the following issue: {input_check}"
      )
    )
    #nolint end
  }
  return(invisible(NULL))
}


#' @export
#' @rdname is_forecast
is_forecast_binary <- function(x) {
  inherits(x, "forecast_binary") && inherits(x, "forecast")
}


#' @importFrom stats na.omit
#' @importFrom data.table setattr copy
#' @rdname score
#' @export
score.forecast_binary <- function(forecast, metrics = get_metrics(forecast), ...) {
  forecast <- clean_forecast(forecast, copy = TRUE, na.omit = TRUE)
  metrics <- validate_metrics(metrics)
  forecast <- as.data.table(forecast)

  scores <- apply_metrics(
    forecast, metrics,
    forecast$observed, forecast$predicted
  )
  scores[, `:=`(predicted = NULL, observed = NULL)]

  scores <- as_scores(scores, metrics = names(metrics))
  return(scores[])
}


#' Get default metrics for binary forecasts
#'
#' @description
#' For binary forecasts, the default scoring rules are:
#' - "brier_score" = [brier_score()]
#' - "log_score" = [logs_binary()]
#' @inheritSection illustration-input-metric-binary-point Input format
#' @param x A forecast object (a validated data.table with predicted and
#'   observed values, see [as_forecast_binary()]).
#' @param select A character vector of scoring rules to select from the list. If
#'   `select` is `NULL` (the default), all possible scoring rules are returned.
#' @param exclude A character vector of scoring rules to exclude from the list.
#'   If `select` is not `NULL`, this argument is ignored.
#' @param ... unused
#' @returns A list of scoring functions.
#' @export
#' @family get_metrics functions
#' @keywords handle-metrics
#' @examples
#' get_metrics(example_binary)
#' get_metrics(example_binary, select = "brier_score")
#' get_metrics(example_binary, exclude = "log_score")
get_metrics.forecast_binary <- function(x, select = NULL, exclude = NULL, ...) {
  all <- list(
    brier_score = brier_score,
    log_score = logs_binary
  )
  select_metrics(all, select, exclude)
}


#' Binary forecast example data
#'
#' A data set with binary predictions for COVID-19 cases and deaths constructed
#' from data submitted to the European Forecast Hub.
#'
#' Predictions in the data set were constructed based on the continuous example
#' data by looking at the number of samples below the mean prediction.
#' The outcome was constructed as whether or not the actually
#' observed value was below or above that mean prediction.
#' This should not be understood as sound statistical practice, but rather
#' as a practical way to create an example data set.
#'
#' The data was created using the script create-example-data.R in the inst/
#' folder (or the top level folder in a compiled package).
#'
#' @format An object of class `forecast_binary` (see [as_forecast_binary()])
#' with the following columns:
#' \describe{
#'   \item{location}{the country for which a prediction was made}
#'   \item{location_name}{name of the country for which a prediction was made}
#'   \item{target_end_date}{the date for which a prediction was made}
#'   \item{target_type}{the target to be predicted (cases or deaths)}
#'   \item{observed}{A factor with observed values}
#'   \item{forecast_date}{the date on which a prediction was made}
#'   \item{model}{name of the model that generated the forecasts}
#'   \item{horizon}{forecast horizon in weeks}
#'   \item{predicted}{predicted value}
#' }
# nolint start
#' @source \url{https://github.com/european-modelling-hubs/covid19-forecast-hub-europe_archive/commit/a42867b1ea152c57e25b04f9faa26cfd4bfd8fa6/}
# nolint end
"example_binary"
