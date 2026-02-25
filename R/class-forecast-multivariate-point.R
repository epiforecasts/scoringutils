#' @title Create a `forecast` object for multivariate point forecasts
#' @inherit as_forecast_doc_template params description
#' @inheritParams as_forecast_multivariate_sample
#' @details
#' # Target format
#'
#' The input for all further scoring needs to be a data.frame or similar
#' with the following columns:
#' - `observed`: Column of type `numeric` with observed values.
#' - `predicted`: Column of type `numeric` with predicted values.
#' - `mv_group_id`: Column of any type with unique identifiers
#'    (unique within a single forecast) for each multivariate group.
#'    This column is created automatically using the `forecast_unit`
#'    and the `joint_across` arguments.
#'
#' For convenience, we recommend an additional column `model` holding
#' the name of the forecaster or model that produced a prediction, but
#' this is not strictly necessary.
#'
#' See the [example_point] data set for an example of point forecast data.
#' @inheritSection forecast_types Forecast unit
#' @param ... Unused
#' @family functions to create forecast objects
#' @returns A `forecast` object of class `forecast_multivariate_point`
#' @export
#' @keywords as_forecast transform
# nolint start: object_name_linter
as_forecast_multivariate_point <- function(data, ...) {
  UseMethod("as_forecast_multivariate_point")
}
# nolint end


#' @rdname as_forecast_multivariate_point
#' @inheritParams as_forecast_multivariate_sample.default
#' @export
#' @importFrom cli cli_abort
# nolint start: object_name_linter
as_forecast_multivariate_point.default <- function(
    data,
    joint_across = NULL,
    forecast_unit = NULL,
    observed = NULL,
    predicted = NULL,
    ...
) {
  data <- as_forecast_generic(
    data,
    forecast_unit,
    observed = observed,
    predicted = predicted
  )
  data <- ensure_mv_grouping(data, joint_across)

  data <- new_forecast(data, "forecast_multivariate_point")
  assert_forecast(data)
  return(data)
}
# nolint end


#' @export
#' @rdname assert_forecast
#' @importFrom cli cli_abort
#' @keywords validate-forecast-object
assert_forecast.forecast_multivariate_point <- function(
  forecast, forecast_type = NULL, verbose = TRUE, ...
) {
  assert(check_columns_present(forecast, ".mv_group_id"))
  forecast <- assert_forecast_generic(forecast, verbose)

  input_check <- check_input_point(
    forecast$observed, forecast$predicted
  )
  if (!isTRUE(input_check)) {
    cli_abort(
      c(
        "!" = "Checking `forecast`: {input_check}"
      )
    )
  }

  assert_forecast_type(
    forecast,
    actual = "multivariate_point",
    desired = forecast_type
  )
  return(invisible(NULL))
}


#' @export
#' @rdname is_forecast
# nolint start: object_name_linter
is_forecast_multivariate_point <- function(x) {
  inherits(x, "forecast_multivariate_point") &&
    inherits(x, "forecast")
}
# nolint end


#' @importFrom stats na.omit
#' @importFrom data.table setattr copy
#' @rdname score
#' @export
score.forecast_multivariate_point <- function(
  forecast, metrics = get_metrics(forecast), ...
) {
  forecast <- clean_forecast(
    forecast, copy = TRUE, na.omit = TRUE
  )
  forecast_unit <- get_forecast_unit(forecast)
  metrics <- validate_metrics(metrics)
  forecast <- as.data.table(forecast)

  f_transposed <- forecast[, .(
    predicted = list(predicted),
    observed = unique(observed)
  ),
  by = c(forecast_unit, ".mv_group_id")
  ]

  observed <- f_transposed$observed
  predicted <- do.call(rbind, f_transposed$predicted)
  predicted <- matrix(predicted, ncol = 1)
  f_transposed[, c("observed", "predicted") := NULL]

  mv_group_id <- f_transposed$.mv_group_id

  result <- score_multivariate_apply(
    f_transposed, metrics,
    observed, predicted, mv_group_id
  )

  scores <- as_scores(result, metrics = names(metrics))
  return(scores[])
}


#' Get default metrics for multivariate point forecasts
#'
#' @description
#' For multivariate point forecasts, the default scoring rule is:
#' - "variogram_score" =
#'   [variogram_score_multivariate_point()]
#' @inheritSection illustration-input-metric-binary-point Input format
#' @inheritParams get_metrics.forecast_binary
#' @export
#' @family get_metrics functions
#' @keywords handle-metrics
#' @examples
#' data <- data.frame(
#'   observed = c(1, 2, 3),
#'   predicted = c(1.1, 2.2, 3.3),
#'   target = c("a", "b", "c"),
#'   model = "m1",
#'   date = "2020-01-01"
#' )
#' ex <- as_forecast_multivariate_point(
#'   data,
#'   forecast_unit = c("model", "date", "target"),
#'   joint_across = "target"
#' )
#' get_metrics(ex)
# nolint start: object_name_linter
get_metrics.forecast_multivariate_point <- function(
    x, select = NULL, exclude = NULL, ...
) {
  all <- list(
    variogram_score = variogram_score_multivariate_point
  )
  select_metrics(all, select, exclude)
}
# nolint end
