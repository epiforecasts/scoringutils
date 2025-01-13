#' @title Create a `forecast` object for point forecasts
#' @inherit as_forecast_doc_template params description
#' @details
#' # Required input
#'
#' The input needs to be a data.frame or similar for the default method
#' with the following columns:
#' - `observed`: Column of type `numeric` with observed values.
#' - `predicted`: Column of type `numeric` with predicted values.
#'
#' For convenience, we recommend an additional column `model` holding the name
#' of the forecaster or model that produced a prediction, but this is not
#' strictly necessary.
#'
#' See the [example_point] data set for an example.
#' @param ... Unused
#' @returns A `forecast` object of class `forecast_point`
#' @family functions to create forecast objects
#' @export
#' @keywords as_forecast transform
as_forecast_point <- function(data, ...) {
  UseMethod("as_forecast_point")
}


#' @rdname as_forecast_point
#' @export
#' @method as_forecast_point default
#' @importFrom cli cli_warn
as_forecast_point.default <- function(data,
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
  data <- new_forecast(data, "forecast_point")
  assert_forecast(data)
  return(data)
}


#' @export
#' @rdname assert_forecast
#' @importFrom cli cli_abort
#' @keywords validate-forecast-object
assert_forecast.forecast_point <- function(
  forecast, forecast_type = NULL, verbose = TRUE, ...
) {
  forecast <- assert_forecast_generic(forecast, verbose)
  assert_forecast_type(forecast, actual = "point", desired = forecast_type)
  #nolint start: keyword_quote_linter object_usage_linter
  input_check <- check_input_point(forecast$observed, forecast$predicted)
  if (!isTRUE(input_check)) {
    cli_abort(
      c(
        "!" = "Checking `forecast`: Input looks like a point forecast, but found
        the following issue: {input_check}"
      )
    )
    #nolint end
  }
  return(invisible(NULL))
}


#' @export
#' @rdname is_forecast
is_forecast_point <- function(x) {
  inherits(x, "forecast_point") && inherits(x, "forecast")
}


#' @importFrom Metrics se ae ape
#' @importFrom stats na.omit
#' @importFrom data.table setattr copy
#' @rdname score
#' @export
score.forecast_point <- function(forecast, metrics = get_metrics(forecast), ...) {
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


#' Get default metrics for point forecasts
#'
#' @description
#' For point forecasts, the default scoring rules are:
#' - "ae_point" = [ae()][Metrics::ae()]
#' - "se_point" = [se()][Metrics::se()]
#' - "ape" = [ape()][Metrics::ape()]
#'
#' A note of caution: Every scoring rule for a point forecast
#' is implicitly minimised by a specific aspect of the predictive distribution
#' (see Gneiting, 2011).
#'
#' The mean squared error, for example, is only a meaningful scoring rule if
#' the forecaster actually reported the mean of their predictive distribution
#' as a point forecast. If the forecaster reported the median, then the mean
#' absolute error would be the appropriate scoring rule. If the scoring rule
#' and the predictive task do not align, the results will be misleading.
#'
#' Failure to respect this correspondence can lead to grossly misleading
#' results! Consider the example in the section below.
#' @inheritSection illustration-input-metric-binary-point Input format
#' @inheritParams get_metrics.forecast_binary
#' @export
#' @family get_metrics functions
#' @keywords handle-metrics
#' @examples
#' get_metrics(example_point, select = "ape")
#'
#' library(magrittr)
#' set.seed(123)
#' n <- 500
#' observed <- rnorm(n, 5, 4)^2
#'
#' predicted_mu <- mean(observed)
#' predicted_not_mu <- predicted_mu - rnorm(n, 10, 2)
#'
#' df <- data.frame(
#'   model = rep(c("perfect", "bad"), each = n),
#'   predicted = c(rep(predicted_mu, n), predicted_not_mu),
#'   observed = rep(observed, 2),
#'   id = rep(1:n, 2)
#' ) %>%
#'   as_forecast_point()
#' score(df) %>%
#'   summarise_scores()
#' @references
#' Making and Evaluating Point Forecasts, Gneiting, Tilmann, 2011,
#' Journal of the American Statistical Association.
get_metrics.forecast_point <- function(x, select = NULL, exclude = NULL, ...) {
  all <- list(
    ae_point = Metrics::ae,
    se_point = Metrics::se,
    ape = Metrics::ape
  )
  select_metrics(all, select, exclude)
}


#' Point forecast example data
#'
#' A data set with predictions for COVID-19 cases and deaths submitted to the
#' European Forecast Hub. This data set is like the quantile example data, only
#' that the median has been replaced by a point forecast.
#'
#' The data was created using the script create-example-data.R in the inst/
#' folder (or the top level folder in a compiled package).
#'
#' @format An object of class `forecast_point` (see [as_forecast_point()])
#' with the following columns:
#' \describe{
#'   \item{location}{the country for which a prediction was made}
#'   \item{target_end_date}{the date for which a prediction was made}
#'   \item{target_type}{the target to be predicted (cases or deaths)}
#'   \item{observed}{observed values}
#'   \item{location_name}{name of the country for which a prediction was made}
#'   \item{forecast_date}{the date on which a prediction was made}
#'   \item{predicted}{predicted value}
#'   \item{model}{name of the model that generated the forecasts}
#'   \item{horizon}{forecast horizon in weeks}
#' }
# nolint start
#' @source \url{https://github.com/european-modelling-hubs/covid19-forecast-hub-europe_archive/commit/a42867b1ea152c57e25b04f9faa26cfd4bfd8fa6/}
# nolint end
"example_point"
