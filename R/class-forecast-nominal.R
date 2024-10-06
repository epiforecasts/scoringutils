#' @title Create a `forecast` object for nominal forecasts
#' @description
#' Nominal forecasts are a form of categorical forecasts where the possible
#' outcomes that the observed values can assume are not ordered. In that sense,
#' Nominal forecasts represent a generalisation of binary forecasts.
#' @inheritParams as_forecast
#' @param predicted_label (optional) Name of the column in `data` that denotes
#'   the outcome to which a predicted probability corresponds to.
#'   This column will be renamed to "predicted_label". Only applicable to
#'   nominal forecasts.
#' @family functions to create forecast objects
#' @keywords as_forecast
#' @export
as_forecast_nominal <- function(data,
                                forecast_unit = NULL,
                                observed = NULL,
                                predicted = NULL,
                                predicted_label = NULL) {
  assert_character(predicted_label, len = 1, null.ok = TRUE)
  assert_subset(predicted_label, names(data), empty.ok = TRUE)
  if (!is.null(predicted_label)) {
    setnames(data, old = predicted_label, new = "predicted_label")
  }

  data <- as_forecast_generic(data, forecast_unit, observed, predicted)
  data <- new_forecast(data, "forecast_nominal")
  assert_forecast(data)
  return(data)
}


#' @export
#' @keywords check-forecasts
#' @importFrom checkmate assert_names assert_set_equal test_set_equal
assert_forecast.forecast_nominal <- function(
  forecast, forecast_type = NULL, verbose = TRUE, ...
) {
  forecast <- assert_forecast_generic(forecast, verbose)
  assert(check_columns_present(forecast, "predicted_label"))
  assert_names(
    colnames(forecast),
    disjunct.from = c("sample_id", "quantile_level")
  )
  assert_forecast_type(forecast, actual = "nominal", desired = forecast_type)

  # levels need to be the same
  outcomes <- levels(forecast$observed)
  assert_set_equal(levels(forecast$predicted_label), outcomes)

  # forecasts need to be complete
  forecast_unit <- get_forecast_unit(forecast)
  complete <- as.data.table(forecast)[, .(
    correct = test_set_equal(as.character(predicted_label), outcomes)
  ), by = forecast_unit]

  if (!all(complete$correct)) {
    first_issue <- complete[(correct), ..forecast_unit][1]
    first_issue <- lapply(first_issue, FUN = as.character)
    #nolint start: keyword_quote_linter object_usage_linter duplicate_argument_linter
    issue_location <- paste(names(first_issue), "==", first_issue)
    cli_abort(
      c(`!` = "Found incomplete forecasts",
        `i` = "For a nominal forecast, all possible outcomes must be assigned
        a probability explicitly.",
        `i` = "Found first missing probabilities in the forecast identified by
        {.emph {issue_location}}")
    )
    #nolint end
  }
  return(forecast[])
}


#' @export
#' @rdname is_forecast
is_forecast_nominal <- function(x) {
  inherits(x, "forecast_nominal") && inherits(x, "forecast")
}


#' @importFrom stats na.omit
#' @importFrom data.table setattr
#' @rdname score
#' @export
score.forecast_nominal <- function(forecast, metrics = get_metrics(forecast), ...) {
  forecast <- clean_forecast(forecast, copy = TRUE, na.omit = TRUE)
  forecast_unit <- get_forecast_unit(forecast)
  metrics <- validate_metrics(metrics)
  forecast <- as.data.table(forecast)

  # transpose the forecasts that belong to the same forecast unit
  # make sure the labels and predictions are ordered in the same way
  f_transposed <- forecast[, .(
    predicted = list(predicted[order(predicted_label)]),
    observed = unique(observed)
  ), by = forecast_unit]

  observed <- f_transposed$observed
  predicted <- do.call(rbind, f_transposed$predicted)
  predicted_label <- sort(unique(forecast$predicted_label, na.last = TRUE))
  f_transposed[, c("observed", "predicted") := NULL]

  scores <- apply_metrics(
    f_transposed, metrics,
    observed, predicted, predicted_label, ...
  )
  scores <- as_scores(scores, metrics = names(metrics))
  return(scores[])
}


#' Get default metrics for nominal forecasts
#' @inheritParams get_metrics.forecast_binary
#' @description
#' For nominal forecasts, the default scoring rule is:
#' - "log_score" = [logs_nominal()]
#' @export
#' @family `get_metrics` functions
#' @keywords handle-metrics
#' @examples
#' get_metrics(example_nominal)
get_metrics.forecast_nominal <- function(x, select = NULL, exclude = NULL, ...) {
  all <- list(
    log_score = logs_nominal
  )
  select_metrics(all, select, exclude)
}


#' Nominal example data
#'
#' A data set with predictions for COVID-19 cases and deaths submitted to the
#' European Forecast Hub.
#'
#' The data was created using the script create-example-data.R in the inst/
#' folder (or the top level folder in a compiled package).
#'
#' @format An object of class `forecast_nominal` (see [as_forecast()]) with the
#' following columns:
#' \describe{
#'   \item{location}{the country for which a prediction was made}
#'   \item{target_end_date}{the date for which a prediction was made}
#'   \item{target_type}{the target to be predicted (cases or deaths)}
#'   \item{observed}{Numeric: observed values}
#'   \item{location_name}{name of the country for which a prediction was made}
#'   \item{forecast_date}{the date on which a prediction was made}
#'   \item{predicted_label}{outcome that a probabilty corresponds to}
#'   \item{predicted}{predicted value}
#'   \item{model}{name of the model that generated the forecasts}
#'   \item{horizon}{forecast horizon in weeks}
#' }
# nolint start
#' @source \url{https://github.com/european-modelling-hubs/covid19-forecast-hub-europe/commit/a42867b1ea152c57e25b04f9faa26cfd4bfd8fa6/}
# nolint end
"example_nominal"
