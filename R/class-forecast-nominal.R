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
#' @rdname is_forecast
is_forecast_nominal <- function(x) {
  inherits(x, "forecast_nominal") && inherits(x, "forecast")
}

