#' @title Change data from a sample based format to a quantile format
#'
#' @description
#' Transform data from a format that is based on predictive samples to a format
#' based on plain quantiles.
#'
#' @param forecast A `forecast` object of class `forecast_sample` (a validated
#'   data.table with predicted and observed values, see [as_forecast()]).
#'
#' @param quantile_level A numeric vector of quantile levels for which
#'   quantiles will be computed.
#' @param type Type argument passed down to the quantile function. For more
#'   information, see [quantile()].
#' @return a data.table in a long interval range format
#' @importFrom data.table as.data.table
#' @importFrom stats quantile
#' @importFrom methods hasArg
#' @importFrom checkmate assert_numeric
#' @keywords data-handling
#' @export
#' @examples
#' sample_to_quantile(as_forecast(example_sample_discrete))
sample_to_quantile <- function(forecast,
                               quantile_level = c(0.05, 0.25, 0.5, 0.75, 0.95),
                               type = 7) {
  forecast <- copy(forecast)
  assert_forecast(forecast, forecast_type = "sample", verbose = FALSE)
  assert_numeric(quantile_level, min.len = 1)
  reserved_columns <- c("predicted", "sample_id")
  by <- setdiff(colnames(forecast), reserved_columns)

  quantile_level <- unique(
    round(c(quantile_level, 1 - quantile_level), digits = 10)
  )

  forecast <-
    forecast[, .(quantile_level = quantile_level,
                 predicted = quantile(x = predicted, probs = ..quantile_level,
                                      type = ..type, na.rm = TRUE)),
             by = by]

  return(as_forecast(forecast))
}


# ==================== Functions internally used for scoring ===================
# These functions would ideally be replaced in the future

#' Transform from a quantile format to an interval format
#' @description
#' **Quantile format**
#' In a quantile format, a prediction is characterised by one or multiple
#' predicted values and the corresponding quantile levels. For example, a
#' prediction in a quantile format could be represented by the 0.05, 0.25, 0.5,
#' 0.75 and 0.95 quantiles of the predictive distribution.
#'
#' **Interval format**
#' In the interval format, two quantiles are assumed to form a prediction
#' interval. Prediction intervals need to be symmetric around the median and
#' are characterised by a lower and an upper bound. The lower bound is defined
#' by the lower quantile and the upper bound is defined by the upper quantile.
#' A 90% prediction interval, for example, covers 90% of the probability mass
#' and is defined by the 5% and 95% quantiles. A forecast could therefore
#' be characterised by one or multiple prediction intervals, e.g. the lower
#' and upper bounds of the 50% and 90% prediction intervals (corresponding to
#' the 0.25 and 0.75 as well as the 0.05 and 0.095 quantiles).
#' @param ... Arguments
#' @return A data.table with forecasts in an interval format.
#' @keywords data-handling
#' @export
quantile_to_interval <- function(...) {
  UseMethod("quantile_to_interval")
}


#' @param forecast A data.table with forecasts in a quantile-based format (see
#'   [as_forecast()]).
#' @param format The format of the output. Either "long" or "wide". If "long"
#'   (the default), there will be a column `boundary` (with values either
#'   "upper" or "lower" and a column `interval_range` that contains the range of
#'   the interval. If "wide", there will be a column `interval_range` and two
#'   columns `lower` and `upper` that contain the lower and upper bounds of the
#'   prediction interval, respectively.
#' @param keep_quantile_col keep the `quantile_level` column in the final
#'   output after transformation (default is FALSE). This only works if
#'   `format = "long"`. If `format = "wide"`, the `quantile_level` column will
#'   always be dropped.
#' @return
#' *quantile_to_interval.data.frame*:
#'   a data.table in an interval format (either "long" or "wide"), with or
#'   without a `quantile_level` column. Rows will not be reordered.
#' @importFrom data.table copy
#' @export
#' @rdname quantile_to_interval
#' @keywords data-handling
quantile_to_interval.data.frame <- function(forecast,
                                            format = "long",
                                            keep_quantile_col = FALSE,
                                            ...) {
  forecast <- ensure_data.table(forecast)

  forecast[, boundary := ifelse(quantile_level <= 0.5, "lower", "upper")]
  forecast[, interval_range := get_range_from_quantile(quantile_level)]

  # add median quantile
  median <- forecast[quantile_level == 0.5, ]
  median[, boundary := "upper"]

  forecast <- data.table::rbindlist(list(forecast, median))
  if (!keep_quantile_col) {
    forecast[, quantile_level := NULL]
  }

  if (format == "wide") {
    suppressWarnings(forecast[, "quantile_level" := NULL])
    forecast <- dcast(forecast, ... ~ boundary, value.var = "predicted")
    # if there are NA values in `predicted`, this introduces a column "NA"
    if ("NA" %in% colnames(forecast) && all(is.na(forecast[["NA"]]))) {
      forecast[, "NA" := NULL]
    }
  }
  return(forecast[])
}


#' @inheritParams wis
#' @return
#' *quantile_to_interval.numeric*:
#' a data.table in a wide interval format with columns `forecast_id`,
#' `observed`, `lower`, `upper`, and `interval_range`. The `forecast_id` column
#' is a unique identifier for each forecast. Rows will be reordered according to
#' `forecast_id` and `interval_range`.
#' @export
#' @rdname quantile_to_interval
#' @keywords data-handling
quantile_to_interval.numeric <- function(observed,
                                         predicted,
                                         quantile_level,
                                         ...) {
  assert_input_quantile(observed, predicted, quantile_level)

  n <- length(observed)
  N <- length(quantile_level)

  dt <- data.table(
    forecast_id = rep(1:n, each = N),
    observed = rep(observed, each = N),
    predicted = as.vector(t(predicted)),
    quantile_level = quantile_level
  )
  out <- quantile_to_interval(dt, format = "wide")
  out <- out[order(forecast_id, interval_range)]
  return(out)
}


#' @title Change data from a sample-based format to a long interval range format
#'
#' @description
#'
#' Transform data from a format that is based on predictive samples to a format
#' based on interval ranges.
#'
#' @inheritParams sample_to_quantile
#' @param keep_quantile_col keep quantile_level column, default is TRUE
#' @return A data.table in a long interval interval range format
#' @importFrom data.table as.data.table
#' @importFrom stats quantile
#' @keywords internal

sample_to_interval_long <- function(data,
                                    interval_range = c(0, 50, 90),
                                    type = 7,
                                    keep_quantile_col = TRUE) {
  data <- ensure_data.table(data)

  lower_quantiles <- (100 - interval_range) / 200
  upper_quantiles <- 1 - lower_quantiles
  quantile_levels <- sort(unique(c(lower_quantiles, upper_quantiles)))

  data <- sample_to_quantile(
    data,
    quantile_level = quantile_levels,
    type = type
  )

  data <- quantile_to_interval(data, keep_quantile_col = keep_quantile_col)

  return(data[])
}

#' Get interval range belonging to a quantile
#' @description
#' Every quantile can be thought of either as the lower or the
#' upper bound of a symmetric central prediction interval. This helper function
#' returns the range of the central prediction interval to which the quantile
#' belongs.
#'
#' Due to numeric instability that sometimes occurred in the past, ranges are
#' rounded to 10 decimal places. This is not a problem for the vast majority of
#' use cases, but it is something to be aware of.
#' @param quantile_level A numeric vector of quantile levels of size N.
#' @return a numeric vector of interval ranges of size N
#' @keywords internal
get_range_from_quantile <- function(quantile_level) {
  boundary <- ifelse(quantile_level <= 0.5, "lower", "upper")
  interval_range <- ifelse(
    boundary == "lower",
    round((1 - 2 * quantile_level) * 100, digits = 10),
    round((2 * quantile_level - 1) * 100, digits = 10)
  )
  return(interval_range)
}
