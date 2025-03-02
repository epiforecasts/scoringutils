# ==================== Functions internally used for scoring ===================
# These functions would ideally be replaced in the future

#' Transform from a quantile format to an interval format
#' @description
#' Internal helper function to transform from a quantile format to an interval
#' format (which is no longer a supported forecast format, but still used
#' internally. The function mimics an S3 generic, but is not actually an S3
#' generic, as we want the functions to be internal and not exported.)
#'
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
#' @returns A data.table with forecasts in an interval format.
#' @keywords internal
#' @importFrom cli cli_abort
quantile_to_interval <- function(...) {
  dots <- list(...)
  if (is.data.frame(dots[[1]])) {
    do.call(quantile_to_interval_dataframe, dots)
  } else if (is.numeric(dots[[1]])) {
    do.call(quantile_to_interval_numeric, dots)
  } else {
    cli_abort("Input must be either a data.frame or a numeric vector.")
  }
}


#' @param forecast A data.table with forecasts in a quantile-based format (see
#'   [as_forecast_quantile()]).
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
#' *quantile_to_interval_dataframe*:
#'   a data.table in an interval format (either "long" or "wide"), with or
#'   without a `quantile_level` column. Rows will not be reordered.
#' @importFrom data.table copy
#' @rdname quantile_to_interval
#' @keywords internal
quantile_to_interval_dataframe <- function(forecast,
                                           format = "long",
                                           keep_quantile_col = FALSE,
                                           ...) {
  # After this transformation, the object will no longer be a valid forecast
  # object so we unclass it
  forecast <- as.data.table(forecast)

  forecast[, boundary := ifelse(quantile_level <= 0.5, "lower", "upper")]
  forecast[, interval_range := get_range_from_quantile(quantile_level)]

  # add median quantile
  median <- forecast[quantile_level == 0.5, ]
  median[, boundary := "upper"]

  forecast <- data.table::rbindlist(list(forecast, median))
  if (!keep_quantile_col) {
    forecast[, quantile_level := NULL]
  }

  if (length(unique(forecast$boundary)) < 2) {
    cli_abort(
      c(
        #nolint start: keyword_quote_linter
        `!` = "No valid forecast intervals found.",
        `i` = "A forecast interval comprises two
      quantiles with quantile levels symmetric around the median
      (e.g. 0.25 and 0.75)"
        #nolint end
      )
    )
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
#' @rdname quantile_to_interval
#' @keywords internal
quantile_to_interval_numeric <- function(observed,
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
#' @inheritParams as_forecast_quantile
#' @param keep_quantile_col keep quantile_level column, default is TRUE
#' @returns A data.table in a long interval interval range format
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

  data <- as_forecast_quantile(
    data,
    probs = quantile_levels,
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
#' @returns a numeric vector of interval ranges of size N
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
