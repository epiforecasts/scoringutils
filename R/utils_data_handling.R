#' @title Merge forecast data and observations
#'
#' @description
#'
#' The function more or less provides a wrapper around `merge` that
#' aims to handle the merging well if additional columns are present
#' in one or both data sets. If in doubt, you should probably merge the
#' data sets manually.
#'
#' @param forecasts A data.frame with the forecast data (as can be passed to
#'   [score()]).
#' @param observations A data.frame with the observations.
#' @param join Character, one of `c("left", "full", "right")`. Determines the
#'   type of the join. Usually, a left join is appropriate, but sometimes you
#'   may want to do a full join to keep dates for which there is a forecast, but
#'   no ground truth data.
#' @param by Character vector that denotes the columns by which to merge. Any
#'   value that is not a column in observations will be removed.
#' @return a data.table with forecasts and observations
#' @importFrom checkmate assert_subset
#' @examples
#' forecasts <- example_quantile_forecasts_only
#' observations <- example_truth_only
#' merge_pred_and_obs(forecasts, observations)
#' @keywords data-handling
#' @export

merge_pred_and_obs <- function(forecasts, observations,
                               join = c("left", "full", "right"),
                               by = NULL) {
  forecasts <- ensure_data.table(forecasts)
  observations <- ensure_data.table(observations)
  join <- match.arg(join)
  assert_subset(by, intersect(names(forecasts), names(observations)))

  if (is.null(by)) {
    protected_columns <- c(
      "predicted", "observed", "sample_id", "quantile_level",
      "interval_range", "boundary"
    )
    by <- setdiff(colnames(forecasts), protected_columns)
  }

  obs_cols <- colnames(observations)
  by <- intersect(by, obs_cols)

  join <- match.arg(join)

  if (join == "left") {
    # do a left_join, where all data in the observations are kept.
    combined <- merge(observations, forecasts, by = by, all.x = TRUE)
  } else if (join == "full") {
    # do a full, where all data is kept.
    combined <- merge(observations, forecasts, by = by, all = TRUE)
  } else {
    combined <- merge(observations, forecasts, by = by, all.y = TRUE)
  }


  # get colnames that are the same for x and y
  colnames <- colnames(combined)
  colnames_x <- colnames[endsWith(colnames, ".x")]
  colnames_y <- colnames[endsWith(colnames, ".y")]

  # extract basenames
  basenames_x <- sub(".x$", "", colnames_x)
  basenames_y <- sub(".y$", "", colnames_y)

  # see whether the column name as well as the content is the same
  content_x <- as.list(combined[, ..colnames_x])
  content_y <- as.list(combined[, ..colnames_y])
  overlapping <- (content_x %in% content_y) & (basenames_x == basenames_y)
  overlap_names <- colnames_x[overlapping]
  basenames_overlap <- sub(".x$", "", overlap_names)

  # delete overlapping columns
  if (length(basenames_overlap) > 0) {
    combined[, paste0(basenames_overlap, ".x") := NULL]
    combined[, paste0(basenames_overlap, ".y") := NULL]
  }

  return(combined[])
}

#' @title Change data from a sample based format to a quantile format
#'
#' @description
#' Transform data from a format that is based on predictive samples to a format
#' based on plain quantiles.
#'
#' @param data A `forecast` object of class `forecast_sample` (a validated
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
#' sample_to_quantile(as_forecast(example_integer))
sample_to_quantile <- function(forecast,
                               quantile_level = c(0.05, 0.25, 0.5, 0.75, 0.95),
                               type = 7) {
  forecast <- copy(forecast)
  suppressWarnings(
    suppressMessages(
      validate_forecast(forecast, forecast_type = "sample")
    )
  )
  assert_numeric(quantile_level, min.len = 1)
  reserved_columns <- c("predicted", "sample_id")
  by <- setdiff(colnames(forecast), reserved_columns)

  quantile_level <- unique(
    round(c(quantile_level, 1 - quantile_level), digits = 10)
  )

  forecast <- forecast[, .(quantile_level = quantile_level,
                   predicted = quantile(x = predicted, probs = ..quantile_level,
                                        type = ..type, na.rm = TRUE)),
               by = by]

  return(as_forecast(forecast))
}


# ==================== Functions internally used for scoring ===================
# These functions would ideally be replaced in the future

#' @title Change forecast from an interval format to a quantile format
#'
#' @description
#' Transform forecast from a format that uses interval ranges to denote quantiles
#' to a format that uses quantiles only.
#'
#' @param forecast A data.frame following the specifications from
#'   [score()]) for quantile forecasts.
#' @param keep_range_col Logical, default is `FALSE`. Keep the
#'   interval_range and boundary columns after transformation?
#' @return a data.table in a plain quantile format
#' @keywords internal
interval_long_to_quantile <- function(forecast,
                                      keep_range_col = FALSE) {
  forecast <- ensure_data.table(forecast)

  # filter out duplicated median
  # note that this also filters out instances where range is NA, i.e.
  # a point forecast. This should probably be dealt with in the future
  forecast <- forecast[!(interval_range == 0 & boundary == "upper"), ]

  forecast[, quantile_level := ifelse(
    boundary == "lower",
    round((100 - interval_range) / 200, 10),
    round((1 - (100 - interval_range) / 200), 10)
  )]

  if (!keep_range_col) {
    forecast[, c("interval_range", "boundary") := NULL]
  }

  return(unique(forecast)[])
}


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
