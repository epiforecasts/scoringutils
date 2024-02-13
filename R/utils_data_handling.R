#' @title Merge Forecast Data And Observations
#'
#' @description
#'
#' The function more or less provides a wrapper around `merge` that
#' aims to handle the merging well if additional columns are present
#' in one or both data sets. If in doubt, you should probably merge the
#' data sets manually.
#'
#' @param forecasts data.frame with the forecast data (as can be passed to
#' [score()]).
#' @param observations data.frame with the observations
#' @param join character, one of `c("left", "full", "right")`. Determines the
#' type of the join. Usually, a left join is appropriate, but sometimes you
#' may want to do a full join to keep dates for which there is a forecast, but
#' no ground truth data.
#' @param by character vector that denotes the columns by which to merge. Any
#' value that is not a column in observations will be removed.
#' @return a data.frame with forecasts and observations
#' @examples
#' forecasts <- example_quantile_forecasts_only
#' observations <- example_truth_only
#' merge_pred_and_obs(forecasts, observations)
#' @keywords data-handling
#' @export

merge_pred_and_obs <- function(forecasts, observations,
                               join = c("left", "full", "right"),
                               by = NULL) {
  forecasts <- data.table::as.data.table(forecasts)
  observations <- data.table::as.data.table(observations)

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

#' @title Change Data from a Sample Based Format to a Quantile Format
#'
#' @description
#'
#' Transform data from a format that is based on predictive samples to a format
#' based on plain quantiles.
#'
#'
#' @param data a data.frame with samples
#' @param quantile_level a numeric vector of quantile levels for which
#' quantiles will be computed
#' @param type type argument passed down to the quantile function. For more
#' information, see [quantile()]
#' @return a data.frame in a long interval range format
#' @importFrom data.table as.data.table
#' @importFrom stats quantile
#' @importFrom methods hasArg
#' @keywords data-handling
#' @export
#' @examples
#' sample_to_quantile(example_integer)
sample_to_quantile <- function(data,
                               quantile_level = c(0.05, 0.25, 0.5, 0.75, 0.95),
                               type = 7) {
  data <- ensure_data.table(data)
  reserved_columns <- c("predicted", "sample_id")
  by <- setdiff(colnames(data), reserved_columns)

  quantile_level <- unique(
    round(c(quantile_level, 1 - quantile_level), digits = 10)
  )

  data <- data[, .(quantile_level = quantile_level,
                   predicted = quantile(x = predicted, prob = ..quantile_level,
                                        type = ..type, na.rm = TRUE)),
               by = by]

  return(data[])
}


# ==================== Functions internally used for scoring ===================
# These functions would ideally be replaced in the future

#' @title Change Data from an Interval Format to a Quantile Format
#'
#' @description
#'
#' Transform data from a format that uses interval ranges to denote quantiles
#' to a format that uses quantiles only.
#'
#' @param data a data.frame following the specifications from
#' [score()]) for quantile forecasts.
#' @param keep_range_col keep the interval_range and boundary columns after
#' transformation (default is FALSE)
#' @return a data.frame in a plain quantile format
#' @importFrom data.table copy
#' @keywords internal


interval_long_to_quantile <- function(data,
                                      keep_range_col = FALSE) {
  data <- data.table::as.data.table(data)

  # filter out duplicated median
  # note that this also filters out instances where range is NA, i.e.
  # a point forecast. This should probably be dealt with in the future
  data <- data[!(interval_range == 0 & boundary == "upper"), ]

  data[, quantile_level := ifelse(
    boundary == "lower",
    round((100 - interval_range) / 200, 10),
    round((1 - (100 - interval_range) / 200), 10)
  )]

  if (!keep_range_col) {
    data[, c("interval_range", "boundary") := NULL]
  }


  return(unique(data)[])
}


#' Transform From a Quantile Format to an Interval Format
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
#' @param ... method arguments
#' @keywords data-handling
#' @export
quantile_to_interval <- function(...) {
  UseMethod("quantile_to_interval")
}


#' @param dt a data.table with columns `quantile_level` and `predicted`
#' @param format the format of the output. Either "long" or "wide". If "long"
#' (the default), there will be a column `boundary` (with values either "upper"
#' or "lower" and a column `interval_range` that contains the range of the
#' interval. If "wide", there will be a column `interval_range` and two columns
#' `lower` and `upper` that contain the lower and upper bounds of the
#' prediction interval, respectively.
#' @param keep_quantile_col keep the `quantile_level` column in the final
#' output after transformation (default is FALSE). This only works if
#' `format = "long"`. If `format = "wide"`, the `quantile_level` column will
#' always be dropped.
#' @return
#' *quantile_to_interval.data.frame*:
#' a data.frame in an interval format (either "long" or "wide"), with or
#' without a `quantile_level` column. Rows will not be reordered.
#' @importFrom data.table copy
#' @export
#' @rdname quantile_to_interval
#' @keywords data-handling
quantile_to_interval.data.frame <- function(dt,
                                            format = "long",
                                            keep_quantile_col = FALSE,
                                            ...) {
  dt <- ensure_data.table(dt)

  dt[, boundary := ifelse(quantile_level <= 0.5, "lower", "upper")]
  dt[, interval_range := get_range_from_quantile(quantile_level)]

  # add median quantile
  median <- dt[quantile_level == 0.5, ]
  median[, boundary := "upper"]

  dt <- data.table::rbindlist(list(dt, median))
  if (!keep_quantile_col) {
    dt[, quantile_level := NULL]
  }

  if (format == "wide") {
    suppressWarnings(dt[, "quantile_level" := NULL])
    dt <- dcast(dt, ... ~ boundary, value.var = "predicted")
    # if there are NA values in `predicted`, this introduces a column "NA"
    if ("NA" %in% colnames(dt) && all(is.na(dt[["NA"]]))) {
      dt[, "NA" := NULL]
    }
  }
  return(dt[])
}


#' @param observed a numeric vector of observed values of size n
#' @param predicted a numeric vector of predicted values of size n x N. If
#' `observed` is a single number, then `predicted` can be a vector of length N
#' @param quantile_level a numeric vector of quantile levels of size N
#' @return
#' *quantile_to_interval.numeric*:
#' a data.frame in a wide interval format with columns `forecast_id`,
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


#' @title Change Data from a Sample Based Format to a Long Interval Range Format
#'
#' @description
#'
#' Transform data from a format that is based on predictive samples to a format
#' based on interval ranges
#'
#'
#' @param data a data.frame with samples
#' @param interval_range a numeric vector of interval ranges to extract
#' (e.g. `c(0, 50, 90)`)
#' @param type type argument passed down to the quantile function. For more
#' information, see [quantile()]
#' @param keep_quantile_col keep quantile_level column, default is TRUE
#' @return a data.frame in a long interval interval range format
#' @importFrom data.table as.data.table
#' @importFrom stats quantile
#' @keywords internal

sample_to_interval_long <- function(data,
                                    interval_range = c(0, 50, 90),
                                    type = 7,
                                    keep_quantile_col = TRUE) {
  data <- data.table::as.data.table(data)

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

#' Get Interval Range Belonging to a Quantile
#' @description Every quantile can be thought of either as the lower or the
#' upper bound of a symmetric central prediction interval. This helper function
#' returns the range of the central prediction interval to which the quantile
#' belongs.
#'
#' Due to numeric instability that sometimes occurred in the past, ranges are
#' rounded to 10 decimal places. This is not a problem for the vast majority of
#' use cases, but it is something to be aware of.
#' @param quantile_level a numeric vector of quantile levels of size N
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
