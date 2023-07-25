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
      "prediction", "true_value", "sample", "quantile",
      "range", "boundary"
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
  overlapping <- (as.list(combined[, ..colnames_x]) %in% as.list(combined[, ..colnames_y])) & basenames_x == basenames_y
  overlap_names <- colnames_x[overlapping]
  basenames_overlap <- sub(".x$", "", overlap_names)

  # delete overlapping columns
  if (length(basenames_overlap > 0)) {
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
#' @param quantiles a numeric vector of quantiles to extract
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
                               quantiles = c(0.05, 0.25, 0.5, 0.75, 0.95),
                               type = 7) {
  data <- data.table::as.data.table(data)

  reserved_columns <- c("prediction", "sample")
  by <- setdiff(colnames(data), reserved_columns)

  data <- data[, .(quantile = quantiles,
                   prediction = quantile(x = prediction, prob = ..quantiles,
                                         type = ..type, na.rm = TRUE)),
               by = by]

  return(data[])
}


# ==================== Functions internally used for scoring ===================
# These functions would ideally be replaced in the future

#' @title Change Data from a Range Format to a Quantile Format
#'
#' @description
#'
#' Transform data from a format that uses interval ranges to denote quantiles
#' to a format that uses quantiles only.
#'
#' @param data a data.frame following the specifications from
#' [score()]) for quantile forecasts.
#' @param keep_range_col keep the range and boundary columns after
#' transformation (default is FALSE)
#' @return a data.frame in a plain quantile format
#' @importFrom data.table copy
#' @keywords internal


range_long_to_quantile <- function(data,
                                   keep_range_col = FALSE) {
  data <- data.table::as.data.table(data)

  # filter out duplicated median
  # note that this also filters out instances where range is NA, i.e.
  # a point forecast. This should probably be dealt with in the future
  data <- data[!(range == 0 & boundary == "upper"), ]

  data[, quantile := ifelse(
    boundary == "lower",
    round((100 - range) / 200, 10),
    round((1 - (100 - range) / 200), 10)
  )]

  if (!keep_range_col) {
    data[, c("range", "boundary") := NULL]
  }


  return(unique(data)[])
}


#' @title Change Data from a Plain Quantile Format to a Long Range Format
#'
#' @description
#'
#' Transform data from a format that uses quantiles only to one that uses
#' interval ranges to denote quantiles.
#'
#' @param data a data.frame in quantile format
#' @param keep_quantile_col keep the quantile column in the final
#' output after transformation (default is FALSE)
#' @return a data.frame in a long interval range format
#' @importFrom data.table copy
#' @keywords internal

quantile_to_range_long <- function(data,
                                   keep_quantile_col = TRUE) {
  data <- data.table::as.data.table(data)

  data[, boundary := ifelse(quantile <= 0.5, "lower", "upper")]
  data[, range := ifelse(
    boundary == "lower",
    round((1 - 2 * quantile) * 100, 10),
    round((2 * quantile - 1) * 100, 10)
  )]

  # add median quantile
  median <- data[quantile == 0.5, ]
  median[, boundary := "upper"]

  data <- data.table::rbindlist(list(data, median))

  if (!keep_quantile_col) {
    data[, "quantile" := NULL]
  }

  # if only point forecasts are scored, we only have NA values for range and
  # boundary. In that instance we need to set the type of the columns
  # explicitly to avoid future collisions.
  data[, `:=`(
    boundary = as.character(boundary),
    range = as.numeric(range)
  )]

  return(data[])
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
#' @param range a numeric vector of interval ranges to extract
#' (e.g. `c(0, 50, 90)`)
#' @param type type argument passed down to the quantile function. For more
#' information, see [quantile()]
#' @param keep_quantile_col keep quantile column, default is TRUE
#' @return a data.frame in a long interval range format
#' @importFrom data.table as.data.table
#' @importFrom stats quantile
#' @keywords internal

sample_to_range_long <- function(data,
                                 range = c(0, 50, 90),
                                 type = 7,
                                 keep_quantile_col = TRUE) {
  data <- data.table::as.data.table(data)

  lower_quantiles <- (100 - range) / 200
  upper_quantiles <- 1 - lower_quantiles
  quantiles <- sort(unique(c(lower_quantiles, upper_quantiles)))

  data <- sample_to_quantile(
    data,
    quantiles = quantiles,
    type = type
  )

  data <- quantile_to_range_long(data, keep_quantile_col = keep_quantile_col)

  return(data[])
}
