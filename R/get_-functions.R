# Functions that help to obtain information about the data

#' @title Infer forecast type from data
#' @description
#' Helper function to infer the forecast type based on a
#' data.frame or similar with forecasts and observed values. See the details
#' section below for information on the different forecast types.
#' @inheritParams as_forecast
#' @inheritSection forecast_types Forecast types and input formats
#' @importFrom cli cli_abort
#' @return
#' Character vector of length one with either "binary", "quantile",
#' "sample" or "point".
#' @export
#' @keywords diagnose-inputs
get_forecast_type <- function(data) {
  assert_data_frame(data)
  assert(check_columns_present(data, c("observed", "predicted")))
  if (test_forecast_type_is_nominal(data)) {
    forecast_type <- "nominal"
  } else if (test_forecast_type_is_binary(data)) {
    forecast_type <- "binary"
  } else if (test_forecast_type_is_quantile(data)) {
    forecast_type <- "quantile"
  } else if (test_forecast_type_is_sample(data)) {
    forecast_type <- "sample"
  } else if (test_forecast_type_is_point(data)) {
    forecast_type <- "point"
  } else {
    #nolint start: keyword_quote_linter
    cli_abort(
      c(
        "!" = "Checking `data`: input doesn't satisfy criteria for any
        forecast type. ",
        "i" = "Are you missing a column `quantile_level` or `sample_id`?
        Please check the vignette for additional info."
      )
    )
    #nolint end
  }
  return(forecast_type)
}


#' Test whether data could be a binary forecast.
#' @description Checks type of the necessary columns.
#' @inheritParams document_check_functions
#' @importFrom checkmate test_factor test_numeric
#' @return Returns TRUE if basic requirements are satisfied and FALSE otherwise
#' @keywords internal_input_check
test_forecast_type_is_binary <- function(data) {
  observed_correct <- test_factor(x = data$observed)
  predicted_correct <- test_numeric(x = data$predicted)
  return(observed_correct && predicted_correct)
}

#' Test whether data could be a sample-based forecast.
#' @description Checks type of the necessary columns.
#' @inheritParams document_check_functions
#' @return Returns TRUE if basic requirements are satisfied and FALSE otherwise
#' @keywords internal_input_check
test_forecast_type_is_sample <- function(data) {
  observed_correct <- test_numeric(x = data$observed)
  predicted_correct <- test_numeric(x = data$predicted)
  columns_correct <- test_columns_present(data, "sample_id")
  return(observed_correct && predicted_correct && columns_correct)
}

#' Test whether data could be a point forecast.
#' @description Checks type of the necessary columns.
#' @inheritParams document_check_functions
#' @return Returns TRUE if basic requirements are satisfied and FALSE otherwise
#' @keywords internal_input_check
test_forecast_type_is_point <- function(data) {
  observed_correct <- test_numeric(x = data$observed)
  predicted_correct <- test_numeric(x = data$predicted)
  columns_correct <- test_columns_not_present(
    data, c("sample_id", "quantile_level")
  )
  return(observed_correct && predicted_correct && columns_correct)
}

#' Test whether data could be a quantile forecast.
#' @description Checks type of the necessary columns.
#' @inheritParams document_check_functions
#' @return Returns TRUE if basic requirements are satisfied and FALSE otherwise
#' @keywords internal_input_check
test_forecast_type_is_quantile <- function(data) {
  observed_correct <- test_numeric(x = data$observed)
  predicted_correct <- test_numeric(x = data$predicted)
  columns_correct <- test_columns_present(data, "quantile_level")
  return(observed_correct && predicted_correct && columns_correct)
}

#' Test whether data could be a nominal forecast.
#' @description Checks type of the necessary columns.
#' @inheritParams document_check_functions
#' @return Returns TRUE if basic requirements are satisfied and FALSE otherwise
#' @keywords internal_input_check
test_forecast_type_is_nominal <- function(data) {
  observed_correct <- test_factor(x = data$observed)
  predicted_correct <- test_numeric(x = data$predicted)
  columns_correct <- test_columns_present(data, "predicted_label")
  predicted_label_correct <- test_factor(x = data$predicted_label)
  return(observed_correct && predicted_correct &&
           columns_correct && predicted_label_correct)
}

#' Assert that forecast type is as expected
#' @param data A forecast object (see [as_forecast()]).
#' @param actual The actual forecast type of the data
#' @param desired The desired forecast type of the data
#' @inherit document_assert_functions return
#' @importFrom cli cli_abort
#' @importFrom checkmate assert_character
#' @keywords internal_input_check
assert_forecast_type <- function(data,
                                 actual = get_forecast_type(data),
                                 desired = NULL) {
  assert_character(desired, null.ok = TRUE)
  if (!is.null(desired) && desired != actual) {
    #nolint start: object_usage_linter keyword_quote_linter
    cli_abort(
      c(
        "!" = "Forecast type determined by scoringutils based on input:
        {.val {actual}}.",
        "i" = "Desired forecast type: {.val {desired}}."
      )
    )
    #nolint end
  }
  return(invisible(NULL))
}


#' @title Get type of a vector or matrix of observed values or predictions
#'
#' @description
#' Internal helper function to get the type of a vector (usually
#' of observed or predicted values). The function checks whether the input is
#' a factor, or else whether it is integer (or can be coerced to integer) or
#' whether it's continuous.
#' @param x Input the type should be determined for.
#' @importFrom cli cli_abort
#' @return
#' Character vector of length one with either "classification",
#' "integer", or "continuous".
#' @keywords internal_input_check
get_type <- function(x) {
  if (is.factor(x)) {
    return("classification")
  }
  assert_numeric(as.vector(x))
  if (all(is.na(as.vector(x)))) {
    cli_abort("Can't get type: all values of are {.val NA}.")
  }
  if (is.integer(x)) {
    return("integer")
  }
  if (
    isTRUE(all.equal(as.vector(x), as.integer(x))) && !all(is.na(as.integer(x)))
  ) {
    return("integer")
  } else {
    return("continuous")
  }
}


#' @title Get names of the metrics that were used for scoring
#' @description
#' When applying a scoring rule via [score()], the names of the scoring rules
#' become column names of the
#' resulting data.table. In addition, an attribute `metrics` will be
#' added to the output, holding the names of the scores as a vector.
#'
#' This is done so that functions like [get_forecast_unit()] or
#' [summarise_scores()] can still identify which columns are part of the
#' forecast unit and which hold a score.
#'
#' `get_metrics()` accesses and returns the `metrics` attribute. If there is no
#' attribute, the function will return `NULL` (or, if `error = TRUE` will
#' produce an error instead). In addition, it checks the column names of the
#' input for consistency with the data stored in the `metrics` attribute.
#'
#' **Handling a missing or inconsistent `metrics` attribute**:
#'
#' If the metrics attribute is missing or is not consistent with the column
#' names of the data.table, you can either
#'
#' - run [score()] again, specifying names for the scoring rules manually, or
#' - add/update the attribute manually using
#' `attr(scores, "metrics") <- c("names", "of", "your", "scores")` (the
#' order does not matter).
#'
#' @param scores A data.table with an attribute `metrics`.
#' @param error Throw an error if there is no attribute called `metrics`?
#' Default is FALSE.
#' @importFrom cli cli_abort cli_warn
#' @importFrom checkmate assert_data_frame
#' @return
#' Character vector with the names of the scoring rules that were used
#' for scoring or `NULL` if no scores were computed previously.
#' @keywords handle-metrics
#' @export
get_metrics <- function(scores, error = FALSE) {
  assert_data_frame(scores)
  metrics <- attr(scores, "metrics")
  if (error && is.null(metrics)) {
    #nolint start: keyword_quote_linter
    cli_abort(
      c(
        "!" = "Input needs an attribute `metrics` with the names of the
         scoring rules that were used for scoring.",
        "i" = "See `?get_metrics` for further information."
      )
    )
    #nolint end
  }

  if (!all(metrics %in% names(scores))) {
    #nolint start: keyword_quote_linter object_usage_linter
    missing <- setdiff(metrics, names(scores))
    cli_warn(
      c(
        "!" = "The following scores have been previously computed, but are no
            longer column names of the data: {.val {missing}}",
        "i" = "See {.code ?get_metrics} for further information."
      )
    )
    #nolint end
  }

  return(metrics)
}


#' @title Get unit of a single forecast
#' @description
#' Helper function to get the unit of a single forecast, i.e.
#' the column names that define where a single forecast was made for.
#' This just takes all columns that are available in the data and subtracts
#' the columns that are protected, i.e. those returned by
#' [get_protected_columns()] as well as the names of the metrics that were
#' specified during scoring, if any.
#' @inheritParams as_forecast
#' @inheritSection forecast_types Forecast unit
#' @return
#' A character vector with the column names that define the unit of
#' a single forecast
#' @importFrom checkmate assert_data_frame
#' @export
#' @keywords diagnose-inputs
get_forecast_unit <- function(data) {
  assert_data_frame(data)
  protected_columns <- get_protected_columns(data)
  protected_columns <- c(protected_columns, attr(data, "metrics"))
  forecast_unit <- setdiff(colnames(data), unique(protected_columns))
  return(forecast_unit)
}


#' @title Get protected columns from data
#'
#' @description Helper function to get the names of all columns in a data frame
#' that are protected columns.
#'
#' @inheritParams as_forecast
#'
#' @return
#' A character vector with the names of protected columns in the data.
#' If data is `NULL` (default) then it returns a list of all columns that are
#' protected in scoringutils.
#'
#' @keywords internal
get_protected_columns <- function(data = NULL) {

  protected_columns <- c(
    "predicted", "observed", "sample_id", "quantile_level", "upper", "lower",
    "pit_value", "interval_range", "boundary", "predicted_label",
    "interval_coverage", "interval_coverage_deviation",
    "quantile_coverage", "quantile_coverage_deviation",
    grep("_relative_skill$", names(data), value = TRUE),
    grep("coverage_", names(data), fixed = TRUE, value = TRUE)
  )

  if (is.null(data)) {
    return(protected_columns)
  }

  # only return protected columns that are present
  datacols <- colnames(data)
  protected_columns <- intersect(
    datacols,
    protected_columns
  )

  return(protected_columns)
}


#' @title Find duplicate forecasts
#'
#' @description
#' Helper function to identify duplicate forecasts, i.e.
#' instances where there is more than one forecast for the same prediction
#' target.
#'
#' @param data A data.frame as used for [score()]
#' @param counts Should the output show the number of duplicates per forecast
#'   unit instead of the individual duplicated rows? Default is `FALSE`.
#' @return A data.frame with all rows for which a duplicate forecast was found
#' @export
#' @importFrom checkmate assert_data_frame assert_subset
#' @importFrom data.table setorderv
#' @keywords diagnose-inputs
#' @examples
#' example <- rbind(example_quantile, example_quantile[1000:1010])
#' get_duplicate_forecasts(example)

get_duplicate_forecasts <- function(
  data,
  counts = FALSE
) {
  assert_data_frame(data)
  data <- ensure_data.table(data)
  forecast_unit <- get_forecast_unit(data)
  available_type <- c("sample_id", "quantile_level", "predicted_label") %in% colnames(data)
  type <- c("sample_id", "quantile_level", "predicted_label")[available_type]
  data <- as.data.table(data)
  data[, scoringutils_InternalDuplicateCheck := .N, by = c(forecast_unit, type)]
  out <- data[scoringutils_InternalDuplicateCheck > 1]

  col <- colnames(data)[
    colnames(data) %in% c("sample_id", "quantile_level", "predicted_label")
  ]
  setorderv(out, cols = c(forecast_unit, col, "predicted"))
  out[, scoringutils_InternalDuplicateCheck := NULL]

  if (counts) {
    out <- out[, .(n_duplicates = .N), by = c(get_forecast_unit(out))]
  }

  return(out[])
}


#' @title Get quantile and interval coverage values for quantile-based forecasts
#'
#' @description
#' For a validated forecast object in a quantile-based format
#' (see [as_forecast()] for more information), this function computes:
#' - interval coverage of central prediction intervals
#' - quantile coverage for predictive quantiles
#' - the deviation between desired and actual coverage (both for interval and
#' quantile coverage)
#'
#' Coverage values are computed for a specific level of grouping, as specified
#' in the `by` argument. By default, coverage values are computed per model.
#'
#' **Interval coverage**
#'
#' Interval coverage for a given interval range is defined as the proportion of
#' observations that fall within the corresponding central prediction intervals.
#' Central prediction intervals are symmetric around the median and formed
#' by two quantiles that denote the lower and upper bound. For example, the 50%
#' central prediction interval is the interval between the 0.25 and 0.75
#' quantiles of the predictive distribution.
#'
#' **Quantile coverage**
#'
#' Quantile coverage for a given quantile level is defined as the proportion of
#' observed values that are smaller than the corresponding predictive quantile.
#' For example, the 0.5 quantile coverage is the proportion of observed values
#' that are smaller than the 0.5 quantile of the predictive distribution.
#' Just as above, for a single observation and the quantile of a single
#' predictive distribution, the value will either be `TRUE` or `FALSE`.
#'
#' **Coverage deviation**
#'
#' The coverage deviation is the difference between the desired coverage
#' (can be either interval or quantile coverage) and the
#' actual coverage. For example, if the desired coverage is 90% and the actual
#' coverage is 80%, the coverage deviation is -0.1.
#' @return
#' A data.table with columns as specified in `by` and additional
#' columns for the coverage values described above
#' @inheritParams score
#' @param by character vector that denotes the level of grouping for which the
#'   coverage values should be computed. By default (`"model"`), one coverage
#'   value per model will be returned.
#' @return
#' a data.table with columns "interval_coverage",
#' "interval_coverage_deviation", "quantile_coverage",
#' "quantile_coverage_deviation" and the columns specified in `by`.
#' @importFrom data.table setcolorder
#' @importFrom checkmate assert_subset
#' @examples
#' library(magrittr) # pipe operator
#' example_quantile %>%
#'   as_forecast_quantile() %>%
#'   get_coverage(by = "model")
#' @export
#' @keywords scoring
#' @export
get_coverage <- function(forecast, by = "model") {
  # input checks ---------------------------------------------------------------
  forecast <- clean_forecast(forecast, copy = TRUE, na.omit = TRUE)
  assert_subset(get_forecast_type(forecast), "quantile")

  # remove "quantile_level" and "interval_range" from `by` if present, as these
  # are included anyway
  by <- setdiff(by, c("quantile_level", "interval_range"))
  assert_subset(by, names(forecast))

  # convert to wide interval format and compute interval coverage --------------
  interval_forecast <- quantile_to_interval(forecast, format = "wide")
  interval_forecast[,
    interval_coverage := (observed <= upper) & (observed >= lower)
  ][, c("lower", "upper", "observed") := NULL]
  interval_forecast[, interval_coverage_deviation :=
                      interval_coverage - interval_range / 100]

  # merge interval range data with original data -------------------------------
  # preparations
  forecast[, interval_range := get_range_from_quantile(quantile_level)]
  forecast_cols <- colnames(forecast) # store so we can reset column order later
  forecast_unit <- get_forecast_unit(forecast)

  forecast <- merge(forecast, interval_forecast,
                    by = unique(c(forecast_unit, "interval_range")))

  # compute quantile coverage and deviation ------------------------------------
  forecast[, quantile_coverage := observed <= predicted]
  forecast[, quantile_coverage_deviation := quantile_coverage - quantile_level]

  # summarise coverage values according to `by` and cleanup --------------------
  # reset column order
  new_metrics <- c("interval_coverage", "interval_coverage_deviation",
                   "quantile_coverage", "quantile_coverage_deviation")
  setcolorder(forecast, unique(c(forecast_cols, "interval_range", new_metrics)))
  # remove forecast class and convert to regular data.table
  forecast <- as.data.table(forecast)
  by <- unique(c(by, "quantile_level", "interval_range"))
  # summarise
  forecast <- forecast[, lapply(.SD, mean), by = by, .SDcols = new_metrics]
  return(forecast[])
}


#' @title Count number of available forecasts
#'
#' @description
#' Given a data set with forecasts, this function counts the number of
#' available forecasts.
#' The level of grouping can be specified using the `by` argument (e.g. to
#' count the number of forecasts per model, or the number of forecasts per
#' model and location).
#' This is useful to determine whether there are any missing forecasts.
#'
#' @param by character vector or `NULL` (the default) that denotes the
#'   categories over which the number of forecasts should be counted.
#'   By default this will be the unit of a single forecast (i.e.
#'   all available columns (apart from a few "protected" columns such as
#'   'predicted' and 'observed') plus "quantile_level" or "sample_id" where
#'   present).
#'
#' @param collapse character vector (default: `c("quantile_level", "sample_id"`)
#'   with names of categories for which the number of rows should be collapsed
#'   to one when counting. For example, a single forecast is usually represented
#'   by a set of several quantiles or samples and collapsing these to one makes
#'   sure that a single forecast only gets counted once. Setting
#'   `collapse = c()` would mean that all quantiles / samples would be counted
#'   as individual forecasts.
#'
#' @return A data.table with columns as specified in `by` and an additional
#' column "count" with the number of forecasts.
#'
#' @inheritParams score
#' @importFrom data.table .I .N nafill
#' @export
#' @keywords gain-insights
#' @examples
#' \dontshow{
#'   data.table::setDTthreads(2) # restricts number of cores used on CRAN
#' }
#'
#' library(magrittr) # pipe operator
#' example_quantile %>%
#'   as_forecast_quantile() %>%
#'   get_forecast_counts(by = c("model", "target_type"))
get_forecast_counts <- function(forecast,
                                by = get_forecast_unit(forecast),
                                collapse = c("quantile_level", "sample_id")) {
  forecast <- clean_forecast(forecast, copy = TRUE, na.omit = TRUE)
  forecast_unit <- get_forecast_unit(forecast)
  assert_subset(by, names(forecast))

  # collapse several rows to 1, e.g. treat a set of 10 quantiles as one,
  # because they all belong to one single forecast that should be counted once
  collapse_by <- setdiff(
    c(forecast_unit, "quantile_level", "sample_id"),
    collapse
  )
  # filter "quantile_level", "sample" if in `collapse_by`, but not the forecast
  collapse_by <- intersect(collapse_by, names(forecast))

  forecast <- forecast[forecast[, .I[1], by = collapse_by]$V1]

  # count number of rows = number of forecasts
  out <- as.data.table(forecast)[, .(count = .N), by = by]

  # make sure that all combinations in "by" are included in the output (with
  # count = 0). To achieve that, take unique values in `forecast` and expand grid
  col_vecs <- unclass(out)
  col_vecs$count <- NULL
  col_vecs <- lapply(col_vecs, unique)
  out_empty <- expand.grid(col_vecs, stringsAsFactors = FALSE)

  out <- merge(out, out_empty, by = by, all.y = TRUE)
  out[, count := nafill(count, fill = 0)]

  return(out[])
}
