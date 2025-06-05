#' @title Create a `forecast` object for sample-based multivariate forecasts
#' @inherit as_forecast_doc_template params description
#' @details
#' # Required input
#'
#' The input needs to be a data.frame or similar for the default method
#' with the following columns:
#' - `observed`: Column of type `numeric` with observed values.
#' - `predicted`: Column of type `numeric` with predicted values. Predicted
#'    values represent random samples from the predictive distribution.
#' - `sample_id`: Column of any type with unique identifiers
#'    (unique within a single forecast) for each sample.
#' - `grouping_id`: Column of any type with unique identifiers
#'    (unique within a single forecast) for each group.
#'
#' For convenience, we recommend an additional column `model` holding the name
#' of the forecaster or model that produced a prediction, but this is not
#' strictly necessary.
#'
#' See the [example_sample_continuous] and [example_sample_discrete] data set
#' for an example
#' @inheritSection forecast_types Forecast unit
#' @param ... Unused
#' @family functions to create forecast objects
#' @returns A `forecast` object of class `forecast_sample`
#' @export
#' @keywords as_forecast transform
as_forecast_sample_multivariate <- function(data, ...) {
  UseMethod("as_forecast_sample_multivariate")
}


#' @rdname as_forecast_sample
#' @param sample_id (optional) Name of the column in `data` that contains the
#'   sample id. This column will be renamed to "sample_id".
#' @param grouping ADD
#' @export
#' @importFrom cli cli_warn
as_forecast_sample_multivariate.default <- function(data,
                                                    grouping,
                                                    forecast_unit = NULL,
                                                    observed = NULL,
                                                    predicted = NULL,
                                                    sample_id = NULL,
                                                    ...) {
  data <- as_forecast_generic(
    data,
    forecast_unit,
    observed = observed,
    predicted = predicted,
    sample_id = sample_id
  )
  data <- set_grouping(data, grouping)
  data <- new_forecast(data, "forecast_sample_multivariate")
  assert_forecast(data)
  return(data)
}

#' @title Return column names to define the grouping for multivariate forecasts
#' @param across Character vector with the names of the columns over which to
#'   group.
#' @inheritParams get_forecast_unit
#' @importFrom cli cli_abort
#' @importFrom checkmate assert_character assert_subset
#' @return
#' A character vector with the names of the columns that define the grouping.
#' @export
define_grouping_cols <- function(data, across) {
  if (missing(across) || is.null(across)) {
    cli_abort("{.arg across} is required to denote the variable across
    which to form groups for multivariate forecasts.")
  }
  assert_character(across, min.len = 1, null.ok = FALSE)
  assert_subset(across, names(data), empty.ok = FALSE)
  forecast_unit <- get_forecast_unit(data)
  grouping_unit <- setdiff(forecast_unit, across)
  return(grouping_unit)
}

#' @export
#' @rdname assert_forecast
#' @keywords validate-forecast-object
assert_forecast.forecast_sample_multivariate <- function(
  forecast, forecast_type = NULL, verbose = TRUE, ...
) {
  assert(check_columns_present(forecast, c("sample_id", ".scoringutils_group_id")))
  forecast <- assert_forecast_generic(forecast, verbose)

  # make sure that for every .scoringutils_group_id, the number of samples per
  # forecast unit is the same
  sample_lengths <- as.data.table(forecast)[, .(
    .scoringutils_N = length(sample_id)
  ),
  by = c(get_forecast_unit(forecast), ".scoringutils_group_id")
  ]
  group_variations <- sample_lengths[, .(
    .scoringutils_N = length(unique(.scoringutils_N))
  ),
  by = .scoringutils_group_id
  ]
  if (any(group_variations$.scoringutils_N > 1)) {
    problematic_groups <- group_variations[.scoringutils_N > 1, .scoringutils_group_id]
    cli_abort(
      "Found groups with inconsistent sample lengths.
      Groups {.val {problematic_groups}} have different numbers of samples."
    )
  }

  assert_forecast_type(forecast, actual = "forecast_sample_multivariate", desired = forecast_type)
  return(invisible(NULL))
}


#' @export
#' @rdname is_forecast
is_forecast_sample_multivariate <- function(x) {
  inherits(x, "forecast_sample_multivariate") && inherits(x, "forecast")
}


#' @importFrom stats na.omit
#' @importFrom data.table setattr copy
#' @importFrom methods formalArgs
#' @rdname score
#' @export
score.forecast_sample_multivariate <- function(forecast, metrics = get_metrics(forecast), ...) {
  forecast <- clean_forecast(forecast, copy = TRUE, na.omit = TRUE)
  forecast_unit <- get_forecast_unit(forecast)
  metrics <- validate_metrics(metrics)
  forecast <- as.data.table(forecast)

  # transpose the forecasts that belong to the same forecast unit
  f_transposed <- forecast[, .(
    predicted = list(predicted),
    observed = unique(observed),
    .scoringutils_N = length(sample_id)
  ),
  by = forecast_unit
  ]

  # split according to number of samples and do calculations for different
  # sample lengths separately
  f_split <- split(f_transposed, f_transposed$.scoringutils_N)

  split_result <- lapply(f_split, function(forecast_same_length) {
    observed <- forecast_same_length$observed
    predicted <- do.call(rbind, forecast_same_length$predicted)
    forecast_same_length[, c("observed", "predicted", ".scoringutils_N") := NULL]

    grouping_id <- forecast_same_length$.scoringutils_group_id

    # for multivariate scores, multiple rows collapse to a single score.
    # we therefore create a new data.table, compute scores, and merge back.
    temp_dt <- unique(forecast_same_length[, .SD, .SDcols = ".scoringutils_group_id"])
    result <- apply_metrics(
      temp_dt,
      metrics = metrics,
      observed, predicted,
      grouping_id
    )

    setcolorder(result, c(setdiff(colnames(result), ".scoringutils_group_id"), ".scoringutils_group_id"))

    return(result)
  })
  scores <- rbindlist(split_result, fill = TRUE)
  scores <- as_scores(scores, metrics = names(metrics))

  return(scores[])
}


#' Get default metrics for sample-based forecasts
#'
#' @description
#' For sample-based multivariate forecasts, the default scoring rules are:
#' - "energy_score" = [energy_score_multivariate()]
#' @inheritSection illustration-input-metric-sample Input format
#' @inheritParams get_metrics.forecast_binary
#' @export
#' @family get_metrics functions
#' @keywords handle-metrics
#' @examples
#' grouping <- define_grouping_cols(c("location_name", "location"), data = example_sample_continuous)
#' example <- as_forecast_sample_multivariate(example_sample_continuous, grouping = grouping)
#' get_metrics(example)
get_metrics.forecast_sample_multivariate <- function(x, select = NULL, exclude = NULL, ...) {
  all <- list(
    energy_score = energy_score_multivariate
  )
  select_metrics(all, select, exclude)
}

#' @title Set grouping
#' @description
#' Helper function to set the grouping of a forecast.
#' @inheritParams as_forecast_doc_template
#' @param grouping Character vector with the names of the columns that
#'   define the grouping.
#' @importFrom data.table ':=' is.data.table copy
#' @importFrom checkmate assert_character assert_subset
set_grouping <- function(data, grouping) {
  data <- ensure_data.table(data)
  assert_character(grouping, min.len = 1)
  assert_subset(grouping, colnames(data))

  data[, .scoringutils_group_id := .GRP, by = grouping]

  data[, .scoringutils_count := .N, by = eval(get_forecast_unit(data))]

  for (group_id in unique(data$.scoringutils_group_id)) {
    counts <- data[.scoringutils_group_id == group_id, .scoringutils_count]
    unique_counts <- unique(counts)

    if (length(unique_counts) > 1) {
      cli_abort(
        "All forecasts (as defined by the forecast unit) in a group must have
        the same number of samples. This is not the case for group
        {.val {group_id}}. Seeing {.val {unique_counts}} samples."
      )
    }
  }
  data[, .scoringutils_count := NULL]
  return(data)
}


#' @title Get grouping for a multivariate forecast
#' @description
#' Helper function to get the grouping for a multivariate forecast.
#' @inheritParams score
#' @return
#' A character vector with the names of the columns that define the grouping.
#' @export
get_grouping <- function(forecast) {
  if (!(".scoringutils_group_id" %in% names(forecast))) {
    return(get_forecast_unit(forecast))
  }
  data <- as.data.table(forecast)
  # this iterates over every column, and for every column checks if there
  # is always only one unique value per group specified by .scoringutils_group_id
  # if that is the case, the column is part of the grouping vector.
  grouping_cols <- names(data)[sapply(names(data), function(col) {
    data[, all(length(unique(.SD[[col]])) == 1), by = ".scoringutils_group_id"][, all(V1)]
  })]
  return(grouping_cols)
}


#' Multivariate forecast example data
#'
#' A data set with continuous multivariate predictions for COVID-19 cases and
#' deaths constructed from data submitted to the European Forecast Hub.
#'
#' The data was created using the script create-example-data.R in the inst/
#' folder (or the top level folder in a compiled package).
#'
#' @format An object of class `forecast_sample_multivariate`
#' (see [as_forecast_sample_multivariate()]) with the following columns:
#' \describe{
#'   \item{location}{the country for which a prediction was made}
#'   \item{target_end_date}{the date for which a prediction was made}
#'   \item{target_type}{the target to be predicted (cases or deaths)}
#'   \item{observed}{observed values}
#'   \item{location_name}{name of the country for which a prediction was made}
#'   \item{forecast_date}{the date on which a prediction was made}
#'   \item{model}{name of the model that generated the forecasts}
#'   \item{horizon}{forecast horizon in weeks}
#'   \item{predicted}{predicted value}
#'   \item{sample_id}{id for the corresponding sample}
#'   \item{.scoringutils_group_id}{id for the corresponding group}
#' }
# nolint start
#' @source \url{https://github.com/european-modelling-hubs/covid19-forecast-hub-europe_archive/commit/a42867b1ea152c57e25b04f9faa26cfd4bfd8fa6/}
# nolint end
"example_sample_multivariate"
