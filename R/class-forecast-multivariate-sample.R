#' @title Create a `forecast` object for sample-based multivariate forecasts
#' @inherit as_forecast_doc_template params description
#' @param forecast_unit (optional) Name of the columns in `data` (after
#'   any renaming of columns) that denote the unit of a
#'   single univariate (!) forecast. See [get_forecast_unit()] for details.
#'   If `NULL` (the default), all columns that are not required columns are
#'   assumed to form the unit of a single forecast. If specified, all columns
#'   that are not part of the forecast unit (or required columns) will be removed.
#'   Multivariate forecasts are defined by a) specifying the univariate forecast
#'   unit (i.e. the unit of a single forecast if that forecast were univariate)
#'   and b) specifying which variables are pooled together to form a
#'   multivariate forecast.
#' @details
#' # Target format
#'
#' The input for all further scoring needs to be a data.frame or similar
#' with the following columns:
#' - `observed`: Column of type `numeric` with observed values.
#' - `predicted`: Column of type `numeric` with predicted values. Predicted
#'    values represent random samples from the predictive distribution.
#' - `sample_id`: Column of any type with unique identifiers
#'    (unique within a single forecast) for each sample.
#' - `mv_group_id`: Column of any type with unique identifiers
#'    (unique within a single forecast) for each multivariate group. This column
#'    is created automatically using the `forecast_unit` and the `joint_across`
#'    arguments.
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
# nolint start: object_name_linter
as_forecast_multivariate_sample <- function(data, ...) {
  UseMethod("as_forecast_multivariate_sample")
}
# nolint end


#' @rdname as_forecast_multivariate_sample
#' @param sample_id (optional) Name of the column in `data` that contains the
#'   sample id. This column will be renamed to "sample_id".
#' @param joint_across Character vector with columns names that define the
#'   variables which are forecasted jointly. Conceptually, several univariate
#'   forecasts are pooled together to form a single multivariate forecasts.
#'   For example, if you have a column `country` and want to define
#'   a multivariate forecast for several countries at once, you could set
#'   `joint_across = "country"`.
#' @export
#' @importFrom cli cli_warn
as_forecast_multivariate_sample.default <- function(data,
                                                    joint_across,
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
  data <- set_grouping(data, joint_across)

  data <- new_forecast(data, "forecast_multivariate_sample")
  assert_forecast(data)
  return(data)
}


#' @export
#' @rdname assert_forecast
#' @importFrom cli cli_abort qty
#' @keywords validate-forecast-object
assert_forecast.forecast_multivariate_sample <- function(
  forecast, forecast_type = NULL, verbose = TRUE, ...
) {
  assert(check_columns_present(forecast, c("sample_id", ".mv_group_id")))
  forecast <- assert_forecast_generic(forecast, verbose)

  # make sure that for every .mv_group_id, the number of samples per
  # forecast unit is the same
  sample_lengths <- as.data.table(forecast)[, .(
    .scoringutils_N = length(sample_id)
  ),
  by = c(get_forecast_unit(forecast), ".mv_group_id")
  ]
  group_variations <- sample_lengths[, .(
    .scoringutils_N = length(unique(.scoringutils_N))
  ),
  by = .mv_group_id
  ]
  if (any(group_variations$.scoringutils_N > 1)) {
    # nolint start: object_usage_linter
    problematic_groups <- group_variations[.scoringutils_N > 1, .mv_group_id]
    cli_abort(
      "Found the following {qty(length(problematic_groups))} group{?s} with an
      inconsistent sample length, compared to other groups:
      {.val {problematic_groups}}"
    )
    # nolint end
  }

  assert_forecast_type(forecast, actual = "multivariate_sample", desired = forecast_type)
  return(invisible(NULL))
}


#' @export
#' @rdname is_forecast
# nolint start: object_name_linter
is_forecast_multivariate_sample <- function(x) {
  inherits(x, "forecast_multivariate_sample") && inherits(x, "forecast")
}
# nolint end


#' @importFrom stats na.omit
#' @importFrom data.table setattr copy
#' @importFrom methods formalArgs
#' @rdname score
#' @export
score.forecast_multivariate_sample <- function(forecast, metrics = get_metrics(forecast), ...) {
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
  by = c(forecast_unit, ".mv_group_id")
  ]

  # split according to number of samples and do calculations for different
  # sample lengths separately
  f_split <- split(f_transposed, f_transposed$.scoringutils_N)

  split_result <- lapply(f_split, function(forecast_same_length) {
    observed <- forecast_same_length$observed
    predicted <- do.call(rbind, forecast_same_length$predicted)
    forecast_same_length[, c("observed", "predicted", ".scoringutils_N") := NULL]

    mv_group_id <- forecast_same_length$.mv_group_id

    # for multivariate scores, multiple rows collapse to a single score.
    # we therefore have to create a new data.table with the correct dimensions
    # and groups
    grouping_cols <- get_grouping(forecast_same_length)
    temp_dt <- unique(forecast_same_length[, .SD, .SDcols = c(grouping_cols, ".mv_group_id")])
    result <- apply_metrics(
      temp_dt,
      metrics = metrics,
      observed, predicted,
      mv_group_id
    )

    setcolorder(result, c(setdiff(colnames(result), ".mv_group_id"), ".mv_group_id"))

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
#' example <- as_forecast_multivariate_sample(
#'   example_sample_continuous, joint_across = c("location", "location_name")
#' )
#' get_metrics(example)
get_metrics.forecast_multivariate_sample <- function(x, select = NULL, exclude = NULL, ...) {
  all <- list(
    energy_score = energy_score_multivariate
  )
  select_metrics(all, select, exclude)
}

#' @title Set grouping
#' @description
#' Helper function to set the grouping of a forecast.
#' @inheritParams as_forecast_doc_template
#' @inheritParams as_forecast_multivariate_sample
#' @importFrom data.table ':=' is.data.table copy setkeyv key
#' @importFrom checkmate assert_character assert_subset
#' @importFrom cli cli_abort
#' @return
#' A data.table with an additional column `.mv_group_id` that
#' contains the group id for each row.
#' @keywords internal
set_grouping <- function(data, joint_across) {
  data <- ensure_data.table(data)
  assert_character(joint_across, min.len = 1)
  assert_subset(joint_across, colnames(data))

  by <- setdiff(get_forecast_unit(data), joint_across)

  data[, .mv_group_id := .GRP, by = by]

  existing_keys <- key(data)
  data[, .scoringutils_count := .N, by = eval(get_forecast_unit(data))]

  for (mv_group_id in unique(data$.mv_group_id)) {
    counts <- data[.mv_group_id == mv_group_id, .scoringutils_count]
    unique_counts <- unique(counts)

    if (length(unique_counts) > 1) {
      cli_abort(
        "All univariate forecasts (as defined by the univariate forecast unit)
        in a group must have the same number of samples.
        This is not the case for group
        {.val {mv_group_id}}. Seeing {.val {unique_counts}} samples."
      )
    }
  }
  setkeyv(data, existing_keys)
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
#' @keywords diagnose-inputs
get_grouping <- function(forecast) {
  if (!(".mv_group_id" %in% names(forecast))) {
    cli_abort("No column `.mv_group_id` found in the forecast object.")
  }
  data <- as.data.table(forecast)
  # this iterates over every column, and for every column checks if there
  # is always only one unique value per group specified by .mv_group_id
  # if that is the case, the column is part of the grouping vector.
  grouping_cols <- names(data)[sapply(names(data), function(col) {
    data[, all(length(unique(.SD[[col]])) == 1), by = ".mv_group_id"][, all(V1)]
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
#' @format An object of class `forecast_multivariate_sample`
#' (see [as_forecast_multivariate_sample()]) with the following columns:
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
#'   \item{.mv_group_id}{id for the corresponding group}
#' }
# nolint start
#' @source \url{https://github.com/european-modelling-hubs/covid19-forecast-hub-europe_archive/commit/a42867b1ea152c57e25b04f9faa26cfd4bfd8fa6/}
# nolint end
"example_multivariate_sample"
