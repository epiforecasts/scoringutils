#' @title Evaluate forecasts in a data.frame format
#'
#' @description `score()` applies a selection of scoring metrics to a data.frame
#' of forecasts. It is the workhorse of the `scoringutils` package.
#' `score()` is a generic that dispatches to different methods depending on the
#' class of the input data. The default method is `score.default()`, which
#' validates the input, assigns as class based on the forecast type, and then
#' calls `score()` again to dispatch to the appropriate method. See below for
#' more information on how forecast types are determined.
#'
#' @details
#' **Forecast types and input format**
#'
#' Various different forecast types / forecast formats are supported. At the
#' moment, those are
#' - point forecasts
#' - binary forecasts ("soft binary classification")
#' - Probabilistic forecasts in a quantile-based format (a forecast is
#' represented as a set of predictive quantiles)
#' - Probabilistic forecasts in a sample-based format (a forecast is represented
#' as a set of predictive samples)
#'
#' Forecast types are determined based on the columns present in the input data.
#'
#' *Point forecasts* require a column `observed` of type numeric and a column
#' `predicted` of type numeric.
#'
#' *Binary forecasts* require a column `observed` of type factor with exactly
#' two levels and a column `predicted` of type numeric with probabilities,
#' corresponding to the probability that `observed` is equal to the second
#' factor level. See details [here][brier_score()] for more information.
#'
#' *Quantile-based forecasts* require a column `observed` of type numeric,
#' a column `predicted` of type numeric, and a column `quantile` of type numeric
#' with quantile-levels (between 0 and 1).
#'
#' *Sample-based forecasts* require a column `observed` of type numeric,
#' a column `predicted` of type numeric, and a column `sample_id` of type
#' numeric with sample indices.
#'
#' For more information see the vignettes and the example data
#' ([example_quantile], [example_continuous], [example_integer],
#' [example_point()], and [example_binary]).
#'
#' **Forecast unit**
#'
#' In order to score forecasts, `scoringutils` needs to know which of the rows
#' of the data belong together and jointly form a single forecasts. This is
#' easy e.g. for point forecast, where there is one row per forecast. For
#' quantile or sample-based forecasts, however, there are multiple rows that
#' belong to single forecast.
#'
#' The *forecast unit* or *unit of a single forecast* is then described by the
#' combination of columns that uniquely identify a single forecast.
#' For example, we could have forecasts made by different models in various
#' locations at different time points, each for several weeks into the future.
#' The forecast unit could then be described as
#' `forecast_unit = c("model", "location", "forecast_date", "forecast_horizon")`.
#' `scoringutils` automatically tries to determine the unit of a single
#' forecast. It uses all existing columns for this, which means that no columns
#' must be present that are unrelated to the forecast unit. As a very simplistic
#' example, if you had an additional row, "even", that is one if the row number
#' is even and zero otherwise, then this would mess up scoring as `scoringutils`
#' then thinks that this column was relevant in defining the forecast unit.
#'
#' In order to avoid issues, we recommend using the function
#' [set_forecast_unit()] to determine the forecast unit manually.
#' The function simply drops unneeded columns, while making sure that all
#' necessary, 'protected columns' like "predicted" or "observed" are retained.
#'
#' **Validating inputs**
#'
#' We recommend that users validate their input prior to scoring using the
#' function [validate()] (though this will also be run internally by [score()]).
#' The function checks the input data and provides helpful information.
#'
#'
#' **Further help**
#'
#' For additional help and examples, check out the [Getting Started
#' Vignette](https://epiforecasts.io/scoringutils/articles/scoringutils.html) as
#' well as the paper [Evaluating Forecasts with scoringutils in
#' R](https://arxiv.org/abs/2205.07090).
#'
#' @param data A data.frame or data.table with predicted and observed values.
#' @param metrics A named list of scoring functions. Names will be used as
#' column names in the output. See [metrics_point()], [metrics_binary()],
#' `metrics_quantile()`, and [metrics_sample()] for more information on the
#' default metrics used.
#' @param ... additional arguments
#'
#' @return A data.table with unsummarised scores. This will generally be
#' one score per forecast (as defined by the unit of a single forecast).
#'
#' For quantile-based forecasts, one score per quantile will be returned
#' instead. This is done as scores can be computed and may be of interest
#' for individual quantiles. You can call [summarise_scores()]) on the
#' unsummarised scores to obtain one score per forecast unit for quantile-based
#' forecasts.
#'
#' @importFrom data.table ':=' as.data.table
#'
#' @examples
#' library(magrittr) # pipe operator
#' data.table::setDTthreads(1) # only needed to avoid issues on CRAN
#'
#' validated <- validate(example_quantile)
#' score(validated) %>%
#'   summarise_scores(by = c("model", "target_type"))
#'
#' # set forecast unit manually (to avoid issues with scoringutils trying to
#' # determine the forecast unit automatically), check forecasts before scoring
#' example_quantile %>%
#'   set_forecast_unit(
#'     c("location", "target_end_date", "target_type", "horizon", "model")
#'   ) %>%
#'   validate() %>%
#'   score()
#'
#' # forecast formats with different metrics
#' \dontrun{
#' score(example_binary)
#' score(example_quantile)
#' score(example_point)
#' score(example_integer)
#' score(example_continuous)
#' }
#'
#' @author Nikos Bosse \email{nikosbosse@@gmail.com}
#' @references
#' Bosse NI, Gruson H, Cori A, van Leeuwen E, Funk S, Abbott S
#' (2022) Evaluating Forecasts with scoringutils in R.
#' \doi{10.48550/arXiv.2205.07090}
#'
#' @export

score <- function(data, ...) {
  UseMethod("score")
}

#' @rdname score
#' @export
score.default <- function(data, ...) {
  data <- validate(data)
  score(data, ...)
}

#' @rdname score
#' @export
score.scoringutils_binary <- function(data, metrics = metrics_binary, ...) {
  data <- validate(data)
  data <- remove_na_observed_predicted(data)
  metrics <- validate_metrics(metrics)

  # Extract the arguments passed in ...
  args <- list(...)
  lapply(seq_along(metrics), function(i, ...) {
    metric_name <- names(metrics[i])
    fun <- metrics[[i]]
    matching_args <- filter_function_args(fun, args)

    data[, (metric_name) := do.call(
      fun, c(list(observed, predicted), matching_args)
    )]
    return()
  }, ...)

  setattr(data, "metric_names", names(metrics))

  return(data[])

}


#' @importFrom Metrics se ae ape
#' @rdname score
#' @export
score.scoringutils_point <- function(data, metrics = metrics_point, ...) {
  data <- validate(data)
  data <- remove_na_observed_predicted(data)
  metrics <- validate_metrics(metrics)

  # Extract the arguments passed in ...
  args <- list(...)
  lapply(seq_along(metrics), function(i, ...) {
    metric_name <- names(metrics[i])
    fun <- metrics[[i]]
    matching_args <- filter_function_args(fun, args)

    data[, (metric_name) := do.call(
      fun, c(list(observed, predicted), matching_args)
    )]
    return()
  }, ...)

  setattr(data, "metric_names", names(metrics))

  return(data[])
}

#' @rdname score
#' @export
score.scoringutils_sample <- function(data, metrics = metrics_sample, ...) {
  data <- validate(data)
  data <- remove_na_observed_predicted(data)
  forecast_unit <- attr(data, "forecast_unit")
  metrics <- validate_metrics(metrics)

  # Extract the arguments passed in ...
  args <- list(...)
  lapply(seq_along(metrics), function(i, ...) {
    metric_name <- names(metrics[i])
    fun <- metrics[[i]]
    matching_args <- filter_function_args(fun, args)

    data[, (metric_name) := do.call(
      fun, c(list(unique(observed), t(predicted)), matching_args)
    ), by = forecast_unit]
    return()
  },
  ...)

  data <- data[
    , lapply(.SD, unique),
    .SDcols = colnames(data) %like% paste(names(metrics), collapse = "|"),
    by = forecast_unit
  ]

  setattr(data, "metric_names", names(metrics))

  return(data[])
}

#' @importFrom data.table `:=` as.data.table rbindlist %like%
#' @rdname score
#' @export
score.scoringutils_quantile <- function(data, metrics = metrics_quantile, ...) {
  data <- validate(data)
  data <- remove_na_observed_predicted(data)
  forecast_unit <- attr(data, "forecast_unit")
  metrics <- validate_metrics(metrics)

  # Extract the arguments passed in ...
  args <- list(...)

  # transpose the forecasts that belong to the same forecast unit
  # make sure the quantiles and predictions are ordered in the same way
  d_transposed <- data[, .(predicted = list(predicted[order(quantile)]),
                           observed = unique(observed),
                           quantile = list(quantile[order(quantile)]),
                           N = length(quantile)), by = forecast_unit]

  # split according to quantile lengths and do calculations for different
  # quantile lengths separately. The function `wis()` assumes that all
  # forecasts have the same quantiles
  d_split <- split(d_transposed, d_transposed$N)

  split_result <- lapply(d_split, function(data) {
    # create a matrix out of the list of predicted values and quantiles
    observed <- data$observed
    predicted <- do.call(rbind, data$predicted)
    quantile <- unlist(unique(data$quantile))
    data[, c("observed", "predicted", "quantile", "N") := NULL]

    # for each metric, compute score
    lapply(seq_along(metrics), function(i, ...) {
      metric_name <- names(metrics[i])
      fun <- metrics[[i]]
      matching_args <- filter_function_args(fun, args)

      data[, eval(metric_name) := do.call(
        fun, c(list(observed), list(predicted), list(quantile), matching_args)
      )]
      return()
    },
    ...)
    return(data)
  })

  data <- rbindlist(split_result)
  setattr(data, "metric_names", names(metrics))

  return(data[])
}
