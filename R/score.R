#' @title Evaluate forecasts in a data.frame format
#' @description `score()` applies a selection of scoring metrics to a data.frame
#' of forecasts. It is the workhorse of the `scoringutils` package.
#' `score()` is a generic that dispatches to different methods depending on the
#' class of the input data.
#'
#' We recommend that users call [as_forecast()] prior to calling `score()` to
#' validate the input data and convert it to a forecast object (though
#' `score.default()` will do this if it hasn't happened before).
#' See below for more information on forecast types and input formats.
#' For additional help and examples, check out the [Getting Started
#' Vignette](https://epiforecasts.io/scoringutils/articles/scoringutils.html) as
#' well as the paper [Evaluating Forecasts with scoringutils in
#' R](https://arxiv.org/abs/2205.07090).
#' @inheritSection forecast_types Forecast types and input format
#' @inheritSection forecast_types Forecast unit
#' @param data A data.frame or data.table with predicted and observed values.
#' @param metrics A named list of scoring functions. Names will be used as
#' column names in the output. See [metrics_point()], [metrics_binary()],
#' `metrics_quantile()`, and [metrics_sample()] for more information on the
#' default metrics used.
#' @param ... additional arguments
#' @return A data.table with unsummarised scores. This will generally be
#' one score per forecast (as defined by the unit of a single forecast).
#'
#' For quantile-based forecasts, one score per quantile will be returned
#' instead. This is done as scores can be computed and may be of interest
#' for individual quantiles. You can call [summarise_scores()]) on the
#' unsummarised scores to obtain one score per forecast unit for quantile-based
#' forecasts.
#' @importFrom data.table ':=' as.data.table
#' @examples
#' library(magrittr) # pipe operator
#' data.table::setDTthreads(1) # only needed to avoid issues on CRAN
#'
#' validated <- as_forecast(example_quantile)
#' score(validated) %>%
#'   summarise_scores(by = c("model", "target_type"))
#'
#' # set forecast unit manually (to avoid issues with scoringutils trying to
#' # determine the forecast unit automatically), check forecasts before scoring
#' example_quantile %>%
#'   set_forecast_unit(
#'     c("location", "target_end_date", "target_type", "horizon", "model")
#'   ) %>%
#'   as_forecast() %>%
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
#' @author Nikos Bosse \email{nikosbosse@@gmail.com}
#' @references
#' Bosse NI, Gruson H, Cori A, van Leeuwen E, Funk S, Abbott S
#' (2022) Evaluating Forecasts with scoringutils in R.
#' \doi{10.48550/arXiv.2205.07090}
#' @export

score <- function(data, ...) {
  UseMethod("score")
}

#' @rdname score
#' @export
score.default <- function(data, ...) {
  data <- as_forecast(data)
  score(data, ...)
}

#' @rdname score
#' @export
score.scoringutils_binary <- function(data, metrics = metrics_binary, ...) {
  data <- validate(data)
  data <- remove_na_observed_predicted(data)
  metrics <- validate_metrics(metrics)

  data <- apply_metrics(
    data, metrics,
    data$observed, data$predicted, ...
  )

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

  data <- apply_metrics(
    data, metrics,
    data$observed, data$predicted, ...
  )

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

  # transpose the forecasts that belong to the same forecast unit
  d_transposed <- data[, .(predicted = list(predicted),
                           observed = unique(observed),
                           scoringutils_N = length(list(sample_id))),
                       by = forecast_unit]

  # split according to number of samples and do calculations for different
  # sample lengths separately
  d_split <- split(d_transposed, d_transposed$scoringutils_N)

  split_result <- lapply(d_split, function(data) {
    # create a matrix
    observed <- data$observed
    predicted <- do.call(rbind, data$predicted)
    data[, c("observed", "predicted", "scoringutils_N") := NULL]

    data <- apply_metrics(
      data, metrics,
      observed, predicted, ...
    )
    return(data)
  })
  data <- rbindlist(split_result)
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

  # transpose the forecasts that belong to the same forecast unit
  # make sure the quantiles and predictions are ordered in the same way
  d_transposed <- data[, .(
    predicted = list(predicted[order(quantile)]),
    observed = unique(observed),
    quantile = list(sort(quantile, na.last = TRUE)),
    scoringutils_quantile = toString(sort(quantile, na.last = TRUE))
  ),
  by = forecast_unit]

  # split according to quantile lengths and do calculations for different
  # quantile lengths separately. The function `wis()` assumes that all
  # forecasts have the same quantiles
  d_split <- split(d_transposed, d_transposed$scoringutils_quantile)

  split_result <- lapply(d_split, function(data) {
    # create a matrix out of the list of predicted values and quantiles
    observed <- data$observed
    predicted <- do.call(rbind, data$predicted)
    quantile <- unlist(unique(data$quantile))
    data[, c("observed", "predicted", "quantile", "scoringutils_quantile") := NULL]

    data <- apply_metrics(
      data, metrics,
      observed, predicted, quantile, ...
    )
    return(data)
  })

  data <- rbindlist(split_result)
  setattr(data, "metric_names", names(metrics))

  return(data[])
}

apply_metrics <- function(data, metrics, ...) {
  expr <- expression(
    data[, (metric_name) := do.call(run_safely, list(..., fun = fun))]
  )
  lapply(seq_along(metrics), function(i, data, ...) {
    metric_name <- names(metrics[i]) # nolint
    fun <- metrics[[i]] # nolint
    eval(expr)
  }, data, ...)
  return(data)
}
