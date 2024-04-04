#' @title Evaluate forecasts
#' @description `score()` applies a selection of scoring metrics to a forecast
#' object (a data.table with forecasts and observations) as produced by
#' [as_forecast()].
#' `score()` is a generic that dispatches to different methods depending on the
#' class of the input data.
#'
#' See the details section for more information on forecast types and input
#' formats. For additional help and examples, check out the [Getting Started
#' Vignette](https://epiforecasts.io/scoringutils/articles/scoringutils.html) as
#' well as the paper [Evaluating Forecasts with scoringutils in
#' R](https://arxiv.org/abs/2205.07090).
#' @inheritSection forecast_types Forecast types and input formats
#' @inheritSection forecast_types Forecast unit
#' @param forecast A forecast object (a validated data.table with predicted and
#'   observed values, see [as_forecast()])
#' @param metrics A named list of scoring functions. Names will be used as
#'   column names in the output. See [metrics_point()], [metrics_binary()],
#'   [metrics_quantile()], and [metrics_sample()] for more information on the
#'   default metrics used.
#' @param ... Additional arguments
#' @return
#' An object of class `scores`. This object is a data.table with
#' unsummarised scores (one score per forecast) and has an additional attribute
#' `metrics` with the names of the metrics used for scoring. See
#' [summarise_scores()]) for information on how to summarise
#' scores.
#' @importFrom data.table ':=' as.data.table
#' @importFrom stats na.omit
#' @examples
#' library(magrittr) # pipe operator
#' \dontshow{
#'   data.table::setDTthreads(2) # restricts number of cores used on CRAN
#' }
#'
#' validated <- as_forecast(example_quantile)
#' score(validated) %>%
#'   summarise_scores(by = c("model", "target_type"))
#'
#' # set forecast unit manually (to avoid issues with scoringutils trying to
#' # determine the forecast unit automatically)
#' example_quantile %>%
#'   as_forecast(
#'     forecast_unit = c(
#'       "location", "target_end_date", "target_type", "horizon", "model"
#'     )
#'   ) %>%
#'   score()
#'
#' # forecast formats with different metrics
#' \dontrun{
#' score(as_forecast(example_binary))
#' score(as_forecast(example_quantile))
#' score(as_forecast(example_point))
#' score(as_forecast(example_sample_discrete))
#' score(as_forecast(example_sample_continuous))
#' }
#' @author Nikos Bosse \email{nikosbosse@@gmail.com}
#' @references
#' Bosse NI, Gruson H, Cori A, van Leeuwen E, Funk S, Abbott S
#' (2022) Evaluating Forecasts with scoringutils in R.
#' \doi{10.48550/arXiv.2205.07090}
#' @export

score <- function(forecast, metrics, ...) {
  UseMethod("score")
}

#' @importFrom cli cli_abort
#' @export
score.default <- function(forecast, metrics, ...) {
  cli_abort(
    c(
      "!" = "The input needs to be a forecast object.",
      "i" = "Please run `as_forecast()` first." # nolint
    )
  )
}

#' @importFrom stats na.omit
#' @importFrom data.table setattr copy
#' @rdname score
#' @export
score.forecast_binary <- function(forecast, metrics = metrics_binary(), ...) {
  forecast <- copy(forecast)
  suppressWarnings(suppressMessages(validate_forecast(forecast)))
  forecast <- na.omit(forecast)
  metrics <- validate_metrics(metrics)

  scores <- apply_metrics(
    forecast, metrics,
    forecast$observed, forecast$predicted, ...
  )

  scores <- as_scores(scores, metrics = names(metrics))
  return(scores[])
}


#' @importFrom Metrics se ae ape
#' @importFrom stats na.omit
#' @importFrom data.table setattr copy
#' @rdname score
#' @export
score.forecast_point <- function(forecast, metrics = metrics_point(), ...) {
  forecast <- copy(forecast)
  suppressWarnings(suppressMessages(validate_forecast(forecast)))
  forecast <- na.omit(forecast)
  metrics <- validate_metrics(metrics)

  scores <- apply_metrics(
    forecast, metrics,
    forecast$observed, forecast$predicted, ...
  )

  scores <- as_scores(scores, metrics = names(metrics))
  return(scores[])
}

#' @importFrom stats na.omit
#' @importFrom data.table setattr copy
#' @rdname score
#' @export
score.forecast_sample <- function(forecast, metrics = metrics_sample(), ...) {
  forecast <- copy(forecast)
  suppressWarnings(suppressMessages(validate_forecast(forecast)))
  forecast <- na.omit(forecast)
  forecast_unit <- get_forecast_unit(forecast)
  metrics <- validate_metrics(metrics)

  # transpose the forecasts that belong to the same forecast unit
  f_transposed <- forecast[, .(predicted = list(predicted),
                               observed = unique(observed),
                               scoringutils_N = length(list(sample_id))),
                           by = forecast_unit]

  # split according to number of samples and do calculations for different
  # sample lengths separately
  f_split <- split(f_transposed, f_transposed$scoringutils_N)

  split_result <- lapply(f_split, function(forecast) {
    # create a matrix
    observed <- forecast$observed
    predicted <- do.call(rbind, forecast$predicted)
    forecast[, c("observed", "predicted", "scoringutils_N") := NULL]

    forecast <- apply_metrics(
      forecast, metrics,
      observed, predicted, ...
    )
    return(forecast)
  })
  scores <- rbindlist(split_result)
  scores <- as_scores(scores, metrics = names(metrics))
  return(scores[])
}


#' @importFrom stats na.omit
#' @importFrom data.table `:=` as.data.table rbindlist %like% setattr copy
#' @rdname score
#' @export
score.forecast_quantile <- function(forecast, metrics = metrics_quantile(), ...) {
  forecast <- copy(forecast)
  suppressWarnings(suppressMessages(validate_forecast(forecast)))
  forecast <- na.omit(forecast)
  forecast_unit <- get_forecast_unit(forecast)
  metrics <- validate_metrics(metrics)

  # transpose the forecasts that belong to the same forecast unit
  # make sure the quantiles and predictions are ordered in the same way
  f_transposed <- forecast[, .(
    predicted = list(predicted[order(quantile_level)]),
    observed = unique(observed),
    quantile_level = list(sort(quantile_level, na.last = TRUE)),
    scoringutils_quantile_level = toString(sort(quantile_level, na.last = TRUE))
  ), by = forecast_unit]

  # split according to quantile_level lengths and do calculations for different
  # quantile_level lengths separately. The function `wis()` assumes that all
  # forecasts have the same quantile_levels
  f_split <- split(f_transposed, f_transposed$scoringutils_quantile_level)

  split_result <- lapply(f_split, function(forecast) {
    # create a matrix out of the list of predicted values and quantile_levels
    observed <- forecast$observed
    predicted <- do.call(rbind, forecast$predicted)
    quantile_level <- unlist(unique(forecast$quantile_level))
    forecast[, c(
      "observed", "predicted", "quantile_level", "scoringutils_quantile_level"
    ) := NULL]

    forecast <- apply_metrics(
      forecast, metrics,
      observed, predicted, quantile_level, ...
    )
    return(forecast)
  })
  scores <- rbindlist(split_result)

  scores <- as_scores(scores, metrics = names(metrics))

  return(scores[])
}


#' @title Apply a list of functions to a data table of forecasts
#' @description
#' This helper function applies scoring rules (stored as a list of
#' functions) to a data table of forecasts. `apply_metrics` is used within
#' `score()` to apply all scoring rules to the data.
#' Scoring rules are wrapped in [run_safely()] to catch errors and to make
#' sure that only arguments are passed to the scoring rule that are actually
#' accepted by it.
#' @inheritParams score
#' @return A data table with the forecasts and the calculated metrics.
#' @keywords internal
apply_metrics <- function(forecast, metrics, ...) {
  expr <- expression(
    forecast[, (metric_name) := do.call(run_safely, list(..., fun = fun))]
  )
  lapply(seq_along(metrics), function(i, forecast, ...) {
    metric_name <- names(metrics[i]) # nolint
    fun <- metrics[[i]] # nolint
    eval(expr)
  }, forecast, ...)
  return(forecast)
}


#' Construct an object of class `scores`
#' @description
#' This function creates an object of class `scores` based on a
#' data.table or similar.
#' @param scores A data.table or similar with scores as produced by [score()].
#' @param metrics A character vector with the names of the scores
#'   (i.e. the names of the scoring rules used for scoring).
#' @param ... Additional arguments to [as.data.table()]
#' @keywords internal
#' @importFrom data.table as.data.table setattr
#' @return An object of class `scores`
#' @examples
#' \dontrun{
#' df <- data.frame(
#'   model = "A",
#'   wis = "0.1"
#' )
#' new_scores(df, "wis")
#' }
new_scores <- function(scores, metrics, ...) {
  scores <- as.data.table(scores, ...)
  class(scores) <- c("scores", class(scores))
  setattr(scores, "metrics", metrics)
  return(scores[])
}


#' Create an object of class `scores` from data
#' @description This convenience function wraps [new_scores()] and validates
#'   the `scores` object.
#' @inherit new_scores params return
#' @importFrom checkmate assert_data_frame
#' @keywords internal
as_scores <- function(scores, metrics) {
  assert_data_frame(scores)
  present_metrics <- metrics[metrics %in% colnames(scores)]
  scores <- new_scores(scores, present_metrics)
  validate_scores(scores)
  return(scores[])
}


#' Validate an object of class `scores`
#' @description
#' This function validates an object of class `scores`, checking
#' that it has the correct class and that it has a `metrics` attribute.
#' @inheritParams new_scores
#' @returns Returns `NULL` invisibly
#' @importFrom checkmate assert_class assert_data_frame
#' @keywords internal
validate_scores <- function(scores) {
  assert_data_frame(scores)
  assert_class(scores, "scores")
  # error if no metrics exists +
  # throw warning if any of the metrics is not in the data
  get_metrics(scores, error = TRUE)
  return(invisible(NULL))
}

##' @method `[` scores
##' @export
`[.scores` <- function(x, ...) {
  ret <- NextMethod()
  if (is.data.frame(ret)) {
    attr(ret, "metrics") <- attr(x, "metrics")
  }
  return(ret)
}
