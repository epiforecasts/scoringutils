#' @title Evaluate forecasts
#'
#' @description This function allows automatic scoring of forecasts using a
#' range of metrics. For most users it will be the workhorse for
#' scoring forecasts as it wraps the lower level functions package functions.
#' However, these functions are also available if you wish to make use of them
#' independently.
#'
#' A range of forecasts formats are supported, including quantile-based,
#' sample-based, binary forecasts. Prior to scoring, users may wish to make use
#' of [validate()] to ensure that the input data is in a supported
#' format though this will also be run internally by [score()]. Examples for
#' each format are also provided (see the documentation for `data` below or in
#' [validate()]).
#'
#' Each format has a set of required columns (see below). Additional columns may
#' be present to indicate a grouping of forecasts. For example, we could have
#' forecasts made by different models in various locations at different time
#' points, each for several weeks into the future. It is important, that there
#' are only columns present which are relevant in order to group forecasts.
#' A combination of different columns should uniquely define the
#' *unit of a single forecast*, meaning that a single forecast is defined by the
#' values in the other columns. Adding additional unrelated columns may alter
#' results.
#'
#' To obtain a quick overview of the currently supported evaluation metrics,
#' have a look at the [metrics] data included in the package. The column
#' `metrics$Name` gives an overview of all available metric names that can be
#' computed. If interested in an unsupported metric please open a [feature
#' request](https://github.com/epiforecasts/scoringutils/issues) or consider
#' contributing a pull request.
#'
#' For additional help and examples, check out the [Getting Started
#' Vignette](https://epiforecasts.io/scoringutils/articles/scoringutils.html)
#' as well as the paper [Evaluating Forecasts with scoringutils in
#' R](https://arxiv.org/abs/2205.07090).
#'
#' @param data A data.frame or data.table with the following columns:
#' - `observed` - the observed values
#' - `predicted` - predictions, predictive samples or predictive quantiles
#' - `model` - name of the model or forecaster who made a prediction
#'
#' Depending on the forecast type, one of the following columns may be required:
#' - `sample_id` - index for the predictive samples in the 'predicted' column
#' - `quantile`: quantile-level of the corresponding value in `predicted`
#'
#' For more information see the vignettes and the example data
#' ([example_quantile], [example_continuous],
#' [example_integer], [example_point()], and [example_binary]).
#'
#' @param metrics the metrics you want to have in the output. If `NULL` (the
#' default), all available metrics will be computed. For a list of available
#' metrics see [available_metrics()], or  check the [metrics] data set.
#'
#' @param ... additional parameters passed down to other functions.
#'
#' @return A data.table with unsummarised scores. There will be one score per
#' quantile or sample_id, which is usually not desired, so you should almost
#' always run [summarise_scores()] on the unsummarised scores.
#'
#' @importFrom data.table ':=' as.data.table
#'
#' @examples
#' library(magrittr) # pipe operator
#' data.table::setDTthreads(1) # only needed to avoid issues on CRAN
#'
#' validate(example_quantile)
#' score(example_quantile) %>%
#'   add_coverage(by = c("model", "target_type")) %>%
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
#' score(example_integer)
#' score(example_continuous)
#' }
#'
#' # score point forecasts (marked by 'NA' in the quantile column)
#' score(example_point) %>%
#'   summarise_scores(by = "model", na.rm = TRUE)
#'
#' @author Nikos Bosse \email{nikosbosse@@gmail.com}
#' @references Funk S, Camacho A, Kucharski AJ, Lowe R, Eggo RM, Edmunds WJ
#' (2019) Assessing the performance of real-time epidemic forecasts: A
#' case study of Ebola in the Western Area region of Sierra Leone, 2014-15.
#' PLoS Comput Biol 15(2): e1006785. \doi{10.1371/journal.pcbi.1006785}
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
  forecast_unit <- attr(data, "forecast_unit")
  metrics <- validate_metrics(metrics)

  # Extract the arguments passed in ...
  args <- list(...)
  lapply(seq_along(metrics), function(i, ...) {
    metric_name <- names(metrics[i])
    fun <- metrics[[i]]
    matching_args <- filter_function_args(fun, args)

    data[, (metric_name) := do.call(
      fun, c(list(observed, predicted), matching_args))
    ]
    return()
  }, ...)

  return(data[])

}


#' @importFrom Metrics se ae ape
#' @rdname score
#' @export
score.scoringutils_point <- function(data, metrics = metrics_point, ...) {
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
      fun, c(list(observed, predicted), matching_args))
    ]
    return()
  }, ...)

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
      fun, c(list(unique(observed), t(predicted)), matching_args)),
      by = forecast_unit
    ]
    return()
  }, ...)

  data <- data[
    , lapply(.SD, unique),
    .SDcols = colnames(data) %like% paste(names(metrics), collapse = "|"),
    by = forecast_unit
  ]


  return(data[])
}

#' @rdname score
#' @export
score.scoringutils_quantile <- function(data, metrics = NULL, ...) {
  data <- validate(data)
  data <- remove_na_observed_predicted(data)
  forecast_unit <- attr(data, "forecast_unit")

  metrics <- check_metrics(metrics)
  scores <- score_quantile(
    data = data,
    forecast_unit = forecast_unit,
    metrics = metrics,
    ...
  )

  return(scores[])
}


#' @importFrom checkmate assert_list test_list check_function

validate_metrics <- function(metrics) {

  assert_list(metrics, min.len = 1)

  if (!test_list(metrics, names = "named")) {
    # construct substitute names based on the name of the function provided to
    # the parent function --> get call object and extract the metrics part of it

    n_frames <- sys.nframe()

    for (i in 1:n_frames) {
      call <- sys.call(-i)[["metrics"]]
      if (!is.null(call)) {
        substitute_names <- as.character(sys.call(-4)[["metrics"]])
        substitute_names <- substitute_names[!(substitute_names %in% c("c", "list"))]
        break
      }
    }

    if (is.null(names(metrics))) {
      names(metrics) <- substitute_names
    }
    # Replace missing metric names by their substitutes
    is_missing_name <- names(metrics) == ""
    names(metrics)[is_missing_name] <- substitute_names[is_missing_name]
  }


  for (i in seq_along(metrics)) {
    check_fun <- check_function(metrics[[i]])
    if (!is.logical(check_fun)) {
      warning("`Metrics` element number ", i, " is not a valid function")
      names(metrics)[i] <- "scoringutils_delete"
    }
  }
  metrics[names(metrics) == "scoringutils_delete"] <- NULL

  assert_list(metrics, min.len = 1, .var.name = "valid metrics")

  return(metrics)
}



