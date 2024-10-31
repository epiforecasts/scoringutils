#' @title Evaluate forecasts
#' @description `score()` applies a selection of scoring metrics to a forecast
#' object.
#' `score()` is a generic that dispatches to different methods depending on the
#' class of the input data.
#'
#' See [as_forecast_binary()], [as_forecast_quantile()] etc. for information on
#' how to create a forecast object.
#'
#' See [get_forecast_unit()] for more information on the concept of a forecast
#' unit.
#'
#' For additional help and examples, check out the paper
#' [Evaluating Forecasts with scoringutils in
#' R](https://arxiv.org/abs/2205.07090).
#' @param forecast A forecast object (a validated data.table with predicted and
#'   observed values).
#' @param metrics A named list of scoring functions. Names will be used as
#'   column names in the output. See [get_metrics()] for more information on the
#'   default metrics used. See the *Customising metrics* section below for
#'   information on how to pass custom arguments to scoring functions.
#' @param ... Currently unused. You *cannot* pass additional arguments to scoring
#'   functions via `...`. See the *Customising metrics* section below for
#'   details on how to use [purrr::partial()] to pass arguments to individual
#'   metrics.
#' @details
#' **Customising metrics**
#'
#' If you want to pass arguments to a scoring function, you need change the
#' scoring function itself via e.g. [purrr::partial()] and pass an updated list
#' of functions with your custom metric to the `metrics` argument in `score()`.
#' For example, to use [interval_coverage()] with `interval_range = 90`, you
#' would define a new function, e.g.
#' `interval_coverage_90 <- purrr::partial(interval_coverage, interval_range = 90)`
#' and pass this new function to `metrics` in `score()`.
#'
#' Note that if you want to pass a variable as an argument, you can
#' unquote it with `!!` to make sure the value is evaluated only once when the
#' function is created. Consider the following example:
#' ```
#' custom_arg <- "foo"
#' print1 <- purrr::partial(print, x = custom_arg)
#' print2 <- purrr::partial(print, x = !!custom_arg)
#'
#' custom_arg <- "bar"
#' print1() # prints 'bar'
#' print2() # prints 'foo'
#' ```
#'
#' @return
#' An object of class `scores`. This object is a data.table with
#' unsummarised scores (one score per forecast) and has an additional attribute
#' `metrics` with the names of the metrics used for scoring. See
#' [summarise_scores()]) for information on how to summarise
#' scores.
#' @importFrom data.table ':=' as.data.table
#' @importFrom stats na.omit
#' @keywords scoring
#' @examples
#' library(magrittr) # pipe operator
#' \dontshow{
#'   data.table::setDTthreads(2) # restricts number of cores used on CRAN
#' }
#'
#' validated <- as_forecast_quantile(example_quantile)
#' score(validated) %>%
#'   summarise_scores(by = c("model", "target_type"))
#'
#' # set forecast unit manually (to avoid issues with scoringutils trying to
#' # determine the forecast unit automatically)
#' example_quantile %>%
#'   as_forecast_quantile(
#'     forecast_unit = c(
#'       "location", "target_end_date", "target_type", "horizon", "model"
#'     )
#'   ) %>%
#'   score()
#'
#' # forecast formats with different metrics
#' \dontrun{
#' score(as_forecast_binary(example_binary))
#' score(as_forecast_quantile(example_quantile))
#' score(as_forecast_point(example_point))
#' score(as_forecast_sample(example_sample_discrete))
#' score(as_forecast_sample(example_sample_continuous))
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
    #nolint start: keyword_quote_linter
    c(
      "!" = "The input needs to be a valid forecast object.",
      "i" = "Please convert to a `forecast` object first by calling the
      appropriate {.fn as_forecast_<type>} function)."
    )
    #nolint end
  )
}


#' @title Apply a list of functions to a data table of forecasts
#' @description
#' This helper function applies scoring rules (stored as a list of
#' functions) to a data table of forecasts. `apply_metrics` is used within
#' `score()` to apply all scoring rules to the data.
#' Scoring rules are wrapped in [run_safely()] to catch errors and to make
#' sure that only arguments are passed to the scoring rule that are actually
#' accepted by it.
#' @param ... Additional arguments to be passed to the scoring rules. Note that
#'   this is currently not used, as all calls to `apply_scores` currently
#'   avoid passing arguments via `...` and instead expect that the metrics
#'   directly be modified using [purrr::partial()].
#' @inheritParams score
#' @returns A data table with the forecasts and the calculated metrics.
#' @keywords internal
apply_metrics <- function(forecast, metrics, ...) {
  lapply(names(metrics), function(metric_name) {
    result <- do.call(
      run_safely,
      list(..., fun = metrics[[metric_name]], metric_name = metric_name)
    )
    if (!is.null(result)) {
      forecast[, (metric_name) := result]
    }
  })
  return(forecast)
}


#' @title Run a function safely
#' @description
#' This is a wrapper/helper function designed to run a function safely
#' when it is not completely clear what arguments could be passed to the
#' function.
#'
#' All named arguments in `...` that are not accepted by `fun` are removed.
#' All unnamed arguments are passed on to the function. In case `fun` errors,
#' the error will be converted to a warning and `run_safely` returns `NULL`.
#'
#' `run_safely` can be useful when constructing functions to be used as
#' metrics in [score()].
#'
#' @param ... Arguments to pass to `fun`.
#' @param fun A function to execute.
#' @param metric_name A character string with the name of the metric. Used to
#'   provide a more informative warning message in case `fun` errors.
#' @importFrom cli cli_warn
#' @importFrom checkmate assert_function
#' @returns The result of `fun` or `NULL` if `fun` errors
#' @keywords internal
#' @examples
#' f <- function(x) {x}
#' scoringutils:::run_safely(2, fun = f, metric_name = "f")
#' scoringutils:::run_safely(2, y = 3, fun = f, metric_name = "f")
#' scoringutils:::run_safely(fun = f, metric_name = "f")
#' scoringutils:::run_safely(y = 3, fun = f, metric_name = "f")
run_safely <- function(..., fun, metric_name) {
  assert_function(fun)
  args <- list(...)
  # Check if the function accepts ... as an argument
  if ("..." %in% names(formals(fun))) {
    valid_args <- args
  } else if (is.null(names(args))) {
    # if no arguments are named, just pass all arguments on
    valid_args <- args
  } else {
    # Identify the arguments that fun() accepts
    possible_args <- names(formals(fun))
    # keep valid arguments as well as unnamed arguments
    valid_args <- args[names(args) == "" | names(args) %in% possible_args]
  }

  result <- try(do.call(fun, valid_args), silent = TRUE)

  if (inherits(result, "try-error")) {
    #nolint start: object_usage_linter
    msg <- conditionMessage(attr(result, "condition"))
    cli_warn(
      c(
        "!" = "Computation for {.var {metric_name}} failed.
        Error: {msg}."
      )
    )
    #nolint end
    return(NULL)
  }
  return(result)
}


#' @title Validate metrics
#'
#' @description
#' This function validates whether the list of metrics is a list
#' of valid functions.
#'
#' The function is used in [score()] to make sure that all metrics are valid
#' functions.
#'
#' @param metrics A named list with metrics. Every element should be a scoring
#'   function to be applied to the data.
#' @importFrom cli cli_warn
#'
#' @return
#' A named list of metrics, with those filtered out that are not
#' valid functions
#' @importFrom checkmate assert_list test_list check_function
#' @keywords internal_input_check
validate_metrics <- function(metrics) {

  assert_list(metrics, min.len = 1, names = "named")

  for (i in seq_along(metrics)) {
    check_fun <- check_function(metrics[[i]])
    if (!isTRUE(check_fun)) {
      #nolint start: keyword_quote_linter
      cli_warn(
        c(
          "!" = "`Metrics` element number {i} is not a valid function."
        )
      )
      #nolint end
      names(metrics)[i] <- "scoringutils_delete"
    }
  }
  metrics[names(metrics) == "scoringutils_delete"] <- NULL

  assert_list(metrics, min.len = 1, .var.name = "valid metrics")

  return(metrics)
}
