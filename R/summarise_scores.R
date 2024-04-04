#' @title Summarise scores as produced by [score()]
#'
#' @description
#' Summarise scores as produced by [score()].
#'
#' `summarise_scores` relies on a way to identify the names of the scores and
#' distinguish them from columns that denote the unit of a single forecast.
#' Internally, this is done via a stored attribute, `metrics` that stores
#' the names of the scores. This means, however, that you need to be careful
#' with renaming scores after they have been produced by [score()]. If you
#' do, you also have to manually update the attribute by calling
#' `attr(scores, "metrics") <- new_names`.
#'
#' @param scores An object of class `scores` (a data.table with
#'   scores and an additional attribute `metrics` as produced by [score()]).
#' @param by Character vector with column names to summarise scores by. Default
#'   is `model`, meaning that there will be one score per model in the output.
#' @param across Character vector with column names to summarise scores
#'   across (meaning that the specified columns will be dropped). This is an
#'   alternative to specifying `by` directly. If `across` is set, `by` will be
#'   ignored. If `across` is `NULL` (default), then `by` will be used.
#' @param fun A function used for summarising scores. Default is [mean()].
#' @param ... Additional parameters that can be passed to the summary function
#'   provided to `fun`. For more information see the documentation of the
#'   respective function.
#' @return
#' A data.table with summarised scores. Scores are summarised according
#' to the names of the columns of the original data specified in `by` or
#' `across` using the `fun` passed to `summarise_scores()`.
#' @examples
#' \dontshow{
#'   data.table::setDTthreads(2) # restricts number of cores used on CRAN
#' }
#' library(magrittr) # pipe operator
#' scores <- score(as_forecast(example_sample_continuous))
#'
#' # get scores by model
#' summarise_scores(scores,by = "model")
#'
#' # get scores by model and target type
#' summarise_scores(scores, by = c("model", "target_type"))
#'
#' # Get scores summarised across horizon, forecast date, and target end date
#' summarise_scores(
#'  scores, across = c("horizon", "forecast_date", "target_end_date")
#' )
#'
#' # get standard deviation
#' summarise_scores(scores, by = "model", fun = sd)
#'
#' # round digits
#' summarise_scores(scores,by = "model") %>%
#'   summarise_scores(fun = signif, digits = 2)
#' @export
#' @importFrom checkmate assert_subset assert_function test_subset
#'   assert_data_frame
#' @keywords scoring

summarise_scores <- function(scores,
                             by = "model",
                             across = NULL,
                             fun = mean,
                             ...) {
  # input checking ------------------------------------------------------------
  assert_data_frame(scores)
  assert_subset(by, names(scores), empty.ok = TRUE)
  assert_subset(across, names(scores), empty.ok = TRUE)
  assert_function(fun)
  metrics <- get_metrics(scores, error = TRUE)

  forecast_unit <- get_forecast_unit(scores)

  # if across is provided, calculate new `by`
  if (!is.null(across)) {
    if (!setequal(by, "model")) {
      warning("You specified `across` and `by` at the same time.",
              "`by` will be ignored.")
    }
    by <- setdiff(forecast_unit, across)
  }

  # summarise scores -----------------------------------------------------------
  scores <- scores[, lapply(.SD, fun, ...),
    by = c(by),
    .SDcols = colnames(scores) %like% paste(metrics, collapse = "|")
  ]

  attr(scores, "metrics") <- metrics
  return(scores[])
}

#' @rdname summarise_scores
#' @keywords scoring
#' @export
summarize_scores <- summarise_scores
