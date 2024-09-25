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
#'   is "model", i.e. scores are summarised by the "model" column.
#' @param fun A function used for summarising scores. Default is [mean()].
#' @param ... Additional parameters that can be passed to the summary function
#'   provided to `fun`. For more information see the documentation of the
#'   respective function.
#' @return
#' A data.table with summarised scores. Scores are summarised according
#' to the names of the columns of the original data specified in `by`
#' using the `fun` passed to `summarise_scores()`.
#' @examples
#' \dontshow{
#'   data.table::setDTthreads(2) # restricts number of cores used on CRAN
#' }
#' library(magrittr) # pipe operator
#' scores <- example_sample_continuous %>%
#'  as_forecast_sample() %>%
#'  score()
#'
#' # get scores by model
#' summarise_scores(scores, by = "model")
#'
#' # get scores by model and target type
#' summarise_scores(scores, by = c("model", "target_type"))
#'
#' # get standard deviation
#' summarise_scores(scores, by = "model", fun = sd)
#'
#' # round digits
#' summarise_scores(scores, by = "model") %>%
#'   summarise_scores(fun = signif, digits = 2)
#' @export
#' @importFrom checkmate assert_subset assert_function test_subset
#'   assert_data_frame
#' @keywords scoring

summarise_scores <- function(scores,
                             by = "model",
                             fun = mean,
                             ...) {
  # input checking ------------------------------------------------------------
  assert_data_frame(scores)
  scores <- ensure_data.table(scores)
  assert_subset(by, names(scores), empty.ok = FALSE)
  assert_function(fun)

  metrics <- get_metrics.scores(scores, error = TRUE)

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
