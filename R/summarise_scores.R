#' @title Summarise scores as produced by [score()]
#'
#' @description Summarise scores as produced by [score()].
#'
#' `summarise_scores` relies on a way to identify the names of the scores and
#' distinguish them from columns that denote the unit of a single forecast.
#' Internally, this is done via a stored attribute, `score_names` that stores
#' the names of the scores. This means, however, that you need to be careful
#' with renaming scores after they have been produced by [score()]. If you
#' do, you also have to manually update the attribute by calling
#' `attr(scores, "score_names") <- new_names`.
#'
#' @param scores An object of class `scores` (a data.table with
#' scores and an additional attribute `score_names` as produced by [score()])
#' @param by character vector with column names to summarise scores by. Default
#' is `model`, meaning that there will be one score per model in the output.
#' @param across character vector with column names to summarise scores
#' across (meaning that the specified columns will be dropped). This is an
#' alternative to specifying `by` directly. If `across` is set, `by` will be
#' ignored. If `across` is `NULL` (default), then `by` will be used.
#' @param fun a function used for summarising scores. Default is [mean()].
#' @param ... additional parameters that can be passed to the summary function
#' provided to `fun`. For more information see the documentation of the
#' respective function.
#' @return a data.table with summarised scores. Scores are summarised according
#' to the names of the columns of the original data specified in `by` or
#' `across` using the `fun` passed to `summarise_scores()`.
#' @examples
#' \dontshow{
#'   data.table::setDTthreads(2) # restricts number of cores used on CRAN
#' }
#' library(magrittr) # pipe operator
#' scores <- score(as_forecast(example_continuous))
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
#' @keywords scoring

summarise_scores <- function(scores,
                             by = "model",
                             across = NULL,
                             fun = mean,
                             ...) {
  # input checking ------------------------------------------------------------
  # Check the score names attribute exists
  score_names <- attr(scores, "score_names")
  if (is.null(score_names)) {
    stop("`scores` needs to have an attribute `score_names` with ",
         "the names of the metrics that were used for scoring.")
  }

  if (!test_subset(score_names, names(scores))) {
    warning(
      "The names of the scores previously computed do not match the names ",
      "of the columns in `scores`. This may lead to unexpected results."
    )
  }

  # get the forecast unit (which relies on the presence of a scores attribute)
  forecast_unit <- get_forecast_unit(scores)

  assert_subset(by, names(scores), empty.ok = TRUE)
  assert_subset(across, names(scores), empty.ok = TRUE)
  assert_function(fun)

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
    .SDcols = colnames(scores) %like% paste(score_names, collapse = "|")
  ]

  attr(scores, "score_names") <- score_names
  return(scores[])
}

#' @rdname summarise_scores
#' @keywords scoring
#' @export
summarize_scores <- summarise_scores



#' @title Add pairwise comparisons
#' @description Adds a columns with relative skills computed by running
#' pairwise comparisons on the scores.
#' For more information on
#' the computation of relative skill, see [pairwise_comparison()].
#' Relative skill will be calculated for the aggregation level specified in
#' `by`.
#' @inheritParams pairwise_comparison
#' @export
#' @keywords keyword scoring
add_pairwise_comparison <- function(
  scores,
  by = "model",
  metric = intersect(c("wis", "crps", "brier_score"), names(scores)),
  baseline = NULL
) {

  # input checks are done in `pairwise_comparison()`
  # do pairwise comparisons ----------------------------------------------------
  pairwise <- pairwise_comparison(
    scores = scores,
    metric = metric,
    baseline = baseline,
    by = by
  )

  # store original score_names
  score_names <- get_score_names(scores)

  if (!is.null(pairwise)) {
    # delete unnecessary columns
    pairwise[, c(
      "compare_against", "mean_scores_ratio",
      "pval", "adj_pval"
    ) := NULL]
    pairwise <- unique(pairwise)

    # merge back
    scores <- merge(
      scores, pairwise, all.x = TRUE, by = get_forecast_unit(pairwise)
    )
  }

  # Update score names
  new_score_names <- paste(
    metric, c("relative_skill", "scaled_relative_skill"),
    sep = "_"
  )
  new_score_names <- new_score_names[new_score_names %in% names(scores)]
  scores <- new_scores(scores, score_names = c(score_names, new_score_names))

  return(scores)
}
