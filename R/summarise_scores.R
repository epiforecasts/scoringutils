#' @title Summarise scores as produced by [score()]
#'
#' @description Summarise scores as produced by [score()]
#'
#' @inheritParams pairwise_comparison
#' @inheritParams score
#' @param by character vector with column names to summarise scores by. Default
#' is `NULL`, meaning that the only summary that takes is place is summarising
#' over samples or quantiles (in case of quantile-based forecasts), such that
#' there is one score per forecast as defined by the *unit of a single forecast*
#' (rather than one score for every sample or quantile).
#' The *unit of a single forecast* is determined by the columns present in the
#' input data that do not correspond to a metric produced by [score()], which
#' indicate indicate a grouping of forecasts (for example there may be one
#' forecast per day, location and model). Adding additional, unrelated, columns
#' may alter results in an unpredictable way.
#' @param across character vector with column names from the vector of variables
#' that define the *unit of a single forecast* (see above) to summarise scores
#' across (meaning that the specified columns will be dropped). This is an
#' alternative to specifying `by` directly. If `NULL` (default), then `by` will
#' be used or inferred internally if also not specified. Only  one of `across`
#' and `by`  may be used at a time.
#' @param fun a function used for summarising scores. Default is `mean`.
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
#' \dontrun{
#' scores <- score(example_continuous)
#' }
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
#' @keywords scoring

summarise_scores <- function(scores,
                             by = NULL,
                             across = NULL,
                             fun = mean,
                             ...) {
  if (!is.null(across) && !is.null(by)) {
    stop("You cannot specify both 'across' and 'by'. Please choose one.")
  }

  score_names <- attr(scores, "score_names")
  if (is.null(score_names)) {
    stop("`scores` needs to have an attribute `score_names` with the names of
         the metrics that were used for scoring.")
  }

  # preparations ---------------------------------------------------------------
  # get unit of a single forecast
  forecast_unit <- get_forecast_unit(scores)

  # if by is not provided, set to the unit of a single forecast
  if (is.null(by)) {
    by <- forecast_unit
  }

  # if across is provided, remove from by
  if (!is.null(across)) {
    if (!all(across %in% forecast_unit)) {
      stop(
        "The columns specified in 'across' must be a subset of the columns ",
        "that define the forecast unit (possible options are ",
        toString(forecast_unit),
        "). Please check your input and try again."
      )
    }
    by <- setdiff(forecast_unit, across)
  }

  # check input arguments and check whether relative skill can be computed
  assert(check_columns_present(scores, by))

  # store attributes as they may be dropped in data.table operations
  stored_attributes <- c(
    get_scoringutils_attributes(scores),
    list(
      scoringutils_by = by,
      unsummarised_scores =  scores
    )
  )

  # summarise scores -----------------------------------------------------------
  scores <- scores[, lapply(.SD, fun, ...),
    by = c(by),
    .SDcols = colnames(scores) %like% paste(score_names, collapse = "|")
  ]

  # remove unnecessary columns -------------------------------------------------
  # if neither quantile nor range are in by, remove coverage and
  # quantile_coverage because averaging does not make sense
  if (!("interval_range" %in% by) && ("coverage" %in% colnames(scores))) {
    scores[, "coverage" := NULL]
  }
  if (!("quantile_level" %in% by) && "quantile_coverage" %in% names(scores)) {
    scores[, "quantile_coverage" := NULL]
  }

  scores <- assign_attributes(scores, stored_attributes)
  return(scores[])
}

#' @rdname summarise_scores
#' @keywords scoring
#' @export
summarize_scores <- summarise_scores



#' @title Add pairwise comparisons
#' @description Adds a columns with relative skills computed by running
#' pairwise comparisons on the scores.
#'
#' a column called
#' 'model' must be present in the input data. For more information on
#' the computation of relative skill, see [pairwise_comparison()].
#' Relative skill will be calculated for the aggregation level specified in
#' `by`.
#' WRITE MORE INFO HERE.
#'
#'
#' @param scores MORE INFO HERE.
#' @param by character vector with column names to summarise scores by. Default
#' is "model", meaning that there will be one relative skill score per model.
#' @param metric character with the name of the metric for which
#' a relative skill shall be computed.
#' @param baseline character string with the name of a model. If a baseline is
#' given, then a scaled relative skill with respect to the baseline will be
#' returned. By default (`NULL`), relative skill will not be scaled with
#' respect to a baseline model.
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
