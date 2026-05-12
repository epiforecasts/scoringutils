#' @title Filter scores
#'
#' @description
#' Filter a `scores` object using a supplied strategy function.
#' `filter_scores()` is responsible for preserving the `scores`
#' class and the `metrics` attribute; the strategy is
#' responsible only for the filtering logic.
#'
#' Strategies are constructed by helpers such as
#' [filter_to_intersection()] and [filter_to_include()] and can
#' also be user-defined. A strategy is a function with
#' signature `function(scores, compare)` that returns a
#' filtered data.table with the same columns as its input.
#'
#' @param scores An object of class `scores` (a data.table with
#'   an additional `metrics` attribute as produced by [score()]).
#' @param strategy A strategy function. See Description for the
#'   expected signature. Default: [filter_to_intersection()].
#' @param compare Character string (default `"model"`) naming the
#'   column whose values are compared when deciding which
#'   target combinations to keep.
#'
#' @return A `scores` object with the same class and `metrics`
#'   attribute as the input, with rows filtered according to
#'   `strategy`.
#'
#' @seealso [filter_to_intersection()], [filter_to_include()],
#'   \code{vignette("handling-missing-forecasts")}
#' @importFrom cli cli_inform
#' @importFrom checkmate assert_class assert_character
#'   assert_subset
#' @export
#' @keywords postprocess-scores
#' @examples
#' \dontshow{
#'   data.table::setDTthreads(2)
#' }
#' scores <- example_quantile |>
#'   as_forecast_quantile() |>
#'   score()
#'
#' # Keep only targets covered by every model (the default)
#' filter_scores(scores)
#'
#' # Keep targets covered by at least 75% of models
#' filter_scores(
#'   scores,
#'   strategy = filter_to_intersection(min_coverage = 0.75)
#' )
#'
#' # Keep only targets covered by a named model
#' filter_scores(
#'   scores,
#'   strategy = filter_to_include("EuroCOVIDhub-baseline")
#' )
filter_scores <- function(
  scores,
  strategy = filter_to_intersection(),
  compare = "model"
) {
  assert_class(scores, "scores")
  assert_character(compare, len = 1)
  assert_subset(compare, names(scores))
  assert_strategy(strategy, required = "compare")

  original_metrics <- attr(scores, "metrics")

  result <- strategy(scores, compare = compare)

  n_before <- nrow(scores)
  n_after <- nrow(result)
  n_dropped <- n_before - n_after

  if (n_dropped == 0) {
    cli_inform(c(
      i = "No rows filtered. Returning scores unchanged."
    ))
    return(scores)
  }

  cli_inform(c(
    i = "Filtered out {n_dropped} rows.",
    i = "{n_after} of {n_before} rows remaining." # nolint: duplicate_argument_linter
  ))

  return(new_scores(result, original_metrics))
}


#' @title Filter to target combinations meeting a coverage threshold
#'
#' @description
#' Strategy for [filter_scores()] that keeps target combinations
#' covered by at least `min_coverage` of the values in the
#' `compare` column. With the default `min_coverage = 1`, only
#' target combinations present for every compare value are kept
#' (strict intersection across the full set).
#'
#' To restrict to the targets covered by a named subset of
#' compare values instead of by a proportion, use
#' [filter_to_include()].
#'
#' @param min_coverage Numeric between 0 and 1 (default `1`).
#'   Minimum proportion of compare values that must cover a
#'   target combination for it to be kept.
#'
#' @return A strategy function for [filter_scores()]. Intended
#'   to be passed to `filter_scores()` rather than called
#'   directly — `filter_scores()` is where the `scores` class
#'   and `metrics` attribute are preserved.
#'
#' @seealso [filter_scores()], [filter_to_include()]
#' @importFrom data.table as.data.table setkeyv uniqueN
#' @importFrom checkmate assert_number
#' @export
#' @keywords postprocess-scores
#' @examples
#' \dontshow{
#'   data.table::setDTthreads(2)
#' }
#' scores <- example_quantile |>
#'   as_forecast_quantile() |>
#'   score()
#' filter_scores(
#'   scores,
#'   strategy = filter_to_intersection(min_coverage = 0.75)
#' )
filter_to_intersection <- function(min_coverage = 1) {
  assert_number(min_coverage, lower = 0, upper = 1)

  function(scores, compare = "model") {
    scores <- data.table::as.data.table(scores)
    forecast_unit <- get_forecast_unit(scores)
    target_cols <- setdiff(forecast_unit, compare)

    n_total <- data.table::uniqueN(scores[[compare]])

    target_coverage <- scores[,
      .(n_compare = data.table::uniqueN(get(compare))),
      by = target_cols
    ]

    keep <- target_coverage$n_compare / n_total >= min_coverage
    qualifying <- target_coverage[keep, target_cols, with = FALSE]

    data.table::setkeyv(scores, target_cols)
    data.table::setkeyv(qualifying, target_cols)
    scores[qualifying, nomatch = NULL]
  }
}


#' @title Filter to targets covered by named compare values
#'
#' @description
#' Strategy for [filter_scores()] that restricts the kept
#' target combinations to those covered by every value listed
#' in `include`. With a single value this keeps only that
#' value's targets; with several values, the intersection of
#' their target sets is kept.
#'
#' To use a proportion-based threshold over all compare values
#' instead, use [filter_to_intersection()].
#'
#' @param include Character vector of length one or more. Values
#'   from the `compare` column whose target sets should be
#'   intersected.
#'
#' @return A strategy function for [filter_scores()]. Intended
#'   to be passed to `filter_scores()` rather than called
#'   directly — `filter_scores()` is where the `scores` class
#'   and `metrics` attribute are preserved.
#'
#' @seealso [filter_scores()], [filter_to_intersection()]
#' @importFrom data.table as.data.table setkeyv
#' @importFrom checkmate assert_character
#' @importFrom cli cli_abort
#' @export
#' @keywords postprocess-scores
#' @examples
#' \dontshow{
#'   data.table::setDTthreads(2)
#' }
#' scores <- example_quantile |>
#'   as_forecast_quantile() |>
#'   score()
#' filter_scores(
#'   scores,
#'   strategy = filter_to_include("EuroCOVIDhub-baseline")
#' )
filter_to_include <- function(include) {
  assert_character(include, min.len = 1)

  function(scores, compare = "model") {
    scores <- data.table::as.data.table(scores)
    forecast_unit <- get_forecast_unit(scores)
    target_cols <- setdiff(forecast_unit, compare)

    unknown <- setdiff(include, unique(scores[[compare]]))
    if (length(unknown) > 0) {
      cli_abort(c(
        "!" = paste0(
          "{.val {unknown}} not found in ",
          "{.arg {compare}} column."
        )
      ))
    }

    target_sets <- lapply(include, function(v) {
      unique(
        scores[
          scores[[compare]] == v,
          target_cols,
          with = FALSE
        ]
      )
    })

    qualifying <- Reduce(
      function(a, b) merge(a, b, by = target_cols),
      target_sets
    )

    data.table::setkeyv(scores, target_cols)
    data.table::setkeyv(qualifying, target_cols)
    scores[qualifying, nomatch = NULL]
  }
}
