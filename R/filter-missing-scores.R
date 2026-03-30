#' @title Filter scores with missing model-target combinations
#'
#' @description
#' Filters a `scores` object to remove target combinations where
#' one or more models have missing scores.
#' The filtering behaviour is controlled by the `strategy`
#' argument, which defaults to [filter_to_intersection()].
#'
#' @param scores An object of class `scores` (a data.table with
#'   scores and an additional attribute `metrics` as produced
#'   by [score()]).
#' @param strategy A strategy function as returned by
#'   [filter_to_intersection()]. Default is
#'   `filter_to_intersection()`.
#' @param compare Character string (default `"model"`) naming the
#'   column whose values are compared for missingness.
#'
#' @return A filtered `scores` object with the same class and
#'   `metrics` attribute as the input.
#'
#' @importFrom cli cli_inform
#' @importFrom checkmate assert_class assert_character
#'   assert_function
#' @export
#' @keywords handle-metrics
filter_missing_scores <- function(
  scores,
  strategy = filter_to_intersection(),
  compare = "model"
) {
  assert_class(scores, "scores")
  assert_character(compare, len = 1)
  assert_function(strategy)

  original_class <- class(scores)
  original_metrics <- attr(scores, "metrics")

  #nolint start: object_usage_linter
  missing <- build_missing_grid(scores, compare = compare)
  #nolint end

  if (nrow(missing) == 0) {
    #nolint start: keyword_quote_linter
    cli_inform(c(
      "i" = "No missing score combinations found. Returning
       scores unchanged."
    ))
    #nolint end
    return(scores)
  }

  result <- strategy(scores, compare = compare)

  n_before <- nrow(scores)
  n_after <- nrow(result)
  #nolint start: object_usage_linter
  n_dropped <- n_before - n_after
  #nolint end

  #nolint start: keyword_quote_linter
  cli_inform(c(
    "i" = "Filtered out {n_dropped} rows with missing
     {compare} combinations.",
    "i" = "{n_after} of {n_before} rows remaining."
  ))
  #nolint end

  # Preserve class and metrics
  class(result) <- original_class
  data.table::setattr(result, "metrics", original_metrics)

  return(result)
}


#' @title Filter to intersection of model-target combinations
#'
#' @description
#' Strategy factory for [filter_missing_scores()].
#' Returns a function that keeps only target combinations
#' covered by a minimum proportion of models.
#'
#' @param min_coverage Numeric between 0 and 1 (default `1`).
#'   Minimum proportion of models that must cover a target
#'   combination for it to be kept.
#' @param models Character vector or `NULL` (default). If
#'   provided, the target grid is restricted to targets covered
#'   by these models. When multiple models are specified, only
#'   the intersection of their targets is used.
#'
#' @return A function with signature `function(scores, compare)`
#'   suitable for use as a strategy in
#'   [filter_missing_scores()].
#'
#' @importFrom data.table as.data.table setkeyv
#' @importFrom checkmate assert_number assert_character
#' @export
#' @keywords handle-metrics
filter_to_intersection <- function(
  min_coverage = 1,
  models = NULL
) {
  assert_number(min_coverage, lower = 0, upper = 1)
  if (!is.null(models)) {
    assert_character(models, min.len = 1)
  }

  function(scores, compare = "model") {
    scores <- data.table::as.data.table(scores)
    forecast_unit <- get_forecast_unit(scores)
    target_cols <- setdiff(forecast_unit, compare)

    if (!is.null(models)) {
      # Restrict to targets covered by specified models
      model_targets <- lapply(models, function(m) {
        unique(
          scores[
            scores[[compare]] == m,
            target_cols,
            with = FALSE
          ]
        )
      })
      # Intersection of all specified models' targets
      qualifying <- model_targets[[1]]
      if (length(model_targets) > 1) {
        for (i in seq(2, length(model_targets))) {
          data.table::setkeyv(qualifying, target_cols)
          data.table::setkeyv(
            model_targets[[i]], target_cols
          )
          qualifying <- merge(
            qualifying, model_targets[[i]],
            by = target_cols
          )
        }
      }
    } else {
      # Count models per target combination
      all_models <- unique(scores[[compare]])
      n_total <- length(all_models)

      target_coverage <- scores[
        , .(n_models = data.table::uniqueN(get(compare))),
        by = target_cols
      ]
      #nolint start: object_usage_linter
      qualifying <- target_coverage[
        n_models / n_total >= min_coverage,
        #nolint end
        target_cols,
        with = FALSE
      ]
    }

    # Semi-join: keep scores rows matching qualifying targets
    data.table::setkeyv(scores, target_cols)
    data.table::setkeyv(qualifying, target_cols)
    result <- scores[qualifying, nomatch = NULL]

    return(result)
  }
}
