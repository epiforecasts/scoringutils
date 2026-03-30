#' @title Filter scores
#'
#' @description
#' Filters a `scores` object according to a given strategy.
#' The filtering behaviour is controlled by the `strategy`
#' argument, which defaults to [filter_to_intersection()].
#' This is a general-purpose filtering function that delegates
#' all logic to the strategy.
#'
#' @param scores An object of class `scores` (a data.table with
#'   scores and an additional attribute `metrics` as produced
#'   by [score()]).
#' @param strategy A strategy function as returned by
#'   [filter_to_intersection()]. Default is
#'   `filter_to_intersection()`.
#' @param compare Character string (default `"model"`) naming the
#'   column whose values are compared for filtering.
#'
#' @return A filtered `scores` object with the same class and
#'   `metrics` attribute as the input.
#'
#' @importFrom cli cli_inform
#' @importFrom checkmate assert_class assert_character
#'   assert_function assert_subset
#' @export
#' @keywords handle-metrics
filter_scores <- function(
  scores,
  strategy = filter_to_intersection(),
  compare = "model"
) {
  assert_class(scores, "scores")
  assert_character(compare, len = 1)
  assert_subset(compare, names(scores))
  assert_function(strategy)

  original_class <- class(scores)
  original_metrics <- attr(scores, "metrics")

  result <- strategy(scores, compare = compare)

  n_before <- nrow(scores)
  n_after <- nrow(result)
  #nolint start: object_usage_linter
  n_dropped <- n_before - n_after
  #nolint end

  if (n_dropped == 0) {
    #nolint start: keyword_quote_linter
    cli_inform(c(
      "i" = "No rows filtered. Returning scores unchanged."
    ))
    #nolint end
    return(scores)
  }

  #nolint start: keyword_quote_linter
  cli_inform(c(
    "i" = "Filtered out {n_dropped} rows.",
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
#' Strategy factory for [filter_scores()].
#' Returns a function that keeps only target combinations
#' covered by a minimum proportion of comparators.
#'
#' @param min_coverage Numeric between 0 and 1 (default `1`).
#'   Minimum proportion of comparators that must cover a
#'   target combination for it to be kept.
#' @param include Character vector or `NULL` (default). If
#'   provided, the target grid is restricted to targets
#'   covered by these values of the `compare` column. When
#'   multiple values are given, only the intersection of
#'   their targets is used.
#'
#' @return A function with signature `function(scores, compare)`
#'   suitable for use as a strategy in
#'   [filter_scores()].
#'
#' @importFrom data.table as.data.table setkeyv
#' @importFrom checkmate assert_number assert_character
#' @export
#' @keywords handle-metrics
filter_to_intersection <- function(
  min_coverage = 1,
  include = NULL
) {
  assert_number(min_coverage, lower = 0, upper = 1)
  if (!is.null(include)) {
    assert_character(include, min.len = 1)
  }

  function(scores, compare = "model") {
    scores <- data.table::as.data.table(scores)
    forecast_unit <- get_forecast_unit(scores)
    target_cols <- setdiff(forecast_unit, compare)

    if (!is.null(include)) {
      unknown <- setdiff(include, unique(scores[[compare]]))
      if (length(unknown) > 0) {
        cli::cli_abort(c(
          "!" = paste0(
            "{.val {unknown}} not found in ",
            "{.arg {compare}} column."
          )
        ))
      }
      # Restrict to targets covered by specified values
      model_targets <- lapply(include, function(m) {
        unique(
          scores[
            scores[[compare]] == m,
            target_cols,
            with = FALSE
          ]
        )
      })
      # Intersection of all specified values' targets
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
      # Count include per target combination
      all_include <- unique(scores[[compare]])
      n_total <- length(all_include)

      target_coverage <- scores[
        , .(n_include = data.table::uniqueN(get(compare))),
        by = target_cols
      ]
      #nolint start: object_usage_linter
      qualifying <- target_coverage[
        n_include / n_total >= min_coverage,
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
