#' @title Impute missing scores
#'
#' @description
#' Fills in scores for forecast-target combinations that are
#' missing from the data, using a user-specified imputation
#' strategy. This is useful to ensure all models are evaluated
#' on the same set of targets, which avoids bias when
#' summarising scores.
#'
#' Missing combinations are identified by comparing each
#' element in `compare` against the full set of targets
#' present across all elements. The strategy function then
#' provides the imputed values for the missing metric columns.
#'
#' An `.imputed` column is added to the output indicating
#' which rows were imputed (`TRUE`) and which are original
#' (`FALSE`).
#'
#' @param scores An object of class `scores` (a data.table
#'   with scores and an additional attribute `metrics` as
#'   produced by [score()]).
#' @param strategy A function or factory-created function that
#'   fills missing metric values. Built-in options are
#'   [impute_worst_score()], [impute_mean_score()],
#'   [impute_na_score()], and [impute_model_score()].
#'   The function must accept four arguments:
#'   `(scores, missing_rows, metrics, compare)` and return
#'   `missing_rows` with metric columns filled.
#' @param compare Character vector of length one with the
#'   column name that defines the unit of comparison.
#'   Default is `"model"`.
#'
#' @return An object of class `scores` with an additional
#'   `.imputed` column. Rows that were imputed have
#'   `.imputed = TRUE`.
#'
#' @importFrom data.table copy set rbindlist setattr
#' @importFrom checkmate assert_class assert_function
#'   assert_character assert_subset
#' @importFrom cli cli_abort
#' @export
#' @keywords handle-metrics
#' @examples
#' \dontshow{
#'   data.table::setDTthreads(2)
#' }
#' scores <- example_quantile |>
#'   as_forecast_quantile() |>
#'   score()
#'
#' # Impute with NA values
#' impute_missing_scores(scores, strategy = impute_na_score())
impute_missing_scores <- function(
  scores,
  strategy,
  compare = "model"
) {
  assert_class(scores, "scores")
  metrics <- get_metrics.scores(scores, error = TRUE)
  assert_character(compare, len = 1)
  assert_subset(compare, names(scores))
  assert_function(strategy)

  scores <- copy(scores)

  missing_rows <- build_missing_grid(scores, compare) # nolint: object_usage_linter

  if (nrow(missing_rows) == 0) {
    data.table::set(scores, j = ".imputed", value = FALSE)
    return(scores[])
  }

  filled <- strategy(scores, missing_rows, metrics, compare)

  data.table::set(filled, j = ".imputed", value = TRUE)
  data.table::set(scores, j = ".imputed", value = FALSE)

  out <- rbindlist(list(scores, filled), use.names = TRUE,
                   fill = TRUE)

  out <- new_scores(out, metrics)
  return(out[])
}


#' @title Impute with worst (maximum) observed score
#'
#' @description
#' Creates an imputation strategy that fills each missing
#' metric with the worst (maximum) observed value for that
#' metric within the same target combination across all
#' elements of `compare`.
#'
#' @return A function suitable for use as the `strategy`
#'   argument in [impute_missing_scores()].
#' @export
#' @keywords handle-metrics
#' @examples
#' \dontshow{
#'   data.table::setDTthreads(2)
#' }
#' scores <- example_quantile |>
#'   as_forecast_quantile() |>
#'   score()
#'
#' impute_missing_scores(scores, strategy = impute_worst_score())
impute_worst_score <- function() {
  function(scores, missing_rows, metrics, compare) {
    fu <- get_forecast_unit(scores)
    target_cols <- setdiff(fu, compare)

    for (m in metrics) {
      if (!(m %in% names(scores))) next
      # Compute max per target combination
      agg <- scores[,
        .(..val = max(get(m), na.rm = TRUE)),
        by = target_cols
      ]
      # Merge onto missing_rows
      missing_rows <- merge(
        missing_rows, agg,
        by = target_cols, all.x = TRUE
      )
      data.table::set(
        missing_rows, j = m,
        value = missing_rows[["..val"]]
      )
      data.table::set(
        missing_rows, j = "..val", value = NULL
      )
    }
    return(missing_rows)
  }
}


#' @title Impute with mean observed score
#'
#' @description
#' Creates an imputation strategy that fills each missing
#' metric with the mean observed value for that metric within
#' the same target combination across all elements of
#' `compare`.
#'
#' @return A function suitable for use as the `strategy`
#'   argument in [impute_missing_scores()].
#' @export
#' @keywords handle-metrics
#' @examples
#' \dontshow{
#'   data.table::setDTthreads(2)
#' }
#' scores <- example_quantile |>
#'   as_forecast_quantile() |>
#'   score()
#'
#' impute_missing_scores(scores, strategy = impute_mean_score())
impute_mean_score <- function() {
  function(scores, missing_rows, metrics, compare) {
    fu <- get_forecast_unit(scores)
    target_cols <- setdiff(fu, compare)

    for (m in metrics) {
      if (!(m %in% names(scores))) next
      agg <- scores[,
        .(..val = mean(get(m), na.rm = TRUE)),
        by = target_cols
      ]
      missing_rows <- merge(
        missing_rows, agg,
        by = target_cols, all.x = TRUE
      )
      data.table::set(
        missing_rows, j = m,
        value = missing_rows[["..val"]]
      )
      data.table::set(
        missing_rows, j = "..val", value = NULL
      )
    }
    return(missing_rows)
  }
}


#' @title Impute with NA values
#'
#' @description
#' Creates an imputation strategy that fills each missing
#' metric with `NA_real_`.
#'
#' @return A function suitable for use as the `strategy`
#'   argument in [impute_missing_scores()].
#' @export
#' @keywords handle-metrics
#' @examples
#' \dontshow{
#'   data.table::setDTthreads(2)
#' }
#' scores <- example_quantile |>
#'   as_forecast_quantile() |>
#'   score()
#'
#' impute_missing_scores(scores, strategy = impute_na_score())
impute_na_score <- function() {
  function(scores, missing_rows, metrics, compare) {
    for (m in metrics) {
      data.table::set(missing_rows, j = m, value = NA_real_)
    }
    return(missing_rows)
  }
}


#' @title Impute with a reference model's scores
#'
#' @description
#' Creates an imputation strategy that fills missing scores
#' with the actual scores from a specified reference model
#' for each target combination.
#'
#' @param model Character string naming the reference model
#'   whose scores should be used for imputation. The reference
#'   model must have scores for all target combinations that
#'   need imputing.
#'
#' @return A function suitable for use as the `strategy`
#'   argument in [impute_missing_scores()].
#'
#' @importFrom cli cli_abort
#' @export
#' @keywords handle-metrics
#' @examples
#' \dontshow{
#'   data.table::setDTthreads(2)
#' }
#' scores <- example_quantile |>
#'   as_forecast_quantile() |>
#'   score()
#'
#' impute_missing_scores(
#'   scores,
#'   strategy = impute_model_score("EuroCOVIDhub-baseline")
#' )
impute_model_score <- function(model) {
  assert_character(model, len = 1)
  # Store in a different name to avoid collision with
  # the "model" column in data.table expressions
  ref_model_name <- model
  function(scores, missing_rows, metrics, compare) {
    fu <- get_forecast_unit(scores)
    target_cols <- setdiff(fu, compare)

    ref <- scores[
      get(compare) == ref_model_name
    ]

    if (nrow(ref) == 0) {
      cli_abort(
        c(
          "!" = "Reference model {.val {ref_model_name}}
             not found in scores."
        )
      )
    }

    # Check that the reference model has scores for all
    # needed target combinations
    needed <- unique(
      missing_rows[, target_cols, with = FALSE]
    )
    available <- unique(
      ref[, target_cols, with = FALSE]
    )
    missing_targets <- needed[!available,
      on = target_cols
    ]
    if (nrow(missing_targets) > 0) {
      cli_abort(
        c(
          "!" = "Reference model {.val {ref_model_name}}
             is missing scores for
             {nrow(missing_targets)} target
             combination{?s} that need imputing."
        )
      )
    }

    # Merge reference model scores onto missing rows
    ref_scores <- ref[,
      c(
        target_cols,
        metrics[metrics %in% names(ref)]
      ),
      with = FALSE
    ]
    missing_rows <- merge(
      missing_rows[,
        setdiff(names(missing_rows), metrics),
        with = FALSE
      ],
      ref_scores,
      by = target_cols,
      all.x = TRUE
    )
    return(missing_rows)
  }
}
