#' @title Impute missing scores
#'
#' @description
#' Fills in scores for forecast-target combinations that are
#' missing from the data, using a user-specified imputation
#' strategy. This is useful to ensure all models are evaluated
#' on the same set of targets, which avoids bias when
#' summarising scores.
#'
#' Missing combinations are identified by comparing each value
#' of the `compare` column against the union of targets observed
#' across all values. The strategy is then called to fill the
#' metric columns for those rows.
#'
#' An `.imputed` column is added to the output indicating which
#' rows were imputed (`TRUE`) and which are original (`FALSE`).
#'
#' @param scores An object of class `scores` (a data.table with
#'   an additional `metrics` attribute as produced by [score()]).
#' @param strategy A strategy function with signature
#'   `function(scores, missing_rows, metrics, compare)` that
#'   returns `missing_rows` with the metric columns filled.
#'   Built-in options are [impute_worst_score()],
#'   [impute_mean_score()], [impute_na_score()], and
#'   [impute_model_score()]. Custom strategies are also
#'   supported.
#' @param compare Character string (default `"model"`) naming the
#'   column whose values are compared against each target to
#'   identify missing combinations.
#'
#' @return A `scores` object with an additional `.imputed`
#'   column. Rows that were imputed have `.imputed = TRUE`.
#'
#' @seealso [impute_worst_score()], [impute_mean_score()],
#'   [impute_na_score()], [impute_model_score()],
#'   \code{vignette("handling-missing-forecasts")}
#' @importFrom data.table copy set rbindlist setattr
#' @importFrom checkmate assert_class assert_character
#'   assert_subset
#' @importFrom cli cli_abort cli_inform
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
  assert_strategy(
    strategy,
    required = c("scores", "missing_rows", "metrics", "compare")
  )

  scores <- copy(scores)

  missing_rows <- build_missing_grid(scores, compare) # nolint: object_usage_linter

  if (nrow(missing_rows) == 0) {
    cli_inform(c(
      i = "No missing scores to impute. Returning scores unchanged."
    ))
    data.table::set(scores, j = ".imputed", value = FALSE)
    return(scores[])
  }

  n_missing <- nrow(missing_rows) # nolint: object_usage_linter
  n_comparators <- length(unique(missing_rows[[compare]])) # nolint: object_usage_linter, line_length_linter
  cli_inform(c(
    i = "Imputing {n_missing} missing score row{?s}.",
    i = "{n_comparators} {compare} {cli::qty(n_comparators)}value{?s} affected." # nolint: line_length_linter
  ))

  filled <- strategy(scores, missing_rows, metrics, compare)

  data.table::set(filled, j = ".imputed", value = TRUE)
  data.table::set(scores, j = ".imputed", value = FALSE)

  out <- rbindlist(
    list(scores, filled),
    use.names = TRUE,
    fill = TRUE
  )

  return(new_scores(out, metrics))
}


# Shared implementation for simple summary-based imputation
# (e.g. max, mean). `fn` is a summary function applied to each
# metric within the same target combination across all compare
# values. NA values are ignored; target combinations with no
# non-NA observations produce NA rather than -Inf or NaN.
impute_summary_score <- function(fn) {
  safe_fn <- function(x) {
    x <- x[!is.na(x)]
    if (length(x) == 0) {
      return(NA_real_)
    }
    fn(x)
  }
  function(scores, missing_rows, metrics, compare) {
    fu <- get_forecast_unit(scores)
    target_cols <- setdiff(fu, compare)

    for (m in metrics) {
      if (!(m %in% names(scores))) next
      agg <- scores[,
        .(..val = safe_fn(get(m))),
        by = target_cols
      ]
      missing_rows <- merge(
        missing_rows,
        agg,
        by = target_cols,
        all.x = TRUE
      )
      data.table::set(
        missing_rows,
        j = m,
        value = missing_rows[["..val"]]
      )
      data.table::set(
        missing_rows,
        j = "..val",
        value = NULL
      )
    }
    return(missing_rows)
  }
}


#' @title Impute with worst (maximum) observed score
#'
#' @description
#' Strategy for [impute_missing_scores()] that fills each
#' missing metric with the worst (maximum) observed value for
#' that metric within the same target combination across all
#' values of the `compare` column. Target combinations with no
#' non-NA observations are filled with `NA_real_`.
#'
#' @return A strategy function for [impute_missing_scores()].
#' @seealso [impute_missing_scores()], [impute_mean_score()]
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
#' impute_missing_scores(scores, strategy = impute_worst_score())
impute_worst_score <- function() {
  impute_summary_score(max)
}


#' @title Impute with mean observed score
#'
#' @description
#' Strategy for [impute_missing_scores()] that fills each
#' missing metric with the mean observed value for that metric
#' within the same target combination across all values of the
#' `compare` column. Target combinations with no non-NA
#' observations are filled with `NA_real_`.
#'
#' @return A strategy function for [impute_missing_scores()].
#' @seealso [impute_missing_scores()], [impute_worst_score()]
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
#' impute_missing_scores(scores, strategy = impute_mean_score())
impute_mean_score <- function() {
  impute_summary_score(mean)
}


#' @title Impute with NA values
#'
#' @description
#' Strategy for [impute_missing_scores()] that fills each
#' missing metric with `NA_real_`.
#'
#' @return A strategy function for [impute_missing_scores()].
#' @seealso [impute_missing_scores()]
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
#' Strategy for [impute_missing_scores()] that fills missing
#' scores with the scores of a specified reference model for
#' the same target combination.
#'
#' @param model Character string naming the reference model
#'   whose scores should be used for imputation. The reference
#'   model must have scores for all target combinations that
#'   need imputing; otherwise an error is raised.
#'
#' @return A strategy function for [impute_missing_scores()].
#' @seealso [impute_missing_scores()]
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
#'
#' impute_missing_scores(
#'   scores,
#'   strategy = impute_model_score("EuroCOVIDhub-baseline")
#' )
impute_model_score <- function(model) {
  assert_character(model, len = 1)
  # Store under a different name to avoid collision with
  # the "model" column in data.table expressions below.
  ref_model_name <- model
  function(scores, missing_rows, metrics, compare) {
    fu <- get_forecast_unit(scores)
    target_cols <- setdiff(fu, compare)

    ref <- scores[
      get(compare) == ref_model_name
    ]

    if (nrow(ref) == 0) {
      cli_abort(c(
        "!" = paste0(
          "Reference model {.val {ref_model_name}} ",
          "not found in scores."
        )
      ))
    }

    needed <- unique(
      missing_rows[, target_cols, with = FALSE]
    )
    available <- unique(
      ref[, target_cols, with = FALSE]
    )
    missing_targets <- needed[!available, on = target_cols]
    if (nrow(missing_targets) > 0) {
      cli_abort(c(
        "!" = paste0(
          "Reference model {.val {ref_model_name}} ",
          "is missing scores for ",
          "{nrow(missing_targets)} target ",
          "combination{?s} that need imputing."
        )
      ))
    }

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
