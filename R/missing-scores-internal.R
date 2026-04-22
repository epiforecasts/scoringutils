#' Assert that a strategy has the expected signature
#'
#' @description
#' Internal helper used by [filter_scores()] and
#' [impute_missing_scores()] to check that a user-supplied
#' strategy function has at least the required named formals.
#' This catches common mistakes early (e.g. forgetting the
#' `compare` argument) without constraining the strategy
#' author to a specific internal type.
#'
#' @param strategy A function.
#' @param required Character vector of formal names that
#'   `strategy` must accept.
#'
#' @return `invisible(NULL)`. Called for its side effect of
#'   erroring when the check fails.
#'
#' @importFrom checkmate assert_function
#' @importFrom cli cli_abort
#' @keywords internal
assert_strategy <- function(strategy, required) {
  assert_function(strategy)
  strategy_formals <- names(formals(strategy))
  missing_args <- setdiff(required, strategy_formals)
  if (length(missing_args) > 0) {
    cli_abort(c(
      "!" = paste0(
        "Strategy function is missing required ",
        "argument{?s}: {.arg {missing_args}}."
      ),
      i = paste0(
        "Expected formals including: ",
        "{.arg {required}}."
      )
    ))
  }
  return(invisible(NULL))
}


#' Build grid of missing model-target combinations
#'
#' @description
#' Internal function that detects missing model x target
#' combinations by comparing observed data against the complete
#' grid of all compare-column values crossed with all observed
#' target combinations.
#'
#' @param scores A `scores` object (data.table with a `metrics`
#'   attribute as produced by [score()]).
#' @param compare Character string (default `"model"`) naming the
#'   column whose values are compared against each target.
#'
#' @return A `data.table` with forecast-unit columns for each
#'   missing combination. Zero rows if nothing is missing.
#'
#' @importFrom data.table as.data.table setkeyv rbindlist copy
#' @keywords internal
build_missing_grid <- function(scores, compare = "model") {
  scores <- data.table::as.data.table(scores)
  forecast_unit <- get_forecast_unit(scores)
  target_cols <- setdiff(forecast_unit, compare)

  targets <- unique(scores[, target_cols, with = FALSE])

  # All unique compare values
  compare_vals <- unique(scores[[compare]])

  # Complete grid: every compare value x every observed target
  complete <- data.table::rbindlist(lapply(
    compare_vals,
    function(val) {
      out <- data.table::copy(targets)
      out[, (compare) := val]
      out
    }
  ))

  # Observed combinations (forecast unit cols only)
  observed <- unique(scores[, forecast_unit, with = FALSE])

  # Anti-join: rows in complete but not in observed
  data.table::setkeyv(complete, forecast_unit)
  data.table::setkeyv(observed, forecast_unit)
  missing <- complete[!observed]

  return(missing)
}
