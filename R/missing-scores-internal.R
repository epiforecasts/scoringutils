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
