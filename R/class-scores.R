#' Construct an object of class `scores`
#' @description
#' This function creates an object of class `scores` based on a
#' data.table or similar.
#' @param scores A data.table or similar with scores as produced by [score()].
#' @param metrics A character vector with the names of the scores
#'   (i.e. the names of the scoring rules used for scoring).
#' @param ... Additional arguments to [data.table::as.data.table()]
#' @keywords internal
#' @importFrom data.table as.data.table setattr
#' @returns An object of class `scores`
#' @examples
#' \dontrun{
#' df <- data.frame(
#'   model = "A",
#'   wis = "0.1"
#' )
#' new_scores(df, "wis")
#' }
new_scores <- function(scores, metrics, ...) {
  scores <- as.data.table(scores, ...)
  class(scores) <- c("scores", class(scores))
  setattr(scores, "metrics", metrics)
  return(scores[])
}


#' Create an object of class `scores` from data
#' @description This convenience function wraps [new_scores()] and validates
#'   the `scores` object.
#' @inherit new_scores params return
#' @importFrom checkmate assert_data_frame
#' @keywords internal
as_scores <- function(scores, metrics) {
  assert_data_frame(scores)
  present_metrics <- metrics[metrics %in% colnames(scores)]
  scores <- new_scores(scores, present_metrics)
  assert_scores(scores)
  return(scores[])
}


#' Validate an object of class `scores`
#' @description
#' This function validates an object of class `scores`, checking
#' that it has the correct class and that it has a `metrics` attribute.
#' @inheritParams new_scores
#' @returns Returns `NULL` invisibly
#' @importFrom checkmate assert_class assert_data_frame
#' @keywords internal
assert_scores <- function(scores) {
  assert_data_frame(scores)
  assert_class(scores, "scores")
  # error if no metrics exists +
  # throw warning if any of the metrics is not in the data
  get_metrics.scores(scores, error = TRUE)
  return(invisible(NULL))
}

#' @method `[` scores
#' @importFrom data.table setattr
#' @export
`[.scores` <- function(x, ...) {
  ret <- NextMethod()
  if (is.data.table(ret)) {
    setattr(ret, "metrics", attr(x, "metrics"))
  } else if (is.data.frame(ret)) {
    attr(ret, "metrics") <- attr(x, "metrics")
  }
  return(ret)
}


#' @title Get names of the metrics that were used for scoring
#' @description
#' When applying a scoring rule via [score()], the names of the scoring rules
#' become column names of the
#' resulting data.table. In addition, an attribute `metrics` will be
#' added to the output, holding the names of the scores as a vector.
#'
#' This is done so that functions like [get_forecast_unit()] or
#' [summarise_scores()] can still identify which columns are part of the
#' forecast unit and which hold a score.
#'
#' `get_metrics()` accesses and returns the `metrics` attribute. If there is no
#' attribute, the function will return `NULL` (or, if `error = TRUE` will
#' produce an error instead). In addition, it checks the column names of the
#' input for consistency with the data stored in the `metrics` attribute.
#'
#' **Handling a missing or inconsistent `metrics` attribute**:
#'
#' If the metrics attribute is missing or is not consistent with the column
#' names of the data.table, you can either
#'
#' - run [score()] again, specifying names for the scoring rules manually, or
#' - add/update the attribute manually using
#' `attr(scores, "metrics") <- c("names", "of", "your", "scores")` (the
#' order does not matter).
#'
#' @param x A `scores` object, (a data.table with an attribute `metrics` as
#'   produced by [score()]).
#' @param error Throw an error if there is no attribute called `metrics`?
#'   Default is FALSE.
#' @param ... unused
#' @importFrom cli cli_abort cli_warn
#' @importFrom checkmate assert_data_frame
#' @return
#' Character vector with the names of the scoring rules that were used
#' for scoring.
#' @keywords handle-metrics
#' @family get_metrics functions
#' @export
get_metrics.scores <- function(x, error = FALSE, ...) {
  assert_data_frame(x)
  metrics <- attr(x, "metrics")
  if (error && is.null(metrics)) {
    #nolint start: keyword_quote_linter
    cli_abort(
      c(
        "!" = "Input needs an attribute `metrics` with the names of the
         scoring rules that were used for scoring.",
        "i" = "See `?get_metrics` for further information."
      )
    )
    #nolint end
  }

  if (!all(metrics %in% names(x))) {
    #nolint start: keyword_quote_linter object_usage_linter
    missing <- setdiff(metrics, names(x))
    cli_warn(
      c(
        "!" = "The following scores have been previously computed, but are no
            longer column names of the data: {.val {missing}}",
        "i" = "See {.code ?get_metrics} for further information."
      )
    )
    #nolint end
  }

  return(metrics)
}
