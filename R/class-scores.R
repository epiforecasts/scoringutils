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
#' @return An object of class `scores`
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
