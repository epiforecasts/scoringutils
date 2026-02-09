#' @title Validate metrics
#'
#' @description
#' This function validates whether the list of metrics is a list
#' of valid functions.
#'
#' The function is used in [score()] to make sure that all metrics are valid
#' functions.
#'
#' @param metrics A named list with metrics. Every element should be a scoring
#'   function to be applied to the data.
#' @importFrom cli cli_warn
#'
#' @return
#' A named list of metrics, with those filtered out that are not
#' valid functions
#' @importFrom checkmate assert_list test_list check_function
#' @keywords internal_input_check
validate_metrics <- function(metrics) {

  assert_list(metrics, min.len = 1, names = "named")

  for (i in seq_along(metrics)) {
    check_fun <- check_function(metrics[[i]])
    if (!isTRUE(check_fun)) {
      #nolint start: keyword_quote_linter
      cli_warn(
        c(
          "!" = "`Metrics` element number {i} is not a valid function."
        )
      )
      #nolint end
      names(metrics)[i] <- "scoringutils_delete"
    }
  }
  metrics[names(metrics) == "scoringutils_delete"] <- NULL

  assert_list(metrics, min.len = 1, .var.name = "valid metrics")

  return(metrics)
}
