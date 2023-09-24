#' Convert to yardstick format for class predictions
#'
#' @description
#' A function to convert from the format for binary forecasts that
#' `scoringutils` uses to the one used by the `yardstick` package for class
#' predictions.
#'
#' For class predictions, `yardstick` doesn't use probabilities, but takes the
#' actual outcome (0 or 1) as the prediction. The function therefore converts
#' the predictions into either 0 or 1 and makes both "prediction" and
#' "true_value" a factor.
#'
#' See [example_binary] and
#' \url{https://yardstick.tidymodels.org/articles/metric-types.html} for
#' more information.
#'
#' @param binary_predictions A data frame of binary predictions following the
#' same format used for [score()]. `to_yardstick_binary_class` can also be
#' called on the output of [score()].
#' @param fun A function used to convert predictions into 0s and 1s. The default
#' is [round()].
#' @param ... Additional arguments to be passed to `fun`,
#' @return A data.table that conforms to the formatting requirements of
#' `yardstick`.
#' @export
#' @examples
#'
#'
#' \dontrun{
#' library(yardstick)
#' library(dplyr)
#' ex <- to_yardstick_binary_class(example_binary)
#'
#' ex |>
#'   group_by(model) |>
#'   accuracy(truth = true_value, estimate = prediction)
#'
#' }
#' @keywords data-handling

to_yardstick_binary_class <- function(binary_predictions, fun = round, ...) {
  dt <- as.data.table(binary_predictions)
  dt[, true_value := as.factor(true_value)]
  dt[, prediction := as.factor(
    fun(prediction, ...)
  )]
  return(dt[])
}


#' Convert to yardstick format for class probability predictions
#'
#' @description
#' A function to convert from the format for binary forecasts that
#' `scoringutils` uses to the one used by `yardstick` for class
#' probability predictions.
#'
#' The format `yardstick` uses for (binary) class probability predictions is
#' very similar to the one used by `scoringutils`. The only difference is that
#' `yardstick` expects the outcome to be a factor. The function merely
#' converts "true_value" to a factor.
#'
#' See [example_binary] and
#' \url{https://yardstick.tidymodels.org/articles/metric-types.html} for
#' more details on the formats used.
#'
#' @param binary_predictions A data frame of binary predictions following the
#' same format used for [score()]. `to_yardstick_binary_class` can also be
#' called on the output of [score()].
#' @return A data.table that conforms to the formatting requirements of
#' `yardstick`.
#' @export
#' @examples
#'
#'
#' \dontrun{
#' library(yardstick)
#' library(dplyr)
#' ex <- to_yardstick_binary_class_prob(example_binary)
#'
#' ex |>
#'   group_by(model) |>
#'   filter(!is.na(prediction)) |>
#'   average_precision(truth = true_value, prediction, event_level = "first")
#'
#' }
#' @keywords data-handling

# Binary class probability predictions
to_yardstick_binary_class_prob <- function(binary_predictions) {
  binary_predictions <- as.data.table(binary_predictions)
  binary_predictions[, true_value := factor(true_value, levels = c(1, 0))]
  return(binary_predictions[])
}
