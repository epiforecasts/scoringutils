#' @title Transform forecasts and observed values
#'
#' @description Function to transform forecasts and true values before scoring.
#'
#' @details There are a few reasons, depending on the circumstances, for
#' why this might be desirable (check out the linked reference for more info).
#' In epidemiology, for example, it may be useful to log-transform incidence
#' counts before evaluating forecasts using scores such as the weighted interval
#' score (WIS) or the continuous ranked probability score (CRPS).
#' Log-transforming forecasts and observations changes the interpretation of
#' the score from a measure of absolute distance between forecast and
#' observation to a score that evaluates a forecast of the exponential growth
#' rate. Another motivation can be to apply a variance-stabilising
#' transformation or to standardise incidence counts by population.
#'
#' Note that if you want to apply a transformation, it is important to transform
#' the forecasts and observations and then apply the score. Applying a
#' transformation after the score risks losing propriety of the proper scoring
#' rule.
#'
#' @inheritParams score
#' @param fun A function used to transform both true values and predictions.
#' The default function is [log_shift()], a custom function that is essentially
#' the same as [log()], but has an additional arguments (`offset`)
#' that allows you add an offset before applying the logarithm.
#' @param append whether or not to append a transformed version of the data to
#' the currently existing data (default is TRUE). If selected, the data gets
#' transformed and appended to the existing data frame, making it possible to
#' use the outcome directly in [score()]. An additional column, 'scale', gets
#' created that denotes which rows or untransformed ('scale' has the value
#' "natural") and which have been transformed ('scale' has the value passed to
#' the argument `label`).
#' @param label A string for the newly created 'scale' column to denote the
#' newly transformed values. Only relevant if `append = TRUE`.
#' @param ... Additional parameters to pass to the function you supplied.
#' @return A data.table with either a transformed version of the data, or one
#' with both the untransformed and the transformed data. includes the original data as well as a
#' transformation of the original data. There will be one additional column,
#' 'scale', present which will be set to "natural" for the untransformed
#' forecasts.
#'
#' @importFrom data.table ':=' is.data.table copy
#' @author Nikos Bosse \email{nikosbosse@@gmail.com}
#' @export
#' @references Transformation of forecasts for evaluating predictive
#' performance in an epidemiological context
#' Nikos I. Bosse, Sam Abbott, Anne Cori, Edwin van Leeuwen, Johannes Bracher,
#' Sebastian Funk
#' medRxiv 2023.01.23.23284722
#' \doi{https://doi.org/10.1101/2023.01.23.23284722}
#' <https://www.medrxiv.org/content/10.1101/2023.01.23.23284722v1> # nolint
#' @keywords check-forecasts
#' @examples
#'
#' library(magrittr) # pipe operator
#'
#' # transform forecasts using the natural logarithm
#' # negative values need to be handled (here by replacing them with 0)
#' example_quantile %>%
#'   .[, true_value := ifelse(true_value < 0, 0, true_value)] %>%
#'   transform_forecasts(append = FALSE)
#'
#' # alternatively, integrating the truncation in the transformation function:
#' transform_forecasts(example_quantile,
#'                     fun = function(x) {log_shift(pmax(0, x))},
#'                     append = FALSE)
#'
#' # specifying an offset for the log transformation removes the
#' # warning caused by zeros in the data
#' example_quantile %>%
#'   .[, true_value := ifelse(true_value < 0, 0, true_value)] %>%
#'   transform_forecasts(offset = 1, append = FALSE)
#'
#' # adding square root transformed forecasts to the orginal ones
#' example_quantile %>%
#'   .[, true_value := ifelse(true_value < 0, 0, true_value)] %>%
#'   transform_forecasts(fun = sqrt, label = "sqrt") %>%
#'   score() %>%
#'   summarise_scores(by = c("model", "scale"))
#'
#' # adding multiple transformations
#' example_quantile %>%
#'   .[, true_value := ifelse(true_value < 0, 0, true_value)] %>%
#'   transform_forecasts(fun = log_shift, offset = 1) %>%
#'   transform_forecasts(fun = sqrt, label = "sqrt")

transform_forecasts <- function(data,
                                fun = log_shift,
                                append = TRUE,
                                label = "log",
                                ...) {
  original_data <- as.data.table(data)
  scale_col_present <- ("scale" %in% colnames(original_data))

  # Error handling
  if (scale_col_present) {
    if (!("natural" %in% original_data$scale)) {
      stop(
        "If a column 'scale' is present, entries with scale =='natural' are required for the transformation"
      )
    }
    if (append && (label %in% original_data$scale)) {
      w <- paste0(
        "Appending new transformations with label '",
        label,
        "', even though that entry is already present in column 'scale'."
      )
      warning(w)
    }
  }

  if (append) {
    if (scale_col_present) {
      transformed_data <-
        data.table::copy(original_data)[scale == "natural"]
    } else {
      transformed_data <- data.table::copy(original_data)
      original_data[, scale := "natural"]
    }
    transformed_data[, prediction := fun(prediction, ...)]
    transformed_data[, true_value := fun(true_value, ...)]
    transformed_data[, scale := label]
    out <- rbind(original_data, transformed_data)
    return(out[])
  }

  # check if a column called "scale" is already present and if so, only
  # restrict to transformations of the original data
  if (scale_col_present) {
    original_data[scale == "natural", prediction := fun(prediction, ...)]
    original_data[scale == "natural", true_value := fun(true_value, ...)]
    original_data[scale == "natural", scale := label]
  } else {
    original_data[, prediction := fun(prediction, ...)]
    original_data[, true_value := fun(true_value, ...)]
  }
  return(original_data[])
}






#' @title Log transformation with an additive shift
#'
#' @description Function that shifts a value by some offset and then applies the
#' natural logarithm to it.
#'
#' @details The output is computed as log(x + offset)
#'
#' @param x vector of input values to be transformed
#' @param offset number to add to the input value before taking the natural
#' logarithm
#' @param base a positive or complex number: the base with respect to which
#' logarithms are computed. Defaults to e = exp(1).
#' @return A numeric vector with transformed values
#' @export
#' @references Transformation of forecasts for evaluating predictive
#' performance in an epidemiological context
#' Nikos I. Bosse, Sam Abbott, Anne Cori, Edwin van Leeuwen, Johannes Bracher,
#' Sebastian Funk
#' medRxiv 2023.01.23.23284722
#' \doi{https://doi.org/10.1101/2023.01.23.23284722}
#' <https://www.medrxiv.org/content/10.1101/2023.01.23.23284722v1> # nolint
#' @keywords check-forecasts
#' @examples
#'
#' log_shift(1:10)
#' log_shift(0:9, offset = 1)
#'
#' transform_forecasts(
#'   example_quantile[true_value > 0, ],
#'   fun = log_shift,
#'   offset = 1
#'  )

log_shift <- function(x,
                      offset = 0,
                      base = exp(1)) {

  if (any (x < 0, na.rm = TRUE)) {
    w <- paste("Detected input values < 0.")
    stop(w)
  }

  if (any(x == 0, na.rm = TRUE) && offset == 0) {
    w <- paste0("Detected zeros in input values.",
                "Try specifying offset = 1 (or any other offset).")
    warning(w)
  }
  log(x + offset, base = base)
}
