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
#' If `fun = log` it automatically checks internally whether there are any
#' zero values. If so, the function tries to apply `log(x + 1)` instead and
#' gives a warning. You can pass an argument `offset = 1` (or any other value)
#' to the function to avoid the warning.
#' @param append whether or not to append a transformed version of the data to
#' the currently existing data (default is TRUE). If selected, the data gets
#' transformed and appended to the existing data frame, making it possible to
#' use the outcome directly in `score()`. An additional column, 'scale', gets
#' created that denotes which rows or untransformed ('scale' has the value
#' "natural") and which have been transformed ('scale' has the value passed to
#' the argument `label`).
#' @param label A string for the newly created 'scale' column to denote the
#' newly transformed values. Only relevant if `append = TRUE`.
#' @param ... Additional parameters to pass to the function you supplied. If
#' you're using `fun = log`, one parameter added by default is the option to
#' set `offset = 1` (or any other number) to apply the function
#' `log (1 + offset)` instead (useful if you have e.g zero values in the data).
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
#' # add log transformed forecasts (produces a warning as some values are zero)
#' transform_forecasts(example_quantile, truncate = TRUE, append = FALSE)
#'
#' # specifying an offset manually for the log transformation removes the warning
#' transform_forecasts(example_quantile, truncate = TRUE, offset = 1)
#'
#' # adding multiple transformations
#' library(magrittr) # pipe operator
#' example_quantile %>%
#'   # truncate all negative values for both log and sqrt
#'   transform_forecasts(fun = function(x) pmax(0, x), append = FALSE) %>%
#'   transform_forecasts(offset = 1) %>%
#'   transform_forecasts(fun = sqrt, label = "sqrt") %>%
#'   score() %>%
#'   summarise_scores(by = c("model", "scale"))

transform_forecasts <- function(data,
                                fun = log_shift,
                                append = TRUE,
                                label = "log",
                                ...) {

  if (identical(fun, sqrt)) {
    if (any(data$true_value < 0, na.rm = TRUE) |
        any(data$prediction < 0, na.rm = TRUE)) {
      stop("Can't apply sqrt transformation, values < 0 present")
    }
  }

  # check if a column called "scale" is already present and if so, only
  # restrict to transformations of the original data
  if ("scale" %in% colnames(data)) {
    if (!("natural" %in% data$scale)) {
      stop("If a column 'scale' is present, entries with scale =='natural' are needed")
    }
    transformed_data <- data.table::copy(data[data$scale == "natural", ])
  } else {
    transformed_data <- data.table::copy(data)
  }

  transformed_data[, prediction := fun(prediction, ...)]
  transformed_data[, true_value := fun(true_value, ...)]

  if (append) {
    data <- as.data.table(data)

    if (!("scale" %in% colnames(data))) {
      data[, scale := "natural"]
    } else if  (label %in% data$scale) {
      warning(
        paste0(
          "Appending new transformations with label '", label,
          "', even though that entry is already present in column 'scale'."
        )
      )
    }
    transformed_data[, scale := label]
    out <- rbind(data, transformed_data)
    return(out[])
  }
  out <- transformed_data
  return(out[])
}






#' @title Log transformation with an additive shift
#'
#' @description Function that shifts a value by some offset and then applies the
#' natural logarithm to it.
#'
#' @details The output is computed as log(x + offset).
#'
#' @param x vector of input values to be transformed
#' @param offset number to add to the input value before taking the natural
#' logarithm
#' @param truncate whether to truncate negative values to 0. Default is FALSE.
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
#' log_shift(-1:9, offset = 1, truncate = TRUE)
#'
#' transform_forecasts(
#'   example_quantile,
#'   fun = log_shift,
#'   offset = 1,
#'   truncate = TRUE
#'  )

log_shift <- function(
    x,
    offset = 0,
    truncate = FALSE,
    base = exp(1)
) {

  if (truncate) {
    x <- pmax(0, x)
  }

  if (any (x < 0, na.rm = TRUE)) {
    w <- paste(
      "Detected input values < 0.",
      "Try truncating negative values (use truncate = TRUE)"
    )
    warning(w)
  }

  if (any(x == 0, na.rm = TRUE) && offset == 0) {
    w <- paste0(
      "Detected zeros in input values.",
      "Try specifying offset = 1 (or any other offset)."
    )
    warning(w)
  }
  log(x + offset, base = base)
}

