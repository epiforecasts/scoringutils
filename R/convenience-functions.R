#' @title Add transformations
#'
#' @description Add transformations of the forecasts and observations for
#' later scoring. For more information on why this might be desirable, check
#' out the linked reference.
#'
#' @inheritParams score
#' @param fun A function used to transform both true values and predictions
#' @param label A string for the newly created 'scale' column to denote the
#' newly transformed values.
#' @return A data.table that includes the original data as well as a
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
#' add_transformation(example_quantile)

transform_forecasts <- function(data,
                                transform = c("log", "sqrt"),
                                fun = NULL,
                                add = TRUE,
                                label = transform,
                                ...) {

  if (is.null(fun)) {
    if (transform == "log") {
      if (any(data$true_value < 0 | any(data$prediction < 0))) {
        stop("Can't apply log transformation, values < 0 present")
      } else if (any(data$true_value == 0 | any(data$prediction == 0))) {
        fun <- function(x) {log(x + 1)}
        message("Detected zeros in the data, using log(x + 1) as transformation instead.")
      } else {
        fun <- log
      }
    }

    if (transform == "sqrt") {
      if (any(data$true_value < 0 | any(data$prediction < 0))) {
        stop("Can't apply sqrt transformation, values < 0 present")
      } else {
        fun <- sqrt
      }
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

  if (add) {
    data <- as.data.table(data)
    data[, scale := "natural"]
    transformed_data[, scale := label]
    out <- rbind(data, transformed_data)
    return(out)
  }



  return(out)
}
