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
#'
#' @param fun A function used to transform both true values and predictions.
#' The default function is [log_shift()], a custom function that is essentially
#' the same as [log()], but has an additional arguments (`offset`)
#' that allows you add an offset before applying the logarithm. This is often
#'  helpful as the natural log transformation is not defined at zero. A common,
#'  and pragmatic solution, is to add a small offset to the data before applying
#'  the log transformation. In our work we have often used an offset of 1 but
#' the precise value will depend on your application.
#'
#' @param append Logical, defaults to `TRUE`. Whether or not to append a
#' transformed version of the data to the currently existing data (`TRUE`). If
#' selected, the data gets transformed and appended to the existing data frame,
#' making it possible to use the outcome directly in [score()]. An additional
#' column, 'scale', gets created that denotes which rows or untransformed
#' ('scale' has the value "natural") and which have been transformed ('scale'
#' has the value passed to the argument `label`).
#'
#' @param label A string for the newly created 'scale' column to denote the
#' newly transformed values. Only relevant if `append = TRUE`.
#'
#' @param ... Additional parameters to pass to the function you supplied. For
#' the default option of [log_shift()] this could be the `offset` argument.
#'
#' @return A `data.table` with either a transformed version of the data, or one
#' with both the untransformed and the transformed data. includes the original
#' data as well as a transformation of the original data. There will be one
#' additional column, `scale', present which will be set to "natural" for the
#' untransformed forecasts.
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
#' <https://www.medrxiv.org/content/10.1101/2023.01.23.23284722v1>
#' @keywords check-forecasts
#' @examples
#'
#' library(magrittr) # pipe operator
#'
#' # transform forecasts using the natural logarithm
#' # negative values need to be handled (here by replacing them with 0)
#' example_quantile %>%
#'   .[, true_value := ifelse(true_value < 0, 0, true_value)] %>%
#' # Here we use the default function log_shift() which is essentially the same
#' # as log(), but has an additional arguments (offset) that allows you add an
#' # offset before applying the logarithm.
#'   transform_forecasts(append = FALSE) %>%
#'   head()
#'
#' # alternatively, integrating the truncation in the transformation function:
#' example_quantile %>%
#'  transform_forecasts(
#'    fun = function(x) {log_shift(pmax(0, x))}, append = FALSE
#'  ) %>%
#'  head()
#'
#' # specifying an offset for the log transformation removes the
#' # warning caused by zeros in the data
#' example_quantile %>%
#'   .[, true_value := ifelse(true_value < 0, 0, true_value)] %>%
#'   transform_forecasts(offset = 1, append = FALSE) %>%
#'   head()
#'
#' # adding square root transformed forecasts to the original ones
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
#'   transform_forecasts(fun = sqrt, label = "sqrt") %>%
#'   head()

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
#' log_shift(1:10)
#' log_shift(0:9, offset = 1)
#'
#' transform_forecasts(
#'   example_quantile[true_value > 0, ],
#'   fun = log_shift,
#'   offset = 1
#'  )

log_shift <- function(x, offset = 0, base = exp(1)) {

  if (any(x < 0, na.rm = TRUE)) {
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


#' @title Set unit of a single forecast manually
#'
#' @description Helper function to set the unit of a single forecast (i.e. the
#' combination of columns that uniquely define a single forecast) manually.
#' This simple function keeps the columns specified in `forecast_unit` (plus
#' additional protected columns, e.g. for true values, predictions or quantile
#' levels) and removes duplicate rows.
#' If not done manually, `scoringutils` attempts to determine the unit
#' of a single forecast automatically by simply assuming that all column names
#' are relevant to determine the forecast unit. This may lead to unexpected
#' behaviour, so setting the forecast unit explicitly can help make the code
#' easier to debug and easier to read. When used as part of a workflow,
#' `set_forecast_unit()` can be directly piped into `check_forecasts()` to
#' check everything is in order.
#'
#' @inheritParams score
#' @param forecast_unit Character vector with the names of the columns that
#' uniquely identify a single forecast.
#' @return A data.table with only those columns kept that are relevant to
#' scoring or denote the unit of a single forecast as specified by the user.
#'
#' @importFrom data.table ':=' is.data.table copy
#' @export
#' @keywords data-handling
#' @examples
#' set_forecast_unit(
#'   example_quantile,
#'   c("location", "target_end_date", "target_type", "horizon", "model")
#' )

set_forecast_unit <- function(data, forecast_unit) {
  data <- as.data.table(data)
  datacols <- colnames(data)
  missing <- forecast_unit[!(forecast_unit %in% datacols)]

  if (length(missing) > 0) {
    warning(
      "Column(s) '",
      missing,
      "' are not columns of the data and will be ignored."
    )
    forecast_unit <- intersect(forecast_unit, datacols)
  }

  keep_cols <- c(get_protected_columns(data), forecast_unit)
  out <- unique(data[, .SD, .SDcols = keep_cols])[]
  return(out)
}
