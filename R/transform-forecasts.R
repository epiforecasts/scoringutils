#' @title Transform forecasts and observed values
#'
#' @description
#' Function to transform forecasts and observed values before scoring.
#'
#' @details
#' There are a few reasons, depending on the circumstances, for
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
#' @param fun A function used to transform both observed values and predictions.
#'   The default function is [log_shift()], a custom function that is
#'   essentially the same as [log()], but has an additional arguments (`offset`)
#'   that allows you add an offset before applying the logarithm. This is often
#'   helpful as the natural log transformation is not defined at zero. A
#'   common, and pragmatic solution, is to add a small offset to the data
#'   before applying the log transformation. In our work we have often used an
#'   offset of 1 but the precise value will depend on your application.
#'
#' @param append Logical, defaults to `TRUE`. Whether or not to append a
#'   transformed version of the data to the currently existing data (`TRUE`). If
#'   selected, the data gets transformed and appended to the existing data,
#'   making it possible to use the outcome directly in [score()]. An additional
#'   column, 'scale', gets created that denotes which rows or untransformed
#'   ('scale' has the value "natural") and which have been transformed ('scale'
#'   has the value passed to the argument `label`).
#'
#' @param label A string for the newly created 'scale' column to denote the
#'   newly transformed values. Only relevant if `append = TRUE`.
#'
#' @param ... Additional parameters to pass to the function you supplied. For
#'   the default option of [log_shift()] this could be the `offset` argument.
#'
#' @returns A forecast object with either a transformed version of the data, or
#'   one with both the untransformed and the transformed data. includes the
#'   original data as well as a transformation of the original data. There will
#'   be one additional column, `scale', present which will be set to "natural"
#'   for the untransformed forecasts.
#'
#' @importFrom data.table ':=' is.data.table copy
#' @importFrom cli cli_abort cli_warn
#' @author Nikos Bosse \email{nikosbosse@@gmail.com}
#' @export
#' @references Transformation of forecasts for evaluating predictive
#' performance in an epidemiological context
#' Nikos I. Bosse, Sam Abbott, Anne Cori, Edwin van Leeuwen, Johannes Bracher,
#' Sebastian Funk
#' medRxiv 2023.01.23.23284722
#' \doi{https://doi.org/10.1101/2023.01.23.23284722}
#' <https://www.medrxiv.org/content/10.1101/2023.01.23.23284722v1>
#' @keywords transform
#' @examples
#' library(magrittr) # pipe operator
#'
#' # transform forecasts using the natural logarithm
#' # negative values need to be handled (here by replacing them with 0)
#' example_quantile %>%
#'   .[, observed := ifelse(observed < 0, 0, observed)] %>%
#'   as_forecast_quantile() %>%
#' # Here we use the default function log_shift() which is essentially the same
#' # as log(), but has an additional arguments (offset) that allows you add an
#' # offset before applying the logarithm.
#'   transform_forecasts(append = FALSE) %>%
#'   head()
#'
#' # alternatively, integrating the truncation in the transformation function:
#' example_quantile %>%
#'   as_forecast_quantile() %>%
#'  transform_forecasts(
#'    fun = function(x) {log_shift(pmax(0, x))}, append = FALSE
#'  ) %>%
#'  head()
#'
#' # specifying an offset for the log transformation removes the
#' # warning caused by zeros in the data
#' example_quantile %>%
#'   as_forecast_quantile() %>%
#'   .[, observed := ifelse(observed < 0, 0, observed)] %>%
#'   transform_forecasts(offset = 1, append = FALSE) %>%
#'   head()
#'
#' # adding square root transformed forecasts to the original ones
#' example_quantile %>%
#'   .[, observed := ifelse(observed < 0, 0, observed)] %>%
#'   as_forecast_quantile() %>%
#'   transform_forecasts(fun = sqrt, label = "sqrt") %>%
#'   score() %>%
#'   summarise_scores(by = c("model", "scale"))
#'
#' # adding multiple transformations
#' example_quantile %>%
#'   as_forecast_quantile() %>%
#'   .[, observed := ifelse(observed < 0, 0, observed)] %>%
#'   transform_forecasts(fun = log_shift, offset = 1) %>%
#'   transform_forecasts(fun = sqrt, label = "sqrt") %>%
#'   head()

transform_forecasts <- function(forecast,
                                fun = log_shift,
                                append = TRUE,
                                label = "log",
                                ...) {
  original_forecast <- clean_forecast(forecast, copy = TRUE)
  assert_function(fun)
  assert_logical(append, len = 1)
  assert_character(label, len = 1)

  # store forecast type to construct a valid forecast object later
  forecast_type <- get_forecast_type(original_forecast)

  scale_col_present <- ("scale" %in% colnames(original_forecast))

  # Error handling
  if (scale_col_present) {
    if (!("natural" %in% original_forecast$scale)) {
      #nolint start: keyword_quote_linter
      cli_abort(
        c(
          `!` = "If a column 'scale' is present, entries with scale =='natural'
          are required for the transformation."
        )
      )
    }
    if (append && (label %in% original_forecast$scale)) {
      cli_warn(
        c(
          "i" = "Appending new transformations with label '{label}'
          even though that entry is already present in column 'scale'."
        )
      )
      #nolint end
    }
  }

  if (append) {
    if (scale_col_present) {
      transformed_forecast <- copy(original_forecast)[scale == "natural"]
    } else {
      transformed_forecast <- copy(original_forecast)
      original_forecast[, scale := "natural"]
    }
    transformed_forecast[, predicted := fun(predicted, ...)]
    transformed_forecast[, observed := fun(observed, ...)]
    transformed_forecast[, scale := label]
    out <- rbind(original_forecast, transformed_forecast)

    # construct a new valid forecast object after binding rows together
    fn_name <- paste0("as_forecast_", forecast_type)
    fn <- get(fn_name)
    out <- suppressWarnings(suppressMessages(do.call(fn, list(out))))

    return(out[])
  }

  # check if a column called "scale" is already present and if so, only
  # restrict to transformations of the original forecast
  if (scale_col_present) {
    original_forecast[scale == "natural", predicted := fun(predicted, ...)]
    original_forecast[scale == "natural", observed := fun(observed, ...)]
    original_forecast[scale == "natural", scale := label]
  } else {
    original_forecast[, predicted := fun(predicted, ...)]
    original_forecast[, observed := fun(observed, ...)]
  }
  return(original_forecast[])
}


#' @title Log transformation with an additive shift
#'
#' @description
#' Function that shifts a value by some offset and then applies the
#' natural logarithm to it.
#'
#' @details The output is computed as log(x + offset)
#'
#' @param x vector of input values to be transformed
#' @param offset Number to add to the input value before taking the natural
#'   logarithm.
#' @param base A positive number: the base with respect to which
#'   logarithms are computed. Defaults to e = exp(1).
#' @importFrom cli cli_abort cli_warn
#' @returns A numeric vector with transformed values
#' @export
#' @references Transformation of forecasts for evaluating predictive
#'   performance in an epidemiological context
#'   Nikos I. Bosse, Sam Abbott, Anne Cori, Edwin van Leeuwen, Johannes Bracher,
#'   Sebastian Funk
#'   medRxiv 2023.01.23.23284722
#'   \doi{https://doi.org/10.1101/2023.01.23.23284722}
#'   <https://www.medrxiv.org/content/10.1101/2023.01.23.23284722v1> # nolint
#' @keywords transform
#' @importFrom checkmate assert_numeric assert_number
#' @examples
#' library(magrittr) # pipe operator
#' log_shift(1:10)
#' log_shift(0:9, offset = 1)
#'
#' example_quantile[observed > 0, ] %>%
#'   as_forecast_quantile() %>%
#'   transform_forecasts(fun = log_shift, offset = 1)

log_shift <- function(x, offset = 0, base = exp(1)) {

  assert_numeric(x, min.len = 1)
  assert_number(offset)
  assert_number(base, lower = 0)

  if (any(x < 0, na.rm = TRUE)) {
    #nolint start: keyword_quote_linter
    cli_abort(
      c(
        "!" = "Detected input values < 0."
      )
    )
  }
  if (any(x == 0, na.rm = TRUE) && offset == 0) {
    cli_warn(
      c(
        "!" = "Detected zeros in input values.",
        "i" = "Try specifying offset = 1 (or any other offset)."
      )
    )
    #nolint end
  }
  log(x + offset, base = base)
}
