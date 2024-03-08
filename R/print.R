#' @title Print Information About A Forecast Object
#' @description This function prints information about a forecast object,
#' including "Forecast type", "Score columns",
#' "Forecast unit".
#'
#' @param x An object of class 'forecast_*' object as produced by
#' `as_forecast()`
#' @param ... additional arguments for [print()]
#' @return returns x invisibly
#' @importFrom cli cli_inform cli_warn col_blue cli_text
#' @export
#' @keywords check-forecasts
#' @examples
#' dat <- as_forecast(example_quantile)
#' print(dat)
print.forecast_binary <- function(x, ...) {

  # check whether object passes validation
  validation <- try(do.call(validate_forecast, list(data = x)), silent = TRUE)
  if (inherits(validation, "try-error")) {
    cli_warn(
      c(
        "!" = "Error in validating forecast object: {validation}."
      )
    )
  }

  # get forecast type, forecast unit and score columns
  forecast_type <- try(
    do.call(get_forecast_type, list(data = x)),
    silent = TRUE
  )
  forecast_unit <- get_forecast_unit(x)

  # Print forecast object information
  if (inherits(forecast_type, "try-error")) {
    cli_inform(
      "Could not determine forecast type due to error in validation."
    )
  } else {
    cli_text(
      col_blue(
        "Forecast type:"
      )
    )
    cli_text(
      "{forecast_type}"
    )
  }

  if (length(forecast_unit) == 0) {
    cli_inform(
      c(
        "!" = "Could not determine forecast unit."
      )
    )
  } else {
    cli_text(
      col_blue(
        "Forecast unit:"
      )
    )
    cli_text(
      "{forecast_unit}"
    )
  }

  cat("\n")
  NextMethod(x, ...)

  return(invisible(x))
}

#' @rdname print.forecast_binary
#' @export
print.forecast_quantile <- print.forecast_binary

#' @rdname print.forecast_binary
#' @export
print.forecast_point <- print.forecast_binary

#' @rdname print.forecast_binary
#' @export
print.forecast_sample <- print.forecast_binary
