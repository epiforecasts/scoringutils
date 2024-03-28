#' @title Print information about a forecast object
#' @description
#' This function prints information about a forecast object,
#' including "Forecast type", "Score columns",
#' "Forecast unit".
#'
#' @param x A forecast object (a validated data.table with predicted and
#'   observed values, see [as_forecast()]).
#' @param ... Additional arguments for [print()].
#' @return Returns `x` invisibly.
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
  forecast_unit <- try(
    do.call(get_forecast_unit, list(data = x)),
    silent = TRUE
  )

  # Print forecast object information
  if (inherits(forecast_type, "try-error")) {
    cli_inform(
      c(
        "!" = "Could not determine forecast type due to error in validation." #nolint
      )
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

  if (inherits(forecast_unit, "try-error")) {
    cli_inform(
      c(
        "!" = "Could not determine forecast unit." #nolint
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
