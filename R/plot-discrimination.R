#' @title Plot discrimination for binary forecasts
#'
#' @description
#' Visualise the discrimination ability of binary forecasts by plotting the
#' distribution of predicted probabilities, stratified by the observed outcome.
#' A well-discriminating model will show clearly separated distributions for
#' the two observed levels.
#'
#' @param forecast A data.table (or data.frame) containing at least columns
#'   `observed` (factor with two levels) and `predicted` (numeric probabilities
#'   between 0 and 1). Typically a `forecast_binary` object or the output of
#'   [as_forecast_binary()].
#' @returns A ggplot object showing overlapping density curves of predicted
#'   probabilities, coloured by observed outcome level.
#' @importFrom ggplot2 ggplot aes geom_density labs .data
#' @importFrom checkmate assert assert_data_frame
#' @export
#' @examples
#' library(ggplot2)
#' plot_discrimination(na.omit(example_binary))
#'
#' plot_discrimination(na.omit(example_binary)) +
#'   facet_wrap(~model)

plot_discrimination <- function(forecast) {
  forecast <- ensure_data.table(forecast)
  assert(check_columns_present(forecast, c("observed", "predicted")))

  plot <- ggplot(
    forecast,
    aes(x = .data[["predicted"]], fill = .data[["observed"]])
  ) +
    geom_density(alpha = 0.5) +
    labs(
      x = "Predicted probability",
      y = "Density",
      fill = "Observed"
    ) +
    theme_scoringutils()

  return(plot)
}
