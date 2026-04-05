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
#' @param type Character, either `"histogram"` (default) or `"density"`.
#'   `"histogram"` shows a histogram with proportions on the y-axis;
#'   `"density"` shows kernel density curves.
#' @returns A ggplot object showing the distribution of predicted
#'   probabilities, coloured by observed outcome level.
#' @importFrom ggplot2 ggplot aes geom_density geom_histogram
#'   after_stat labs .data
#' @importFrom checkmate assert assert_data_frame assert_choice
#' @export
#' @examples
#' library(ggplot2)
#' plot_discrimination(na.omit(example_binary))
#'
#' plot_discrimination(na.omit(example_binary), type = "density")
#'
#' plot_discrimination(na.omit(example_binary)) +
#'   facet_wrap(~model)

plot_discrimination <- function(forecast, type = c("histogram", "density")) {
  forecast <- ensure_data.table(forecast)
  assert(check_columns_present(forecast, c("observed", "predicted")))
  type <- match.arg(type)

  plot <- ggplot(
    forecast,
    aes(x = .data[["predicted"]], fill = .data[["observed"]])
  )

  if (type == "density") {
    plot <- plot +
      geom_density(alpha = 0.5) +
      labs(y = "Density")
  } else {
    plot <- plot +
      geom_histogram(
        aes(y = after_stat(density)),
        position = "identity", alpha = 0.5
      ) +
      labs(y = "Density")
  }

  plot <- plot +
    labs(
      x = "Predicted probability",
      fill = "Observed"
    ) +
    theme_scoringutils()

  return(plot)
}
