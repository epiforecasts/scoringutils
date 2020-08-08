#' #' @title Plot Scores
#' #'
#' #' @description
#' #' Make a simple histogram of the probability integral transformed values to
#' #' visually check whether a uniform distribution seems likely.
#' #'
#' #' @param scores Unsummarised scores as produced by \code{\link{eval_forecasts}}
#' #' @param by character vector to do grouping by
#' #' @param facet_formula formula for facetting in ggplot
#' #' @return list of plots
#' #' @importFrom ggplot2 ggplot
#' #' @examples
#' #' scores <- scoringutils::eval_forecasts(scoringutils::quantile_example_data_long,
#' #'                                        summarised = FALSE)
#' #'
#'
#' # question for by: do we want NULL or by = "model" as default?
#' #
#' # plot_scores <- function(scores, by = NULL,
#' #                         facet_formula = ~ range,
#' #                         plot_group = NULL) {
#' #
#' #   data.table::setDT(scores)
#' #
#' #   # create model column if it doesn't exist
#' #   if(!("model" %in% colnames(scores))) {
#' #     scores[, model = "Model"]
#' #   }
#' #
#' #   if (is.null(by)) {
#' #     by <- setdiff(colnames(scores),
#' #                   c("Interval_Score", "calibration", "sharpness", "bias",
#' #                     "true_values", "id", "lower", "upper"))
#' #   }
#' #
#' #   if (!("model" %in% by)) {
#' #     by <- c("model", by)
#' #   }
#' #
#' #   # calculate all quantities needed for plotting
#' #   df <- scores[, .(mean_score = mean(Interval_Score),
#' #                  lower25_score = quantile(Interval_Score, 0.25, na.rm = TRUE),
#' #                  upper75_score = quantile(Interval_Score, 0.75, na.rm = TRUE),
#' #                  lower05_score = quantile(Interval_Score, 0.05, na.rm = TRUE),
#' #                  upper95_score = quantile(Interval_Score, 0.95, na.rm = TRUE),
#' #                  mean_calibration = mean(calibration),
#' #                  lower25_calibration = quantile(calibration, 0.25, na.rm = TRUE),
#' #                  upper75_calibration = quantile(calibration, 0.75, na.rm = TRUE),
#' #                  lower05_calibration = quantile(calibration, 0.05, na.rm = TRUE),
#' #                  upper95_calibration = quantile(calibration, 0.95, na.rm = TRUE),
#' #                  mean_bias = mean(bias),
#' #                  lower25_bias = quantile(bias, 0.25, na.rm = TRUE),
#' #                  upper75_bias = quantile(bias, 0.75, na.rm = TRUE),
#' #                  lower05_bias = quantile(bias, 0.05, na.rm = TRUE),
#' #                  upper95_bias = quantile(bias, 0.95, na.rm = TRUE),
#' #                  mean_sharpness = mean(sharpness),
#' #                  lower25_sharpness = quantile(sharpness, 0.25, na.rm = TRUE),
#' #                  upper75_sharpness = quantile(sharpness, 0.75, na.rm = TRUE),
#' #                  lower05_sharpness = quantile(sharpness, 0.05, na.rm = TRUE),
#' #                  upper95_sharpness = quantile(sharpness, 0.95, na.rm = TRUE)), by = by]
#' #
#' #
#' #   # need to test and think of something better
#' #   if (is.null(facet_formula)) {
#' #     lhs <- setdiff(by, c("range", "model"))
#' #     facet_formula <- as.formula(paste(lhs, "~ range", collapse = " "))
#' #   }
#' #
#' #
#' #   plots <- list()
#' #
#' #   colours <- setdiff(by, c("range", "model"))
#' #
#' #   plots[["interval_score_plot"]]
#' #     ggplot2::ggplot(df, ggplot2::aes(x = model)) +
#' #     ggplot2::geom_linerange(ggplot2::aes(ymin = lower25_score, ymax = upper75_score),
#' #                             size = 2,
#' #                             position = ggplot2::position_dodge2(width = 0.4,
#' #                                                                padding = 0)) +
#' #     ggplot2::geom_linerange(ggplot2::aes(ymin = lower05_score, ymax = upper95_score),
#' #                             size = 2,
#' #                             alpha = 0.4,
#' #                             position = ggplot2::position_dodge2(width = 0.4,
#' #                                                                padding = 0)) +
#' #     ggplot2::facet_grid(~ range, scales = "free",
#' #                         labeller = "label_both") +
#' #
#' #     #cowplot::theme_cowplot() +
#' #     # ggplot2::coord_flip() +
#' #     ggplot2::theme(text = ggplot2::element_text(family = "Sans Serif"),
#' #                    legend.position = "bottom")
#' #
#' #   plots[["calibration_plot"]] <-
#' #     ggplot2::ggplot(scores, ggplot2::aes(x = model, colour = model)) +
#' #     ggplot2::geom_hline(ggplot2::aes(yintercept = range/100), colour = "gray") +
#' #     ggplot2::geom_linerange(ggplot2::aes(ymin = lower25_calibration, ymax = upper75_calibration)) +
#' #     ggplot2::geom_linerange(ggplot2::aes(ymin = lower05_calibration, ymax = upper95_calibration),
#' #                             alpha = 0.1) +
#' #     ggplot2::geom_point(ggplot2::aes(y = mean_calibration)) +
#' #     ggplot2::facet_wrap(facet_formula, scales = "free_y",
#' #                         labeller = "label_both",
#' #                         ncol = 1) +
#' #     #cowplot::theme_cowplot() +
#' #     ggplot2::theme(text = ggplot2::element_text(family = "Sans Serif"),
#' #                    legend.position = "bottom")
#' #
#' #   # change facet_formula here to remove range
#' #   # formula_string <- gsub("range", "", facet_formula)
#' #   # facet_formula_without <- as.formula(paste(formula_string, collapse = " "))
#' #
#' #   plots[["bias_plot"]] <-
#' #     ggplot2::ggplot(scores, ggplot2::aes(x = model, colour = model)) +
#' #     ggplot2::geom_hline(ggplot2::aes(yintercept = 0.5), colour = "gray") +
#' #     ggplot2::geom_linerange(ggplot2::aes(ymin = lower25_bias, ymax = upper75_bias)) +
#' #     ggplot2::geom_linerange(ggplot2::aes(ymin = lower05_bias, ymax = upper95_bias),
#' #                             alpha = 0.1) +
#' #     ggplot2::geom_point(ggplot2::aes(y = mean_bias)) +
#' #     # ggplot2::facet_grid(facet_formula_without, scales = "free_y", labeller = "label_both") +
#' #     #cowplot::theme_cowplot() +
#' #     ggplot2::theme(text = ggplot2::element_text(family = "Sans Serif"),
#' #                    legend.position = "bottom")
#' #
#' #   plots[["sharpness_plot"]] <-
#' #     ggplot2::ggplot(scores, ggplot2::aes(x = model, colour = model)) +
#' #     ggplot2::geom_linerange(ggplot2::aes(ymin = lower25_sharpness, ymax = upper75_sharpness)) +
#' #     ggplot2::geom_linerange(ggplot2::aes(ymin = lower05_sharpness, ymax = upper95_sharpness),
#' #                             alpha = 0.1) +
#' #     ggplot2::geom_point(ggplot2::aes(y = mean_sharpness)) +
#' #     # ggplot2::facet_grid(facet_formula_without, scales = "free_y", labeller = "label_both") +
#' #     #cowplot::theme_cowplot() +
#' #     ggplot2::theme(text = ggplot2::element_text(family = "Sans Serif"),
#' #                    legend.position = "bottom")
#' #
#' #   return(plots)
#' # }
#'
#'
#' #' @title Plot True Versus Observed Data
#' #'
#' #' @description
#' #' @description
#' #' Given a data.frame that follows the structure shown in
#' #' \code{\link{quantile_example_data_wide}}, the function generates a plot
#' #' of true vs. observed values.
#' #'
#' #' @param data a data.frame following the specifications from
#' #' \code{\link{eval_forecasts}}) for quantile forecasts. For an example, see
#' #' \code{\link{quantile_example_data_wide}})
#' #'
#' #' @return a ggplot2 object with a plot of true vs. observed values
#' #'
#' #' @importFrom ggplot2 ggplot geom_line geom_ribbon xlab ylab aes_string facet_wrap
#' #' @examples
#' #' data <- scoringutils::quantile_example_data_wide
#' #' plot_true_vs_pred(data,
#' #'                   x = "id",
#' #'                   y = "true_values",
#' #'                   facet_formula = horizon ~ model)
#' #'
#'
#' plot_true_vs_pred <- function(data,
#'                               x = "epiweek",
#'                               y = "true_values",
#'                               inner_y_max = "upper_50",
#'                               inner_y_min = "lower_50",
#'                               outer_y_max = "upper_90",
#'                               outer_y_min = "lower_90",
#'                               fill = "model",
#'                               facet_formula = ~ model,
#'                               num_facet_cols = NULL,
#'                               xlab = x,
#'                               ylab = y) {
#'
#'
#'   out <- ggplot2::ggplot(data, ggplot2::aes_string(x = x,
#'                                                    fill = fill)) +
#'     ggplot2::geom_ribbon(ggplot2::aes_string(ymax = outer_y_max,
#'                                              ymin = outer_y_min),
#'                          alpha = 0.3) +
#'     ggplot2::geom_ribbon(ggplot2::aes_string(ymax = inner_y_max,
#'                                              ymin = inner_y_min),
#'                          alpha = 0.6) +
#'     ggplot2::facet_wrap(facet_formula, ncol = num_facet_cols,
#'                         scales = "free") +
#'     ggplot2::geom_line(ggplot2::aes_string(y = y), colour = "black",
#'                        linetype = "dashed") +
#'     ggplot2::theme(text = ggplot2::element_text(family = "Sans Serif"),
#'                    legend.position = "bottom") +
#'     ggplot2::xlab(xlab) +
#'     ggplot2::ylab(ylab)
#'
#'   return(out)
#'
#' }
#'
#'
#' dat <- data.table::dcast(data, ... ~ paste("sampl_", sample, sep = ""),
#'                          value.var = "predictions")
#'
#' # compute pit p-values
#' dat[, c("pit_p_val", "pit_sd", "plot") := do.call(pit, c(list(true_values,
#'                                                               as.matrix(.SD)),
#'                                                          pit_arguments)),
#'     .SDcols = names(dat)[grepl("sampl_", names(dat))], by = by]
#'
#'
#'
#'
