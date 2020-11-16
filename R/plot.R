#' @title Plot Coloured Score Table
#'
#' @description
#' Plots a coloured table of summarised scores obtained using
#' \code{\link{eval_forecasts}}
#'
#' @param summarised_scores A data.frame of summarised scores as produced by
#' \code{\link{eval_forecasts}}
#' @param y the variable to be shown on the y-axis. If \code{NULL} (default),
#' all columns that are not scoring metrics will be used. Alternatively,
#' you can specify a vector with column names, e.g.
#' \code{y = c("model", "location")}. These column names will be concatenated
#' to create a unique row identifier (e.g. "model1_location1")
#' @param select_metrics A character vector with the metrics to show. If set to
#' \code{NULL} (default), all metrics present in \code{summarised_scores} will
#' be shown
#' @param facet_formula formula for facetting in ggplot. If this is \code{NULL}
#' (the default), no facetting will take place
#' @param facet_wrap_or_grid Use ggplot2's \code{facet_wrap} or
#' \code{facet_grid}? Anything other than "facet_wrap" will be interpreted as
#' \code{facet_grid}. This only takes effect if \code{facet_formula} is not
#' \code{NULL}
#' @param ncol Number of columns for facet wrap. Only relevant if
#' \code{facet_formula} is given and \code{facet_wrap_or_grid == "facet_wrap"}
#' @return A ggplot2 object with a coloured table of summarised scores
#' @importFrom ggplot2 ggplot aes element_blank element_text labs coord_cartesian
#' @importFrom data.table setDT melt
#' @importFrom stats sd
#' @export
#'
#' @examples
#' scores <- scoringutils::eval_forecasts(scoringutils::quantile_example_data_wide,
#'                                        by = c("model", "id", "horizon"),
#'                                        summarise_by = c("model", "horizon"))
#' scoringutils::score_table(scores, y = "model", facet_formula = ~ horizon,
#'                           ncol = 1)
#'
#' scoringutils::score_table(scores, y = c("model", "horizon"))
#'
#' # yields the same result in this case
#' scoringutils::score_table(scores)

score_table <- function(summarised_scores,
                        y = NULL,
                        select_metrics = NULL,
                        facet_formula = NULL,
                        ncol = NULL,
                        facet_wrap_or_grid = "facet_wrap") {


  # identify metrics -----------------------------------------------------------
  # identify metrics by looking at which of the available column names
  # are metrics. All other variables are treated as identifier variables

  all_metrics <- list_of_avail_metrics()

  metrics <- names(summarised_scores)[names(summarised_scores) %in% all_metrics]
  id_vars <- names(summarised_scores)[!(names(summarised_scores) %in% all_metrics)]


  # compute scaled values ------------------------------------------------------
  # scaling is done in order to colour the different scores
  # for most metrics larger is worse, but others like bias are better if they
  # are close to zero and deviations in both directions is worse
  summarised_scores <- data.table::as.data.table(summarised_scores)

  # define which metrics are scaled using min (larger is worse) and
  # which not (metrics like bias where deviations in both directions are bad)
  metrics_non_min <- c("bias", "coverage_deviation")
  metrics_no_color <- c("coverage")
  metrics_min <- setdiff(metrics, c(metrics_non_min, metrics_no_color))

  if (!is.null(select_metrics)) {
    metrics_non_min <- metrics_non_min[metrics_non_min %in% metrics_select]
    metrics_min <- metrics_min[metrics_min %in% metrics_select]
  }

  # write scale functions that can be used in data.table
  scale <- function(x) {
    scaled <- x / sd(x, na.rm = TRUE)
    return(scaled)
  }
  scale_min <- function(x) {
    scaled <- (x - min(x)) / sd(x, na.rm = TRUE)
    return(scaled)
  }

  # pivot longer and add scaled values
  df <- data.table::melt(summarised_scores, value.vars = metrics,
                         id.vars = id_vars,
                         variable.name = "metric")

  df[metric %in% metrics_min, value_scaled := scale_min(value),
     by = metric]
  df[metric %in% metrics_non_min, value_scaled := scale(value),
     by = metric]
  df[metric %in% metrics_no_color, value_scaled := 0,
  by = metric]


  if (is.null(y)) {
    # create an identifier column by concatinating all columns that
    # are not a metric
    identifier_columns <- names(df)[!names(df) %in%
                                      c("metric", "value", "value_scaled")]
  } else {
    identifier_columns <- y
  }

  df[, identif := do.call(paste, c(.SD, sep = "_")),
     .SDcols = identifier_columns]

  # make plot with all metrics that are not NA
  plot <- ggplot2::ggplot(df[!is.na(value), ],
                          ggplot2::aes(y = identif, x = metric)) +
    #ggplot2::geom_tile(fill = "blue") +
    ggplot2::geom_tile(ggplot2::aes(fill = value_scaled), colour = "white") +
    ggplot2::geom_text(ggplot2::aes(y = identif, label = round(value, 2))) +
    ggplot2::scale_fill_gradient2(low = "steelblue", high = "salmon") +
    ggplot2::theme_light() +
    ggplot2::theme(legend.title = ggplot2::element_blank(),
                   legend.position = "none",
                   axis.text.x = ggplot2::element_text(angle = 45, vjust = 1,
                                                       hjust=1)) +
    ggplot2::labs(x = "", y = "") +
    ggplot2::coord_cartesian(expand=FALSE)

  # colouring for pit_p_val is not ideal

  if (!is.null(facet_formula)) {
    if (facet_wrap_or_grid == "facet_wrap") {
      plot <- plot +
        ggplot2::facet_wrap(facet_formula, ncol = ncol)
    } else {
      plot <- plot +
        ggplot2::facet_grid(facet_formula)
    }
  }

  return(plot)

}




#' @title Plot Correlation Between Metrics
#'
#' @description
#' Plots a coloured table of summarised scores obtained using
#' \code{\link{eval_forecasts}}
#'
#' @param scores A data.frame of scores as produced by
#' \code{\link{eval_forecasts}}
#' @param select_metrics A character vector with the metrics to show. If set to
#' \code{NULL} (default), all metrics present in \code{summarised_scores} will
#' be shown
#' @return A ggplot2 object showing a coloured matrix of correlations
#' between metrics
#' @importFrom ggplot2 ggplot geom_tile geom_text aes scale_fill_gradient2
#' element_text labs coord_cartesian theme theme_minimal
#' @importFrom stats cor na.omit
#' @importFrom data.table setDT melt
#' @importFrom forcats fct_relevel fct_rev
#' @export
#'
#' @examples
#' scores <- scoringutils::eval_forecasts(scoringutils::quantile_example_data_wide,
#'                                        by = c("model", "id", "horizon"),
#'                                        summarise_by = c("model", "id"))
#' scoringutils::correlation_plot(scores)


correlation_plot <- function(scores,
                             select_metrics = NULL) {

  # define possible metrics
  all_metrics <- list_of_avail_metrics()


  # find metrics present
  metrics <- names(scores)[names(scores) %in% all_metrics]

  # restrict to selected metrics
  if (!is.null(select_metrics)) {
    metrics <- metrics[metrics %in% metrics_select]
  }

  # remove all non metrics and non-numeric columns
  df <- scores[, .SD, .SDcols = sapply(scores,
                                 function(x) {
                                   (all(is.numeric(x))) && all(is.finite(x))
                                 })]
  df <- df[, .SD, .SDcols = names(df) %in% metrics]

  # define correlation matrix
  cor_mat <- round(cor(as.matrix(df)), 2)

  # define function to obtain upper triangle of matrix
  get_lower_tri <- function(cormat){
    cormat[lower.tri(cormat)] <- NA
    return(cormat)
  }

  # get plot data.frame
  plot_df <- data.table::setDT(as.data.frame(get_lower_tri(cor_mat)),
                               keep.rownames = TRUE)[, metric := rn][, rn := NULL]
  plot_df <- na.omit(data.table::melt(plot_df, id.vars = "metric"))

  # refactor levels according to the metrics
  metrics <- unique(plot_df$metric)
  plot_df[, metric := forcats::fct_relevel(metric, metrics)]
  plot_df[, variable := forcats::fct_relevel(variable, metrics)]
  plot_df[, variable := forcats::fct_rev(variable)]

  plot <- ggplot2::ggplot(plot_df, ggplot2::aes(x = variable, y = metric,
                                                fill = value)) +
    ggplot2::geom_tile(color = "white") +
    ggplot2::geom_text(ggplot2::aes(y = metric, label = value)) +
    ggplot2::scale_fill_gradient2(low = "steelblue", mid = "white",
                                  high = "salmon",
                                  name = "Correlation",
                                  breaks = c(-1, -0.5, 0, 0.5, 1)) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, vjust = 1,
                                                       hjust=1)) +
    ggplot2::labs(x = "", y = "") +
    ggplot2::coord_cartesian(expand = FALSE)

  return(plot)

}






#' @title Plot Contributions to the Weighted Interval Score
#'
#' @description
#' Visualise the components of the weighted interval score: penalties for
#' over-predicition, under-prediction and for a lack of sharpness
#'
#' @param scores A data.frame of scores based on quantile forecasts as
#' produced by \code{\link{eval_forecasts}}
#' @param x The variable from the scores you want to show on the x-Axis.
#' Usually this will be "model"
#' @param group Choose a grouping variable for the plot that gets directly
#' passed down to ggplot. Default is \code{NULL}
#' @param relative_contributions show relative contributions instead of absolute
#' contributions. Default is FALSE and this functionality is not available yet.
#' @param facet_formula facetting formula passed down to ggplot. Default is
#' \code{NULL}
#' @param scales scales argument that gets passed down to ggplot. Only necessary
#' if you make use of facetting. Default is "free_y"
#' @param nrow nrow argument that gets passed down to ggplot. Specifies the
#' number of rows to use for \code{facet_wrap} in ggplot
#' @param xlab Label for the x-axis. Default is the variable name on the x-axis
#' @param ylab Label for the y-axis. Default is "WIS contributions"
#' @return A ggplot2 object showing a contributions from the three components of
#' the weighted interval score
#' @importFrom ggplot2 ggplot aes_string aes geom_linerange facet_wrap labs
#' theme theme_minimal unit
#' @export
#'
#' @examples
#' scores <- scoringutils::eval_forecasts(scoringutils::quantile_example_data_wide,
#'                                        by = c("model", "id", "horizon"),
#'                                        summarise_by = c("model", "horizon"))
#' scoringutils::wis_components(scores, x = "model", facet_formula = ~ horizon)
#'
#' @references
#' Bracher J, Ray E, Gneiting T, Reich, N (2020) Evaluating epidemic forecasts
#' in an interval format. \url{https://arxiv.org/abs/2005.12881}


wis_components <- function(scores,
                           x = "model",
                           group = NULL,
                           relative_contributions = FALSE,
                           facet_formula = NULL,
                           scales = "free_y",
                           nrow = NULL,
                           xlab = x,
                           ylab = "WIS contributions") {

  scores <- data.table::as.data.table(scores)

  plot <- ggplot2::ggplot(scores, ggplot2::aes_string(x = x, group = group)) +
    ggplot2::geom_linerange(ggplot2::aes(ymax = underprediction,
                                         ymin = 0, colour = "Underprediction"),
                            size = 3) +
    ggplot2::geom_linerange(ggplot2::aes(ymax = overprediction + underprediction,
                                         ymin = underprediction,
                                         colour = "Overprediction"),
                            size = 3)  +
    ggplot2::geom_linerange(ggplot2::aes(ymax = sharpness + overprediction + underprediction,
                                         ymin = overprediction + underprediction,
                                         colour = "Sharpness"),
                            size = 3) +
    ggplot2::facet_wrap(facet_formula, nrow = nrow,
                        scales = scales) +
    ggplot2::labs(x = xlab, y = ylab) +
    ggplot2::theme_minimal() +
    ggplot2::theme(panel.spacing = ggplot2::unit(4, "mm"),
                   axis.text.x = ggplot2::element_text(angle = 45, vjust = 1,
                                                       hjust=1))

  return(plot)

}



#' @title Plot Metrics by Range of the Prediction Interval
#'
#' @description
#' Visualise the metrics by range, e.g. if you are interested how different
#' interval ranges contribute to the overal interval score, or how sharpness
#' changes by range.
#'
#' @param scores A data.frame of scores based on quantile forecasts as
#' produced by \code{\link{eval_forecasts}}. Note that "range" must be included
#' in the \code{summarise_by} argument when running \code{eval_forecasts}
#' @param y The variable from the scores you want to show on the y-Axis.
#' This could be something like "interval_score" (the default) or "sharpness"
#' @param x The variable from the scores you want to show on the x-Axis.
#' Usually this will be "model"
#' @param colour Charachter vector of length one used to determine a variable
#' for colouring dots. The Default is "range".
#' @param xlab Label for the x-axis. Default is the variable name on the x-axis
#' @param ylab Label for the y-axis. Default is "WIS contributions"
#' @return A ggplot2 object showing a contributions from the three components of
#' the weighted interval score
#' @importFrom ggplot2 ggplot aes_string aes geom_point geom_line
#' expand_limits theme theme_light element_text scale_color_continuous labs
#' @export
#'
#' @examples
#' scores <- scoringutils::eval_forecasts(scoringutils::quantile_example_data_long,
#'                                        by = c("model", "id", "horizon"),
#'                                        summarise_by = c("model", "horizon", "range"))
#' scoringutils::range_plot(scores, x = "model")
#'
#' scoringutils::range_plot(scores, y = "sharpness", x = "model")


range_plot <- function(scores,
                       y = "interval_score",
                       x = "model",
                       colour = "range",
                       xlab = x,
                       ylab = y) {

  ggplot2::ggplot(scores,
                  ggplot2::aes_string(x = x,
                                      y = y,
                                      colour = colour)) +
    ggplot2::geom_point(size = 2) +
    ggplot2::geom_line(ggplot2::aes(group = range),
                       colour = "black",
                       size = 0.01) +
    ggplot2::theme_light() +
    ggplot2::expand_limits(y = 0) +
    ggplot2::scale_color_continuous(low = "steelblue", high = "salmon") +
    ggplot2::theme(legend.position = "right",
                   axis.text.x = ggplot2::element_text(angle = 45, vjust = 1,
                                                       hjust=1)) +
    ggplot2::labs(y = ylab,
                  x = xlab)

}










#' @title Create a Heatmap of a Scoring Metric
#'
#' @description
#' This function can be used to create a heatmap of one metric across different
#' groups, e.g. the interval score obtained by several forecasting models in
#' different locations.
#'
#' @param scores A data.frame of scores based on quantile forecasts as
#' produced by \code{\link{eval_forecasts}}.
#' @param y The variable from the scores you want to show on the y-Axis. The
#' default for this is "model"
#' @param x The variable from the scores you want to show on the x-Axis. This
#' could be something like "horizon", or "location"
#' @param metric the metric that determines the value and colour shown in the
#' tiles of the heatmap
#' @param xlab Label for the x-axis. Default is the variable name on the x-axis
#' @param ylab Label for the y-axis. Default is the variable name on the y-axis
#' @return A ggplot2 object showing a heatmap of the desired metric
#' @importFrom data.table setDT `:=`
#' @importFrom ggplot2 ggplot aes_string aes geom_tile geom_text
#' scale_fill_gradient2 labs element_text coord_cartesian
#' @export
#'
#' @examples
#' scores <- scoringutils::eval_forecasts(scoringutils::quantile_example_data_long,
#'                                        by = c("model", "id", "horizon"),
#'                                        summarise_by = c("model", "horizon"))
#'
#' scoringutils::score_heatmap(scores, x = "horizon", metric = "bias")
#'



score_heatmap <- function(scores,
                         y = "model",
                         x,
                         metric,
                         ylab = y,
                         xlab = x) {


  data.table::setDT(scores)

  scores[, eval(metric) := round(get(metric), 2)]

  ggplot2::ggplot(scores,
                  ggplot2::aes_string(y = y,
                                      x = x,
                                      fill = metric)) +
    ggplot2::geom_tile() +
    ggplot2::geom_text(ggplot2::aes_string(label = metric)) +
    ggplot2::scale_fill_gradient2(low = "skyblue", high = "red") +
    ggplot2::labs(y = ylab, x = xlab) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, vjust = 1,
                                                       hjust=1)) +
    ggplot2::coord_cartesian(expand = FALSE)

}




#' @title Plot Predictions vs True Values
#'
#' @description
#' Make a plot of observed and predicted values
#'
#' @param data a data.frame that follows the same specifications outlined in
#' \code{\link{eval_forecasts}}. The data.frame needs to have columns called
#' "true_value", "prediction" and then either a column called sample, or one
#' called "quantile" or two columns called "range" and "boundary".
#' @param x character vector of length one that denotes the name of the variable
#' on the x-axis. Usually, this will be "date", but it can be anything else.
#' @param add_truth_data additional truth data, e.g. past values for which
#' no predictions are available. This should be a data.frame with a column
#' called "true_value" and another column corresponding to the x variable selected
#' for the plot
#' @param range numeric vector indicating the interval ranges to plot. If 0 is
#' included in range, the median prediction will be shown.
#' @param facet_formula formula for facetting in ggplot. If this is \code{NULL}
#' (the default), no facetting will take place
#' @param facet_wrap_or_grid Use ggplot2's \code{facet_wrap} or
#' \code{facet_grid}? Anything other than "facet_wrap" will be interpreted as
#' \code{facet_grid}. This only takes effect if \code{facet_formula} is not
#' \code{NULL}
#' @param scales scales argument that gets passed down to ggplot. Only necessary
#' if you make use of facetting. Default is "free_y"
#' @param xlab Label for the x-axis. Default is the variable name on the x-axis
#' @param ylab Label for the y-axis. Default is "True and predicted values"
#' @return ggplot object with a plot of true vs predicted values
#' @importFrom ggplot2 ggplot scale_colour_manual scale_fill_manual
#' facet_wrap facet_grid
#' @importFrom data.table dcast
#' @export
#'
#' @examples
#' example1 <- scoringutils::continuous_example_data
#' example2 <- scoringutils::quantile_example_data_long
#'
#' scoringutils::plot_predictions(example1, x = "id",
#'                                facet_formula = ~ horizon)
#' scoringutils::plot_predictions(example2, x = "id",
#'                                facet_formula = ~ horizon)


plot_predictions <- function(data,
                             x = "date",
                             add_truth_data = NULL,
                             range = c(0, 50, 90),
                             facet_formula = NULL,
                             facet_wrap_or_grid = "facet_wrap",
                             scales = "free_y",
                             xlab = x,
                             ylab = "True and predicted values") {


  # find out what type of predictions we have
  colnames <- colnames(data)

  if ("sample" %in% colnames) {
    data <- scoringutils::sample_to_range(data)
    data[, quantile := NULL]
  } else if ("quantile" %in% colnames) {
    data <- scoringutils::quantile_to_range(data)
  }

  # select appropriate boundaries and pivot wider
  select <- data$range %in% setdiff(range, 0)

  intervals <- data[select, ]

  if ("quantile" %in% names(intervals)) {
    intervals[, quantile := NULL]
  }

  intervals <- data.table::dcast(intervals, ... ~ boundary,
                    value.var = "prediction")
  intervals[, range := as.factor(range)]


  plot <- ggplot2::ggplot(intervals, ggplot2::aes_string(x = x)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = lower, ymax = upper,
                                      group = range, fill = range),
                         alpha = 0.4) +
    ggplot2::geom_point(ggplot2::aes(y = true_value), size = 0.5) +
    ggplot2::geom_line(ggplot2::aes(y = true_value, colour = "actual"),
                       lwd = 0.2) +
    ggplot2::scale_colour_manual("",values = c("black", "steelblue4")) +
    ggplot2::scale_fill_manual("range", values = c("steelblue3",
                                              "lightskyblue3",
                                              "lightskyblue2",
                                              "lightskyblue1")) +
    ggplot2::theme_minimal()

  if (0 %in% range) {
    select_median <- (data$range %in% 0 & data$boundary == "lower")
    median <- data[select_median]

    plot <- plot +
      ggplot2::geom_line(data = median,
                         mapping = ggplot2::aes(y = prediction, colour = "median"),
                         lwd = 0.4)
  }


  if (!is.null(facet_formula)) {
    if (facet_wrap_or_grid == "facet_wrap") {
      plot <- plot +
        ggplot2::facet_wrap(facet_formula, scales = scales)
    } else {
      plot <- plot +
        ggplot2::facet_grid(facet_formula, scales = scales)
    }
  }

  plot <- plot +
    ggplot2::labs(x = xlab, y = ylab)

  if(!is.null(add_truth_data)) {
    plot <- plot +
      ggplot2::geom_line(data = add_truth_data,
                         ggplot2::aes(y = true_value, colour = "actual"),
                         lwd = 0.2)
  }

  return(plot)
}







#' @title Plot Interval Coverage
#'
#' @description
#' Plot interval coverage
#'
#' @param summarised_scores Summarised scores as produced by
#' \code{\link{eval_forecasts}}. Make sure that "range" is included in
#' \code{summarise_by} when producing the summarised scores
#' @param colour According to which variable shall the graphs be coloured?
#' Default is "model".
#' @param facet_formula formula for facetting in ggplot. If this is \code{NULL}
#' (the default), no facetting will take place
#' @param facet_wrap_or_grid Use ggplot2's \code{facet_wrap} or
#' \code{facet_grid}? Anything other than "facet_wrap" will be interpreted as
#' \code{facet_grid}. This only takes effect if \code{facet_formula} is not
#' \code{NULL}
#' @param scales scales argument that gets passed down to ggplot. Only necessary
#' if you make use of facetting. Default is "free_y"
#' @return ggplot object with a plot of interval coverage
#' @importFrom ggplot2 ggplot scale_colour_manual scale_fill_manual
#' facet_wrap facet_grid
#' @importFrom data.table dcast
#' @export
#'
#' @examples
#' example1 <- scoringutils::quantile_example_data_long
#' scores <- scoringutils::eval_forecasts(example1,
#'                                        summarise_by = c("model", "range"))
#' interval_coverage(scores)

interval_coverage <- function(summarised_scores,
                              colour = "model",
                              facet_formula = NULL,
                              facet_wrap_or_grid = "facet_wrap",
                              scales = "free_y") {
  ## overall model calibration - empirical interval coverage
  p1 <- ggplot2::ggplot(summarised_scores, ggplot2::aes_string(x = "range",
                                                    colour = colour)) +
    ggplot2::geom_polygon(data = data.frame(x = c(0, 0, 100),
                                            y = c(0, 100, 100),
                                            g = c("o", "o", "o")),
                          ggplot2::aes(x = x, y = y, group = g,
                                       fill = g),
                          alpha = 0.05,
                          colour = "white",
                          fill = "olivedrab3") +
    ggplot2::geom_line(ggplot2::aes(y = range), colour = "grey",
                       linetype = "dashed") +
    ggplot2::geom_line(ggplot2::aes(y = coverage * 100)) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::ylab("% Obs inside interval") +
    ggplot2::xlab("Interval range") +
    ggplot2::coord_cartesian(expand = FALSE)

  if (!is.null(facet_formula)) {
    if (facet_wrap_or_grid == "facet_wrap") {
      p1 <- p1 +
        ggplot2::facet_wrap(facet_formula, scales = scales)
    } else {
      p1 <- p1 +
        ggplot2::facet_grid(facet_formula, scales = scales)
    }
  }

  return(p1)
}





#' @title Plot Quantile Coverage
#'
#' @description
#' Plot quantile coverage
#'
#' @param summarised_scores Summarised scores as produced by
#' \code{\link{eval_forecasts}}. Make sure that "quantile" is included in
#' \code{summarise_by} when producing the summarised scores
#' @param colour According to which variable shall the graphs be coloured?
#' Default is "model".
#' @param facet_formula formula for facetting in ggplot. If this is \code{NULL}
#' (the default), no facetting will take place
#' @param facet_wrap_or_grid Use ggplot2's \code{facet_wrap} or
#' \code{facet_grid}? Anything other than "facet_wrap" will be interpreted as
#' \code{facet_grid}. This only takes effect if \code{facet_formula} is not
#' \code{NULL}
#' @param scales scales argument that gets passed down to ggplot. Only necessary
#' if you make use of facetting. Default is "free_y"
#' @return ggplot object with a plot of interval coverage
#' @importFrom ggplot2 ggplot scale_colour_manual scale_fill_manual
#' facet_wrap facet_grid
#' @importFrom data.table dcast
#' @export
#'
#' @examples
#' example1 <- scoringutils::quantile_example_data_long
#' scores <- scoringutils::eval_forecasts(example1,
#'                                        summarise_by = c("model", "quantile"))
#' quantile_coverage(scores)

quantile_coverage <- function(summarised_scores,
                              colour = "model",
                              facet_formula = NULL,
                              facet_wrap_or_grid = "facet_wrap",
                              scales = "free_y") {

  p2 <- ggplot2::ggplot(data = summarised_scores,
                        ggplot2::aes_string(x = "quantile", colour = colour)) +
    ggplot2::geom_polygon(data = data.frame(x = c(0, 0.5, 0.5,
                                                  0.5, 0.5, 1),
                                            y = c(0, 0, 0.5,
                                                  0.5, 1, 1),
                                            g = c("o", "o", "o")),
                          ggplot2::aes(x = x, y = y, group = g,
                                       fill = g),
                          alpha = 0.05,
                          colour = "white",
                          fill = "olivedrab3") +
    ggplot2::geom_line(ggplot2::aes(y = quantile), colour = "grey",
                       linetype = "dashed") +
    ggplot2::geom_line(ggplot2::aes(y = quantile_coverage)) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::xlab("Quantile") +
    ggplot2::ylab("% obs below quantile") +
    ggplot2::coord_cartesian(expand = FALSE)

  if (!is.null(facet_formula)) {
    if (facet_wrap_or_grid == "facet_wrap") {
      p2 <- p2 +
        ggplot2::facet_wrap(facet_formula, scales = scales)
    } else {
      p2 <- p2 +
        ggplot2::facet_grid(facet_formula, scales = scales)
    }
  }

  return(p2)

}
