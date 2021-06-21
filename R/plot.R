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
#' scores <- scoringutils::eval_forecasts(scoringutils::quantile_example_data,
#'                                        summarise_by = c("model", "value_desc"))
#' scoringutils::score_table(scores, y = "model", facet_formula = ~ value_desc,
#'                            ncol = 1)
#'
#' # can also put target description on the y-axis
#' scoringutils::score_table(scores, y = c("model", "value_desc"))
#'
#' # yields the same result in this case
#' scoringutils::score_table(scores)
#'
#'
#' scores <- scoringutils::eval_forecasts(scoringutils::integer_example_data,
#'                                         summarise_by = c("model", "value_desc"))
#' scoringutils::score_table(scores, y = "model", facet_formula = ~ value_desc,
#'                            ncol = 1)
#'
#' # only show selected metrics
#' scoringutils::score_table(scores, y = "model", facet_formula = ~ value_desc,
#'                            ncol = 1, select_metrics = c("crps", "bias"))

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


  # metrics to delete
  summarised_scores <- data.table::as.data.table(summarised_scores)

  if (!is.null(select_metrics)) {
    to_delete <- setdiff(metrics, select_metrics)
    summarised_scores[, (to_delete) := NULL]
  }

  # compute scaled values ------------------------------------------------------
  # scaling is done in order to colour the different scores
  # for most metrics larger is worse, but others like bias are better if they
  # are close to zero and deviations in both directions are bad

  # define which metrics are scaled using min (larger is worse) and
  # which not (metrics like bias where deviations in both directions are bad)
  metrics_zero_good <- c("bias", "coverage_deviation")
  metrics_no_color <- c("coverage")
  metrics_p_val <- c("pit_p_val")
  metrics_min_good <- setdiff(metrics, c(metrics_zero_good, metrics_p_val,
                                         metrics_no_color))

  # write scale functions that can be used in data.table
  scale <- function(x) {
    scaled <- x / sd(x, na.rm = TRUE)
    return(scaled)
  }
  scale_min_good <- function(x) {
    scaled <- (x - min(x)) / sd(x, na.rm = TRUE)
    return(scaled)
  }
  scale_p_val <- function(x) {
    out <- rep(0, length(x))
    out[x < 0.1] <- 0.2
    out[x < 0.05] <- 0.5
    out[x < 0.01] <- 1
    return(out)
  }

  # pivot longer and add scaled values
  df <- data.table::melt(summarised_scores, value.vars = metrics,
                         id.vars = id_vars,
                         variable.name = "metric")

  df[metric %in% metrics_min_good, value_scaled := scale_min_good(value),
     by = metric]
  df[metric %in% metrics_zero_good, value_scaled := scale(value),
     by = metric]
  df[metric %in% metrics_no_color, value_scaled := 0,
     by = metric]
  df[metric %in% metrics_p_val, value_scaled := scale_p_val(value),
     by = metric]


  # create identifier column for plot if not given -----------------------------
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


  # plot -----------------------------------------------------------------------
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
                   axis.text.x = ggplot2::element_text(angle = 90, vjust = 1,
                                                       hjust=1)) +
    ggplot2::labs(x = "", y = "") +
    ggplot2::coord_cartesian(expand=FALSE)

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
#' Plots a coloured table of scores obtained using
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
#' element_text labs coord_cartesian theme theme_light
#' @importFrom stats cor na.omit
#' @importFrom data.table setDT melt
#' @importFrom forcats fct_relevel fct_rev
#' @export
#'
#' @examples
#' scores <- scoringutils::eval_forecasts(scoringutils::quantile_example_data)
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
    ggplot2::geom_tile(color = "white",
                       width = 0.97, height = 0.97) +
    ggplot2::geom_text(ggplot2::aes(y = metric, label = value)) +
    ggplot2::scale_fill_gradient2(low = "steelblue", mid = "white",
                                  high = "salmon",
                                  name = "Correlation",
                                  breaks = c(-1, -0.5, 0, 0.5, 1)) +
    ggplot2::theme_light() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 1,
                                                       hjust=1),
                   panel.grid.major.y = ggplot2::element_blank(),
                   panel.grid.minor.y = ggplot2::element_blank(),
                   panel.grid.major.x = ggplot2::element_blank(),
                   panel.grid.minor.x = ggplot2::element_blank()) +
    ggplot2::labs(x = "", y = "") +
    ggplot2::coord_cartesian(expand = FALSE) +
    ggplot2::labs(title = "Correlation between metrics")

  return(plot)
}






#' @title Plot Contributions to the Weighted Interval Score
#'
#' @description
#' Visualise the components of the weighted interval score: penalties for
#' over-prediction, under-prediction and for a lack of sharpness
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
#' @param facet_wrap_or_grid Use ggplot2's \code{facet_wrap} or
#' \code{facet_grid}? Anything other than "facet_wrap" will be interpreted as
#' \code{facet_grid}. This only takes effect if \code{facet_formula} is not
#' \code{NULL}
#' @param ncol Number of columns for facet wrap. Only relevant if
#' \code{facet_formula} is given and \code{facet_wrap_or_grid == "facet_wrap"}
#' @param x_text_angle Angle for the text on the x-axis. Default is 90
#' @param xlab Label for the x-axis. Default is the variable name on the x-axis
#' @param ylab Label for the y-axis. Default is "WIS contributions"
#' @return A ggplot2 object showing a contributions from the three components of
#' the weighted interval score
#' @importFrom ggplot2 ggplot aes_string aes geom_linerange facet_wrap labs
#' theme theme_light unit
#' @export
#'
#' @examples
#' scores <- scoringutils::eval_forecasts(scoringutils::quantile_example_data,
#'                                        summarise_by = c("model", "value_desc"))
#' scoringutils::wis_components(scores, x = "model", facet_formula = ~ value_desc,
#'                              relative_contributions = TRUE)
#' scoringutils::wis_components(scores, x = "model", facet_formula = ~ value_desc,
#'                              relative_contributions = FALSE)
#' @references
#' Bracher J, Ray E, Gneiting T, Reich, N (2020) Evaluating epidemic forecasts
#' in an interval format. \url{https://arxiv.org/abs/2005.12881}


wis_components <- function(scores,
                           x = "model",
                           group = NULL,
                           relative_contributions = FALSE,
                           facet_formula = NULL,
                           scales = "free_y",
                           ncol = NULL,
                           facet_wrap_or_grid = "facet_wrap",
                           x_text_angle = 90,
                           xlab = x,
                           ylab = "WIS contributions") {

  scores <- data.table::as.data.table(scores)

  scores <- data.table::melt(scores,
                             measure.vars = c("overprediction",
                                              "underprediction",
                                              "sharpness"),
                             variable.name = "wis_component_name",
                             value.name = "component_value")

  # stack or fill the geom_col position
  col_position <- ifelse(relative_contributions, "fill", "stack")

  plot <- ggplot2::ggplot(scores, ggplot2::aes_string(x = x, group = group)) +
    ggplot2::geom_col(position = col_position,
                      ggplot2::aes(y = component_value, fill = wis_component_name)) +
    ggplot2::facet_wrap(facet_formula, ncol = ncol,
                        scales = scales) +
    ggplot2::labs(x = xlab, y = ylab) +
    ggplot2::theme_light() +
    ggplot2::theme(panel.spacing = ggplot2::unit(4, "mm"),
                   axis.text.x = ggplot2::element_text(angle = x_text_angle,
                                                       vjust = 1,
                                                       hjust=1))

  if (!is.null(facet_formula)) {
    if (facet_wrap_or_grid == "facet_wrap") {
      plot <- plot +
        ggplot2::facet_wrap(facet_formula, ncol = ncol,
                            scales = scales)
    } else {
      plot <- plot +
        ggplot2::facet_grid(facet_formula, scales = scales)
    }
  }

  return(plot)

}



#' @title Plot Metrics by Range of the Prediction Interval
#'
#' @description
#' Visualise the metrics by range, e.g. if you are interested how different
#' interval ranges contribute to the overall interval score, or how sharpness
#' changes by range.
#'
#' @param scores A data.frame of scores based on quantile forecasts as
#' produced by \code{\link{eval_forecasts}}. Note that "range" must be included
#' in the \code{summarise_by} argument when running \code{eval_forecasts}
#' @param y The variable from the scores you want to show on the y-Axis.
#' This could be something like "interval_score" (the default) or "sharpness"
#' @param x The variable from the scores you want to show on the x-Axis.
#' Usually this will be "model"
#' @param colour Character vector of length one used to determine a variable
#' for colouring dots. The Default is "range".
#' @param facet_formula facetting formula passed down to ggplot. Default is
#' \code{NULL}
#' @param scales scales argument that gets passed down to ggplot. Only necessary
#' if you make use of facetting. Default is "free_y"
#' @param facet_wrap_or_grid Use ggplot2's \code{facet_wrap} or
#' \code{facet_grid}? Anything other than "facet_wrap" will be interpreted as
#' \code{facet_grid}. This only takes effect if \code{facet_formula} is not
#' \code{NULL}
#' @param ncol Number of columns for facet wrap. Only relevant if
#' \code{facet_formula} is given and \code{facet_wrap_or_grid == "facet_wrap"}
#' @param xlab Label for the x-axis. Default is the variable name on the x-axis
#' @param ylab Label for the y-axis. Default is "WIS contributions"
#' @return A ggplot2 object showing a contributions from the three components of
#' the weighted interval score
#' @importFrom ggplot2 ggplot aes_string aes geom_point geom_line
#' expand_limits theme theme_light element_text scale_color_continuous labs
#' @export
#'
#' @examples
#' scores <- scoringutils::eval_forecasts(scoringutils::quantile_example_data,
#'                                         summarise_by = c("model", "value_desc", "range"))
#'
#' scores <- scoringutils::eval_forecasts(scoringutils::range_example_data_long,
#'                                         summarise_by = c("model", "value_desc", "range"))
#' scoringutils::range_plot(scores, x = "model", facet_formula = ~ value_desc)
#'
#' # visualise sharpness instead of interval score
#' scoringutils::range_plot(scores, y = "sharpness", x = "model",
#'                           facet_formula =  ~value_desc)
#'
#' # we saw above that sharpness values crossed. Let's look at the unweighted WIS
#' scores <- scoringutils::eval_forecasts(scoringutils::range_example_data_long,
#'                                         interval_score_arguments = list(weigh = FALSE),
#'                                         summarise_by = c("model", "value_desc", "range"))
#' scoringutils::range_plot(scores, y = "sharpness", x = "model",
#'                           facet_formula =  ~value_desc)


range_plot <- function(scores,
                       y = "interval_score",
                       x = "model",
                       colour = "range",
                       facet_formula = NULL,
                       scales = "free_y",
                       ncol = NULL,
                       facet_wrap_or_grid = "facet_wrap",
                       xlab = x,
                       ylab = y) {

  plot <- ggplot2::ggplot(scores,
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
                   axis.text.x = ggplot2::element_text(angle = 90, vjust = 1,
                                                       hjust=1)) +
    ggplot2::labs(y = ylab,
                  x = xlab)

  if (!is.null(facet_formula)) {
    if (facet_wrap_or_grid == "facet_wrap") {
      plot <- plot +
        ggplot2::facet_wrap(facet_formula, ncol = ncol,
                            scales = scales)
    } else {
      plot <- plot +
        ggplot2::facet_grid(facet_formula, scales = scales)
    }
  }

  return(plot)
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
#' @param facet_formula facetting formula passed down to ggplot. Default is
#' \code{NULL}
#' @param scales scales argument that gets passed down to ggplot. Only necessary
#' if you make use of facetting. Default is "free_y"
#' @param facet_wrap_or_grid Use ggplot2's \code{facet_wrap} or
#' \code{facet_grid}? Anything other than "facet_wrap" will be interpreted as
#' \code{facet_grid}. This only takes effect if \code{facet_formula} is not
#' \code{NULL}
#' @param ncol Number of columns for facet wrap. Only relevant if
#' \code{facet_formula} is given and \code{facet_wrap_or_grid == "facet_wrap"}
#' @return A ggplot2 object showing a heatmap of the desired metric
#' @importFrom data.table setDT `:=`
#' @importFrom ggplot2 ggplot aes_string aes geom_tile geom_text
#' scale_fill_gradient2 labs element_text coord_cartesian
#' @export
#'
#' @examples
#' scores <- scoringutils::eval_forecasts(scoringutils::quantile_example_data,
#'                                        summarise_by = c("model", "value_desc", "range"))
#'
#' scoringutils::score_heatmap(scores, x = "value_desc", metric = "bias")
#'



score_heatmap <- function(scores,
                          y = "model",
                          x,
                          metric,
                          facet_formula = NULL,
                          scales = "free_y",
                          ncol = NULL,
                          facet_wrap_or_grid = "facet_wrap",
                          ylab = y,
                          xlab = x) {


  data.table::setDT(scores)

  scores[, eval(metric) := round(get(metric), 2)]

  plot <- ggplot2::ggplot(scores,
                  ggplot2::aes_string(y = y,
                                      x = x,
                                      fill = metric)) +
    ggplot2::geom_tile() +
    ggplot2::geom_text(ggplot2::aes_string(label = metric)) +
    ggplot2::scale_fill_gradient2(low = "skyblue", high = "red") +
    ggplot2::labs(y = ylab, x = xlab) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 1,
                                                       hjust=1)) +
    ggplot2::coord_cartesian(expand = FALSE)

  if (!is.null(facet_formula)) {
    if (facet_wrap_or_grid == "facet_wrap") {
      plot <- plot +
        ggplot2::facet_wrap(facet_formula, ncol = ncol,
                            scales = scales)
    } else {
      plot <- plot +
        ggplot2::facet_grid(facet_formula, scales = scales)
    }
  }
  return(plot)
}




#' @title Plot Predictions vs True Values
#'
#' @description
#' Make a plot of observed and predicted values
#'
#' @param data a data.frame that follows the same specifications outlined in
#' \code{\link{eval_forecasts}}. The data.frame needs to have columns called
#' "true_value", "prediction" and then either a column called sample, or one
#' called "quantile" or two columns called "range" and "boundary". Internally,
#' these will be separated into a truth and forecast data set in order to be
#' able to apply different filtering to truth data and forecasts. Alternatively
#' you can directly provide a separate truth and forecasts data frame as input.
#' These data sets, however, need to be mergeable, in order to connect forecasts
#' and truth data for plotting.
#' @param x character vector of length one that denotes the name of the variable
#' @param forecasts data.frame with forecasts, that should follow the same
#' general guidelines as the `data` input. Argument can be used to supply
#' forecasts and truth data independently. Default is `NULL`.
#' @param truth_data data.frame with a column called `true_value`
#' on the x-axis. Usually, this will be "date", but it can be anything else.
#' @param merge_by character vector with column names that `forecasts` and
#' `truth_data` should be merged on. Default is `NULL` and merge will be
#' attempted automatically.
#' @param filter_truth a list with character strings that are used to filter
#' the truth data. Every element is parsed as an expression and evaluated
#' in order to filter the truth data.
#' @param filter_forecasts a list with character strings that are used to filter
#' the truth data. Every element is parsed as an expression and evaluated
#' in order to filter the forecasts data.
#' @param filter_both same as `filter_truth` and `filter_forecasts`, but
#' applied to both data sets for convenience.
#' @param remove_from_truth character vector of columns to remove from the
#' truth data. The reason these columns are removed is that sometimes different
#' models or forecasters don't cover the same periods. Removing these columns
#' from the truth data makes sure that nevertheless all available truth data
#' is plotted (instead of having different true values depending on the
#' period covered by a certain model).
#' @param range numeric vector indicating the interval ranges to plot. If 0 is
#' included in range, the median prediction will be shown.
#' @param facet_formula formula for facetting in ggplot. If this is \code{NULL}
#' (the default), no facetting will take place
#' @param facet_wrap_or_grid Use ggplot2's \code{facet_wrap} or
#' \code{facet_grid}? Anything other than "facet_wrap" will be interpreted as
#' \code{facet_grid}. This only takes effect if \code{facet_formula} is not
#' \code{NULL}
#' @param ncol Number of columns for facet wrap. Only relevant if
#' \code{facet_formula} is given and \code{facet_wrap_or_grid == "facet_wrap"}
#' @param scales scales argument that gets passed down to ggplot. Only necessary
#' if you make use of facetting. Default is "free_y"
#' @param allow_truth_without_pred logical, whether or not
#' to allow instances where there is truth data, but no forecast. If `FALSE`
#' (the default), these get filtered out.
#' @param xlab Label for the x-axis. Default is the variable name on the x-axis
#' @param ylab Label for the y-axis. Default is "True and predicted values"
#' @param verbose print out additional helpful messages (default is TRUE)
#' @return ggplot object with a plot of true vs predicted values
#' @importFrom ggplot2 ggplot scale_colour_manual scale_fill_manual
#' facet_wrap facet_grid
#' @importFrom data.table dcast
#' @export
#'
#' @examples
#' example1 <- scoringutils::continuous_example_data
#' example2 <- scoringutils::range_example_data_long
#'
#' scoringutils::plot_predictions(example1, x = "value_date",
#'                                filter_truth = list('value_date <= "2020-06-22"',
#'                                                    'value_date > "2020-05-01"'),
#'                                filter_forecasts = list("model == 'SIRCOVID'",
#'                                                        'creation_date == "2020-06-22"'),
#'                                facet_formula = geography ~ value_desc)
#'
#' scoringutils::plot_predictions(example2, x = "value_date",
#'                                filter_truth = list('value_date <= "2020-06-22"',
#'                                                    'value_date > "2020-05-01"'),
#'                                filter_forecasts = list("model == 'SIRCOVID'",
#'                                                        'creation_date == "2020-06-22"'),
#'                                allow_truth_without_pred = TRUE,
#'                                facet_formula = geography ~ value_desc)

plot_predictions <- function(data = NULL,
                             forecasts = NULL,
                             truth_data = NULL,
                             merge_by = NULL,
                             x = "date",
                             filter_truth = list(),
                             filter_forecasts = list(),
                             filter_both = list(),
                             range = c(0, 50, 90),
                             facet_formula = NULL,
                             facet_wrap_or_grid = "facet_wrap",
                             ncol = NULL,
                             scales = "free_y",
                             allow_truth_without_pred = FALSE,
                             remove_from_truth = c("model", "forecaster", "quantile", "prediction", "sample", "interval"),
                             xlab = x,
                             ylab = "True and predicted values",
                             verbose = TRUE) {

  # preparations ---------------------------------------------------------------
  # check data argument is provided
  if (is.null(data) && (is.null(truth_data) | is.null(forecasts))) {
    stop("need arguments 'data' in function 'eval_forecasts()', or alternatively 'forecasts' and 'truth_data'")
  }

  if (is.null(data)) {
    data <- merge_pred_and_obs(forecasts, truth_data, by = merge_by, join = "full")
    if (nrow(data) == 0) {
      if (verbose) {
        warning("After attempting to merge, only an empty data.table was left")
      }
      return(data)
    }
  }

  # split truth data and forecasts in order to apply different filtering
  truth_data <- data.table::as.data.table(data)[!is.na(true_value)]
  forecasts <- data.table::as.data.table(data)[!is.na(prediction)]


  # function to filter forecast and truth data
  filter_df <- function(data, filter_list) {
    data <- data.table::copy(data)
    # filter as specified by the user
    for (expr in filter_list) {
      data <- data[eval(parse(text = expr)), ]
    }
    return(data)
  }

  truth_data <- filter_df(truth_data, c(filter_both, filter_truth))
  forecasts <- filter_df(forecasts, c(filter_both, filter_forecasts))

  # if specificed, get all combinations of the facet variables present in the
  # forecasts and filter the truth_data accordingly
  if (!allow_truth_without_pred && !is.null(facet_formula)) {
    facet_vars <- all.vars(facet_formula)
    index <- colnames(forecasts)[(colnames(forecasts) %in% facet_vars)]
    combinations_forecasts <- unique(data.table::copy(forecasts)[, ..index])
    data.table::setkey(combinations_forecasts)
    data.table::setkey(truth_data)

    # keep part where predictions are na so they don't get removed by merging
    truth_without_pred <- truth_data[is.na(prediction)]
    truth_data <- merge(truth_data, combinations_forecasts)
    # add back together
    truth_data <- data.table::rbindlist(list(truth_without_pred, truth_data),
                                        use.names = TRUE)
  }

  # delete certain columns that denominate the forecaster from the truth data
  truth_data <- delete_columns(truth_data, remove_from_truth)

  # find out what type of predictions we have. convert sample based to
  # range data
  colnames <- colnames(forecasts)
  if ("sample" %in% colnames) {
    forecasts <- scoringutils::sample_to_range_long(forecasts,
                                                    keep_quantile_col = FALSE)
  } else if ("quantile" %in% colnames) {
    forecasts <- scoringutils::quantile_to_range_long(forecasts,
                                                      keep_quantile_col = FALSE)
  }

  # select appropriate boundaries and pivot wider
  select <- forecasts$range %in% setdiff(range, 0)
  intervals <- forecasts[select, ]

  # delete quantile column in intervals if present. This is important for
  # pivoting
  if ("quantile" %in% names(intervals)) {
    intervals[, quantile := NULL]
  }

  # if there isn't any data to plot, return NULL
  if (nrow(intervals) == 0) {
    return(NULL)
  }

  # pivot wider and convert range to a factor
  intervals <- data.table::dcast(intervals, ... ~ boundary,
                                 value.var = "prediction")
  intervals[, range := as.factor(range)]

  # plot prediciton rnages
  plot <- ggplot2::ggplot(intervals, ggplot2::aes(x = !!ggplot2::sym(x))) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = lower, ymax = upper,
                                      group = range, fill = range),
                         alpha = 0.4) +
    ggplot2::scale_colour_manual("",values = c("black", "steelblue4")) +
    ggplot2::scale_fill_manual("range", values = c("steelblue3",
                                                   "lightskyblue3",
                                                   "lightskyblue2",
                                                   "lightskyblue1")) +
    ggplot2::theme_light()

  # add median in a different colour
  if (0 %in% range) {
    select_median <- (forecasts$range %in% 0 & forecasts$boundary == "lower")
    median <- forecasts[select_median]

    plot <- plot +
      ggplot2::geom_line(data = median,
                         mapping = ggplot2::aes(y = prediction, colour = "median"),
                         lwd = 0.4)
  }


  # facet if specified by the user
  if (!is.null(facet_formula)) {
    if (facet_wrap_or_grid == "facet_wrap") {
      plot <- plot +
        ggplot2::facet_wrap(facet_formula, scales = scales, ncol = ncol)
    } else {
      plot <- plot +
        ggplot2::facet_grid(facet_formula, scales = scales)
    }
  }

  # add true_values
  if (nrow(truth_data) > 0) {
    plot <- plot +
      ggplot2::labs(x = xlab, y = ylab)

    plot <- plot +
      ggplot2::geom_point(data = truth_data,
                          ggplot2::aes(y = true_value, colour = "actual"),
                          size = 0.5) +
      ggplot2::geom_line(data = truth_data,
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
#' example1 <- scoringutils::range_example_data_long
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
    ggplot2::theme_light() +
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
#' example1 <- scoringutils::range_example_data_long
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
    ggplot2::theme_light() +
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












#' @title Visualise Where Forecasts Are Available
#'
#' @description
#' Visualise Where Forecasts Are Available
#'
#' @param data data.frame with predictions in the same format required for
#' \code{\link{eval_forecasts}}
#' @param y character vector of length one that denotes the name of the column
#' to appear on the y-axis of the plot
#' @param x character vector of length one that denotes the name of the column
#' to appear on the x-axis of the plot
#' @param make_x_factor logical (default is TRUE). Whether or not to convert
#' the variable on the x-axis to a factor. This has an effect e.g. if dates
#' are shown on the x-axis.
#' @param summarise_by character vector or \code{NULL} (the default) that
#' denotes the categories over which the number of forecasts should be summed
#' up. By default (i.e. \code{summarise_by = NULL}) this will be all the
#' columns that appear in either x, y, or the facetting formula.
#' @param collapse_to_one logical. If \code{TRUE}) (the default), everything
#' not included in \code{by} will be counted only once. This is useful, for
#' example, if you don't want to count every single sample or quantile, but
#' instead treat one set of samples or quantiles as one forecast.
#' @param by character vector or \code{NULL} (the default) that denotes the
#' unit of an individual forecast. This argument behaves similarly to the
#' \code{by} argument in \code{link{eval_forecasts}}. By default, all columns
#' are used that are not part of any internally protected columns like "sample"
#' or "prediction" or similar. The \code{by} argument is only necessary if
#' \code{collapse_to_one = TRUE} to indicate which rows not to collapse to one.
#' @param show_numbers logical (default is \code{TRUE}) that indicates whether
#' or not to show the actual count numbers on the plot
#' @param facet_formula formula for facetting in ggplot. If this is \code{NULL}
#' (the default), no facetting will take place
#' @param facet_wrap_or_grid character. Use ggplot2's \code{facet_wrap} or
#' \code{facet_grid}? Anything other than "facet_wrap" will be interpreted as
#' \code{facet_grid}. This only takes effect if \code{facet_formula} is not
#' \code{NULL}
#' @param scales character. The scales argument gets passed down to ggplot.
#' Only necessary
#' if you make use of facetting. Default is "fixed"
#' @param legend_position character that indicates where to put the legend.
#' The argument gets passed to ggplot2. By default ("none"), no legend is shown.
#' @return ggplot object with a plot of interval coverage
#' @importFrom ggplot2 ggplot scale_colour_manual scale_fill_manual
#' facet_wrap facet_grid
#' @importFrom data.table dcast .I .N
#' @export
#'
#' @examples
#' example1 <- scoringutils::range_example_data_long
#' show_avail_forecasts(example1, x = "value_date", facet_formula = ~ value_desc)

show_avail_forecasts <- function(data,
                                 y = "model",
                                 x = "forecast_date",
                                 make_x_factor = TRUE,
                                 summarise_by = NULL,
                                 collapse_to_one = TRUE,
                                 by = NULL,
                                 show_numbers = TRUE,
                                 facet_formula = NULL,
                                 facet_wrap_or_grid = "facet_wrap",
                                 scales = "fixed",
                                 legend_position = "none") {

  data <- data.table::as.data.table(data)

  if (is.null(summarise_by)) {
    facet_vars <- all.vars(facet_formula)
    summarise_by <- unique(c(x, y, facet_vars))
  }

  data <- data[!is.na(prediction),]

  if (collapse_to_one) {
    # only count one forecast per group in by
    # this e.g. makes sure that quantiles and samples are not counted
    # multiple times
    if (is.null(by)) {
      protected_columns <- c("prediction", "true_value", "sample", "quantile",
                             "range", "boundary")
      by <- setdiff(colnames(data), protected_columns)
    }
    data <- data[data[, .I[1], by = by]$V1]
  }

  # count items per group in summarise_by
  df <- data[, .(n_obs = .N), by = summarise_by]

  if (make_x_factor) {
    df[, eval(x) := as.factor(get(x))]
  }

  plot <- ggplot2::ggplot(df,
                          ggplot2::aes_string(y = y, x = x)) +
    ggplot2::geom_tile(ggplot2::aes(fill = n_obs),
                       width = 0.97, height = 0.97) +
    ggplot2::scale_fill_gradient(low = "grey95", high = "steelblue",
                                 na.value = "lightgrey") +
    ggplot2::theme_light() +
    ggplot2::theme(panel.grid.major.x = ggplot2::element_blank(),
                   panel.grid.minor.x = ggplot2::element_blank(),
                   legend.position = legend_position,
                   axis.text.x = ggplot2::element_text(angle = 90, vjust = 1,
                                                       hjust=1)) +
    ggplot2::theme(panel.spacing = ggplot2::unit(2, "lines"))

  if (show_numbers) {
    plot <- plot +
      ggplot2::geom_text(ggplot2::aes(label = n_obs))
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

  return(plot)
}


