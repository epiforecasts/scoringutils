#' @title Plot Coloured Score Table
#'
#' @description
#' Plots a coloured table of summarised scores obtained using
#' [score()].
#'
#' @param y the variable to be shown on the y-axis. Instead of a single character string,
#' you can also specify a vector with column names, e.g.
#' `y = c("model", "location")`. These column names will be concatenated
#' to create a unique row identifier (e.g. "model1_location1").
#' @param by A character vector that determines how the colour shading for the
#' plot gets computed. By default (`NULL`), shading will be determined per
#' metric, but you can provide additional column names (see examples).
#' @param metrics A character vector with the metrics to show. If set to
#' `NULL` (default), all metrics present in `scores` will be shown.
#'
#' @return A ggplot2 object with a coloured table of summarised scores
#' @inheritParams pairwise_comparison
#' @importFrom ggplot2 ggplot aes element_blank element_text labs coord_cartesian coord_flip
#' @importFrom data.table setDT melt
#' @importFrom stats sd
#' @export
#'
#' @examples
#' library(ggplot2)
#' library(magrittr) # pipe operator
#'
#' scores <- score(example_quantile) %>%
#'   summarise_scores(by = c("model", "target_type")) %>%
#'   summarise_scores(fun = signif, digits = 2)
#'
#' plot_score_table(scores, y = "model", by = "target_type") +
#'   facet_wrap(~target_type, ncol = 1)
#'
#' # can also put target description on the y-axis
#' plot_score_table(scores,
#'                  y = c("model", "target_type"),
#'                  by = "target_type")

plot_score_table <- function(scores,
                             y = "model",
                             by = NULL,
                             metrics = NULL) {

  # identify metrics -----------------------------------------------------------
  id_vars <- get_forecast_unit(scores)
  if (is.null(metrics)) {
    metrics <- names(scores)[names(scores) %in% available_metrics()]
  }

  scores <- delete_columns(
    scores,
    names(scores)[!(names(scores) %in% c(metrics, id_vars))]
  )

  # compute scaled values ------------------------------------------------------
  # scaling is done in order to colour the different scores
  # for most metrics larger is worse, but others like bias are better if they
  # are close to zero and deviations in both directions are bad

  # define which metrics are scaled using min (larger is worse) and
  # which not (metrics like bias where deviations in both directions are bad)
  metrics_zero_good <- c("bias", "coverage_deviation")
  metrics_no_color <- c("coverage")

  metrics_min_good <- setdiff(metrics, c(
    metrics_zero_good, metrics_no_color
  ))

  # write scale functions that can be used in data.table
  scale <- function(x) {
    scaled <- x / sd(x, na.rm = TRUE)
    return(scaled)
  }
  scale_min_good <- function(x) {
    scaled <- (x - min(x)) / sd(x, na.rm = TRUE)
    return(scaled)
  }

  # pivot longer and add scaled values
  df <- data.table::melt(scores,
    value.vars = metrics,
    id.vars = id_vars,
    variable.name = "metric"
  )

  df[metric %in% metrics_min_good, value_scaled := scale_min_good(value),
    by = c("metric", by)
  ]
  df[metric %in% metrics_zero_good, value_scaled := scale(value),
    by = c("metric", by)
  ]
  df[metric %in% metrics_no_color, value_scaled := 0,
    by = c("metric", by)
  ]

  # create identifier column for plot ------------------------------------------
  # if there is only one column, leave column as is. Reason to do that is that
  # users can then pass in a factor and keep the ordering of that column intact
  if (length(y) > 1) {
    df[, identifCol := do.call(paste, c(.SD, sep = "_")),
       .SDcols = y[y %in% names(df)]
    ]
  } else {
    setnames(df, old = eval(y), new = "identifCol")
  }

  # plot -----------------------------------------------------------------------
  # make plot with all metrics that are not NA
  plot <- ggplot(
    df[!is.na(value), ],
    aes(y = identifCol, x = metric)
  ) +
    geom_tile(aes(fill = value_scaled), colour = "white", show.legend = FALSE) +
    geom_text(aes(y = identifCol, label = value)) +
    scale_fill_gradient2(low = "steelblue", high = "salmon") +
    theme_scoringutils() +
    theme(
      legend.title = element_blank(),
      legend.position = "none",
      axis.text.x = element_text(
        angle = 90, vjust = 1,
        hjust = 1
      )
    ) +
    labs(x = "", y = "") +
    coord_cartesian(expand = FALSE)

  return(plot)
}

#' @title Plot Contributions to the Weighted Interval Score
#'
#' @description
#' Visualise the components of the weighted interval score: penalties for
#' over-prediction, under-prediction and for high dispersion (lack of sharpness)
#'
#' @param scores A data.frame of scores based on quantile forecasts as
#' produced by [score()] and summarised using [summarise_scores()]
#' @param x The variable from the scores you want to show on the x-Axis.
#' Usually this will be "model".
#' @param relative_contributions show relative contributions instead of absolute
#' contributions. Default is FALSE and this functionality is not available yet.
#' @param flip boolean (default is `FALSE`), whether or not to flip the axes.
#' @return A ggplot2 object showing a contributions from the three components of
#' the weighted interval score
#' @importFrom ggplot2 ggplot aes geom_linerange facet_wrap labs
#' scale_fill_discrete
#' theme theme_light unit guides guide_legend .data
#' @export
#' @examples
#' library(ggplot2)
#' scores <- score(example_quantile)
#' scores <- summarise_scores(scores, by = c("model", "target_type"))
#'
#' plot_wis(scores,
#'   x = "model",
#'   relative_contributions = TRUE
#' ) +
#'   facet_wrap(~target_type)
#' plot_wis(scores,
#'   x = "model",
#'   relative_contributions = FALSE
#' ) +
#'   facet_wrap(~target_type, scales = "free_x")
#' @references
#' Bracher J, Ray E, Gneiting T, Reich, N (2020) Evaluating epidemic forecasts
#' in an interval format. <https://journals.plos.org/ploscompbiol/article?id=10.1371/journal.pcbi.1008618>

plot_wis <- function(scores,
                     x = "model",
                     relative_contributions = FALSE,
                     flip = FALSE) {
  scores <- data.table::as.data.table(scores)

  scores <- data.table::melt(scores,
    measure.vars = c(
      "overprediction",
      "underprediction",
      "dispersion"
    ),
    variable.name = "wis_component_name",
    value.name = "component_value"
  )

  # stack or fill the geom_col position
  col_position <- ifelse(relative_contributions, "fill", "stack")

  plot <- ggplot(scores, aes(y = .data[[x]])) +
    geom_col(
      position = col_position,
      aes(x = component_value, fill = wis_component_name)
    ) +
    theme_scoringutils() +
    scale_fill_discrete(type = c("#DF536B", "#61D04F", "#2297E6")) +
    guides(fill = guide_legend(title = "WIS component")) +
    xlab("WIS contributions")

  if (flip) {
    plot <- plot +
      theme(
        panel.spacing = unit(4, "mm"),
        axis.text.x = element_text(
          angle = 90,
          vjust = 1,
          hjust = 1
        )
      ) +
      coord_flip()
  }

  return(plot)
}

#' @title Plot Metrics by Range of the Prediction Interval
#'
#' @description
#' Visualise the metrics by range, e.g. if you are interested how different
#' interval ranges contribute to the overall interval score, or how
#' sharpness / dispersion changes by range.
#'
#' @param scores A data.frame of scores based on quantile forecasts as
#' produced by [score()] or [summarise_scores()]. Note that "range" must be included
#' in the `by` argument when running [summarise_scores()]
#' @param y The variable from the scores you want to show on the y-Axis.
#' This could be something like "interval_score" (the default) or "dispersion"
#' @param x The variable from the scores you want to show on the x-Axis.
#' Usually this will be "model"
#' @param colour Character vector of length one used to determine a variable
#' for colouring dots. The Default is "range".
#' @return A ggplot2 object showing a contributions from the three components of
#' the weighted interval score
#' @importFrom ggplot2 ggplot aes aes geom_point geom_line
#' expand_limits theme theme_light element_text scale_color_continuous labs
#' @export
#' @examples
#' library(ggplot2)
#' scores <- score(example_quantile)
#' scores <- summarise_scores(scores, by = c("model", "target_type", "range"))
#'
#' plot_ranges(scores, x = "model") +
#'   facet_wrap(~target_type, scales = "free")
#'
#' # visualise dispersion instead of interval score
#' plot_ranges(scores, y = "dispersion", x = "model") +
#'   facet_wrap(~target_type)

plot_ranges <- function(scores,
                        y = "interval_score",
                        x = "model",
                        colour = "range") {
  plot <- ggplot(
    scores,
    aes(
      x = .data[[x]],
      y = .data[[y]],
      colour = .data[[colour]]
    )
  ) +
    geom_point(size = 2) +
    geom_line(aes(group = range),
      colour = "black",
      size = 0.01
    ) +
    scale_color_continuous(low = "steelblue", high = "salmon") +
    theme_scoringutils() +
    expand_limits(y = 0) +
    theme(
      legend.position = "right",
      axis.text.x = element_text(
        angle = 90, vjust = 1,
        hjust = 1
      )
    )

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
#' produced by [score()].
#' @param y The variable from the scores you want to show on the y-Axis. The
#' default for this is "model"
#' @param x The variable from the scores you want to show on the x-Axis. This
#' could be something like "horizon", or "location"
#' @param metric the metric that determines the value and colour shown in the
#' tiles of the heatmap
#' @return A ggplot2 object showing a heatmap of the desired metric
#' @importFrom data.table setDT `:=`
#' @importFrom ggplot2 ggplot  aes geom_tile geom_text .data
#' scale_fill_gradient2 labs element_text coord_cartesian
#' @export
#' @examples
#' scores <- score(example_quantile)
#' scores <- summarise_scores(scores, by = c("model", "target_type", "range"))
#'
#' plot_heatmap(scores, x = "target_type", metric = "bias")

plot_heatmap <- function(scores,
                         y = "model",
                         x,
                         metric) {
  data.table::setDT(scores)

  scores[, eval(metric) := round(get(metric), 2)]

  plot <- ggplot(
    scores,
    aes(
      y = .data[[y]],
      x = .data[[x]],
      fill = .data[[metric]]
    )
  ) +
    geom_tile() +
    geom_text(aes(label = .data[[metric]])) +
    scale_fill_gradient2(low = "steelblue", high = "salmon") +
    theme_scoringutils() +
    theme(axis.text.x = element_text(
      angle = 90, vjust = 1,
      hjust = 1
    )) +
    coord_cartesian(expand = FALSE)

  return(plot)
}

#' @title Plot Predictions vs True Values
#'
#' @description
#' Make a plot of observed and predicted values
#'
#' @param data a data.frame that follows the same specifications outlined in
#' [score()]. To customise your plotting, you can filter your data using the
#' function [make_NA()].
#' @param by character vector with column names that denote categories by which
#' the plot should be stratified. If for example you want to have a facetted
#' plot, this should be a character vector with the columns used in facetting
#' (note that the facetting still needs to be done outside of the function call)
#' @param x character vector of length one that denotes the name of the variable
#' @param range numeric vector indicating the interval ranges to plot. If 0 is
#' included in range, the median prediction will be shown.
#' @return ggplot object with a plot of true vs predicted values
#' @importFrom ggplot2 ggplot scale_colour_manual scale_fill_manual theme_light
#' @importFrom ggplot2 facet_wrap facet_grid aes geom_line .data
#' @importFrom data.table dcast
#' @importFrom ggdist geom_lineribbon
#' @export
#' @examples
#' library(ggplot2)
#' library(magrittr)
#'
#' example_continuous %>%
#'   make_NA (
#'     what = "truth",
#'     target_end_date >= "2021-07-22",
#'     target_end_date < "2021-05-01"
#'   ) %>%
#'   make_NA (
#'     what = "forecast",
#'     model != 'EuroCOVIDhub-ensemble',
#'     forecast_date != "2021-06-07"
#'   ) %>%
#'   plot_predictions (
#'     x = "target_end_date",
#'     by = c("target_type", "location"),
#'     range = c(0, 50, 90, 95)
#'   ) +
#'   facet_wrap(~ location + target_type, scales = "free_y") +
#'   aes(fill = model, color = model)
#'
#' example_continuous %>%
#'   make_NA (
#'     what = "truth",
#'     target_end_date >= "2021-07-22",
#'     target_end_date < "2021-05-01"
#'   ) %>%
#'   make_NA (
#'     what = "forecast",
#'     forecast_date != "2021-06-07"
#'   ) %>%
#'   plot_predictions (
#'     x = "target_end_date",
#'     by = c("target_type", "location"),
#'     range = c(0)
#'   ) +
#'   facet_wrap(~ location + target_type, scales = "free_y") +
#'   aes(fill = model, color = model)

plot_predictions <- function(data,
                             by = NULL,
                             x = "date",
                             range = c(0, 50, 90)) {

  # split truth data and forecasts in order to apply different filtering
  truth_data <- data.table::as.data.table(data)[!is.na(true_value)]
  forecasts <- data.table::as.data.table(data)[!is.na(prediction)]

  del_cols <-
    colnames(truth_data)[!(colnames(truth_data) %in% c(by, "true_value", x))]

  truth_data <- delete_columns(
    truth_data,
    del_cols,
    make_unique = TRUE
  )

  # find out what type of predictions we have. convert sample based to
  # range data
  prediction_type <- get_prediction_type(data)
  if (prediction_type %in% c("integer", "continuous")) {
    forecasts <- sample_to_range_long(forecasts,
      range = range,
      keep_quantile_col = FALSE
    )
  } else if (prediction_type == "quantile") {
    forecasts <- quantile_to_range_long(forecasts,
      keep_quantile_col = FALSE
    )
  }

  # select appropriate boundaries and pivot wider
  select <- forecasts$range %in% setdiff(range, 0)
  intervals <- forecasts[select, ]

  # delete quantile column in intervals if present. This is important for
  # pivoting
  if ("quantile" %in% names(intervals)) {
    intervals[, quantile := NULL]
  }

  plot <- ggplot(data = data, aes(x = .data[[x]])) +
    theme_scoringutils() +
    ylab("True and predicted values")

  if (nrow(intervals) != 0) {
    # pivot wider and convert range to a factor
    intervals <- data.table::dcast(intervals, ... ~ boundary,
                                   value.var = "prediction")

    # only plot ranges if there are ranges to plot
    plot <- plot +
      ggdist::geom_lineribbon(
        data = intervals,
        aes(
          ymin = lower, ymax = upper,
          # We use the fill_ramp aesthetic for this instead of the default fill
          # because we want to keep fill to be able to use it for other
          # variables
          fill_ramp = factor(range, levels = sort(unique(range), decreasing = TRUE))
        ),
        lwd = 0.4
      ) +
      ggdist::scale_fill_ramp_discrete(
        name = "range",
        # range arguemnt was added to make sure that the line for the median
        # and the ribbon don't have the same opacity, making the line
        # invisible
        range = c(0.15, 0.75)
      )
  }

  # We could treat this step as part of ggdist::geom_lineribbon() but we treat
  # it separately here to deal with the case when only the median is provided
  # (in which case ggdist::geom_lineribbon() will fail)
  if (0 %in% range) {
    select_median <- (forecasts$range %in% 0 & forecasts$boundary == "lower")
    median <- forecasts[select_median]

    if (nrow(median) > 0) {
      plot <- plot +
        geom_line(
          data = median,
          mapping = aes(y = prediction),
          lwd = 0.4
        )
    }
  }

  # add true_values
  if (nrow(truth_data) > 0) {
    plot <- plot +
      geom_point(
        data = truth_data,
        show.legend = FALSE,
        inherit.aes = FALSE,
        aes(x = .data[[x]], y = true_value),
        color = "black",
        size = 0.5
      ) +
      geom_line(
        data = truth_data,
        inherit.aes = FALSE,
        show.legend = FALSE,
        aes(x = .data[[x]], y = true_value),
        linetype = 1,
        color = "grey40",
        lwd = 0.2
      )
  }

  return(plot)
}

#' @title Make Rows NA in Data for Plotting
#'
#' @description
#' Filters the data and turns values into `NA` before the data gets passed to
#' [plot_predictions()]. The reason to do this is to this is that it allows to
#' 'filter' prediction and truth data separately. Any value that is NA will then
#' be removed in the subsequent call to [plot_predictions()].
#'
#' @inheritParams score
#' @param what character vector that determines which values should be turned
#' into `NA`. If `what = "truth"`, values in the column 'true_value' will be
#' turned into `NA`. If `what = "forecast"`, values in the column 'prediction'
#' will be turned into `NA`. If `what = "both"`, values in both column will be
#' turned into `NA`.
#' @param ... logical statements used to filter the data
#' @return A data.table
#' @importFrom rlang enexprs
#' @keywords plotting
#' @export
#'
#' @examples
#' make_NA (
#'     example_continuous,
#'     what = "truth",
#'     target_end_date >= "2021-07-22",
#'     target_end_date < "2021-05-01"
#'   )

make_NA <- function(data = NULL,
                    what = c("truth", "forecast", "both"),
                    ...) {

  check_not_null(data = data)

  data <- data.table::copy(data)
  what <- match.arg(what)

  # turn ... arguments into expressions
  args <- enexprs(...)

  vars <- c()
  if (what %in% c("forecast", "both")) {
    vars <- c(vars, "prediction")
  }
  if (what %in% c("truth", "both")) {
    vars <- c(vars, "true_value")
  }
  for (expr in args) {
    data <- data[eval(expr), eval(vars) := NA_real_]
  }
  return(data[])
}

#' @rdname make_NA
#' @keywords plotting
#' @export
make_na <- make_NA

#' @title Plot Interval Coverage
#'
#' @description
#' Plot interval coverage
#'
#' @param scores A data.frame of scores based on quantile forecasts as
#' produced by [score()] or [summarise_scores()]. Note that "range" must be included
#' in the `by` argument when running [summarise_scores()]
#' @param colour According to which variable shall the graphs be coloured?
#' Default is "model".
#' @return ggplot object with a plot of interval coverage
#' @importFrom ggplot2 ggplot scale_colour_manual scale_fill_manual .data
#' facet_wrap facet_grid geom_polygon
#' @importFrom data.table dcast
#' @export
#' @examples
#' library("scoringutils")
#' scores <- score(example_quantile)
#' scores <- summarise_scores(scores, by = c("model", "range"))
#' plot_interval_coverage(scores)

plot_interval_coverage <- function(scores,
                                   colour = "model") {
  ## overall model calibration - empirical interval coverage
  p1 <- ggplot(scores, aes(
    x = range,
    colour = .data[[colour]]
  )) +
    geom_polygon(
      data = data.frame(
        x = c(0, 0, 100),
        y = c(0, 100, 100),
        g = c("o", "o", "o")
      ),
      aes(
        x = x, y = y, group = g,
        fill = g
      ),
      alpha = 0.05,
      colour = "white",
      fill = "olivedrab3"
    ) +
    geom_line(aes(y = range),
      colour = "grey",
      linetype = "dashed"
    ) +
    geom_line(aes(y = coverage * 100)) +
    theme_scoringutils() +
    ylab("% Obs inside interval") +
    xlab("Nominal interval coverage") +
    coord_cartesian(expand = FALSE)

  return(p1)
}

#' @title Plot Quantile Coverage
#'
#' @description
#' Plot quantile coverage
#'
#' @param scores A data.frame of scores based on quantile forecasts as
#' produced by [score()] or [summarise_scores()]. Note that "range" must be included
#' in the `by` argument when running [summarise_scores()]
#' @param colour According to which variable shall the graphs be coloured?
#' Default is "model".
#' @return ggplot object with a plot of interval coverage
#' @importFrom ggplot2 ggplot scale_colour_manual scale_fill_manual .data aes
#' scale_y_continuous
#' @importFrom data.table dcast
#' @export
#' @examples
#' scores <- score(example_quantile)
#' scores <- summarise_scores(scores, by = c("model", "quantile"))
#' plot_quantile_coverage(scores)

plot_quantile_coverage <- function(scores,
                                   colour = "model") {
  p2 <- ggplot(
    data = scores,
    aes(x = quantile, colour = .data[[colour]])
  ) +
    geom_polygon(
      data = data.frame(
        x = c(
          0, 0.5, 0.5,
          0.5, 0.5, 1
        ),
        y = c(
          0, 0, 0.5,
          0.5, 1, 1
        ),
        g = c("o", "o", "o")
      ),
      aes(
        x = x, y = y, group = g,
        fill = g
      ),
      alpha = 0.05,
      colour = "white",
      fill = "olivedrab3"
    ) +
    geom_line(aes(y = quantile),
      colour = "grey",
      linetype = "dashed"
    ) +
    geom_line(aes(y = quantile_coverage)) +
    theme_scoringutils() +
    xlab("Quantile") +
    ylab("% Obs below quantile") +
    scale_y_continuous(labels = function(x) {paste(100 * x)}) +
    coord_cartesian(expand = FALSE)

  return(p2)
}

#' @title Plot Heatmap of Pairwise Comparisons
#'
#' @description
#' Creates a heatmap of the ratios or pvalues from a pairwise comparison
#' between models
#'
#' @param comparison_result A data.frame as produced by
#' [pairwise_comparison()]
#' @param type character vector of length one that is either
#'  "mean_scores_ratio", "pval", or "together". This denotes whether to
#' visualise the ratio or the p-value of the pairwise comparison or both.
#' Default is "mean_scores_ratio".
#' @param smaller_is_good logical (default is `TRUE`) that indicates whether
#' smaller or larger values are to be interpreted as 'good' (as you could just
#' invert the mean scores ratio). This option is not supported when type =
#' "pval"
#' @importFrom ggplot2 ggplot aes geom_tile geom_text labs coord_cartesian
#' scale_fill_gradient2 theme_light element_text
#' @importFrom data.table as.data.table setnames rbindlist
#' @importFrom stats reorder
#' @importFrom ggplot2 labs coord_cartesian facet_wrap facet_grid theme
#' element_text element_blank
#' @export
#' @examples
#' library(ggplot2)
#' df <- data.frame(
#'   model = rep(c("model1", "model2", "model3"), each = 10),
#'   id = rep(1:10),
#'   interval_score = abs(rnorm(30, mean = rep(c(1, 1.3, 2), each = 10))),
#'   ae_median = (abs(rnorm(30)))
#' )
#'
#' scores <- score(example_quantile)
#' pairwise <- pairwise_comparison(scores, by = "target_type")
#' plot_pairwise_comparison(pairwise) +
#'   facet_wrap(~target_type)

plot_pairwise_comparison <- function(comparison_result,
                                     type = c("mean_scores_ratio", "pval", "together"),
                                     smaller_is_good = TRUE) {
  comparison_result <- data.table::as.data.table(comparison_result)

  comparison_result[, model := reorder(model, -relative_skill)]
  levels <- levels(comparison_result$model)


  get_fill_scale <- function(values, breaks, plot_scales) {
    values[is.na(values)] <- 1 # this would be either ratio = 1 or pval = 1
    scale <- cut(values,
      breaks = breaks,
      include.lowest = TRUE,
      right = FALSE,
      labels = plot_scales
    )
    # scale[is.na(scale)] <- 0
    return(as.numeric(as.character(scale)))
  }

  type <- match.arg(type)

  if (type == "together") {
    # obtain only the upper triangle of the comparison
    # that is used for showing ratios
    # need to change the order if larger is good
    if (smaller_is_good) {
      unique_comb <- as.data.frame(t(combn(rev(levels), 2)))
    } else {
      unique_comb <- as.data.frame(t(combn((levels), 2)))
    }

    colnames(unique_comb) <- c("model", "compare_against")
    upper_triangle <- merge(comparison_result, unique_comb)

    # change levels for plotting order
    upper_triangle[, `:=`(
      model = factor(model, levels),
      compare_against = factor(compare_against, levels)
    )]

    # reverse y and x if larger is better
    if (!smaller_is_good) {
      data.table::setnames(
        upper_triangle,
        c("model", "compare_against"),
        c("compare_against", "model")
      )
    }

    # modify upper triangle ------------------------------------------------------
    # add columns where a model is compared with itself. make adj_pval NA
    # to plot it as grey later on
    equal <- data.table::data.table(
      model = levels,
      compare_against = levels,
      mean_scores_ratio = 1,
      pval = NA,
      adj_pval = NA
    )
    upper_triangle_complete <- data.table::rbindlist(list(
      upper_triangle,
      equal
    ), fill = TRUE)

    # define interest variable
    upper_triangle_complete[, var_of_interest := round(mean_scores_ratio, 2)]

    # implemnt breaks for colour heatmap
    breaks <- c(0, 0.1, 0.5, 0.75, 1, 1.33, 2, 10, Inf)
    plot_scales <- c(-1, -0.5, -0.25, 0, 0, 0.25, 0.5, 1)
    if (!smaller_is_good) {
      plot_scales <- rev(plot_scales)
    }
    upper_triangle_complete[, fill_col := get_fill_scale(
      var_of_interest,
      breaks, plot_scales
    )]

    # create mean_scores_ratios in plot
    plot <- ggplot(
      upper_triangle_complete,
      aes(
        x = compare_against,
        y = model,
        fill = fill_col
      )
    ) +
      geom_tile(width = 0.98, height = 0.98) +
      geom_text(aes(label = var_of_interest),
        na.rm = TRUE
      ) +
      scale_fill_gradient2(
        low = "steelblue", mid = "grey95",
        high = "salmon",
        na.value = "lightgrey",
        midpoint = 0,
        limits = c(-1, 1),
        name = NULL
      ) +
      theme_scoringutils() +
      theme(
        axis.text.x = element_text(
          angle = 90, vjust = 1,
          hjust = 1, color = "brown4"
        ),
        axis.text.y = element_text(color = "steelblue4"),
        legend.position = "none"
      ) +
      labs(
        x = "", y = "",
        title = "Pairwise comparisons - mean_scores_ratio (upper) and pval (lower)"
      ) +
      coord_cartesian(expand = FALSE)

    # add pvalues to plot --------------------------------------------------------
    # obtain lower triangle for the pvalues
    lower_triangle <- data.table::copy(upper_triangle)
    data.table::setnames(
      lower_triangle,
      c("model", "compare_against"),
      c("compare_against", "model")
    )

    lower_triangle[, var_of_interest := round(adj_pval, 3)]
    # implemnt breaks for colour heatmap
    breaks <- c(0, 0.01, 0.05, 0.1, 1)
    plot_scales <- c(0.8, 0.5, 0.1, 0.000001)
    lower_triangle[, fill_col := get_fill_scale(
      var_of_interest,
      breaks, plot_scales
    )]

    fill_rule <- ifelse(
      lower_triangle$fill_col == 0.000001, "grey95", "palegreen3"
    )
    lower_triangle[, var_of_interest := as.character(var_of_interest)]
    lower_triangle[, var_of_interest := ifelse(var_of_interest == "0",
      "< 0.001", var_of_interest
    )]

    plot <- plot +
      geom_tile(
        data = lower_triangle,
        aes(alpha = fill_col),
        fill = fill_rule,
        color = "white",
        width = 0.97, height = 0.97
      ) +
      geom_text(
        data = lower_triangle,
        aes(label = var_of_interest),
        na.rm = TRUE
      )
  } else{
    if (type == "mean_scores_ratio") {
      comparison_result[, var_of_interest := round(mean_scores_ratio, 2)]

      # implemnt breaks for colour heatmap
      breaks <- c(0, 0.1, 0.5, 0.75, 1, 1.33, 2, 10, Inf)
      plot_scales <- c(-1, -0.5, -0.25, 0, 0, 0.25, 0.5, 1)
      comparison_result[, fill_col := get_fill_scale(
        var_of_interest,
        breaks, plot_scales
      )]

      high_col <- "salmon"
    } else {
      if (!smaller_is_good) {
        stop("smaller_is_good is the only supported option with type pval")
      }
      comparison_result[, var_of_interest := round(pval, 3)]
      # implemnt breaks for colour heatmap
      breaks <- c(0, 0.01, 0.05, 0.1, 1)
      plot_scales <- c(1, 0.5, 0.1, 0)
      comparison_result[, fill_col := get_fill_scale(
        var_of_interest,
        breaks, plot_scales
      )]

      high_col <- "palegreen3"
      comparison_result[, var_of_interest := as.character(var_of_interest)]
      comparison_result[, var_of_interest := ifelse(var_of_interest == "0",
        "< 0.001", var_of_interest
      )]
    }
  plot <- ggplot(
    comparison_result,
    aes(
      y = reorder(model, 1 / mean_scores_ratio, FUN = geom_mean_helper),
      x = reorder(compare_against, mean_scores_ratio, FUN = geom_mean_helper),
      fill = fill_col
    )
  ) +
    geom_tile(
      color = "white",
      width = 0.97, height = 0.97
    ) +
    geom_text(aes(label = var_of_interest),
      na.rm = TRUE
    ) +
    scale_fill_gradient2(
      low = "steelblue", mid = "grey95",
      high = high_col,
      na.value = "lightgrey",
      midpoint = 0,
      limits = c(-1, 1),
      name = NULL
    ) +
    theme_scoringutils() +
    theme(
      axis.text.x = element_text(
        angle = 90, vjust = 1,
        hjust = 1
      ),
      legend.position = "none"
    ) +
    labs(
      x = "", y = ""
    ) +
    coord_cartesian(expand = FALSE)
    if (type == "mean_scores_ratio") {
      plot <- plot +
        theme(
          axis.text.x = element_text(
            angle = 90, vjust = 1,
            hjust = 1, color = "brown4"
          ),
          axis.text.y = element_text(color = "steelblue4")
        )
    }
  }

  return(plot)
}

#' @title PIT Histogram
#'
#' @description
#' Make a simple histogram of the probability integral transformed values to
#' visually check whether a uniform distribution seems likely.
#'
#' @param pit either a vector with the PIT values of size n, or a data.frame as
#' produced by [pit()]
#' @param num_bins the number of bins in the PIT histogram, default is "auto".
#' When `num_bins == "auto"`, [plot_pit()] will either display 10 bins, or it
#' will display a bin for each available quantile in case you passed in data in
#' a quantile-based format.
#' You can control the number of bins by supplying a number. This is fine for
#' sample-based pit histograms, but may fail for quantile-based formats. In this
#' case it is preferred to supply explicit breaks points using the `breaks`
#' argument.
#' @param breaks numeric vector with the break points for the bins in the
#' PIT histogram. This is preferred when creating a PIT histogram based on
#' quantile-based data. Default is `NULL` and breaks will be determined by
#' `num_bins`.
#' @importFrom stats as.formula
#' @importFrom ggplot2 geom_col
#' @return vector with the scoring values
#' @examples
#' # PIT histogram in vector based format
#' true_values <- rnorm(30, mean = 1:30)
#' predictions <- replicate(200, rnorm(n = 30, mean = 1:30))
#' pit <- pit_sample(true_values, predictions)
#' plot_pit(pit)
#'
#' # quantile-based pit
#' pit <- pit(example_quantile, by = c("model"))
#' plot_pit(pit, breaks = seq(0.1, 1, 0.1))
#'
#' # sample-based pit
#' pit <- pit(example_integer, by = c("model"))
#' plot_pit(pit)
#' @importFrom ggplot2 ggplot aes xlab ylab geom_histogram stat theme_light
#' @export

plot_pit <- function(pit,
                     num_bins = "auto",
                     breaks = NULL) {
  if ("quantile" %in% names(pit)) {
    type <- "quantile-based"
  } else {
    type <- "sample-based"
  }

  # use breaks if explicitly given, otherwise assign based on number of bins
  if (!is.null(breaks)) {
    plot_quantiles <- breaks
  } else if (is.null(num_bins) | num_bins == "auto") {
    # automatically set number of bins
    if (type == "sample-based") {
      num_bins <- 10
      width <- 1 / num_bins
      plot_quantiles <- seq(width, 1, width)
    }
    if (type == "quantile-based") {
      plot_quantiles <- unique(pit$quantile)
    }
  } else {
    # if num_bins is explicitly given
    width <- 1 / num_bins
    plot_quantiles <- seq(width, 1, width)
  }

  # function for data.frames
  if (is.data.frame(pit)) {
    facet_cols <- get_forecast_unit(pit)
    formula <- as.formula(paste("~", paste(facet_cols, collapse = "+")))

    # quantile version
    if (type == "quantile-based") {
      if (num_bins == "auto") {
      } else {
        width <- 1 / num_bins
        plot_quantiles <- seq(width, 1, width)
      }

      if (!is.null(breaks)) {
        plot_quantiles <- breaks
      }

      hist <- ggplot(
        data = pit[quantile %in% plot_quantiles],
        aes(x = quantile, y = pit_value)
      ) +
        geom_col(position = "dodge") +
        facet_wrap(formula)
    }

    if (type == "sample-based") {
      hist <- ggplot(
        data = pit,
        aes(x = pit_value)
      ) +
        geom_histogram(aes(y = stat(count) / sum(count)),
          breaks = plot_quantiles,
          colour = "grey"
        ) +
        facet_wrap(formula)
    }
  } else {
    # non data.frame version
    hist <- ggplot(
      data = data.frame(x = pit),
      aes(x = x)
    ) +
      geom_histogram(aes(y = stat(count) / sum(count)),
        breaks = plot_quantiles,
        colour = "grey"
      )
  }

  hist <- hist +
    xlab("PIT") +
    ylab("Frequency") +
    theme_scoringutils()

  return(hist)
}

#' @title Visualise Where Forecasts Are Available
#'
#' @description
#' Visualise Where Forecasts Are Available
#'
#' @param avail_forecasts data.frame with a column called `Number forecasts` as
#' produced by [avail_forecasts()]
#' @param y character vector of length one that denotes the name of the column
#' to appear on the y-axis of the plot. Default is "model".
#' @param x character vector of length one that denotes the name of the column
#' to appear on the x-axis of the plot. Default is "forecast_date".
#' @param make_x_factor logical (default is TRUE). Whether or not to convert
#' the variable on the x-axis to a factor. This has an effect e.g. if dates
#' are shown on the x-axis.
#' @param show_numbers logical (default is `TRUE`) that indicates whether
#' or not to show the actual count numbers on the plot
#' @return ggplot object with a plot of interval coverage
#' @importFrom ggplot2 ggplot scale_colour_manual scale_fill_manual
#' geom_tile scale_fill_gradient .data
#' @importFrom data.table dcast .I .N
#' @export
#' @examples
#' library(ggplot2)
#' avail_forecasts <- avail_forecasts(
#'   example_quantile, by = c("model", "target_type", "target_end_date")
#' )
#' plot_avail_forecasts(
#'  avail_forecasts, x = "target_end_date", show_numbers = FALSE
#' ) +
#'  facet_wrap("target_type")

plot_avail_forecasts <- function(avail_forecasts,
                                 y = "model",
                                 x = "forecast_date",
                                 make_x_factor = TRUE,
                                 show_numbers = TRUE) {
  avail_forecasts <- as.data.table(avail_forecasts)

  if (make_x_factor) {
    avail_forecasts[, eval(x) := as.factor(get(x))]
  }

  plot <- ggplot(
    avail_forecasts,
    aes(y = .data[[y]], x = .data[[x]])
  ) +
    geom_tile(aes(fill = `Number forecasts`),
      width = 0.97, height = 0.97
    ) +
    scale_fill_gradient(
      low = "grey95", high = "steelblue",
      na.value = "lightgrey"
    ) +
    theme_scoringutils() +
    theme(
      axis.text.x = element_text(
        angle = 90, vjust = 1,
        hjust = 1
      )
    ) +
    theme(panel.spacing = unit(2, "lines"))

  if (show_numbers) {
    plot <- plot +
      geom_text(aes(label = `Number forecasts`))
  }

  return(plot)
}

#' @title Plot Correlation Between Metrics
#'
#' @description
#' Plots a heatmap of correlations between different metrics
#'
#' @param correlations A data.table of correlations between scores as produced
#' by [correlation()].
#' @return A ggplot2 object showing a coloured matrix of correlations
#' between metrics
#' @importFrom ggplot2 ggplot geom_tile geom_text aes scale_fill_gradient2
#' element_text labs coord_cartesian theme element_blank
#' @importFrom data.table setDT melt
#' @export
#' @examples
#' scores <- score(example_quantile)
#' correlations <- correlation(
#'  summarise_scores(scores)
#' )
#' plot_correlation(correlations)

plot_correlation <- function(correlations) {

  metrics <- names(correlations)[names(correlations) %in% available_metrics()]

  lower_triangle <- get_lower_tri(correlations[, .SD, .SDcols = metrics])
  rownames(lower_triangle) <- colnames(lower_triangle)

  # get plot data.frame
  plot_df <- data.table::as.data.table(lower_triangle)[, metric := metrics]
  plot_df <- na.omit(data.table::melt(plot_df, id.vars = "metric"))

  # refactor levels according to the metrics
  plot_df[, metric := factor(metric, levels = metrics)]
  plot_df[, variable := factor(variable, rev(metrics))]

  plot <- ggplot(plot_df, aes(
    x = variable, y = metric,
    fill = value
  )) +
    geom_tile(
      color = "white",
      width = 0.97, height = 0.97
    ) +
    geom_text(aes(y = metric, label = value)) +
    scale_fill_gradient2(
      low = "steelblue", mid = "white",
      high = "salmon",
      name = "Correlation",
      breaks = c(-1, -0.5, 0, 0.5, 1)
    ) +
    theme_scoringutils() +
    theme(
      axis.text.x = element_text(
        angle = 90, vjust = 1,
        hjust = 1
      )
    ) +
    labs(x = "", y = "") +
    coord_cartesian(expand = FALSE)
  return(plot)
}

#' @title Scoringutils ggplot2 theme
#'
#' @description
#' A theme for ggplot2 plots used in scoringutils
#' @return A ggplot2 theme
#' @importFrom ggplot2 theme theme_minimal element_line `%+replace%`
#' @keywords plotting
#' @export
theme_scoringutils <- function() {
  theme_minimal() %+replace%
    theme(axis.line = element_line(colour = "grey80"),
          axis.ticks = element_line(colour = "grey80"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          legend.position = "bottom")
}
