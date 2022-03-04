#' @title Plot Coloured Score Table
#'
#' @description
#' Plots a coloured table of summarised scores obtained using
#' [score()].
#'
#' @param y the variable to be shown on the y-axis. If `NULL` (default),
#' all columns that are not scoring metrics will be used. Alternatively,
#' you can specify a vector with column names, e.g.
#' `y = c("model", "location")`. These column names will be concatenated
#' to create a unique row identifier (e.g. "model1_location1").
#'
#' @param select_metrics A character vector with the metrics to show. If set to
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
#' scores <- score(example_quantile)
#' scores <- summarise_scores(scores, by = c("model", "target_type"))
#'
#' plot_score_table(scores, y = "model") +
#'   facet_wrap(~target_type, ncol = 1)
#'
#' # can also put target description on the y-axis
#' plot_score_table(scores, y = c("model", "target_type"))
#'
#' # yields the same result in this case
#' plot_score_table(scores)
#'
#' scores <- score(example_integer)
#' scores <- summarise_scores(scores, by = c("model", "target_type"))
#'
#' # only show selected metrics
#' plot_score_table(scores,
#'   y = "model",
#'   select_metrics = c("crps", "bias")
#' ) +
#'   facet_wrap(~target_type, ncol = 1)
plot_score_table <- function(scores,
                             y = NULL,
                             select_metrics = NULL) {

  # identify metrics -----------------------------------------------------------
  # identify metrics by looking at which of the available column names
  # are metrics. All other variables are treated as identifier variables
  all_metrics <- available_metrics()

  metrics <- names(scores)[names(scores) %in% all_metrics]
  id_vars <- names(scores)[!(names(scores) %in% all_metrics)]


  # metrics to delete
  scores <- data.table::as.data.table(scores)

  if (!is.null(select_metrics)) {
    to_delete <- setdiff(metrics, select_metrics)
    scores[, (to_delete) := NULL]
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
  metrics_min_good <- setdiff(metrics, c(
    metrics_zero_good, metrics_p_val,
    metrics_no_color
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
  scale_p_val <- function(x) {
    out <- rep(0, length(x))
    out[x < 0.1] <- 0.2
    out[x < 0.05] <- 0.5
    out[x < 0.01] <- 1
    return(out)
  }

  # pivot longer and add scaled values
  df <- data.table::melt(scores,
    value.vars = metrics,
    id.vars = id_vars,
    variable.name = "metric"
  )

  df[metric %in% metrics_min_good, value_scaled := scale_min_good(value),
    by = metric
  ]
  df[metric %in% metrics_zero_good, value_scaled := scale(value),
    by = metric
  ]
  df[metric %in% metrics_no_color, value_scaled := 0,
    by = metric
  ]
  df[metric %in% metrics_p_val, value_scaled := scale_p_val(value),
    by = metric
  ]


  # create identifier column for plot if not given -----------------------------
  if (is.null(y)) {
    # create an identifier column by concatinating all columns that
    # are not a metric
    identifier_columns <- names(df)[!names(df) %in%
      c("metric", "value", "value_scaled")]
  } else {
    identifier_columns <- y
  }

  # if there is only one column, leave column as is. Reason to do that is that
  # users can then pass in a factor and keep the ordering of that column intact
  if (length(identifier_columns) > 1) {
    df[, identif := do.call(paste, c(.SD, sep = "_")),
      .SDcols = identifier_columns
    ]
  } else {
    setnames(df, old = eval(identifier_columns), new = "identif")
  }

  # plot -----------------------------------------------------------------------
  # make plot with all metrics that are not NA
  plot <- ggplot(
    df[!is.na(value), ],
    aes(y = identif, x = metric)
  ) +
    # geom_tile(fill = "blue") +
    geom_tile(aes(fill = value_scaled), colour = "white", show.legend = FALSE) +
    geom_text(aes(y = identif, label = round(value, 2))) +
    scale_fill_gradient2(low = "steelblue", high = "salmon") +
    theme_light() +
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
#' @importFrom ggplot2 ggplot aes_string aes geom_linerange facet_wrap labs
#' theme theme_light unit guides guide_legend
#' @export
#'
#' @examples
#' library("scoringutils")
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

  plot <- ggplot(scores, aes_string(y = x)) +
    geom_col(
      position = col_position,
      aes(x = component_value, fill = wis_component_name)
    ) +
    theme_light() +
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
#' @importFrom ggplot2 ggplot aes_string aes geom_point geom_line
#' expand_limits theme theme_light element_text scale_color_continuous labs
#' @export
#'
#' @examples
#' library("scoringutils")
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
    aes_string(
      x = x,
      y = y,
      colour = colour
    )
  ) +
    geom_point(size = 2) +
    geom_line(aes(group = range),
      colour = "black",
      size = 0.01
    ) +
    theme_light() +
    expand_limits(y = 0) +
    scale_color_continuous(low = "steelblue", high = "salmon") +
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
#' @importFrom ggplot2 ggplot aes_string aes geom_tile geom_text
#' scale_fill_gradient2 labs element_text coord_cartesian
#' @export
#'
#' @examples
#' library("scoringutils")
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
    aes_string(
      y = y,
      x = x,
      fill = metric
    )
  ) +
    geom_tile() +
    geom_text(aes_string(label = metric)) +
    scale_fill_gradient2(low = "skyblue", high = "red") +
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
#' [score()]. The data.frame needs to have columns called
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
#' @param facet_formula formula for facetting in ggplot. If this is `NULL`
#' (the default), no facetting will take place
#' @param facet_wrap_or_grid Use ggplot2's `facet_wrap` or
#' `facet_grid`? Anything other than "facet_wrap" will be interpreted as
#' `facet_grid`. This only takes effect if `facet_formula` is not
#' `NULL`
#' @param ncol Number of columns for facet wrap. Only relevant if
#' `facet_formula` is given and `facet_wrap_or_grid == "facet_wrap"`
#' @param scales scales argument that gets passed down to ggplot. Only necessary
#' if you make use of facetting. Default is "free_y"
#' @param allow_truth_without_pred logical, whether or not
#' to allow instances where there is truth data, but no forecast. If `FALSE`
#' (the default), these get filtered out.
#' @return ggplot object with a plot of true vs predicted values
#' @importFrom ggplot2 ggplot scale_colour_manual scale_fill_manual
#' facet_wrap facet_grid sym geom_ribbon
#' @importFrom data.table dcast
#' @export
#'
#' @examples
#' example1 <- scoringutils::example_continuous
#'
#' plot_predictions(
#'   example1,
#'   x = "target_end_date",
#'   filter_truth = list(
#'     'target_end_date <= "2021-07-22"',
#'     'target_end_date > "2021-05-01"'
#'   ),
#'   filter_forecasts = list(
#'     "model == 'EuroCOVIDhub-ensemble'",
#'     'forecast_date == "2021-06-07"'
#'   ),
#'   facet_formula = location ~ target_type,
#'   range = c(0, 50, 90, 95)
#' )
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
                             remove_from_truth = c("model", "forecaster", "quantile", "prediction", "sample", "interval")) {

  # preparations ---------------------------------------------------------------
  # check data argument is provided
  if (is.null(data) && (is.null(truth_data) | is.null(forecasts))) {
    stop("need arguments 'data' in function 'score()', or alternatively 'forecasts' and 'truth_data'")
  }

  if (is.null(data)) {
    data <- merge_pred_and_obs(forecasts, truth_data, by = merge_by, join = "full")
    if (nrow(data) == 0) {
      warning("After attempting to merge, only an empty data.table was left")
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

  # if specified, get all combinations of the facet variables present in the
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
      use.names = TRUE
    )
  }

  # delete certain columns that denominate the forecaster from the truth data
  truth_data <- delete_columns(truth_data, remove_from_truth, make_unique = TRUE)

  # find out what type of predictions we have. convert sample based to
  # range data
  colnames <- colnames(forecasts)
  if ("sample" %in% colnames) {
    forecasts <- sample_to_range_long(forecasts,
      range = range,
      keep_quantile_col = FALSE
    )
  } else if ("quantile" %in% colnames) {
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

  pal <- grDevices::colorRampPalette(c("lightskyblue1", "steelblue3"))

  plot <- ggplot(data = data, aes(x = !!sym(x))) +
    scale_colour_manual("", values = c("black", "steelblue4")) +
    scale_fill_manual(name = "range", values = pal(length(range))) +
    theme_light()

  if (nrow(intervals) != 0) {
    # pivot wider and convert range to a factor
    intervals <- data.table::dcast(intervals, ... ~ boundary,
      value.var = "prediction"
    )
    intervals[, range := factor(range,
      levels = sort(unique(range), decreasing = TRUE),
      ordered = TRUE
    )]

    # plot prediction ranges
    plot <- plot +
      geom_ribbon(
        data = intervals,
        aes(
          ymin = lower, ymax = upper,
          group = range, fill = range
        )
      )
  }

  # add median in a different colour
  if (0 %in% range) {
    select_median <- (forecasts$range %in% 0 & forecasts$boundary == "lower")
    median <- forecasts[select_median]

    if (nrow(median) > 0) {
      plot <- plot +
        geom_line(
          data = median,
          mapping = aes(y = prediction, colour = "median"),
          lwd = 0.4
        )
    }
  }

  # add true_values
  if (nrow(truth_data) > 0) {
    plot <- plot +
      geom_point(
        data = truth_data,
        aes(y = true_value, colour = "actual"),
        size = 0.5
      ) +
      geom_line(
        data = truth_data,
        aes(y = true_value, colour = "actual"),
        lwd = 0.2
      )
  }

  plot <- plot +
    ylab("True and predicted values")

  # facet if specified by the user
  if (!is.null(facet_formula)) {
    if (facet_wrap_or_grid == "facet_wrap") {
      plot <- plot +
        facet_wrap(facet_formula, scales = scales, ncol = ncol)
    } else {
      plot <- plot +
        facet_grid(facet_formula, scales = scales)
    }
  }

  return(plot)
}







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
#' @importFrom ggplot2 ggplot scale_colour_manual scale_fill_manual
#' facet_wrap facet_grid geom_polygon
#' @importFrom data.table dcast
#' @export
#'
#' @examples
#' library("scoringutils")
#' scores <- score(example_quantile)
#' scores <- summarise_scores(scores, by = c("model", "range"))
#' plot_interval_coverage(scores)
plot_interval_coverage <- function(scores,
                                   colour = "model") {
  ## overall model calibration - empirical interval coverage
  p1 <- ggplot(scores, aes_string(
    x = "range",
    colour = colour
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
    theme_light() +
    theme(legend.position = "bottom") +
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
#' @importFrom ggplot2 ggplot scale_colour_manual scale_fill_manual
#' scale_y_continuous
#' @importFrom data.table dcast
#' @export
#'
#' @examples
#' library("scoringutils")
#' scores <- score(example_quantile)
#' scores <- summarise_scores(scores, by = c("model", "quantile"))
#' plot_quantile_coverage(scores)
plot_quantile_coverage <- function(scores,
                                   colour = "model") {
  p2 <- ggplot(
    data = scores,
    aes_string(x = "quantile", colour = colour)
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
    theme_light() +
    theme(legend.position = "bottom") +
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
#' @param type character vector of length one that is either "mean_scores_ratio" or "pval".
#' This denotes whether to visualise the ratio or the p-value of the
#' pairwise comparison. Default is "mean_scores_ratio"
#' @param smaller_is_good logical (default is `TRUE`) that indicates whether
#' smaller or larger values are to be interpreted as 'good' (as you could just
#' invert the mean scores ratio)
#' @importFrom ggplot2 ggplot aes geom_tile geom_text labs coord_cartesian
#' scale_fill_gradient2 theme_light element_text
#' @importFrom data.table as.data.table setnames rbindlist
#' @importFrom stats reorder
#' @importFrom ggplot2 labs coord_cartesian facet_wrap facet_grid theme
#' element_text element_blank
#' @export
#'
#' @examples
#' library(ggplot2)
#' library(scoringutils)
#' df <- data.frame(
#'   model = rep(c("model1", "model2", "model3"), each = 10),
#'   id = rep(1:10),
#'   interval_score = abs(rnorm(30, mean = rep(c(1, 1.3, 2), each = 10))),
#'   aem = (abs(rnorm(30)))
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
    plot <- ggplot2::ggplot(
      upper_triangle_complete,
      ggplot2::aes(
        x = compare_against,
        y = model,
        fill = fill_col
      )
    ) +
      ggplot2::geom_tile(width = 0.98, height = 0.98) +
      ggplot2::geom_text(ggplot2::aes(label = var_of_interest),
        na.rm = TRUE
      ) +
      ggplot2::scale_fill_gradient2(
        low = "skyblue", mid = "grey95",
        high = "brown1",
        na.value = "lightgrey",
        midpoint = 0,
        limits = c(-1, 1),
        name = NULL
      ) +
      ggplot2::theme_light() +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(
          angle = 90, vjust = 1,
          hjust = 1, color = "brown4"
        ),
        axis.text.y = ggplot2::element_text(color = "steelblue4"),
        panel.grid.major = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank(),
        # panel.background = ggplot2::element_rect(fill = "grey90"),
        # axis.line.y = ggplot2::element_line(color = "steelblue4", size = 4),
        # axis.line.x = ggplot2::element_line(color = "brown3", size = 4),
        legend.position = "none"
      ) +
      ggplot2::labs(
        x = "", y = "",
        title = "Pairwise comparisons - mean_scores_ratio (upper) and pval (lower)"
      ) +
      ggplot2::coord_cartesian(expand = FALSE)

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

    fill_rule <- ifelse(lower_triangle$fill_col == 0.000001, "grey95", "palegreen3")
    lower_triangle[, var_of_interest := as.character(var_of_interest)]
    lower_triangle[, var_of_interest := ifelse(var_of_interest == "0",
      "< 0.001", var_of_interest
    )]

    plot <- plot +
      ggplot2::geom_tile(
        data = lower_triangle,
        ggplot2::aes(alpha = fill_col),
        fill = fill_rule,
        color = "white",
        width = 0.97, height = 0.97
      ) +
      ggplot2::geom_text(
        data = lower_triangle,
        ggplot2::aes(label = var_of_interest),
        na.rm = TRUE
      )
  } else if (type == "mean_scores_ratio") {
    comparison_result[, var_of_interest := round(mean_scores_ratio, 2)]

    # implemnt breaks for colour heatmap
    breaks <- c(0, 0.1, 0.5, 0.75, 1, 1.33, 2, 10, Inf)
    plot_scales <- c(-1, -0.5, -0.25, 0, 0, 0.25, 0.5, 1)
    comparison_result[, fill_col := get_fill_scale(
      var_of_interest,
      breaks, plot_scales
    )]

    high_col <- "brown1"
  } else {
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

  plot <- ggplot2::ggplot(
    comparison_result,
    ggplot2::aes(
      y = reorder(model, 1 / mean_scores_ratio, FUN = geom_mean_helper),
      x = reorder(compare_against, mean_scores_ratio, FUN = geom_mean_helper),
      fill = fill_col
    )
  ) +
    ggplot2::geom_tile(
      color = "white",
      width = 0.97, height = 0.97
    ) +
    ggplot2::geom_text(ggplot2::aes(label = var_of_interest),
      na.rm = TRUE
    ) +
    ggplot2::scale_fill_gradient2(
      low = "skyblue", mid = "grey95",
      high = high_col,
      na.value = "lightgrey",
      midpoint = 0,
      limits = c(-1, 1),
      name = NULL
    ) +
    ggplot2::theme_light() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(
        angle = 90, vjust = 1,
        hjust = 1
      ),
      legend.position = "none"
    ) +
    ggplot2::labs(
      x = "", y = "",
      title = "Pairwise comparisons - p-value whether mean scores ratio equal to 1"
    ) +
    ggplot2::coord_cartesian(expand = FALSE)

  if (type == "mean_scores_ratio") {
    plot <- plot +
      ggplot2::theme(
        panel.grid.major = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank(),
        axis.text.x = ggplot2::element_text(
          angle = 90, vjust = 1,
          hjust = 1, color = "brown4"
        ),
        axis.text.y = ggplot2::element_text(color = "steelblue4")
      ) +
      ggplot2::ggtitle("Pairwise comparisons - ratio of mean scores (for overlapping forecast sets)")
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
#' library(scoringutils)
#'
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
      hist <- ggplot2::ggplot(
        data = pit,
        aes(x = pit_value)
      ) +
        ggplot2::geom_histogram(aes(y = stat(count) / sum(count)),
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
    theme_light()

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
#' geom_tile scale_fill_gradient aes_string
#' @importFrom data.table dcast .I .N
#' @export
#'
#' @examples
#' library(scoringutils)
#' library(ggplot2)
#' avail_forecasts <- avail_forecasts(example_quantile,
#'   by = c(
#'     "model", "target_type",
#'     "target_end_date"
#'   )
#' )
#' plot_avail_forecasts(avail_forecasts,
#'   x = "target_end_date",
#'   show_numbers = FALSE
#' ) +
#'   facet_wrap("target_type")
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
    aes_string(y = y, x = x)
  ) +
    geom_tile(aes(fill = `Number forecasts`),
      width = 0.97, height = 0.97
    ) +
    scale_fill_gradient(
      low = "grey95", high = "steelblue",
      na.value = "lightgrey"
    ) +
    theme_light() +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
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
