#' @title Plot Coloured Score Table
#'
#' @description
#' Plots a coloured table of summarised scores obtained using
#' [score()]
#'
#' @param scores A data.frame of summarised scores as produced by
#' [score()]
#' @param y the variable to be shown on the y-axis. If `NULL` (default),
#' all columns that are not scoring metrics will be used. Alternatively,
#' you can specify a vector with column names, e.g.
#' `y = c("model", "location")`. These column names will be concatenated
#' to create a unique row identifier (e.g. "model1_location1")
#' @param select_metrics A character vector with the metrics to show. If set to
#' `NULL` (default), all metrics present in `scores` will
#' be shown
#' @return A ggplot2 object with a coloured table of summarised scores
#' @importFrom ggplot2 ggplot aes element_blank element_text labs coord_cartesian
#' @importFrom data.table setDT melt
#' @importFrom stats sd
#' @export
#'
#' @examples
#' library("scoringutils")
#' library(ggplot2)
#' scores <- score(example_quantile)
#' scores <- summarise_scores(scores, by = c("model", "target_type"))
#'
#' score_table(scores, y = "model") +
#'   facet_wrap(~target_type, ncol = 1)
#'
#' # can also put target description on the y-axis
#' score_table(scores, y = c("model", "target_type"))
#'
#' # yields the same result in this case
#' score_table(scores)
#'
#' scores <- score(example_integer)
#' scores <- summarise_scores(scores, by = c("model", "target_type"))
#'
#' # only show selected metrics
#' score_table(scores,
#'   y = "model",
#'   select_metrics = c("crps", "bias")
#' ) +
#'   facet_wrap(~target_type, ncol = 1)
score_table <- function(scores,
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

  df[, identif := do.call(paste, c(.SD, sep = "_")),
    .SDcols = identifier_columns
  ]


  # plot -----------------------------------------------------------------------
  # make plot with all metrics that are not NA
  plot <- ggplot(
    df[!is.na(value), ],
    aes(y = identif, x = metric)
  ) +
    # geom_tile(fill = "blue") +
    geom_tile(aes(fill = value_scaled), colour = "white") +
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
#' Usually this will be "model"
#' @param relative_contributions show relative contributions instead of absolute
#' contributions. Default is FALSE and this functionality is not available yet.
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
#'   facet_wrap(~target_type)
#' @references
#' Bracher J, Ray E, Gneiting T, Reich, N (2020) Evaluating epidemic forecasts
#' in an interval format. <https://journals.plos.org/ploscompbiol/article?id=10.1371/journal.pcbi.1008618>

plot_wis <- function(scores,
                     x = "model",
                     relative_contributions = FALSE) {
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

  plot <- ggplot(scores, aes_string(x = x)) +
    geom_col(
      position = col_position,
      aes(y = component_value, fill = wis_component_name)
    ) +
    theme_light() +
    theme(
      panel.spacing = unit(4, "mm"),
      axis.text.x = element_text(
        angle = 90,
        vjust = 1,
        hjust = 1
      )
    ) +
    guides(fill = guide_legend(title = "WIS component")) +
    ylab("WIS contributions")

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
      use.names = TRUE
    )
  }

  # delete certain columns that denominate the forecaster from the truth data
  truth_data <- delete_columns(truth_data, remove_from_truth)

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
    xlab("Interval range") +
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
    ylab("% obs below quantile") +
    coord_cartesian(expand = FALSE)

  return(p2)
}
