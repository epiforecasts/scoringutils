#' @title Plot contributions to the weighted interval score
#'
#' @description
#' Visualise the components of the weighted interval score: penalties for
#' over-prediction, under-prediction and for high dispersion (lack of
#' sharpness).
#'
#' @param scores A data.table of scores based on quantile forecasts as
#'   produced by [score()] and summarised using [summarise_scores()].
#' @param x The variable from the scores you want to show on the x-Axis.
#'   Usually this will be "model".
#' @param relative_contributions Logical. Show relative contributions instead
#'   of absolute contributions? Default is `FALSE` and this functionality is not
#'   available yet.
#' @param flip Boolean (default is `FALSE`), whether or not to flip the axes.
#' @return A ggplot object showing a contributions from the three components of
#'   the weighted interval score.
#' @importFrom ggplot2 ggplot aes geom_linerange facet_wrap labs
#' scale_fill_discrete coord_flip
#' theme theme_light unit guides guide_legend .data
#' @importFrom data.table melt
#' @importFrom checkmate assert_subset assert_logical
#' @return A ggplot object with a visualisation of the WIS decomposition
#' @export
#' @examples
#' library(ggplot2)
#' scores <- score(as_forecast(example_quantile))
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
  # input checks
  scores <- ensure_data.table(scores)
  wis_components <- c("overprediction", "underprediction", "dispersion")
  assert(check_columns_present(scores, wis_components))
  assert_subset(x, names(scores))
  assert_logical(relative_contributions, len = 1)
  assert_logical(flip, len = 1)

  scores <- melt(scores,
    measure.vars = wis_components,
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


#' @title Create a heatmap of a scoring metric
#'
#' @description
#' This function can be used to create a heatmap of one metric across different
#' groups, e.g. the interval score obtained by several forecasting models in
#' different locations.
#'
#' @param scores A data.frame of scores based on quantile forecasts as
#' produced by [score()].
#' @param y The variable from the scores you want to show on the y-Axis. The
#'   default for this is "model"
#' @param x The variable from the scores you want to show on the x-Axis. This
#'   could be something like "horizon", or "location"
#' @param metric String, the metric that determines the value and colour shown
#'   in the tiles of the heatmap.
#' @return A ggplot object showing a heatmap of the desired metric
#' @importFrom data.table setDT `:=`
#' @importFrom ggplot2 ggplot  aes geom_tile geom_text .data
#' scale_fill_gradient2 labs element_text coord_cartesian
#' @importFrom checkmate assert_subset
#' @export
#' @examples
#' scores <- score(as_forecast(example_quantile))
#' scores <- summarise_scores(scores, by = c("model", "target_type"))
#' scores <- summarise_scores(
#'   scores, by = c("model", "target_type"),
#'   fun = signif, digits = 2
#' )
#'
#' plot_heatmap(scores, x = "target_type", metric = "bias")

plot_heatmap <- function(scores,
                         y = "model",
                         x,
                         metric) {
  scores <- ensure_data.table(scores)
  assert_subset(y, names(scores))
  assert_subset(x, names(scores))
  assert_subset(metric, names(scores))

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


#' @title Plot interval coverage
#'
#' @description
#' Plot interval coverage values (see [get_coverage()] for more information).
#'
#' @param coverage A data frame of coverage values as produced by
#' [get_coverage()].
#' @param colour According to which variable shall the graphs be coloured?
#' Default is "model".
#' @return ggplot object with a plot of interval coverage
#' @importFrom ggplot2 ggplot scale_colour_manual scale_fill_manual .data
#' facet_wrap facet_grid geom_polygon geom_line
#' @importFrom checkmate assert_subset
#' @importFrom data.table dcast
#' @export
#' @examples
#' \dontshow{
#'   data.table::setDTthreads(2) # restricts number of cores used on CRAN
#' }
#' coverage <- get_coverage(as_forecast(example_quantile), by = "model")
#' plot_interval_coverage(coverage)
plot_interval_coverage <- function(coverage,
                                   colour = "model") {
  coverage <- ensure_data.table(coverage)
  assert_subset(colour, names(coverage))

  # in case quantile columns are present, remove them and then take unique
  # values. This doesn't visually affect the plot, but prevents lines from being
  # drawn twice.
  del <- c("quantile_level", "quantile_coverage", "quantile_coverage_deviation")
  suppressWarnings(coverage[, eval(del) := NULL])
  coverage <- unique(coverage)

  ## overall model calibration - empirical interval coverage
  p1 <- ggplot(coverage, aes(
    x = interval_range,
    colour = .data[[colour]]
  )) +
    geom_polygon(
      data = data.frame(
        x = c(0, 0, 100),
        y = c(0, 100, 100),
        g = c("o", "o", "o"),
        stringsAsFactors = TRUE
      ),
      aes(
        x = x, y = y, group = g,
        fill = g
      ),
      alpha = 0.05,
      colour = "white",
      fill = "olivedrab3"
    ) +
    geom_line(aes(y = interval_range),
      colour = "grey",
      linetype = "dashed"
    ) +
    geom_line(aes(y = interval_coverage * 100)) +
    theme_scoringutils() +
    ylab("% Obs inside interval") +
    xlab("Nominal interval coverage") +
    coord_cartesian(expand = FALSE)

  return(p1)
}

#' @title Plot quantile coverage
#'
#' @description
#' Plot quantile coverage values (see [get_coverage()] for more information).
#'
#' @inheritParams plot_interval_coverage
#' @param colour String, according to which variable shall the graphs be
#' coloured? Default is "model".
#' @return A ggplot object with a plot of interval coverage
#' @importFrom ggplot2 ggplot scale_colour_manual scale_fill_manual .data aes
#'   scale_y_continuous geom_line
#' @importFrom checkmate assert_subset assert_data_frame
#' @importFrom data.table dcast
#' @export
#' @examples
#' coverage <- get_coverage(as_forecast(example_quantile), by = "model")
#' plot_quantile_coverage(coverage)

plot_quantile_coverage <- function(coverage,
                                   colour = "model") {
  coverage <- assert_data_frame(coverage)
  assert_subset(colour, names(coverage))

  p2 <- ggplot(
    data = coverage,
    aes(x = quantile_level, colour = .data[[colour]])
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
        g = c("o", "o", "o"),
        stringsAsFactors = TRUE
      ),
      aes(
        x = x, y = y, group = g,
        fill = g
      ),
      alpha = 0.05,
      colour = "white",
      fill = "olivedrab3"
    ) +
    geom_line(aes(y = quantile_level),
      colour = "grey",
      linetype = "dashed"
    ) +
    geom_line(aes(y = quantile_coverage)) +
    theme_scoringutils() +
    xlab("Quantile level") +
    ylab("% Obs below quantile level") +
    scale_y_continuous(
      labels = function(x) {
        paste(100 * x)
      }
    ) +
    coord_cartesian(expand = FALSE)

  return(p2)
}

#' @title Plot heatmap of pairwise comparisons
#'
#' @description
#' Creates a heatmap of the ratios or pvalues from a pairwise comparison
#' between models.
#'
#' @param comparison_result A data.frame as produced by
#' [get_pairwise_comparisons()].
#' @param type Character vector of length one that is either
#'   "mean_scores_ratio" or "pval". This denotes whether to
#'   visualise the ratio or the p-value of the pairwise comparison.
#'   Default is "mean_scores_ratio".
#' @importFrom ggplot2 ggplot aes geom_tile geom_text labs coord_cartesian
#'   scale_fill_gradient2 theme_light element_text
#' @importFrom data.table as.data.table setnames rbindlist
#' @importFrom stats reorder
#' @importFrom ggplot2 labs coord_cartesian facet_wrap facet_grid theme
#'   element_text element_blank
#' @return
#' A ggplot object with a heatmap of mean score ratios from pairwise
#' comparisons.
#' @export
#' @examples
#' library(ggplot2)
#' scores <- score(as_forecast(example_quantile))
#' pairwise <- get_pairwise_comparisons(scores, by = "target_type")
#' plot_pairwise_comparisons(pairwise, type = "mean_scores_ratio") +
#'   facet_wrap(~target_type)

plot_pairwise_comparisons <- function(comparison_result,
                                      type = c("mean_scores_ratio", "pval")) {
  comparison_result <- ensure_data.table(comparison_result)
  type <- match.arg(type)

  relative_skill_metric <- grep(
    "(?<!scaled)_relative_skill$", colnames(comparison_result),
    value = TRUE, perl = TRUE
  )
  comparison_result[, model := reorder(model, -get(relative_skill_metric))]
  levels <- levels(comparison_result$model)

  get_fill_scale <- function(values, breaks, plot_scales) {
    values[is.na(values)] <- 1 # this would be either ratio = 1 or pval = 1
    scale <- cut(values,
      breaks = breaks,
      include.lowest = TRUE,
      right = FALSE,
      labels = plot_scales
    )
    return(as.numeric(as.character(scale)))
  }

  type <- match.arg(type)

  if (type == "mean_scores_ratio") {
    comparison_result[, var_of_interest := round(mean_scores_ratio, 2)]

    # implement breaks for colour heatmap
    breaks <- c(0, 0.1, 0.5, 0.75, 1, 1.33, 2, 10, Inf)
    plot_scales <- c(-1, -0.5, -0.25, 0, 0, 0.25, 0.5, 1)
    comparison_result[, fill_col := get_fill_scale(
      var_of_interest,
      breaks, plot_scales
    )]

    high_col <- "salmon"
  } else if (type == "pval") {
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
                                                  "< 0.001", var_of_interest)]
  }

  plot <- ggplot(
    comparison_result,
    aes(
      y = reorder(model, 1 / mean_scores_ratio, FUN = geometric_mean),
      x = reorder(compare_against, mean_scores_ratio, FUN = geometric_mean),
      fill = fill_col
    )
  ) +
    geom_tile(
      color = "white",
      width = 0.97, height = 0.97
    ) +
    geom_text(aes(label = var_of_interest),
              na.rm = TRUE) +
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

  return(plot)
}


#' @title PIT histogram
#'
#' @description
#' Make a simple histogram of the probability integral transformed values to
#' visually check whether a uniform distribution seems likely.
#'
#' @param pit Either a vector with the PIT values, or a data.table as
#'   produced by [get_pit()].
#' @param num_bins The number of bins in the PIT histogram, default is "auto".
#'   When `num_bins == "auto"`, [plot_pit()] will either display 10 bins, or it
#'   will display a bin for each available quantile in case you passed in data in
#'   a quantile-based format.
#'   You can control the number of bins by supplying a number. This is fine for
#'   sample-based pit histograms, but may fail for quantile-based formats. In this
#'   case it is preferred to supply explicit breaks points using the `breaks`
#'   argument.
#' @param breaks Numeric vector with the break points for the bins in the
#'   PIT histogram. This is preferred when creating a PIT histogram based on
#'   quantile-based data. Default is `NULL` and breaks will be determined by
#'   `num_bins`. If `breaks` is used, `num_bins` will be ignored.
#' @importFrom stats as.formula
#' @importFrom ggplot2 geom_col
#' @importFrom stats density
#' @return A ggplot object with a histogram of PIT values
#' @examples
#' \dontshow{
#'   data.table::setDTthreads(2) # restricts number of cores used on CRAN
#' }
#'
#' # PIT histogram in vector based format
#' observed <- rnorm(30, mean = 1:30)
#' predicted <- replicate(200, rnorm(n = 30, mean = 1:30))
#' pit <- pit_sample(observed, predicted)
#' plot_pit(pit)
#'
#' # quantile-based pit
#' pit <- get_pit(as_forecast(example_quantile), by = "model")
#' plot_pit(pit, breaks = seq(0.1, 1, 0.1))
#'
#' # sample-based pit
#' pit <- get_pit(as_forecast(example_sample_discrete), by = "model")
#' plot_pit(pit)
#' @importFrom ggplot2 ggplot aes xlab ylab geom_histogram stat theme_light after_stat
#' @importFrom checkmate assert check_set_equal check_number
#' @export

plot_pit <- function(pit,
                     num_bins = "auto",
                     breaks = NULL) {
  assert(
    check_set_equal(num_bins, "auto"),
    check_number(num_bins, lower = 1)
  )
  assert_numeric(breaks, lower = 0, upper = 1, null.ok = TRUE)

  # vector-format is always sample-based, for data.frames there are two options
  if ("quantile_level" %in% names(pit)) {
    type <- "quantile-based"
  } else {
    type <- "sample-based"
  }

  # use breaks if explicitly given, otherwise assign based on number of bins
  if (!is.null(breaks)) {
    plot_quantiles <- breaks
  } else if (is.null(num_bins) || num_bins == "auto") {
    # automatically set number of bins
    if (type == "sample-based") {
      num_bins <- 10
      width <- 1 / num_bins
      plot_quantiles <- seq(width, 1, width)
    }
    if (type == "quantile-based") {
      plot_quantiles <- unique(pit$quantile_level)
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
        data = pit[quantile_level %in% plot_quantiles],
        aes(x = quantile_level, y = pit_value)
      ) +
        geom_col(position = "dodge") +
        facet_wrap(formula)
    }

    if (type == "sample-based") {
      hist <- ggplot(
        data = pit,
        aes(x = pit_value)
      ) +
        geom_histogram(aes(y = after_stat(width * density)),
          breaks = plot_quantiles,
          colour = "grey"
        ) +
        facet_wrap(formula)
    }
  } else {
    # non data.frame version
    hist <- ggplot(
      data = data.frame(x = pit, stringsAsFactors = TRUE),
      aes(x = x)
    ) +
      geom_histogram(aes(y = after_stat(width * density)),
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

#' @title Visualise the number of available forecasts
#'
#' @description
#' Visualise Where Forecasts Are Available.
#' @param forecast_counts A data.table (or similar) with a column `count`
#'   holding forecast counts, as produced by [get_forecast_counts()].
#' @param x Character vector of length one that denotes the name of the column
#'   to appear on the x-axis of the plot.
#' @param y Character vector of length one that denotes the name of the column
#'   to appear on the y-axis of the plot. Default is "model".
#' @param x_as_factor Logical (default is `TRUE`). Whether or not to convert
#'   the variable on the x-axis to a factor. This has an effect e.g. if dates
#'   are shown on the x-axis.
#' @param show_counts Logical (default is `TRUE`) that indicates whether
#'   or not to show the actual count numbers on the plot.
#' @return A ggplot object with a plot of forecast counts
#' @importFrom ggplot2 ggplot scale_colour_manual scale_fill_manual
#'   geom_tile scale_fill_gradient .data
#' @importFrom data.table dcast .I .N
#' @importFrom checkmate assert_subset assert_logical
#' @export
#' @examples
#' library(ggplot2)
#' forecast_counts <- get_forecast_counts(
#'   as_forecast(example_quantile),
#'   by = c("model", "target_type", "target_end_date")
#' )
#' plot_forecast_counts(
#'  forecast_counts, x = "target_end_date", show_counts = FALSE
#' ) +
#'  facet_wrap("target_type")

plot_forecast_counts <- function(forecast_counts,
                                 x,
                                 y = "model",
                                 x_as_factor = TRUE,
                                 show_counts = TRUE) {

  forecast_counts <- ensure_data.table(forecast_counts)
  assert_subset(y, colnames(forecast_counts))
  assert_subset(x, colnames(forecast_counts))
  assert_logical(x_as_factor, len = 1)
  assert_logical(show_counts, len = 1)

  if (x_as_factor) {
    forecast_counts[, eval(x) := as.factor(get(x))]
  }

  setnames(forecast_counts, old = "count", new = "Count")

  plot <- ggplot(
    forecast_counts,
    aes(y = .data[[y]], x = .data[[x]])
  ) +
    geom_tile(aes(fill = `Count`),
              width = 0.97, height = 0.97) +
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
  if (show_counts) {
    plot <- plot +
      geom_text(aes(label = `Count`))
  }
  return(plot)
}


#' @title Plot correlation between metrics
#'
#' @description
#' Plots a heatmap of correlations between different metrics.
#'
#' @param correlations A data.table of correlations between scores as produced
#'   by [get_correlations()].
#' @param digits A number indicating how many decimal places the correlations
#'   should be rounded to. By default (`digits = NULL`) no rounding takes place.
#' @return
#' A ggplot object showing a coloured matrix of correlations between metrics.
#' @importFrom ggplot2 ggplot geom_tile geom_text aes scale_fill_gradient2
#' element_text labs coord_cartesian theme element_blank
#' @importFrom data.table setDT melt
#' @importFrom checkmate assert_data_frame
#' @export
#' @return A ggplot object with a visualisation of correlations between metrics
#' @examples
#' scores <- score(as_forecast(example_quantile))
#' correlations <- get_correlations(
#'  summarise_scores(scores)
#' )
#' plot_correlations(correlations, digits = 2)

plot_correlations <- function(correlations, digits = NULL) {

  assert_data_frame(correlations)
  metrics <- get_metrics(correlations, error = TRUE)

  lower_triangle <- get_lower_tri(correlations[, .SD, .SDcols = metrics])

  if (!is.null(digits)) {
    lower_triangle <- round(lower_triangle, digits)
  }


  # check correlations is actually a matrix of correlations
  col_present <- check_columns_present(correlations, "metric")
  if (any(lower_triangle > 1, na.rm = TRUE) || !isTRUE(col_present)) {
    #nolint start: keyword_quote_linter
    cli_abort(
      c(
        "Found correlations > 1 or missing `metric` column.",
        "i" = "Did you forget to call {.fn scoringutils::get_correlations}?"
      )
    )
    #nolint end
  }

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
#' A theme for ggplot2 plots used in `scoringutils`.
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
