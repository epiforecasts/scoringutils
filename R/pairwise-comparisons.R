#' @title Do Pairwise Comparisons of Scores
#'
#' @description
#'
#' Make pairwise comparisons between models. The code for the pairwise
#' comparisons is inspired by an implementation by Johannes Bracher.
#'
#' The implementation of the permutation test follows the function
#' `permutationTest` from the `surveillance` package by Michael Höhle,
#' Andrea Riebler and Michaela Paul.
#'
#' @param scores A data.frame of unsummarised scores as produced by
#' [eval_forecasts()]
#' @param metric A character vector of length one with the metric to do the
#' comparison on. The default is "auto", meaning that either "interval_score",
#' "crps", or "brier_score" will be selected where available.
#' See [available_metrics()] for available metrics.
#' @param summarise_by character vector of columns to group the summary by. By
#' default, this is equal to `by` and no summary takes place. But sometimes you
#' may want to to summarise over categories different from the scoring.
#' `summarise_by` is also the grouping level used to compute (and possibly plot)
#' the probability integral transform(pit).
#' @param baseline character vector of length one that denotes the baseline
#' model against which to compare other models.
#' @param ... additional arguments, such as test options that can get passed
#' down to lower level functions. The following options are available:
#' `oneSided` (Boolean, default is `FALSE`, whether two conduct a one-sided
#' instead of a two-sided test), `test_type` (character, either "non_parametric"
#' or "permutation" determining which kind of test shall be conducted to
#' determine p-values. Default is "non-parametric), `n_permutations` (number of
#' permutations for a permutation test. Default is 999). See
#' [compare_two_models()] for more information.
#' @return A ggplot2 object with a coloured table of summarised scores
#' @importFrom data.table as.data.table data.table setnames copy
#' @importFrom stats sd rbinom wilcox.test p.adjust
#' @importFrom utils combn
#' @export
#' @author Johannes Bracher, https://jbracher.github.io/
#' @author Nikos Bosse
#' @examples
#' df <- data.frame(model = rep(c("model1", "model2", "model3"), each = 10),
#'                  date = as.Date("2020-01-01") + rep(1:5, each = 2),
#'                  location = c(1, 2),
#'                  interval_score = (abs(rnorm(30))),
#'                  aem = (abs(rnorm(30))))
#'
#' res <- scoringutils::pairwise_comparison(df,
#'                            baseline = "model1")
#' scoringutils::plot_pairwise_comparison(res)
#'
#' eval <- scoringutils::eval_forecasts(scoringutils::quantile_example_data)
#' pairwise <- pairwise_comparison(eval, summarise_by = c("model"))
#' @author Nikos Bosse \email{nikosbosse@@gmail.com}
#' @author Johannes Bracher, \email{johannes.bracher@@kit.edu}

pairwise_comparison <- function(scores,
                                metric = "auto",
                                baseline = NULL,
                                summarise_by = c("model"),
                                ...) {

  scores <- data.table::as.data.table(scores)

  # determine metric automatically
  if (metric == "auto") {
    metric <- infer_rel_skill_metric(scores)
  }

  # identify unit of single observation.
  forecast_unit <- get_unit_of_forecast(scores)

  # get rid of all unnecessary columns and keep only metric and forecast unit
  scores <- scores[, .SD, .SDcols = c(forecast_unit, metric)]
  scores <- unique(scores)

  # split data set into groups determined by summarise_by
  split_by <- setdiff(summarise_by, "model")
  split_scores <- split(scores, by = split_by)

  results <- lapply(split_scores,
                    FUN = function(scores) {
                      out <- pairwise_comparison_one_group(scores = scores,
                                                           metric = metric,
                                                           baseline = baseline,
                                                           summarise_by = summarise_by,
                                                           ...)
                    })

  out <- data.table::rbindlist(results)

  return(out)
}


#' @title Add relative skill to eval_forecasts()
#'
#' @description
#'
#' This function will only be called within [eval_forecasts()] and serves to
#' make pairwise comparisons from within that function. It uses the
#' `summarise_by` argument as well as the data from [eval_forecasts()].
#' Essentially, it wraps [pairwise_comparison()] and deals with the specifics
#' necessary to work with [eval_forecasts()].
#' @inheritParams eval_forecasts
#' @param unsummarised_scores unsummarised scores to be passed from
#' [eval_forecasts()]
#'
#' @keywords internal

add_rel_skill_to_eval_forecasts <- function(unsummarised_scores,
                                            rel_skill_metric,
                                            baseline,
                                            summarise_by) {

  # determine metric automatically
  if (rel_skill_metric == "auto") {
    rel_skill_metric <- infer_rel_skill_metric(unsummarised_scores)
  }

  forecast_unit <- get_unit_of_forecast(unsummarised_scores)

  # summarise scores over all quantiles, ranges or samples in order to not
  # include them in the calculation of relative scores
  scores <- unsummarised_scores[, lapply(.SD, mean, na.rm = TRUE),
                by = forecast_unit,
                .SDcols = colnames(unsummarised_scores) %in% c(rel_skill_metric)]

  # remove range and quantile from summarise_by if they are present
  summarise_by <- setdiff(summarise_by, c("range", "quantile", "sample"))

  # if summarise_by is equal to forecast_unit, then pairwise comparisons don't make sense
  if (identical(sort(summarise_by), sort(forecast_unit))) {
    summarise_by <- "model"
    message("relative skill can only be computed if `summarise_by` is different from the unit of a single forecast. `summarise_by` was set to 'model'")
  }

  # do pairwise comparison
  pairwise <- pairwise_comparison(scores = scores,
                                  metric = rel_skill_metric,
                                  baseline = baseline,
                                  summarise_by = summarise_by)

  # delete unnecessary columns from the output
  cols_to_delete <- setdiff(colnames(pairwise),
                            unique(c(summarise_by, "model", "relative_skill", "scaled_rel_skill")))
  if (length(cols_to_delete > 1)) {
    pairwise[, eval(cols_to_delete) := NULL]
  }
  pairwise <- unique(pairwise)
  out <- merge(scores, pairwise, all.x = TRUE,
               by = unique(c("model", summarise_by)))

  # also delete skill metric from output
  out[, eval(rel_skill_metric) := NULL]

  return(out[])
}




#' @title Do Pairwise Comparison for one Set of Forecasts
#'
#' @description
#'
#' This function does the pairwise comparison for one set of forecasts, but
#' multiple models involved. It gets called from [pairwise_comparison()].
#' [pairwise_comparison()] splits the data into arbitrary subgroups specified
#' by the user (e.g. if pairwise comparison should be done separately for
#' different forecast targets) and then the actual pairwise comparison for that
#' subgroup is managed from [pairwise_comparison_one_group()]. In order to
#' actually do the comparison between two models over a subset of common
#' forecasts it calls [compare_two_models()].
#' @inheritParams pairwise_comparison

pairwise_comparison_one_group <- function(scores,
                                          metric,
                                          baseline,
                                          summarise_by,
                                          ...) {

  if (!("model" %in% names(scores))) {
    stop("pairwise compairons require a column called 'model'")
  }

  # get list of models
  models <- unique(scores$model)

  # if there aren't enough models to do any comparison, return NULL
  if (length(models) < 2) {
    return(NULL)
  }

  # create a data.frame with results
  # we only need to do the calculation once, because for the ratio that
  # should just be the inverse and for the permutation the result should
  # be the same

  # set up initial data.frame with all possible pairwise comparisons
  combinations <- data.table::as.data.table(t(combn(models, m = 2)))
  colnames(combinations) <- c("model", "compare_against")

  combinations[, c("ratio", "pval") := compare_two_models(scores = scores,
                                                          name_model1 = model,
                                                          name_model2 = compare_against,
                                                          metric = metric,
                                                          ...),
               by = seq_len(NROW(combinations))]

  combinations <- combinations[order(ratio)]
  combinations[, adj_pval := p.adjust(pval)]

  # mirror computations
  combinations_mirrored <- data.table::copy(combinations)
  data.table::setnames(combinations_mirrored,
                       old = c("model", "compare_against"),
                       new = c("compare_against", "model"))
  combinations_mirrored[, ratio := 1 / ratio]

  # add a one for those that are the same
  combinations_equal <- data.table::data.table(model = models,
                                               compare_against = models,
                                               ratio = 1,
                                               pval = 1,
                                               adj_pval = 1)

  result <- data.table::rbindlist(list(combinations,
                                       combinations_mirrored,
                                       combinations_equal),
                                  use.names = TRUE)

  # make result character instead of factor
  result[, `:=`("model" = as.character(model),
                "compare_against" = as.character(compare_against))]


  # calculate relative skill as geometric mean
  # small theta is again better. If a baseline is given, exclude it
  # from the computation of the geometric mean
  # maybe there is a more elegant way to do this
  if (!is.null(baseline)) {
    result_without_baseline <- data.table::copy(result)
    # filter out all ratios where compare_against is the baseline
    result_without_baseline <- result_without_baseline[compare_against != baseline, ]
    result_without_baseline[, `:=` (theta = geom_mean_helper(ratio)),
                            by = "model"]
    # merge back to retain the ratios even for comparisons with the baseline
    result <- merge(result, result_without_baseline, all.x = TRUE)
    # avoid mixture of NA and NaN which can cause problems downstream
    result[is.na(theta), theta := NA_real_]
    # remove NAs form merge in the thetas
    result[, theta := unique(na.omit(theta)), by = "model"]
  } else {
    result[, `:=` (theta = geom_mean_helper(ratio),
                   rel_to_baseline = NA_real_),
           by = "model"]
  }

  if(!is.null(baseline)) {
    baseline_theta <- unique(result[model == baseline, ]$theta)
    result[, rel_to_baseline := theta / baseline_theta]
  }

  # remove all the rows that are not present in summarise_by before merging
  cols_to_keep <- unique(c(summarise_by, "model"))
  cols_to_remove <- colnames(scores)[!(colnames(scores) %in% cols_to_keep)]
  scores[, eval(cols_to_remove) := NULL]
  scores <- unique(scores)
  # allow.cartesian needs to be set as sometimes rows will be duplicated a lot
  out <- merge(scores, result, by = "model", all = TRUE)

  # rename ratio to mean_scores_ratio
  data.table::setnames(out, old = c("ratio", "theta", "rel_to_baseline"),
                       new = c("mean_scores_ratio", "relative_skill", "scaled_rel_skill"))

  return(out[])
}







#' @title Compare Two Models Based on Subset of Common Forecasts
#'
#' @description
#'
#' This function compares two models based on the subset of forecasts for which
#' both models have made a prediction. It gets called
#' from [pairwise_comparison_one_group()], which handles the
#' comparison of multiple models on a single set of forecasts (there are no
#' subsets of forecasts to be distinguished). [pairwise_comparison_one_group()]
#' in turn gets called from from [pairwise_comparison()] which can handle
#' pairwise comparisons for a set of forecasts with multiple subsets, e.g.
#' pairwise comparisons for one set of forecasts, but done separately for two
#' different forecast targets.
#' @inheritParams pairwise_comparison
#' @param name_model1 character, name of the first model
#' @param name_model2 character, name of the model to compare against
#' @param oneSided Boolean, default is `FALSE`, whether two conduct a one-sided
#' instead of a two-sided test to determine significance in a pairwise comparison
#' @param test_type character, either "non_parametric" (the default) or
#' "permutation". This determines which kind of test shall be conducted to
#' determine p-values.
#' @param n_permutations numeric, the number of permutations for a
#' permutation test. Default is 999.
#' @author Johannes Bracher, \email{johannes.bracher@@kit.edu}
#' @author Nikos Bosse \email{nikosbosse@@gmail.com}

compare_two_models <- function(scores,
                               name_model1,
                               name_model2,
                               metric,
                               oneSided = FALSE,
                               test_type = c("non_parametric", "permutation"),
                               n_permutations = 999) {

  scores <- data.table::as.data.table(scores)

  forecast_unit <- get_unit_of_forecast(scores)

  if (!("model" %in% names(scores))) {
    stop("pairwise comparisons require a column called 'model'")
  }

  # select only columns in c(by, var)
  a <- scores[model == name_model1, ]
  b <- scores[model == name_model2, ]

  # remove "model" from 'by' before merging
  merge_by <- setdiff(forecast_unit, "model")

  overlap <- merge(a, b, by = merge_by, allow.cartesian = TRUE)
unique(overlap)

  if (nrow(overlap) == 0) {
    return(list(ratio = NA_real_, pval = NA_real_))
  }

  values_x <- overlap[[paste0(metric, ".x")]]
  values_y <- overlap[[paste0(metric, ".y")]]

  # calculate ratio to of average scores achieved by both models.
  # this should be equivalent to theta_ij in Johannes Bracher's document.
  # ratio < 1 --> model1 is better.
  # note we could also take mean(values_x) / mean(values_y), as it cancels out
  ratio <- sum(values_x) / sum(values_y)

  # test whether the ratio is significantly different from one
  # equivalently, one can test whether the difference between the two values
  # is significantly different from zero.
  if (test_type[1] == "permutation") {
    # adapted from the surveillance package
    pval <- permutation_test(values_x, values_y,
                             nPermutation = n_permutations,
                             oneSided = oneSided,
                             comparison_mode = "difference")
  } else {
    # this probably needs some more thought
    # alternative: do a paired t-test on ranks?
    pval <- wilcox.test(values_x, values_y, paired = TRUE)$p.value
  }
  return(list(mean_scores_ratio = ratio,
              pval = pval))
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
#' @param facet_formula facetting formula passed down to ggplot. Default is
#' `NULL`
#' @param scales scales argument that gets passed down to ggplot. Only necessary
#' if you make use of facetting. Default is "free_y"
#' @param facet_wrap_or_grid Use ggplot2's `facet_wrap` or
#' `facet_grid`? Anything other than "facet_wrap" will be interpreted as
#' `facet_grid`. This only takes effect if `facet_formula` is not
#' `NULL`
#' @param ncol Number of columns for facet wrap. Only relevant if
#' `facet_formula` is given and `facet_wrap_or_grid == "facet_wrap"`
#' @importFrom ggplot2 ggplot aes geom_tile geom_text labs coord_cartesian
#' scale_fill_gradient2 theme_light element_text
#' @importFrom data.table as.data.table setnames rbindlist
#' @importFrom stats reorder
#' @importFrom ggplot2 labs coord_cartesian facet_wrap facet_grid theme
#' element_text element_blank
#' @export
#'
#' @examples
#' df <- data.frame(model = rep(c("model1", "model2", "model3"), each = 10),
#'                  id = rep(1:10),
#'                  interval_score = abs(rnorm(30, mean = rep(c(1, 1.3, 2), each = 10))),
#'                  aem = (abs(rnorm(30))))
#'
#' data <- scoringutils::quantile_example_data
#' scores <- scoringutils::eval_forecasts(data)
#' pairwise <- pairwise_comparison(scores,
#'                                 summarise_by = "value_desc")
#' scoringutils::plot_pairwise_comparison(pairwise,
#'                                        facet_formula = ~ value_desc,
#'                                        scales = "fixed")


plot_pairwise_comparison <- function(comparison_result,
                                     type = c("mean_scores_ratio", "pval", "together"),
                                     smaller_is_good = TRUE,
                                     facet_formula = NULL,
                                     scales = "free_y",
                                     ncol = NULL,
                                     facet_wrap_or_grid = "facet_wrap") {

  comparison_result <- data.table::as.data.table(comparison_result)

  comparison_result[, model := reorder(model, -relative_skill)]
  levels <- levels(comparison_result$model)


  get_fill_scale <- function(values, breaks, plot_scales) {
    values[is.na(values)] <- 1 # this would be either ratio = 1 or pval = 1
    scale <- cut(values, breaks = breaks,
                 include.lowest = TRUE,
                 right = FALSE,
                 labels = plot_scales)
    # scale[is.na(scale)] <- 0
    return(as.numeric(as.character(scale)))
  }

  if (type[1] == "together") {
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
    upper_triangle[, `:=` (model = factor(model, levels),
                           compare_against = factor(compare_against, levels))]

    # reverse y and x if larger is better
    if (!smaller_is_good) {
      data.table::setnames(upper_triangle,
                           c("model", "compare_against"),
                           c("compare_against", "model"))
    }

    # modify upper triangle ------------------------------------------------------
    # add columns where a model is compared with itself. make adj_pval NA
    # to plot it as grey later on
    equal <- data.table::data.table(model = levels,
                                    compare_against = levels,
                                    mean_scores_ratio = 1,
                                    pval = NA,
                                    adj_pval = NA)
    upper_triangle_complete <- data.table::rbindlist(list(upper_triangle,
                                                          equal), fill = TRUE)

    # define interest variable
    upper_triangle_complete[, var_of_interest := round(mean_scores_ratio, 2)]

    # implemnt breaks for colour heatmap
    breaks <- c(0, 0.1, 0.5, 0.75, 1, 1.33, 2, 10, Inf)
    plot_scales <- c(-1, -0.5, -0.25, 0, 0, 0.25, 0.5, 1)
    if (!smaller_is_good) {
      plot_scales <- rev(plot_scales)
    }
    upper_triangle_complete[, fill_col := get_fill_scale(var_of_interest,
                                                         breaks, plot_scales)]

    # create mean_scores_ratios in plot
    plot <- ggplot2::ggplot(upper_triangle_complete,
                            ggplot2::aes(x = compare_against,
                                         y = model,
                                         fill = fill_col)) +
      ggplot2::geom_tile(width = 0.98, height = 0.98) +
      ggplot2::geom_text(ggplot2::aes(label = var_of_interest),
                         na.rm = TRUE) +
      ggplot2::scale_fill_gradient2(low = "skyblue", mid = "grey95",
                                    high = "brown1",
                                    na.value = "lightgrey",
                                    midpoint = 0,
                                    limits = c(-1,1),
                                    name = NULL) +
      ggplot2::theme_light() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 1,
                                                         hjust=1, color = "brown4"),
                     axis.text.y = ggplot2::element_text(color = "steelblue4"),
                     panel.grid.major = ggplot2::element_blank(),
                     panel.grid.minor = ggplot2::element_blank(),
                     # panel.background = ggplot2::element_rect(fill = "grey90"),
                     # axis.line.y = ggplot2::element_line(color = "steelblue4", size = 4),
                     # axis.line.x = ggplot2::element_line(color = "brown3", size = 4),
                     legend.position = "none") +
      ggplot2::labs(x = "", y = "",
                    title = "Pairwise comparisons - mean_scores_ratio (upper) and pval (lower)") +
      ggplot2::coord_cartesian(expand = FALSE)

    # add pvalues to plot --------------------------------------------------------
    # obtain lower triangle for the pvalues
    lower_triangle <- data.table::copy(upper_triangle)
    data.table::setnames(lower_triangle,
                         c("model", "compare_against"),
                         c("compare_against", "model"))

    lower_triangle[, var_of_interest := round(adj_pval, 3)]
    # implemnt breaks for colour heatmap
    breaks <- c(0, 0.01, 0.05, 0.1, 1)
    plot_scales <- c(0.8, 0.5, 0.1, 0.000001)
    lower_triangle[, fill_col := get_fill_scale(var_of_interest,
                                                breaks, plot_scales)]

    fill_rule <- ifelse(lower_triangle$fill_col == 0.000001, "grey95", "palegreen3")
    lower_triangle[, var_of_interest := as.character(var_of_interest)]
    lower_triangle[, var_of_interest := ifelse(var_of_interest == "0",
                                               "< 0.001", var_of_interest)]

    plot <- plot +
      ggplot2::geom_tile(data = lower_triangle,
                         ggplot2::aes(alpha = fill_col),
                         fill = fill_rule,
                         color = "white",
                         width = 0.97, height = 0.97) +
      ggplot2::geom_text(data = lower_triangle,
                         ggplot2::aes(label = var_of_interest),
                         na.rm = TRUE)

  } else if (type[1] == "mean_scores_ratio") {
    comparison_result[, var_of_interest := round(mean_scores_ratio, 2)]

    # implemnt breaks for colour heatmap
    breaks <- c(0, 0.1, 0.5, 0.75, 1, 1.33, 2, 10, Inf)
    plot_scales <- c(-1, -0.5, -0.25, 0, 0, 0.25, 0.5, 1)
    comparison_result[, fill_col := get_fill_scale(var_of_interest,
                                                   breaks, plot_scales)]

    high_col = "brown1"

  } else {
    comparison_result[, var_of_interest := round(pval, 3)]
    # implemnt breaks for colour heatmap
    breaks <- c(0, 0.01, 0.05, 0.1, 1)
    plot_scales <- c(1, 0.5, 0.1, 0)
    comparison_result[, fill_col := get_fill_scale(var_of_interest,
                                                   breaks, plot_scales)]

    high_col = "palegreen3"
    comparison_result[, var_of_interest := as.character(var_of_interest)]
    comparison_result[, var_of_interest := ifelse(var_of_interest == "0",
                                                  "< 0.001", var_of_interest)]
  }

  plot <- ggplot2::ggplot(comparison_result,
                          ggplot2::aes(y = reorder(model, 1 / mean_scores_ratio, FUN = geom_mean_helper),
                                       x = reorder(compare_against, mean_scores_ratio, FUN = geom_mean_helper),
                                       fill = fill_col)) +
    ggplot2::geom_tile(color = "white",
                       width = 0.97, height = 0.97) +
    ggplot2::geom_text(ggplot2::aes(label = var_of_interest),
                       na.rm = TRUE) +
    ggplot2::scale_fill_gradient2(low = "skyblue", mid = "grey95",
                                  high = high_col,
                                  na.value = "lightgrey",
                                  midpoint = 0,
                                  limits = c(-1,1),
                                  name = NULL) +
    ggplot2::theme_light() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 1,
                                                       hjust=1),
                   legend.position = "none") +
    ggplot2::labs(x = "", y = "",
                  title = "Pairwise comparisons - p-value whether mean scores ratio equal to 1") +
    ggplot2::coord_cartesian(expand = FALSE)

  if (type[1] == "mean_scores_ratio") {
    plot <- plot +
      ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                     panel.grid.minor = ggplot2::element_blank(),
                     axis.text.x = ggplot2::element_text(angle = 90, vjust = 1,
                                                         hjust=1, color = "brown4"),
                     axis.text.y = ggplot2::element_text(color = "steelblue4")) +
      ggplot2::ggtitle("Pairwise comparisons - ratio of mean scores (for overlapping forecast sets)")
  }

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




#' @title Infer metric for pairwise comparisons
#'
#' @description
#' Helper function to infer the metric for which pairwise comparisons shall
#' be made. The function simply checks the names of the available columns and
#' chooses the most widely used metric.
#' @param scores A data.table of scores as produced by [eval_forecasts()]
#' @keywords internal

infer_rel_skill_metric <- function(scores) {

  if ("interval_score" %in% colnames(scores)) {
    rel_skill_metric <- "interval_score"
  } else if ("crps" %in% colnames(scores)) {
    rel_skill_metric <- "crps"
  } else if ("brier_score" %in% colnames(scores)) {
    rel_skill_metric <- "brier_score"
  } else {
    stop("automatically assigning a metric to compute relative skills on failed. ",
         "Please provide a metric.")
  }

  return(rel_skill_metric)
}







