#' @title Obtain pairwise comparisons between models
#'
#' @description
#'
#' Compare scores obtained by different models in a pairwise tournament. All
#' combinations of two models are compared against each other based on the
#' overlapping set of available forecasts common to both models.
#'
#' The input should be a `scores` object as produced by [score()]. Note that
#' adding additional unrelated columns can unpredictably change results, as
#' all present columns are taken into account when determining the set of
#' overlapping forecasts between two models.
#'
#' The output of the pairwise comparisons is a set of mean score ratios,
#' relative skill scores and p-values.
#'
#' \if{html}{
#'   \out{<div style="text-align: left">}
#'   \figure{pairwise-illustration.png}{options: style="width:750px;max-width:100\%;"}
#'   \out{</div><p>}
#'   Illustration of the pairwise comparison process.
#'
#' }
#' \if{latex}{
#'   \figure{pairwise-illustration.png}
#' }
#'
#' *Mean score ratios*
#'
#' For every pair of two models, a mean score ratio is computed. This is simply
#' the mean score of the first model divided by the mean score of the second.
#' Mean score ratios are computed based on the set of overlapping forecasts
#' between the two models. That means that only scores for those targets are
#' taken into account for which both models have submitted a forecast.
#'
#' *(Scaled) Relative skill scores*
#'
#' The relative score of a model is the geometric mean of all mean score
#' ratios which involve that model.
#' If a baseline is provided, scaled relative skill scores will be calculated
#' as well. Scaled relative skill scores are simply the relative skill score of
#' a model divided by the relative skill score of the baseline model.
#'
#' *p-values*
#'
#' In addition, the function computes p-values for the comparison between two
#' models (again based on the set of overlapping forecasts). P-values can be
#' computed in two ways: based on a nonparametric Wilcoxon signed-rank test
#' (internally using [wilcox.test()] with `paired = TRUE`) or based on a
#' permutation test. The permutation test is based on the difference in mean
#' scores between two models. The default null hypothesis is that the mean score
#' difference is zero (see [permutation_test()]).
#' Adjusted p-values are computed by calling [p.adjust()] on the raw p-values.
#'
#' The code for the pairwise comparisons is inspired by an implementation by
#' Johannes Bracher.
#' The implementation of the permutation test follows the function
#' `permutationTest` from the `surveillance` package by Michael Höhle,
#' Andrea Riebler and Michaela Paul.
#'
#' @param compare Character vector with a single colum name that defines the
#'   elements for the pairwise comparison. For example, if this is set to
#'   "model" (the default), then elements of the "model" column will be
#'   compared.
#' @param by Character vector with column names that define further grouping
#'   levels for the pairwise comparisons. By default this is `NULL` and there
#'   will be one relative skill score per distinct entry of the column selected
#'   in `compare`. If further columns are given here, for example, `by =
#'   "location"` with `compare = "model"`, then one separate relative skill
#'   score is calculated for every model in every location.
#' @param metric A string with the name of the metric for which
#'   a relative skill shall be computed. By default this is either "crps",
#'   "wis" or "brier_score" if any of these are available.
#' @param baseline A string with the name of a model. If a baseline is
#'   given, then a scaled relative skill with respect to the baseline will be
#'   returned. By default (`NULL`), relative skill will not be scaled with
#'   respect to a baseline model.
#' @param ... Additional arguments for the comparison between two models. See
#'   [compare_forecasts()] for more information.
#' @inheritParams summarise_scores
#' @returns A data.table with the results of pairwise comparisons
#' containing the mean score ratios (`mean_scores_ratio`),
#' unadjusted (`pval`) and adjusted (`adj_pval`) p-values, and relative skill
#' values of each model (`..._relative_skill`). If a baseline model is given
#' then the scaled relative skill is reported as well
#' (`..._scaled_relative_skill`).
#' @importFrom data.table as.data.table data.table setnames copy
#' @importFrom stats sd rbinom wilcox.test p.adjust
#' @importFrom utils combn
#' @importFrom checkmate assert_subset assert_character assert_disjunct
#' @importFrom cli cli_abort cli_inform cli_warn
#' @export
#' @author Nikos Bosse \email{nikosbosse@@gmail.com}
#' @author Johannes Bracher, \email{johannes.bracher@@kit.edu}
#' @keywords scoring
#' @examples
#' \dontshow{
#'   data.table::setDTthreads(2) # restricts number of cores used on CRAN
#' }
#'
#' library(magrittr) # pipe operator
#'
#' scores <- example_quantile %>%
#'  as_forecast_quantile() %>%
#'  score()
#'
#' pairwise <- get_pairwise_comparisons(scores, by = "target_type")
#' pairwise2 <- get_pairwise_comparisons(
#'   scores, by = "target_type", baseline = "EuroCOVIDhub-baseline"
#' )
#'
#' library(ggplot2)
#' plot_pairwise_comparisons(pairwise, type = "mean_scores_ratio") +
#'   facet_wrap(~target_type)

get_pairwise_comparisons <- function(
  scores,
  compare = "model",
  by = NULL,
  metric = intersect(c("wis", "crps", "brier_score"), names(scores)),
  baseline = NULL,
  ...
) {

  # input checks ---------------------------------------------------------------
  scores <- ensure_data.table(scores)
  # check that column in 'compare' is present
  assert(check_columns_present(scores, compare))
  # check that column(s) in `by` ar not in `compare`
  assert_disjunct(by, compare)

  # we need the score names attribute to make sure we can determine the
  # forecast unit correctly, so here we check it exists
  metrics <- get_metrics.scores(scores, error = TRUE)

  # check that metric is a subset of the scores and is of length 1
  assert_subset(metric, metrics, empty.ok = FALSE)
  assert_character(metric, len = 1)

  # check that columns in 'by' are present
  #nolint start: keyword_quote_linter object_usage_linter
  if (length(by) > 0) {
    by_cols <- check_columns_present(scores, by)
    if (!isTRUE(by_cols)) {
      cli_abort(
        c(
          "!" = "Not all columns specified in `by` are present: {.var {by_cols}}"
        )
      )
      #nolint end
    }
  } else {
    ## set explicitly to character(0) in case it was given as NULL
    by <- character(0)
  }

  # check that baseline exists
  comparators <- as.vector(unique(scores[[compare]]))
  assert_subset(baseline, comparators)

  # check there are enough comparators
  if (length(setdiff(comparators, baseline)) < 2) {
    #nolint start: keyword_quote_linter
    cli_abort(
      c(
        "!" = "More than one non-baseline model is needed to compute
        pairwise compairisons."
      )
    )
    #nolint end
  }

  # check that values of the chosen metric are not NA
  if (anyNA(scores[[metric]])) {
    scores <- scores[!is.na(scores[[metric]])]
    if (nrow(scores) == 0) {
      #nolint start: keyword_quote_linter object_usage_linter
      cli_abort(
        c(
          "!" = "After removing {.val NA} values for {.var {metric}},
         no values were left."
        )
      )
    }
    cli_warn(
      c(
        "!" = "Some values for the metric {.var {metric}}
         are NA. These have been removed.",
        "i" = "Maybe choose a different metric?"
      )
    )
    #nolint end
  }

  # check that all values of the chosen metric are positive
  if (any(sign(scores[[metric]]) < 0) && any(sign(scores[[metric]]) > 0)) {
    #nolint start: keyword_quote_linter object_usage_linter
    cli_abort(
      c(
        "!" = "To compute pairwise comparisons, all values of {.var {metric}}
       must have the same sign."
      )
    )
    #nolint end
  }

  # identify unit of single observation.
  forecast_unit <- get_forecast_unit(scores)

  # if compare is equal to forecast_unit, then pairwise comparisons don't make
  # sense
  # if compare == forecast_unit then all relative skill scores will simply be 1.
  if (setequal(compare, forecast_unit)) {
    #nolint start: keyword_quote_linter
    cli_warn(
      c(
        "!" = "`compare` is set to the unit of a single forecast. This doesn't
            look right.",
        "i" = "All relative skill scores will be equal to 1."
      )
    )
    #nolint end
  } else if (setequal(c(compare, by), forecast_unit)) {
    #nolint start: keyword_quote_linter
    cli_inform(
      c(
        "!" = "relative skill can only be computed if the combination of
        `compare` and `by` is different from the unit of a single forecast.",
        "i" = "`by` was set to an empty character vector"
      )
    )
    #nolint end
    by <- character(0)
  }

  # do the pairwise comparison -------------------------------------------------
  # split data set into groups determined by 'by'
  split_scores <- split(scores, by = by)

  results <- lapply(split_scores,
    FUN = function(scores) {
      pairwise_comparison_one_group(
        scores = scores,
        metric = metric,
        baseline = baseline,
        compare = compare,
        by = by,
        ...
      )
    }
  )

  out <- data.table::rbindlist(results)

  return(out[])
}

#' @title Do pairwise comparison for one set of forecasts
#'
#' @description
#' This function does the pairwise comparison for one set of forecasts, but
#' multiple models involved. It gets called from [get_pairwise_comparisons()].
#' [get_pairwise_comparisons()] splits the data into arbitrary subgroups
#' specified by the user (e.g. if pairwise comparison should be done separately
#' for different forecast targets) and then the actual pairwise comparison for
#' that subgroup is managed from [pairwise_comparison_one_group()]. In order to
#' actually do the comparison between two models over a subset of common
#' forecasts it calls [compare_forecasts()].
#' @inherit get_pairwise_comparisons params return
#' @importFrom cli cli_abort
#' @importFrom data.table setnames
#' @keywords internal

pairwise_comparison_one_group <- function(scores,
                                          metric,
                                          baseline,
                                          compare = "model",
                                          by,
                                          ...) {
  if (!(compare %in% names(scores))) {
    cli_abort(
      "pairwise comparisons require a column as given by `compare`"
    )
  }

  # get list of models
  comparators <- unique(scores[[compare]])

  # if there aren't enough models to do any comparison, abort
  if (length(comparators) < 2) {
    cli_abort(
      c("!" = "There are not enough comparators to do any comparison")
    )
  }

  # create a data.frame with results
  # we only need to do the calculation once, because for the ratio that
  # should just be the inverse and for the permutation the result should
  # be the same

  # set up initial data.frame with all possible pairwise comparisons
  combinations <- data.table::as.data.table(t(combn(comparators, m = 2)))
  colnames(combinations) <- c("..compare", "compare_against")

  combinations[, c("ratio", "pval") := compare_forecasts(
    compare = compare,
    scores = scores,
    name_comparator1 = ..compare,
    name_comparator2 = compare_against,
    metric = metric,
    ...
  ),
  by = seq_len(NROW(combinations))
  ]

  combinations <- combinations[order(ratio)]
  combinations[, adj_pval := p.adjust(pval)]

  # mirror computations
  combinations_mirrored <- data.table::copy(combinations)
  setnames(combinations_mirrored,
    old = c("..compare", "compare_against"),
    new = c("compare_against", "..compare")
  )
  combinations_mirrored[, ratio := 1 / ratio]

  # add a one for those that are the same
  combinations_equal <- data.table::data.table(
    ..compare = comparators,
    compare_against = comparators,
    ratio = 1,
    pval = 1,
    adj_pval = 1
  )
  result <- data.table::rbindlist(list(
    combinations,
    combinations_mirrored,
    combinations_equal
  ),
  use.names = TRUE
  )

  # make result character instead of factor
  result[, `:=`(
    ..compare = as.character(..compare),
    compare_against = as.character(compare_against)
  )]

  setnames(result, old = "..compare", new = compare)

  # calculate relative skill as geometric mean
  # small theta is again better (assuming that the score is negatively oriented)
  result[, `:=`(
    theta = geometric_mean(ratio)
  ),
  by = compare,
  ]

  if (!is.null(baseline)) {
    baseline_theta <- unique(result[get(compare) == baseline, ]$theta)
    if (length(baseline_theta) == 0) {
      cli_abort(
        "Baseline comparator {.var {baseline}} missing."
      )
    }
    result[, rel_to_baseline := theta / baseline_theta]
  }

  # remove all the rows that are not present in by before merging
  cols_to_keep <- unique(c(by, compare))
  cols_to_remove <- colnames(scores)[!(colnames(scores) %in% cols_to_keep)]
  scores[, eval(cols_to_remove) := NULL]
  scores <- unique(scores)
  # allow.cartesian needs to be set as sometimes rows will be duplicated a lot
  out <- merge(scores, result, by = compare, all = TRUE)

  # rename ratio to mean_scores_ratio
  data.table::setnames(out,
    old = c("ratio", "theta"),
    new = c(
      "mean_scores_ratio",
      paste(metric, "relative_skill", sep = "_")
    )
  )
  if (!is.null(baseline)) {
    data.table::setnames(out,
      old = "rel_to_baseline",
      new = paste(metric, "scaled_relative_skill", sep = "_")
    )
  }

  return(out[])
}

#' @title Compare a subset of common forecasts
#'
#' @description
#' This function compares two comparators based on the subset of forecasts for which
#' both comparators have made a prediction. It gets called
#' from [pairwise_comparison_one_group()], which handles the
#' comparison of multiple comparators on a single set of forecasts (there are no
#' subsets of forecasts to be distinguished). [pairwise_comparison_one_group()]
#' in turn gets called from from [get_pairwise_comparisons()] which can handle
#' pairwise comparisons for a set of forecasts with multiple subsets, e.g.
#' pairwise comparisons for one set of forecasts, but done separately for two
#' different forecast targets.
#' @inheritParams get_pairwise_comparisons
#' @param name_comparator1 Character, name of the first comparator
#' @param name_comparator2 Character, name of the comparator to compare against
#' @param one_sided Boolean, default is `FALSE`, whether two conduct a one-sided
#'   instead of a two-sided test to determine significance in a pairwise
#'   comparison.
#' @param test_type Character, either "non_parametric" (the default), "permutation",
#'   or NULL. This determines which kind of test shall be conducted to determine
#'   p-values. If NULL, no test will be conducted and p-values will be NA.
#' @param n_permutations Numeric, the number of permutations for a
#'   permutation test. Default is 999.
#' @returns A list with mean score ratios and p-values for the comparison
#' between two comparators
#' @importFrom cli cli_abort
#' @author Johannes Bracher, \email{johannes.bracher@@kit.edu}
#' @author Nikos Bosse \email{nikosbosse@@gmail.com}
#' @keywords internal

compare_forecasts <- function(scores,
                              compare = "model",
                              name_comparator1,
                              name_comparator2,
                              metric,
                              one_sided = FALSE,
                              test_type = c("non_parametric", "permutation", NULL),
                              n_permutations = 999) {
  scores <- data.table::as.data.table(scores)

  forecast_unit <- get_forecast_unit(scores)

  if (!(compare %in% names(scores))) {
    cli_abort(
      "pairwise comparisons require a column as given by `compare`"
    )
  }

  # select only columns in c(by, var)
  a <- scores[get(compare) == name_comparator1]
  b <- scores[get(compare) == name_comparator2]

  # remove compare column from 'by' before merging
  merge_by <- setdiff(forecast_unit, compare)

  overlap <- merge(a, b, by = merge_by, allow.cartesian = TRUE)
  overlap <- unique(overlap)

  if (nrow(overlap) == 0) {
    return(list(ratio = NA_real_, pval = NA_real_))
  }

  values_x <- overlap[[paste0(metric, ".x")]]
  values_y <- overlap[[paste0(metric, ".y")]]

  # calculate ratio to of average scores achieved by both comparator.
  # this should be equivalent to theta_ij in Johannes Bracher's document.
  # ratio < 1 --> comparator 1 is better.
  # note we could also take mean(values_x) / mean(values_y), as it cancels out
  ratio <- sum(values_x) / sum(values_y)

  # If test_type is NULL, return NA for p-value
  if (is.null(test_type)) {
    pval <- NA_real_
  } else {
    # test whether the ratio is significantly different from one
    # equivalently, one can test whether the difference between the two values
    # is significantly different from zero.
    test_type <- match.arg(test_type)
    if (test_type == "permutation") {
      # adapted from the surveillance package
      pval <- permutation_test(values_x, values_y,
        n_permutation = n_permutations,
        one_sided = one_sided,
        comparison_mode = "difference"
      )
    } else {
      # this probably needs some more thought
      # alternative: do a paired t-test on ranks?
      pval <- wilcox.test(values_x, values_y, paired = TRUE)$p.value
    }
  }

  return(list(
    mean_scores_ratio = ratio,
    pval = pval
  ))
}


#' @title Calculate geometric mean
#'
#' @details
#' Used in [get_pairwise_comparisons()].
#'
#' @param x Numeric vector of values for which to calculate the geometric mean.
#' @returns The geometric mean of the values in `x`. `NA` values are ignored.
#'
#' @keywords internal
geometric_mean <- function(x) {
  geometric_mean <- exp(mean(log(x[!is.na(x)])))
  return(geometric_mean)
}

#' @title Simple permutation test
#'
#' @description
#' The implementation of the permutation test follows the
#' function
#' `permutationTest` from the `surveillance` package by Michael Höhle,
#' Andrea Riebler and Michaela Paul.
#' The function compares two vectors of scores. It computes the mean of each
#' vector independently and then takes either the difference or the ratio of
#' the two. This observed difference or ratio is compared against the same
#' test statistic based on permutations of the original data.
#'
#' Used in [get_pairwise_comparisons()].
#'
#' @param scores1 Vector of scores to compare against another vector of scores.
#' @param scores2 A second vector of scores to compare against the first
#' @param n_permutation The number of replications to use for a permutation
#' test. More replications yield more exact results, but require more
#' computation.
#' @param one_sided Whether or not to compute a one-sided test. Default is
#'   `FALSE`.
#' @param comparison_mode How to compute the test statistic for the comparison
#'   of the two scores. Should be either "difference" or "ratio".
#'
#' @returns p-value of the permutation test
#' @keywords internal
permutation_test <- function(scores1,
                             scores2,
                             n_permutation = 999,
                             one_sided = FALSE,
                             comparison_mode = c("difference", "ratio")) {
  nTime <- length(scores1)
  meanscores1 <- mean(scores1)
  meanscores2 <- mean(scores2)
  comparison_mode <- match.arg(comparison_mode)
  if (comparison_mode == "ratio") {
    # distinguish between on-sided and two-sided:
    test_stat_observed <- ifelse(
      one_sided,
      meanscores1 / meanscores2,
      max(meanscores1 / meanscores2, meanscores2 / meanscores1)
    )
  } else {
    test_stat_observed <- ifelse(
      one_sided,
      meanscores1 - meanscores2,
      abs(meanscores1 - meanscores2)
    )
  }
  test_stat_permuted <- replicate(n_permutation, {
    sel <- rbinom(nTime, size = 1, prob = 0.5)
    g1 <- (sum(scores1[sel == 0]) + sum(scores2[sel == 1])) / nTime
    g2 <- (sum(scores1[sel == 1]) + sum(scores2[sel == 0])) / nTime
    if (comparison_mode == "ratio") {
      ifelse(one_sided, g1 / g2, max(g1 / g2, g2 / g1))
    } else {
      ifelse(one_sided, g1 - g2, abs(g1 - g2))
    }
  })
  pVal <- (1 + sum(test_stat_permuted >= test_stat_observed)) / (n_permutation + 1)
  # plus ones to make sure p-val is never 0?
  return(pVal)
}


#' Add relative skill scores based on pairwise comparisons
#'
#' Adds a columns with relative skills computed by running
#' pairwise comparisons on the scores.
#' For more information on
#' the computation of relative skill, see [get_pairwise_comparisons()].
#' Relative skill will be calculated for the aggregation level specified in
#' `by`.
#'
#' @inheritParams get_pairwise_comparisons
#' @export
#' @keywords scoring
add_relative_skill <- function(
  scores,
  compare = "model",
  by = NULL,
  metric = intersect(c("wis", "crps", "brier_score"), names(scores)),
  baseline = NULL,
  ...
) {

  # input checks are done in `get_pairwise_comparisons()`
  # do pairwise comparisons ----------------------------------------------------
  pairwise <- get_pairwise_comparisons(
    scores = scores,
    metric = metric,
    baseline = baseline,
    compare = compare,
    by = by,
    ...
  )

  # store original metrics
  metrics <- get_metrics.scores(scores)

  # delete unnecessary columns
  pairwise[, c(
    "compare_against", "mean_scores_ratio",
    "pval", "adj_pval"
  ) := NULL]
  pairwise <- unique(pairwise)

  # merge back
  scores <- merge(
    scores, pairwise, all.x = TRUE, by = get_forecast_unit(pairwise)
  )

  # Update score names
  new_metrics <- paste(
    metric, c("relative_skill", "scaled_relative_skill"),
    sep = "_"
  )
  new_metrics <- new_metrics[new_metrics %in% names(scores)]
  scores <- new_scores(scores, metrics = c(metrics, new_metrics))

  return(scores)
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
#' library(magrittr) # pipe operator
#' scores <- example_quantile %>%
#'   as_forecast_quantile %>%
#'   score()
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
    scale <- cut(
      values,
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
