#' @title Do Pairwise Comparisons of Scores
#'
#' @description
#'
#' Compute relative scores between different models making pairwise
#' comparisons. Pairwise comparisons are a sort of pairwise tournament where all
#' combinations of two models are compared against each other based on the
#' overlapping set of available forecasts common to both models.
#' Internally, a ratio of the mean scores of both models is computed.
#' The relative score of a model is then the geometric mean of all mean score
#' ratios which involve that model. When a baseline is provided, then that
#' baseline is excluded from the relative scores for individual models
#' (which therefore differ slightly from relative scores without a baseline)
#' and all relative scores are scaled by (i.e. divided by) the relative score of
#' the baseline model.
#' Usually, the function input should be unsummarised scores as
#' produced by [score()].
#' Note that the function internally infers the *unit of a single forecast* by
#' determining all columns in the input that do not correspond to metrics
#' computed by [score()]. Adding unrelated columns will change results in an
#' unpredictable way.
#'
#' The code for the pairwise comparisons is inspired by an implementation by
#' Johannes Bracher.
#' The implementation of the permutation test follows the function
#' `permutationTest` from the `surveillance` package by Michael Höhle,
#' Andrea Riebler and Michaela Paul.
#'
#' @param scores A data.table of scores as produced by [score()].
#' @param metric A character vector of length one with the metric to do the
#' comparison on. The default is "auto", meaning that either "interval_score",
#' "crps", or "brier_score" will be selected where available.
#' See [available_metrics()] for available metrics.
#' @param by character vector with names of columns present in the input
#' data.frame. `by` determines how pairwise comparisons will be computed.
#' You will get a relative skill score for every grouping level determined in
#' `by`. If, for example, `by = c("model", "location")`. Then you will get a
#' separate relative skill score for every model in every location. Internally,
#' the data.frame will be split according `by` (but removing "model" before
#' splitting) and the pairwise comparisons will be computed separately for the
#' split data.frames.
#' @param baseline character vector of length one that denotes the baseline
#' model against which to compare other models.
#' @param ... additional arguments for the comparison between two models. See
#' [compare_two_models()] for more information.
#' @return A ggplot2 object with a coloured table of summarised scores
#' @importFrom data.table as.data.table data.table setnames copy
#' @importFrom stats sd rbinom wilcox.test p.adjust
#' @importFrom utils combn
#' @export
#' @author Nikos Bosse \email{nikosbosse@@gmail.com}
#' @author Johannes Bracher, \email{johannes.bracher@@kit.edu}
#' @keywords scoring
#' @examples
#' data.table::setDTthreads(1) # only needed to avoid issues on CRAN
#'
#' scores <- score(example_quantile)
#' pairwise <- pairwise_comparison(scores, by = "target_type")
#'
#' library(ggplot2)
#' plot_pairwise_comparison(pairwise, type = "mean_scores_ratio") +
#'   facet_wrap(~target_type)

pairwise_comparison <- function(scores,
                                by = c("model"),
                                metric = "auto",
                                baseline = NULL,
                                ...) {

  metric <- match.arg(metric, c("auto", available_metrics()))

  scores <- data.table::as.data.table(scores)

  # determine metric automatically
  if (metric == "auto") {
    metric <- infer_rel_skill_metric(scores)
  }

  # check that values of the chosen metric are not NA
  if (anyNA(scores[[metric]])) {
    msg <- paste0("Some values for the metric '", metric,
                  "' are NA. These have been removed. ",
                  "Maybe choose a different metric?")
    warning(msg)

    scores <- scores[!is.na(scores[[metric]])]
    if (nrow(scores) == 0) {
      msg <- paste0("After removing NA values for '", metric,
                    "', no values were left.")
      warning(msg)
      return(NULL)
    }
  }

  # check that all values of the chosen metric are positive
  if (any(sign(scores[[metric]]) < 0)) {
    if (any(sign(scores) > 0)) {
      msg <- paste("To compute pairwise comparisons, all values of", metric,
                   "must have the same sign.")
      stop(msg)
    }
  }

  # identify unit of single observation.
  forecast_unit <- get_forecast_unit(scores)

  # if by is equal to forecast_unit, then pairwise comparisons don't make sense
  if (setequal(by, forecast_unit)) {
    by <- "model"
    message("relative skill can only be computed if `by` is different from the unit of a single forecast. `by` was set to 'model'")
  }

  # summarise scores over everything (e.g. quantiles, ranges or samples) in
  # order to not to include those in the calculation of relative scores. Also
  # gets rid of all unnecessary columns and keep only metric and forecast unit
  scores <- scores[, lapply(.SD, mean, na.rm = TRUE),
    by = forecast_unit,
    .SDcols = metric
  ]

  # split data set into groups determined by 'by'
  split_by <- setdiff(by, "model")
  split_scores <- split(scores, by = split_by)

  results <- lapply(split_scores,
    FUN = function(scores) {
      out <- pairwise_comparison_one_group(
        scores = scores,
        metric = metric,
        baseline = baseline,
        by = by,
        ...
      )
    }
  )

  out <- data.table::rbindlist(results)

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
#' @keywords internal

pairwise_comparison_one_group <- function(scores,
                                          metric,
                                          baseline,
                                          by,
                                          ...) {
  if (!("model" %in% names(scores))) {
    stop("pairwise compairons require a column called 'model'")
  }

  if (nrow(scores) == 0) {
    return(NULL)
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

  combinations[, c("ratio", "pval") := compare_two_models(
    scores = scores,
    name_model1 = model,
    name_model2 = compare_against,
    metric = metric,
    ...
  ),
  by = seq_len(NROW(combinations))
  ]

  combinations <- combinations[order(ratio)]
  combinations[, adj_pval := p.adjust(pval)]

  # mirror computations
  combinations_mirrored <- data.table::copy(combinations)
  data.table::setnames(combinations_mirrored,
    old = c("model", "compare_against"),
    new = c("compare_against", "model")
  )
  combinations_mirrored[, ratio := 1 / ratio]

  # add a one for those that are the same
  combinations_equal <- data.table::data.table(
    model = models,
    compare_against = models,
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
    "model" = as.character(model),
    "compare_against" = as.character(compare_against)
  )]

  # calculate relative skill as geometric mean
  # small theta is again better (assuming that the score is negatively oriented)
  result[, `:=`(
    theta = geom_mean_helper(ratio),
    rel_to_baseline = NA_real_
  ),
  by = "model"
  ]

  if (!is.null(baseline)) {
    baseline_theta <- unique(result[model == baseline, ]$theta)
    if (length(baseline_theta) == 0) {
      stop("Baseline model ", baseline, " missing.")
    }
    result[, rel_to_baseline := theta / baseline_theta]
  }

  # remove all the rows that are not present in by before merging
  cols_to_keep <- unique(c(by, "model"))
  cols_to_remove <- colnames(scores)[!(colnames(scores) %in% cols_to_keep)]
  scores[, eval(cols_to_remove) := NULL]
  scores <- unique(scores)
  # allow.cartesian needs to be set as sometimes rows will be duplicated a lot
  out <- merge(scores, result, by = "model", all = TRUE)

  # rename ratio to mean_scores_ratio
  data.table::setnames(out,
    old = c("ratio", "theta", "rel_to_baseline"),
    new = c("mean_scores_ratio", "relative_skill", "scaled_rel_skill")
  )

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
#' @param one_sided Boolean, default is `FALSE`, whether two conduct a one-sided
#' instead of a two-sided test to determine significance in a pairwise
#' comparison.
#' @param test_type character, either "non_parametric" (the default) or
#' "permutation". This determines which kind of test shall be conducted to
#' determine p-values.
#' @param n_permutations numeric, the number of permutations for a
#' permutation test. Default is 999.
#' @author Johannes Bracher, \email{johannes.bracher@@kit.edu}
#' @author Nikos Bosse \email{nikosbosse@@gmail.com}
#' @keywords internal

compare_two_models <- function(scores,
                               name_model1,
                               name_model2,
                               metric,
                               one_sided = FALSE,
                               test_type = c("non_parametric", "permutation"),
                               n_permutations = 999) {
  scores <- data.table::as.data.table(scores)

  forecast_unit <- get_forecast_unit(scores)

  if (!("model" %in% names(scores))) {
    stop("pairwise comparisons require a column called 'model'")
  }

  # select only columns in c(by, var)
  a <- scores[model == name_model1]
  b <- scores[model == name_model2]

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
  return(list(
    mean_scores_ratio = ratio,
    pval = pval
  ))
}

#' @title Infer metric for pairwise comparisons
#'
#' @description
#' Helper function to infer the metric for which pairwise comparisons shall
#' be made. The function simply checks the names of the available columns and
#' chooses the most widely used metric.
#' @inheritParams pairwise_comparison
#' @keywords internal

infer_rel_skill_metric <- function(scores) {
  if ("interval_score" %in% colnames(scores)) {
    rel_skill_metric <- "interval_score"
  } else if ("crps" %in% colnames(scores)) {
    rel_skill_metric <- "crps"
  } else if ("brier_score" %in% colnames(scores)) {
    rel_skill_metric <- "brier_score"
  } else {
    stop(
      "automatically assigning a metric to compute relative skills on failed. ",
      "Please provide a metric."
    )
  }

  return(rel_skill_metric)
}
