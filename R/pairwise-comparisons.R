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
#' @param by character vector with column names that define the grouping level
#' for the pairwise comparisons. By default (`model`), there will be one
#' relative skill score per model. If, for example,
#' `by = c("model", "location")`. Then you will get a
#' separate relative skill score for every model in every location. Internally,
#' the data.table with scores will be split according `by` (removing "model"
#' before splitting) and the pairwise comparisons will be computed separately
#' for the split data.tables.
#' @param metric A string with the name of the metric for which
#' a relative skill shall be computed. By default this is either "crps",
#' "wis" or "brier_score" if any of these are available.
#' @param baseline A string with the name of a model. If a baseline is
#' given, then a scaled relative skill with respect to the baseline will be
#' returned. By default (`NULL`), relative skill will not be scaled with
#' respect to a baseline model.
#' @param ... additional arguments for the comparison between two models. See
#' [compare_two_models()] for more information.
#' @inheritParams summarise_scores
#' @return A data.table with pairwise comparisons
#' @importFrom data.table as.data.table data.table setnames copy
#' @importFrom stats sd rbinom wilcox.test p.adjust
#' @importFrom utils combn
#' @importFrom checkmate assert_subset assert_character
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
#' scores <- score(as_forecast(example_quantile))
#' pairwise <- pairwise_comparison(scores, by = "target_type")
#'
#' library(ggplot2)
#' plot_pairwise_comparison(pairwise, type = "mean_scores_ratio") +
#'   facet_wrap(~target_type)

pairwise_comparison <- function(
  scores,
  by = "model",
  metric = intersect(c("wis", "crps", "brier_score"), names(scores)),
  baseline = NULL,
  ...
) {

  # input checks ---------------------------------------------------------------
  scores <- ensure_data.table(scores)

  # we need the score names attribute to make sure we can determine the
  # forecast unit correctly, so here we check it exists
  metrics <- get_metrics(scores, error = TRUE)

  # check that metric is a subset of the scores and is of length 1
  assert_subset(metric, metrics, empty.ok = FALSE)
  assert_character(metric, len = 1)

  # check that model column + columns in 'by' are present
  #nolint start: keyword_quote_linter object_usage_linter
  by_cols <- check_columns_present(scores, by)
  if (!is.logical(by_cols)) {
    cli_abort(
      c(
        "!" = "Not all columns specified in `by` are present: {.var {by_cols}}"
      )
    )
    #nolint end
  }
  assert(check_columns_present(scores, "model"))

  # check that baseline is one of the existing models
  models <- as.vector(unique(scores$model))
  assert_subset(baseline, models)

  # check there are enough models
  if (length(setdiff(models, baseline)) < 2) {
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

  # if by is equal to forecast_unit, then pairwise comparisons don't make sense
  # if by == forecast_unit == "model" then this will pass and all relative skill
  # scores will simply be 1.
  if (setequal(by, forecast_unit)) {
    if (setequal(by, "model")) {
      #nolint start: keyword_quote_linter
      cli_warn(
        c(
          "!" = "`by` is set to 'model', which is also the unit of a single
         forecast. This doesn't look right.",
        "i" = "All relative skill scores will be equal to 1."
        )
      )
    } else {
      by <- "model"
      cli_inform(
        c(
          "!" = "relative skill can only be computed if `by` is different from the
        unit of a single forecast.",
        "i" = "`by` was set to 'model'"
        )
      )
      #nolint end
    }
  }

  # do the pairwise comparison -------------------------------------------------
  # split data set into groups determined by 'by'
  split_by <- setdiff(by, "model")
  split_scores <- split(scores, by = split_by)

  results <- lapply(split_scores,
    FUN = function(scores) {
      pairwise_comparison_one_group(
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
#' @inherit pairwise_comparison params return
#' @importFrom cli cli_abort
#' @keywords internal

pairwise_comparison_one_group <- function(scores,
                                          metric,
                                          baseline,
                                          by,
                                          ...) {
  if (!("model" %in% names(scores))) {
    cli_abort(
      "pairwise comparisons require a column called 'model'"
    )
  }

  # get list of models
  models <- unique(scores$model)

  # if there aren't enough models to do any comparison, abort
  if (length(models) < 2) {
    cli_abort(
      c("!" = "There are not enough models to do any comparison")
    )
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
    model = as.character(model),
    compare_against = as.character(compare_against)
  )]

  # calculate relative skill as geometric mean
  # small theta is again better (assuming that the score is negatively oriented)
  result[, `:=`(
    theta = geometric_mean(ratio)
  ),
  by = "model"
  ]

  if (!is.null(baseline)) {
    baseline_theta <- unique(result[model == baseline, ]$theta)
    if (length(baseline_theta) == 0) {
      cli_abort(
        "Baseline model {.var {baseline}} missing."
      )
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
#' @return A list with mean score ratios and p-values for the comparison
#' between two models
#' @importFrom cli cli_abort
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
    cli_abort(
      "pairwise comparisons require a column called 'model'"
    )
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


#' @title Calculate Geometric Mean
#'
#' @details
#' Used in [pairwise_comparison()].
#'
#' @param x numeric vector of values for which to calculate the geometric mean
#' @return the geometric mean of the values in `x`. `NA` values are ignored.
#'
#' @keywords internal
geometric_mean <- function(x) {
  geometric_mean <- exp(mean(log(x[!is.na(x)])))
  return(geometric_mean)
}

#' @title Simple permutation test
#'
#' @description The implementation of the permutation test follows the
#' function
#' `permutationTest` from the `surveillance` package by Michael Höhle,
#' Andrea Riebler and Michaela Paul.
#' The function compares two vectors of scores. It computes the mean of each
#' vector independently and then takes either the difference or the ratio of
#' the two. This observed difference or ratio is compared against the same
#' test statistic based on permutations of the original data.
#'
#' Used in [pairwise_comparison()].
#'
#' @param scores1 vector of scores to compare against another vector of scores
#' @param scores2 A second vector of scores to compare against the first
#' @param n_permutation The number of replications to use for a permutation
#' test. More replications yield more exact results, but require more
#' computation.
#' @param one_sided Whether or not to compute a one-sided test. Default is
#' `FALSE`,
#' @param comparison_mode How to compute the test statistic for the comparison
#' of the two scores. Should be either "difference" or "ratio".
#'
#' @return p-value of the permutation test
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


#' @title Add pairwise comparisons
#' @description Adds a columns with relative skills computed by running
#' pairwise comparisons on the scores.
#' For more information on
#' the computation of relative skill, see [pairwise_comparison()].
#' Relative skill will be calculated for the aggregation level specified in
#' `by`.
#' @inheritParams pairwise_comparison
#' @export
#' @keywords keyword scoring
add_pairwise_comparison <- function(
  scores,
  by = "model",
  metric = intersect(c("wis", "crps", "brier_score"), names(scores)),
  baseline = NULL
) {

  # input checks are done in `pairwise_comparison()`
  # do pairwise comparisons ----------------------------------------------------
  pairwise <- pairwise_comparison(
    scores = scores,
    metric = metric,
    baseline = baseline,
    by = by
  )

  # store original metrics
  metrics <- get_metrics(scores)

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
