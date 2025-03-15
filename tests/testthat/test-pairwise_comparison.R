test_that("get_pairwise_comparisons() works", {
  # define some toy data using a format suitable for github.com/reichlab/covidHubUtils
  test_truth <- data.frame(
    model = rep("truth_source", 30),
    location = paste0("location_", rep(1:3, each = 10)),
    target_end_date = as.Date("2020-01-01") + rep(1:10, times = 3),
    target_variable = "inc death",
    value = c(
      sort(c(4, 1, 5, 5, 6)), sort(c(12, 4, 5, 12, 53)),
      sort(c(8, 6, 3, 1, 46)), sort(c(6, 3, 5, 8, 5)),
      sort(c(3, 1, 5, 7, 7)), sort(c(3, 2, 6, 8, 5))
      )
  )

  test_forecasts <- data.table::as.data.table(test_truth)
  test_forecasts <- data.table::CJ(
    model = "m",
    model_sd = c(1, 2, 3, 4),
    samples = 1:100
  )
  test_forecasts[, model := paste0(model, model_sd)]
  test_forecasts <- merge(
    test_forecasts[, dummy_id := 1],
    data.table::as.data.table(test_truth)[, dummy_id := 1][, model := NULL],
    by = "dummy_id", allow.cartesian = TRUE
  )[, dummy_id := NULL]

  set.seed(123)
  test_forecasts[, value := sapply(value, function(x) {
      abs(rnorm(n = 1, mean = x, sd = model_sd))
    }),
    by = .(model, target_end_date, location, target_variable)
  ]

  quantiles <- c(0.05, 0.25, 0.5, 0.75, 0.95)
  test_forecasts <- test_forecasts[,
    .(quantile_level = quantiles, value = quantile(value, quantiles)),
    by = .(
      model, location, target_end_date, target_variable
    )
  ]

  # make a version of that data that conforms to scoringutils format
  truth_formatted <- data.table::as.data.table(test_truth)
  truth_formatted[, `:=`(model = NULL)]
  data.table::setnames(truth_formatted, old = "value", new = "observed")
  forecasts_formatted <- data.table::as.data.table(test_forecasts)
  data.table::setnames(forecasts_formatted, old = "value", new = "predicted")

  data_formatted <- merge(
    forecasts_formatted,
    truth_formatted
  ) %>%
    as_forecast_quantile()

  # evaluate the toy forecasts, once with and once without a baseline model specified
  eval <- score(data_formatted)

  # check with relative skills
  eval_without_rel_skill <- summarise_scores(
    eval,
    by = c(
      "model", "location", "target_end_date",
      "target_variable"
    )
  )
  eval_without_baseline <- suppressMessages(
    add_relative_skill(eval_without_rel_skill)
  )

  eval_with_baseline <- suppressMessages(
    add_relative_skill(eval_without_rel_skill, baseline = "m1")
  )


  # extract the relative_skill values
  relative_skills_without <- eval_without_baseline[, .(
    model = unique(model),
    relative_skill = unique(wis_relative_skill)
  )]
  relative_skills_with <- eval_with_baseline[, .(
    model = unique(model),
    relative_skill = unique(wis_scaled_relative_skill)
  )]

  # prepare scores for the code Johannes Bracher wrote
  scores_johannes <- data.table::copy(eval_without_baseline) # doesn't matter which one
  data.table::setnames(scores_johannes,
    old = c("location", "target_end_date", "wis"),
    new = c("unit", "timezero", "wis")
  )


  # -----------------------------------------------------------------------------#
  ## rerun code from Johannes Bracher to see whether results agree
  pairwise_comparison_jb <- function(scores, mx, my, subset = rep(TRUE, nrow(scores)),
                                     permutation_test = FALSE) {
    # apply subset:
    scores <- scores[subset, ]

    # subsets of available scores for both models:
    subx <- subset(scores, model == mx)
    suby <- subset(scores, model == my)

    # merge together and restrict to overlap:
    sub <- merge(subx, suby,
      by = c("timezero", "unit"),
      all.x = FALSE, all.y = FALSE
    )

    # compute ratio:
    ratio <- sum(sub$wis.x) / sum(sub$wis.y)

    # perform permutation tests:
    if (permutation_test) {
      pval <- scoringutils:::permutation_test(sub$wis.x, sub$wis.y,
        n_permutation = 999,
        comparison_mode = "difference"
      )

      # aggregate by forecast date:
      sub_fcd <- aggregate(cbind(wis.x, wis.y) ~ timezero, data = sub, FUN = mean)
      pval_fcd <- scoringutils:::permutation_test(sub_fcd$wis.x, sub_fcd$wis.y,
        n_permutation = 999
      )
    } else {
      pval <- NULL
      pval_fcd <- NULL
    }

    return(list(ratio = ratio, pval = pval, pval_fcd = pval_fcd, mx = mx, my = my))
  }

  models <- paste0("m", 1:4)
  # matrices to store:
  results_ratio <- results_pval <- results_pval_fcd <- matrix(
    ncol = length(models),
    nrow = length(models),
    dimnames = list(models, models)
  )

  set.seed(123) # set seed for permutation tests
  for (mx in seq_along(models)) {
    for (my in 1:mx) {
      pwc <- pairwise_comparison_jb(
        scores = scores_johannes, mx = models[mx], my = models[my],
        permutation_test = TRUE
      )
      results_ratio[mx, my] <- pwc$ratio
      results_ratio[my, mx] <- 1 / pwc$ratio
      results_pval[mx, my] <-
        results_pval[my, mx] <- pwc$pval
      results_pval_fcd[mx, my] <-
        results_pval_fcd[my, mx] <- pwc$pval_fcd
    }
  }
  # -----------------------------------------------------------------------------#

  # compare results without a baseline specified
  geometric_mean_ratios <- exp(rowMeans(log(results_ratio), na.rm = TRUE))
  names(geometric_mean_ratios) <- NULL
  expect_equal(relative_skills_without$relative_skill, geometric_mean_ratios)

  # comparison with a baseline
  ind_baseline <- which(rownames(results_ratio) == "m1")
  geometric_mean_ratios <- exp(rowMeans(log(results_ratio[, -ind_baseline]), na.rm = TRUE))
  ratios_baseline <- results_ratio[, "m1"]
  ratios_scaled <- geometric_mean_ratios / geometric_mean_ratios["m1"]

  names(ratios_scaled) <- NULL
  expect_equal(relative_skills_with$relative_skill, ratios_scaled)


  # scoringutils can also do pairwise comparisons for different subcategories
  # comparison for a subset of the data vs. spliting by category within scoringutils
  scores_johannes_subset <- scores_johannes[unit == "location_3"]
  results_ratio <- results_pval <- results_pval_fcd <- matrix(
    ncol = length(models),
    nrow = length(models),
    dimnames = list(models, models)
  )

  set.seed(123) # set seed for permutation tests
  for (mx in seq_along(models)) {
    for (my in 1:mx) {
      pwc <- pairwise_comparison_jb(
        scores = scores_johannes_subset, mx = models[mx], my = models[my],
        permutation_test = TRUE
      )
      results_ratio[mx, my] <- pwc$ratio
      results_ratio[my, mx] <- 1 / pwc$ratio
      results_pval[mx, my] <-
        results_pval[my, mx] <- pwc$pval
      results_pval_fcd[mx, my] <-
        results_pval_fcd[my, mx] <- pwc$pval_fcd
    }
  }
  ind_baseline <- which(rownames(results_ratio) == "m1")
  geometric_mean_ratios <- exp(rowMeans(log(results_ratio[, -ind_baseline]), na.rm = TRUE))
  ratios_baseline <- results_ratio[, "m1"]
  ratios_scaled <- geometric_mean_ratios / geometric_mean_ratios["m1"]
  names(ratios_scaled) <- NULL

  eval <- score(data_formatted)
  eval_summarised <- summarise_scores(eval, by = c("model", "location"))
  eval_with_baseline <- add_relative_skill(eval, by = "location", baseline = "m1")
  eval_with_baseline <- summarise_scores(eval_with_baseline, by = c("model", "location"))

  relative_skills_with <- eval_with_baseline[
    location == "location_3",
    .(
      model = unique(model),
      relative_skill = unique(wis_scaled_relative_skill)
    )
  ]

  expect_equal(relative_skills_with$relative_skill, ratios_scaled)
})

test_that("get_pairwise_comparisons() work in score() with integer data", {
  eval <- suppressMessages(score(forecast = as_forecast_sample(example_sample_discrete)))
  eval_summarised <- summarise_scores(eval, by = c("model", "target_type"))
  eval <- add_relative_skill(eval_summarised)
  expect_true("crps_relative_skill" %in% colnames(eval))
})


test_that("get_pairwise_comparisons() work in score() with binary data", {
  eval <- suppressMessages(score(forecast = as_forecast_binary(example_binary)))
  eval_summarised <- summarise_scores(eval, by = c("model", "target_type"))
  eval <- add_relative_skill(eval_summarised)
  expect_true("brier_score_relative_skill" %in% colnames(eval))
})


# tests for pairwise comparison function ---------------------------------------

test_that("get_pairwise_comparisons() works", {
  df <- data.frame(
    model = rep(c("model1", "model2", "model3"), each = 10),
    date = as.Date("2020-01-01") + rep(1:5, each = 2),
    location = c(1, 2),
    wis = (abs(rnorm(30))),
    ae_median = (abs(rnorm(30)))
  )
  attr(df, "metrics") <- c("wis", "ae_median")

  res <- suppressMessages(get_pairwise_comparisons(df, baseline = "model1"))

  colnames <- c(
    "model", "compare_against", "mean_scores_ratio",
    "pval", "adj_pval", "wis_relative_skill", "wis_scaled_relative_skill"
  )

  expect_true(all(colnames %in% colnames(res)))

  # output class is as expected
  expect_s3_class(res, c("data.table", "data.frame"), exact = TRUE)
  expect_s3_class(
    get_pairwise_comparisons(scores_quantile),
    c("data.table", "data.frame"), exact = TRUE
  )
})


test_that("get_pairwise_comparisons() and `add_relative_skill()` give same result", {
  eval <- scores_sample_continuous

  pairwise <- get_pairwise_comparisons(eval,
    compare = "model",
    metric = "crps"
  )

  eval2 <- add_relative_skill(scores_sample_continuous, compare = "model")
  eval2 <- summarise_scores(eval2, by = "model")

  expect_equal(
    sort(unique(pairwise$crps_relative_skill)), sort(eval2$crps_relative_skill)
  )
})

test_that("get_pairwise_comparisons() realises when there is no baseline model", {
  expect_error(
    get_pairwise_comparisons(scores_quantile, baseline = "missing_model"),
    "Assertion on 'baseline' failed: Must be a subset of"
  )
})

test_that("Basic input checks for `add_relative_skill() work", {
  eval <- data.table::copy(scores_sample_continuous)

  # check that model column + columns in 'by' + baseline model are present
  expect_error(
    add_relative_skill(
      eval, compare = "model", by = "missing", metric = "crps"
    ),
    "Not all columns specified in `by` are present:"
  )

  # check that none of the columns in `by` are in `compare`
  expect_error(
    add_relative_skill(
      eval, by = c("model", "target_type"), metric = "crps"
    ),
    "Must be disjunct from \\{'model'\\}"
  )

  # error if baseline is not present
  expect_error(
    add_relative_skill(
      eval, compare = "model", baseline = "missing", metric = "crps"
    ),
    "Assertion on 'baseline' failed: Must be a subset of"
  )

  # error if not enough models are present
  eval_few <- eval[model %in% c("EuroCOVIDhub-ensemble", "EuroCOVIDhub-baseline")]
  expect_no_error(
    add_relative_skill(
      eval_few, compare = "model", metric = "crps"
    )
  )
  expect_error(
    add_relative_skill(
      eval_few, compare = "model", baseline = "EuroCOVIDhub-baseline",
      metric = "crps"
    ),
    "More than one non-baseline model is needed to compute pairwise compairisons."
  )

  # error if no relative skill metric is found
  expect_error(
    add_relative_skill(
      eval, compare = "model",
      metric = "missing"
    )
  )
  eval_nometric <- data.table::copy(eval)[, "crps" := NULL]
  expect_error(
    suppressWarnings(add_relative_skill(
      eval_nometric, compare = "model"
    )),
    "Assertion on 'metric' failed: Must be a subset of "
  )

  # error if no model column is found
  eval_nomodel <- data.table::copy(eval)[, "model" := NULL]
  expect_error(
    add_relative_skill(
      eval_nomodel, by = "target_type", metric = "crps"
    ),
    "Assertion on 'scores' failed: Column 'model' not found in data."
  )

  # error if there isn't a metrics attribute
  eval_noattribute <- data.table::copy(eval)
  attr(eval_noattribute, "metrics") <- NULL
  expect_error(
    add_relative_skill(
      eval_noattribute, compare = "model", metric = "crps"
    ),
    "needs an attribute `metrics`"
  )

  # warning if there are NAs in the column for which a relative metric is computed
  eval_nas <- data.table::copy(eval)
  eval_nas[1:10, "crps" := NA]
  expect_warning(
    add_relative_skill(
      eval_nas, compare = "model", metric = "crps"
    ),
    "Some values for the metric `crps` are NA. These have been removed."
  )

  # warning if there are no values left after removing NAs
  eval_nas[, "crps" := NA]
  expect_error(
    add_relative_skill(
      eval_nas, compare = "model", metric = "crps"
    ),
    "After removing \"NA\" values for `crps`, no values were left."
  )

  # error if not all values for the relative skill metric have the same sign
  eval_diffsign <- data.table::copy(eval)
  eval_diffsign[1:10, "crps" := -eval_diffsign[1:10, "crps"]]
  expect_error(
    add_relative_skill(
      eval_diffsign, compare = "model", metric = "crps"
    ),
    "To compute pairwise comparisons, all values of `crps` must have the same sign."
  )

  # message if `by` is equal to the forecast unit
  fu <- get_forecast_unit(eval)
  expect_message(
    add_relative_skill(
      eval, compare = "model", by = setdiff(fu, "model"), metric = "crps"
    ),
    "relative skill can only be computed if the combination of `compare` and `by` is different from the unit of a single forecast."
  )

  # warning if by is equal to the forecast unit and also by is "model"
  eval_summ <- summarise_scores(eval, by = "model")
  expect_warning(
    add_relative_skill(
      eval_summ, compare = "model", metric = "crps"
    ),
    "`compare` is set to the unit of a single forecast."
  )
})

test_that("get_pairwise_comparisons() throws errors with wrong inputs", {
  test <- data.table::copy(scores_sample_continuous)
  test <- test[, "model" := NULL]

  # expect error if no model column is found
  expect_error(
    get_pairwise_comparisons(test, compare = "model", metric = "crps"),
    "Assertion on 'scores' failed: Column 'model' not found in data."
  )
})

test_that("pairwise_comparison_one_group() throws error with wrong inputs", {
  test <- data.table::copy(scores_sample_continuous)
  test <- test[, "model" := NULL]

  # expect error if no model column is found
  expect_error(
    pairwise_comparison_one_group(test, compare = "model", metric = "crps"),
    "pairwise comparisons require a column as given by `compare`"
  )

  # expect error as a result if scores has zero rows
  test <- data.table::copy(scores_sample_continuous)[model == "impossible"]
  expect_error(
    pairwise_comparison_one_group(test, compare = "model", metric = "crps"),
    "not enough comparators"
  )

  # expect error if there aren't enough models
  test <- data.table::copy(scores_sample_continuous)[model == "EuroCOVIDhub-ensemble"]
  expect_error(
    pairwise_comparison_one_group(test, compare = "model", metric = "crps"),
    "not enough comparators"
  )

  # expect error if baseline model is missing
  test <- data.table::copy(scores_sample_continuous)
  expect_error(
    pairwise_comparison_one_group(test, compare = "model", baseline = "missing", metric = "crps"),
    "Baseline comparator `missing` missing"
  )
})

test_that("compare_forecasts() throws error with wrong inputs", {
  test <- data.table::copy(scores_sample_continuous)
  test <- test[, "model" := NULL]

  # expect error if no model column is found
  expect_error(
    compare_forecasts(test, compare = "model", metric = "crps"),
    "pairwise comparisons require a column as given by `compare`"
  )
})

test_that("add_relative_skill() works with point forecasts", {
  expect_no_condition(
    pw_point <- add_relative_skill(
      scores_point,
      metric = "se_point"
    )
  )
  pw_point <- summarise_scores(pw_point, by = "model")

  pw_manual <- get_pairwise_comparisons(
    scores_point, compare = "model", metric = "se_point"
  )

  expect_equal(
    pw_point$relative_skill,
    unique(pw_manual$relative_skill)
  )
})

test_that("add_relative_skill() can compute relative measures", {
  scores_with <- add_relative_skill(
    scores_quantile
  )
  expect_s3_class(
    scores_with,
    c("scores", "data.table", "data.frame"),
    exact = TRUE
  )

  scores_with <- summarise_scores(scores_with, by = "model")

  expect_equal(
    scores_with[, wis_relative_skill],
    c(1.6, 0.81, 0.75, 1.03), tolerance = 0.01
  )

  scores_with <- add_relative_skill(
    scores_quantile, compare = "model",
    metric = "ae_median"
  )
  scores_with <- summarise_scores(scores_with, by = "model")

  expect_equal(
    scores_with[, ae_median_relative_skill],
    c(1.6, 0.78, 0.77, 1.04), tolerance = 0.01
  )
})

# test more esoteric options to make at least sure they don't produce an error
test_that("permutation_tests work as expected", {
  expect_no_condition(
    get_pairwise_comparisons(
      scores_quantile,
      test_type = "permutation",
      one_sided = TRUE,
      n_permutations = 50
    )
  )
})


# ==============================================================================
# plot_pairwise_comparison()
# ==============================================================================
pairwise <- get_pairwise_comparisons(scores_quantile, by = "target_type")

test_that("plot_pairwise_comparisons() works as expected", {
  p <- plot_pairwise_comparisons(pairwise) +
    ggplot2::facet_wrap(~target_type)
  expect_s3_class(p, "ggplot")
  skip_on_cran()
  vdiffr::expect_doppelganger("plot_pairwise_comparison", p)
})

test_that("plot_pairwise_comparisons() works when showing p values", {
  p <- plot_pairwise_comparisons(pairwise, type = "pval") +
    ggplot2::facet_wrap(~target_type)
  expect_s3_class(p, "ggplot")
  skip_on_cran()
  vdiffr::expect_doppelganger("plot_pairwise_comparison_pval", p)
})

test_that("add_relative_skill() works without warnings when not computing p-values", {
  forecast_quantile <- example_quantile %>%
    as_forecast_quantile(
      forecast_unit = c(
        "location", "forecast_date", "target_end_date",
        "target_type", "model", "horizon"
      )
    )

  scores <- forecast_quantile %>%
    score(metrics = get_metrics(forecast_quantile, "ae_median"))

  expect_no_warning(
    scores_w_rel_skill <- scores %>%
      add_relative_skill(
        compare = "model",
        by = "location",
        metric = "ae_median",
        test_type = NULL
      )
  )

  # Additional checks to ensure the function worked correctly
  expect_true("ae_median_relative_skill" %in% names(scores_w_rel_skill))
  expect_true(is.numeric(scores_w_rel_skill$ae_median_relative_skill))
  expect_false(any(is.na(scores_w_rel_skill$ae_median_relative_skill)))
})
