test_that("pairwise_comparison() works", {
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
    .(quantile = quantiles, value = quantile(value, quantiles)),
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

  data_formatted <- scoringutils::merge_pred_and_obs(
    forecasts_formatted,
    truth_formatted
  )

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
    add_pairwise_comparison(eval_without_rel_skill)
  )

  eval_with_baseline <- suppressMessages(
    add_pairwise_comparison(eval_without_rel_skill, baseline = "m1")
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
  eval_with_baseline <- add_pairwise_comparison(eval_summarised, baseline = "m1")

  relative_skills_with <- eval_with_baseline[
    location == "location_3",
    .(
      model = unique(model),
      relative_skill = unique(wis_scaled_relative_skill)
    )
  ]

  expect_equal(relative_skills_with$relative_skill, ratios_scaled)
})

test_that("pairwise_comparison() work in score() with integer data", {
  eval <- suppressMessages(score(data = example_integer))
  eval_summarised <- summarise_scores(eval, by = "model")
  eval <- add_pairwise_comparison(eval_summarised)
  expect_true("crps_relative_skill" %in% colnames(eval))
})


test_that("pairwise_comparison() work in score() with binary data", {
  eval <- suppressMessages(score(data = example_binary))
  eval_summarised <- summarise_scores(eval, by = "model")
  eval <- add_pairwise_comparison(eval_summarised)
  expect_true("brier_score_relative_skill" %in% colnames(eval))
})


# tests for pairwise comparison function ---------------------------------------

test_that("pairwise_comparison() works", {
  df <- data.frame(
    model = rep(c("model1", "model2", "model3"), each = 10),
    date = as.Date("2020-01-01") + rep(1:5, each = 2),
    location = c(1, 2),
    wis = (abs(rnorm(30))),
    ae_median = (abs(rnorm(30)))
  )

  res <- suppressMessages(pairwise_comparison(df, baseline = "model1"))

  colnames <- c(
    "model", "compare_against", "mean_scores_ratio",
    "pval", "adj_pval", "wis_relative_skill", "wis_scaled_relative_skill"
  )

  expect_true(all(colnames %in% colnames(res)))
})


test_that("pairwise_comparison() works inside and outside of score()", {
  eval <- scores_continuous

  pairwise <- suppressMessages(pairwise_comparison(eval,
    by = "model",
    rule = "crps"
  ))

  eval2_summarised <- summarise_scores(scores_continuous, by = "model")
  eval2 <- add_pairwise_comparison(eval2_summarised)

  expect_equal(
    sort(unique(pairwise$relative_skill)), sort(eval2$relative_skill)
  )
})

test_that("pairwise_comparison() realises when there is no baseline model", {
  expect_error(
    pairwise_comparison(scores_quantile, baseline = "missing_model"), "missing"
  )
})

test_that("Order of `add_pairwise_comparison()` and `summarise_scores()` doesn't matter", {
  pw1 <- suppressMessages(add_pairwise_comparison(scores_quantile))
  pw1_sum <- summarise_scores(pw1, by = "model")

  pw2 <- summarise_scores(scores_quantile, by = "model")
  pw2 <- add_pairwise_comparison(pw2)

  expect_true(all(pw1_sum == pw2, na.rm = TRUE))
  expect_true(all(names(attributes(pw2)) == names(attributes(pw1_sum))))
})
