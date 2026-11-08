# Tests that `score()` returns correct values on the example data.
#
# The reference implementations below are written directly from the
# definitions of the scoring rules in plain R. They deliberately do not call
# scoringRules or any scoringutils metric function, so that they provide an
# independent check of every default metric for every forecast type. They do
# share a few base R helpers with the package: `median()`, `mean()` and the
# bandwidth rule `bw.nrd()` of the sample log score.
# The snapshot tests at the end additionally guard the summarised scores of all
# example data sets against unintended changes to the example data or to
# `summarise_scores()`.

# The non-missing rows of an example data set as a plain data.table
forecast_data <- function(forecast) {
  as.data.table(na.omit(forecast))
}

# Compare the scores returned by `score()` with reference scores computed
# for each forecast. `by` are the columns that identify a forecast.
expect_scores_match_reference <- function(scores, reference, by) {
  metrics <- setdiff(colnames(reference), by)
  testthat::expect_setequal(get_metrics(scores), metrics)
  testthat::expect_identical(nrow(scores), nrow(reference))
  combined <- merge(
    as.data.table(scores), reference,
    by = by, suffixes = c("", "_reference")
  )
  testthat::expect_identical(nrow(combined), nrow(reference))
  for (metric in metrics) {
    testthat::expect_equal(
      combined[[metric]], combined[[paste0(metric, "_reference")]],
      tolerance = 1e-8,
      label = metric, expected.label = paste(metric, "(reference)")
    )
  }
}


# ============================================================================ #
# Reference implementations ====================================================
# ============================================================================ #

# Weighted interval score and its components (Bracher et al., 2021), computed
# from the central prediction intervals formed by the quantiles, together with
# bias, interval coverage and absolute error of the median.
reference_quantile <- function(observed, predicted, quantile_level) {
  y <- observed[1]
  tau <- round(quantile_level, 10)
  med <- predicted[tau == 0.5]
  lower_tau <- sort(tau[tau < 0.5])
  lower <- predicted[match(lower_tau, tau)]
  upper <- predicted[match(round(1 - lower_tau, 10), tau)]
  alpha <- 2 * lower_tau
  # the median counts as an interval with weight 1/2
  normalisation <- length(alpha) + 0.5

  dispersion <- sum(alpha / 2 * (upper - lower)) / normalisation
  overprediction <-
    (0.5 * max(med - y, 0) + sum(pmax(lower - y, 0))) / normalisation
  underprediction <-
    (0.5 * max(y - med, 0) + sum(pmax(y - upper, 0))) / normalisation

  # bias: 1 - 2 * (quantile level of the innermost quantile beyond y)
  bias <- if (y == med) {
    0
  } else if (y < med) {
    1 - 2 * max(c(0, tau[predicted <= y]))
  } else {
    1 - 2 * min(c(1, tau[predicted >= y]))
  }

  covered <- function(range) {
    lower_q <- predicted[tau == round((100 - range) / 200, 10)]
    upper_q <- predicted[tau == round((100 + range) / 200, 10)]
    y >= lower_q && y <= upper_q
  }

  list(
    wis = dispersion + overprediction + underprediction,
    overprediction = overprediction,
    underprediction = underprediction,
    dispersion = dispersion,
    bias = bias,
    interval_coverage_50 = covered(50),
    interval_coverage_90 = covered(90),
    ae_median = abs(y - med)
  )
}

# CRPS of the empirical distribution of the samples, in its kernel ("energy")
# form E|X - y| - 1/2 E|X - X'|.
reference_crps <- function(y, x) {
  mean(abs(x - y)) - 0.5 * mean(abs(outer(x, x, "-")))
}

reference_sample <- function(observed, predicted, integer_valued) {
  y <- observed[1]
  x <- predicted
  med <- median(x)
  mu <- mean(x)
  variance <- mean((x - mu)^2)

  crps <- reference_crps(y, x)
  # dispersion is the CRPS the forecast would get if y were its median
  dispersion <- reference_crps(med, x)

  bias <- if (integer_valued) {
    1 - (mean(x <= y) + mean(x <= y - 1))
  } else {
    1 - 2 * (mean(x < y) + 0.5 * mean(x == y))
  }

  list(
    bias = bias,
    dss = (y - mu)^2 / variance + log(variance),
    crps = crps,
    overprediction = if (y < med) crps - dispersion else 0,
    underprediction = if (y > med) crps - dispersion else 0,
    dispersion = dispersion,
    # Gaussian kernel density estimate with the bandwidth from stats::bw.nrd()
    log_score = -log(mean(dnorm(y, mean = x, sd = bw.nrd(x)))),
    # median absolute deviation, scaled to be consistent for the normal sd
    mad = 1.4826 * median(abs(x - med)),
    ae_median = abs(y - med),
    se_mean = (y - mu)^2
  )
}

# Energy score and variogram score (order p = 0.5, unit weights) of a
# multivariate forecast. `y` is a vector of observations, `x` a matrix of
# samples with one row per observation.
reference_multivariate_sample <- function(y, x) {
  n_samples <- ncol(x)
  sample_distances <- vapply(
    seq_len(n_samples), function(j) sqrt(sum((x[, j] - y)^2)), numeric(1)
  )
  pairwise_distances <- outer(
    seq_len(n_samples), seq_len(n_samples),
    Vectorize(function(i, j) sqrt(sum((x[, i] - x[, j])^2)))
  )
  energy_score <- mean(sample_distances) - 0.5 * mean(pairwise_distances)

  variogram_score <- 0
  for (i in seq_along(y)) {
    for (j in seq_along(y)) {
      variogram_score <- variogram_score +
        (abs(y[i] - y[j])^0.5 - mean(abs(x[i, ] - x[j, ])^0.5))^2
    }
  }
  list(energy_score = energy_score, variogram_score = variogram_score)
}


# ============================================================================ #
# Comparison with score() ======================================================
# ============================================================================ #

test_that("score() returns correct scores for quantile forecasts", {
  forecast_unit <- get_forecast_unit(example_quantile)
  reference <- forecast_data(example_quantile)[
    , reference_quantile(observed, predicted, quantile_level),
    by = forecast_unit
  ]
  expect_scores_match_reference(scores_quantile, reference, forecast_unit)
})

test_that("score() returns correct scores for quantile forecasts with ties", {
  # The example data contain no observation equal to the upper bound of an
  # interval, so the closed upper bound of the interval coverage (and the
  # bias for an observation equal to a quantile) is checked on a small
  # hand-made forecast. The observations equal, in turn, every quantile.
  predicted <- c(1, 2, 4, 7, 11)
  forecast <- as_forecast_quantile(data.table(
    model = "m",
    id = rep(1:5, each = 5),
    observed = rep(predicted, each = 5),
    predicted = rep(predicted, 5),
    quantile_level = rep(c(0.05, 0.25, 0.5, 0.75, 0.95), 5)
  ))
  reference <- forecast_data(forecast)[
    , reference_quantile(observed, predicted, quantile_level),
    by = c("model", "id")
  ]
  expect_scores_match_reference(score(forecast), reference, c("model", "id"))
})

test_that("score() returns correct scores for continuous sample forecasts", {
  forecast_unit <- get_forecast_unit(example_sample_continuous)
  reference <- forecast_data(example_sample_continuous)[
    , reference_sample(observed, predicted, integer_valued = FALSE),
    by = forecast_unit
  ]
  expect_scores_match_reference(
    scores_sample_continuous, reference, forecast_unit
  )
})

test_that("score() is correct for continuous samples tied to the observation", {
  # The example data contain no samples equal to the observation, so the tie
  # term of the continuous bias is checked on a small hand-made forecast.
  forecast <- as_forecast_sample(data.table(
    model = "m",
    id = rep(1:2, each = 8),
    observed = rep(c(2.5, 3.5), each = 8),
    predicted = c(
      0.5, 1.5, 2.5, 2.5, 3.5, 4.5, 2.5, 5.5,
      1.5, 3.5, 3.5, 4.5, 6.5, 7.5, 2.5, 8.5
    ),
    sample_id = rep(1:8, 2)
  ))
  reference <- forecast_data(forecast)[
    , reference_sample(observed, predicted, integer_valued = FALSE),
    by = c("model", "id")
  ]
  expect_scores_match_reference(score(forecast), reference, c("model", "id"))
})

test_that("score() returns correct scores for discrete sample forecasts", {
  forecast_unit <- get_forecast_unit(example_sample_discrete)
  reference <- forecast_data(example_sample_discrete)[
    , reference_sample(observed, predicted, integer_valued = TRUE),
    by = forecast_unit
  ]
  expect_scores_match_reference(
    scores_sample_discrete, reference, forecast_unit
  )
})

test_that("score() returns correct scores for binary forecasts", {
  forecast_unit <- get_forecast_unit(example_binary)
  reference <- forecast_data(example_binary)[, {
    outcome <- as.numeric(observed == levels(observed)[2])
    list(
      brier_score = (outcome - predicted)^2,
      log_score = -log(outcome * predicted + (1 - outcome) * (1 - predicted))
    )
  }, by = forecast_unit]
  expect_scores_match_reference(scores_binary, reference, forecast_unit)
})

test_that("score() returns correct scores for point forecasts", {
  forecast_unit <- get_forecast_unit(example_point)
  reference <- forecast_data(example_point)[, list(
    ae_point = abs(observed - predicted),
    se_point = (observed - predicted)^2,
    ape = abs(observed - predicted) / abs(observed)
  ), by = forecast_unit]
  expect_scores_match_reference(scores_point, reference, forecast_unit)
})

test_that("score() returns correct scores for nominal forecasts", {
  forecast_unit <- get_forecast_unit(example_nominal)
  reference <- forecast_data(example_nominal)[, list(
    log_score = -log(predicted[predicted_label == observed[1]])
  ), by = forecast_unit]
  expect_scores_match_reference(scores_nominal, reference, forecast_unit)
})

test_that("score() returns correct scores for ordinal forecasts", {
  forecast_unit <- get_forecast_unit(example_ordinal)
  reference <- forecast_data(example_ordinal)[, {
    # ranked probability score: squared distance between the predictive and
    # the observed cumulative distribution, summed over all levels
    level_index <- as.integer(predicted_label)
    cdf <- cumsum(predicted[order(level_index)])
    observed_cdf <- as.numeric(sort(level_index) >= as.integer(observed[1]))
    list(
      log_score = -log(predicted[predicted_label == observed[1]]),
      rps = sum((cdf - observed_cdf)^2)
    )
  }, by = forecast_unit]
  expect_scores_match_reference(score(example_ordinal), reference, forecast_unit)
})

test_that("score() returns correct scores for multivariate sample forecasts", {
  reference <- forecast_data(example_multivariate_sample)[, {
    # one row per observation (location), one column per sample
    wide <- dcast(.SD, location ~ sample_id, value.var = "predicted")
    observations <- unique(.SD[, c("location", "observed")])
    y <- observations$observed[match(wide$location, observations$location)]
    reference_multivariate_sample(y, as.matrix(wide[, -1]))
  }, by = ".mv_group_id"]
  expect_scores_match_reference(
    score(example_multivariate_sample), reference, ".mv_group_id"
  )
})


# ============================================================================ #
# Regression snapshot ==========================================================
# ============================================================================ #

# These snapshots mostly overlap with the per-forecast checks above. They are
# kept as a cheap guard against unintended changes to the example data or to
# `summarise_scores()`, which the checks above would silently follow.
# Scores are summarised by model and target type. Non-finite scores (e.g. an
# infinite log score) are dropped before averaging, as they would otherwise
# mask all other values in their group. Note that a single near-degenerate
# forecast (almost all samples zero) dominates the mean dss of the baseline
# model for Cases. Values are compared with a relative tolerance rather than
# printed, so that floating point differences across platforms (e.g. a value
# that rounds differently in the last printed digit) do not fail the snapshot.
# Rows are sorted so that the snapshot does not depend on the row order of the
# scores.
summarise_example_scores <- function(scores) {
  summary <- summarise_scores(
    scores,
    by = c("model", "target_type"),
    fun = function(x) mean(x[is.finite(x)])
  )
  setorderv(summary, c("model", "target_type"))
  as.data.frame(summary)
}

test_that("summarised scores for example_quantile have not changed", {
  expect_snapshot_value(
    summarise_example_scores(scores_quantile),
    style = "json2", tolerance = 1e-6
  )
})

test_that("summarised scores for example_sample_continuous have not changed", {
  expect_snapshot_value(
    summarise_example_scores(scores_sample_continuous),
    style = "json2", tolerance = 1e-6
  )
})

test_that("summarised scores for example_sample_discrete have not changed", {
  expect_snapshot_value(
    summarise_example_scores(scores_sample_discrete),
    style = "json2", tolerance = 1e-6
  )
})

test_that("summarised scores for example_point have not changed", {
  expect_snapshot_value(
    summarise_example_scores(scores_point),
    style = "json2", tolerance = 1e-6
  )
})

test_that("summarised scores for example_binary have not changed", {
  expect_snapshot_value(
    summarise_example_scores(scores_binary),
    style = "json2", tolerance = 1e-6
  )
})

test_that("summarised scores for example_nominal have not changed", {
  expect_snapshot_value(
    summarise_example_scores(scores_nominal),
    style = "json2", tolerance = 1e-6
  )
})

test_that("summarised scores for example_ordinal have not changed", {
  expect_snapshot_value(
    summarise_example_scores(score(example_ordinal)),
    style = "json2", tolerance = 1e-6
  )
})
