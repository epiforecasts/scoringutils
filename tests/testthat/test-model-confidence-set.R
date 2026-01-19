# tests for get_model_confidence_set() -----------------------------------------

test_that("get_model_confidence_set() works with basic input", {
  pairwise <- get_pairwise_comparisons(scores_quantile)
  mcs <- get_model_confidence_set(pairwise)

  # Check output structure
 expect_s3_class(mcs, c("data.table", "data.frame"), exact = TRUE)
  expect_true(all(c("model", "relative_skill", "pval_vs_best",
                    "adj_pval_vs_best", "in_mcs") %in% names(mcs)))

  # Check that all models are present
  expect_equal(sort(unique(mcs$model)), sort(unique(pairwise$model)))

  # Check that best model is in MCS
  best_model <- mcs[which.min(relative_skill), model]
  expect_true(mcs[model == best_model, in_mcs])

  # Check that best model has pval = 1

  expect_equal(mcs[model == best_model, pval_vs_best], 1)
  expect_equal(mcs[model == best_model, adj_pval_vs_best], 1)

  # Check that in_mcs is logical
  expect_type(mcs$in_mcs, "logical")

  # Check sorted by relative skill
  expect_equal(mcs$relative_skill, sort(mcs$relative_skill))
})

test_that("get_model_confidence_set() works with grouping (conditional MCS)", {
  pairwise <- get_pairwise_comparisons(scores_quantile, by = "target_type")
  mcs <- get_model_confidence_set(pairwise)

  # Check that grouping column is preserved
  expect_true("target_type" %in% names(mcs))

  # Check that we have results for each group
  expect_equal(sort(unique(mcs$target_type)), sort(unique(pairwise$target_type)))

  # Check that each group has its own best model (with in_mcs = TRUE)
  mcs_by_group <- split(mcs, by = "target_type")
  for (grp in mcs_by_group) {
    expect_true(any(grp$in_mcs))
    # Best model in each group should be in MCS
    best_in_grp <- grp[which.min(relative_skill), model]
    expect_true(grp[model == best_in_grp, in_mcs])
  }
})

test_that("get_model_confidence_set() respects alpha parameter", {
  pairwise <- get_pairwise_comparisons(scores_quantile)

  # More stringent alpha should result in same or smaller MCS
  mcs_10 <- get_model_confidence_set(pairwise, alpha = 0.1)
  mcs_05 <- get_model_confidence_set(pairwise, alpha = 0.05)
  mcs_01 <- get_model_confidence_set(pairwise, alpha = 0.01)

  n_in_mcs_10 <- sum(mcs_10$in_mcs)
  n_in_mcs_05 <- sum(mcs_05$in_mcs)
  n_in_mcs_01 <- sum(mcs_01$in_mcs)

  # More stringent alpha (smaller) should give equal or larger MCS
  # because we reject fewer models
  expect_true(n_in_mcs_01 >= n_in_mcs_05)
  expect_true(n_in_mcs_05 >= n_in_mcs_10)

  # Very lenient alpha should include only best model
  mcs_99 <- get_model_confidence_set(pairwise, alpha = 0.99)
  # At alpha = 0.99, most models will be excluded unless pval > 0.99
  expect_true(sum(mcs_99$in_mcs) >= 1)  # At least best model
})

test_that("get_model_confidence_set() uses attributes from pairwise output",
{
  pairwise <- get_pairwise_comparisons(scores_quantile, by = "target_type")

  # Check attributes are set
  expect_equal(attr(pairwise, "compare"), "model")
  expect_equal(attr(pairwise, "metric"), "wis")
  expect_equal(attr(pairwise, "by"), "target_type")

  # MCS should work using these attributes
  mcs <- get_model_confidence_set(pairwise)
  expect_true("target_type" %in% names(mcs))
})

test_that("get_model_confidence_set() works without attributes (backwards
compatibility)", {
  pairwise <- get_pairwise_comparisons(scores_quantile)

  # Remove attributes to simulate old output or manual data.table
  pairwise_no_attr <- data.table::copy(pairwise)
  attr(pairwise_no_attr, "compare") <- NULL
  attr(pairwise_no_attr, "metric") <- NULL
  attr(pairwise_no_attr, "by") <- NULL

  # Should still work by detecting from column names
  mcs <- get_model_confidence_set(pairwise_no_attr)
  expect_s3_class(mcs, "data.table")
  expect_true("in_mcs" %in% names(mcs))
})

test_that("get_model_confidence_set() throws error with missing columns", {
  pairwise <- get_pairwise_comparisons(scores_quantile)

  # Remove required column
  pairwise_bad <- data.table::copy(pairwise)
  pairwise_bad[, pval := NULL]

  expect_error(
    get_model_confidence_set(pairwise_bad),
    "Missing required columns"
  )

  # Remove compare column
  pairwise_bad2 <- data.table::copy(pairwise)
  pairwise_bad2[, model := NULL]
  attr(pairwise_bad2, "compare") <- NULL

  expect_error(
    get_model_confidence_set(pairwise_bad2),
    "Assertion on 'compare' failed"
  )
})

test_that("get_model_confidence_set() throws error with no skill column", {
  pairwise <- get_pairwise_comparisons(scores_quantile)

  # Remove skill column and attribute
  pairwise_bad <- data.table::copy(pairwise)
  pairwise_bad[, wis_relative_skill := NULL]
  attr(pairwise_bad, "metric") <- NULL

  expect_error(
    get_model_confidence_set(pairwise_bad),
    "No relative skill column found"
  )
})

test_that("get_model_confidence_set() validates alpha parameter", {
  pairwise <- get_pairwise_comparisons(scores_quantile)

  expect_error(
    get_model_confidence_set(pairwise, alpha = -0.1),
    "Assertion on 'alpha' failed"
  )

  expect_error(
    get_model_confidence_set(pairwise, alpha = 1.5),
    "Assertion on 'alpha' failed"
  )

  expect_error(
    get_model_confidence_set(pairwise, alpha = "0.1"),
    "Assertion on 'alpha' failed"
  )
})

test_that("get_model_confidence_set() validates compare parameter", {
  pairwise <- get_pairwise_comparisons(scores_quantile)

  expect_error(
    get_model_confidence_set(pairwise, compare = "nonexistent"),
    "Assertion on 'compare' failed"
  )

  expect_error(
    get_model_confidence_set(pairwise, compare = c("model", "other")),
    "Assertion on 'compare' failed"
  )
})

test_that("get_model_confidence_set() handles different metrics", {
  # Use ae_median instead of default wis
  pairwise <- get_pairwise_comparisons(scores_quantile, metric = "ae_median")
  mcs <- get_model_confidence_set(pairwise)

  expect_s3_class(mcs, "data.table")
  expect_true("in_mcs" %in% names(mcs))
  expect_true(any(mcs$in_mcs))
})

test_that("get_model_confidence_set() example from documentation works", {
  scores <- example_quantile |>
    as_forecast_quantile() |>
    score()

  # Basic MCS
  mcs <- scores |>
    get_pairwise_comparisons() |>
    get_model_confidence_set()

  expect_s3_class(mcs, "data.table")
  expect_true(sum(mcs$in_mcs) >= 1)

  # Conditional MCS by target type
  mcs_by_target <- scores |>
    get_pairwise_comparisons(by = "target_type") |>
    get_model_confidence_set()

  expect_true("target_type" %in% names(mcs_by_target))
})
