# ==============================================================================
# get_metrics.scores()
# ==============================================================================
test_that("get_metrics.scores() works as expected", {
  expect_null(
    get_metrics.scores(as.data.frame(as.matrix(scores_point)))
  )

  expect_true(
    "brier_score" %in% get_metrics.scores(scores_binary)
  )

  expect_equal(
    get_metrics.scores(scores_sample_continuous),
    attr(scores_sample_continuous, "metrics")
  )

  # check that function errors if `error = TRUE` and not otherwise
  expect_error(
    get_metrics.scores(example_quantile, error = TRUE),
    "Input needs an attribute"
  )
  expect_no_condition(
    get_metrics.scores(scores_sample_continuous)
  )

  # expect warning if some column changed
  ex <- data.table::copy(scores_sample_continuous)
  data.table::setnames(ex, old = "crps", new = "changed")
  expect_warning(
    get_metrics.scores(ex),
    "scores have been previously computed, but are no longer column names"
  )
})
