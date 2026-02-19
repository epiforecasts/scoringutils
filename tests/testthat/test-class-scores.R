# ==============================================================================
# get_metrics.scores() # nolint: commented_code_linter
# ==============================================================================
test_that("get_metrics.scores() works as expected", {
  expect_null(
    get_metrics.scores(as.data.frame(as.matrix(scores_point)))
  )

  expect_true(
    "brier_score" %in% get_metrics.scores(scores_binary)
  )

  expect_identical(
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


# ==============================================================================
# print.scores() # nolint: commented_code_linter
# ==============================================================================
test_that("print.scores() outputs metrics and vignette link", {
  msg_output <- capture.output(
    capture.output(print(scores_quantile)),
    type = "message"
  )
  msg_text <- paste(msg_output, collapse = "\n")
  metrics <- attr(scores_quantile, "metrics")

  # Check that metric names appear in the message output (cli_text -> stderr)
  for (m in metrics) {
    expect_true(
      grepl(m, msg_text, fixed = TRUE),
      info = paste("Expected metric", m, "in print output")
    )
  }

  # Check that a link to the scoring rules vignette is present
  expect_true(
    grepl("scoring-rules", msg_text, fixed = TRUE),
    info = "Expected link to scoring-rules vignette in print output"
  )
})

test_that("print.scores() works for all forecast types", {
  all_scores <- list(
    scores_quantile = scores_quantile,
    scores_binary = scores_binary,
    scores_point = scores_point,
    scores_sample_continuous = scores_sample_continuous,
    scores_sample_discrete = scores_sample_discrete,
    scores_nominal = scores_nominal
  )

  for (name in names(all_scores)) {
    sc <- all_scores[[name]]
    msg_output <- capture.output(
      capture.output(print(sc)),
      type = "message"
    )
    msg_text <- paste(msg_output, collapse = "\n")

    # Each print should include the metric names in the cli output
    metrics <- attr(sc, "metrics")
    for (m in metrics) {
      expect_true(
        grepl(m, msg_text, fixed = TRUE),
        info = paste("Expected metric", m, "in print output for", name)
      )
    }
  }
})

test_that("print.scores() returns x invisibly", {
  result <- withVisible(capture.output(r <- print(scores_quantile)))
  expect_identical(r, scores_quantile)
})

test_that("print.scores() includes data.table output", {
  output_scores <- capture.output(print(scores_quantile))
  output_dt <- capture.output(print(data.table::as.data.table(scores_quantile)))
  # All data.table lines should appear in the scores output
  expect_true(all(output_dt %in% output_scores))
})

test_that("print.scores() handles missing metrics attribute gracefully", {
  ex <- data.table::copy(scores_quantile)
  attr(ex, "metrics") <- NULL
  expect_no_error(capture.output(print(ex)))
})

test_that("print.scores() handles renamed metric columns with warning", {
  ex <- data.table::copy(scores_sample_continuous)
  data.table::setnames(ex, old = "crps", new = "changed")
  expect_warning(
    suppressMessages(capture.output(print(ex))),
    "scores have been previously computed"
  )
})
