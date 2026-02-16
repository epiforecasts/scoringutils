# ==============================================================================
# as_forecast_point() # nolint: commented_code_linter
# ==============================================================================

test_that("as_forecast_point() works", {
  expect_no_condition(
    as_forecast_point(as_forecast_quantile(na.omit(example_quantile)))
  )
})


# ==============================================================================
# is_forecast_point() # nolint: commented_code_linter
# ==============================================================================
test_that("is_forecast_point() works as expected", {
  expect_true(is_forecast_point(example_point))
  expect_false(is_forecast_point(example_binary))
  expect_false(is_forecast_point(example_quantile))
  expect_false(is_forecast_point(example_sample_continuous))
  expect_false(is_forecast_point(example_nominal))
})


# ==============================================================================
# assert_forecast.forecast_point() # nolint: commented_code_linter
# ==============================================================================

test_that("assert_forecast.forecast_point() works as expected", {
  test <- na.omit(data.table::as.data.table(example_point))
  test <- as_forecast_point(test)

  # expect an error if column is changed to character after initial validation.
  expect_warning(
    test[, "predicted" := as.character(predicted)],
    "Input looks like a point forecast, but found the following issue"
  )
  expect_error(
    assert_forecast(test),
    "Input looks like a point forecast, but found the following issue"
  )
})

test_that("assert_forecast.forecast_point() complains if the forecast type is wrong", {
  expect_error(
    assert_forecast(na.omit(example_point), forecast_type = "quantile"),
    "Forecast type determined by scoringutils based on input:"
  )
})


# ==============================================================================
# score.forecast_point() # nolint: commented_code_linter
# ==============================================================================
test_that("function produces output for a point case", {
  expect_named(
    scores_binary,
    c(get_forecast_unit(example_binary), names(get_metrics(example_binary)))
  )

  eval <- summarise_scores(scores_point, by = c("model", "target_type"))

  expect_gt(
    nrow(eval), 1
  )
  expect_identical(
    colnames(eval),
    c("model", "target_type", names(get_metrics(example_point)))
  )

  expect_s3_class(eval, c("scores", "data.table", "data.frame"), exact = TRUE)
})

test_that("Changing metrics names works", {
  metrics_test <- get_metrics(example_point)
  names(metrics_test)[1] <- "just_testing"
  eval <- suppressMessages(score(as_forecast_point(example_point),
    metrics = metrics_test
  ))
  eval_summarised <- summarise_scores(eval, by = "model")
  expect_identical(
    colnames(eval_summarised),
    c("model", "just_testing", names(get_metrics(example_point))[-1])
  )
})

test_that("score.forecast_point() errors with only NA values", {
  # [.forecast()` will warn even before score()
  only_nas <- suppressWarnings(
    copy(example_point)[, predicted := NA_real_]
  )
  expect_error(
    score(only_nas),
    "After removing rows with NA values in the data, no forecasts are left."
  )
})

# ==============================================================================
# get_metrics.forecast_point() # nolint: commented_code_linter
# ==============================================================================
test_that("get_metrics.forecast_point() works as expected", {
  expect_type(
    get_metrics(example_point), "list"
  )

  expect_identical(
    get_metrics.scores(scores_point),
    c("ae_point", "se_point", "ape")
  )
})

test_that("internal ae replacement produces identical results", {
  observed <- c(1, -15, 22, 0, 5.5)
  predicted <- c(5, 6, 7, 0, 5.5)
  ae_fn <- get_metrics(example_point, select = "ae_point")[[1]]
  expect_identical(ae_fn(observed, predicted), abs(observed - predicted))
  expect_identical(ae_fn(5, 5), 0)
  expect_identical(ae_fn(-10, 5), 15)
})

test_that("internal se replacement produces identical results", {
  observed <- c(1, -15, 22, 0, 5.5)
  predicted <- c(5, 6, 7, 0, 5.5)
  se_fn <- get_metrics(example_point, select = "se_point")[[1]]
  expect_identical(se_fn(observed, predicted), (observed - predicted)^2)
  expect_identical(se_fn(5, 5), 0)
  expect_identical(se_fn(-10, 5), 225)
})

test_that("internal ape replacement produces identical results", {
  observed <- c(1, -15, 22, 5.5, 100)
  predicted <- c(5, 6, 7, 0, 100)
  ape_fn <- get_metrics(example_point, select = "ape")[[1]]
  expect_equal(ape_fn(observed, predicted), abs(observed - predicted) / abs(observed))
  expect_identical(ape_fn(5, 5), 0)
  expect_identical(ape_fn(0, 5), Inf)
})

test_that("Metrics package is not in DESCRIPTION Imports", {
  desc_text <- readLines(system.file("DESCRIPTION", package = "scoringutils"))
  imports_lines <- desc_text[grepl("^Imports:|^\\s+Metrics", desc_text)]
  expect_false(any(grepl("\\bMetrics\\b", imports_lines)))
})

test_that("score() with point forecasts produces correct results after Metrics removal", {
  scores <- score(example_point)
  input <- na.omit(as.data.table(example_point))
  expect_equal(scores$ae_point, abs(input$observed - input$predicted))
  expect_equal(scores$se_point, (input$observed - input$predicted)^2)
  expect_equal(scores$ape, abs(input$observed - input$predicted) / abs(input$observed))
  expect_true(all(c("ae_point", "se_point", "ape") %in% colnames(scores)))
})

test_that("get_metrics.forecast_point() returns expected functions", {
  metrics <- get_metrics(example_point)
  expect_type(metrics, "list")
  expect_named(metrics, c("ae_point", "se_point", "ape"))
  expect_true(all(vapply(metrics, is.function, logical(1))))
  expect_true(all(vapply(metrics, function(f) length(formals(f)) == 2, logical(1))))
})
