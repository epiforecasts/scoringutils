# ==============================================================================
# as_forecast_binary() # nolint: commented_code_linter
# ==============================================================================
test_that("output of as_forecast_binary() is accepted as input to score()", {
  check <- suppressMessages(as_forecast_binary(example_binary))
  score_check <- expect_no_error(score(na.omit(check)))
  expect_identical(score_check, suppressMessages(score(as_forecast_binary(example_binary))))
})


# ==============================================================================
# is_forecast_binary() # nolint: commented_code_linter
# ==============================================================================
test_that("is_forecast_binary() works as expected", {
  expect_true(is_forecast_binary(example_binary))
  expect_false(is_forecast_binary(example_point))
  expect_false(is_forecast_binary(example_quantile))
  expect_false(is_forecast_binary(example_sample_continuous))
  expect_false(is_forecast_binary(example_nominal))
})


# ==============================================================================
# assert_forecast.forecast_binary() # nolint: commented_code_linter
# ==============================================================================
test_that("assert_forecast.forecast_binary works as expected", {
  test <- na.omit(as.data.table(example_binary))
  test[, "sample_id" := seq_len(nrow(test))]

  # error if there is a superfluous sample_id column
  expect_error(
    as_forecast_binary(test),
    paste(
      "Input looks like a binary forecast, but an additional column",
      "called `sample_id` or `quantile` was found."
    )
  )

  # expect error if probabilties are not in [0, 1]
  test <- na.omit(as.data.table(example_binary))
  test[, "predicted" := predicted + 1]
  expect_error(
    as_forecast_binary(test),
    "Input looks like a binary forecast, but found the following issue"
  )
})



test_that("as_forecast_binary() warns when data has reversed 0/1 factor levels", {
  dt <- data.table(
    model = "m1",
    id = 1:4,
    observed = factor(c(0, 1, 1, 0), levels = c("1", "0")),
    predicted = c(0.1, 0.9, 0.8, 0.2)
  )
  expect_warning(
    as_forecast_binary(dt),
    "counterintuitive"
  )
})

test_that("score() produces correct results with standard 0/1 factor levels", {
  # example_binary has standard levels c("0", "1"), should not warn about levels
  expect_no_warning(
    suppressMessages(score(as_forecast_binary(example_binary)))
  )
})


# ==============================================================================
# score.forecast_binary() # nolint: commented_code_linter
# ==============================================================================
test_that("function produces output for a binary case", {
  expect_named(
    scores_binary,
    c(get_forecast_unit(example_binary), names(get_metrics(example_binary)))
  )

  eval <- summarise_scores(scores_binary, by = c("model", "target_type"))

  expect_gt(
    nrow(eval), 1
  )
  expect_identical(
    colnames(eval),
    c(
      "model", "target_type",
      "brier_score",
      "log_score"
    )
  )

  expect_true("brier_score" %in% names(eval))

  expect_s3_class(eval, c("scores", "data.table", "data.frame"), exact = TRUE)
})

test_that("score.forecast_binary() errors with only NA values", {
  # [.forecast()` will warn even before score()
  only_nas <- suppressWarnings(
    copy(example_binary)[, predicted := NA_real_]
  )
  expect_error(
    score(only_nas),
    "After removing rows with NA values in the data, no forecasts are left."
  )
})

test_that("score() gives same result for binary as regular function", {
  manual_eval <- brier_score(
    factor(example_binary$observed),
    example_binary$predicted
  )
  expect_equal( # nolint: expect_identical_linter
    scores_binary$brier_score, manual_eval[!is.na(manual_eval)]
  )
})

test_that(
  "passing additional functions to score binary works handles them",
  {
    test_fun <- function(x, y, ...) {
      if (hasArg("test")) {
        message("test argument found")
      }
      return(y)
    }

    df <- as_forecast_binary(
      example_binary[model == "EuroCOVIDhub-ensemble" &
                       target_type == "Cases" & location == "DE"]
    )

    # passing a simple function works
    expect_identical(
      score(df,
        metrics = list(identity = function(x, y) {
          return(y)
        })
      )$identity,
      df$predicted
    )
  }
)

# ==============================================================================
# get_metrics.forecast_binary() # nolint: commented_code_linter
# ==============================================================================

test_that("get_metrics.forecast_binary() works as expected", {
  expect_type(
    get_metrics(example_binary), "list"
  )
})
