# ==============================================================================
# assert_forecast.forecast_binary()
# ==============================================================================
test_that("assert_forecast.forecast_binary works as expected", {
  test <- na.omit(as.data.table(example_binary))
  test[, "sample_id" := 1:nrow(test)]

  # error if there is a superfluous sample_id column
  expect_error(
    as_forecast_binary(test),
    "Input looks like a binary forecast, but an additional column called `sample_id` or `quantile` was found."
  )

  # expect error if probabilties are not in [0, 1]
  test <- na.omit(as.data.table(example_binary))
  test[, "predicted" := predicted + 1]
  expect_error(
    as_forecast_binary(test),
    "Input looks like a binary forecast, but found the following issue"
  )
})



# ==============================================================================
# score.forecast_binary()
# ==============================================================================
test_that("function produces output for a binary case", {

  expect_equal(
    names(scores_binary),
    c(get_forecast_unit(example_binary), names(get_metrics(example_binary)))
  )

  eval <- summarise_scores(scores_binary, by = c("model", "target_type"))

  expect_equal(
    nrow(eval) > 1,
    TRUE
  )
  expect_equal(
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
  expect_equal(scores_binary$brier_score, manual_eval[!is.na(manual_eval)])
})

test_that(
  "passing additional functions to score binary works handles them", {
    test_fun <- function(x, y, ...) {
      if (hasArg("test")) {
        message("test argument found")
      }
      return(y)
    }

    df <- example_binary[model == "EuroCOVIDhub-ensemble" &
                           target_type == "Cases" & location == "DE"] %>%
      as_forecast_binary()

    # passing a simple function works
    expect_equal(
      score(df,
            metrics = list("identity" = function(x, y) {return(y)}))$identity,
      df$predicted
    )
  }
)
