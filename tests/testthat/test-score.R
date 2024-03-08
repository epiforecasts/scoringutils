# =============================================================================
# Check creation of objects of class `scores`
# =============================================================================

test_that("new_scores() works", {
  expect_equal(
    class(new_scores(data.frame(), metrics = "")),
    c("scores", "data.table", "data.frame")
  )

  expect_error(
    new_scores(data.frame()),
    "missing, with no default"
  )
})

test_that("as_scores() works", {
  expect_equal(
    class(scoringutils:::as_scores(data.frame(wis = 1), metrics = "wis")),
    c("scores", "data.table", "data.frame")
  )
  expect_warning(
    scoringutils:::as_scores(data.frame(), metrics = "wis"),
    "The following scores have been previously computed"
  )
})

test_that("validate_scores() works", {
  expect_error(
    validate_scores(data.frame()),
    "Must inherit from class 'scores'"
  )
})

test_that("Output of `score()` has the class `scores()`", {
  expect_no_condition(validate_scores(scores_point))
  expect_no_condition(validate_scores(scores_binary))
  expect_no_condition(validate_scores(scores_continuous))
  expect_no_condition(validate_scores(scores_quantile))
})

# =============================================================================
# `score()`
# =============================================================================

# common error handling --------------------------------------------------------
test_that("function throws an error if data is not a forecast object", {
  expect_error(
    score(data = NULL),
    "The input needs to be a forecast object."
  )
})

# test_that("score() warns if column name equals a metric name", {
#   data <- data.frame(
#     observed = rep(1:10, each = 2),
#     predicted = rep(c(-0.3, 0.3), 10) + rep(1:10, each = 2),
#     model = "Model 1",
#     date = as.Date("2020-01-01") + rep(1:10, each = 2),
#     quantile = rep(c(0.1, 0.9), times = 10),
#     bias = 3
#   )
#
#   expect_warning(suppressMessages(score(data = data)))
# })



# test binary case -------------------------------------------------------------
test_that("function produces output for a binary case", {
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
})

test_that("score.forecast_binary() errors with only NA values", {
  only_nas <- copy(as_forecast(example_binary))[, predicted := NA_real_]
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
      as_forecast()

    # passing a simple function works
    expect_equal(
      score(df,
            metrics = list("identity" = function(x, y) {return(y)}))$identity,
      df$predicted
    )


    # passing an additional argument that is not part of the function
    # definition works
    expect_equal(
      score(df,
            metrics = list("identity" = function(x, y) {return(y)}),
            additional_arg = "something")$identity,
      df$predicted
    )

    # passing an additional function to one that accepts ... works
    expect_message(
      score(df,
            metrics = list("test_function" = test_fun),
            test = "something"),
      "test argument found"
    )

    # passing an argument that's the same as a named argument
    expect_equal(
      unique(
        score(df,
              metrics = list("test_function" = test_fun),
              y = "something")$test_function
      ),
      "something"
    )


    ## Additional tests for validate_metrics()
    # passing in something that's not a function or a known metric
    expect_warning(
      expect_warning(
        score(df, metrics = list(
          "test1" = test_fun, "test" = test_fun, "hi" = "hi", "2" = 3)
        ),
        "`Metrics` element number 3 is not a valid function"
      ),
      "`Metrics` element number 4 is not a valid function")

    # passing a single named argument for metrics by position
    expect_contains(
      names(score(df, list("hi" = test_fun))),
      "hi")

    # providing an additional, unrelated function argument works
    expect_no_error(
      score(df, unnecessary_argument = "unnecessary")
    )
    expect_no_error(
      score(df, metrics = list("brier_score" = brier_score),
            unnecessary_argument = "unnecessary")
    )
  }
)


# test point case --------------------------------------------------------------
test_that("function produces output for a point case", {
  eval <- summarise_scores(scores_point, by = c("model", "target_type"))

  expect_equal(
    nrow(eval) > 1,
    TRUE
  )
  expect_equal(
    colnames(eval),
    c("model", "target_type", names(rules_point()))
  )
})

test_that("Changing metrics names works", {
  metrics_test <- rules_point()
  names(metrics_test)[1] = "just_testing"
  eval <- suppressMessages(score(as_forecast(example_point),
                                 metrics = metrics_test))
  eval_summarised <- summarise_scores(eval, by = "model")
  expect_equal(
    colnames(eval_summarised),
    c("model", "just_testing", names(rules_point())[-1])
  )
})


test_that("score.forecast_point() errors with only NA values", {
  only_nas <- copy(as_forecast(example_point))[, predicted := NA_real_]
  expect_error(
    score(only_nas),
    "After removing rows with NA values in the data, no forecasts are left."
  )
})

# test quantile case -----------------------------------------------------------
test_that("score_quantile correctly handles separate results = FALSE", {
  df <- example_quantile[model == "EuroCOVIDhub-ensemble" &
                           target_type == "Cases" & location == "DE"] %>%
    as_forecast()
  eval <- score(df[!is.na(predicted)], separate_results = FALSE)

  expect_equal(
    nrow(eval) > 1,
    TRUE
  )
  expect_true(all(names(rules_quantile()) %in% colnames(eval)))
})


test_that("score() quantile produces desired metrics", {
  data <- data.frame(
    observed = rep(1:10, each = 3),
    predicted = rep(c(-0.3, 0, 0.3), 10) + rep(1:10, each = 3),
    model = "Model 1",
    date = as.Date("2020-01-01") + rep(1:10, each = 3),
    quantile_level = rep(c(0.1, 0.5, 0.9), times = 10)
  )

  data <-suppressWarnings(suppressMessages(as_forecast(data)))

  out <- score(data = data, metrics = metrics_no_cov)
  metrics <- c(
    "dispersion", "underprediction", "overprediction",
    "bias", "ae_median"
  )

  expect_true(all(metrics %in% colnames(out)))
})


test_that("calculation of ae_median is correct for a quantile format case", {
  eval <- summarise_scores(scores_quantile,by = "model")

  example <- scoringutils::example_quantile
  ae <- example[quantile_level == 0.5, ae := abs(observed - predicted)][!is.na(model), .(mean = mean(ae, na.rm = TRUE)),
    by = "model"
  ]$mean

  expect_equal(sort(eval$ae_median), sort(ae))
})


test_that("all quantile and range formats yield the same result", {
  eval1 <- summarise_scores(scores_quantile, by = "model")

  df <- data.table::copy(example_quantile)

  ae <- df[
    quantile_level == 0.5, ae := abs(observed - predicted)][
    !is.na(model), .(mean = mean(ae, na.rm = TRUE)),
    by = "model"
  ]$mean

  expect_equal(sort(eval1$ae_median), sort(ae))
})

test_that("WIS is the same with other metrics omitted or included", {
  eval <- suppressMessages(score(as_forecast(example_quantile),
    metrics = list("wis" = wis)
  ))

  eval2 <- scores_quantile

  expect_equal(
    sum(eval$wis),
    sum(eval2$wis)
  )
})


test_that("score.forecast_quantile() errors with only NA values", {
  only_nas <- copy(as_forecast(example_quantile))[, predicted := NA_real_]
  expect_error(
    score(only_nas),
    "After removing rows with NA values in the data, no forecasts are left."
  )
})





# test integer and continuous case ---------------------------------------------
test_that("function produces output for a continuous format case", {

  eval <- scores_continuous

  only_nas <- copy(as_forecast(example_continuous))[, predicted := NA_real_]
  expect_error(
    score(as_forecast(only_nas)),
    "After removing rows with NA values in the data, no forecasts are left."
  )

  expect_equal(
    nrow(eval) > 1,
    TRUE
  )

  expect_equal(
    nrow(eval),
    887
  )
})

test_that("function throws an error if data is missing", {
  expect_error(suppressMessages(score(data = NULL)))
})

# =============================================================================
# `apply_rules()`
# =============================================================================

test_that("apply_rules() works", {

  dt <- data.table::data.table(x = 1:10)
  scoringutils:::apply_rules(
    data = dt, metrics = list("test" = function(x) x + 1),
    dt$x
  )
  expect_equal(dt$test, 2:11)

  # additional named argument works
  expect_no_condition(
    scoringutils:::apply_rules(
      data = dt, metrics = list("test" = function(x) x + 1),
      dt$x, y = dt$test)
  )

  # additional unnamed argument does not work

  expect_warning(
    scoringutils:::apply_rules(
      data = dt, metrics = list("test" = function(x) x + 1),
      dt$x, dt$test)
  )
})

# attributes
test_that("`[` preserves attributes", {
  test <- data.table::copy(scores_binary)
  class(test) <- c("scores", "data.frame")
  expect_true("metrics" %in% names(attributes(test)))
  expect_true("metrics" %in% names(attributes(test[1:10])))
})
