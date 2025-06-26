# ==============================================================================
# as_forecast_sample_multivariate()
# ==============================================================================
test_that("as_forecast_sample_multivariate() works as expected", {
    test <- na.omit(data.table::copy(example_sample_continuous))
    data.table::setnames(test,
        old = c("observed", "predicted", "sample_id"),
        new = c("obs", "pred", "sample")
    )
    expect_no_condition(
        as_forecast_sample_multivariate(test,
            observed = "obs", predicted = "pred",
            forecast_unit = c(
                "location", "model", "target_type",
                "target_end_date", "horizon"
            ),
            grouping = c(
                "model", "target_type",
                "target_end_date", "horizon"
            ),
            sample_id = "sample"
        )
    )
})


# ==============================================================================
# is_forecast_sample()
# ==============================================================================
test_that("is_forecast_sample() works as expected", {
    expect_true(is_forecast_sample_multivariate(example_sample_multivariate))
    expect_false(is_forecast_sample_multivariate(example_binary))
    expect_false(is_forecast_sample(example_sample_multivariate))
})


# ==============================================================================
# get_metrics.forecast_sample()
# ==============================================================================

test_that("get_metrics.forecast_sample_multivariate() works as expected", {
    expect_true(
        is.list(get_metrics(example_sample_multivariate))
    )
})


# ==============================================================================
# define_grouping_cols()
# ==============================================================================
test_that("define_grouping_cols() works as expected", {
  data <- example_sample_multivariate
  forecast_unit <- get_forecast_unit(data)

  # Test basic functionality
  result <- define_grouping_cols(data, across = "location")
  expect_type(result, "character")
  expect_true(all(result %in% forecast_unit))
  expect_false("location" %in% result)

  # Test with multiple across variables
  result <- define_grouping_cols(data, across = c("location", "target_type"))
  expect_type(result, "character")
  expect_true(all(result %in% forecast_unit))
  expect_false(any(c("location", "target_type") %in% result))
})

test_that("define_grouping_cols() handles errors appropriately", {
  data <- example_sample_multivariate

  # Test missing across argument
  expect_error(
    define_grouping_cols(data),
    "required to denote the variable across which to form groups for multivariate forecasts."
  )

  # Test invalid column names
  expect_error(
    define_grouping_cols(data, across = "nonexistent_column"),
    "Must be a subset of"
  )

  # Test empty across vector
  expect_error(
    define_grouping_cols(data, across = character()),
    "Must have length >= 1"
  )
})

# ==============================================================================
# set_grouping() and get_grouping()
# ==============================================================================
test_that("set_grouping() works as expected", {
  data <- example_sample_multivariate
  grouping <- c("model", "target_type", "target_end_date", "horizon")

  # Test basic functionality
  result <- set_grouping(data, grouping)
  expect_true(".scoringutils_group_id" %in% names(result))
  expect_true(is.numeric(result$.scoringutils_group_id))

  # Test that groups are consistent
  group_counts <- as.data.table(result)[, .N, by = .scoringutils_group_id]
  expect_true(all(group_counts$N > 0))
})

test_that("get_grouping() works as expected", {
  data <- example_sample_multivariate
  grouping <- c("model", "target_type", "target_end_date", "horizon")
  data <- set_grouping(data, grouping)

  # Test that get_grouping returns the correct columns
  result <- get_grouping(data)
  expect_type(result, "character")
  expect_true(all(grouping %in% result))
})

test_that("get_grouping() falls back to forecast_unit when group_id is missing", {
  data <- example_sample_multivariate

  # Remove the group_id column
  data_bad <- as.data.table(data)
  data_bad[, .scoringutils_group_id := NULL]

  expect_error(
    get_grouping(data_bad),
    "No column `.scoringutils_group_id` found in the forecast object."
  )
})

# ==============================================================================
# score.forecast_sample_multivariate()
# ==============================================================================
test_that("score.forecast_sample_multivariate() works as expected", {
  data <- example_sample_multivariate

  # Test basic scoring
  scores <- score(data)
  expect_true(is.data.table(scores))
  expect_true("energy_score" %in% names(scores))

  # Test with specific metrics
  scores <- score(data, metrics = list(energy_score = energy_score_multivariate))
  expect_true(is.data.table(scores))
  expect_true("energy_score" %in% names(scores))

  # Test that scores are numeric
  expect_true(is.numeric(scores$energy_score))
})

# ==============================================================================
# Error cases for as_forecast_sample_multivariate()
# ==============================================================================
test_that("as_forecast_sample_multivariate() handles errors appropriately", {
  data <- as.data.table(example_sample_multivariate)

  # Test with missing required columns
  data_bad <- data[, !"sample_id"]
  expect_error(
    as_forecast_sample_multivariate(data_bad,
      grouping = c("model", "target_type")),
    "Assertion on 'forecast' failed: Column 'sample_id' not found in data."
  )

  # Test with inconsistent sample lengths within groups
  data_bad <- copy(data)
  data_bad <- rbind(data_bad[1000:1010], data_bad)
  expect_error(
    as_forecast_sample_multivariate(data_bad,
      grouping = c("model", "target_type")),
    "All forecasts \\(as defined by the forecast unit\\) in a group must have the same number of samples"
  )

  # Test with inconsistent sample lengths after forecast object creation
  expect_warning(
    example_sample_multivariate[-(1000:1010), ],
    "Found the following group with an inconsistent sample length"
  )

   # Test with invalid grouping columns
  expect_error(
    as_forecast_sample_multivariate(data,
      grouping = c("nonexistent_column")),
    "Must be a subset of"
  )
})
