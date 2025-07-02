# ==============================================================================
# as_forecast_multivariate_sample()
# ==============================================================================
test_that("as_forecast_multivariate_sample() works as expected", {
  test <- na.omit(data.table::copy(example_sample_continuous))
  data.table::setnames(test,
      old = c("observed", "predicted", "sample_id"),
      new = c("obs", "pred", "sample")
  )
  expect_no_condition(
      as_forecast_multivariate_sample(test,
          observed = "obs", predicted = "pred",
          forecast_unit = c(
              "location", "model", "target_type",
              "target_end_date", "horizon"
          ),
          by = c(
              "model", "target_type",
              "target_end_date", "horizon"
          ),
          sample_id = "sample"
      )
  )
})

test_that("as_forecast_multivariate_sample() creates expected structure", {
  test <- na.omit(data.table::copy(example_sample_continuous))
  data.table::setnames(test,
      old = c("observed", "predicted", "sample_id"),
      new = c("obs", "pred", "sample")
  )

  result <- as_forecast_multivariate_sample(test,
      observed = "obs", predicted = "pred",
      forecast_unit = c(
          "location", "model", "target_type",
          "target_end_date", "horizon"
      ),
      by = c(
          "model", "target_type",
          "target_end_date", "horizon"
      ),
      sample_id = "sample"
  )

  # Snapshot the print output to capture the structure
  expect_snapshot(print(result))

  # Snapshot the class and structure
  expect_snapshot({
      cat("Class:", class(result), "\n")
      cat("Forecast type:", scoringutils:::get_forecast_type(result), "\n")
      cat("Forecast unit:", paste(get_forecast_unit(result), collapse = ", "), "\n")
      cat("Number of rows:", nrow(result), "\n")
      cat("Number of columns:", ncol(result), "\n")
      cat("Column names:", paste(names(result), collapse = ", "), "\n")
      cat("Number of unique groups:", length(unique(result$.scoringutils_group_id)), "\n")
  })
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

test_that("set_grouping() preserves existing keys correctly", {
data <- example_sample_multivariate
grouping <- c("model", "target_type", "target_end_date", "horizon")

# Test case 1: No existing keys
data_no_keys <- copy(data)
nokeys <- NULL
setkeyv(data_no_keys, nokeys)  # Ensure no keys
expect_null(key(data_no_keys))

result_no_keys <- set_grouping(data_no_keys, grouping)
expect_null(key(result_no_keys))  # Should still have no keys

# Test case 2: With existing keys
data_with_keys <- copy(data)
keys <- c("location", "model")
setkeyv(data_with_keys, keys)  # Set some keys
original_keys <- key(data_with_keys)
expect_equal(original_keys, c("location", "model"))

result_with_keys <- scoringutils:::set_grouping(data_with_keys, grouping)
expect_equal(key(result_with_keys), original_keys)  # Should preserve original keys

# Test case 3: Verify functionality still works with keys preserved
expect_true(".scoringutils_group_id" %in% names(result_with_keys))
expect_true(is.numeric(result_with_keys$.scoringutils_group_id))

# Test that groups are consistent
group_counts <- as.data.table(result_with_keys)[, .N, by = .scoringutils_group_id]
expect_true(all(group_counts$N > 0))
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

test_that("score.forecast_sample_multivariate() creates expected output structure", {
data <- example_sample_multivariate

# Test basic scoring and capture snapshot
scores <- score(data)

# Snapshot the print output to capture the structure
expect_snapshot(print(scores))

# Snapshot the detailed structure
expect_snapshot({
  cat("Class:", class(scores), "\n")
  cat("Number of rows:", nrow(scores), "\n")
  cat("Number of columns:", ncol(scores), "\n")
  cat("Column names:", paste(names(scores), collapse = ", "), "\n")
  cat("Energy score range:", paste(range(scores$energy_score, na.rm = TRUE), collapse = " to "), "\n")
  cat("Number of non-NA energy scores:", sum(!is.na(scores$energy_score)), "\n")
  cat("Sample of energy scores:", paste(head(scores$energy_score, 5), collapse = ", "), "\n")
})

# Test with specific metrics and capture snapshot
scores_specific <- score(data, metrics = list(energy_score = energy_score_multivariate))

expect_snapshot({
  cat("=== Specific Metrics Test ===\n")
  cat("Class:", class(scores_specific), "\n")
  cat("Number of rows:", nrow(scores_specific), "\n")
  cat("Number of columns:", ncol(scores_specific), "\n")
  cat("Column names:", paste(names(scores_specific), collapse = ", "), "\n")
  cat("Energy score range:", paste(range(scores_specific$energy_score, na.rm = TRUE), collapse = " to "), "\n")
})
})

# ==============================================================================
# Error cases for as_forecast_multivariate_sample()
# ==============================================================================
test_that("as_forecast_multivariate_sample() handles errors appropriately", {
data <- as.data.table(example_sample_multivariate)

# Test with missing required columns
data_bad <- data[, !"sample_id"]
expect_error(
  as_forecast_multivariate_sample(data_bad,
    by = c("model", "target_type")),
  "Assertion on 'forecast' failed: Column 'sample_id' not found in data."
)

# Test with inconsistent sample lengths within groups
data_bad <- copy(data)
data_bad <- rbind(data_bad[1000:1010], data_bad)
expect_error(
  as_forecast_multivariate_sample(data_bad,
    by = c("model", "target_type")),
  "All forecasts \\(as defined by the forecast unit\\) in a group must have the same number of samples"
)

# Test with inconsistent sample lengths after forecast object creation
expect_warning(
  example_sample_multivariate[-(1000:1010), ],
  "Found the following group with an inconsistent sample length"
)

  # Test with invalid grouping columns
expect_error(
  as_forecast_multivariate_sample(data,
    by = c("nonexistent_column")),
  "Must be a subset of"
)
})
