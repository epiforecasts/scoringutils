# ==============================================================================
# as_forecast_multivariate_sample() # nolint: commented_code_linter
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
      joint_across = "location",
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
    joint_across = "location",
    sample_id = "sample"
  )

  # Snapshot the print output to capture the structure
  expect_snapshot(print(result))

  # Snapshot the class and structure
  expect_snapshot({
    cat("Class:", class(result), "\n")
    cat(
      "Forecast type:",
      scoringutils:::get_forecast_type(result), "\n"
    )
    cat(
      "Forecast unit:",
      toString(get_forecast_unit(result)), "\n"
    )
    cat("Number of rows:", nrow(result), "\n")
    cat("Number of columns:", ncol(result), "\n")
    cat(
      "Column names:",
      toString(names(result)), "\n"
    )
    cat(
      "Number of unique groups:",
      length(unique(result$.mv_group_id)), "\n"
    )
  })
})

test_that("class includes both new and deprecated class names", {
  result <- as_forecast_multivariate_sample(
    na.omit(data.table::copy(example_sample_continuous)),
    joint_across = "location"
  )
  expect_s3_class(result, "forecast_multivariate_sample")
  expect_s3_class(result, "forecast_sample_multivariate")
  expect_s3_class(result, "forecast")
  # New class should come before deprecated class
  cls <- class(result)
  expect_lt(
    which(cls == "forecast_multivariate_sample"),
    which(cls == "forecast_sample_multivariate")
  )
})


# ==============================================================================
# is_forecast_multivariate_sample()
# ==============================================================================
test_that("is_forecast_multivariate_sample() works as expected", {
  expect_true(
    is_forecast_multivariate_sample(example_multivariate_sample)
  )
  expect_false(
    is_forecast_multivariate_sample(example_binary)
  )
  expect_false(
    is_forecast_sample(example_multivariate_sample)
  )
})

test_that("is_forecast_sample_multivariate() is deprecated", {
  expect_warning(
    result <- is_forecast_sample_multivariate(
      example_multivariate_sample
    ),
    "deprecated"
  )
  expect_true(result)
})


# ==============================================================================
# get_metrics.forecast_multivariate_sample()
# ==============================================================================

test_that(
  "get_metrics.forecast_multivariate_sample() works as expected",
  {
    expect_type(
      get_metrics(example_multivariate_sample), "list"
    )
  }
)


# ==============================================================================
# set_grouping() and get_grouping()
# ==============================================================================
test_that("set_grouping() works as expected", {
  data <- example_multivariate_sample
  grouping <- c("model", "target_type", "target_end_date", "horizon")

  # Test basic functionality
  result <- set_grouping(data, grouping)
  expect_true(".mv_group_id" %in% names(result))
  expect_type(result$.mv_group_id, "integer")

  # Test that groups are consistent
  group_counts <- as.data.table(result)[, .N, by = .mv_group_id]
  expect_true(all(group_counts$N > 0))
})

test_that("get_grouping() works as expected", {
  data <- example_multivariate_sample
  grouping <- c(
    "model", "target_type", "target_end_date", "horizon"
  )
  joint_across <- setdiff(get_forecast_unit(data), grouping)
  data <- scoringutils:::set_grouping(data, joint_across)

  # Test that get_grouping returns the correct columns
  result <- get_grouping(data)
  expect_type(result, "character")
  expect_true(all(grouping %in% result))
})

test_that(
  "get_grouping() errors when mv_group_id is missing",
  {
    data <- example_multivariate_sample

    # Remove the mv_group_id column
    data_bad <- as.data.table(data)
    data_bad[, .mv_group_id := NULL]

    expect_error(
      get_grouping(data_bad),
      "No column `.mv_group_id` found in the forecast object."
    )
  }
)

test_that("set_grouping() preserves existing keys correctly", {
  data <- example_multivariate_sample
  grouping <- c(
    "model", "target_type", "target_end_date", "horizon"
  )

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

  result_with_keys <- scoringutils:::set_grouping(
    data_with_keys, grouping
  )
  expect_equal(key(result_with_keys), original_keys)

  # Test case 3: Verify functionality still works with keys preserved
  expect_true(".mv_group_id" %in% names(result_with_keys))
  expect_type(result_with_keys$.mv_group_id, "integer")

  # Test that groups are consistent
  group_counts <- as.data.table(
    result_with_keys
  )[, .N, by = .mv_group_id]
  expect_true(all(group_counts$N > 0))
})

# ==============================================================================
# variogram_score_multivariate()
# ==============================================================================
test_that(
  "variogram_score_multivariate() works as expected",
  {
    data <- example_multivariate_sample

    # Test that variogram score is included in default metrics
    metrics <- get_metrics(data)
    expect_true("variogram_score" %in% names(metrics))

    # Test basic scoring includes variogram_score
    scores <- score(data)
    expect_true(is.data.table(scores))
    expect_true("variogram_score" %in% names(scores))

    # Test that scores are numeric and non-negative
    expect_type(scores$variogram_score, "double")
    expect_true(
      all(scores$variogram_score >= 0, na.rm = TRUE)
    )
  }
)

test_that(
  "variogram_score_multivariate() works with custom p",
  {
    data <- example_multivariate_sample

    # Test with p = 1
    scores_p1 <- score(
      data,
      metrics = list(
        variogram_score = purrr::partial(
          variogram_score_multivariate, p = 1
        )
      )
    )
    expect_true(is.data.table(scores_p1))
    expect_type(scores_p1$variogram_score, "double")
    expect_true(
      all(scores_p1$variogram_score >= 0, na.rm = TRUE)
    )

    # Test with p = 0.5 (default)
    scores_p05 <- score(
      data,
      metrics = list(
        variogram_score = variogram_score_multivariate
      )
    )
    expect_type(scores_p05$variogram_score, "double")

    # Scores with different p values should differ
    expect_false(
      all(scores_p1$variogram_score ==
            scores_p05$variogram_score)
    )
  }
)


# ==============================================================================
# score.forecast_multivariate_sample()
# ==============================================================================
test_that(
  "score.forecast_multivariate_sample() works as expected",
  {
    data <- example_multivariate_sample

    # Test basic scoring
    scores <- score(data)
    expect_true(is.data.table(scores))
    expect_true("energy_score" %in% names(scores))
    expect_true("variogram_score" %in% names(scores))

    # Test with specific metrics
    scores <- score(
      data,
      metrics = list(
        energy_score = energy_score_multivariate
      )
    )
    expect_true(is.data.table(scores))
    expect_true("energy_score" %in% names(scores))

    # Test that scores are numeric
    expect_type(scores$energy_score, "double")
  }
)

test_that(
  "score.forecast_multivariate_sample() creates expected output",
  {
    data <- example_multivariate_sample

    # Test basic scoring and capture snapshot
    scores <- score(data)

    # Snapshot the print output to capture the structure
    expect_snapshot(print(scores))

    # Snapshot the detailed structure
    expect_snapshot({
      cat("Class:", class(scores), "\n")
      cat("Number of rows:", nrow(scores), "\n")
      cat("Number of columns:", ncol(scores), "\n")
      cat(
        "Column names:",
        toString(names(scores)), "\n"
      )
      cat(
        "Energy score range:",
        paste(
          range(scores$energy_score, na.rm = TRUE),
          collapse = " to "
        ), "\n"
      )
      cat(
        "Number of non-NA energy scores:",
        sum(!is.na(scores$energy_score)), "\n"
      )
      cat(
        "Sample of energy scores:",
        toString(head(scores$energy_score, 5)), "\n"
      )
    })

    # Test with specific metrics and capture snapshot
    scores_specific <- score(
      data,
      metrics = list(
        energy_score = energy_score_multivariate
      )
    )

    expect_snapshot({
      cat("=== Specific Metrics Test ===\n")
      cat("Class:", class(scores_specific), "\n")
      cat("Number of rows:", nrow(scores_specific), "\n")
      cat(
        "Number of columns:", ncol(scores_specific), "\n"
      )
      cat(
        "Column names:",
        toString(names(scores_specific)), "\n"
      )
      cat(
        "Energy score range:",
        paste(
          range(
            scores_specific$energy_score, na.rm = TRUE
          ),
          collapse = " to "
        ), "\n"
      )
    })
  }
)

# ==============================================================================
# Error cases for as_forecast_multivariate_sample()
# ==============================================================================
test_that(
  "as_forecast_multivariate_sample() handles errors appropriately",
  {
    data <- as.data.table(example_multivariate_sample)

    # Test with missing required columns
    data_bad <- data[, !"sample_id"]
    expect_error(
      as_forecast_multivariate_sample(
        data_bad,
        joint_across = c("location", "location_name")
      ),
      "Column 'sample_id' not found in data."
    )

    # Test with inconsistent sample lengths within groups
    data_bad <- copy(data)
    data_bad <- rbind(data_bad[1000:1010], data_bad)
    expect_error(
      as_forecast_multivariate_sample(
        data_bad,
        joint_across = c("location", "location_name")
      ),
      "All univariate forecasts"
    )

    # Test with inconsistent sample lengths after object creation
    expect_warning(
      example_multivariate_sample[-(1000:1010), ],
      "Found the following group with an inconsistent"
    )

    # Test that joint_across is required when .mv_group_id is absent
    data_no_group <- data[, !".mv_group_id"]
    expect_error(
      as_forecast_multivariate_sample(data_no_group),
      "joint_across.*must be provided"
    )

    # Test with invalid grouping columns
    expect_error(
      as_forecast_multivariate_sample(
        data,
        joint_across = c("nonexistent_column")
      ),
      "Must be a subset of"
    )
  }
)
