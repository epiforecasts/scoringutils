factor_levels <- c("one", "two", "three")
predicted_label <- factor(c("one", "two", "three"), levels = factor_levels, ordered = TRUE)
observed <- factor(c("one", "two", "two"), levels = factor_levels, ordered = TRUE)
predicted <- matrix(c(0.8, 0.1, 0.4,
                      0.1, 0.2, 0.4,
                      0.1, 0.7, 0.2),
                    nrow = 3)

# ============================================================================ #
# Input handling ===============================================================
# ============================================================================ #
test_that("Input checking for ordinal forecasts works", {
  # everything correct
  expect_no_condition(
    assert_input_ordinal(observed, predicted, predicted_label)
  )

  # works with a single observations
  expect_no_condition(
    assert_input_ordinal(observed[1], predicted[1, ], predicted_label)
  )
  expect_no_condition(
    assert_input_ordinal(observed[1], matrix(predicted[1, ], nrow = 1), predicted_label)
  )

  # observed and predicted_label have different level lengths
  observed_wrong <- factor(observed, levels = c("one", "two"), ordered = TRUE)
  expect_error(
    assert_input_ordinal(observed_wrong, predicted, predicted_label),
    "Assertion on 'predicted_label' failed: Must have length 2, but has length 3."
  )

  # observed and predicted_label have different levels, resulting in an `NA` in predicted_label
  predicted_label_wrong <- factor(predicted_label, levels = c("one", "two", "four"))
  expect_error(
    assert_input_ordinal(observed, predicted, predicted_label_wrong),
    "Assertion on 'predicted_label' failed: Contains missing values"
  )

  # 6 observations, but only 3 forecasts
  observed_wrong <- factor(
    c("one", "two", "one", "one", "two", "three"),
    levels = c("one", "two", "three"),
    ordered = TRUE
  )
  expect_error(
    assert_input_ordinal(observed_wrong, predicted, predicted_label),
    "Assertion on 'predicted' failed: Must have exactly 6 rows, but has 3 rows."
  )

  # observed value or predicted_label is not factor
  expect_error(
    assert_input_ordinal(as.numeric(observed), predicted, predicted_label),
    "Assertion on 'observed' failed: Must be of type 'factor', not 'double'."
  )
  expect_error(
    assert_input_ordinal(observed, predicted, as.character(predicted_label)),
    "Assertion on 'predicted_label' failed: Must be of type 'factor', not 'character'."
  )

  # a single observation
  expect_no_condition(
    assert_input_ordinal(observed[1], predicted[1, ], predicted_label),
  )
  expect_no_condition(
    assert_input_ordinal(observed[1], as.numeric(predicted[1, ]), predicted_label),
  )

  # wrong dimensions
  expect_error(
    assert_input_ordinal(observed[1], predicted, predicted_label),
    "One of the following must apply:"
  )

  # NA values in observed are permitted
  observed2 <- factor(c("one", "two", "missing"), levels = c("one", "two", "three"), ordered = TRUE)
  expect_no_condition(
    assert_input_ordinal(observed2, predicted, predicted_label)
  )

  # NA values in predicted are permitted and treated as zeros
  # (as long as probabilities still sum to one)
  predicted2 <- predicted
  predicted2[2, 2] <- NA
  expect_error(
    assert_input_ordinal(observed, predicted2, predicted_label),
    "Probabilities belonging to a single forecast must sum to one"
  )
  predicted2[2, 1] <- 0.3
  expect_no_condition(
    assert_input_ordinal(observed, predicted2, predicted_label)
  )
})


# ============================================================================ #
# logs ordinal =============================================================== #
# ============================================================================ #
test_that("logs_categorical() works as expected with ordered factors", {
  expect_no_condition(
    res <- logs_categorical(observed, predicted, predicted_label)
  )
  res_manual <- -log(c(predicted[1, 1], predicted[2, 2], predicted[3, 2]))
  expect_equal(res, res_manual)
})

# ============================================================================ #
# rps ordinal =============================================================== #
# ============================================================================ #
result <- c(0.05, 0.5, 0.2)
test_that("rps_ordinal() works as expected", {
  expect_no_condition(
    res <- rps_ordinal(observed, predicted, predicted_label)
  )

  expect_equal(res, result)

  # works with changed order of levels
  predicted_label2 <- factor(c("three", "one", "two"), levels = factor_levels, ordered = TRUE)
  expect_equal(
    rps_ordinal(observed, predicted, predicted_label2),
    scoringRules::rps_probs(as.numeric(observed), predicted[, c(3, 1, 2)])
  )
})

test_that("rps_ordinal() works with a single observation", {
  expect_equal(
    rps_ordinal(observed[1], predicted[1, ], predicted_label),
    result[1]
  )
})
