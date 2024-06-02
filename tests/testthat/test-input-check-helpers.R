test_that("Check equal length works if all arguments have length 1", {
  out <- interval_score(
    observed = 5,
    lower = 4,
    upper = 6,
    interval_range = 95,
    weigh = TRUE,
    separate_results = FALSE
  )
  expect_equal(out, 0.05)
})

test_that("ensure_model_column works", {
  test <- data.table::copy(example_binary)
  expect_warning(
    ensure_model_column(test[, model := NULL]),
    "There is no column called `model` in the data."
  )
  expect_true(
    setequal(
      ensure_model_column(example_binary),
      example_binary
    )
  )
})

test_that("check_number_per_forecast works", {
  expect_identical(
    capture.output(
      check_number_per_forecast(
        example_binary, forecast_unit = "location_name"
      )
    ),
    paste(
      "[1] \"Some forecasts have different numbers of rows",
      "(e.g. quantiles or samples). scoringutils found: 224, 215.",
      "This may be a problem (it can potentially distort scores,",
      "making it more difficult to compare them),",
      "so make sure this is intended.\""
    )
  )
  expect_true(
    check_number_per_forecast(
      example_binary
    )
  )
})


test_that("check_duplicates works", {
  example_bin <- rbind(example_binary[1:2, ], example_binary[1:2, ])
  expect_identical(
    capture.output(
      check_duplicates(example_bin)
    ),
    paste(
      "[1] \"There are instances with more than one forecast for the same",
      "target. This can't be right and needs to be resolved.",
      "Maybe you need to check the unit of a single forecast and add",
      "missing columns? Use the function get_duplicate_forecasts() to",
      "identify duplicate rows\""
    )
  )
  expect_true(
    check_duplicates(example_binary)
  )
})

test_that("check_columns_present works", {
  expect_identical(
    capture.output(
      check_columns_present(example_binary, c("loc1", "loc2", "loc3"))
    ),
    paste(
      "[1] \"Columns 'loc1', 'loc2', 'loc3' not found in data\""
    )
  )
  expect_identical(
    capture.output(
      check_columns_present(example_binary, c("loc1"))
    ),
    paste(
      "[1] \"Column 'loc1' not found in data\""
    )
  )
  expect_true(
    check_columns_present(example_binary, c("location_name"))
  )
  expect_true(
    check_columns_present(example_binary, columns = NULL)
  )
})

test_that("test_columns_not_present works", {
  expect_true(
    test_columns_not_present(example_binary, "sample_id")
  )
  expect_false(
    test_columns_not_present(example_binary, "location")
  )
})
