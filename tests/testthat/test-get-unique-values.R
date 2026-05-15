# ==============================================================================
# `get_unique_values()` # nolint: commented_code_linter
# ==============================================================================
test_that("get_unique_values() works with a quantile forecast", {
  forecast <- suppressMessages(as_forecast_quantile(example_quantile))
  result <- get_unique_values(forecast)

  expect_s3_class(result, c("data.table", "data.frame"), exact = TRUE)

  # Should have a column for the column name and one for the unique count
  expect_true("column" %in% names(result) || "Column" %in% names(result) ||
                length(names(result)) == 2)
  expect_true(ncol(result) == 2)

  # The column names in the result should be exactly the forecast unit columns
  fu <- get_forecast_unit(forecast)
  col_col <- names(result)[1]
  expect_setequal(result[[col_col]], fu)

  # Check some expected unique value counts (after clean_forecast with na.omit)
  count_col <- names(result)[2]
  location_count <- result[result[[col_col]] == "location", ][[count_col]]
  expect_identical(location_count, 4L)
  target_type_count <- result[result[[col_col]] == "target_type", ][[count_col]]
  expect_identical(target_type_count, 2L)
  location_name_count <- result[
    result[[col_col]] == "location_name",
  ][[count_col]]
  expect_identical(location_name_count, 4L)
})

test_that("get_unique_values() works with different forecast types", {
  forecast_binary <- suppressMessages(as_forecast_binary(example_binary))
  result_binary <- get_unique_values(forecast_binary)

  expect_s3_class(result_binary, c("data.table", "data.frame"), exact = TRUE)

  # Should only contain forecast unit columns, not protected columns
  col_col <- names(result_binary)[1]
  expect_false("predicted" %in% result_binary[[col_col]])
  expect_false("observed" %in% result_binary[[col_col]])

  forecast_sample <- suppressMessages(
    as_forecast_sample(example_sample_continuous)
  )
  result_sample <- get_unique_values(forecast_sample)

  expect_s3_class(result_sample, c("data.table", "data.frame"), exact = TRUE)

  # sample_id should not appear (it is a protected column)
  col_col_s <- names(result_sample)[1]
  expect_false("sample_id" %in% result_sample[[col_col_s]])
})

test_that("get_unique_values() returns correct output structure", {
  dt <- data.table::data.table(
    location = c("A", "A", "B"),
    model = c("m1", "m2", "m1"),
    observed = c(1, 2, 3),
    predicted = c(1.1, 2.1, 3.1)
  )
  forecast <- suppressMessages(as_forecast_point(dt))
  result <- get_unique_values(forecast)

  expect_s3_class(result, c("data.table", "data.frame"), exact = TRUE)

  col_col <- names(result)[1]
  count_col <- names(result)[2]

  # Should have exactly 2 rows: location and model
  expect_identical(nrow(result), 2L)
  expect_setequal(result[[col_col]], c("location", "model"))

  # observed and predicted should NOT appear
  expect_false("observed" %in% result[[col_col]])
  expect_false("predicted" %in% result[[col_col]])

  # Unique counts
  location_count <- result[result[[col_col]] == "location", ][[count_col]]
  model_count <- result[result[[col_col]] == "model", ][[count_col]]
  expect_identical(location_count, 2L)
  expect_identical(model_count, 2L)
})

test_that("get_unique_values() accepts a `by` argument for grouping", {
  forecast <- suppressMessages(as_forecast_quantile(example_quantile))
  result <- get_unique_values(forecast, by = "model")

  expect_s3_class(result, c("data.table", "data.frame"), exact = TRUE)

  # Should have a model column in the result

  expect_true("model" %in% names(result))

  # "model" should not appear in the column-name column since it's the

  # grouping variable
  col_col <- setdiff(names(result), c("model", names(result)[ncol(result)]))[1]
  if (is.na(col_col)) {
    col_col <- names(result)[1]
  }

  # Should have rows for each model x column combination
  # After na.omit there are 4 models
  fu <- setdiff(get_forecast_unit(forecast), "model")
  n_models <- length(unique(
    scoringutils:::clean_forecast(forecast, copy = TRUE, na.omit = TRUE)$model
  ))
  expect_gte(nrow(result), n_models)
})

test_that("get_unique_values() errors on non-forecast input", {
  expect_error(get_unique_values("not a forecast"))
  expect_error(get_unique_values(42))
})

test_that("get_unique_values() handles data with NAs correctly", {
  dt <- data.table::copy(example_quantile)
  dt[1:5, location := NA]
  forecast <- suppressMessages(as_forecast_quantile(dt))

  # Should not error
  expect_no_error(result <- get_unique_values(forecast))
  expect_s3_class(result, c("data.table", "data.frame"), exact = TRUE)
})
