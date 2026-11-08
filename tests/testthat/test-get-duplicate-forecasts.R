# ==============================================================================
# get_duplicate_forecasts() # nolint: commented_code_linter
# ==============================================================================
test_that("get_duplicate_forecasts() works for quantile forecasts", {
  fc <- as_forecast_quantile(example_quantile)
  expect_identical(nrow(get_duplicate_forecasts(fc)), 0L)

  fc_dup <- rbind(fc, fc[1000:1010])
  class(fc_dup) <- class(fc)
  expect_identical(nrow(get_duplicate_forecasts(fc_dup)), 22L)
})

test_that("get_duplicate_forecasts() works for sample forecasts", {
  fc <- as_forecast_sample(example_sample_continuous)
  expect_identical(nrow(get_duplicate_forecasts(fc)), 0L)

  fc_dup <- rbind(fc, fc[1040:1050])
  class(fc_dup) <- class(fc)
  expect_identical(nrow(get_duplicate_forecasts(fc_dup)), 22L)
})


test_that("get_duplicate_forecasts() works for binary forecasts", {
  fc <- as_forecast_binary(example_binary)
  expect_identical(nrow(get_duplicate_forecasts(fc)), 0L)

  fc_dup <- rbind(fc, fc[1000:1010])
  class(fc_dup) <- class(fc)
  expect_identical(nrow(get_duplicate_forecasts(fc_dup)), 22L)
})

test_that("get_duplicate_forecasts() works for point forecasts", {
  fc <- as_forecast_point(example_point)
  expect_identical(nrow(get_duplicate_forecasts(fc)), 0L)

  fc_dup <- rbind(fc, fc[1010:1020])
  class(fc_dup) <- class(fc)
  expect_identical(nrow(get_duplicate_forecasts(fc_dup)), 22L)
})

test_that("get_duplicate_forecasts() works with type on raw data", {
  bad <- rbind(example_quantile, example_quantile[1000:1010])
  expect_identical(
    nrow(get_duplicate_forecasts(bad, type = "quantile")),
    22L
  )
  expect_identical(
    nrow(get_duplicate_forecasts(example_quantile, type = "quantile")),
    0L
  )
})

test_that("get_duplicate_forecasts() warns without type on raw data", {
  raw <- as.data.frame(example_quantile)
  expect_warning(
    get_duplicate_forecasts(raw),
    "without specifying `type` was deprecated in scoringutils 2.3.0"
  )
})

test_that("deprecated fallback preserves old behaviour", {
  bad <- rbind(example_quantile, example_quantile[1000:1010])
  raw <- as.data.frame(bad)
  suppressWarnings(
    result <- get_duplicate_forecasts(raw)
  )
  expect_identical(nrow(result), 22L)
})

test_that("get_duplicate_forecasts() ignores type on forecast objects", {
  fc <- as_forecast_quantile(example_quantile)
  expect_identical(
    nrow(get_duplicate_forecasts(fc, type = "sample")),
    nrow(get_duplicate_forecasts(fc))
  )
})

test_that("get_duplicate_forecasts() respects forecast_unit argument", {
  fc <- as_forecast_quantile(
    example_quantile,
    forecast_unit = c(
      "location", "target_end_date", "target_type", "location_name",
      "forecast_date", "model"
    )
  )
  expect_no_condition(get_duplicate_forecasts(fc))
})

test_that("get_duplicate_forecasts() returns the expected class", {
  fc <- as_forecast_point(example_point)
  expect_s3_class(
    get_duplicate_forecasts(fc),
    c("data.table", "data.frame")
  )
})

test_that("get_duplicate_forecasts() shows counts correctly", {
  fc <- as_forecast_quantile(example_quantile)
  fc_dup <- rbind(fc, fc[101:110])
  class(fc_dup) <- class(fc)
  duplicates <- get_duplicate_forecasts(fc_dup, counts = TRUE)
  expect_identical(nrow(duplicates), 2L)
  expect_identical(unique(duplicates$n_duplicates), 10L)
})


# ==============================================================================
# get_forecast_type_ids() # nolint: commented_code_linter
# ==============================================================================
test_that("get_forecast_type_ids() returns correct columns per type", {
  expect_identical(
    get_forecast_type_ids(as_forecast_quantile(example_quantile)),
    "quantile_level"
  )
  expect_identical(
    get_forecast_type_ids(as_forecast_sample(example_sample_continuous)),
    "sample_id"
  )
  expect_identical(
    get_forecast_type_ids(as_forecast_nominal(example_nominal)),
    "predicted_label"
  )
  expect_identical(
    get_forecast_type_ids(as_forecast_ordinal(example_ordinal)),
    "predicted_label"
  )
})


test_that("get_forecast_type_ids() default returns no IDs", {
  expect_identical(
    get_forecast_type_ids(as_forecast_binary(example_binary)),
    character(0)
  )
  expect_identical(
    get_forecast_type_ids(as_forecast_point(example_point)),
    character(0)
  )
  expect_identical(
    get_forecast_type_ids(data.frame(x = 1)),
    character(0)
  )
})


# ==============================================================================
# check_duplicates() # nolint: commented_code_linter
# ==============================================================================
test_that("check_duplicates includes type and forecast_unit in message", {
  fc <- as_forecast_quantile(example_quantile)
  fc_dup <- rbind(fc[1000:1010], fc[1000:1010])
  class(fc_dup) <- class(fc)

  msg <- check_duplicates(fc_dup)
  expect_match(msg, 'type = "quantile"')
  expect_match(msg, "forecast_unit")
  expect_match(msg, "get_duplicate_forecasts")
})


test_that("check_duplicates lists the duplicated forecast units", {
  fc <- as_forecast_point(example_point)
  fc_dup <- rbind(fc[1000:1001], fc[1000:1001])
  class(fc_dup) <- class(fc)

  msg <- check_duplicates(fc_dup)
  expect_match(msg, "Duplicated forecast units:", fixed = TRUE)
  expect_match(
    msg,
    paste0(
      "\n  - location = IT, target_end_date = 2021-07-17, ",
      "target_type = Cases, location_name = Italy, ",
      "forecast_date = 2021-06-28, model = EuroCOVIDhub-baseline, ",
      "horizon = 3 (2 rows)"
    ),
    fixed = TRUE
  )
  expect_no_match(msg, "more forecast unit", fixed = TRUE)
})


test_that("check_duplicates shows at most five duplicated forecast units", {
  fc <- as_forecast_point(example_point)
  fc_dup <- rbind(fc[1000:1006], fc[1000:1006])
  class(fc_dup) <- class(fc)

  msg <- check_duplicates(fc_dup)
  expect_length(gregexpr("\n  - ", msg, fixed = TRUE)[[1]], 5L)
  expect_match(msg, "\n  ... and 2 more forecast unit(s)", fixed = TRUE)
})


test_that("check_duplicates counts all rows of a duplicated quantile forecast", {
  fc <- na.omit(as_forecast_quantile(example_quantile))
  fc_dup <- rbind(fc, fc[1:23])
  class(fc_dup) <- class(fc)

  msg <- check_duplicates(fc_dup)
  expect_length(gregexpr("\n  - ", msg, fixed = TRUE)[[1]], 1L)
  expect_match(
    msg,
    "model = EuroCOVIDhub-ensemble, horizon = 1 (46 rows)",
    fixed = TRUE
  )
})


test_that("check_duplicates groups correctly when data has a column `fu`", {
  fc <- as_forecast_point(example_point)
  fc[, fu := "a"]
  fc_dup <- rbind(fc[1000:1002], fc[1000:1002])
  class(fc_dup) <- class(fc)

  msg <- check_duplicates(fc_dup)
  expect_length(gregexpr("\n  - ", msg, fixed = TRUE)[[1]], 3L)
  expect_match(
    msg,
    "\n  - location = IT, target_end_date = 2021-07-17, ",
    fixed = TRUE
  )
  expect_match(msg, "horizon = 3, fu = a (2 rows)", fixed = TRUE)
})


test_that("check_duplicates returns TRUE when no duplicates", {
  fc <- as_forecast_binary(example_binary)
  expect_true(check_duplicates(fc))
})


test_that("check_duplicates detects binary duplicates", {
  fc <- as_forecast_binary(example_binary)
  fc_dup <- rbind(fc[1000:1002], fc[1000:1002])
  class(fc_dup) <- class(fc)
  expect_match(
    check_duplicates(fc_dup),
    "There are instances with more than one forecast"
  )
})
