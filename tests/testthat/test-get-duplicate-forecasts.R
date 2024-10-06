# ==============================================================================
# get_duplicate_forecasts()
# ==============================================================================
test_that("get_duplicate_forecasts() works as expected for quantile", {
  expect_no_condition(get_duplicate_forecasts(
    example_quantile,
    forecast_unit =
      c("location", "target_end_date", "target_type", "location_name",
        "forecast_date", "model")
  )
  )

  expect_equal(nrow(get_duplicate_forecasts(example_quantile)), 0)
  expect_equal(
    nrow(
      get_duplicate_forecasts(rbind(example_quantile, example_quantile[1000:1010]))),
    22
  )
})

test_that("get_duplicate_forecasts() works as expected for sample", {
  expect_equal(nrow(get_duplicate_forecasts(example_sample_continuous)), 0)
  expect_equal(
    nrow(
      get_duplicate_forecasts(rbind(example_sample_continuous, example_sample_continuous[1040:1050]))),
    22
  )
})


test_that("get_duplicate_forecasts() works as expected for binary", {
  expect_equal(nrow(get_duplicate_forecasts(example_binary)), 0)
  expect_equal(
    nrow(
      get_duplicate_forecasts(rbind(example_binary, example_binary[1000:1010]))),
    22
  )
})

test_that("get_duplicate_forecasts() works as expected for point", {
  expect_equal(nrow(get_duplicate_forecasts(example_binary)), 0)
  expect_equal(
    nrow(
      get_duplicate_forecasts(rbind(example_point, example_point[1010:1020]))),
    22
  )

  expect_s3_class(
    get_duplicate_forecasts(as.data.frame(example_point)),
    c("data.table", "data.frame"),
    exact = TRUE
  )
})

test_that("get_duplicate_forecasts() returns the expected class", {
  expect_equal(
    class(get_duplicate_forecasts(example_point)),
    c("data.table", "data.frame")
  )
})

test_that("get_duplicate_forecasts() works as expected with a data.frame", {
  duplicates <- get_duplicate_forecasts(
    rbind(example_quantile_df, example_quantile_df[101:110, ])
  )
  expect_equal(nrow(duplicates), 20)
})

test_that("get_duplicate_forecasts() shows counts correctly", {
  duplicates <- get_duplicate_forecasts(
    rbind(example_quantile, example_quantile[101:110, ]),
    counts = TRUE
  )
  expect_equal(nrow(duplicates), 2)
  expect_equal(unique(duplicates$n_duplicates), 10)
})


# ==============================================================================
# check_duplicates()
# ==============================================================================
test_that("check_duplicates works", {
  example_bin <- rbind(example_binary[1000:1002, ], example_binary[1000:1002, ])
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


test_that("check_duplicates() works", {
  bad <- rbind(
    example_quantile[1000:1010],
    example_quantile[1000:1010]
  )

  expect_equal(check_duplicates(bad),
               "There are instances with more than one forecast for the same target. This can't be right and needs to be resolved. Maybe you need to check the unit of a single forecast and add missing columns? Use the function get_duplicate_forecasts() to identify duplicate rows"
  )
})

