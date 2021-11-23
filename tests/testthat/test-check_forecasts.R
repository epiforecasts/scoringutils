test_that("check_forecasts() function works", {
  check <- check_forecasts(quantile_example_data)
  expect_equal(class(check), "scoringutils_check")
})


test_that("check_forecasts() function has an error for empty data.frame", {
  expect_error(check_forecasts(data.frame()))
})
