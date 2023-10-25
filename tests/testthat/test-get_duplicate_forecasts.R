test_that("get_duplicate_forecasts() works as expected for quantile", {
  expect_equal(nrow(get_duplicate_forecasts(example_quantile)), 0)
  expect_equal(
    nrow(
      get_duplicate_forecasts(rbind(example_quantile, example_quantile[1000:1010]))),
    22
  )
})

test_that("get_duplicate_forecasts() works as expected for sample", {
  expect_equal(nrow(get_duplicate_forecasts(example_continuous)), 0)
  expect_equal(
    nrow(
      get_duplicate_forecasts(rbind(example_continuous, example_continuous[1040:1050]))),
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
})

