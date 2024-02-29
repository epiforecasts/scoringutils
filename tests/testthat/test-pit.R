test_that("pit_sample() function throws an error when missing observed", {
  observed <- rpois(10, lambda = 1:10)
  predicted <- replicate(50, rpois(n = 10, lambda = 1:10))

  expect_error(
    pit_sample(predicted = predicted),
    "missing in function"
  )
})

test_that("pit_sample() function throws an error when missing 'predicted'", {
  observed <- rpois(10, lambda = 1:10)
  predicted <- replicate(50, rpois(n = 10, lambda = 1:10))

  expect_error(
    pit_sample(predicted = predicted),
    "missing in function"
  )
})


test_that("pit_sample() function works for integer observed and predicted", {
  observed <- rpois(10, lambda = 1:10)
  predicted <- replicate(10, rpois(10, lambda = 1:10))
  output <- pit_sample(
    observed = observed,
    predicted = predicted,
    n_replicates = 56
  )
  expect_equal(
    length(output),
    560
  )
})

test_that("pit_sample() function works for continuous observed and predicted", {
  observed <- rnorm(10)
  predicted <- replicate(10, rnorm(10))
  output <- pit_sample(
    observed = observed,
    predicted = predicted,
    n_replicates = 56
  )
  expect_equal(
    length(output),
    10
  )
})

test_that("pit function works for continuous integer and quantile data", {
  pit_quantile <- suppressMessages(pit(example_quantile, by = "model"))
  pit_continuous <- suppressMessages(pit(example_continuous,
    by = c("model", "target_type")
  ))
  pit_integer <- suppressMessages(pit(example_integer,
    by = c("model", "location")
  ))

  expect_equal(names(pit_quantile), c("model", "quantile_level", "pit_value"))
  expect_equal(names(pit_continuous), c("model", "target_type", "pit_value"))
  expect_equal(names(pit_integer), c("model", "location", "pit_value"))

  # check printing works
  testthat::expect_output(print(pit_quantile))
  testthat::expect_output(print(pit_continuous))
  testthat::expect_output(print(pit_integer))

  # check class is correct
  expect_s3_class(pit_quantile, c("data.table", "data.frame"), exact = TRUE)
  expect_s3_class(pit_continuous, c("data.table", "data.frame"), exact = TRUE)
  expect_s3_class(pit_integer, c("data.table", "data.frame"), exact = TRUE)
})
