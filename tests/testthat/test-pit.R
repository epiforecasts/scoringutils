test_that("pit_sample() function throws an error when missing observed", {
  observed <- rpois(10, lambda = 1:10)
  predicted <- replicate(50, rpois(n = 10, lambda = 1:10))

  expect_error(
    pit_sample(predicted = predicted),
    "observed` or `predicted` missing in function 'pit_sample()"
  )
})

test_that("pit_sample() function throws an error when missing 'predicted'", {
  observed <- rpois(10, lambda = 1:10)
  predicted <- replicate(50, rpois(n = 10, lambda = 1:10))

  expect_error(
    pit_sample(predicted = predicted),
    "observed` or `predicted` missing in function 'pit_sample()"
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
  pit1 <- suppressMessages(pit(example_quantile, by = "model"))
  pit2 <- suppressMessages(pit(example_continuous,
    by = c("model", "target_type")
  ))
  pit3 <- suppressMessages(pit(example_integer,
    by = c("model", "location")
  ))

  expect_equal(names(pit1), c("model", "quantile", "pit_value"))
  expect_equal(names(pit2), c("model", "target_type", "pit_value"))
  expect_equal(names(pit3), c("model", "location", "pit_value"))
})

