test_that("absolute error (sample based) works", {
  true_values <- rnorm(30, mean = 1:30)
  predicted_values <- rnorm(30, mean = 1:30)

  scoringutils <- scoringutils::ae_median_sample(true_values, predicted_values)

  ae <- abs(true_values - predicted_values)
  expect_equal(ae, scoringutils)
})


