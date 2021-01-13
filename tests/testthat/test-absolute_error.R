test_that("absolute error (sample based) works", {
  true_values <- rnorm(30, mean = 1:30)
  predicted_values <- rnorm(30, mean = 1:30)

  scoringutils2 <- scoringutils2::ae_median_sample(true_values, predicted_values)
  scoringutils <- scoringutils::ae_median(true_values, predicted_values)

  expect_equal(scoringutils2, scoringutils)
})


