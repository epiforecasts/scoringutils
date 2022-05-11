test_that("Check equal length works if all arguments have length 1", {
  out <- interval_score(
    true_values = 5,
    lower = 4,
    upper = 6,
    interval_range = 95,
    weigh = TRUE,
    separate_results = FALSE
  )
  expect_equal(out, 0.05)
})
