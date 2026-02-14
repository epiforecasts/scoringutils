test_that("Check equal length works if all arguments have length 1", {
  out <- interval_score(
    observed = 5,
    lower = 4,
    upper = 6,
    interval_range = 95,
    weigh = TRUE,
    separate_results = FALSE
  )
  expect_equal(out, 0.05)
})


test_that("test_columns_not_present works", {
  expect_true(
    test_columns_not_present(example_binary, "sample_id")
  )
  expect_false(
    test_columns_not_present(example_binary, "location")
  )
})

