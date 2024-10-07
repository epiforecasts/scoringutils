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


test_that("check_columns_present works", {
  expect_identical(
    capture.output(
      check_columns_present(example_binary, c("loc1", "loc2", "loc3"))
    ),
    paste(
      "[1] \"Columns 'loc1', 'loc2', 'loc3' not found in data\""
    )
  )
  expect_identical(
    capture.output(
      check_columns_present(example_binary, c("loc1"))
    ),
    paste(
      "[1] \"Column 'loc1' not found in data\""
    )
  )
  expect_true(
    check_columns_present(example_binary, c("location_name"))
  )
  expect_true(
    check_columns_present(example_binary, columns = NULL)
  )
})

test_that("test_columns_not_present works", {
  expect_true(
    test_columns_not_present(example_binary, "sample_id")
  )
  expect_false(
    test_columns_not_present(example_binary, "location")
  )
})

test_that("check_columns_present() works", {
  expect_equal(
    check_columns_present(example_quantile, c("observed", "predicted", "nop")),
    "Column 'nop' not found in data"
  )
  expect_true(
    check_columns_present(example_quantile, c("observed", "predicted"))
  )
})
