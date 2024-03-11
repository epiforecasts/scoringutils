test_that("validate_metrics() works as expected", {
  ## Additional tests for validate_metrics()
  # passing in something that's not a function or a known metric
  expect_warning(
    expect_warning(
      score(df, metrics = list(
        "test1" = identity, "test" = test_fun, "hi" = "hi", "2" = 3)
      ),
      "`Metrics` element number 3 is not a valid function"
    ),
    "`Metrics` element number 4 is not a valid function")
})
