test_that("validate_metrics() works as expected", {
  test_fun <- function(x, y, ...) {
    if (hasArg("test")) {
      message("test argument found")
    }
    return(y)
  }
  ## Additional tests for validate_metrics()
  # passing in something that's not a function or a known metric
  expect_warning(
    expect_warning(
      score(as_forecast_binary(na.omit(example_binary)), metrics = list(
        "test1" = test_fun, "test" = test_fun, "hi" = "hi", "2" = 3)
      ),
      "`Metrics` element number 3 is not a valid function"
    ),
    "`Metrics` element number 4 is not a valid function")
})
