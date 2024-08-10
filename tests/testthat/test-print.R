test_that("print() works on forecast_* objects", {
  # Check print works on each forecast object
  test_dat <- list(forecast_binary, forecast_quantile,
                   forecast_point, forecast_sample_continuous,
                   forecast_sample_discrete)
  for (dat in test_dat){
    forecast_type <- get_forecast_type(dat)
    forecast_unit <- get_forecast_unit(dat)

    fn_name <- paste0("as_forecast_", forecast_type)
    fn <- get(fn_name)
    dat <- suppressWarnings(suppressMessages(do.call(fn, list(dat))))

    # Check Forecast type
    expect_snapshot(print(dat))
    expect_snapshot(print(dat))
    # Check Forecast unit
    expect_snapshot(print(dat))
    expect_snapshot(print(dat))

    # Check print.data.table works.
    output_original <- capture.output(print(dat))
    output_test <- capture.output(print(data.table(dat)))
    expect_contains(output_original, output_test)
  }
})

test_that("print methods fail gracefully", {
  test <- as_forecast_quantile(na.omit(example_quantile))
  class(test) <- c("forecast", "data.table", "data.frame")

  # message if forecast type can't be computed
  expect_warning(
    expect_message(
      expect_output(
        print(test),
        pattern = "Forecast unit:"
      ),
      "Could not determine forecast type due to error in validation."
    ),
    "Error in validating forecast object:"
  )

  # message if forecast unit can't be computed
  test <- 1:10
  class(test) <- c("forecast", "forecast_point")
  expect_warning(
    expect_message(
      expect_output(
        print(test),
        pattern = "Forecast unit:"
      ),
      "Could not determine forecast unit."
    ),
    "Error in validating forecast object:"
  )
})
