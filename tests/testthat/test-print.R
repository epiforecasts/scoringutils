test_that("print() works on forecast_* objects", {
  # Check print works on each forecast object
  test_dat <- list(na.omit(example_binary), na.omit(example_quantile),
                   na.omit(example_point), na.omit(example_continuous), na.omit(example_integer))
  for (dat in test_dat){
    dat <- as_forecast(dat)
    forecast_type <- get_forecast_type(dat)
    forecast_unit <- get_forecast_unit(dat)

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
  test <- as_forecast(na.omit(example_quantile))
  test$observed <- NULL

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
  class(test) <- "forecast_point"
  expect_warning(
    expect_message(
      expect_message(
        expect_output(
          print(test),
          pattern = "Forecast unit:"
        ),
        "Could not determine forecast unit."
      ),
      "Could not determine forecast type"
    ),
    "Error in validating forecast object:"
  )
})
