test_that("print() works on forecast_* objects", {
  # Check print works on each forecast object
  test_dat <- list(example_binary, example_quantile,
                   example_point, example_sample_continuous,
                   example_sample_discrete)
  test_dat <- lapply(test_dat, na.omit)
  for (dat in test_dat){
    forecast_type <- scoringutils:::get_forecast_type(dat)
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
    output_original <- suppressMessages(capture.output(print(dat)))
    output_test <- suppressMessages(capture.output(print(data.table(dat))))
    expect_contains(output_original, output_test)
  }
})
