test_that("print() works on forecast_* objects", {
  # Check print works on each forecast object
  test_dat <- list(na.omit(example_binary), na.omit(example_quantile),
                   na.omit(example_point), na.omit(example_sample_continuous), na.omit(example_sample_discrete))
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
