# ==============================================================================
# `get_forecast_counts()`
# ==============================================================================
test_that("get_forecast_counts() works as expected", {
  af <- data.table::copy(example_quantile)
  af <- get_forecast_counts(
    af,
    by = c("model", "target_type", "target_end_date")
  )

  expect_type(af, "list")
  expect_type(af$target_type, "character")
  expect_type(af$`count`, "integer")
  expect_equal(nrow(af[is.na(`count`)]), 0)
  af <- example_quantile %>%
    get_forecast_counts(by = "model")
  expect_equal(nrow(af), 4)
  expect_equal(af$`count`, c(256, 256, 128, 247))

  # Ensure the returning object class is exactly same as a data.table.
  expect_s3_class(af, c("data.table", "data.frame"), exact = TRUE)

  # Setting `collapse = c()` means that all quantiles and samples are counted
  af <- example_quantile %>%
    get_forecast_counts(by = "model", collapse = c())
  expect_equal(nrow(af), 4)
  expect_equal(af$`count`, c(5888, 5888, 2944, 5681))

  af <- example_quantile %>%
    get_forecast_counts()
  expect_equal(nrow(af), 50688)

  expect_error(
    get_forecast_counts(example_quantile, by = NULL),
    "Assertion on 'by' failed: Must be a subset of"
  )

  # check whether collapsing also works for model-based forecasts
  af <- example_sample_discrete %>%
    get_forecast_counts(by = "model")
  expect_equal(nrow(af), 4)

  af <- example_sample_discrete %>%
    get_forecast_counts(by = "model", collapse = c())
  expect_equal(af$count, c(10240, 10240, 5120, 9880))
})


# ==============================================================================
# `plot_forecast_counts()`
# ==============================================================================
test_that("plot_forecast_counts() works as expected", {
  available_forecasts <- na.omit(example_quantile) %>%
    as_forecast_quantile() %>%
    get_forecast_counts(
      by = c("model", "target_type", "target_end_date")
    )
  p <- plot_forecast_counts(available_forecasts,
    x = "target_end_date", show_counts = FALSE
  ) +
    facet_wrap("target_type")
  expect_s3_class(p, "ggplot")
  skip_on_cran()
  vdiffr::expect_doppelganger("plot_available_forecasts", p)
})
