# ==============================================================================
# `get_forecast_counts()` # nolint: commented_code_linter
# ==============================================================================
test_that("get_forecast_counts() works as expected", {
  af <- data.table::copy(example_quantile)
  af <- get_forecast_counts(
    af,
    by = c("model", "target_type", "target_end_date")
  )

  expect_type(af, "list")
  expect_type(af$target_type, "character")
  expect_type(af$count, "integer")
  expect_identical(nrow(af[is.na(count)]), 0L)
  af <- get_forecast_counts(example_quantile, by = "model")
  expect_identical(nrow(af), 4L)
  expect_identical(af$count, c(256L, 256L, 128L, 247L))

  # Ensure the returning object class is exactly same as a data.table.
  expect_s3_class(af, c("data.table", "data.frame"), exact = TRUE)

  # Setting `collapse = c()` means that all quantiles and samples are counted
  af <- get_forecast_counts(
    example_quantile, by = "model", collapse = character(0)
  )
  expect_identical(nrow(af), 4L)
  expect_identical(af$count, c(5888L, 5888L, 2944L, 5681L))

  af <- get_forecast_counts(example_quantile)
  expect_identical(nrow(af), 50688L)

  expect_error(
    get_forecast_counts(example_quantile, by = NULL),
    "Assertion on 'by' failed: Must be a subset of"
  )

  # check whether collapsing also works for model-based forecasts
  af <- get_forecast_counts(example_sample_discrete, by = "model")
  expect_identical(nrow(af), 4L)

  af <- get_forecast_counts(
    example_sample_discrete, by = "model", collapse = character(0)
  )
  expect_identical(af$count, c(10240L, 10240L, 5120L, 9880L))
})


# ==============================================================================
# `plot_forecast_counts()` # nolint: commented_code_linter
# ==============================================================================
test_that("plot_forecast_counts() works as expected", {
  available_forecasts <- na.omit(example_quantile) |>
    as_forecast_quantile() |>
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
