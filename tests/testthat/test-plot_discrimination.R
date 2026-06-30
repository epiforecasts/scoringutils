forecast_binary <- na.omit(as_forecast_binary(example_binary))

test_that("plot_discrimination() works with default histogram type", {
  p <- plot_discrimination(forecast_binary)
  expect_s3_class(p, "ggplot")
})

test_that("plot_discrimination() works with density type", {
  p <- plot_discrimination(forecast_binary, type = "density")
  expect_s3_class(p, "ggplot")
  skip_on_cran()
  skip_if(getRversion() < "4.2.0", "density estimation differs on R < 4.2")
  vdiffr::expect_doppelganger("plot_discrimination_density", p)
})

test_that("plot_discrimination() works with faceting by model", {
  p <- plot_discrimination(forecast_binary) +
    facet_wrap(~model)
  expect_s3_class(p, "ggplot")
})

test_that("plot_discrimination() errors for non-forecast input", {
  df <- data.frame(
    observed = factor(c("0", "0", "1", "1"), levels = c("0", "1")),
    predicted = c(0.1, 0.3, 0.7, 0.9),
    model = "test_model"
  )
  expect_error(
    plot_discrimination(df),
    "needs to be a valid forecast object"
  )
})

test_that("plot_discrimination() errors for the wrong forecast type", {
  expect_error(
    plot_discrimination(as_forecast_quantile(example_quantile)),
    "Forecast type determined by scoringutils"
  )
})

test_that("plot_discrimination() errors with invalid type", {
  expect_error(
    plot_discrimination(forecast_binary, type = "invalid"),
    "arg"
  )
})

test_that("plot_discrimination() handles single-model data", {
  single_model <- forecast_binary[model == "EuroCOVIDhub-ensemble"]
  p <- plot_discrimination(single_model)
  expect_s3_class(p, "ggplot")
})

test_that("plot_discrimination() shows separation between observed levels", {
  df <- data.frame(
    observed = factor(c(rep("0", 50), rep("1", 50)), levels = c("0", "1")),
    predicted = c(rep(0.1, 50), rep(0.9, 50)),
    model = "perfect",
    id = seq_len(100)
  )
  forecast <- as_forecast_binary(df, forecast_unit = c("model", "id"))
  p <- plot_discrimination(forecast)
  expect_s3_class(p, "ggplot")

  build_data <- ggplot2::ggplot_build(p)
  layer_data <- build_data$data[[1]]
  expect_gte(length(unique(layer_data$group)), 2)
})

test_that("plot_discrimination() handles identical predictions", {
  df <- data.frame(
    observed = factor(c("0", "0", "1", "1"), levels = c("0", "1")),
    predicted = c(0.5, 0.5, 0.5, 0.5),
    model = "constant",
    id = seq_len(4)
  )
  forecast <- as_forecast_binary(df, forecast_unit = c("model", "id"))
  expect_no_error(plot_discrimination(forecast))
})
