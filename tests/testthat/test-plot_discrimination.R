test_that("plot_discrimination() works with a forecast_binary object", {
  p <- plot_discrimination(na.omit(example_binary))
  expect_s3_class(p, "ggplot")
  skip_on_cran()
  vdiffr::expect_doppelganger("plot_discrimination", p)
})

test_that("plot_discrimination() works with faceting by model", {
  p <- plot_discrimination(na.omit(example_binary)) +
    facet_wrap(~model)
  expect_s3_class(p, "ggplot")
  skip_on_cran()
  vdiffr::expect_doppelganger("plot_discrimination_facet_model", p)
})

test_that("plot_discrimination() works with a plain data.frame input", {
  df <- data.frame(
    observed = factor(c("0", "0", "1", "1"), levels = c("0", "1")),
    predicted = c(0.1, 0.3, 0.7, 0.9),
    model = "test_model"
  )
  p <- plot_discrimination(df)
  expect_s3_class(p, "ggplot")
})

test_that("plot_discrimination() errors with missing required columns", {
  df_no_observed <- data.frame(predicted = c(0.1, 0.5, 0.9))
  df_no_predicted <- data.frame(
    observed = factor(c("0", "1", "0"), levels = c("0", "1"))
  )
  expect_error(plot_discrimination(df_no_observed), "observed")
  expect_error(plot_discrimination(df_no_predicted), "predicted")
})

test_that("plot_discrimination() handles single-model data", {
  single_model <- na.omit(example_binary)[
    model == "EuroCOVIDhub-ensemble"
  ]
  p <- plot_discrimination(single_model)
  expect_s3_class(p, "ggplot")
})

test_that("plot_discrimination() shows separation between observed levels", {
  df <- data.frame(
    observed = factor(c(rep("0", 50), rep("1", 50)), levels = c("0", "1")),
    predicted = c(rep(0.1, 50), rep(0.9, 50)),
    model = "perfect"
  )
  p <- plot_discrimination(df)
  expect_s3_class(p, "ggplot")

  build_data <- ggplot2::ggplot_build(p)
  # The density layer should have at least 2 groups
  layer_data <- build_data$data[[1]]
  expect_true(length(unique(layer_data$group)) >= 2)
})

test_that("plot_discrimination() handles edge case with all identical predictions", {
  df <- data.frame(
    observed = factor(c("0", "0", "1", "1"), levels = c("0", "1")),
    predicted = c(0.5, 0.5, 0.5, 0.5),
    model = "constant"
  )
  expect_no_error(plot_discrimination(df))
})
