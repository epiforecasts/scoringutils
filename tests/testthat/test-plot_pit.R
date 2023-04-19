test_that("plot_pit() works as expected with quantile forecasts", {
  pit <- suppressMessages(
    pit(example_quantile,by = "model")
  )
  p <- plot_pit(pit, breaks = seq(0.1, 1, 0.1))
  expect_s3_class(p, "ggplot")
  skip_on_cran()
  vdiffr::expect_doppelganger("plot_pit_quantile", p)
})

test_that("plot_pit() works as expected with integer forecasts", {
  pit <- suppressMessages(
    pit(example_integer,by = "model")
  )
  p <- plot_pit(pit)
  expect_s3_class(p, "ggplot")
  skip_on_cran()
  vdiffr::expect_doppelganger("plot_pit_integer", p)
})

test_that("plot_pit() works as expected with sample forecasts", {
  true_values <- rnorm(30, mean = 1:30)
  predictions <- replicate(200, rnorm(n = 30, mean = 1:30))
  pit <- pit_sample(true_values, predictions)
  p <- plot_pit(pit)
  expect_s3_class(p, "ggplot")
  skip_on_cran()
  vdiffr::expect_doppelganger("plot_pit_sample", p)
})