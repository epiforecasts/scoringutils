test_that("plot_pit() works as expected with quantile forecasts", {
  pit <- example_quantile %>%
    na.omit() %>%
    as_forecast() %>%
    get_pit(by = "model")
  p <- plot_pit(pit, breaks = seq(0.1, 1, 0.1))
  expect_s3_class(p, "ggplot")
  skip_on_cran()
  vdiffr::expect_doppelganger("plot_pit_quantile", p)
})

test_that("plot_pit() works as expected with integer forecasts", {
  pit <- example_integer %>%
    na.omit() %>%
    as_forecast() %>%
    get_pit(by = "model")
  p <- plot_pit(pit)
  expect_s3_class(p, "ggplot")
  skip_on_cran()
  vdiffr::expect_doppelganger("plot_pit_integer", p)
})

test_that("plot_pit() works as expected with sample forecasts", {
  observed <- rnorm(30, mean = 1:30)
  predicted <- replicate(200, rnorm(n = 30, mean = 1:30))
  pit <- pit_sample(observed, predicted)
  p <- plot_pit(pit)
  expect_s3_class(p, "ggplot")
  skip_on_cran()
  vdiffr::expect_doppelganger("plot_pit_sample", p)
})
