test_that("plot_interval_coverage() works as expected", {
  coverage <- add_coverage(example_quantile) %>%
    summarise_scores(by = c("model", "range"))
  p <- plot_interval_coverage(coverage)
  expect_s3_class(p, "ggplot")
  skip_on_cran()
  suppressWarnings(vdiffr::expect_doppelganger("plot_interval_coverage", p))
})
