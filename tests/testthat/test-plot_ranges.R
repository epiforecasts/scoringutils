m <- modifyList(metrics_no_cov_no_ae, list("bias" = NULL))

sum_scores <- copy(example_quantile) %>%
  na.omit() %>%
  .[, interval_range := scoringutils:::get_range_from_quantile(quantile)] %>%
  score(metrics = m) %>%
  summarise_scores(by = c("model", "target_type", "interval_range"))

sum_scores[, range := interval_range]

test_that("plot_ranges() works as expected with interval score", {
  p <- plot_ranges(sum_scores, x = "model") +
    facet_wrap(~target_type, scales = "free")
  expect_s3_class(p, "ggplot")
  skip_on_cran()
  suppressWarnings(vdiffr::expect_doppelganger("plot_ranges_interval", p))
})

test_that("plot_ranges() works as expected with dispersion", {
  p <- plot_ranges(sum_scores, y = "dispersion", x = "model") +
    facet_wrap(~target_type)
  expect_s3_class(p, "ggplot")
  skip_on_cran()
  suppressWarnings(vdiffr::expect_doppelganger("plot_ranges_dispersion", p))
})
