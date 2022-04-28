sum_scores <- suppressMessages(
  summarise_scores(scores, by = c("model", "target_type", "range"))
)

test_that("plot_ranges() works as expected with interval score", {
  p <- plot_ranges(sum_scores, x = "model") +
  facet_wrap(~target_type, scales = "free")
  expect_s3_class(p, "ggplot")
  skip_on_cran()
  vdiffr::expect_doppelganger("plot_ranges_interval", p)
})

test_that("plot_ranges() works as expected with dispersion", {
  p <- plot_ranges(sum_scores, y = "dispersion", x = "model") +
    facet_wrap(~target_type)
  expect_s3_class(p, "ggplot")
  skip_on_cran()
  vdiffr::expect_doppelganger("plot_ranges_dispersion", p)
})
