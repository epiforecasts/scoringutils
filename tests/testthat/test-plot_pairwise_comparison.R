pairwise <- suppressMessages(
  pairwise_comparison(scores, by = "target_type")
)

test_that("plot_pairwise_comparison() works as expected", {
  p <- plot_pairwise_comparison(pairwise) +
    ggplot2::facet_wrap(~target_type)
  expect_s3_class(p, "ggplot")
  skip_on_cran()
  vdiffr::expect_doppelganger("plot_pairwise_comparison", p)
})

test_that("plot_pairwise_comparison() works when showing p values", {
  p <- plot_pairwise_comparison(pairwise, type = "pval") +
    ggplot2::facet_wrap(~target_type)
  expect_s3_class(p, "ggplot")
  skip_on_cran()
  vdiffr::expect_doppelganger("plot_pairwise_comparison_pval", p)
})
