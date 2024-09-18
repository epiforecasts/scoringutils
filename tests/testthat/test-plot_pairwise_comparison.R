pairwise <- suppressMessages(
  get_pairwise_comparisons(scores_quantile, compare = "model", by = "target_type")
)

test_that("plot_pairwise_comparisons() works as expected", {
  p <- plot_pairwise_comparisons(pairwise) +
    ggplot2::facet_wrap(~target_type)
  expect_s3_class(p, "ggplot")
  skip_on_cran()
  vdiffr::expect_doppelganger("plot_pairwise_comparison", p)
})

test_that("plot_pairwise_comparisons() works when showing p values", {
  p <- plot_pairwise_comparisons(pairwise, type = "pval") +
    ggplot2::facet_wrap(~target_type)
  expect_s3_class(p, "ggplot")
  skip_on_cran()
  vdiffr::expect_doppelganger("plot_pairwise_comparison_pval", p)
})
