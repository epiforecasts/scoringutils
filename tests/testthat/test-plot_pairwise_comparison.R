
test_that("plot_pairwise_comparison() works as expected", {
  scores <- suppressMessagesscore(example_quantile))
  pairwise <- suppressMessages(
    pairwise_comparison(scores, by = "target_type")
  )
  p <- plot_pairwise_comparison(pairwise) +
    ggplot2::facet_wrap(~target_type)
  expect_s3_class(p, "ggplot")
  skip_on_cran()
  vdiffr::expect_doppelganger("plot_pairwise_comparison", p)
})
