test_that("plot works as expected for correlation", {
  correlations <- correlation(summarise_scores(scores))
  p <- plot(correlations)
  expect_s3_class(p, "ggplot")
  skip_on_cran()
  vdiffr::expect_doppelganger("plot__correlation", p)
})
