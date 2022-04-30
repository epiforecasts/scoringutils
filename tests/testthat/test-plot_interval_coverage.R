library(ggplot2, quietly = TRUE)

test_that("plot_interval_coverage() works as expected", {
  scores <- suppressMessages(
    summarise_scores(scores, by = c("model", "range"))
  )
  p <- plot_interval_coverage(scores)
  expect_s3_class(p, "ggplot")
  skip_on_cran()
  vdiffr::expect_doppelganger("plot_interval_coverage", p)
})