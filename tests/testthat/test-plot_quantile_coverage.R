library(ggplot2, quietly = TRUE)

scores <- suppressMessages(score(example_quantile))
scores <- suppressMessages(
  summarise_scores(scores, by = c("model", "quantile"))
)

test_that("plot_quantile_coverage() works as expected", {
  p <- plot_quantile_coverage(scores)
  expect_s3_class(p, "ggplot")
  skip_on_cran()
  vdiffr::expect_doppelganger("plot_quantile_coverage", p)
})