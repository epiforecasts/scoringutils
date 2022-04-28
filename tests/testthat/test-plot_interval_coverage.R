library(ggplot2, quietly = TRUE)

scores <- suppressMessages(score(example_quantile))
scores <- suppressMessages(
  summarise_scores(scores, by = c("model", "range"))
)

test_that("plot_interval_coverage() works as expected", {
  p <- plot_interval_coverage(scores)
  expect_s3_class(p, "ggplot")
  skip_on_cran()
  vdiffr::expect_doppelganger("plot_interval_coverage", p)
})