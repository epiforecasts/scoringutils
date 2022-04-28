library(ggplot2, quietly = TRUE)

scores <- suppressMessages(score(example_quantile))
scores <- suppressMessages(
  summarise_scores(scores, by = c("model", "target_type", "range"))
)

test_that("plot_heatmap() works as expected", {
  p <- plot_heatmap(scores, x = "target_type", metric = "bias")
  expect_s3_class(p, "ggplot")
  skip_on_cran()
  vdiffr::expect_doppelganger("plot_heatmap", p)
})