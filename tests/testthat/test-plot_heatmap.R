library(ggplot2, quietly = TRUE)

test_that("plot_heatmap() works as expected", {
  scores <- scores_quantile %>%
    summarise_scores(by = c("model", "target_type")) %>%
    summarise_scores(by = c("model", "target_type"), fun = round, digits = 2)
  p <- plot_heatmap(scores, x = "target_type", metric = "bias")
  expect_s3_class(p, "ggplot")
  skip_on_cran()
  vdiffr::expect_doppelganger("plot_heatmap", p)
})
