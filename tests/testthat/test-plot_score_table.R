suppressMessages(library(magrittr))

test_that("plot_score_table() works as expected", {
  p <- suppressMessages(
    example_quantile %>%
      score() %>%
      add_coverage(by = c("model")) %>%
      summarise_scores(by = c("model")) %>%
      summarise_scores(by = c("model"), fun = signif, digits = 1) %>%
      plot_score_table()
  )

  expect_s3_class(p, "ggplot")

  skip_on_cran()
  vdiffr::expect_doppelganger("plot_score_table", p)
})
