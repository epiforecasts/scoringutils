library(magrittr, quietly = TRUE)
library(ggplot2, quietly = TRUE)

sum_scores <- suppressMessages(
  summarise_scores(scores, by = c("model", "target_type"))
)

test_that("plot_wis() works as expected with relative contributions", {
  p <- plot_wis(sum_scores,
    x = "model",
    relative_contributions = TRUE
  ) +
    facet_wrap(~target_type)
  expect_s3_class(p, "ggplot")
  skip_on_cran()
  vdiffr::expect_doppelganger("plot_wis", p)
})

test_that("plot_wis() works as expected without relative contributions", {
  p <- plot_wis(sum_scores,
    x = "model",
    relative_contributions = FALSE
  ) +
    facet_wrap(~target_type)
  expect_s3_class(p, "ggplot")
  skip_on_cran()
  vdiffr::expect_doppelganger("plot_wis_no_relative", p)
})
