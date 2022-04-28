library(magrittr, quietly = TRUE)
library(ggplot2, quietly = TRUE)

scores <- suppressMessages(score(example_quantile))
scores <- suppressMessages(
  summarise_scores(scores, by = c("model", "target_type"))
)
#' plot_wis(scores,
#'   x = "model",
#'   relative_contributions = TRUE
#' ) +
#'   facet_wrap(~target_type)
#' plot_wis(scores,
#'   x = "model",
#'   relative_contributions = FALSE
#' ) +
#'   facet_wrap(~target_type, scales = "free_x")
#' 
test_that("plot_wis() works as expected with relative contributions", {
  p <- plot_wis(scores,
    x = "model",
    relative_contributions = TRUE
  ) +
    facet_wrap(~target_type)

  expect_s3_class(p, "ggplot")

  skip_on_cran()
  vdiffr::expect_doppelganger("plot_wis", p)
})

test_that("plot_wis() works as expected without relative contributions", {
  p <- plot_wis(scores,
    x = "model",
    relative_contributions = FALSE
  ) +
    facet_wrap(~target_type)

  expect_s3_class(p, "ggplot")

  skip_on_cran()
  vdiffr::expect_doppelganger("plot_wis_no_relative", p)
})
