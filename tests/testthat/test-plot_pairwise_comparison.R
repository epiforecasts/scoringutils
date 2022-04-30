pairwise <- suppressMessages(
  pairwise_comparison(scores, by = "target_type")
)

test_that("plot_pairwise_comparison() works as expected", {
  p <- plot_pairwise_comparison(pairwise) +
    ggplot2::facet_wrap(~target_type)
  expect_s3_class(p, "ggplot")
  skip_on_cran()
  vdiffr::expect_doppelganger("plot_pairwise_comparison", p)
})

test_that("plot_pairwise_comparison() works when showing p values", {
  p <- plot_pairwise_comparison(pairwise, type = "pval") +
    ggplot2::facet_wrap(~target_type)
  expect_s3_class(p, "ggplot")
  skip_on_cran()
  vdiffr::expect_doppelganger("plot_pairwise_comparison_pval", p)
})

test_that("plot_pairwise_comparison() works when everything", {
  p <- plot_pairwise_comparison(pairwise, type = "together") +
    ggplot2::facet_wrap(~target_type)
  expect_s3_class(p, "ggplot")
  skip_on_cran()
  vdiffr::expect_doppelganger("plot_pairwise_together", p)
})

test_that("plot_pairwise_comparison() works as expected when smaller is bad", {
  p <- plot_pairwise_comparison(pairwise, smaller_is_good = FALSE) +
    ggplot2::facet_wrap(~target_type)
  expect_s3_class(p, "ggplot")
  skip_on_cran()
  vdiffr::expect_doppelganger("plot_pairwise_comparison_sib", p)
})

test_that("plot_pairwise_comparison() doesn't work when showing p values and
           smaller is bad", {
  expect_error(
    plot_pairwise_comparison(
      pairwise, type = "pval", smaller_is_good = FALSE
    ) +
    ggplot2::facet_wrap(~target_type)
  )
})

test_that("plot_pairwise_comparison() works when everything is shown together and smaller is bad", {
  p <- plot_pairwise_comparison(
    pairwise, type = "together", smaller_is_good = FALSE
  ) +
    ggplot2::facet_wrap(~target_type)
  expect_s3_class(p, "ggplot")
  skip_on_cran()
  vdiffr::expect_doppelganger("plot_pairwise_together_sib", p)
})