test_that("summarise_scores() works without any arguments", {
  scores <- score(example_quantile)
  expect_true("quantile" %in% names(scores))

  scores <- summarise_scores(scores)
  expect_false("quantile" %in% names(scores))
})

