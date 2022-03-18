test_that("summarise_scores() works without any arguments", {
  scores <- score(example_quantile)
  expect_true("quantile" %in% names(scores))

  scores <- summarise_scores(scores)
  expect_false("quantile" %in% names(scores))

  s2 <- summarise_scores(scores,
    by = c(
      "location", "target_end_date", "target_type",
      "location_name", "forecast_date", "model",
      "horizon"
    )
  )

  expect_equal(nrow(scores), nrow(s2))
})

test_that("summarise_scores() handles wrong by argument well", {
  scores <- score(example_quantile)

  expect_error(summarise_scores(scores, by = "not_present"),
    "The following items in `by` are notvalid column names of the data: 'not_present'. Check and run `summarise_scores()` again",
    fixed = TRUE
  )

  expect_error(summarise_scores(scores, by = "sample"),
    "The following items in `by` are notvalid column names of the data: 'sample'. Check and run `summarise_scores()` again",
    fixed = TRUE
  )
})
