test_that("summarise_scores() works without any arguments", {
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

  expect_error(summarise_scores(scores, by = "not_present"),
    "The following items in `by` are notvalid column names of the data: 'not_present'. Check and run `summarise_scores()` again", # nolint
    fixed = TRUE
  )

  expect_error(summarise_scores(scores, by = "sample"),
    "The following items in `by` are notvalid column names of the data: 'sample'. Check and run `summarise_scores()` again", # nolint
    fixed = TRUE
  )
})

test_that("summarise_scores() works with point forecasts in a quantile format", {
  ex <- data.table::copy(example_quantile)

  ex[quantile == 0.5, quantile := NA_real_]

  scores <- suppressMessages(score(ex))

  summarise_scores(scores, by = "model",
                   na.rm = TRUE)

  summarise_scores(scores, by = "model",
                   na.rm = TRUE,
                   relative_skill = TRUE)

  scores <- suppressMessages(score(example_point[is.na(quantile)]))

  expect_warning(
    expect_warning(
      summarise_scores(
        scores, by = "model", relative_skill = TRUE, na.rm = TRUE)
    )
  )
})

test_that("summarise_scores() can compute relative measures", {
  ex <- data.table::copy(example_quantile)
  scores <- suppressMessages(score(ex))

  expect_equal(
    summarise_scores(
      scores, by = "model", relative_skill = TRUE
    )[, relative_skill],
    c(1.6, 0.81, 0.75, 1.03), tolerance = 0.01
  )

  expect_equal(
    summarise_scores(
      scores, by = "model", relative_skill = TRUE,
      relative_skill_metric = "ae_median"
    )[, relative_skill],
    c(1.6, 0.78, 0.77, 1.04), tolerance = 0.01
  )
})

test_that("summarise_scores(): metric is deprecated", {
  ex <- data.table::copy(example_quantile)
  scores <- suppressMessages(score(ex))

  expect_equal(
    suppressWarnings(summarise_scores(
    scores, by = "model", metric = "auto", relative_skill = TRUE
  ))[, relative_skill],
    c(1.6, 0.81, 0.75, 1.03), tolerance = 0.01
  )  
  expect_snapshot(
    x <- summarise_scores(
      scores, by = "model", metric = "auto", relative_skill = TRUE
    )
  )  
})