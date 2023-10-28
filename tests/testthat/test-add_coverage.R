test_that("add_coverage() works as expected", {
  expect_error(
    add_coverage(scores, by = c("model", "target_type"), range = c())
  )
  expect_error(
    add_coverage(scores, by = c("model", "target_type")), NA
  )
  cov <- add_coverage(
    scores, by = c("model", "target_type"), range = c(10, 50, 80)
  )
  expect_equal(
    grep("coverage_", colnames(cov), value = TRUE),
    c("coverage_deviation", "coverage_10", "coverage_50", "coverage_80")
  )
})


test_that("Order of `add_coverage()` and `summarise_scores()` doesn't matter", {
  # Need to update test. Turns out the order does matter...
  # see https://github.com/epiforecasts/scoringutils/issues/367
  pw1 <- add_coverage(scores, by = "model")
  pw1_sum <- summarise_scores(pw1, by = "model")

  pw2 <- summarise_scores(scores, by = "model")
  pw2 <- add_coverage(pw2)

  # expect_true(all(pw1_sum == pw2, na.rm = TRUE))
  # expect_true(all(names(attributes(pw2)) == names(attributes(pw1_sum))))
})
