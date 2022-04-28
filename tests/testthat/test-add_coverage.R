test_that("add_coverage() works as expected", {
  expect_error(add_coverage(scores))
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
