# ex_coverage <- scores_quantile[model == "EuroCOVIDhub-ensemble"]
#
# test_that("add_coverage() works as expected", {
#   expect_error(
#     add_coverage(ex_coverage, by = c("model", "target_type"), range = c())
#   )
#   expect_error(
#     add_coverage(ex_coverage, by = c("model", "target_type")), NA
#   )
#   cov <- add_coverage(
#     scores_quantile, by = c("model", "target_type"), range = c(10, 20)
#   )
#   expect_equal(
#     grep("coverage_", colnames(cov), value = TRUE),
#     c("coverage_deviation", "coverage_10", "coverage_20")
#   )
# })
#
#
# test_that("Order of `add_coverage()` and `summarise_scores()` doesn't matter", {
#   # Need to update test. Turns out the order does matter...
#   # see https://github.com/epiforecasts/scoringutils/issues/367
#   pw1 <- add_coverage(ex_coverage, by = "model")
#   pw1_sum <- summarise_scores(pw1, by = "model")
#
#   pw2 <- summarise_scores(ex_coverage, by = "model")
#   pw2 <- add_coverage(pw2)
#
#   # expect_true(all(pw1_sum == pw2, na.rm = TRUE))
#   # expect_true(all(names(attributes(pw2)) == names(attributes(pw1_sum))))
# })
