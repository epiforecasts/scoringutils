test_that("find_duplicates() works as expected", {
  expect_equal(nrow(find_duplicates(example_quantile)), 0)
  expect_equal(
    nrow(
      find_duplicates(rbind(example_quantile, example_quantile[1000:1010]))),
    22
  )
})