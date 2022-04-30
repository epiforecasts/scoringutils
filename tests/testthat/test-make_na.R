suppressMessages(library(magrittr))

test_that("make_na() works as expected", {

  expect_error(
    example_quantile %>%
    make_na(what = "both",
            target_end_date < "1999-01-01"),
    NA
  )

  expect_error(
    example_quantile %>%
    make_na(what = "both",
            'target_end_date < "1999-01-01"'),
    NA
  )

  expect_error(make_na(example_quantile, what = "something wrong"))

  expect_error(make_na())

  expect_error(
    example_quantile %>%
      make_NA(what = "truth",
              this_is_wrong < "1999-01-01")
  )
})

test_that("make_NA works as make_na", {
  expect_error(
    example_quantile %>%
    make_NA(what = "both",
            target_end_date < "1999-01-01"),
    NA
  )
})