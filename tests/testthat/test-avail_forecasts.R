test_that("avail_forecasts() works as expected", {
  af <- suppressMessages(
    avail_forecasts(example_quantile,
      by = c("model", "target_type", "target_end_date")
    )
  )
  expect_type(af, "list")
  expect_type(af$target_type, "character")
  expect_type(af$`Number forecasts`, "integer")
  expect_equal(nrow(af[is.na(`Number forecasts`)]), 0)
  af <- suppressMessages(
    avail_forecasts(example_quantile,
      by = c("model")
    )
  )
  expect_equal(nrow(af), 4)
  expect_equal(af$`Number forecasts`, c(256, 256, 247, 128))
  af <- suppressMessages(
    avail_forecasts(example_quantile,
      by = c("model"), collapse = c()
    )
  )
  expect_equal(nrow(af), 4)
  expect_equal(af$`Number forecasts`, c(5888, 5888, 5681, 2944))
  af <- suppressMessages(
    avail_forecasts(example_quantile)
  )
  expect_equal(nrow(af), 887)
})