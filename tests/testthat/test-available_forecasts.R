test_that("available_forecasts() works as expected", {
  af <- suppressMessages(
    available_forecasts(example_quantile,
      by = c("model", "target_type", "target_end_date")
    )
  )
  expect_type(af, "list")
  expect_type(af$target_type, "character")
  expect_type(af$`count`, "integer")
  expect_equal(nrow(af[is.na(`count`)]), 0)
  af <- suppressMessages(
    available_forecasts(example_quantile,
     by = "model"
    )
  )
  expect_equal(nrow(af), 4)
  expect_equal(af$`count`, c(256, 256, 128, 247))
  af <- suppressMessages(
    available_forecasts(example_quantile,
     by = "model", collapse = c()
    )
  )
  expect_equal(nrow(af), 4)
  expect_equal(af$`count`, c(5888, 5888, 2944, 5681))
  af <- suppressMessages(
    available_forecasts(example_quantile)
  )
  expect_equal(nrow(af), 50688)
})
