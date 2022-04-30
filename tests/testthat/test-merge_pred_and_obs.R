test_that("merge pred and obs works", {
  data <- example_quantile
  forecasts <- example_quantile_forecasts_only
  truth_data <- example_truth_only

  eval1 <- suppressMessages(score(data = data))

  data2 <- merge_pred_and_obs(
    forecasts = forecasts,
    observations = truth_data
  )

  eval2 <- suppressMessages(score(data = data2))


  data.table::setcolorder(eval1, colnames(eval2))
  eval1 <- eval1[order(location, target_type, model, forecast_date, horizon)]

  # for some reason merge sometimes turns characters into factors.
  # Reverse this here.
  # not sure this needs a general solution
  eval2[, model := as.character(model)]

  eval2 <- eval2[order(location, target_type, model, forecast_date, horizon)]
  expect_equal(as.data.frame(eval1), as.data.frame(eval2), ignore_attr = TRUE)
})
