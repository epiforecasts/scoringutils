test_that("merge pred and obs works within eval_forecasts", {

  data <- scoringutils::quantile_example_data
  forecasts <- scoringutils::example_quantile_forecasts_only
  truth_data <- scoringutils::example_truth_data_only

  eval1 <- scoringutils::eval_forecasts(data = data)

  eval2 <- scoringutils::eval_forecasts(forecasts = forecasts,
                                        truth_data = truth_data)


  data.table::setcolorder(eval1, colnames(eval2))
  eval1 <- eval1[order(geography, value_type, model, value_date)]

  # for some reason merge sometimes turns characters into factors.
  # Reverse this here.
  # not sure this needs a general solution
  eval2[, model := as.character(model)]

  eval2 <- eval2[order(geography, value_type, model, value_date)]
  expect_equal(as.data.frame(eval1), as.data.frame(eval2), ignore_attr = TRUE)
})



