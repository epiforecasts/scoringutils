test_that("plot_predictions() works with point forecasts", {

  d <- range_example_data_long
  d <- d[d$range == 0 | is.na(d$range), ]
  p <- scoringutils::plot_predictions(
    d,
    x = "value_date",
    filter_truth = list('value_date <= "2020-06-22"',
                       'value_date > "2020-05-01"'),
    filter_forecasts = list("model == 'SIRCOVID'",
                           'creation_date == "2020-06-22"'),
    allow_truth_without_pred = TRUE,
    facet_formula = geography ~ value_desc
  )

  vdiffr::expect_doppelganger('plot_predictions_point_forecasts', p)

})

test_that("plot_predictions() can handle an arbitrary number of quantiles", {

  example2 <- scoringutils::range_example_data_long

  p <- scoringutils::plot_predictions(
    example2, x = "value_date",
    filter_truth = list('value_date <= "2020-06-22"',
                       'value_date > "2020-05-01"'),
    filter_forecasts = list("model == 'SIRCOVID'",
                           'creation_date == "2020-06-22"'),
    allow_truth_without_pred = TRUE,
    facet_formula = geography ~ value_desc,
    range = c(0, 10, 20, 30, 40, 50, 60)
  )

  vdiffr::expect_doppelganger('plot_predictions_many_quantiles', p)
})
