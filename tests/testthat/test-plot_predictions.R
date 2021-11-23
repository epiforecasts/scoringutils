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
