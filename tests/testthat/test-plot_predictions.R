test_that("plot_predictions() works with point forecasts", {

  d <- example_range_long
  d <- d[d$range == 0 | is.na(d$range), ]
  p <- scoringutils::plot_predictions(
    d,
    x = "target_end_date",
    filter_truth = list('target_end_date <= "2021-07-22"',
                       'target_end_date > "2021-05-01"'),
    filter_forecasts = list("model == 'EuroCOVIDhub-ensemble'",
                           'forecast_date == "2021-06-07"'),
    allow_truth_without_pred = TRUE,
    facet_formula = location ~ target_type
  )

  expect_s3_class(p, "ggplot")

  skip_on_cran()
  vdiffr::expect_doppelganger('point_forecasts', p)

})

test_that("plot_predictions() can handle an arbitrary number of quantiles", {

  example2 <- scoringutils::example_range_long

  p <- scoringutils::plot_predictions(
    example2,
    x = "target_end_date",
    filter_truth = list('target_end_date <= "2021-07-22"',
                        'target_end_date > "2021-05-01"'),
    filter_forecasts = list("model == 'EuroCOVIDhub-ensemble'",
                            'forecast_date == "2021-06-07"'),
    allow_truth_without_pred = TRUE,
    facet_formula = location ~ target_type,
    range = c(0, 10, 20, 30, 40, 50, 60)
  )
  expect_s3_class(p, "ggplot")

  skip_on_cran()
  vdiffr::expect_doppelganger('many_quantiles', p)

  example1 <- scoringutils::example_continuous

  p2 <- scoringutils::plot_predictions(
    example1,
    x = "target_end_date",
    filter_truth = list('target_end_date <= "2021-07-22"',
                        'target_end_date > "2021-05-01"'),
    filter_forecasts = list("model == 'EuroCOVIDhub-ensemble'",
                            'forecast_date == "2021-06-07"'),
    facet_formula = location ~ target_type,
    range = c(0, 50, 90, 95)
  )
  expect_s3_class(p2, "ggplot")

  skip_on_cran()
  vdiffr::expect_doppelganger('many_quantiles_from_sample', p2)
})


