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

  expect_s3_class(p, "ggplot")

  skip_on_cran()
  vdiffr::expect_doppelganger('point_forecasts', p)

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
  expect_s3_class(p, "ggplot")

  skip_on_cran()
  vdiffr::expect_doppelganger('many_quantiles', p)

  example1 <- scoringutils::continuous_example_data

  p2 <- scoringutils::plot_predictions(
    example1, x = "value_date",
    filter_truth = list('value_date <= "2020-06-22"',
                       'value_date > "2020-05-01"'),
    filter_forecasts = list("model == 'SIRCOVID'",
                           'creation_date == "2020-06-22"'),
    facet_formula = geography ~ value_desc,
    range = c(0, 50, 90, 95)
  )
  expect_s3_class(p2, "ggplot")

  skip_on_cran()
  vdiffr::expect_doppelganger('many_quantiles_from_sample', p2)
})


