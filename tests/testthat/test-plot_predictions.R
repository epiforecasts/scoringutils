test_that("plot_predictions() works with point forecasts", {
  d <- scoringutils::example_quantile
  d <- d[d$quantile == 0.5 | is.na(d$quantile), ]

  p <-
    d %>%
    make_NA(what = "truth",
            target_end_date <= '2021-05-01',
            target_end_date > '2021-07-22') %>%
    make_NA(what = "forecast",
            model != 'EuroCOVIDhub-ensemble',
            forecast_date != '2021-06-07') %>%
    plot_predictions(
      by = c("location", "target_type"),
      x = "target_end_date",
    ) +
    facet_wrap(location ~ target_type, scales = "free_y")

  expect_s3_class(p, "ggplot")

  skip_on_cran()
  vdiffr::expect_doppelganger("point_forecasts", p)
})

test_that("plot_predictions() can handle an arbitrary number of quantiles", {
  example2 <- scoringutils::example_quantile

  p <-
    example2 %>%
    make_NA(what = "truth",
            target_end_date <= '2021-05-01',
            target_end_date > '2021-07-22') %>%
    make_NA(what = "forecast",
            model != 'EuroCOVIDhub-ensemble',
            forecast_date != '2021-06-07') %>%
    plot_predictions(
      by = c("location", "target_type"),
      x = "target_end_date",
      range = c(0, 10, 20, 30, 40, 50, 60)
    ) +
    facet_wrap(location ~ target_type, scales = "free_y")

  expect_s3_class(p, "ggplot")

  skip_on_cran()
  vdiffr::expect_doppelganger("many_quantiles", p)

  example1 <- scoringutils::example_continuous

  p2 <-
    example1 %>%
    make_NA(what = "truth",
            target_end_date <= '2021-05-01',
            target_end_date > '2021-07-22') %>%
    make_NA(what = "forecast",
            model != 'EuroCOVIDhub-ensemble',
            forecast_date != '2021-06-07') %>%
    plot_predictions(
      by = c("location", "target_type"),
      x = "target_end_date",
      range = c(0, 50, 90, 95)
    ) +
    facet_wrap(location ~ target_type, scales = "free_y")
  expect_s3_class(p2, "ggplot")

  skip_on_cran()
  vdiffr::expect_doppelganger("many_quantiles_from_sample", p2)
})

test_that("plot_predictions() works without median", {

  example3 <- subset(
    scoringutils::example_quantile,
    is.na(quantile) | quantile != 0.5
  )

  p <- example3 %>%
    make_NA(what = "truth",
            target_end_date <= '2021-06-25',
            target_end_date > '2021-07-12') %>%
    make_NA(what = "forecast",
            model != 'EuroCOVIDhub-ensemble',
            forecast_date != '2021-07-12') %>%
    plot_predictions(
      x = "target_end_date",
      by = c("location_name", "target_type")
    ) +
    facet_wrap(location_name ~ target_type, scales = "free_y")

  expect_s3_class(p, "ggplot")

  skip_on_cran()
  vdiffr::expect_doppelganger("no_median", p)
})


test_that("make_NA() works as expected", {

  example_quantile %>%
    make_NA(what = "both",
            target_end_date < "1999-01-01")

  example_quantile %>%
    make_NA(what = "both",
            'target_end_date < "1999-01-01"')


  expect_error(make_NA(example_quantile, what = "something wrong"))

  expect_error(make_NA())

  expect_error(
    example_quantile %>%
      make_NA(what = "truth",
              this_is_wrong < "1999-01-01")
  )
})
