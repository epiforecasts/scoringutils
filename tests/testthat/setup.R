data.table::setDTthreads(2) # restricts number of cores used on CRAN

metrics_no_cov <- metrics_quantile(
  exclude = c("interval_coverage_50", "interval_coverage_90",
              "interval_coverage_deviation")
)
metrics_no_cov_no_ae <- metrics_quantile(
  exclude = c("interval_coverage_50", "interval_coverage_90",
              "interval_coverage_deviation", "ae_median")
)

example_quantile_df <- as.data.frame(na.omit(example_quantile))
checkmate::assert_number(length(class(example_quantile_df)))

# pre-validated forecast objects
forecast_quantile <- as_forecast_quantile(na.omit(example_quantile))
forecast_sample_continuous <- as_forecast_sample(na.omit(example_sample_continuous))
forecast_sample_discrete <- as_forecast_sample(na.omit(example_sample_discrete))
forecast_point <- as_forecast_point(na.omit(example_point))
forecast_binary <- as_forecast_binary(na.omit(example_binary))
forecast_nominal <- as_forecast_nominal(na.omit(example_nominal))

# pre-computed scores
scores_quantile <- score(forecast_quantile)
scores_sample_continuous <- score(forecast_sample_continuous)
scores_sample_discrete <- score(forecast_sample_discrete)
scores_point <- score(forecast_point)
scores_binary <- score(forecast_binary)
scores_nominal <- score(forecast_nominal)
