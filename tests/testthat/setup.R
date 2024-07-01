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

# compute scores
scores_quantile <- suppressMessages(score(as_forecast_quantile(example_quantile)))
scores_continuous <- suppressMessages(score(as_forecast_sample(example_sample_continuous)))
scores_point <- suppressMessages(score(as_forecast_point(example_point)))
scores_binary <- suppressMessages(score(as_forecast_binary(example_binary)))

class(as.data.frame(example_quantile))
