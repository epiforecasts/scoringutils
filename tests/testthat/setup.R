data.table::setDTthreads(2) # restricts number of cores used on CRAN

metrics_no_cov <- get_metrics(
  example_quantile,
  exclude = c("interval_coverage_50", "interval_coverage_90")
)
metrics_no_cov_no_ae <- get_metrics(
  example_quantile,
  exclude = c("interval_coverage_50", "interval_coverage_90",
              "ae_median")
)

example_quantile_df <- as.data.frame(na.omit(example_quantile))
checkmate::assert_number(length(class(example_quantile_df)))


# pre-computed scores
scores_quantile <- score(example_quantile)
scores_sample_continuous <- score(example_sample_continuous)
scores_sample_discrete <- score(example_sample_discrete)
scores_point <- score(example_point)
scores_binary <- score(example_binary)
scores_nominal <- score(example_nominal)