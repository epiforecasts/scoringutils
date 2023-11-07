metrics_binary <- list(
  "brier_score" = brier_score,
  "log_score" = logs_binary
)
usethis::use_data(metrics_binary, overwrite = TRUE)

metrics_point <- list(
  "ae_point" = Metrics::ae,
  "se_point" = Metrics::se,
  "ape" = Metrics::ape
)
usethis::use_data(metrics_point, overwrite = TRUE)

metrics_sample <- list(
  "bias" = bias_sample,
  "dss" = dss_sample,
  "crps" = crps_sample,
  "log_score" = logs_sample,
  "mad" = mad_sample,
  "ae_median" = ae_median_sample,   # not sure we still want these
  "se_mean" = se_mean_sample       # not sure we still want these
)
usethis::use_data(metrics_sample, overwrite = TRUE)

metrics_quantile <- list()
