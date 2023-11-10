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

metrics_quantile <- list(
  "wis" = wis,
  "overprediction" = overprediction,
  "underprediction" = underprediction,
  "dispersion" = dispersion,
  "bias" = bias_quantile,
  "coverage_50" = \(...) {run_safely(..., range = 50, fun = interval_coverage_quantile)},
  "coverage_90" = \(...) {run_safely(..., range = 90, fun = interval_coverage_quantile)},
  "coverage_deviation" = \(...) {interval_coverage_deviation_quantile(...)}
)
usethis::use_data(metrics_quantile, overwrite = TRUE)
