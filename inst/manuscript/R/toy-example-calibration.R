library(scoringutils)
library(patchwork)
library(ggplot2)
library(data.table)


# Calibration Diagnostic plot --------------------------------------------------
n_truth = 1000
n_samples = 100
true_values <- rnorm(n_truth, 0, 1)
predictions1 <- rnorm(n_truth * n_samples)
predictions2 <- rnorm(n_truth * n_samples, mean = 0.5)
predictions3 <- rnorm(n_truth * n_samples, sd = 1.4)
predictions4 <- rnorm(n_truth * n_samples, sd =  0.7)
# predictions5 <- rnorm(n_truth * n_samples, mean = 0.2, sd =  0.7)

df <- data.table::data.table(true_value = rep(true_values, each = n_samples),
                             id = rep(1:n_truth, each = n_samples),
                             prediction = c(predictions1, predictions2,
                                            predictions3, predictions4),
                             sample = 1:n_samples,
                             model = rep(c("Normal(0, 1)", "Normal(0.5, 1)",
                                           "Normal(0, 1.4)", "Normal(0, 0.7)"),
                                         each = n_truth * n_samples))

# get scores
res <- eval_forecasts(df,
                      summarise_by = c("model"))

# create pit plots
pit <- scoringutils::pit_df(df, summarise_by = "model")
pit_plots <- scoringutils::hist_PIT(pit)

# create coverage plots by transforming to quantile format first
quantiles <- c(0.01, 0.025, seq(0.05, 0.95, 0.05), 0.975, 0.99)
df_quantile <- scoringutils::sample_to_quantile(df,
                                                quantiles = quantiles)

res_quantile <- eval_forecasts(df_quantile,
                               summarise_by = c("model", "range", "quantile"))

res_quantile[, model := factor(model, levels = c("Normal(0, 1)", "Normal(0.5, 1)", "Normal(0, 1.4)", "Normal(0, 0.7)"))]

interval_coverage <- scoringutils::interval_coverage(res_quantile) +
  facet_wrap(~ model, nrow = 1)

quantile_coverage <- scoringutils::quantile_coverage(res_quantile) +
  facet_wrap(~ model, nrow = 1)


# plot with observations
true_value_plot <- ggplot2::ggplot(data = data.frame(x = true_values),
                                   ggplot2::aes(x = x)) +
  ggplot2::geom_histogram(ggplot2::aes(y = ..density..),
                          fill = "grey",
                          colour = "dark grey") +
  theme_minimal() +
  ggplot2::labs(x = "True values",
                y = "Density") +
  ggplot2::theme(legend.position = "bottom")

# plot with standard normal distribution
standard_normal <- true_value_plot +
  ggplot2::geom_function(fun = dnorm, colour = "black") +
  ggplot2::ggtitle("Normal(0, 1)")

# plot with shifted mean
shifted_mean <- true_value_plot +
  ggplot2::geom_function(fun = dnorm, colour = "black", args = list(mean = 0.5)) +
  ggplot2::ggtitle("Normal(0.5, 1)")

# plot with overdispersion
overdispersion <- true_value_plot +
  ggplot2::geom_function(fun = dnorm, colour = "black", args = list(sd = 1.4)) +
  ggplot2::ggtitle("Normal(0, 1.4)")

# plot with underdispersion
underdispersion <- true_value_plot +
  ggplot2::geom_function(fun = dnorm, colour = "black", args = list(sd = 0.7)) +
  ggplot2::ggtitle("Normal(0, 0.7)")

scores_table <- dcast(melt(res, id.vars = "model",
                           variable.name = "score"),
                      score ~ model)
scores_table <- scores_table[, lapply(.SD, round, 2), by = score]

setcolorder(
  scores_table,
  c("score", "Normal(0, 1)", "Normal(0.5, 1)", "Normal(0, 1.4)", "Normal(0, 0.7)")
)

(standard_normal | shifted_mean | overdispersion | underdispersion) /
  (pit_plots$`Normal(0, 1)` | pit_plots$`Normal(0.5, 1)` | pit_plots$`Normal(0, 1.4)` | pit_plots$`Normal(0, 0.7)`) /
  interval_coverage /
  quantile_coverage /
  gridExtra::tableGrob(scores_table)


ggsave("inst/manuscript/plots/calibration-diagnostic-examples.png", width = 12.5, height = 10)
