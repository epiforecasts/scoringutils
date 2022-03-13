library(scoringutils)
library(patchwork)
library(ggplot2)
library(data.table)
library(dplyr)

# generate predictions data.table
n_truth = 1000
n_samples = 1000
predictions <- rnorm(n_truth * n_samples, 0, 1)
true_values1 <- rnorm(n_samples)
true_values2 <- rnorm(n_samples, mean = 0.5)
true_values3 <- rnorm(n_samples, sd = 1.4)
true_values4 <- rnorm(n_samples, sd =  0.7)

df <- data.table(prediction = rep(predictions, each = 4),
                 id = rep(1:n_truth, each = n_samples),
                 true_value = rep(c(true_values1, true_values2,
                                    true_values3, true_values4), each = n_samples),
                 sample = 1:n_samples,
                 `true_distr` = rep(c("Truth: N(0, 1)", "Truth: N(0.5, 1)",
                                      "Truth: N(0, 1.4)", "Truth: N(0, 0.7)"),
                                    each = n_truth * n_samples))

df[, true_distr := factor(`true_distr`,
                     levels = c("Truth: N(0, 1)", "Truth: N(0.5, 1)",
                                "Truth: N(0, 1.4)", "Truth: N(0, 0.7)"))]

# obtain scores and create a table based on scores -----------------------------
res <- score(df)
res_summarised <- summarise_scores(res, by = c("true_distr"))

scores_table_plot <- plot_score_table(res_summarised, y = "true_distr") +
  coord_flip() +
  theme_scoringutils() +
  theme(axis.text.x = element_text(angle = 0, vjust = 0, hjust = 0.5)) +
  theme(legend.position = "none")


# create histogram true vs. predicted ------------------------------------------
pred_hist <- df |>
  ggplot(aes(x = true_value)) +
  facet_wrap(~ true_distr, nrow = 1) +
  geom_histogram(aes(y=..density..),
                 fill = "grey",
                 colour = "dark grey") +
  geom_function(fun = dnorm, colour = "black") +
  theme_scoringutils() +
  labs(y = "Density", x = "Value")


# create pit plots -------------------------------------------------------------
pit <- pit(df, by = "true_distr")
pit_plots <- plot_pit(pit) +
  facet_wrap(~ true_distr, nrow = 1) +
  theme_scoringutils()

# create interval and quantile coverage plots ----------------------------------
# create coverage plots by transforming to quantile format first
quantiles <- c(0.01, 0.025, seq(0.05, 0.95, 0.05), 0.975, 0.99)
df_quantile <- sample_to_quantile(df,
                                  quantiles = quantiles)

res_quantile <- score(df_quantile)
res_quantile <- summarise_scores(res_quantile,
                                 by = c("true_distr", "range", "quantile"))

res_quantile[, true_distr := factor(true_distr,
                                    levels = c("Truth: N(0, 1)", "Truth: N(0.5, 1)",
                                               "Truth: N(0, 1.4)", "Truth: N(0, 0.7)"))]

res_quantile[, model := true_distr]

interval_coverage <- plot_interval_coverage(res_quantile) +
  facet_wrap(~ true_distr, nrow = 1) +
  theme_scoringutils()

quantile_coverage <- plot_quantile_coverage(res_quantile) +
  facet_wrap(~ model, nrow = 1) +
  theme_scoringutils()


# bring plot together ----------------------------------------------------------
p <- pred_hist /
  pit_plots /
  interval_coverage /
  quantile_coverage /
  scores_table_plot +
  plot_layout(guides = 'collect') &
  theme(legend.position = "none") &
  theme(panel.spacing = unit(2, "lines"))

ggsave("inst/manuscript/plots/calibration-diagnostic-examples.png", width = 11.5, height = 11)













#
# # plot with observations
# true_value_plot <- ggplot2::ggplot(data = data.frame(x = true_values),
#                                    ggplot2::aes(x = x)) +
#   ggplot2::geom_histogram(ggplot2::aes(y = ..density..),
#                           fill = "grey",
#                           colour = "dark grey") +
#   theme_minimal() +
#   ggplot2::labs(x = "True values",
#                 y = "Density") +
#   ggplot2::theme(legend.position = "bottom")
#
# # plot with standard normal distribution
# standard_normal <- true_value_plot +
#   ggplot2::geom_function(fun = dnorm, colour = "black") +
#   ggplot2::ggtitle("Normal(0, 1)")
#
# # plot with shifted mean
# shifted_mean <- true_value_plot +
#   ggplot2::geom_function(fun = dnorm, colour = "black", args = list(mean = 0.5)) +
#   ggplot2::ggtitle("Normal(0.5, 1)")
#
# # plot with overdispersion
# overdispersion <- true_value_plot +
#   ggplot2::geom_function(fun = dnorm, colour = "black", args = list(sd = 1.4)) +
#   ggplot2::ggtitle("Normal(0, 1.4)")
#
# # plot with underdispersion
# underdispersion <- true_value_plot +
#   ggplot2::geom_function(fun = dnorm, colour = "black", args = list(sd = 0.7)) +
#   ggplot2::ggtitle("Normal(0, 0.7)")
#
# (standard_normal | shifted_mean | overdispersion | underdispersion) /
#   pit_plots /
#   interval_coverage /
#   quantile_coverage
# # /
# #   gridExtra::tableGrob(scores_table)
#
