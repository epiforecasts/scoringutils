library(scoringutils)
library(patchwork)
library(ggplot2)
library(data.table)

# generate predictions data.table
n_truth = 1000
n_samples = 100
true_values <- rnorm(n_truth, 0, 1)
predictions1 <- rnorm(n_truth * n_samples)
predictions2 <- rnorm(n_truth * n_samples, mean = 0.5)
predictions3 <- rnorm(n_truth * n_samples, sd = 1.4)
predictions4 <- rnorm(n_truth * n_samples, sd =  0.7)

df <- data.table(true_value = rep(true_values, each = n_samples),
                 id = rep(1:n_truth, each = n_samples),
                 prediction = c(predictions1, predictions2,
                                predictions3, predictions4),
                 sample = 1:n_samples,
                 model = rep(c("N(0, 1)", "N(0.5, 1)",
                               "N(0, 1.4)", "N(0, 0.7)"),
                             each = n_truth * n_samples))

df[, model := factor(model, levels = c("N(0, 1)", "N(0.5, 1)",
                                       "N(0, 1.4)", "N(0, 0.7)"))]


# obtain scores and create a table based on scores -----------------------------
res <- score(df)
res <- summarise_scores(res, by = c("model"))

# scores_table <- dcast(melt(res, id.vars = "model",
#                            variable.name = "score"),
#                       score ~ model)
# scores_table <- scores_table[, lapply(.SD, round, 2), by = score]

# setcolorder(
#   scores_table,
#   c("score", "N(0, 1)", "N(0.5, 1)", "N(0, 1.4)", "N(0, 0.7)")
# )

res[, model := factor(model, levels = c("N(0, 1)", "N(0.5, 1)",
                                        "N(0, 1.4)", "N(0, 0.7)"))]

setcolorder(
  res,
  c("score", "N(0, 1)", "N(0.5, 1)", "N(0, 1.4)", "N(0, 0.7)")
)

scores_table_plot <- plot_score_table(res, y = "model") +
  coord_flip() +
  theme(axis.text.x = element_text(angle = 0, vjust = 0, hjust = 0.5)) +
  theme(legend.position = "none")



saveRDS(object = scores_table,
        file = "inst/manuscript/plots/calibration-diagnostic-examples.Rda")


# create histogram true vs. predicted ------------------------------------------
pred_hist <- df |>
  ggplot(aes(x = prediction)) +
  facet_wrap(~ model, nrow = 1) +
  geom_histogram(aes(y=..density..),
                 fill = "grey",
                 colour = "dark grey") +
  geom_function(fun = dnorm, colour = "black") +
  theme_minimal() +
  labs(y = "Density", x = "Value")


# create pit plots -------------------------------------------------------------
pit <- pit(df, by = "model")
pit_plots <- plot_pit(pit) +
  facet_wrap(~ model, nrow = 1) +
  theme_minimal()

# create interval and quantile coverage plots ----------------------------------
# create coverage plots by transforming to quantile format first
quantiles <- c(0.01, 0.025, seq(0.05, 0.95, 0.05), 0.975, 0.99)
df_quantile <- sample_to_quantile(df,
                                  quantiles = quantiles)

res_quantile <- score(df_quantile)
res_quantile <- summarise_scores(res_quantile,
                                 by = c("model", "range", "quantile"))

res_quantile[, model := factor(model,
                               levels = c("N(0, 1)", "N(0.5, 1)",
                                          "N(0, 1.4)", "N(0, 0.7)"))]

interval_coverage <- plot_interval_coverage(res_quantile) +
  facet_wrap(~ model, nrow = 1) +
  theme_minimal()

quantile_coverage <- plot_quantile_coverage(res_quantile) +
  facet_wrap(~ model, nrow = 1) +
  theme_minimal()


# bring plot together ----------------------------------------------------------
pred_hist /
  pit_plots /
  interval_coverage /
  quantile_coverage /
  scores_table_plot +
  plot_layout(guides = 'collect') &
  theme(legend.position = "bottom")

ggsave("inst/manuscript/plots/calibration-diagnostic-examples.png", width = 12.5, height = 11)













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
