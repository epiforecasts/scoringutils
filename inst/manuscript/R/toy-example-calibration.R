library(scoringutils)
library(patchwork)
library(ggplot2)
library(data.table)
library(dplyr)

# generate predictions data.table
n_truth = 2000
n_samples = 2000
truth <- rnorm(n_truth, mean = 0, sd = 1)
predictions1 <- rnorm(n_truth * n_samples, mean = 0, sd = 1)
predictions2 <- rnorm(n_truth * n_samples, mean = 0.5, sd = 1)
predictions3 <- rnorm(n_truth * n_samples, mean = 0, sd = 2)
predictions4 <- rnorm(n_truth * n_samples, mean = 0, sd =  0.5)

df <- data.table(true_value = rep(truth, each = n_samples),
                 id = rep(1:n_truth, each = n_samples),
                 prediction = c(predictions1, predictions2,
                                predictions3, predictions4),
                 sample = 1:n_samples,
                 `model` = rep(c("Pred: N(0, 1)", "Pred: N(0.5, 1)",
                                 "Pred: N(0, 2)", "Pred: N(0, 0.5)"),
                               each = n_truth * n_samples))

df[, model := factor(`model`,
                     levels = c("Pred: N(0, 1)", "Pred: N(0.5, 1)",
                                "Pred: N(0, 2)", "Pred: N(0, 0.5)"))]

if (!file.exists("inst/manuscript/output/calibration-diagnostic-examples.Rda")) {
  res <- score(df)
  pit <- pit(df, by = "model")

  stored <- list(res = res,
                 pit = pit)

  saveRDS(stored, "inst/manuscript/output/calibration-diagnostic-examples.Rda")

} else {

  stored <- readRDS("inst/manuscript/output/calibration-diagnostic-examples.Rda")
}

res_summarised <- summarise_scores(stored$res, by = c("model"))

scores_table_plot <- summarise_scores(res_summarised, fun = signif, digits = 2) |>
  select(-mad) |>
  plot_score_table(y = "model") +
  coord_flip() +
  theme_scoringutils() +
  theme(axis.text.x = element_text(angle = 0, vjust = 0, hjust = 0.5)) +
  theme(legend.position = "none")


# create histogram true vs. predicted ------------------------------------------
pred_hist <- df |>
  ggplot(aes(x = true_value)) +
  facet_wrap(~ model, nrow = 1) +
  geom_histogram(aes(y=..density..),
                 fill = "grey",
                 colour = "dark grey") +
  geom_density(aes(y=..density.., x = prediction),
                 colour = "black") +
  theme_scoringutils() +
  labs(y = "Density", x = "Value")


# create pit plots -------------------------------------------------------------
pit_plots <- plot_pit(stored$pit) +
  facet_wrap(~ model, nrow = 1) +
  theme_scoringutils()

# create interval and quantile coverage plots ----------------------------------
# create coverage plots by transforming to quantile format first
quantiles <- c(0.01, 0.025, seq(0.05, 0.95, 0.05), 0.975, 0.99)
df_quantile <- sample_to_quantile(df,
                                  quantiles = quantiles)

res_quantile <- score(df_quantile)
res_quantile <- summarise_scores(res_quantile,
                                 by = c("model", "range", "quantile"))

res_quantile[, model := factor(model,
                               levels = c("Pred: N(0, 1)", "Pred: N(0.5, 1)",
                                          "Pred: N(0, 2)", "Pred: N(0, 0.5)"))]

res_quantile[, model := model]

interval_coverage <- plot_interval_coverage(res_quantile) +
  facet_wrap(~ model, nrow = 1) +
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
  theme(panel.spacing = unit(2, "lines")) &
  plot_annotation(tag_levels = "A")
p

ggsave("inst/manuscript/output/calibration-diagnostic-examples.png", width = 11.5, height = 11)
