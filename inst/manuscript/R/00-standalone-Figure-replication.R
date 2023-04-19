# =============================================================================#
#        Standalone script to replicate all Figures in the manuscript          #
# =============================================================================#

library(ggplot2)
library(patchwork)
library(scoringutils)
library(data.table)
library(dplyr)
library(tidyr)
library(data.table)
library(scoringRules)

# =============================================================================#
# Figure 1, illustration of sharpness and calibration
# =============================================================================#
p1 <-
  ggplot(data.frame(x = seq(-8, 8, 0.01),
                    x_example = rnorm(n = 1601, mean = 0, sd = 0.45)),
         aes(x = x)) +
  geom_function(fun = dnorm, colour = "black",
                args = list(sd = 0.45)) +
  expand_limits(y = c(0, 1.0), x = c(-3, 3)) +
  scale_y_continuous(breaks = seq(0, 1, 0.25)) +
  ggtitle("More sharp") +
  theme_scoringutils()

p2 <-
  ggplot(data.frame(x = seq(-8, 8, 0.01),
                    x_example = rnorm(n = 1601, mean = 0, sd = 1.25)),
         aes(x = x)) +
  # geom_histogram(aes(x = x_example, y = ..density..), colour = "white", fill = "grey50") +
  geom_function(fun = dnorm, colour = "black",
                args = list(sd = 1.25)) +
  expand_limits(y = c(0, 1.0), x = c(-3, 3)) +
  scale_y_continuous(breaks = seq(0, 1, 0.25)) +
  ggtitle("Less sharp") +
  theme_scoringutils()

p21 <- ggplot(data.frame(x = seq(-8, 8, 0.01),
                         x_example = rnorm(n = 1601, mean = 0, sd = 1.05)),
              aes(x = x)) +
  geom_histogram(aes(x = x_example, y = ..density..), colour = "white", fill = "grey50") +
  geom_function(fun = dnorm, colour = "black",
                args = list(sd = 1)) +
  ggtitle("Well calibrated") +
  labs(y = "Density") +
  theme_scoringutils()

p22 <- ggplot(data.frame(x = seq(-8, 8, 0.01),
                         x_example = rnorm(n = 1601, mean = 1, sd = 1.05)),
              aes(x = x)) +
  geom_histogram(aes(x = x_example, y = ..density..), colour = "white", fill = "grey50") +
  geom_function(fun = dnorm, colour = "black",
                args = list(mean = 2, sd = 1)) +
  ggtitle("Badly calibrated") +
  labs(y = "Density") +
  theme_scoringutils()

p23 <- ggplot(data.frame(x = seq(-8, 8, 0.01),
                         x_example = rnorm(n = 1601, mean = 0, sd = 1.05)),
              aes(x = x)) +
  geom_histogram(aes(x = x_example, y = ..density..), colour = "white", fill = "grey50") +
  geom_function(fun = dnorm, colour = "black",
                args = list(mean = 0, sd = 2.05)) +
  ggtitle("Badly calibrated") +
  labs(y = "Density") +
  theme_scoringutils()

(p1 + p2) /
  (p21 + p22 + p23) &
  plot_annotation(tag_levels = "A")



# =============================================================================#
# Figure 2
# =============================================================================#
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

res_summarised <- summarise_scores(stored$res,by = "model")

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


# =============================================================================#
# Figure 3
# =============================================================================#

# =================== Convergence of scores ====================================
sample_sizes <- seq(50, 5000, 50)
sample_sizes <- round(1 * 10^(seq(1, 5, 0.1)))
n_rep <- 500

true_value = 0
sd <- 3
mu <- 2

# analytical scores
true_crps <- scoringRules::crps(y = 0, family = "normal", mean = mu, sd = sd)
true_logs <- scoringRules::logs(y = 0, family = "normal", mean = mu, sd = sd)
true_dss <- scoringRules::dss_norm(y = 0, mean = mu, sd = sd)

if (!file.exists("inst/manuscript/output/sample-convergence.Rda")) {
  results <- list()
  for (i in sample_sizes) {
    samples <- as.data.table(
      replicate(n_rep,
                rnorm(n = i, mean = mu, sd = sd))
    )
    setnames(samples, as.character(1:n_rep))
    samples[, sample := 1:i]
    samples <- melt(samples, id.vars = "sample",
                    variable.name = "repetition",
                    value.name = "prediction")
    samples[, true_value := true_value]
    results[[paste(i)]] <- score(
      samples, metrics = c("crps", "log_score", "dss")
    )[, n_samples := i]
  }
  saveRDS(results, "inst/manuscript/output/sample-convergence.Rda")
} else {
  results <- readRDS("inst/manuscript/output/sample-convergence.Rda")
}

results <- rbindlist(results)
results <- melt(results, id.vars = c("n_samples", "repetition", "model"),
                variable.name = "score")

label_fn <- function(x) {
  ifelse (x >= 1000,
          paste0(x / 1000, "k"),
          x)
}

df <- results[, .(mean = mean(value),
                  quantile_0.05 = quantile(value, 0.05),
                  quantile_0.25 = quantile(value, 0.25),
                  quantile_0.75 = quantile(value, 0.75),
                  quantile_0.95 = quantile(value, 0.95)),
              by = c("n_samples", "score")]
df[score == "crps", true_score := true_crps]
df[score == "log_score", true_score := true_logs]
df[score == "dss", true_score := true_dss]

df[, score := ifelse(score == "dss", "DSS",
                     ifelse(score == "crps", "CRPS",
                            "Log score"))]

p_convergence <- ggplot(df, aes(x = n_samples)) +
  geom_line(aes(y = mean)) +
  geom_ribbon(aes(ymax = quantile_0.95, ymin = quantile_0.05),
              alpha = 0.1) +
  geom_ribbon(aes(ymax = quantile_0.75, ymin = quantile_0.25),
              alpha = 0.3) +
  geom_hline(aes(yintercept = true_score), linetype = "dashed") +
  facet_wrap(~ score, scales = "free") +
  scale_x_continuous(trans = "log10", labels = label_fn) +
  theme_scoringutils() +
  labs(x = "Number of samples",
       y = "Score based on samples")


# =================== scores and outliers, sd ==================================

# define simulation parameters
n_steps = 500
n_rep <- 5000
true_mean = 0
true_sd = 5
true_values <- rnorm(n = n_rep, mean = true_mean, sd = true_sd)
sd <- 10^(seq(-1, 1.6, length.out = n_steps))
mu <- seq(0, 100, length.out = n_steps)


# look at effect of change in sd on score
res_sd <- data.table(sd = sd,
                     mu = true_mean)

res_sd[, `:=`(CRPS = mean(scoringRules::crps(y = true_values, family = "normal", mean = mu, sd = sd)),
               `Log score` = mean(scoringRules::logs(y = true_values, family = "normal", mean = mu, sd = sd)),
               DSS = mean(scoringRules::dss_norm(y = true_values, mean = mu, sd = sd))),
       by = "sd"]

deviation_sd <- res_sd |>
  melt(id.vars = c("sd", "mu"), value.name = "value", variable.name = "Score") |>
  ggplot(aes(x = sd, y = value, color = Score)) +
  geom_line() +
  scale_color_discrete(type = c("#E69F00", "#56B4E9", "#009E73")) +
  theme_scoringutils() +
  geom_vline(aes(xintercept = 5), linetype = "dashed") +
  coord_cartesian(ylim=c(0, 20)) +
  annotate(geom="text", x=6, y=12, label="Sd of true \ndata-generating \ndistribution: 5",
           color="black", hjust = "left", size = 3) +
  labs(y = "Score", x = "Standard deviation of predictive distribution")



# define simulation parameters
true_values <- seq(0, 4, length.out = 1000)
true_sd = 1
true_mu = 0

# look at effect of change in sd on score
res_mu2 <- data.table(true_value = true_values)

res_mu2[, `:=`(CRPS = scoringRules::crps(y = true_value, family = "normal", mean = true_mu, sd = true_sd) / 10,
                `Log score` = scoringRules::logs(y = true_value, family = "normal", mean = true_mu, sd = true_sd) / 10,
                DSS = scoringRules::dss_norm(y = true_value, mean = true_mu, sd = true_sd) / 10)]

label_fn <- function(x) {
  paste(10*x)
}

outlier <- res_mu2 |>
  melt(id.vars = c("true_value"), value.name = "value", variable.name = "Score") |>
  ggplot(aes(x = true_value, y = value, color = Score)) +
  geom_line() +
  theme_scoringutils() +
  annotate(geom="text", x=0, y=.8, label="Predictive distribution: \nN(0,1)",
           color="black", hjust = "left", size = 3) +
  labs(y = "Score", x = "Observed value") +
  scale_color_discrete(type = c("#E69F00", "#56B4E9", "#009E73")) +
  geom_area(stat = "function", fun = dnorm, color = "grey", fill = "grey", alpha = 0.5, xlim = c(0, 4)) +
  scale_y_continuous(label = label_fn)


layout <- "
AA
BC
"

plot <- p_convergence /
  deviation_sd + outlier +
  plot_layout(guides = "collect",
              design = layout) &
  plot_annotation(tag_levels = "A") &
  theme(legend.position = "bottom")




# =============================================================================#
# Figure 4
# =============================================================================#

quantiles <- seq(0.1, 1, 0.1)
forecast_a <- c(0.3, 0.35, 0.25, 0.04, 0.02, 0.01, 0.01, 0.01, 0.005, 0.005)
forecast_b <- c(0.1, 0.35, 0.05, 0.02, 0.01, 0.01, 0.05, 0.07, 0.2, 0.14)
true_value <- 2

df <- data.table(
  forecaster = rep(c("Forecaster A", "Forecaster B"), each = 10),
  outcome = rep(1:10, 2),
  prob = c(forecast_a, forecast_b),
  true_value = true_value
)

df[, crps := sum((cumsum(prob) - (outcome >= true_value))^2),
   by = c("forecaster")]
df[, log_score := -log(prob[outcome == true_value]),
   by = c("forecaster")]
df[, mean_pred := sum(prob * outcome) / sum(prob),
   by = c("forecaster")]
df[, sd_pred := sqrt(sum((prob * outcome - mean_pred)^2)),
   by = c("forecaster")]
df[, log_score := -log(prob[outcome == true_value]),
   by = c("forecaster")]
df[, dss := ((true_value - mean_pred)^2) / sd_pred + 2 * log(sd_pred),
   by = c("forecaster")]

# sense-check: compute crps using samples
sample_a <- sample(x=1:10, size = 1e5, replace = TRUE, prob = forecast_a)
sample_b <- sample(x=1:10, size = 1e5, replace = TRUE, prob = forecast_b)

crps_a <- scoringutils::crps_sample(2, t(as.matrix(sample_a)))
crps_b <- scoringutils::crps_sample(2, t(as.matrix(sample_b)))

annotation <- df[, .(forecaster, crps, log_score, dss)] |> unique()


ggplot(df, aes(x = factor(outcome), y = prob)) +
  geom_col() +
  geom_text(data = annotation, x = 4, y = 0.3, hjust = "left", size = 3,
            aes(label = paste("CRPS: ", round(crps, 2)))) +
  geom_text(data = annotation,x = 4, y = 0.27, hjust = "left", size = 3,
            aes(label = paste("Log score: ", round(log_score, 2)))) +
  geom_text(data = annotation, x = 4, y = 0.24, hjust = "left", size = 3,
            aes(label = paste("DSS: ", round(dss, 2)))) +
  facet_wrap(~ forecaster) +
  geom_vline(aes(xintercept = 2), linetype = "dashed") +
  theme_scoringutils() +
  labs(y = "Probability assigned", x = "Possible outcomes")



# =============================================================================#
# Figure 5
# =============================================================================#

## Real Data
ex <- example_continuous |>
  filter(model == "EuroCOVIDhub-ensemble")

scores <- ex |>
  score()

setnames(scores, old = c("dss", "crps", "log_score"),
         new = c("DSS", "CRPS", "Log score"))

df <- ex[sample == 1] |>
  merge(scores) |>
  melt(measure.vars = c("DSS", "CRPS", "Log score"),
       variable.name = "Scoring rule", value.name = "Score")

df[, `Scoring rule` := factor(`Scoring rule`, levels = c("CRPS", "DSS", "Log score"))]

p_true <- df |>
  filter(horizon == 3, location == "DE") |>
  ggplot(aes(x = true_value, y = Score, ,group = `Scoring rule`,
             colour = `Scoring rule`)) +
  geom_line() +
  scale_color_discrete(type = c("#E69F00", "#56B4E9", "#009E73")) +
  scale_y_log10() +
  scale_x_log10() +
  labs(x = "Observed value") +
  theme_scoringutils() +
  theme(legend.position = "bottom")


# ------------------------------------------------------------------------------
# illustration:
# in this we see that the mean as well as the variance of the scores scale
# for crps, while the variance stays constant for dss and log score

simulate <- function(n_samples = 5e3,
                     n_replicates = 1e3,
                     true_value = 1,
                     scale_mean = 1,
                     scale_sd = scale_mean) {
  pred <- rnorm(n_replicates * n_samples,
                mean = true_value * scale_mean,
                sd = true_value * scale_sd)

  df <- data.table(
    true_value = true_value * scale_mean,
    prediction = pred,
    sample = 1:n_samples,
    id = paste0("id", rep(1:n_replicates, each = n_samples))
  )

  scores <- score_simulation(df, scale_mean = scale_mean, scale_sd = scale_sd)
  return(scores)
}

score_simulation <- function(df, scale_mean = 1, scale_sd = scale_mean) {
  scores <- score(df)
  m <- summarise_scores(scores, by = "model", fun = mean) |>
    melt(id.vars = "model", value.name = "mean", variable.name = "score")

  s <- summarise_scores(scores, by = "model", fun = stats::sd) |>
    melt(id.vars = "model", value.name = "sd", variable.name = "score")

  out <- merge(m, s, by = c("model", "score")) |>
    melt(id.vars = c("model", "score"), variable.name = "type")

  return(out[])
}

scales_mean <- scales_sd <- c(1, 2, 5, 10, 20, 50)

grid <- expand.grid(
  scale_mean = scales_mean,
  scale_sd = scales_sd
) |>
  setDT()


if (!file.exists("inst/manuscript/output/relation-to-scale-example.Rda")) {
  res <- grid |>
    rowwise() |>
    mutate(simulation := list(simulate(scale_mean = scale_mean, scale_sd = scale_sd)))

  saveRDS(res, file = "inst/manuscript/output/relation-to-scale-example.Rda")
} else {
  res <- readRDS("inst/manuscript/output/relation-to-scale-example.Rda")
}

df <- res |>
  tidyr::unnest(cols = "simulation")

df <- df |>
  filter(score != "bias") |>
  rename(`Scoring rule` = score) |>
  mutate(type = ifelse(type == "mean", "Mean score", "Sd score")) |>
  mutate(`Scoring rule` = ifelse(`Scoring rule` == "dss",
                                 "DSS",
                                 ifelse(`Scoring rule` == "crps", "CRPS", "Log score")))

p1 <- df |>
  filter(scale_mean == 1,
         scale_sd < 20) |>
  ggplot(aes(y = value, x = scale_sd,
             group = `Scoring rule`, color = `Scoring rule`)) +
  geom_line() +
  facet_wrap(~ type, scales = "free") +
  scale_y_log10() +
  scale_color_discrete(type = c("#E69F00", "#56B4E9", "#009E73")) +
  scale_x_log10() +
  theme_scoringutils() +
  labs(y = "Score", x = "Sd of F and G (mean constant)")


p2 <- df |>
  filter(scale_sd == 1,
         scale_mean < 20) |>
  ggplot(aes(y = value, x = scale_mean,
             group = `Scoring rule`, color = `Scoring rule`)) +
  geom_line() +
  facet_wrap(~ type, scales = "free") +
  scale_y_log10() +
  scale_x_log10() +
  scale_color_discrete(type = c("#E69F00", "#56B4E9", "#009E73")) +
  theme_scoringutils() +
  labs(y = "Score", x = "Mean of F and G (sd constant)")

layout <- "
AAACC
BBBCC
"

p2 + p1 + p_true +
  plot_layout(guides = "collect", design = layout) &
  theme(legend.position = "bottom") &
  plot_annotation(tag_levels = 'A')



# =============================================================================#
# Figure 6
# =============================================================================#
avail_forecasts(data = example_integer,
                by = c("model", "target_type", "forecast_date")) |>
  plot_avail_forecasts(x = "forecast_date",
                       show_numbers = FALSE) +
  facet_wrap(~ target_type) +
  labs(y = "Model", x = "Forecast date")



# =============================================================================#
# Figure 7
# =============================================================================#
example_quantile %>%
  make_na(what = "truth",
          target_end_date > "2021-07-15",
          target_end_date <= "2021-05-22") %>%
  make_na(what = "forecast",
          model != "EuroCOVIDhub-ensemble",
          forecast_date != "2021-06-28") %>%
  plot_predictions(x = "target_end_date", by = c("target_type", "location")) +
  aes(colour = model, fill = model) +
  facet_wrap(target_type ~ location, ncol = 4, scales = "free_y") +
  labs(x = "Target end date")



# =============================================================================#
# Figure 8
# =============================================================================#
score(example_quantile) |>
  summarise_scores(by = c("model", "target_type")) |>
  summarise_scores(fun = signif, digits = 2) |>
  plot_score_table(y = "model", by = "target_type") +
  facet_wrap(~ target_type)



# =============================================================================#
# Figure 9
# =============================================================================#
score(example_quantile) |>
  pairwise_comparison(by = c("model", "target_type"),
                      baseline = "EuroCOVIDhub-baseline") |>
  plot_pairwise_comparison() +
  facet_wrap(~ target_type)



# =============================================================================#
# Figure 10
# =============================================================================#
score(example_continuous) |>
  summarise_scores(by = c("model", "location", "target_type")) |>
  plot_heatmap(x = "location", metric = "bias") +
  facet_wrap(~ target_type)



# =============================================================================#
# Figure 11
# =============================================================================#
p1 <- score(example_quantile) |>
  summarise_scores(by = c("model", "target_type")) |>
  plot_wis(relative_contributions = FALSE) +
  facet_wrap(~ target_type,
             scales = "free_x") +
  theme(panel.spacing = unit(1.5, "lines"))

p2 <- score(example_quantile) |>
  summarise_scores(by = c("model", "target_type")) |>
  plot_wis(relative_contributions = TRUE) +
  facet_wrap(~ target_type,
             scales = "free_x") +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank()) +
  theme(panel.spacing = unit(1.5, "lines")) +
  labs(x = "Normalised WIS contributions")

p1 + p2 +
  plot_annotation(tag_levels = "A") +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")



# =============================================================================#
# Figure 12
# =============================================================================#
example_continuous |>
  pit(by = c("model", "target_type")) |>
  plot_pit() +
  facet_grid(target_type ~ model)



# =============================================================================#
# Figure 13
# =============================================================================#
cov_scores <- score(example_quantile) |>
  summarise_scores(by = c("model", "target_type", "range", "quantile"))

p1 <- plot_interval_coverage(cov_scores) +
  facet_wrap(~ target_type) +
  theme(panel.spacing = unit(2, "lines"))

p2 <- plot_quantile_coverage(cov_scores) +
  facet_wrap(~ target_type) +
  theme(panel.spacing = unit(2, "lines"))

p1 / p2 +
  plot_annotation(tag_levels = "A") +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

# =============================================================================#
# Figure 14
# =============================================================================#
correlations <- example_quantile |>
  score() |>
  summarise_scores() |>
  correlation()

correlations |>
  glimpse()

correlations |>
  plot_correlation()
