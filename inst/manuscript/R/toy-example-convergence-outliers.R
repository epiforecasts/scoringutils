library(data.table)
library(scoringutils)
library(ggplot2)

# ==============================================================================
# =================== Convergence of scores ====================================
# ==============================================================================

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


# ==============================================================================
# =================== scores and outliers, sd ==================================
# ==============================================================================

library(scoringRules)
library(dplyr)
library(patchwork)

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

res_sd[, `:=` (CRPS = mean(scoringRules::crps(y = true_values, family = "normal", mean = mu, sd = sd)),
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

res_mu2[, `:=` (CRPS = scoringRules::crps(y = true_value, family = "normal", mean = true_mu, sd = true_sd) / 10,
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
  # geom_vline(aes(xintercept = 0), linetype = "dashed") +
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

ggsave("inst/manuscript/output/score-convergence-outliers.png", plot = plot,
       height = 6, width = 8)

