library(data.table)
library(scoringutils)
library(ggplot2)

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

#
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
  results[[paste(i)]] <- eval_forecasts(
    samples, metrics = c("crps", "log_score", "dss")
  )[, n_samples := i]
}

results2 <- rbindlist(results)

results2 <- melt(results2, id.vars = c("n_samples", "repetition"),
                 variable.name = "score")

label_fn <- function(x) {
  ifelse (x >= 1000,
          paste0(x / 1000, "k"),
          x)
}

df <- results2[, .(mean = mean(value),
                   quantile_0.05 = quantile(value, 0.05),
                   quantile_0.25 = quantile(value, 0.25),
                   quantile_0.75 = quantile(value, 0.75),
                   quantile_0.95 = quantile(value, 0.95)),
               by = c("n_samples", "score")]
df[score == "crps", true_score := true_crps]
df[score == "log_score", true_score := true_logs]
df[score == "dss", true_score := true_dss]

ggplot(df, aes(x = n_samples)) +
  geom_line(aes(y = mean)) +
  geom_ribbon(aes(ymax = quantile_0.95, ymin = quantile_0.05),
              alpha = 0.1) +
  geom_ribbon(aes(ymax = quantile_0.75, ymin = quantile_0.25),
              alpha = 0.3) +
  geom_hline(aes(yintercept = true_score), linetype = "dashed") +
  facet_wrap(~ score, scales = "free") +
  theme_minimal() +
  scale_x_continuous(trans = "log10", labels = label_fn) +
  theme() +
  labs(x = "Number of samples",
       y = "Score based on samples")

ggsave("inst/manuscript/plots/sample-convergence.png")
