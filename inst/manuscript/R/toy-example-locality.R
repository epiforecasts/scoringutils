library(scoringutils)
library(ggplot2)
library(data.table)

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

ggsave("inst/manuscript/output/score-locality.png", height = 3, width = 8)
