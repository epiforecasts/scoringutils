
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
df[, logS := -log(prob[outcome == true_value]),
   by = c("forecaster")]

# sense-check: compute crps using samples
sample_a <- sample(x=1:10, size = 1e5, replace = TRUE, prob = forecast_a)
sample_b <- sample(x=1:10, size = 1e5, replace = TRUE, prob = forecast_b)

crps_a <- scoringutils::crps(true_value_x, t(as.matrix(sample_a)))
crps_b <- scoringutils::crps(true_value_x, t(as.matrix(sample_b)))

ggplot(df, aes(x = factor(outcome), y = prob)) +
  geom_col() +
  geom_text(x = 8, y = 0.3, aes(label = paste("CRPS: ", round(crps, 3)))) +
  geom_text(x = 8, y = 0.27, aes(label = paste("LogS: ", round(logS, 3)))) +
  facet_wrap(~ forecaster) +
  geom_vline(aes(xintercept = 2), linetype = "dashed") +
  theme_light() +
  labs(y = "Probability assigned", x = "Possible outcomes")

ggsave("inst/manuscript/plots/score-locality.png")


# test with WIS. Problem: that doesn't work at the moment as intervals are
# not symmetric
# dt <- copy(df)
# dt[, quantile := cumsum(prob_b)]
# dt[, prediction := outcome]
# eval_forecasts(dt[, .(true_value, prediction, quantile)])
