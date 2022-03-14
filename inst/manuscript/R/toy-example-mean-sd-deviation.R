library(data.table)
library(scoringutils)
library(ggplot2)
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
  theme_scoringutils() +
  geom_vline(aes(xintercept = 5), linetype = "dashed") +
  coord_cartesian(ylim=c(0, 20)) +
  annotate(geom="text", x=6, y=17, label="Sd of true \ndata-generating \ndistribution: 5",
           color="black", hjust = "left") +
  labs(y = "Score", x = "Standard deviation of predictive distribution")

#
# # look at effect of change in mean on score
# res_mu <- data.table(sd = true_sd,
#                      mu = mu,
#                      crps = NA_real_,
#                      dss = NA_real_,
#                      logs = NA_real_)
#
# res_mu[, `:=` (crps = mean(crps_sample(y = true_values, family = "normal", mean = mu, sd = sd)),
#                logs = mean(logs_sample(y = true_values, family = "normal", mean = mu, sd = sd)),
#                dss = mean(dss_norm(y = true_values, mean = mu, sd = sd))),
#        by = "mu"]
#
# deviation_mu <- res_mu |>
#   melt(id.vars = c("sd", "mu"), value.name = "value", variable.name = "Score") |>
#   ggplot(aes(x = mu, y = value, color = Score)) +
#   geom_line() +
#   theme_minimal() +
#   labs(y = "Score", x = "Mean of predictive distribution") +
#   geom_vline(aes(xintercept = 0), linetype = "dashed") +
#   coord_cartesian(ylim=c(0, 150))



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
           color="black", hjust = "left") +
  labs(y = "Score", x = "Observed value") +
  # geom_vline(aes(xintercept = 0), linetype = "dashed") +
  geom_area(stat = "function", fun = dnorm, color = "grey", fill = "grey", alpha = 0.5, xlim = c(0, 4)) +
  scale_y_continuous(label = label_fn)


deviation_sd + outlier +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

ggsave("inst/manuscript/plots/score-deviation-sd-mu.png",
       height = 3, width = 8)
