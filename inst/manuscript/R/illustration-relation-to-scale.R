library(scoringutils)
library(dplyr)
library(ggplot2)
library(data.table)

library(data.table)
library(dplyr)
library(scoringutils)
library(ggplot2)
library(tidyr)
library(patchwork)

sizes_nbinom <- c(0.1, 1, 1e9)
n = 1000
mus <- c(1, 1e1, 1e2, 1e3, 1e4, 1e5)

df <- expand.grid("mu" = mus,
                  "size" = sizes_nbinom)
setDT(df)

df[, `Log score` := mean(scoringRules::logs(y = rnbinom(n, size = size, mu = mu),
                                          family = "negative-binomial",
                                          size = size, mu = mu)),
   by = c("mu", "size")]
df[, DSS := mean(scoringRules::dss_nbinom(y = rnbinom(n, size = size, mu = mu),
                                          size = size, mu = mu)),
   by = c("mu", "size")]
df[, CRPS := mean(scoringRules::crps(y = rnbinom(n, size = size, mu = mu),
                                          family = "negative-binomial",
                                          size = size, mu = mu)),
   by = c("mu", "size")]



df |>
  melt(measure.vars = c("Log score", "DSS"),
       variable.name = "Scoring rule",
       value.name = "Score") |>
  ggplot(aes(y = `Score`, x = mu, color = `Scoring rule`, group = `Scoring rule`)) +
  geom_line() +
  facet_wrap(~ size)



make_plot <- function(scores, summary_fct = mean) {
  p1 <- scores |>
    group_by(state_size, scale, Theta) |>
    summarise(interval_score = summary_fct(interval_score)) |>
    group_by(Theta, scale) |>
    mutate(interval_score = interval_score / mean(interval_score),
           Theta = ifelse(Theta == "1e+09", "1b", Theta)) |>
    ggplot(aes(y = interval_score, x = state_size, colour = Theta)) +
    geom_point(size = 0.4) +
    labs(y = "WIS", x = "Size of state") +
    theme_minimal() +
    facet_wrap(~ scale, scales = "free_y")

  p2 <- p1 +
    scale_x_continuous(trans = "log10") +
    scale_y_continuous(trans = "log10")

  p1 / p2
}




# sample Negative Binomial -----------------------------------------------------
# var = mu + mu^2/size --> increases a lot with mu
# theta = size very high --> poisson distribution
mean_county <- 100

sizes_nbinom <- c(0.1, 1, 1e9)

res <- list()
for (size_nbinom in sizes_nbinom) {
  df <- setup_df(time_points = 100) |>
    mutate(true_value = rnbinom(n = 1, size = size_nbinom,
                                mu = mean_county * state_size),
           prediction = qnbinom(p = quantile, size = size_nbinom,
                                mu = mean_county * state_size))

  scores <- score_states(df) |>
    mutate(Theta = as.character(size_nbinom))

  res[[paste(size_nbinom)]] <- scores
}

saveRDS(res, file = "output/data/simulation-negative-binom.Rda")
res <- readRDS(file = "output/data/simulation-negative-binom.Rda")

out <- rbindlist(res)

make_plot(out, summary_fct = mean) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom") &
  labs(y = "Relative WIS")

ggsave("output/figures/SIM-mean-state-size.png", width = 7, height = 4)

make_plot(out, summary_fct = stats::sd) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom") &
  labs(y = "Realtive WIS sd")

ggsave("output/figures/SIM-sd-state-size.png", width = 7, height = 4)










# example poisson --------------------------------------------------------------
mean_county <- 100
df <- setup_df() |>
  mutate(true_value = rpois(1, mean_county * state_size),
         prediction = qpois(p = quantile, lambda = mean_county * state_size))

scores <- score_states(df)
make_plot(scores)

ggsave("output/figures/SIM-mean-sd-state-size.png", width = 7, height = 4)

# Example very small county mean -----------------------------------------------
mean_county <- 1
df <- setup_df() |>
  mutate(true_value = rpois(1, mean_county * state_size),
         prediction = qpois(p = quantile, lambda = mean_county * state_size))

scores <- score_states(df)

make_plot(scores)




















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


df |>
  filter(horizon == 3, location == "DE") |>
  ggplot(aes(x = true_value, y = Score, ,group = `Scoring rule`,
             colour = `Scoring rule`)) +
  geom_line() +
  scale_y_log10() +
  scale_x_log10() +
  labs(x = "Observed value") +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave("inst/manuscript/plots/illustration-effect-scale.png",
       width = 10, height = 4)







# ------------------------------------------------------------------------------
# different illustration:
# in this we see that the mean as well as the variance of the scores scale
# for crps, while the variance stays constant for dss and log score


library(scoringutils)
library(dplyr)
library(data.table)
library(tidyr)
library(ggplot2)

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

res <- readRDS("inst/manuscript/plots/relation-to-scale-example.Rda")

# res <- grid |>
#   rowwise() |>
#   mutate(simulation := list(simulate(scale_mean = scale_mean, scale_sd = scale_sd)))
#
# saveRDS(res, file = "inst/manuscript/plots/relation-to-scale-example.Rda")

df <- res |>
  tidyr::unnest(cols = "simulation")

df |>
  filter(scale_mean == 1,
         score != "bias",
         scale_sd < 20) |>
  rename(Score = score) |>
  mutate(type = ifelse(type == "mean", "Mean score", "Sd score")) |>
  mutate(Score = ifelse(Score == "dss",
                        "DSS",
                        ifelse(Score == "crps", "CRPS", "Log score"))) |>
  ggplot(aes(y = value, x = scale_sd, group = Score, color = Score)) +
  geom_line() +
  facet_wrap(~ type, scales = "free") +
  scale_y_log10() +
  scale_x_log10() +
  theme_scoringutils() +
  labs(y = "Value", x = "Sd scaling factor (mean held constant)")

ggsave("inst/manuscript/plots/illustration-effect-scale-sim.png")
